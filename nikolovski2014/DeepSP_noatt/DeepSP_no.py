#!/usr/bin/env python
# -*- coding: utf-8 -*-
# @Time : 2021/05/08 11:29
# @Author : wangbing
# @FileName: DeepSP.py

import pandas as pd
import numpy as np
import argparse
import time
from copy import deepcopy
import os
import warnings

from sklearn.metrics import classification_report, f1_score, accuracy_score
from sklearn.model_selection import StratifiedKFold

import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.utils.data import TensorDataset, DataLoader
warnings.filterwarnings('ignore')


def makemap(df0, r, e=1e-6):
    df = np.array(deepcopy(df0))
    n, m = df.shape
    f = int(m / r)
    df = np.hstack([df[:, j * f:(j + 1) * f] /
                    df[:, j * f:(j + 1) * f].sum(1).reshape(-1, 1) for j in range(r)])

    data = np.zeros((n, 1, m, m), dtype=np.float)
    for i in range(n):
        for x in range(m):
            for y in range(m):
                if x < y:
                    data[i, 0, x, y] = df[i, x] - df[i, y]
                if x > y:
                    data[i, 0, x, y] = np.tanh(np.log2(((df[i, x] + e) / (df[i, y] + e))))
                if x == y:
                    data[i, 0, x, y] = df[i, x] - 1 / f
    return data.round(3)


class DeepSP(nn.Module):
    def __init__(self, fraction, out_channels, channel=32, ratio=2, kernel_size=3):
        super(DeepSP, self).__init__()
        self.conv1 = nn.Sequential(
            nn.Conv2d(in_channels=1,
            out_channels=16,
            kernel_size=3,
            stride=1,
            padding=1),
            nn.BatchNorm2d(16),
            nn.ReLU(),
             nn.MaxPool2d(kernel_size=2)
        )
        self.conv2 = nn.Sequential(nn.Conv2d(16, 32, 3, 1, 1),
                                   nn.BatchNorm2d(32), nn.ReLU(),
#                                   nn.MaxPool2d(kernel_size=2)
                                  )
        
    #    self.channel_attention = channel_attention(channel, ratio)
      #  self.spacial_attention = spacial_attention(kernel_size)

        self.fc1 = nn.Sequential(
            nn.Linear(32 * (fraction // 2) ** 2 , 512),
#             nn.Linear(32 * fraction ** 2 , 512),
            nn.ReLU(),
            nn.Dropout(p=0.3))

        self.fc2 = nn.Sequential(
            nn.Linear(512, 256),
            nn.ReLU(),
            nn.Dropout(p=0.3))

        self.fc3 = nn.Linear(256, out_channels)

    def forward(self, x):
        x = self.conv1(x)
        x = self.conv2(x)
       # x = self.channel_attention(x)
      #  x = self.spacial_attention(x)
        x = self.fc1(x.view(x.size(0), -1))
        x = self.fc2(x)
        x = self.fc3(x)
        return x


class MultiFocalLoss(nn.Module):
    def __init__(self, alpha=None, gamma=0):
        super(MultiFocalLoss, self).__init__()
        self.alpha = alpha
        self.gamma = gamma

    def forward(self, outputs, targets):
        if self.alpha is None and self.gamma == 0:
            focal_loss = F.cross_entropy(outputs, targets)

        elif self.alpha is not None and self.gamma == 0:
            self.alpha = torch.tensor(self.alpha) / torch.tensor(self.alpha).sum()
            self.alpha = self.alpha.to(outputs)
            focal_loss = F.cross_entropy(outputs, targets, weight=self.alpha)

        elif self.alpha is None and self.gamma != 0:
            ce_loss = F.cross_entropy(outputs, targets, reduction='none')
            p_t = torch.exp(-ce_loss)
            focal_loss = ((1 - p_t) ** self.gamma * ce_loss).mean()

        else:
            self.alpha = torch.tensor(self.alpha) / torch.tensor(self.alpha).sum()
            self.alpha = self.alpha.to(outputs)
            ce_loss = F.cross_entropy(outputs, targets, reduction='none')
            p_t = torch.exp(-ce_loss)
            ce_loss = F.cross_entropy(outputs, targets, weight=self.alpha, reduction='none')
            focal_loss = ((1 - p_t) ** self.gamma * ce_loss).mean()

        return focal_loss


def train(model, train_loader, val_db, index,  weight):
    x_val, y_val = val_db
    sm = nn.Softmax(dim=1)
    optimizer = torch.optim.Adam(model.parameters(), lr=lr, weight_decay=l2)
    loss_func = MultiFocalLoss(alpha=weight, gamma=2)
    val_f1score_best = 0.0
    for epoch in range(1, epochs + 1):
        model.train()
        for i, (x_batch, y_batch) in enumerate(train_loader):
            out = model(x_batch)
            loss = loss_func(out, y_batch)
            optimizer.zero_grad()
            loss.backward()
            optimizer.step()

        model.eval()
        with torch.no_grad():
            val_out = model(x_val)
            loss = loss_func(val_out, y_val)
            val_loss = loss.item()
            y_valprob = sm(val_out)
            y_valscore, y_valpred = torch.max(y_valprob, 1)
            val_f1score = f1_score(y_val, y_valpred, average='macro')
            val_acc = accuracy_score(y_val, y_valpred)

            if epoch % nepoch == 0:
                print('[{}/{}] val_loss: {} | f1score: {} | accuracy: {}'
                      .format(epoch, epochs, val_loss, val_f1score, val_acc))

            if val_f1score_best < val_f1score:
                bestepoch = epoch
                val_f1score_best = val_f1score
                val_acc_best = val_acc
                val_loss_best = val_loss
                y_valprob_best = y_valprob
                torch.save(model, 'model{}.pkl'.format(index + 1))

    print('bech epoch [{}/{}] | val_loss: {} | f1score: {} | accuracy: {}'
          .format(bestepoch, epochs, val_loss_best, val_f1score_best, val_acc_best))

    return y_valprob_best.numpy()


def cross_validation(x, y):
    y_trainprob = np.zeros((y.shape[0], len(set(target))))
    skf = StratifiedKFold(n_splits=n_splits, shuffle=True, random_state=seed)
    for index, (train_index, val_index) in enumerate(skf.split(x, y)):
        print('Cross validation: ', index + 1)
        x_train, x_val = x[train_index], x[val_index]
        y_train, y_val = y[train_index], y[val_index]
        train_db = TensorDataset(x_train, y_train)
        weight = 1 / np.bincount(y_train)
        train_loader = DataLoader(train_db,
                                  batch_size=batch_size,
                                  num_workers=0,
                                  shuffle=True)
        val_db = (x_val, y_val)
        torch.manual_seed(seed)
        model = DeepSP(fraction, len(set(target)))
        y_trainprob[val_index] = train(model, train_loader, val_db, index, weight)

    return y_trainprob


def predict(x_test):
    sm = nn.Softmax(dim=1)
    y_testprob_result = np.zeros((x_test.shape[0], len(set(target))))
    for index in range(n_splits):
        model = torch.load('model{}.pkl'.format(index + 1))
        model.eval()
        with torch.no_grad():
            y_testprob_result += sm(model(x_test)).numpy() / n_splits
    return y_testprob_result


def compFDR(data):
    data = data.sort_values('score', ascending=False)
    FDR = []
    for i in range(data.shape[0]):
        a = data[['pred', 'markers']].iloc[:(i+1), :]
        FDR.append(sum(a['pred'].values != a['markers'].values) / a.shape[0])
    qvalue = np.zeros(len(FDR))
    qvalue[-1] = FDR[-1]
    qvalue[0] = FDR[0]
    for i in range(len(FDR)-2, 0, -1):
            qvalue[i] = min(FDR[i], qvalue[i+1])
    data['FDR'] = FDR
    data['qvalue'] = qvalue
    return data


def main():
    print('++++++++++++++++++++Loading data & Making feature map++++++++++++++++++++')
    data0 = pd.read_csv(datafile, index_col=0)
    data = deepcopy(data0)
    global target
    global fraction
    print('rawdata shape:', data.shape)
    print('markers stats:')
    print(data['markers'].value_counts())
    train_set = data[data['markers'] != 'unknown'].drop('markers', axis=1)
    target = data.loc[data['markers'] != 'unknown', 'markers']
    test_set = data[data['markers'] == 'unknown'].drop('markers', axis=1)

    train_data = makemap(train_set, rep)
    print('train data feature map: ', train_data.shape)
    fraction = train_data.shape[2]
    class_mapping = {label: idx for idx, label in enumerate(sorted(set(target)))}
    inv_class_mapping = {v: k for k, v in class_mapping.items()}
    train_label = target.map(class_mapping).values

    x = torch.tensor(train_data, dtype=torch.float)
    y = torch.tensor(train_label)
    print('++++++++++++++++++++++++++++Start training+++++++++++++++++++++++++++++')
    y_trainprob = cross_validation(x, y)

    trainresult = pd.DataFrame(data=y_trainprob, index=train_set.index, columns=list(class_mapping.keys()))
    trainresult['markers'] = target
    trainresult['score'] = y_trainprob.max(1)
    trainresult['pred'] = y_trainprob.argmax(1)
    trainresult['pred'] = trainresult['pred'].map(inv_class_mapping)
    trainresult = compFDR(trainresult)
    trainresult.to_csv('{}trainresult.csv'.format(outfile))
    print('+++++++++++++++++++++++++Cross_validation result+++++++++++++++++++++++++')
    print(classification_report(trainresult['markers'], trainresult['pred'], digits=3))

    if getpred == "Y":
        print('++++++++++++++++++++Making feature map & Start prediction+++++++++++++++++++')
        x_test = torch.tensor(makemap(test_set, rep), dtype=torch.float)
        print('test data feature map: ', x_test.shape)
        y_testprob = predict(x_test)
        testresult = pd.DataFrame(data=y_testprob, index=test_set.index, columns=list(class_mapping.keys()))
        testresult['score'] = y_testprob.max(1)
        testresult['pred'] = y_testprob.argmax(1)
        testresult['pred'] = testresult['pred'].map(inv_class_mapping)

        cutoff = trainresult.loc[trainresult['qvalue'] < qval, 'score'].iloc[-1]
        testresult['cutoff'] = max(cutoff, 0.5)
        testresult['toppred'] = 'unknown'
        testresult.loc[testresult['score'] >= cutoff, 'toppred'] = testresult.loc[testresult['score'] >= cutoff, 'pred']
        testresult.to_csv('{}testresult.csv'.format(outfile))
        print('predicting unknown proteins with qvalue(FDR) < {} & score > 0.5'.format(qval))
        print(testresult.toppred.value_counts())
    for index in range(n_splits):
        os.remove('model{}.pkl'.format(index + 1))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description="DeepSP for Subcellular Localization")
    parser.add_argument("-f",
                        "--file",
                        dest='f',
                        type=str,
                        help="file path, first column must be ProteinID, last column must be markers")
    parser.add_argument("-r",
                        "--rep",
                        dest='r',
                        type=int,
                        help="number of repeated experiments")
    parser.add_argument("-o",
                        "--outfile",
                        dest='o',
                        type=str,
                        help="output file prefix, default=outfile + trainresult/testresult.csv",
                        default="")
    parser.add_argument("-p",
                        "--getpred",
                        dest='p',
                        type=str,
                        help="whether to predict unknown proteins, default=Y",
                        default="Y",
                        choices=["Y", "N"])

    parser.add_argument("-q",
                        "--qvalue",
                        dest='q',
                        type=float,
                        help="qvalue(FDR) setting for predicting unknown proteins",
                        default=0.015)
    parser.add_argument("-n",
                        "--n_splits",
                        dest='n',
                        type=int,
                        help="n-fold cross validation, default=5",
                        default=5)
    parser.add_argument("-bs",
                        "--batch_size",
                        dest='bs',
                        type=int,
                        help="mini batch size, default=64",
                        default=64)
    parser.add_argument("-s",
                        "-seed",
                        dest='s',
                        type=int,
                        help="a random seed, default=0",
                        default=0)
    parser.add_argument("-lr",
                        "--learning_rate",
                        dest='lr',
                        type=float,
                        help="learning rate, default=1e-3",
                        default=1e-3)
    parser.add_argument("-l2",
                        "--l2_regularization",
                        dest='l2',
                        type=float,
                        help="l2 regularization, default=1e-4",
                        default=0.0)
    parser.add_argument("-eps",
                        "--epochs",
                        dest='eps',
                        type=int,
                        help="training times, default=100",
                        default=100)
    parser.add_argument("-nep",
                        "--nepoch",
                        dest='nep',
                        type=int,
                        help="print the results every * times, default=20",
                        default=20)
    args = parser.parse_args()

    datafile = args.f
    rep = args.r
    outfile = args.o
    getpred = args.p
    n_splits = args.n
    qval = args.q
    batch_size = args.bs
    seed = args.s
    lr = args.lr
    l2 = args.l2
    epochs = args.eps
    nepoch = args.nep

    t0 = time.time()
    main()
    print('using time: {} m {}s'.format(int((time.time() - t0) // 60), (time.time() - t0) % 60))
