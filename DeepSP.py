# !/usr/bin/env python
# -*- coding: utf-8 -*-
# @Author   : wangbing
# @File       : DeepSP.py
# @Time     : 1/18/2022 9:36 PM
# @Desc     : DeepSP for Protein Subcellular Localization Prediction
# @Emal     : wangbing587@163.com


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
    f_mean = df.mean(1)
    data = np.zeros((n, 1, m, m), dtype=np.float)
    for x in range(n):
        for i in range(m):
            for j in range(m):
                if i < j:
                    data[x, 0, i, j] = df[x, i] - df[x, j]
                if i == j:
                    data[x, 0, i, j] = df[x, i] - f_mean[x]
                if i > j:
                    data[x, 0, i, j] = np.tanh(np.log2((df[x, i] + e) / (df[x, j] + e)))
    return data.round(3)


class channel_attention(nn.Module):
    def __init__(self, channel, ratio=2):
        super(channel_attention, self).__init__()
        self.max_pool = nn.AdaptiveMaxPool2d(1)
        self.avg_pool = nn.AdaptiveAvgPool2d(1)
        self.fc = nn.Sequential(
            nn.Linear(channel, channel // ratio, False),
            nn.ReLU(),
            nn.Linear(channel // ratio, channel, False)
        )
        self.sigmoid = nn.Sigmoid()

    def forward(self, x):
        b, c, h, w = x.size()
        max_pool_out = self.max_pool(x).view([b, c])
        avg_pool_out = self.avg_pool(x).view([b, c])

        max_fc_out = self.fc(max_pool_out)
        avg_fc_out = self.fc(avg_pool_out)

        out = max_fc_out + avg_fc_out
        out = self.sigmoid(out).view([b, c, 1, 1])
        return out * x


class spacial_attention(nn.Module):
    def __init__(self, kernel_size=3):
        super(spacial_attention, self).__init__()
        padding = kernel_size // 2
        self.conv = nn.Conv2d(2, 1, kernel_size, 1, padding, bias=False)
        self.sigmoid = nn.Sigmoid()

    def forward(self, x):
        max_pool_out, _ = torch.max(x, dim=1, keepdim=True)
        avg_pool_out = torch.mean(x, dim=1, keepdim=True)
        pool_out = torch.cat([max_pool_out, avg_pool_out], dim=1)

        out = self.conv(pool_out)
        out = self.sigmoid(out)
        return out * x


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
            nn.MaxPool2d(kernel_size=2))

        self.conv2 = nn.Sequential(nn.Conv2d(16, 32, 3, 1, 1),
                                   nn.BatchNorm2d(32),
                                   nn.ReLU())

        self.channel_attention = channel_attention(channel, ratio)
        self.spacial_attention = spacial_attention(kernel_size)

        self.fc1 = nn.Sequential(
            nn.Linear(32 * (fraction // 2) ** 2, 512),
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
        x = self.channel_attention(x)
        x = self.spacial_attention(x)
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
                print(f'[{epoch}/{epochs}] val_loss: {val_loss} | f1score: {val_f1score} | accuracy: {val_acc}')

            if val_f1score_best < val_f1score:
                bestepoch = epoch
                val_f1score_best = val_f1score
                val_acc_best = val_acc
                val_loss_best = val_loss
                y_valprob_best = y_valprob
                torch.save(model, 'model{}.pkl'.format(index + 1))

    print(f'best epoch [{bestepoch}/{epochs}] | val_loss: {val_loss_best} | f1score: {val_f1score_best} | accuracy: {val_acc_best}')

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


def main():
    print('++++++++++++++++++++Loading data & Making feature map++++++++++++++++++++')
    data0 = pd.read_csv(datafile, index_col=0)
    data = deepcopy(data0)
    global target
    global fraction
    print('Rawdata shape:', data.shape)
    print('Markers stats:')
    print(data['markers'].value_counts())
    train_set = data[data['markers'] != 'unknown'].drop('markers', axis=1)
    target = data.loc[data['markers'] != 'unknown', 'markers']
    test_set = data[data['markers'] == 'unknown'].drop('markers', axis=1)
    train_data = makemap(train_set, rep)
    print('Train data feature map: ', train_data.shape)

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
    trainresult['DeepSPScore'] = y_trainprob.max(1)
    trainresult['DeepSPClassification'] = y_trainprob.argmax(1)
    trainresult['DeepSPClassification'] = trainresult['DeepSPClassification'].map(inv_class_mapping)
    trainresult.to_csv(f'TrainDeepSP_{datafile}')
    print('+++++++++++++++++++++++++Cross_validation result+++++++++++++++++++++++++')
    print(classification_report(trainresult['markers'], trainresult['DeepSPClassification'], digits=3))

    if getpred == "Y":
        print('+++++++++++++++++++Constructing feature map & Start prediction+++++++++++++++++')
        x_test = torch.tensor(makemap(test_set, rep), dtype=torch.float)
        print('Test data feature map: ', x_test.shape)
        y_testprob = predict(x_test)
        testresult = pd.DataFrame(data=y_testprob, index=test_set.index, columns=list(class_mapping.keys()))
        testresult['DeepSPScore'] = y_testprob.max(1)
        testresult['DeepSPClassification'] = y_testprob.argmax(1)
        testresult['DeepSPClassification'] = testresult['DeepSPClassification'].map(inv_class_mapping)
        testresult.to_csv(f'TestDeepSP_{datafile}')
        print('++++++++++++++++++Predicting proteins with unknown PSLs++++++++++++++++++')
        print(testresult['DeepSPClassification'].value_counts())
    for index in range(n_splits):
        os.remove('model{}.pkl'.format(index + 1))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description="DeepSP for Protein Subcellular Localization Prediction")
    parser.add_argument("-f",
                        "--file",
                        dest='f',
                        type=str,
                        help="file path, first column must be ProteinID, last column must be markers")
    parser.add_argument("-r",
                        "--rep",
                        dest='r',
                        type=int,
                        help="number of repeated experiments",)
    parser.add_argument("-p",
                        "--getpred",
                        dest='p',
                        type=str,
                        help="whether to predict unknown proteins, default=Y",
                        default="Y",
                        choices=["Y", "N"])
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
    getpred = args.p
    n_splits = args.n
    batch_size = args.bs
    seed = args.s
    lr = args.lr
    l2 = args.l2
    epochs = args.eps
    nepoch = args.nep

    t0 = time.time()
    main()
    print('using time: {} m {}s'.format(int((time.time() - t0) // 60), (time.time() - t0) % 60))
