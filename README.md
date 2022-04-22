# DeepSP
DeepSP: A Deep Learning Framework for Spatial Proteomics


### Install dependencies
```
pip install -r requirements.txt
```


## DeepSP help
```
$ python DeepSP.py -h
usage: DeepSP.py [-h] [-f F] [-r R] [-o O] [-p {Y,N}] [-q Q] [-n N] [-bs BS]
                 [-s S] [-lr LR] [-l2 L2] [-eps EPS] [-nep NEP]

DeepSP for Subcellular Localization

optional arguments:
  -h, --help            show this help message and exit
  -f F, --file F        file path, first column must be ProteinID, last column
                        must be markers
  -r R, --rep R         number of repeated experiments
  -o O, --outfile O     output file prefix, default=outfile +
                        trainresult/testresult.csv
  -p {Y,N}, --getpred {Y,N}
                        whether to predict unknown proteins, default=Y
  -q Q, --qvalue Q      qvalue(FDR) setting for predicting unknown proteins
  -n N, --n_splits N    n-fold cross validation, default=5
  -bs BS, --batch_size BS
                        mini batch size, default=64
  -s S, -seed S         a random seed, default=0
  -lr LR, --learning_rate LR
                        learning rate, default=1e-3
  -l2 L2, --l2_regularization L2
                        l2 regularization, default=1e-4
  -eps EPS, --epochs EPS
                        training times, default=100
  -nep NEP, --nepoch NEP
                        print the results every * times, default=20
```


## DeepSP example
```
python.py nikolovski2014.csv -f -r 2
```


