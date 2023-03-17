library(pRolocdata)
library(pRoloc)
library(dplyr)

names <- c('nikolovski2014', 'E14TG2aR', 'itzhak2017', 'hirst2018',
           'LOPITDC2019', 'orre2019', 'mulvey2021')
for (name in names){
  dir.create(sprintf('../data/input_rawdata/%s', name))
}



# 2 ?????????, 20??????,?????????Sum=2
data(nikolovski2014)
name <- 'nikolovski2014'
data <- nikolovski2014
df <- data.frame(data@assayData[["exprs"]])
df$markers <- data@featureData@data[["markers"]]
df <- na.omit(df)
write.csv(df, sprintf('../data/input_rawdata/%s/%s.csv', name, name))


# 1 ?????????, 8??????, ?????????Sum=1
data(E14TG2aR)
name <- 'E14TG2aR'
data <- E14TG2aR
df <- data.frame(data@assayData[["exprs"]])
df$markers <- data@featureData@data[["markers"]]
df <- na.omit(df)
write.csv(df, sprintf('../data/input_rawdata/%s/%s.csv', name, name))


# 1 ?????????, 8??????,?????????Sum=1
data(itzhak2017)
name <- 'itzhak2017'
data <- itzhak2017
df <- data.frame(data@assayData[["exprs"]])
df$markers <- data@featureData@data[["markers"]]
df <- na.omit(df)
write.csv(df, sprintf('../data/input_rawdata/%s/%s.csv', name, name))



# 3 ?????????, 15?????????, ?????????Sum~1/3
data(hirst2018)
name <- 'hirst2018'
x <- split(hirst2018, "sample")
df <- data.frame(x@x[["CTRL"]]@assayData[["exprs"]])
df$markers <- x@x[["CTRL"]]@featureData@data[["markers"]]
df <- na.omit(df)
# df$GeneName <- x@x[["CTRL"]]@featureData@data[["Gene.names"]]
write.csv(df, sprintf('../data/input_rawdata/%s/%s.csv', name, name))


# 3?????????,30?????????,?????????Sum~3
data(lopitdcU2OS2018)
name <- 'LOPITDC2019'
data <- lopitdcU2OS2018
df <- data.frame(data@assayData[["exprs"]])
df$markers <- data@featureData@data[["markers"]]
df <- na.omit(df)
write.csv(df, sprintf('../data/input_rawdata/%s/%s.csv', name, name))


# 1 ?????????, 10??????,?????????Sum=1
data(orre2019a431)
name <- 'orre2019'
data <- orre2019a431
df <- data.frame(data@assayData[["exprs"]])
df$markers <- data@featureData@data[["markers"]]
df <- na.omit(df)
write.csv(df, sprintf('../data/input_rawdata/%s/%s.csv', name, name))


# Five different human cancer cell
# lines (epidermoid carcinoma A431, glioblastoma U251, breast
#        cancer MCF7, lung cancer NCI-H322, and lung cancer HCC-
#          827) were fractionated in biological duplicates, generating a total
# of 50 samples.




load('../data/datadownload/thpLOPIT_lps_rep1_mulvey2021.RData')
load('../data/datadownload/thpLOPIT_lps_rep2_mulvey2021.RData')
load('../data/datadownload/thpLOPIT_lps_rep3_mulvey2021.RData')

name <- 'mulvey2021'

# OR loaddata
data(thpLOPIT_lps_rep1_mulvey2021)
data(thpLOPIT_lps_rep2_mulvey2021)
data(thpLOPIT_lps_rep3_mulvey2021)

R1 <- data.frame(thpLOPIT_lps_rep1_mulvey2021@assayData[["exprs"]])
R2 <- data.frame(thpLOPIT_lps_rep2_mulvey2021@assayData[["exprs"]])
R3 <- data.frame(thpLOPIT_lps_rep3_mulvey2021@assayData[["exprs"]])

R1$ProteinID <- rownames(R1)
R2$ProteinID <- rownames(R2)
R3$ProteinID <- rownames(R3)
R3$markers <- thpLOPIT_lps_rep3_mulvey2021@featureData@data[["markers"]]



df <- inner_join(inner_join(R1, R2,  by='ProteinID'), R3, by='ProteinID')
rownames(df) <- df$ProteinID
df <- dplyr::select(df, -ProteinID)
df <- na.omit(df)
write.csv(df, sprintf('../data/input_rawdata/%s/%s.csv', name, name))







