options(repos='http://cran.md.tsukuba.ac.jp')
options(repos='http://R.research.att.com/')
#options(device='quartz')
#install.packages('rgl')
#install.packages('igraph')
#install.packages('ggplot2')
#Sys.setenv("http_proxy"="http://proxy:8080")
#install.packages('/Users/haradatm/Downloads/RMeCab_0.9995.tgz',repos=NULL)

# For Jupyter notebook
#install.packages(c('repr', 'IRdisplay', 'evaluate', 'crayon', 'pbdZMQ', 'devtools', 'uuid', 'digest', 'htmltools'))
#devtools::install_github('IRkernel/IRkernel')
#IRkernel::installspec()

Sys.setenv("TZ" = "Asia/Tokyo")

rm(list=ls())

Sys.time()
args<-commandArgs()
(args)
path1<-args[6]
path2<-args[7]
path3<-args[8]
path4<-args[9]
path5<-args[10]

#x<-read.table(pipe("pbpaste"))

getwd()
#setwd("./work")

### 0.「文書-抽出語」表を読み込む

d <- NULL
d <- read.table("export.csv", header=T, sep=",")

d[d$'h5'<=5000, 1] <- "A_レジャー"
d[d$'h5'>=5001 & d$'h5'<=10000, 1] <- "B_ビジネス"
d[d$'h5'<=1000, 2] <- "01_登別"
d[d$'h5'>=1001 & d$'h5'<= 2000, 2] <- "02_草津"
d[d$'h5'>=2001 & d$'h5'<= 3000, 2] <- "03_箱根"
d[d$'h5'>=3001 & d$'h5'<= 4000, 2] <- "04_道後"
d[d$'h5'>=4001 & d$'h5'<= 5000, 2] <- "05_湯布院"
d[d$'h5'>=5001 & d$'h5'<= 6000, 2] <- "06_札幌"
d[d$'h5'>=6001 & d$'h5'<= 7000, 2] <- "07_名古屋"
d[d$'h5'>=7001 & d$'h5'<= 8000, 2] <- "08_東京"
d[d$'h5'>=8001 & d$'h5'<= 9000, 2] <- "09_大阪"
d[d$'h5'>=9001 & d$'h5'<=10000, 2] <- "10_福岡"

d<-d[,c(1:2,11:ncol(d))]
names(d)[1] <- "カテゴリー"
names(d)[2] <- "エリア" 
d[1:10,1:10]

### 1. 多次元尺度構成法

# (a)「抽出語-文書」表 に転置する

d.t <- t(d[,3:ncol(d)])
d.t[1:10,1:10]

# (b) 距離を計算する (ユークリッド距離)

ed <- dist(d.t, method="binary")

# (c) 多次元尺度構成法で2次元プロットする

library("MASS")
mds <- isoMDS(ed, maxit=3000, tol=0.000001, trace=F)$points
par(family = "HiraKakuProN-W3")
plot(mds, col="red", pch=16)
text(mds, rownames(mds))

### 2. 対応分析

# (a)「抽出語-文書」表 を確認する

d[1:10,1:10]

# (b) 対応分析で2次元プロットする

d0 <- aggregate(d[,3:ncol(d)], by=list(name=d$"カテゴリー"), sum)
row.names(d0) <- d0$name; d0$name <- NULL
d1 <- aggregate(d[,3:ncol(d)], by=list(name=d$"エリア"), sum)
row.names(d1) <- d1$name; d1$name <- NULL
table.N <- d0;
table.N <- rbind(table.N, d1)

# Observed counts
table.N[1:10,1:10]

library(MASS)
d_max <- min(nrow(table.N), ncol(table.N)) - 1
c <- corresp(table.N, nf=d_max)
par(family = "HiraKakuProN-W3")
plot(c)

# (c) イナーシャ(慣性)を確認する
k <- c$cor^2
inertias <- round(k,4)  # イナーシャ(慣性)
k <- round(100*k / sum(k),2)
inertias
k
