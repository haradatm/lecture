#options(repos='http://cran.md.tsukuba.ac.jp')
#options(device='quartz')
#install.packages('rgl')
#install.packages('igraph')
#install.packages('ggplot2')
#Sys.setenv("http_proxy"="http://proxy.intellilink.co.jp:8080")
#install.packages('/Users/haradatm/Downloads/RMeCab_0.9995.tgz',repos=NULL)

rm(list=ls())

Sys.time()
args<-commandArgs()
(args)
path1<-args[6]
path2<-args[7]
path3<-args[8]
path4<-args[9]
path5<-args[10]

path1<-"data/rakuten-eval.txt"

#data<-read.table(pipe("pbpaste"),sep="\t",header=T)
data<-read.table(path1,header=T,sep='\t',row.names=NULL)
dim(data); names(data); 
head(data[,names(data)!="テキスト"])
summary(data[,names(data)!="テキスト"])

# 比率のプロット
t0<-prop.table(xtabs(~data$"総合" + data$"エリア"),margin=2)
t1<-prop.table(xtabs(~data$"サービス" + data$"エリア"),margin=2)
t2<-prop.table(xtabs(~data$"立地" + data$"エリア"),margin=2)
t3<-prop.table(xtabs(~data$"部屋" + data$"エリア"),margin=2)
t4<-prop.table(xtabs(~data$"設備.アメニティ" + data$"エリア")[c(2:6),],margin=2) 
t5<-prop.table(xtabs(~data$"風呂" + data$"エリア")[c(2:6),],margin=2)
t6<-prop.table(xtabs(~data$"食事" + data$"エリア")[c(2:6),],margin=2)

quartz(width=12,height=8,type="pdf",file="plot.pdf")
par(family="serif",mfrow=c(3,3))
barplot(t1,col=heat.colors(nrow(t1)),legend.text=rownames(t1),main="サービス")
barplot(t2,col=heat.colors(nrow(t2)),legend.text=rownames(t2),main="立地")
barplot(t3,col=heat.colors(nrow(t3)),legend.text=rownames(t3),main="部屋")
barplot(t4,col=heat.colors(nrow(t4)),legend.text=rownames(t4),main="設備・アメニティ")
barplot(t5,col=heat.colors(nrow(t5)),legend.text=rownames(t5),main="風呂")
barplot(t6,col=heat.colors(nrow(t6)),legend.text=rownames(t6),main="食事")
barplot(t0,col=heat.colors(nrow(t0)),legend.text=rownames(t0),main="総合")
dev.off()

# 検定
t0<-xtabs(~data$"総合" + data$"エリア")
t1<-xtabs(~data$"サービス" + data$"エリア")
t2<-xtabs(~data$"立地" + data$"エリア")
t3<-xtabs(~data$"部屋" + data$"エリア")
t4<-xtabs(~data$"設備.アメニティ" + data$"エリア")[c(2:6),] 
t5<-xtabs(~data$"風呂" + data$"エリア")[c(2:6),]
t6<-xtabs(~data$"食事" + data$"エリア")[c(2:6),]

chisq_p<-matrix(nrow=4,ncol=3)
rownames(chisq_p)<-c("総合","風呂","食事","サービス")
colnames(chisq_p)<-c("外房-西伊豆","西伊豆-南房総","南房総-外房")
chisq_p["総合","外房-西伊豆"]<-1

# 総合 (t0)
t12<-t0[,c("01-外房","02-西伊豆")　]; t12.chisq<-chisq.test(t12); t12.prop<-prop.test(t12)
t23<-t0[,c("02-西伊豆","03-南房総")]; t23.chisq<-chisq.test(t23); t23.prop<-prop.test(t23)
t13<-t0[,c("01-外房","03-南房総")　]; t13.chisq<-chisq.test(t13); t13.prop<-prop.test(t13)
t12; t12.chisq; t12.prop; chisq_p["総合","外房-西伊豆"　]<-t12.chisq$p.value
t23; t23.chisq; t23.prop; chisq_p["総合","西伊豆-南房総"]<-t23.chisq$p.value
t13; t13.chisq; t13.prop; chisq_p["総合","南房総-外房"　]<-t13.chisq$p.value

# 風呂 (t5)
t12<-t5[,c("01-外房","02-西伊豆")　]; t12.chisq<-chisq.test(t12); t12.prop<-prop.test(t12)
t23<-t5[,c("02-西伊豆","03-南房総")]; t23.chisq<-chisq.test(t23); t23.prop<-prop.test(t23)
t13<-t5[,c("01-外房","03-南房総"　)]; t13.chisq<-chisq.test(t13); t13.prop<-prop.test(t13)
t12; t12.chisq; t12.prop; chisq_p["風呂","外房-西伊豆"　]<-t12.chisq$p.value
t23; t23.chisq; t23.prop; chisq_p["風呂","西伊豆-南房総"]<-t23.chisq$p.value
t13; t13.chisq; t13.prop; chisq_p["風呂","南房総-外房"　]<-t13.chisq$p.value

# 食事 (t6)
t12<-t6[,c("01-外房","02-西伊豆")　]; t12.chisq<-chisq.test(t12); t12.prop<-prop.test(t12)
t23<-t6[,c("02-西伊豆","03-南房総")]; t23.chisq<-chisq.test(t23); t23.prop<-prop.test(t23)
t13<-t6[,c("01-外房","03-南房総")　]; t13.chisq<-chisq.test(t13); t13.prop<-prop.test(t13)
t12; t12.chisq; t12.prop; chisq_p["食事","外房-西伊豆"　]<-t12.chisq$p.value
t23; t23.chisq; t23.prop; chisq_p["食事","西伊豆-南房総"]<-t23.chisq$p.value
t13; t13.chisq; t13.prop; chisq_p["食事","南房総-外房"　]<-t13.chisq$p.value

# サービス (t1)
t12<-t1[,c("01-外房","02-西伊豆")　]; t12.chisq<-chisq.test(t12); t12.prop<-prop.test(t12)
t23<-t1[,c("02-西伊豆","03-南房総")]; t23.chisq<-chisq.test(t23); t23.prop<-prop.test(t23)
t13<-t1[,c("01-外房","03-南房総")　]; t13.chisq<-chisq.test(t13); t13.prop<-prop.test(t13)
t12; t12.chisq; t12.prop; chisq_p["サービス","外房-西伊豆"　]<-t12.chisq$p.value
t23; t23.chisq; t23.prop; chisq_p["サービス","西伊豆-南房総"]<-t23.chisq$p.value
t13; t13.chisq; t13.prop; chisq_p["サービス","南房総-外房"　]<-t13.chisq$p.value
print(chisq_p)
