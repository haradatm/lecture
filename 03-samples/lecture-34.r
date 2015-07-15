# RMeCab のインストール
install.packages ("RMeCab", repos = "http://rmecab.jp/R")
library(RMeCab)

# ファイルを読み込む
path<-"rakuten-eval-utf8.txt"
data<-read.table(path,header=T,sep='\t',row.names=NULL)

# テキストをエリアごとに結合する
text1 = ""
text2 = ""
text3 = ""
for (i in 1:nrow(data)) {
    if (data[i,"エリア"] == "01-外房") text1<-paste(text1,data[i,"テキスト"],sep="\n")
    else if (data[i,"エリア"] == "02-西伊豆") text2<-paste(text2,data[i,"テキスト"],sep="\n")
    else if (data[i,"エリア"] == "03-南房総") text3<-paste(text3,data[i,"テキスト"],sep="\n")
}
text<-rbind(
    data.frame(area="01", text=text1),
    data.frame(area="02", text=text2),
    data.frame(area="03", text=text3)
)

# テキスト列を形態素解析する (エリアごと)
result<-docMatrixDF(text[,2],pos=c("名詞","形容詞","動詞"))
colnames(result)[1]<-"01-外房"
colnames(result)[2]<-"02-西伊豆"
colnames(result)[3]<-"03-南房総"

# 行方向に集計した重みで,データ降順にソートする 
result<-as.data.frame(result[order(apply(result,1,sum),decreasing=T),])

# 行列を転置する (単語は上位150件残す)
t<-t(result)[,1:150]

# 主成分分析
pc<-prcomp(t,scale=TRUE)

# 対応分析
library(MASS)
ca<-corresp(t,n=2)

# プロット
par(family="serif")
biplot(pc, main="因子分析")
biplot(ca, main="主成分分析")
