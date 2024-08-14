
#============================================
#2023/09/08
#============================================


#--------------------------------------------
#ファイルを読み込んでデータフレームを作成する
#1:ディレクトリにファイルを保存し、読み込み方法
DF <- read.table( 'https://raw.githubusercontent.com/harabou/Biostat_Kyoto_pref/main/data/%2305/TokyoSTAT_P25.csv', 
                  sep = ",", 
                  header = TRUE, 
                  stringsAsFactors = FALSE, 
                  fileEncoding="UTF-8")     #文字コードはUTF-8
##ディレクトリの確認法
getwd()

#2:GUIによる読み込み方法
#「Environment」-「import」から
#---------------------------------------------


#第5日目
#FA/PCA/CA

#ファイルを読み込んでデータフレームを作成する
#その１
DF <- read.table( "TokyoSTAT_P25.csv", 
                  sep = ",", 
                  header = TRUE, 
                  stringsAsFactors = FALSE, 
                  fileEncoding="UTF-8")     #文字コードはUTF-8
#その２
TokyoSTAT_P25 <- read.csv('https://raw.githubusercontent.com/harabou/Biostat_Kyoto_pref/main/data/%2305/TokyoSTAT_P25.csv')
DF<-TokyoSTAT_P25


#============================================================
####以後「DF」データセットを使用していく

#write.csv(DF,"DF.csv") #DF.csv, DF.savのファイルを残しておく
#============================================================

#データの概要を確認する
str(DF)
summary(DF[, -c(1:2)])　#1列,2列は削除して読み込んだ
DF　#データ確認


#=================================================
#ライブラリのインストール・読込み
##その1:install.packages("XXXXXX") library(XXXXX)
##その2:GUI「Packages」より
#=================================================

install.packages("psych")
library(psych)




#主成分分析
resultPCA <- prcomp(DF[, -(1:3)], scale=TRUE) #1-3列を除いて使用

#結果の要約
summary(resultPCA)

#各変数ごとの主成分（固有ベクトル）
#第3主成分まで表示
resultPCA$rotation[, 1:3]

#各ケースごとの主成分得点
#5自治体、第3主成分まで表示
resultPCA$x[1:5, 1:3]

#主成分得点(サンプルごとの点数)の値をタテヨコにプロットする
biplot(resultPCA)

#主成分得点をデータフレームに変換
DFpca <- as.data.frame(resultPCA$x)
#行の名前を自治体名に変換
rownames(DFpca) <- DF$市町村

#主成分得点について要約情報を表示
summary(DFpca)[, 1:2]
#標準偏差
apply(DFpca, 2, sd)[1:2]

#============================================================
#write.csv(DFpca,"DFpca.csv") #DFpca.csv, DFpca.savのファイルを残しておく
#============================================================

#fa.parallel：平行分析（PA）の実施（視覚的に適切な因子数を判断）
#fm＝因子抽出法（minres最小残差法、pa主因子法、ml最尤法）
#ここでは最尤法を選択
#1-2列目はIDなので除く
#3列目は次元圧縮の対象ではないので除く
result.prl <- fa.parallel(DF[, -(1:3)], fm="ml")  #plotを確認のこと



##FA（因子分析）の実行

#fa：因子分析の実行
#fm＝因子抽出法（minres最小残差法、pa主因子法、ml最尤法）
#ここでは最尤法を選択
#nfactors＝因子数（抽出したい軸の数）
#rotate＝回転法（直交回転:varimax等、斜交回転:promax等）
#scores＝因子得点算出法
#1-2列目はIDなので除く
#3列目は次元圧縮の対象ではないので除く

resultFA <- fa(DF[, -(1:3)],
               nfactors=3,             #因子数を指定
               fm = "ml",              #pa 主因子法, ols 最小二乗法, ml 最尤法
               rotate = "varimax",     #varimax 直交、promax 斜交
               scores = "regression")  #regression 回帰法

#結果の表示
#digits＝小数点以下表示桁の指定
#sort=TRUEを指定（各項目ごとの因子負荷量がソートされる）
print(resultFA, digits=2, sort=TRUE) 

#結果を図で表示
fa.diagram(resultFA, 
           rsize=0.9, e.size=3.0, #四角と円のサイズ
           marg=c(.9,9,.9,.9),    #余白の設定
           cex=.1)                #文字サイズ

#結果の見方
#MRi...：各項目ごとの因子負荷量（各変数がどれだけ因子に寄与しているか）
#h2：共通性--各変数の値の変動が因子でどれだけ説明できるかを表す
#u2＝1-h2：独自性（uniqueness）--取りこぼしの度合（救えなかった情報）

#因子負荷量(変数ごとの因子への寄与)の値をタテヨコにプロットする
#因子負荷量は、resultFAの中のloadingsに格納されている



#第1因子と第2因子
#枠のみを作成する
#type="n"で点を描かない
plot(resultFA$loadings[, 1],
     resultFA$loadings[, 2], type="n") 
#枠内にテキストを表示する
#   テキストは因子負荷量のリストの行の名前を取得して使う
text(resultFA$loadings[, 1],
     resultFA$loadings[, 2], 
     rownames(resultFA$loadings), col="steelblue")
#y=0の直線を引く
#   点(-1, 0)から点(1, 0)まで線を引けばよい
#   lines(X, Y)でXとYのベクトルを指定する（散布図と同じ）
lines(c(-1, 1), c(0, 0), col="grey")
#x=0の直線を引く
#   点(0, -1)から点(0, 1)まで線を引けばよい
lines(c(0, 0), c(-1, 1), col="grey")

#第3因子と第2因子についても同様
plot(resultFA$loadings[, 3],
     resultFA$loadings[, 2], type="n")
text(resultFA$loadings[, 3],
     resultFA$loadings[, 2], 
     rownames(resultFA$loadings), col="steelblue")
lines(c(-1, 1), c(0, 0), col="grey")
lines(c(0, 0), c(-1, 1), col="grey")






#因子得点(ケースごとの得点)はresultFAの中のscoresに格納されている
head(resultFA$scores)

#因子得点をデータフレームに変換
DFfa <- as.data.frame(resultFA$scores)
#行の名前を自治体名に変換
rownames(DFfa) <- DF$市町村

#意味を考えて因子に名前を付ける
names(DFfa) = c("ビジネス度","都会生活度","非高齢化度")
head(DFfa)

#因子得点について要約情報を表示
summary(DFfa)
#標準偏差
apply(DFfa, 2, sd)


#============================================================
#write.csv(DFfa,"fascore.csv")  #fascore.csv, fascore.savのファイルを残しておく
#============================================================


##因子分析結果を利用したクラスター分析

#==========================
#階層クラスター
#===========================
distance <- dist(DFfa) #ユークリッド距離を求める
# 樹形図作成
hc <- hclust(distance, "ward.D2")
plot(hc)
res <- cutree(hc, k =4)
write.csv(res,"resultDF.csv")

DFf <- cbind(DFfa, res)  #列どうしを結合

#=====================
#k-means法
#====================
kmDF <- kmeans(DFfa,4) 

result_km <- kmDF$cluster
write.csv(result_km,"result_km.csv")


#グラフ描画
library(cluster)
clusplot(DF, kmDF$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

##最終データセット
DF <- cbind(DF, result_km)  #列どうしを結合
write.csv(DF,"DF_all.csv")
#============================================================
#write.csv(DF,"DF_all.csv")  #DF_all.csv, DF_all.savのファイルを残しておく
#============================================================


#================================
#クラスターを使用した属性の比較
#================================

install.packages(tableone)
library(tableone)
all <- CreateTableOne(vars = c("人気度", "ビジネス度","都会生活度","非高齢化度"), strata="res",factorVars=c("res"),data = DF)

all




##----/以下　candy_bar DATA/-------------------------------------------------

#==================================
##クラスター分析
#==================================
# 非類似度（距離）を計算

candy_bars <- read_sav("D:/■京都府大/講義資料/■2023_R/##第5回/dataset/candy_bars.sav")


data <- candy_bars[,8:13]
data
distance <- dist(data) #ユークリッド距離を求める

# 樹形図作成
hc <- hclust(distance, "ward.D2")
plot(hc)
res <- cutree(hc, k =4)

##別
library(tidyverse)
hc %>% factoextra::fviz_dend(
  k=4,
  rect=TRUE, rect_fill=TRUE)
library(igraph)
hc %>% factoextra::fviz_dend(
  k=4,
  rect=TRUE, rect_fill=TRUE,
  type="phylogenic")

answer <- candy_bars[,2]
table <- table(answer, res)

#data出力
write.csv( table,"table.csv")
write.csv(res,"res.csv")


#==========================
##k-means
#==========================


km <-kmeans(data,4)
result2 <- km$cluster
result2
write.csv(result2,"result2.csv")

install.packages(cluster)
library(cluster)
clusplot(data, km$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


#クラスター数の検討
install.packages(factoextra)
library(factoextra)

f<-2:5 %>% map(function(k){
data %>% kmeans(k) %>%
fviz_cluster(data=data, geom="point")+ ggtitle(sprintf("k=%s",k))
})

gridExtra::grid.arrange(f[[1]],f[[2]],f[[3]],f[[4]],ncol=2)
fviz_nbclust(data,kmeans, method="wss")


#data出力 
table2 <- table(answer, result2)
write.csv(table2,"table2.csv")












