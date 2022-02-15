par(family="Zapfino")

# アブストの分析
#abst.path <- "selected_data.csv"  # 頻度
abst.path <- "selected_relative_data.csv"   # 相対頻度
colnum <- 44 # 変数の数
abst<-read.csv(abst.path, row.names=1)
abstp<-abst/rowSums(abst)
abst[is.na(abst)] <- 0
# 主成分分析
abstpp<-100*abst/apply(abst,1,sum)
col<-c(1:8, "saddlebrown", "rosybrown2")　           　
XX<-abstpp[, 1:colnum]
##主成分散布図
pca<-prcomp(XX)$x
plot(pca[, 1], pca[, 2], pch="", xlab="PCA1", ylab="PCA2")
text(pca[, 1], pca[, 2], rownames(XX), col=col)

##t-SNE法による視覚化
install.packages("Rtsne")
library(Rtsne)
set.seed(0)
# perplexityの値は1～4で実行可能。（デフォルトは30）
sne<-Rtsne(as.matrix(XX),perplexity=5)$Y
plot(sne[, 1], sne[, 2], xlab="SNE1", ylab="SNE2")
text(sne[, 1], sne[, 2], rownames(XX), col=col)

