
#============================================
#2023/09/08
#============================================


#--------------------------------------------
#�t�@�C����ǂݍ���Ńf�[�^�t���[�����쐬����
#1:�f�B���N�g���Ƀt�@�C����ۑ����A�ǂݍ��ݕ��@
DF <- read.table( "TokyoSTAT_P25.csv", 
                  sep = ",", 
                  header = TRUE, 
                  stringsAsFactors = FALSE, 
                  fileEncoding="UTF-8")     #�����R�[�h��UTF-8
##�f�B���N�g���̊m�F�@
getwd()

#2:GUI�ɂ��ǂݍ��ݕ��@
#�uEnvironment�v-�uimport�v����
#---------------------------------------------


#��5����
#FA/PCA/CA

#�t�@�C����ǂݍ���Ńf�[�^�t���[�����쐬����
#���̂P
DF <- read.table( "TokyoSTAT_P25.csv", 
                  sep = ",", 
                  header = TRUE, 
                  stringsAsFactors = FALSE, 
                  fileEncoding="UTF-8")     #�����R�[�h��UTF-8
#���̂Q
TokyoSTAT_P25 <- read.csv("C:/Users/biostat_35/Desktop/sample20191122 (1)/Ch04/TokyoSTAT_P25.csv")
DF<-TokyoSTAT_P25


#============================================================
####�Ȍ�uDF�v�f�[�^�Z�b�g���g�p���Ă���

#write.csv(DF,"DF.csv") #DF.csv, DF.sav�̃t�@�C�����c���Ă���
#============================================================

#�f�[�^�̊T�v���m�F����
str(DF)
summary(DF[, -c(1:2)])�@#1��,2��͍폜���ēǂݍ���
DF�@#�f�[�^�m�F


#=================================================
#���C�u�����̃C���X�g�[���E�Ǎ���
##����1:install.packages("XXXXXX") library(XXXXX)
##����2:GUI�uPackages�v���
#=================================================

install.packages("psych")
library(psych)




#�听������
resultPCA <- prcomp(DF[, -(1:3)], scale=TRUE) #1-3��������Ďg�p

#���ʂ̗v��
summary(resultPCA)

#�e�ϐ����Ƃ̎听���i�ŗL�x�N�g���j
#��3�听���܂ŕ\��
resultPCA$rotation[, 1:3]

#�e�P�[�X���Ƃ̎听�����_
#5�����́A��3�听���܂ŕ\��
resultPCA$x[1:5, 1:3]

#�听�����_(�T���v�����Ƃ̓_��)�̒l���^�e���R�Ƀv���b�g����
biplot(resultPCA)

#�听�����_���f�[�^�t���[���ɕϊ�
DFpca <- as.data.frame(resultPCA$x)
#�s�̖��O�������̖��ɕϊ�
rownames(DFpca) <- DF$�s����

#�听�����_�ɂ��ėv�����\��
summary(DFpca)[, 1:2]
#�W���΍�
apply(DFpca, 2, sd)[1:2]

#============================================================
#write.csv(DFpca,"DFpca.csv") #DFpca.csv, DFpca.sav�̃t�@�C�����c���Ă���
#============================================================

#fa.parallel�F���s���́iPA�j�̎��{�i���o�I�ɓK�؂Ȉ��q���𔻒f�j
#fm�����q���o�@�iminres�ŏ��c���@�Apa����q�@�Aml�Ŗޖ@�j
#�����ł͍Ŗޖ@��I��
#1-2��ڂ�ID�Ȃ̂ŏ���
#3��ڂ͎������k�̑Ώۂł͂Ȃ��̂ŏ���
result.prl <- fa.parallel(DF[, -(1:3)], fm="ml")  #plot���m�F�̂���



##FA�i���q���́j�̎��s

#fa�F���q���͂̎��s
#fm�����q���o�@�iminres�ŏ��c���@�Apa����q�@�Aml�Ŗޖ@�j
#�����ł͍Ŗޖ@��I��
#nfactors�����q���i���o���������̐��j
#rotate����]�@�i������]:varimax���A�Ό���]:promax���j
#scores�����q���_�Z�o�@
#1-2��ڂ�ID�Ȃ̂ŏ���
#3��ڂ͎������k�̑Ώۂł͂Ȃ��̂ŏ���

resultFA <- fa(DF[, -(1:3)],
               nfactors=3,             #���q�����w��
               fm = "ml",              #pa ����q�@, ols �ŏ����@, ml �Ŗޖ@
               rotate = "varimax",     #varimax �����Apromax �Ό�
               scores = "regression")  #regression ��A�@

#���ʂ̕\��
#digits�������_�ȉ��\�����̎w��
#sort=TRUE���w��i�e���ڂ��Ƃ̈��q���חʂ��\�[�g�����j
print(resultFA, digits=2, sort=TRUE) 

#���ʂ�}�ŕ\��
fa.diagram(resultFA, 
           rsize=0.9, e.size=3.0, #�l�p�Ɖ~�̃T�C�Y
           marg=c(.9,9,.9,.9),    #�]���̐ݒ�
           cex=.1)                #�����T�C�Y

#���ʂ̌���
#MRi...�F�e���ڂ��Ƃ̈��q���חʁi�e�ϐ����ǂꂾ�����q�Ɋ�^���Ă��邩�j
#h2�F���ʐ�--�e�ϐ��̒l�̕ϓ������q�łǂꂾ�������ł��邩��\��
#u2��1-h2�F�Ǝ����iuniqueness�j--��肱�ڂ��̓x���i�~���Ȃ��������j

#���q���ח�(�ϐ����Ƃ̈��q�ւ̊�^)�̒l���^�e���R�Ƀv���b�g����
#���q���חʂ́AresultFA�̒���loadings�Ɋi�[����Ă���



#��1���q�Ƒ�2���q
#�g�݂̂��쐬����
#type="n"�œ_��`���Ȃ�
plot(resultFA$loadings[, 1],
     resultFA$loadings[, 2], type="n") 
#�g���Ƀe�L�X�g��\������
#   �e�L�X�g�͈��q���חʂ̃��X�g�̍s�̖��O���擾���Ďg��
text(resultFA$loadings[, 1],
     resultFA$loadings[, 2], 
     rownames(resultFA$loadings), col="steelblue")
#y=0�̒���������
#   �_(-1, 0)����_(1, 0)�܂Ő��������΂悢
#   lines(X, Y)��X��Y�̃x�N�g�����w�肷��i�U�z�}�Ɠ����j
lines(c(-1, 1), c(0, 0), col="grey")
#x=0�̒���������
#   �_(0, -1)����_(0, 1)�܂Ő��������΂悢
lines(c(0, 0), c(-1, 1), col="grey")

#��3���q�Ƒ�2���q�ɂ��Ă����l
plot(resultFA$loadings[, 3],
     resultFA$loadings[, 2], type="n")
text(resultFA$loadings[, 3],
     resultFA$loadings[, 2], 
     rownames(resultFA$loadings), col="steelblue")
lines(c(-1, 1), c(0, 0), col="grey")
lines(c(0, 0), c(-1, 1), col="grey")






#���q���_(�P�[�X���Ƃ̓��_)��resultFA�̒���scores�Ɋi�[����Ă���
head(resultFA$scores)

#���q���_���f�[�^�t���[���ɕϊ�
DFfa <- as.data.frame(resultFA$scores)
#�s�̖��O�������̖��ɕϊ�
rownames(DFfa) <- DF$�s����

#�Ӗ����l���Ĉ��q�ɖ��O��t����
names(DFfa) = c("�r�W�l�X�x","�s����x","�񍂗�x")
head(DFfa)

#���q���_�ɂ��ėv�����\��
summary(DFfa)
#�W���΍�
apply(DFfa, 2, sd)


#============================================================
#write.csv(DFfa,"fascore.csv")  #fascore.csv, fascore.sav�̃t�@�C�����c���Ă���
#============================================================


##���q���͌��ʂ𗘗p�����N���X�^�[����

#==========================
#�K�w�N���X�^�[
#===========================
distance <- dist(DFfa) #���[�N���b�h���������߂�
# ���`�}�쐬
hc <- hclust(distance, "ward.D2")
plot(hc)
res <- cutree(hc, k =4)
write.csv(res,"resultDF.csv")

DFf <- cbind(DFfa, res)  #��ǂ���������

#=====================
#k-means�@
#====================
kmDF <- kmeans(DFfa,4) 

result_km <- kmDF$cluster
write.csv(result_km,"result_km.csv")


#�O���t�`��
library(cluster)
clusplot(DF, kmDF$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

##�ŏI�f�[�^�Z�b�g
DF <- cbind(DF, result_km)  #��ǂ���������
write.csv(DF,"DF_all.csv")
#============================================================
#write.csv(DF,"DF_all.csv")  #DF_all.csv, DF_all.sav�̃t�@�C�����c���Ă���
#============================================================


#================================
#�N���X�^�[���g�p���������̔�r
#================================

install.packages(tableone)
library(tableone)
all <- CreateTableOne(vars = c("�l�C�x", "�r�W�l�X�x","�s����x","�񍂗�x"), strata="res",factorVars=c("res"),data = DF)

all




##----/�ȉ��@candy_bar DATA/-------------------------------------------------

#==================================
##�N���X�^�[����
#==================================
# ��ގ��x�i�����j���v�Z

candy_bars <- read_sav("D:/�����s�{��/�u�`����/��2023_R/##��5��/dataset/candy_bars.sav")


data <- candy_bars[,8:13]
data
distance <- dist(data) #���[�N���b�h���������߂�

# ���`�}�쐬
hc <- hclust(distance, "ward.D2")
plot(hc)
res <- cutree(hc, k =4)

##��
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

#data�o��
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


#�N���X�^�[���̌���
install.packages(factoextra)
library(factoextra)

f<-2:5 %>% map(function(k){
data %>% kmeans(k) %>%
fviz_cluster(data=data, geom="point")+ ggtitle(sprintf("k=%s",k))
})

gridExtra::grid.arrange(f[[1]],f[[2]],f[[3]],f[[4]],ncol=2)
fviz_nbclust(data,kmeans, method="wss")


#data�o�� 
table2 <- table(answer, result2)
write.csv(table2,"table2.csv")











