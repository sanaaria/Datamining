data.df<- read.csv("E:/Data5.csv",header = TRUE)
data.df<-data.df[,-1]
quantile(data.df$Target,probs=seq(0,1,1/10))
c<-c()
for (i in 1:2081) {
  if (data.df[i,68]>75463333)
  {c[i]=1}
  else{
    c[i]=0}}
data<-cbind(data.df,target=c)

df=data[,-c(68)]

df.fit=glm(target~.,df,family = "binomial")
summary(df.fit)
df=data[,-c(68,11,12,16,17,20,22,23,26,30,34,37,44,45,46,66,2,3,4)]
df.fit=glm(target~.,df,family = "binomial")
summary(df.fit)
#heatmap
#library("lattice")
#df$Z <- runif(400, 0, 5)
#levelplot(Z~50*50, data=df  ,xlab="X"         main="")
#heatmap
mydata=as.matrix(df[, 20:50])
cormat <- round(cor(mydata),2)
library(reshape2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  theme(axis.text.x = element_text(angle = 90))
df=data[,-c(68,11,12,16,17,20,22,23,26,30,34,37,44,42,45,46,66,2,3,4,53,54)]
#oversampling
set.seed(1)
s1<-sample(rownames(df[df$target==1,]), dim(df[df$target==1,])[1]*0.5)
s2=sample(rownames(df[df$target==0,]), dim(df[df$target==1,])[1]*0.5)
train.rows<-c(s1,s2)
s3<-setdiff(rownames(df[df$target==1,]),s1)
s4<-sample(setdiff(rownames(df[df$target==0,]),s2),945)
valid.rows<-c(s3,s4)
train.df <- df[train.rows, ]                                                 
valid.df <- df[valid.rows, ]                                                 
#nemoodars
library(ggplot2)
#1#target & ostan
ggplot(data = df) + 
  geom_bar(mapping = aes(x = `Ostan`, fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#2#target & TedadAza
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`TedadAza`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))
  theme(axis.text.x = element_text(size = 10))
#3#target & SarparstJensiat
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`SarparstJensiat`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#4#target & SarparstSen
ggplot(df) + geom_boxplot(aes(x = as.factor(target), y = SarparstSen)) + xlab("target")+ylim(c(NA,100))
#5#target & SarparstTahsil
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`SarparstTahsil`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#6#target & SarparstSavad
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`SarparstSavad`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#7#target & SarparstMadrak
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`SarparstMadrak`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#8#target & NahveTasarof
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`NahveTasarof`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#9#target & TedadOtagh
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`TedadOtagh`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#10#target & SatheZirbana
ggplot(df) + geom_boxplot(aes(x = as.factor(target), y = SatheZirbana)) + xlab("target")+ylim(c(NA,150))
#11#target & motor
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`motor`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#12#target & docharkhe
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`docharkhe`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#13#target & radiozabt
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`radiozabt`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#14#target & video
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`video`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#15#target & computer
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`computer`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#16#target & freezer
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`freezer`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#17#target & yakhchal
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`yakhchal`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#18#target & yakhchalfreezer
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`yakhchalfreezer`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#19#target & jarobarghi
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`jarobarghi`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#20#target & lebasshoyi
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`lebasshoyi`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#21#target & khayati
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`khayati`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#22#target & coolerabimoteharek
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`coolerabimoteharek`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#23#target & coolergazimoteharek
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`coolergazimoteharek`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#24#target & microfer
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`microfer`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#25#target & hichkodam
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`hichkodam`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#26#target & lolekeshiab
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`lolekeshiab`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#27#target & bargh
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`bargh`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#28#target & telephone
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`telephone`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#29#target & coolerabisabet
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`coolerabisabet`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#30#target & borodatmarkazi
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`borodatmarkazi`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#31#target & hararatmarkazi
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`hararatmarkazi`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#32#target & package
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`package`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#33#target & coolergazisabet
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`coolergazisabet`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#34#target & fazelabshahri
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`fazelabshahri`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#35#target & nsokhtabgarm
ggplot(data = df) + 
  geom_bar(mapping = aes(x =as.factor(`nsokhtabgarm`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 10))
#36#target & HazineKhoraki
ggplot(df) + geom_boxplot(aes(x = as.factor(target), y = HazineKhoraki)) + xlab("target")+ylim(c(NA,30000000))
#37#target & HazineNoshidani
ggplot(df) + geom_boxplot(aes(x = as.factor(target), y = HazineNoshidani)) + xlab("target")+ylim(c(NA,300000))
#38#target & HazinePoshak
ggplot(df) + geom_boxplot(aes(x = as.factor(target), y = HazinePoshak)) + xlab("target")+ylim(c(NA,2000000))
#39#target & HazineMaskan
ggplot(df) + geom_boxplot(aes(x = as.factor(target), y = HazineMaskan)) + xlab("target")+ylim(c(NA,9990000))
#40#target & HazineLavazemKhanegi
ggplot(df) + geom_boxplot(aes(x = as.factor(target), y = HazineLavazemKhanegi)) + xlab("target")+ylim(c(NA,900000))
#41#target & HazineDarmani
ggplot(df) + geom_boxplot(aes(x = as.factor(target), y = HazineDarmani)) + xlab("target")+ylim(c(NA,900000))
#42#target & HazineHamlonaghl
ggplot(df) + geom_boxplot(aes(x = as.factor(target), y = HazineHamlonaghl)) + xlab("target")+ylim(c(NA,900000))
#43#target & HazineErtebatat
ggplot(df) + geom_boxplot(aes(x = as.factor(target), y = HazineErtebatat)) + xlab("target")+ylim(c(NA,900000))
#44#target & HazineTafrihatFarhangi
ggplot(df) + geom_boxplot(aes(x = as.factor(target), y = HazineTafrihatFarhangi)) + xlab("target")+ylim(c(NA,500000))
#45#target & HazineGhazaAmade
ggplot(df) + geom_boxplot(aes(x = as.factor(target), y = HazineGhazaAmade)) + xlab("target")+ylim(c(NA,5000000))
#45#target & HazineKalaBadavam
ggplot(df) + geom_boxplot(aes(x = as.factor(target), y = HazineKalaBadavam)) + xlab("target")+ylim(c(NA,50000000))

df=data[,-c(68,11,12,16,17,20,22,23,26,30,34,37,44,42,45,46,66,2,3,4,53,54,26,39,40,41,48)]
#oversampling
set.seed(1)
s1<-sample(rownames(df[df$target==1,]), dim(df[df$target==1,])[1]*0.5)
s2=sample(rownames(df[df$target==0,]), dim(df[df$target==1,])[1]*0.5)
train.rows<-c(s1,s2)
s3<-setdiff(rownames(df[df$target==1,]),s1)
s4<-sample(setdiff(rownames(df[df$target==0,]),s2),945)
valid.rows<-c(s3,s4)
train.df <- df[train.rows, ]                                                 
valid.df <- df[valid.rows, ]
#models
library(rpart)
library(rpart.plot)
library(caret)
#desion tree model
#training tree
predict.modeltree.train <- predict(modeltree, train.df , type= "class")
confusionMatrix(predict.modeltree.train, as.factor(train.df$target))
#validation tree
predict.modeltree.valid <- predict(modeltree, valid.df , type= "class")
confusionMatrix(predict.modeltree.valid,as.factor(valid.df $target))
summary(modeltree)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
plot(modeltree)
text(modeltree)
tot_count <- function(x, labs, digits, varlen)
{paste(labs, "\n\nn =", x$frame$n)}

prp(modeltree,faclen = 0, cex = 0.8, node.fun=tot_count,
    extra=106, 
    nn=FALSE, 
    fallen.leaves=TRUE, 
    branch=.5, 
    trace=1, 
    shadow.col="gray", 
    branch.lty=3, 
    split.cex=1.5,
    split.box.col="lightblue", 
    split.border.col="darkblue", 
    split.round=.2)
#logre
train.norm.df<-train.df
norm.values<-preProcess(train.df[,32:42],method = c("center","scale"))
train.norm.df[ ,32:42]<-predict(norm.values,train.df[,32:42])
valid.norm.df<-valid.df
valid.norm.df[ ,32:42]<-predict(norm.values,valid.df[,32:42])
logit.reg <- glm( target~ .,data = train.norm.df, family = "binomial")
summary(logit.reg)
logit.reg.pred.train <- predict(logit.reg,train.norm.df,type="response")
confusionMatrix(as.factor(ifelse(logit.reg.pred.train>0.5,1,0)),as.factor(train.norm.df$target))
logit.reg.pred.valid <- predict(logit.reg,valid.norm.df,type="response")
confusionMatrix(as.factor(ifelse(logit.reg.pred.valid>0.5,1,0)),as.factor(valid.norm.df$target))

#Knn
library(FNN)
s<-sample(seq_len(nrow(valid.df)), size = floor(0.5*nrow(valid.df)))
valid.df.knn <- valid.df[s,]
test.df.knn<-valid.df[-s,]
train.norm.df<-train.df
norm.values<-preProcess(train.df[,31:41],method = c("center","scale"))
train.norm.df[ ,31:41]<-predict(norm.values,train.df[,31:41])
valid.norm.df<-valid.df.knn
test.norm.df<-test.df.knn
valid.norm.df[ ,31:41]<-predict(norm.values,valid.df.knn[,31:41])
test.norm.df[ ,31:41]<-predict(norm.values,test.norm.df[,31:41])
View(train.norm.df)
train.norm.df=na.omit(train.norm.df)
valid.norm.df=na.omit(valid.norm.df)
test.norm.df=na.omit(test.norm.df)
train.norm.df=train.norm.df[ ,-1]
valid.norm.df=valid.norm.df[ ,-1]
test.norm.df=test.norm.df[ ,-1]
for (i in 1:10){
  k_n_n<-knn(train = train.norm.df , test = valid.norm.df , cl=as.factor(train.norm.df$target) , k=i)
  print(confusionMatrix(as.factor(k_n_n),as.factor(valid.norm.df$target)))
  print(i)}
k_n_n<-knn(train = train.norm.df , test = test.norm.df , cl=as.factor(train.norm.df$target) , k=5)
print(confusionMatrix(as.factor(k_n_n), as.factor(test.norm.df$target)))
#nn
library(neuralnet)
library(neuralnet)
train.df.nn<-na.omit(train.df)
valid.df.nn<-na.omit(valid.df)


n <- names(train.df[,-1])
f <- as.formula(paste("train.df.nn$target ~", paste(n[!n %in% "target"], collapse = " + ")))
nn<-neuralnet(f , data = train.df.nn[,-1],linear.output = F,hidden = 5)
prednn<- as.factor(ifelse(predict(nn , valid.df.nn[,-1], type="response")>0.43,1,0))
confusionMatrix(prednn,as.factor(valid.df.nn$target))
pred_train <- as.factor(ifelse(predict(nn , train.df.nn[,-1], type="response")>0.43,1,0))
confusionMatrix(pred_train  ,as.factor(train.df.nn$target        ) )
