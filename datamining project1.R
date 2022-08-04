data.df<- read.csv("E:/data.csv",header = TRUE)
dim(data.df)
c<-c()
for (i in 1:1048575) {
  if (data.df[i,39]>0|data.df[i,40]>0)
  {c[i]<-1}
  else{
    c[i]<-0}}
sample<-cbind(data.df,target=c)
dim(sample)
s<-sample(row.names(sample),5000)
nemoone<-sample[s,]
dim(nemoone)
nemoone<-nemoone[,-40]
nemoone<-nemoone[,-39]
nemoone<-nemoone[,-5]
nemoone<-nemoone[,-3]
nemoone<-nemoone[,-2]
nemoone<-nemoone[,-1]
names(nemoone)
refah.df.smp=nemoone[,-c(5,10,15,20)]
refah.fit=glm(target~.,refah.df.smp,family = "binomial")
summary(refah.fit)
refah.df.smp=nemoone[,-c(5,10,15,20,8,27,28,31)]
dim(refah.df.smp)
refah.fit=glm(target~.,refah.df.smp,family = "binomial")
summary(refah.fit)
refah.df.smp=nemoone[,-c(5,10,15,20,8,27,28,31,37)]
refah.fit=glm(target~.,refah.df.smp,family = "binomial")
summary(refah.fit)
set.seed(1)
#train.rows<-sample(rownames(refah.df.smp), dim(refah.df.smp)[1]*0.6)
s1<-sample(rownames(refah.df.smp[refah.df.smp$target==1,]), dim(refah.df.smp[refah.df.smp$target==1,])[1]*0.5)
s2=sample(rownames(refah.df.smp[refah.df.smp$target==0,]), dim(refah.df.smp[refah.df.smp$target==1,])[1]*0.5)
train.rows<-c(s1,s2)
s3<-setdiff(rownames(refah.df.smp[refah.df.smp$target==1,]),s1)
s4<-sample(setdiff(rownames(refah.df.smp[refah.df.smp$target==0,]),s2),2344)
valid.rows<-c(s3,s4)
train.df <- refah.df.smp[train.rows, ]                                                 
valid.df <- refah.df.smp[valid.rows, ]                                                 
s5<-sample(seq_len(nrow(valid.df)), size = floor(0.5*nrow(valid.df)))
valid.df <- valid.df[s5,]
test.df<-valid.df[-s5,]                                                 
library(ggplot2)
#1#target & GenderId
ggplot(data = refah.df.smp) + 
  geom_bar(mapping = aes(x = as.factor(`GenderId`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
#2#target & isurban
ggplot(data = refah.df.smp) + 
  geom_bar(mapping = aes(x = as.factor(`isurban`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
#3#target & AmCrdtr_95
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = AmCrdtr_95)) + xlab("target")+ylim(c(NA,2000000000))
#4#target & Amdbtr_95
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = Amdbtr_95)) + xlab("target")+ylim(c(NA,200000000))
#5#target & lstPrd_95
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = lstPrd_95)) + xlab("target")+ylim(c(NA,200000000))
#6#target & lstPrd_96
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = lstPrd_96)) + xlab("target")+ylim(c(NA,200000000))
#7#target & SmBnft_96
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = SmBnft_96)) + xlab("target")+ylim(c(NA,10000000))
#8#target & AmCrdtr_97
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = AmCrdtr_97)) + xlab("target")+ylim(c(NA,10000000))
#9#target & Amdbtr_97
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = Amdbtr_97)) + xlab("target")+ylim(c(NA,10000000))
#10#target & SmBnft_97
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = SmBnft_97)) + xlab("target")+ylim(c(NA,20000000))
#11#target & lstPrd_98
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = lstPrd_98)) + xlab("target")+ylim(c(NA,20000000))
#12#target & SmBnft_98
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = SmBnft_98)) + xlab("target")+ylim(c(NA,20000000))
#13#target & Card98_Rials
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = Card98_Rials)) + xlab("target")+ylim(c(NA,20000000))
#14#target & Card98_PaymentCount
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = Card98_PaymentCount)) + xlab("target")+ylim(c(NA,2000))
#15#target & Card99_PaymentCount
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = Card99PaymentCount)) + xlab("target")+ylim(c(NA,1000))
#16#target & IsBiamrKhas
#ggplot(data = refah.df.smp) + 
 # geom_bar(mapping = aes(x = as.factor(`IsBiamrKhas`), fill =as.factor(target)), position = "fill")+
 # theme(axis.text.x = element_text(angle = 90))+
  #theme(axis.text.x = element_text(size = 5))
#16#target & IsBimePardaz_Sandoghha
ggplot(data = refah.df.smp) + 
  geom_bar(mapping = aes(x = as.factor(`IsBimePardaz_Sandoghha`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
#17#target & IsBazneshaste_Sandoghha
ggplot(data = refah.df.smp) + 
  geom_bar(mapping = aes(x = as.factor(`HasBimeSalamat`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
#18#target & daramad_Total_Rials
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = daramad_Total_Rials)) + xlab("target")+ylim(c(NA,100000000))
#19#target & Cars_Count
ggplot(data = refah.df.smp) + 
  geom_bar(mapping = aes(x = as.factor(`Cars_Count`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
#20#target & CarsPrice_Sum
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = CarsPrice_Sum)) + xlab("target")+ylim(c(NA,15000000000))
#21#target & HasBimeSalamat
ggplot(data = refah.df.smp) + 
  geom_bar(mapping = aes(x = as.factor(`HasBimeSalamat`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
#22#target & Amdbtr_96
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = Amdbtr_96)) + xlab("target")+ylim(c(NA,400000000))
#23#target & lstPrd_97
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = lstPrd_97)) + xlab("target")+ylim(c(NA,100000000))
#24#target & AmCrdtr_98
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = AmCrdtr_98)) + xlab("target")+ylim(c(NA,50000000))
#25#target & Card99_Rials
ggplot(refah.df.smp) + geom_boxplot(aes(x = as.factor(target), y = Card99_Rials)) + xlab("target")+ylim(c(NA,1500000000))
#26#target & Trips_Count_AirPilgrimage
ggplot(data = refah.df.smp) + 
  geom_bar(mapping = aes(x = as.factor(`Trips_Count_AirPilgrimage`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
#27#target & Trips_Count_NotAirPilgrimage
ggplot(data = refah.df.smp) + 
  geom_bar(mapping = aes(x = as.factor(`Trips_Count_NotAirPilgrimage`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
#28#target & HasBimeSalamat
ggplot(data = refah.df.smp) + 
  geom_bar(mapping = aes(x = as.factor(`HasBimeSalamat`), fill =as.factor(target)), position = "fill")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(size = 5))
#heatmap
#heatmap
mydata=as.matrix(refah.df.smp[,1:30])
cormat <- round(cor(mydata),2)
library(reshape2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  theme(axis.text.x = element_text(angle = 90))
#models
library(caret)
#training tree
#grid search for best hyperparameters
parms<-c("information","gini")
maxdepth<-seq(1,30,1)
cp_float<-seq(0,45,1)
GridSearchDT<-expand.grid(parms = parms ,maxdepth=maxdepth ,cp=cp_float)
num_models<-nrow(GridSearchDT)
DecisionTree_GridSearch <- list()

for(i in 1:num_models){
  minsplit <- GridSearchDT$minsplit[i]
  maxdepth <- GridSearchDT$maxdepth[i]
  cp_float <- 1/((sqrt(2))^(GridSearchDT$cp[i]))
  DecisionTree_GridSearch[[i]]<-rpart(formula=as.factor(target)~., data=train.df ,
                                      method="class" ,control=rpart.control(
                                        maxdepth=maxdepth ,cp =cp_float ,parms=parms))
}

blA_val_dt<-c()
for(i in 1:num_models){
  a<-as.factor(predict(DecisionTree_GridSearch[[i]],newdata =valid.df ,type="class"))
  b<-as.factor(valid.df$target)
  blA_val_dt[i]<-as.numeric(confusionMatrix(a,b)$byClass)[11]
}

BestDecisionTree <- DecisionTree_GridSearch[[which.max(blA_val_dt)]]
modeltree <- BestDecisionTree

#training tree
predict.modeltree.train <- predict(modeltree, train.df , type= "class")
confusionMatrix(predict.modeltree.train, as.factor(train.df$target))
#validation tree
predict.modeltree.valid <- predict(modeltree, valid.df , type= "class")
confusionMatrix(predict.modeltree.valid, as.factor(valid.df$target))
#test tree
predict.modeltree.test <- predict(modeltree, test.df , type= "class")
confusionMatrix(predict.modeltree.test, as.factor(test.df$target))

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
 norm.values<-preProcess(train.df[,c(3:21,24,26)],method = c("center","scale"))
train.norm.df[,c(3:21,24,26)]<-predict(norm.values,train.df[,c(3:21,24,26)])
valid.norm.df<-valid.df
valid.norm.df[,c(3:21,24,26)]<-predict(norm.values,valid.df[,c(3:21,24,26)])
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
norm.values<-preProcess(train.df[,c(3:21,24,26)],method = c("center","scale"))
train.norm.df[,c(3:21,24,26)]<-predict(norm.values,train.df[,c(3:21,24,26)])
valid.norm.df<-valid.df.knn
test.norm.df<-test.df.knn
valid.norm.df[,c(3:21,24,26)]<-predict(norm.values,valid.df.knn[,c(3:21,24,26)])
test.norm.df[ ,c(3:21,24,26)]<-predict(norm.values,test.norm.df[,c(3:21,24,26)])
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
train.df.nn<-na.omit(train.norm.df)
valid.df.nn<-na.omit(valid.norm.df)


n <- names(train.df.nn[,-1])
f <- as.formula(paste("train.df.nn$target ~", paste(n[!n %in% "target"], collapse = " + ")))
nn<-neuralnet(f , data = train.df.nn[,-1],linear.output = F,hidden = 5)
prednn<- as.factor(ifelse(predict(nn , valid.df.nn[,-1], type="response")>0.43,1,0))
confusionMatrix(prednn,as.factor(valid.df.nn$target))
pred_train <- as.factor(ifelse(predict(nn , train.df.nn[,-1], type="response")>0.43,1,0))
confusionMatrix(pred_train  ,as.factor(train.df.nn$target        ) )
