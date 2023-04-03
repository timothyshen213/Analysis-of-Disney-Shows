data<-read.csv("Disney.csv")
factor_certificate = factor(data$certificate)
library(forcats)
certificate_group = matrix(fct_collapse(factor_certificate, Suitable_for_all = c("G"), Suitable_for_kids = c("TV-Y", "TV-G", "TV-Y7", "TV-Y7-FV", "TV-PG", "6+", "PG"), Suitable_for_teens = c("PG-13", "TV-14"),
                                        Suitable_for_adults = c("R", "TV-MA"), Not_rated = c("No data", "Not Rated", "Unrated", "Approved", "Passed")))
data$certificate = factor(certificate_group)
factor_genre = factor(data$genre)
library(stringr)
data$genre = matrix((str_extract(data$genre, "[aA-zZ]+")))
data$genre = relevel(factor(data$genre), ref = "No")
data$good_rating = as.factor(ifelse(data$rating > 7, "Good", "Bad"))
training.data = data[1:6849,]
test.data = data[6850:7850,]
confusion_matrix = function(pred, actual) {
  table(pred, actual, dnn = c("Predicted Rating", "Actual Rating"))
}

thresholds <- seq(0.01, 0.99, 0.005)
TPR <- numeric(length(thresholds))
FPR <- numeric(length(thresholds))
attach(data)

# Descriptive Analysis
library(ggplot2)
require(GGally)
aldat=disney[-c(1,2,5,8)]
ggpairs(data=aldat,cardinality_threshold=17)
summary(as.numeric(df2$Runtime_min))
table(df2$Genre)
table(df2$Certificate)
hist(as.numeric(df2$Runtime_min), main = "Histogram of Runtime", xlab = "Runtime(minutes)")
ggplot(df2, aes(x = Genre)) + 
  geom_bar() + 
  labs(x = "",
       y = "Frequency",
       title = "Frequency of Genres") +
  coord_flip()
table(df2$Certificate)
ggplot(df2, aes(x = Certificate)) + 
  geom_bar() +
  ggtitle("Count of Certificate")

# Logistic Regression
certificate = relevel(factor(certificate), ref = "Not_rated")
data.lr.cert = glm(good_rating ~ certificate, data = training.data, family = "binomial")
summary(data.lr.cert) # Model 1
prediction1 = ifelse(predict(data.lr.cert, newdata = test.data[c("certificate")], type = "response") > 0.5, "Good", "Bad")
confusion1 = confusion_matrix(prediction, test.data$good_rating)
er1<-1 - sum(diag(confusion1))/sum(confusion1)

genre = relevel(factor(genre), ref = "No")
data.lr.genre = glm(good_rating ~ genre, data = training.data, family = "binomial")
summary(data.lr.genre) # Model 2
prediction2 = ifelse(predict(data.lr.genre, newdata = test.data[c("genre")], type = "response") > 0.5, "Good", "Bad")
confusion2 = confusion_matrix(prediction, test.data$good_rating)
er2<-1 - sum(diag(confusion2))/sum(confusion2)

data.lr.reduced = glm(good_rating ~ certificate + runtime_min, data = training.data, family = "binomial")
summary(data.lr.reduced) # Model 3
prediction3 = ifelse(predict(data.lr.reduced, newdata = test.data[c("certificate", "runtime_min")], type = "response") > 0.5, "Good", "Bad")
confusion3 = confusion_matrix(prediction, test.data$good_rating)
er3<-1 - sum(diag(confusion3))/sum(confusion3)

ROC<-function(model,test,level,true){ # Function for plotting ROC Curve
  thresholds <- seq(0.01, 0.99, 0.005)
  TPR <- numeric(length(thresholds))
  FPR <- numeric(length(thresholds))
  for (i in 1:length(thresholds)) {
    predicted <- factor(ifelse(predict(model, test, type = "response") > thresholds[i],level[1], level[2]),
                        level)
    confusion <- table(as.character(true),
                       predicted,
                       dnn = c("True Rating", "Predicted Rating"))
    TPR[i] <- confusion[2,2]/(confusion[2,2]+confusion[2,1])
    FPR[i] <- confusion[1,2]/(confusion[1,2]+confusion[1,1])
  }
  plot(x = FPR, y = TPR, type = 'l', main = 'ROC Curve')
}

levels<-c("Good","Bad")
ROC(data.lr.cert,test.data[c("certificate")],levels,test.data$good_rating) # ROC Curve for Model 1
ROC(data.lr.reduced, test.data[c("certificate", "runtime_min")],levels,test.data$good_rating) # ROC Curve for Model 2

# k-NN Algorithm
dataset2=data[data$runtime_min<100,]
dataset2=dataset2[,-c(2,3,5,7,8)]
dataset2=dataset2[dataset2$runtime_min!=0,]
dataset2=dataset2[dataset2$rating!=0.0,]
Mean2=mean(dataset2$rating)
testset=dataset2[1:1208,]
trainset=dataset2[1209:6042,]
BAvec=ifelse(trainset$rating>=Mean2,"AM","BM")
trainset=cbind(trainset,BAvec)
trainset=trainset[-c(5:12)]
testsetmod=testset[,2]
trainsetmod=trainset[2]
knnfunction=function(index){ # k-NN Algorithm Function
  dis=abs(testsetmod[index]-trainsetmod)
  dis2=sort(dis[,1])
  dis3=min(dis2[dis2>0])
  dis4=head(rownames(which(dis==dis3,arr.ind=TRUE)),1)
  dis5=trainset$BAvec[row.names(trainset)==dis4]
  pbm=0
  pam=1
  if (dis5=="AM"){
    cp=pam/1
  }else {
    cp=pbm/1
  }
  fr=ifelse(cp>.5,"AM","BM")
  return(fr)
}
preval=NULL
for (i in 1:nrow(testset)){
  preval[i]=knnfunction(i)
}
new_testset=cbind(testset,ifelse(testset$rating>=Mean2,"AM","BM"))
new_testset=new_testset[,4]
cm=table(preval,new_testset,dnn = c("Predicted rating","True rating"))
(sum(diag(cm))/sum(cm))
newdat=data.frame(new_testset,preval)
ptestset=data.frame(testset,new_testset)
ptestset1=data.frame(ptestset,preval)
ptestset2=ptestset1[ptestset1$new_testset!=ptestset1$preval,]
require(ggplot2)
ggplot()+
  geom_point(data=ptestset,mapping=aes(x=rating,y=1:1208,colour=new_testset))+
  labs(x="Rating",y="Index",colour="Above or Below")+
  geom_point(data=ptestset1,mapping = aes(x=rating,y=1:1208,colour=preval),shape=2)+
  geom_vline(xintercept = Mean2,linetype="dashed",color="red")

# Linear Regression
df1 <- data.frame(cbind(certificate_group, genre_group, data$title, data$year, data$runtime_min, data$rating, data$votes, data$director_star))
colnames(df1) <- c("Certificate", "Genre", "Title", "Year", "Runtime_min", "Rating", "Votes", "Director_star")
attach(df1)
df2<-df1[!(df1$Certificate=="Not_rated"),]
df2<-df2[,-8]
df2<-df2[,-4]
df2<-df2[,-3]
attach(df2)
df2$Runtime_min <- as.numeric(df2$Runtime_min)
df2$Votes <- as.numeric(df2$Votes)
df2$Rating<- as.numeric(df2$Rating)
attach(df2)

df1$Genre = relevel(factor(Genre), ref = "No") 
model1 <- lm(Vote~Genre, data = df1) # Model 1
summary(model1)
plot(model1)

df1$Certificate = relevel(factor(Certificate), ref = "Not_rated")
model2 <- lm(Vote~Certificate, data = df1) # Model 2
summary(model2)
plot(model2)

model3 <- lm(Vote~Genre + Certificate + min, data = df1)
summary(model3)
plot(model3)

df2 <- df1[-c(21, 27, 88, 85,59, 119, 1515),] # Outliers removed

df2$Genre = relevel(factor(df2$Genre), ref = "No")
model7 <- lm(log1p(Vote)~Genre, data = df2) # Post-Transform Model 1
summary(model7)
plot(model7)

df2$Certificate = relevel(factor(df2$Certificate), ref = "Not_rated")
model8 <- lm(log1p(Vote)~Certificate, data = df2) # Post-Transform Model 2
summary(model8)
plot(model8)

model9 <- lm(log1p(Vote)~Genre + Certificate + log1p(min), data = df2) # Post-Transform Model 3
summary(model9)
plot(model9)

library(lmtest)
df2$rating <- as.numeric(df2$Rating)
model20 <- lm(Votes~Rating, data = df2) # Model 4
bptest(model20) 
model11 <- lm(log1p(Vote)~log1p(rating), data = df2) # Post-Transform Model 4
bptest(model11)
plot(model11)
summary(model11)

