getwd()
db =read.csv(choose.files())
summary(db)
str(db)
head(db)
db$Outcome<-as.factor(db$Outcome)
head(db$Outcome)
any(is.na(db))
any(is.null(db))

nor <- function(x) {(x-min(x))/max(x)-min(x)}


#KNN
# Run normalization on  coulumns of dataset because they are the predictors
db_norm <- as.data.frame(lapply(db[-9],nor))

summary(db_norm)
set.seed(1234)
split <- sample.split(db_norm, SplitRatio = 0.6)
db_train <- subset(db_norm, split == "TRUE")
NROW(db_train)
db_test <- subset(db_norm, split == "FALSE")
NROW(db_test)


db_train_label<- subset(db[,9], split == "TRUE")
NROW(db_train_label)
table(db[9])
# Extracting 9th column of test dataset to measure accuracy
db_test_label <- subset(db[,9], split == "FALSE")
NROW(db_test_label)

#loaing class
install.packages("class")
library(class)

?knn

# run KNN fuction
pr <- knn(db_train,db_test,cl=db_train_label,k=91)

#create confusion matrix 
tab <- table(pr,db_test_label)
#this fin divides thr correct predictions by total number of predictions that tells us how accurate the model is
tab
accuracy <-function(x) { sum(diag(x)/(sum(rowSums(x))))*100 }

accuracy(tab)
#-------------------------------------------------------------------------------------------------------

#kmeans
#stats
db_cluster<-kmeans(db[,1:8],center=8,nstart = 150)
db_cluster$size
print(db_cluster)
table(db_cluster$cluster, db$Outcome)

aggregate(db[-9],by=list(cluster=db_cluster$cluster),mean)

newdata<-cbind(db[-9],cluster=db_cluster$cluster)
library(cluster)
clusplot(db, db_cluster$cluster, color=T, shade=T, labels=0, lines=0)

ggplot(data=newdata,aes(x=result$cluster))+geom_bar(fill='steelblue')
#.............................................................................
#decision tree1
db_decision_tree_model= rpart(Outcome~.,data=db,method = "class")
plot(db_decision_tree_model)
rpart.plot(db_decision_tree_model)
rpart.plot(db_decision_tree_model,type=3,extra = 103)


#decision tree
nor <- function(x) {(x-min(x))/max(x)-min(x)}



db_norm1 <- as.data.frame(lapply(db[-9],nor))

split <- sample.split(db_norm1, SplitRatio = 0.6)
db_train <- subset(db, split == "TRUE")
NROW(db_train)
db_test <- subset(db_norm, split == "FALSE")
NROW(db_test)
tree<-ctree(Outcome~.,data =db,controls = ctree_control(mincriterion = 0.90,minsplit = 200))
tree
plot(tree)
#prediction
predict(tree,test,type="prob")
predict(tree,test)
tree1<-rpart(Outcome~.,train)
rpart.plot(tree1)
#..........................................................................................

#regression
db1 =read.csv(choose.files())
table(db1$Outcome)
cor(db1)
pairs(db1)
install.packages("psych")
#install package so that pairs.panels can be used
library(psych)
#load the package

model=lm(Insulin~.,data=db)
print(model)
db_train1 <- db_train
db_train1$Predicted_val <- predict(model,db_train[1:9])
head(db_train1)
db_test1 <- db_test
db_test1$Predicted_val <- predict(model,db_test[1:9])
head(db_test1)
#plotting the insulin level vs predicted 
ggplot()+
  geom_point(aes(x=db_train1$Insulin,y=db_train1$Predicted_val),size=3,color="blue")
ggplot()+
  geom_point(aes(x=db_test1$Insulin,y=db_test1$Predicted_val),size=3,color="blue")





















