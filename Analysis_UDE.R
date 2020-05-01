df <- read.csv("C:/Users/User/Downloads/original/Files/Data/House_Price.csv",header = TRUE)

View(df)

str(df)

###UNIVARIATE ANALYSIS

summary(df)  #analysis of data how much is skweness(difference b/w mean and median,diff b/w mean
#and quartiles to find the outliers,find missing values,etc)

hist(df$crime_rate) #check values for particular variables

pairs(~price+crime_rate+n_hot_rooms+rainfall,data = df) #check relationship and outliers

barplot(table(df$airport))

barplot(table(df$waterbody))

barplot(table(df$bus_ter))  #constant value so useless variable for analysis

## OUTLIER TREATMENT

##Capping and Flooring technique

quantile(df$n_hot_rooms,0.99)    #assign last quartile to the variable 
uv <- 3*quantile(df$n_hot_rooms,0.99)
df$n_hot_rooms[df$n_hot_rooms>uv] <- uv
summary(df$n_hot_rooms)

summary(df$rainfall)

lv <- 0.3*quantile(df$rainfall,0.01)
df$rainfall[df$rainfall<lv] <- lv
summary(df$rainfall)
#mean and median are close now
#minimum value is close to 1st quartile

## Missing value treatment

mean(df$n_hos_beds,na.rm = T)

which(is.na(df$n_hos_beds))
df$n_hos_beds[which(is.na(df$n_hos_beds))]<- mean(df$n_hos_beds,na.rm = T)

#summary(df$n_hos_beds)
 #mean(df$n_hos_beds)
 
# Variable Transformation

pairs(~price+crime_rate,data = df)

plot(df$price,df$crime_rate)  #log curve

df$crime_rate <- log(1+df$crime_rate)

df$avg_dist <- (df$dist1+df$dist2+df$dist3+df$dist4)/4

df <- df[,-7:-10]

df <- df[,-14]

# create dummy variable 

install.packages("dummies")

df <- dummy.data.frame(df)

df <- df[,-14]

## correlation matrix

cor(df)

round(cor(df),2)

df <- df[,-16]  #identify multi colinearity and remove unnecessary columns

#Linear Regression

simple_model <- lm(price~room_num,data = df)

summary(simple_model)

plot(df$room_num,df$price)  #linear relationship

abline(simple_model)

#Multiple Linear regression

multiple_model <- lm(price~.,data = df)

summary(multiple_model)

 install.packages("caTools")

 set.seed(0)

 split= sample.split(df,SplitRatio=0.8) 

 training_set = subset(df,split==T) 

 test_set= subset(df,split==F) 

 lm_a = lm(price~.,data=training_set) 

 train_a <- predict(lm_a,training_set)
 
 test_a <- predict(lm_a,test_set) 

 mean((training_set$price-train_a)^2) 

 mean((test_set$price-test_a)^2) 
 
 #subset selection
 
 lm_best<- regsubsets(price~.,data=df,nvmax = 15)

 summary(lm_best) 
  
 summary(lm_best)$adjr2
 
 which.max(summary(lm_best)$adjr2)

 coef(lm_best,8) 

 lm_forward <-  regsubsets(price~.,data=df,nvmax = 15,method = "forward")

 summary(lm_forward) 

 #Shrinkage Method

 x=model.matrix(price~.,data = df)[,-1]
 y=df$price 
grid= 10^seq(10,-2,length=100)
grid

# Logistic Regression with single predictor

glm.fit = glm(sold~price,data = df,family = binomial)

# Linear Discriminant Analysis

lda.fit <- lda(sold~.,data=df)

lda.pred <- predict(lda.fit,df) #find the probabilities for groups

lda.pred$posterior

lda.class= lda.pred$class

table(lda.class,df$sold) #confusion matrix

sum(lda.pred$posterior[,-1]>0.8)

# split into test and train 

set.seed(0)

split <- sample.split(df,SplitRatio = 0.8)

train_set <- subset(df,split==T)

test_set <- subset(df,split==F)

train.fit <- glm(SOld~.,data = train_set,family = binomial)

test_probs <- predict(train_set,test_set,type='response')

#K nearest neibhbours

trainX <- train_set

testX <- test_set 

trainy<- train_set$sold

testX <- test_set$sold

k=3

## TREE

movie <- read.csv("C:/Users/User/Desktop/movie_regression.csv")

summary(movie)

movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken,na.rm = T) 

summary(movie)

set.seed(0)

split <- sample.split(movie,SplitRatio = 0.8)

train <- subset(movie,split==T)

test <- subset(movie,split==F)

#run regression tree model on train set

regtree <- rpart(formula=Collection~.,data = train,control = rpart.control(maxdepth = 3))

#Plot the decision tree

rpart.plot(regtree,box.pallete="RdBu",digits=-3)

#classification tree

df <- read.csv("C:/Users/User/Desktop/movie_classification.csv")
View(df)

df$Time_taken[is.na(df$Time_taken)] <- mean(df$Time_taken,na.rm = T) 

set.seed(0)

split <- sample.split(df,SplitRatio = 0.8)

trainc <- subset(df,split==T)

testc <- subset(df,split==F)

classtree <- rpart(formula = Start_Tech_Oscar~.,data = trainc,method = 'class',control = rpart.control(maxdepth = 3))

#plot the decision tree
rpart.plot(classtree,box.palette = "RdBu",digits = -3)

#predict value at any point

testc$pred <- predict(classtree,testc,type="class")

table(testc$Start_Tech_Oscar,testc$pred)

#Bagging

set.seed(0)

bagging <- randomForest(Collection~.,data = train,mtry=17)

test$bagging <- predict(bagging,test)

MSE2bagging <- mean((test$bagging-test$Collection)^2)

#Random Forest

randomfor <- randomForest(Collection~.,data = train,ntree=500)

#output

test$random <- predict(randomfor,test)
MSE2random <-  mean((test$random-test$Collection)^2) #MSE getting reduced

# Gradient Boosting

set.seed(0)

boosting <- gbm(Collection~.,data = train,distribution = "gaussian",n.trees = 5000,interaction.depth = 4,shrinkage = 0.2, verbose = F)

#distribution 

test$boost <- predict(boosting,test,n.trees=5000)

MSE2boost <- mean((test$boost-test$Collection)^2)

# AdaBoosting 

trainc$Start_Tech_Oscar1 <- as.factor(trainc$Start_Tech_Oscar)

adaboost <- boosting(Start_Tech_Oscar1~.,data=trainc,boos=T)

predada <- predict(adaboost,testc)

table(predada$class,testc$Start_Tech_Oscar)

t1 <- adaboost$trees[[1]]
plot(t1)

text(t1,pretty = 100)


## XGBOOST

library(xgboost)

trainy <- trainc$Start_Tech_Oscar == "1"

trainx <- model.matrix(Start_Tech_Oscar~.-1,data = trainc)

trainx <- trainx[,-12]

testy <- testc$Start_Tech_Oscar =="1"

testx<- model.matrix(Start_Tech_Oscar~.-1,data = testc)

testx <- testx[,-12]

#create data into t-matrix format

Xmatrix <- xgb.DMatrix(data = trainX, label = trainY)
Xmatrix_t <- xgb.DMatrix(data = testX, label = testY)

Xgboosting <-  xgboost(data = Xmatrix,nrounds = 50,objective= "multi:softmax",eta=0.3,num_class=2,max_depth=100)

# SVM Model

movie <- read.csv("C:/Users/User/Desktop/movie_classification.csv")

movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken,na.rm = T) 

set.seed(0)

split = sample.split(movie,0.8)

trainc <- subset(movie,split==T)
testc <- subset(movie,split==F)

# Classification

trainc$Start_Tech_Oscar <- as.factor(trainc$Start_Tech_Oscar)
testc$Start_Tech_Oscar <- as.factor(testc$Start_Tech_Oscar)

svmfit <- svm(Start_Tech_Oscar~.,data = trainc,kernel = "linear",cost=1,scale=T)

summary(svmfit)

#prediction on test set

ypred <- predict(svmfit,testc)

table(predict=ypred,truth=testc$Start_Tech_Oscar)

svmfit$index

# Tune the best value of C / Tune the hyperparamter

set.seed(0)

tune.out <- tune(svm,Start_Tech_Oscar~.,data = trainc,kernel="linear",
                 ranges = list(cost=c(0.001,0.01,1,10,100)))

bestmod <- tune.out$best.model
summary(bestmod)

ypredL <- predict(bestmod,testc)
table(predict=ypredL,truth=testc$Start_Tech_Oscar)

# Polynomial Kernel

svmfitp <- svm(Start_Tech_Oscar~.,data = trainc,kernel="polynomial",cost=1,degree=2)

#tuning

tune.outp<- tune(svm,Start_Tech_Oscar~.,data = trainc,cross=4,kernel="polynomial",ranges = list(cost=c(0.001,0.01,1,10,100)),degree=c(0.5,1,2,3,5))

bestmodp <- tune.outp$best.model

summary(bestmodp)

ypredp <- predict(bestmodp,testc)

table(predict=ypredp,truth=testc$Start_Tech_Oscar)

# Radial Kernel

svmfitr <- svm(Start_Tech_Oscar~.,data = trainc,kernel="radial",cost=1,degree=2)

#tuning

tune.outr<- tune(svm,Start_Tech_Oscar~.,data = trainc,cross=4,kernel="radial",ranges = list(cost=c(0.001,0.01,1,10,100)),gamma=c(0.01,0.1,0.5,1,2,3,4,10,50))

bestmodr <- tune.outr$best.model

summary(bestmodr)

ypredr <- predict(bestmodr,testc)

table(predict=ypredr,truth=testc$Start_Tech_Oscar)

# Regression SVM 

df <- read.csv("C:/Users/User/Desktop/movie_regression.csv")

summary(df)


df$Time_taken[is.na(df$Time_taken)] <- mean(df$time_taken,na.rm = T) 

set.seed(0)

split = sample.split(df,0.8)

train <- subset(df,split==T)
test <- subset(df,split==F)


svmfitR <- svm(Collection~.,data = train,kernel = "linear",cost=0.01,scale=T)

summary(svmfitR)

#prediction on test set

ypredR <- predict(svmfitR,test)

mse <- mean((ypredR-test$Collection)^2)


