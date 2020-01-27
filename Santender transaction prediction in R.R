rm(list=ls())
# Setting working directory
setwd("E:/Edwisor Project/Santender Transaction Prediction in R")
# Checking for the working directory
getwd()
#Loading Train and Test dataset
train <- read.csv("E:/Edwisor Project/Santender Transaction Prediction/train.csv", header=FALSE)
test <- read.csv("E:/Edwisor Project/Santender Transaction Prediction/test.csv", header=FALSE)
#Getting summary of dataset
str(train)
str(test)
#Dimension of train data
dim(train)
dim(test)
#First 10 rows of dataset
head(train)
head(test)
#Typecasting the target variable
train$V2<-as.factor(train$V2)
#Count of target classes
table(train$V2)
#Percentage count of Target classes
table(train$V2)/length(train$V2)*100
#Importing the relevent packages and libraries
install.packages('tidyverse')
library(tidyverse)
install.packages('moments')
library(moments)
install.packages('DataExplorer')
library(DataExplorer)
install.packages('caret')
library(caret)
install.packages('Matrix')
library(Matrix)
install.packages('pdp')
library(pdp)
install.packages('mlbench')
library(mlbench)
install.packages('caTools')
library(caTools)
install.packages('randomForest')
library(randomForest)
install.packages('glmnet')
library(glmnet)
install.packages('mlr')
library(mlr)
install.packages('vita')
library(vita)
install.packages('rBayesianOptimization')
library(rBayesianOptimization)
install.packages('lightgbm')
library(lightgbm)
install.packages('pROC')
library(processx)
install.packages('DMwR')
library(DMwR)
install.packages('ggplot2')
library(ggplot2)
#Bar plot for the count of target classes
ggplot(train,aes(v2))+theme_bw()+geom_bar(stat='count',fill='blue')

# Distribution of train attributes from 3 to 102

for(var in names (train)[c(3:102)])
{
  V2<-train$V2
  plot<-ggplot(train,aes(x=train[[var]],fill=V2))+geom_density(kernel='gaussian')+ggtitle(var)+theme_classic()
  print(plot)
}


# Distribution of train attributes from 103 to 202

for(var in names (train)[c(103:202)])
{
  V2<-train$V2
  plot<-ggplot(train,aes(x=train[[var]],fill=V2))+geom_density(kernel='gaussian')+ggtitle(var)+theme_classic()
  print(plot)
}


# Distribution of test attributes from 2 to 101

plot_density(test[,c(2:101)],ggtheme=theme_classic(),geom_density_args=list(color='blue'))


# Distribution of test attributes from 102 to 201

plot_density(test[,c(102:201)],ggtheme=theme_classic(),geom_density_args=list(color='blue'))


#Applying the function to find the mean values per row in train and test data

train_mean<-apply(train[,-c(1,2)],MARGIN=1,FUN=mean)
test_mean<-apply(test[,-c(1)],MARGIN=1,FUN=mean)

ggplot()+
# Distribution of mean values per row in train data
geom_density(data=train[,-c(1,2)],aes(x=train_mean),kernel='gaussian',show.legend = TRUE,color='blue')+theme_classic()+
# Distribution of mean values row in test data
geom_density(data=test[,-c(1)],aes(x=test_mean),kernel='gaussian',show.legend = TRUE,color='green')+
labs(x='mean values per row',title='Distribution of mean values per row in train and test dataset')


#Applying the function to find the mean values per column in train and test data

train_mean<-apply(train[,-c(1,2)],MARGIN=2,FUN=mean)
test_mean<-apply(test[,-c(1)],MARGIN=2,FUN=mean)

ggplot()+
# Distribution of mean values per row in train data
geom_density(aes(x=train_mean),kernel='gaussian',show.legend = TRUE,color='blue')+theme_classic()+
# Distribution of mean values row in test data
geom_density(aes(x=test_mean),kernel='gaussian',show.legend = TRUE,color='green')+
labs(x='mean values per row',title='Distribution of mean values per columns in train and test dataset')


#Applying the function to find the standard deviation values per row in train and test data

train_sd<-apply(train[,-c(1,2)],MARGIN=1,FUN=sd)
test_sd<-apply(test[,-c(1)],MARGIN=1,FUN=sd)

ggplot()+
  # Distribution of sd values per row in train data
  geom_density(data=train[,-c(1,2)],aes(x=train_sd),kernel='gaussian',show.legend = TRUE,color='blue')+theme_classic()+
  # Distribution of sd values row in test data
  geom_density(data=test[,-c(1)],aes(x=test_sd),kernel='gaussian',show.legend = TRUE,color='green')+
  labs(x='sd values per row',title='Distribution of sd values per row in train and test dataset')


#Applying the function to find the sd values per column in train and test data

train_sd<-apply(train[,-c(1,2)],MARGIN=2,FUN=sd)
test_sd<-apply(test[,-c(1)],MARGIN=2,FUN=sd)

ggplot()+
  # Distribution of sd values per row in train data
  geom_density(aes(x=train_sd),kernel='gaussian',show.legend = TRUE,color='blue')+theme_classic()+
  # Distribution of sd values row in test data
  geom_density(aes(x=test_sd),kernel='gaussian',show.legend = TRUE,color='green')+
  labs(x='sd values per row',title='Distribution of sd values per columns in train and test dataset')


#Applying the function to find the skewness values per row in train and test data

train_skew<-apply(train[,-c(1,2)],MARGIN=1,FUN=skewness)
test_skew<-apply(test[,-c(1)],MARGIN=1,FUN=skewness)

ggplot()+
  # Distribution of skewness per row in train data
  geom_density(data=train[,-c(1,2)],aes(x=train_skew),kernel='gaussian',show.legend = TRUE,color='blue')+theme_classic()+
  # Distribution of skewness values row in test data
  geom_density(data=test[,-c(1)],aes(x=test_skew),kernel='gaussian',show.legend = TRUE,color='green')+
  labs(x='skewness values per row',title='Distribution of skewness values per row in train and test dataset')


#Applying the function to find the skewness values per columns in train and test data

train_skew<-apply(train[,-c(1,2)],MARGIN=2,FUN=skewness)
test_skew<-apply(test[,-c(1)],MARGIN=2,FUN=skewness)

ggplot()+
  # Distribution of skewness per columns in train data
  geom_density(aes(x=train_skew),kernel='gaussian',show.legend = TRUE,color='blue')+theme_classic()+
  # Distribution of skewness values per columns in test data
  geom_density(aes(x=test_skew),kernel='gaussian',show.legend = TRUE,color='green')+
  labs(x='skewness values per row',title='Distribution of skewness values per columns in train and test dataset')


# Missing Values analysis in train dataset
missing_val <-data.frame(missing_val=apply(train,2,function(x){sum(is.na(x))}))
missing_val <- sum(missing_val)
misisng_val

# Missing Values analysis in test dataset
missing_val <-data.frame(missing_val=apply(test,2,function(x){sum(is.na(x))}))
missing_val <- sum(missing_val)
misisng_val

# Correlation Analysis in train data
# convert factor to int
train$V2<-as.numeric(train$V2)
train_correlations<-cor(train[,c(2:202)])
train_correlations

# Correlation Analysis in test data
test_correlations<-cor(test[,c(2:201)])
test_correlations


# Variable Importance

#splitting the training data using simple random sampling
train_index<-sample(1:nrow(train),0.75*nrow(train))
#train data
train_data<-train[train_index,]
#Validation data
valid_data<-train[-train_index,]
#Dimension of train and validation data
dim(train_data)
dim(valid_data)

# Random forest classifier
set.seed(123)
#convert int to factor
train_data$V2<-as.factor(train_data$V2)
#setting mtry
mtry<-floor(sqrt(200))
# setting tune grid
tuneGrid<-expand.grid(.mtry=mtry)
#fitting random forest
rf<-randomForest(V2~.,train_data[,-c(1)],mtry=mtry,ntree=10,importance=TRUE)

#Feature Importnace by random Forest
varImp<-importance(rf,type=2)
varImp

# Partial depndence plot
# Plotting varaible 13
par.var_13<-partial(rf,pred.var=c('var13'),chull=TRUE)
plot.var_13<-autoplot(par.var_13,contour=TRUE)
plot.var_13

# Plotting variable 34
par.var_34<-partial(rf,pred.var=c('var34'),chull=TRUE)
plot.var_34<-autoplot(par.var_34,contour=TRUE)
plot.var_34


#Logistic regression Model

#Training and validation dataset
X_t<-as.matrix(train.data[,-c(1,2)])
y_t<-as.matrix(train.data$V2)
#Validation dataset
X_v<-as.matrix(valid.data[,-c(1,2)])
y_v<-as.matrix(valid.data$V2)
#test dataset
test<-as.matrix(test[,-c(1)])

#Logistic regression model
set.seed(123)
lr_model<-glmnet(X_t,y_t,family='binomial')
summary(lr_model)

# Cross validation prediction
set.seed(123)
cv_lr<-cv.glmnet(X_t,y_t,family='binomial',type.measure='class')
cv_lr

#Plotting missclassification erroe vs log(lambda) where lambda is regularization parameter
cv_lr$lambda.min
#plot the auc score vs log(lambda)
plot(cv_lr)


# Model performance on validation dataset
set.seed(123)
cv_predict.lr<-predict(cv_lr,X_v,s='lambda.min',type='class')
cv_predict.lr

# Confusion matrix
set.seed(123)
#actual target variable
V2<-valid.data$V2
#convert to factor
V2<-as.factor(V2)
#predicted target variable convert to factor
cv_predict.lr<-as.factor(cv_predict.lr,refrence=V2)

#ROC_AUC score and curve
set.seed(123)
cv_predict.lr<-as.numeric(cv_predict.lr)
roc(data=valid.data[,-c(1,2)],response=V2,predictor=cv_predict.lr,auc=TRUE,plot=TRUE)

#Model performance on test data
# when we compare the model accuracy and roc_auc score, concluded that the model is not performing well on imbalanced dataset

# Random oversampling examples(ROSE)
train.rose<-ROSE(V2~.,data=train_data[,-c(1)],seed=32)$data
# target classes in balanced train data
table(train.rose$V2)
valid.rose<-ROSE(V2~.,data=valid_data[,-c(1)],seed=42)$data
#target classes in balanced valid data
table(valid.rose$V2)


# Logistic Regression model
set.seed(123)
lr_rose <-glmnet(as.matrix(train.rose),as.matrix(train.rose$V2),family='binomial')
summary(lr_rose)

# Cross validation prediction
set.seed(123)
cv_rose<-cv.glmet(as.matrix(valid.rose),as.matrix(valid.rose$V2),family='binomial',type.measure='class')
cv_rose

#Plotting missclassifiaction error vs log(lambda)
#Minimum lambda
cv_rose$lambda.min
#plot the auc score vs log(lambda)
plot(cv_rose)

#Model performance on validation dataset
set.seed(123)
cv_predict.rose<-predict(cv_rose,as.matrix(valid.rose),s='lambda.min',type='class')
cv_predict.rose

# confusion Matrix
set.seed(123)
#actual target Variable
target<-valid.rose$V2
#Convert to factor
target<-as.factor(target)

#predicted target variable
#convert to factor
cv_predict.rose<-as.factor(cv_predict.rose)
# Confusion Matrix
confusionMatrix(data=cv_predict.rose,refrence=target)

#ROC_AUC score and curve
set.seed(843)
#convert to numeric
cv_predict.rose<-as.numeric(cv_predict.rose)
roc(data=valid.rose[,-c(1,2)],response=target,predictor=cv_predict.rose,auc=TRUE,plot=TRUE)

# Model performace on test data
set.seed(234)
rose_pred<-predict(lr_rose,test,type='class')
# We observed that the ROSE model is performing well on imbalance data compare to baseline logistic regression

# LightGBM
# Training and Validation dataset

#Convert the dataframe to matrix
set.seed(235)
X_train<-as.matrix(train.data[,-c(1,2)])
y_train<-as.matrix(train.data$V2)
X_valid<-as.matrix(valid.data[,-c(1,2)])
y_valid<-as.matrix(valid.data$V2)
test_data<-as.matrix(test[,-c(1)])

#training data
lgb.train<-lgb.Dataset(data=X_train,label=y_train)
# validation data
lgb.valid<-lgb.Dataset(data=X_valid,label=y_valid)

# Selecting best hyperparameters
set.seed(653)
lgb.grid<-list(objective='binary',metric="auc",boost='gbdt',max_depth=-1,boost_from_average='false',min_sum_hessian_in_leaf=12,feature_fraction=0.05,bagging_fraction=0.45,bagging_freq=5,learning_rate=0.02,tree_learner='serial',num_leaves=20,num_threads=5,min_data_in_bin=150,min_gain_to_split=30,min_data_in_leaf=90,verbosity=-1,is_unbalance=TRUE)

#Training the lgbm model
set.seed(345)
lgbm.model<-lgb.train(params=lgb.grid,data=lgb.train,nrounds=10000,eval_freq=1000,valids=list(val1=lgb.train,val2=lgb.valid),early_stopping_rounds=5000)

#lgbm model performance on test data
set.seed(456)
lgbm_pred_prob<-predict(lgbm.model,test_data)
print(lgbm_pred_prob)

# Convert to binary output 0 and 1 with threshhold 0.5
lgbm_pred<-ifelse(lgbm_pred_prob>0.5,1,0)
print(lgbm_pred)

# Plotting Important features
tree_imp<-lgb.importance(lgbm.model,percentage=TRUE)
lgb.plot.importance(tree_imp,top_n=50,measure='Frequency',left_margin=10)

# Final Submission 
sub_df<-data.frame(ID_code=test_df$ID_code,lgb_predict_prob=lgbm_pred_prob,lgb_predict=lgbm_pred_prob)
write.csv(sub_df,'submission.CSV',roe.names=F)
head(sub_df)

rm(list=ls())

