rm(list=ls(all=TRUE))

bldg_own_data <- read.csv("ML_Building_Quake_Case/Building_Ownership_Use.csv",1)
bldg_str_data <- read.csv("ML_Building_Quake_Case/Building_Structure.csv",1)
train_data <- read.csv("ML_Building_Quake_Case/train.csv",1)
test_data <- read.csv("ML_Building_Quake_Case/test.csv",1)
# merge the data sets
# merge the two building data frames by id, district and municipality
bldg_data <- merge(bldg_own_data,bldg_str_data,by=c("building_id","district_id","vdcmun_id"),sort = FALSE)
head(bldg_data)
rf_train_data <- merge(train_data,bldg_data,by=c("building_id","district_id","vdcmun_id"),sort = FALSE)
head(rf_train_data)
rf_test_data <- merge(test_data,bldg_data,by=c("building_id","district_id","vdcmun_id"),sort = FALSE)
head(rf_test_data)

# *** Exploratory Data Analysis ***

### check number of rows and columns
dim(rf_train_data)
dim(rf_test_data)
### check types of variables
str(rf_train_data)
names(rf_train_data)
str(rf_test_data)
names(rf_test_data)

### convert categorical attributes to factor type
rf_data_num <- rf_train_data[,c('count_families','count_floors_pre_eq','count_floors_post_eq',
                               'age_building','plinth_area_sq_ft','height_ft_pre_eq',
                               'height_ft_post_eq')]
rf_data_cat <- rf_train_data[,-c(17,30:35)]
rf_data_cat <- data.frame(apply(rf_data_cat, 2,function(x){as.factor(x)}))
rf_train_data <- cbind(rf_data_cat,rf_data_num)

rf_data_num <- rf_test_data[,c('count_families','count_floors_pre_eq','count_floors_post_eq',
                                'age_building','plinth_area_sq_ft','height_ft_pre_eq',
                                'height_ft_post_eq')]
rf_data_cat <- rf_test_data[,-c(16,29:34)]
rf_data_cat <- data.frame(apply(rf_data_cat, 2,function(x){as.factor(x)}))
rf_test_data <- cbind(rf_data_cat,rf_data_num)

### validate the type of variables
str(rf_train_data)
str(rf_test_data)
### continuous attributes
names(rf_train_data)[which(sapply(rf_train_data, is.numeric))]
names(rf_test_data)[which(sapply(rf_test_data, is.numeric))]
### discrete attributes
names(rf_train_data)[which(sapply(rf_train_data,is.factor))]
names(rf_test_data)[which(sapply(rf_test_data,is.factor))]

### Check for null or missing values
table(is.na(rf_train_data))
sapply(rf_train_data, function(x) sum(is.na(x))/length(x))*100
cbind(colSums(is.na(rf_train_data)))
# count_families    1
# has_repair_started  33417

table(is.na(rf_test_data))
sapply(rf_test_data, function(x) sum(is.na(x))/length(x))*100
cbind(colSums(is.na(rf_test_data)))
# has_repair_started  21922

library(VIM)
library(mice)
opar <- par(no.readonly = TRUE)
par(bg ="gray63", col="white", col.axis = "white", col.lab = "white", col.main = "white",col.sub = "white")
aggr(rf_train_data,prop = F,cex.axis = 0.4, numbers = T)

opar <- par(no.readonly = TRUE)
par(bg ="gray63", col="white", col.axis = "white", col.lab = "white", col.main = "white",col.sub = "white")
aggr(rf_test_data,prop = F,cex.axis = 0.4, numbers = T)

# imputation
library(mlr)
imp1 <- impute(rf_train_data,target = "damage_grade",classes = list(numeric=imputeMedian(), factor=imputeMode()))
rf_train_data <- imp1$data
#library(DMwR)
#rf_train_data_knn <- knnImputation(rf_train_data,meth = "median")
# verify imputation
cbind(colSums(is.na(rf_train_data)))

imp1 <- impute(rf_test_data,classes = list(numeric=imputeMedian(), factor=imputeMode()))
rf_test_data <- imp1$data
# verify imputation
cbind(colSums(is.na(rf_test_data)))

### Check for constants
sapply(rf_train_data,function(x) length(unique(x)))
sapply(rf_test_data,function(x) length(unique(x)))

head(rf_train_data)
head(rf_test_data)

### Five-number summary
summary(rf_train_data)
summary(rf_test_data)

## outlier treatment - capping (b/w 5th %ile and 95th %ile)
## for 'age_building' and 'plinth_area_sq_ft'
#x <- rf_train_data$age_building
#qnt <- quantile(x,probs = c(.25,.75),na.rm = T)
#caps <- quantile(x, probs=c(.05,.95),na.rm = T)
#H <- 1.5*IQR(x,na.rm = T)
#rf_train_data$age_building[rf_train_data$age_building<(qnt[1]-H)] <- caps[1]
#rf_train_data$age_building[rf_train_data$age_building>(qnt[2]+H)] <- caps[2]

#x <- rf_train_data$plinth_area_sq_ft
#qnt <- quantile(x,probs = c(.25,.75),na.rm = T)
#caps <- quantile(x, probs=c(.05,.95),na.rm = T)
#H <- 1.5*IQR(x,na.rm = T)
#rf_train_data$plinth_area_sq_ft[rf_train_data$plinth_area_sq_ft<(qnt[1]-H)] <- caps[1]
#rf_train_data$plinth_area_sq_ft[rf_train_data$plinth_area_sq_ft>(qnt[2]+H)] <- caps[2]


### check for duplicate columns
library("dplyr")
distinct(rf_train_data, rf_train_data$building_id)
distinct(rf_test_data, rf_test_data$building_id)

# remove building_id, ward_id.x, ward_id.y, vdcmun_id
rf_train_data <- rf_train_data[,-c(1,3,15,28)]
head(rf_train_data)

### continuous attributes
cont_var<-names(rf_train_data)[which(sapply(rf_train_data, is.numeric))]
cont_var
### discrete attributes
cat_var<-names(rf_train_data)[which(sapply(rf_train_data,is.factor))]
cat_var

### Correlation plot
num.data <- subset(rf_train_data[cont_var])
corr <- cor(num.data[,-1])
library(corrplot)
opar2 <- par(no.readonly = TRUE)
corrplot(corr,method = "circle",tl.cex = 0.5,tl.col = "black",number.cex = 0.55,bg = "grey14",
         addgrid.col = "gray50", tl.offset = 2,col = colorRampPalette(c("blue1","ivory2","firebrick2"))(100))

# remove high correlation - height_ft_pre_eq and height_ft_post_eq
rf_train_data <- rf_train_data[,-c(49,50)]
head(rf_train_data)

### continuous attributes
cont_var<-names(rf_train_data)[which(sapply(rf_train_data, is.numeric))]
cont_var
### discrete attributes
cat_var<-names(rf_train_data)[which(sapply(rf_train_data,is.factor))]
cat_var

str(rf_train_data)

### PCA
# convert the categorical variables into numeric using one hot encoding
library(dummies)
# create a dummy data frame
dummy_data <- dummy.data.frame(rf_train_data[,-c(1,3)], names = cat_var)
str(dummy_data)
pca = prcomp(dummy_data,scale. = T)
names(pca)
summary(pca)
pca$rotation
pca$rotation[1:5,1:4] # first 4 PCs and the loadings
biplot(pca,scale=0)
## scree plot
library(factoextra)
fviz_eig(pca)
# variance
pr_var = (pca$sdev)^2
# variance %
prop_varex = pr_var / sum(pr_var)
# plot
plot(prop_varex, xlab = "Principal Component",
     ylab="Proportion of Variance Explained", type="b")
#add a training set with principal components
train.data <- data.frame(damage_grade = rf_train_data$damage_grade, pca$x)
#we are interested in first 44 PCAs
train.data <- train.data[,1:45]
#transform test into PCA
# create a dummy data frame
dummy_data <- dummy.data.frame(rf_test_data[,-c(1:3,14,27)], names = cat_var)
test.data <- predict(pca, newdata = dummy_data)
test.data <- as.data.frame(test.data)
#select the first 44 components
test.data <- test.data[,1:44]
test.data$building_id <- rf_test_data$building_id
#### PCA


## *** Build the Model ***

# distribution
table(rf_train_data$damage_grade)

# Build the classification model using randomForest
#library(foreach)
#library(doSNOW)
library(randomForest)

# Setting number of cores in your machine. In this case, it is 2
#registerDoSNOW(makeCluster(2, type="SOCK"))

set.seed(7)

# Optimal mtry
# Select mtry value with minimum out of bag(OOB) error.
#mtry <- tuneRF(rf_train_data[-3],rf_train_data$damage_grade, stepFactor=0.5)
#print(mtry)
#best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
# Main Random Forest Code. Run 200 trees on 2 cores parallely and then combine them
#rf <- foreach(ntree = rep(200, 2), .combine = combine, .packages = "randomForest") %dopar% 
#  randomForest(damage_grade~.,data=rf_train_data,ntree=ntree, mtry=9, importance=TRUE)

bldg_eq_dmg_rf <- randomForest(damage_grade ~ ., data=rf_train_data, keep.forest=TRUE, ntree=30, mtry=9, importance=TRUE) 
print(bldg_eq_dmg_rf)

# Plotting OOB error rates
plot(bldg_eq_dmg_rf, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest Train data")

bldg_eq_dmg_rf$predicted 
bldg_eq_dmg_rf$importance 

# Evaluate variable importance
importance(bldg_eq_dmg_rf)
#why mean? Because there are many trees and this is mean across all of them.
# plot (directly prints the important attributes) 
varImpPlot(bldg_eq_dmg_rf,
           sort = T,
           main="Variable Importance",
           n.var=30)
# Variable Importance Table
var.imp <- data.frame(importance(bldg_eq_dmg_rf,
                                 type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]
imp_feature<-var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]$Variables

# Find the optimal mtry
mtry <- tuneRF(rf_train_data[c(imp_feature[1:30],'damage_grade')],rf_train_data$damage_grade, ntreeTry=30,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

#to.remove<-c(which(data.frame(bldg_eq_dmg_rf$importance)$MeanDecreaseAccuracy==min(data.frame(bldg_eq_dmg_rf$importance)$MeanDecreaseAccuracy)))
#Remove the variable with the lowest decrease in Accuracy (Least relevant variable)
#bldg_eq_dmg_rf <- randomForest(damage_grade ~ ., data=rf_train_data[-c(to.remove)], keep.forest=TRUE, ntree=30, mtry=9, importance=TRUE) 
#print(bldg_eq_dmg_rf)

bldg_eq_dmg_rf <- randomForest(damage_grade ~ ., data=rf_train_data[c(imp_feature[c(1:35,37)],'damage_grade')], keep.forest=TRUE, ntree=200, importance=TRUE) 
print(bldg_eq_dmg_rf)

# Prediction and Calculate Performance Metrics
predictions <- predict(bldg_eq_dmg_rf,rf_train_data,type='response')
library(pROC)
roc.multi <- multiclass.roc(rf_train_data$damage_grade,as.numeric(predictions))
auc(roc.multi)
rs<-roc.multi[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))


# Predict on Train data 
pred_model_train <-predict(bldg_eq_dmg_rf,rf_train_data[,-c(3)],type="response")
result_train <- table("actual _values"= rf_train_data$damage_grade,pred_model_train);result_train
# Accuracy,Precision and Recall on train
train_accuracy <- sum(diag(result_train))/sum(result_train)*100;train_accuracy

pred_model_train_prob <-predict(bldg_eq_dmg_rf,rf_train_data[,-c(3)],type="prob")
pred_model_train_prob

# scoring
rf_train_data$predict.class <- predict(bldg_eq_dmg_rf, rf_train_data, type="class")
rf_train_data$predict.score <- predict(bldg_eq_dmg_rf, rf_train_data, type="prob")
head(rf_train_data)


# Predict on Test Data
pred_model_test <-predict(bldg_eq_dmg_rf,rf_test_data,type="response")
#result_test <- table("actual _values"= rf_test_data$damage_grade,pred_model_test);result_test

rf_test_data$damage_grade <- pred_model_test
head(rf_test_data)
head(rf_test_data[,c("building_id","damage_grade")])
submission <- rf_test_data[,c("building_id","damage_grade")]

write.csv(submission, "submission.csv",row.names = FALSE)
