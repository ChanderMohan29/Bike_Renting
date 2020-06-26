# Project NAme : Bike Renting  
# submit by Chander Mohan

# clean the environment
rm(list=ls(all=T))
# set working Directory and Check working directory
setwd("E:/data science/project/Bike Renting/chander")
getwd()

# #loading Libraries

x=c("ggplot2","lattice","gridExtra","corrgram","DMwR","usdm","caret","randomForest","e1071",
    "unbalanced","caTools","C50","dummies","e1071","MASS","rpart", "doSNOW","inTrees")

install.packages(x)
lapply(x, require, character.only = TRUE)
#Loading CSV file
df_day = read.csv("day.csv", header = T)

#EXPLORING DATA 
#viewing the data

head(df_day)
dim(df_day) 
str(df_day)

#Summary of the data
summary(df_day)

#unique value of each count
apply(df_day, 2,function(x) length(table(x)))

#Missing Value Analysis
sum(is.na(df_day))

##Visualizing the data
##### Plotting "cnt" with others variables

# "instant"
ggplot(data = df_day, aes(x=instant,y=cnt))+
  geom_bar(stat = 'identity', fill = "dark violet")

# dteday
ggplot(data = df_day, aes(x=dteday,y=cnt))+
  geom_bar(stat = 'identity', fill = "navy blue")

# season
ggplot(data = df_day, aes(x=season,y=cnt))+
  geom_bar(stat = 'identity', fill = "navy blue")

# "yr"
ggplot(data = df_day, aes(x=yr,y=cnt))+
  geom_bar(stat = 'identity', fill = "navy blue")

# "mnth"
ggplot(data = df_day, aes(x=mnth, y=cnt))+
  geom_bar(stat = 'identity', fill = "blue")

# "holiday"
ggplot(data = df_day, aes(x=holiday, y=cnt))+
  geom_bar(stat = 'identity', fill = "blue")

# "holiday"
ggplot(data = df_day, aes(x=holiday, y=cnt))+
  geom_bar(stat = 'identity', fill = "blue")

# "weekday"
ggplot(data = df_day, aes(x=weekday, y=cnt))+
  geom_bar(stat = 'identity', fill = "blue")

# "workingday"
ggplot(data = df_day, aes(x=workingday, y=cnt))+
  geom_bar(stat = 'identity', fill = "blue")

# "holiday"
ggplot(data = df_day, aes(x=holiday, y=cnt))+
  geom_bar(stat = 'identity', fill = "blue")

# "weathersit"
ggplot(data = df_day, aes(x=weathersit, y=cnt))+
  geom_bar(stat = 'identity', fill = "blue")

# "temp"
ggplot(data = df_day, aes(x=temp, y=cnt))+
  geom_point(aes_string(colour = df_day$cnt), size = 0.5, shape = df_day$temp)+
  theme_bw() + ggtitle(" scatter plot Analysis")

# "atemp"
ggplot(data = df_day, aes(x=atemp, y=cnt))+
  geom_point(aes_string(colour = df_day$cnt), size = 0.5, shape = df_day$temp)+
  theme_bw() + ggtitle(" scatter plot Analysis")

# "hum"
ggplot(data = df_day, aes(x=hum, y=cnt))+
  geom_point(aes_string(colour = df_day$cnt), size = 0.5, shape = df_day$temp)+
  theme_bw() + ggtitle(" scatter plot Analysis")

# "windspeed"
ggplot(data = df_day, aes(x=windspeed, y=cnt))+
  geom_point(aes_string(colour = df_day$cnt), size = 0.5, shape = df_day$temp)+
  theme_bw() + ggtitle(" scatter plot Analysis")

# "casual"
ggplot(data = df_day, aes(x=casual, y=cnt))+
  geom_point(aes_string(colour = df_day$cnt), size = 0.5, shape = df_day$temp)+
  theme_bw() + ggtitle(" scatter plot Analysis")

# "registered"
ggplot(data = df_day, aes(x=registered, y=cnt))+
  geom_point(aes_string(colour = df_day$cnt), size = 0.5, shape = df_day$temp)+
  theme_bw() + ggtitle(" scatter plot Analysis")


#Outlier Analysis

#created numeric_columns  for numeric columns from data set

numeric_columns = c("temp","atemp","hum","windspeed","casual","registered")

## Normality check before removing outliers

for(i in 1:length(numeric_columns))
{
  assign(paste0("gn",i),ggplot(df_day,aes_string(x=numeric_columns[i]))+
           geom_histogram(fill="sky blue",colour="black")+
           geom_density()+
           theme_bw()+
           labs(x=numeric_columns[i])+
           ggtitle(paste("Histogram of ",numeric_columns[i])))
}

gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)

# No any variables are  normally distributed. 

### outlier Analysis  # Boxplot distribution and outlier check

for(i in 1:length(numeric_columns)){
  assign(paste0("gn",i),ggplot(df_day,aes_string(y=numeric_columns[i],x="cnt",
                                                  fill=df_day$Absenteeism.time.in.hours))+
           geom_boxplot(outlier.colour = "red",fill="skyblue",outlier.shape = 18,
                        outlier.size = 1,notch = F)+
           theme_bw()+
           labs(y=numeric_columns[i],x="cnt")+
           ggtitle(paste("Box Plot of cnt for",numeric_columns[i])))
}

gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)

# our new numeric variable which contains outliers-
numeric_columns_outliers = c("hum","windspeed","casual")

# Our sample data is less,  ##So applied KNN Imputation Method

for(i in numeric_columns_outliers){
  print(i)
  val=df_day[,i][df_day[,i] %in% boxplot.stats(df_day[,i])$out]
  df_day[,i][df_day[,i] %in% val]= NA
}


df_day = knnImputation(df_day , k=5)

##Now we have to check outliers again beacuse we have imputed knn
for(i in 1:length(numeric_columns)){
  assign(paste0("gn",i),ggplot(df_day,aes_string(y=numeric_columns[i],x="cnt",
                                                  fill=df_day$Absenteeism.time.in.hours))+
           geom_boxplot(outlier.colour = "red",fill="skyblue",outlier.shape = 18,
                        outlier.size = 1,notch = F)+
           theme_bw()+
           labs(y=numeric_columns[i],x="cnt")+
           ggtitle(paste("Box Plot of cnt for",numeric_columns[i])))
}
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,ncol=2)

# Almost all the Outliers have been Removed......

###Feature Selection   ## Checking  Multicollinearity

corrgram(df_day[,numeric_columns], order=F,
         lower.panel=panel.pie,text.panel=panel.txt,main="correlation plot")

# correlaion plot "temp" and "atemp" are highly correlated, so we need to remove one out of them

df_day_new = subset(df_day, select = -c(dteday,atemp))
# "dteday" is irrelevant for prediction.

# checking coliinearity with VIF
vifcor(df_day_new[,-14], th=0.95) 
#--> No variable from the 13 input variables has collinearity problem.


#########ANOVA test

## ANOVA test for Categprical variable
summary(aov(formula = cnt~instant,data = df_day_new))
##            Df    Sum Sq      Mean Sq     F value   Pr(>F)    
#instant       1  1.083e+09     1.083e+09   476.8     <2e-16 ***
#Residuals   729  1.656e+09     2.272e+06                   
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(aov(formula = cnt~season,data = df_day_new))
summary(aov(formula = cnt~yr,data = df_day_new))
summary(aov(formula = cnt~mnth,data = df_day_new))
summary(aov(formula = cnt~holiday,data = df_day_new))
summary(aov(formula = cnt~weekday,data = df_day_new))
summary(aov(formula = cnt~workingday,data = df_day_new))
summary(aov(formula = cnt~weathersit,data = df_day_new))

#Discard those variables whose p value >0.05

#############BACKWARD ELIMINATION#########################

# Run regression model again to check p value , R-squared and Adjusted R-squared
LR_model = lm(cnt~., data=df_day_new)

summary(LR_model) 
# R-squared:0.9827,
##Adjusted R-squared:  0.9824
 
### Dimension Reduction - Discard  "holiday" p>0.75014
df_day_new = subset(df_day, select = -c(dteday,atemp,holiday))

Run regression model again to check p value , R-squared and Adjusted R-squared

LR_model = lm(cnt~., data=df_day_new)

summary(LR_model) 
#R-squared: 0.9827,	
#Adjusted R-squared: 0.9824 

## Dimension Reduction - Discard  "season" p>0.71185

df_day_new = subset(df_day, select = -c(dteday,atemp,holiday,season))

#########################

# Run regression model again to check p value , R-squared and Adjusted R-squared
LR_model = lm(cnt~., data=df_day_new)

summary(LR_model) # R-squared:0.9827,	Adjusted R-squared: 0.9824 
# 
# 
# ## Dimension Reduction - Discard  "windspeed" p> 0.63997  
df_day_new = subset(df_day, select = -c(dteday,atemp,holiday,season,windspeed))

#########################

# # Run regression model again to check p value , R-squared and Adjusted R-squared
LR_model = lm(cnt~., data=df_day_new)

summary(LR_model) # R-squared:0.9827,	Adjusted R-squared: 0.9824 

# ## Dimension Reduction - Discard  "yr" p>0.64768
df_day_new = subset(df_day, select = -c(dteday,atemp,holiday,season,windspeed,yr))

#########################

# # Run regression model again to check p value , R-squared and Adjusted R-squared
LR_model = lm(cnt~., data=df_day_new)
summary(LR_model) 
#Multiple R-squared:  0.9827,	Adjusted R-squared:  0.9824 


###############Feature scaling#########################


numeric_columns = c("temp","casual","registered")
for(i in 1:length(numeric_columns))
{
  assign(paste0("gn",i),ggplot(df_day_new,aes_string(x=numeric_columns[i]))+
           geom_histogram(fill="dark red",colour="black")+
           geom_density()+
           theme_bw()+
           labs(x=numeric_columns[i])+
           ggtitle(paste("Histogram of ",numeric_columns[i])))
}
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=2)

# WE can see no any numeric variables are normally distributed

df_day_new[numeric_columns][1,] 

# Only "casual"  and  "registered" variables need to be scaled

##        temp      casual     registered
##    1   0.344167    331         654


##  Normalization for Non uniformly distributed features
new_numeric_columns = c( "casual","registered" )

for(i in new_numeric_columns){
  print(i)
  df_day_new[,i]=(df_day_new[,i]-min(df_day_new[,i]))/
    (max(df_day_new[,i]-min(df_day_new[,i])))
}



########################## Principal component Analysis############
## devide df_day into raining and test

split = sample.split(df_day_new$cnt, SplitRatio = 0.8)
training_set = subset(df_day_new, split == TRUE)
test_set = subset(df_day_new, split == FALSE)

##principal component analysis
pca = preProcess(x= training_set[-14], method = "pca", pcaComp = 14)
training_set_pca = predict(pca, training_set)
test_set_pca = predict(pca, test_set)

## Plot of explained varience of principal components 
#principal component analysis
prin_comp = prcomp(training_set_pca)
  
##compute standard deviation of each principal component
 std_dev = prin_comp$sdev
 
##compute variance
pr_var = std_dev^2

##proportion of variance explained
prop_varex = pr_var/sum(pr_var)

##cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
ylab = "Cumulative Proportion of Variance Explained",
type = "b")
  
##choosing to 10 explained varience of Principal components
pca = preProcess(x= training_set[-14], method = "pca", pcaComp = 10)
training_set = predict(pca, training_set)
training_set_pca = training_set_pca[c(2:21,1)]
test_set= predict(pca, test_set)
test_set_pca = test_set_pca[c(2:21,1)]

#Not getting  accuray and RMSE upto the mark by applying PCA. So we can comment this code


#################################Machine Learning Model####

# devide dataset into raining and test

set.seed(123)
split = sample.split(df_day_new$cnt, SplitRatio = 0.8)
training_set = subset(df_day_new, split == TRUE)
test_set = subset(df_day_new, split == FALSE)


#### Multiple Linear Regression #####

# Run Multiple Linear Regression
LR_model = lm(cnt~., data=training_set)

print(LR_model)

# Summary of the model
summary(LR_model)

#Lets predict for training data
pred_LR_train = predict(LR_model, training_set[,names(training_set) != "cnt"])

#Lets predict for testing data
pred_LR_test = predict(LR_model, test_set[,names(test_set) != "cnt"])


###  Error Matrics

# For training data 
print(postResample(pred = pred_LR_train, obs = training_set$cnt))
#                   RMSE        Rsquared       MAE  
#              265.3598716     0.9807779    149.1781339   

# For testing data 
print(postResample(pred = pred_LR_test, obs = test_set$cnt))
#                  RMSE        Rsquared       MAE 
#               212.2170503    0.9892882    143.8525429    

regr.eval(test_set$cnt, pred_LR_test, stats = c('mape',"mse"))
#                 mape        mse 
#             4.133941e-02  4.503608e+04   



###################  Decision Tree #####################################

## rpart for regression
DT_model= rpart(cnt ~ ., data = training_set, method = "anova")

summary(DT_model)

#write rules into disk
write(capture.output(summary(DT_model)), "Rules.txt")


#Lets predict for training data
pred_DT_train = predict(DT_model, training_set[,names(training_set) != "cnt"])

#Lets predict for training data
pred_DT_test = predict(DT_model,test_set[,names(test_set) != "cnt"])


# For training data 
print(postResample(pred = pred_DT_train, obs = training_set$cnt))
#                  RMSE     Rsquared       MAE 
#             503.005411   0.930932   376.613617  

# For testing data 
print(postResample(pred = pred_DT_test, obs = test_set$cnt))
#             RMSE         Rsquared       MAE 
#           598.3854972    0.9132051   463.0741917   

regr.eval(test_set$cnt, pred_DT_test, stats = c('mape',"mse"))
#               mape         mse 
#            1.274244e-01   3.580652e+05



#################  Random Forest ############################
# Fitting Random Forest Regression to the dataset

set.seed(1234)
RF_model= randomForest(x = training_set[,names(training_set) != "cnt"],
                       y = training_set$cnt,
                       ntree = 500)

#Lets predict for training data
pred_RF_train = predict(RF_model, training_set[,names(training_set) != "cnt"])

#Lets predict for testing data
pred_RF_test = predict(RF_model, test_set[,names(test_set) != "cnt"])

# For training data 
print(postResample(pred = pred_RF_train, obs = training_set$cnt))
#                    RMSE        Rsquared       MAE   
#                 129.4719764   0.9960124  77.5708665

# For testing data 
print(postResample(pred = pred_RF_test, obs = test_set$cnt))
#                    RMSE      Rsquared        MAE 
#                 240.1007197   0.9865681 165.6189671  


regr.eval(test_set$cnt, pred_RF_test, stats = c('mape',"mse"))
#                 mape         mse 
#              4.883611e-02   5.764836e+04     



#############Support Vector Regression #

# Fitting SVR to the dataset
SVR_model = svm(formula = cnt ~ .,
                data = training_set,
                type = 'eps-regression',
                kernel = 'radial')


#Lets predict for training data
pred_SVR_train = predict(SVR_model, training_set[,names(training_set) != "cnt"])

#Lets predict for testing data
pred_SVR_test = predict(SVR_model, test_set[,names(test_set) != "cnt"])


### Error Matrics
# For training data 
print(postResample(pred = pred_SVR_train, obs = training_set$cnt))
#  RMSE        Rsquared       MAE    
# 218.790003    0.987101  138.842426    


# For testing data 
print(postResample(pred = pred_SVR_test, obs = test_set$cnt))
# RMSE        Rsquared       MAE 
# 221.1328272   0.9880324  159.1453353  --> var = 10 (backward elimination)

regr.eval(test_set$cnt, pred_SVR_test, stats = c('mape',"mse"))
#    mape        mse 
# 4.915313e-02  4.889973e+04  --> var = 10 (backward elimination) 


###################Model selection######

#                              RMSE train      RMSE test         difference     R-squqare
# Multiple Linear Regression: 265.3598716     212.2170503         -53.142       0.9807779
# Decision Tree             : 503.005411      598.3854972          95.38        0.9821495
# Random Forest             : 129.4719764     240.1007197          110.629      0.9865681
# SVR                       : 218.790003      221.1328272          2.342        0.9880324


# SVR has less difference of RMSE test and RMSE train value as well as R^2 is also maximum.
# Therefore, SVR is working as a better pridictive model...


# writing csv file
write.csv(df_day_new,"dataset_output in R.csv", row.names = F)
