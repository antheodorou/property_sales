#Read the dataset
require(foreign)
houses <- read.csv2("ames_iowa_housing_53.csv", header = T)
houses <- as.data.frame(houses)
str(houses)
#####Exploratory Data and Descriptive Analysis######
houses$X <- NULL #same as Order, just the identification number
houses[,sapply(houses, class) == "character"] <- lapply(houses[,sapply(houses, class) == "character"], factor)
houses[,sapply(houses, class) == "integer"] <- lapply(houses[,sapply(houses, class) == "integer"], as.numeric)
houses$MS.SubClass <- as.factor(houses$MS.SubClass)
houses$Overall.Cond <- as.factor(houses$Overall.Cond)
houses$Overall.Qual <- as.factor(houses$Overall.Qual)
#Drop NAs from numeric variables and create for factors a new level "No"
for (i in colnames(houses)) {
  if (sapply(houses[i], class) == "numeric"){
    houses[,i][which(is.na(houses[,i]))] <- 0
  }
  if (sapply(houses[i], class) == "factor"){
    levels(houses[,i]) <- c(levels(houses[,i]),"No")
    houses[,i][is.na(houses[,i])] <- "No"
  }
}
library(psych)
housesnum <- houses[, sapply(houses, class) == "numeric"]
round(t(describe(housesnum)), 2)
#histograms for numeric variables
par(mfrow = c(2,3))
for (i in colnames(housesnum)) {
  hist(housesnum[,i], probability = T, main=names(housesnum)[i], xlab = names(housesnum[i]))
  lines(density(housesnum[,i]), col=2)
  qqnorm(housesnum[,i], main = names(housesnum)[i], xlab = names(housesnum[i]))
  qqline(housesnum[,i])
}
#boxplots for factors
housesfac <- houses[, sapply(houses, class) != "numeric"]
par(mfrow = c(2,3))
for (i in colnames(housesfac)) {
  boxplot(houses$SalePrice ~ housesfac[,i], xlab = names(housesfac[i]))
}
#outliers
for (i in colnames(housesnum)){
  y1<-housesnum[,i]
  out <- boxplot(y1, plot=FALSE )$out
  if(length(out)!=0){
    print('-------------------------------------------------------')
    print( paste('Outliers for variable', i) )
    print( paste(length(out), 'outliers') )
    print( paste(round(100*length(out)/sum(!is.na(y1)),1), '% outliers', sep='' ) )
    print(which( y1 %in% out ))
  }
}
#Associations between numeric variables
par(mfrow = c(1,1))
require(corrplot)
#turn into a 3-column table
corr <- as.data.frame(as.table(cor(housesnum)))
#remove the NA values from above 
corr <- subset(corr, abs(Freq) > 0.5)#select significant values
corr[corr == 1] <- NA 
corr <- na.omit(corr) #drop NAs
corr <- corr[order(-abs(corr$Freq)),] #sort by highest correlation
#turn corr back into matrix in order to plot with corrplot
mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ", method = "number", tl.cex=0.7, number.cex=0.6)
####### SAMPLE/TEST DATASET ##########
require(foreign)
houses_test <- read.csv2("ames_iowa_housing_test.csv", header = T)
houses_test <- as.data.frame(houses_test)
str(houses_test)
#Same EDA as the "houses" data-frame
houses_test$X <- NULL
houses_test[,sapply(houses_test, class) == "character"] <- lapply(houses_test[,sapply(houses_test, class) == "character"], factor)
houses_test[,sapply(houses_test, class) == "integer"] <- lapply(houses_test[,sapply(houses_test, class) == "integer"], as.numeric)
houses_test$MS.SubClass <- as.factor(houses_test$MS.SubClass)
houses_test$Overall.Cond <- as.factor(houses_test$Overall.Cond)
houses_test$Overall.Qual <- as.factor(houses_test$Overall.Qual)
#Drop NAs from numeric variables
for (i in colnames(houses_test)) {
  if (sapply(houses_test[i], class) == "numeric"){
    houses_test[,i][which(is.na(houses_test[,i]))] <- 0
  }
}
#Create for NAs a new level "No"
for (i in colnames(houses_test)) {
  if (sapply(houses_test[i], class) == "factor"){
    levels(houses_test[,i]) <- c(levels(houses_test[,i]),"No")
    houses_test[,i][is.na(houses_test[,i])] <- "No"
  }
}
###Edit the "houses_test" dataset in order to use it later in model evaluation
debug_contr_error(houses_test) # show the levels of every function
#remove the factor "Utilities" because it has 2 factors and is a problem while fitting the model
houses_test1 <- houses_test[-match("Utilities", colnames(houses_test))] 

testmod <- lm(SalePrice ~., data = houses_test1, na.action = 'na.omit' )
X1 <- model.matrix(testmod)
######Fitting the regression model######
#LASSO
require(glmnet)
#Linear model with all the variables of the data-frame
model0 <- lm(SalePrice ~ ., data = houses, na.action='na.omit')
summary(model0)
X <- model.matrix(model0)[,-1]
lasso <- glmnet(X, houses$SalePrice)
plot(lasso, xvar = "lambda", label = T)
lasso1 <- cv.glmnet(X, houses$SalePrice, alpha = 1)
plot(lasso1); plot(lasso1$glmnet.fit, xvar = "lambda")
abline(v=log(c(lasso1$lambda.min, lasso1$lambda.1se)), lty =2)
m <- coef(lasso1, s = "lambda.1se")
m <- m[m[,1]!=0,]
#Drop the columns that LASSO rejected
housesmod <- data.frame(X[,1])
for (i in colnames(X)) {
  for (j in names(m)) {
    if (i == j){
      k <- match(j, colnames(X))
      housesmod[colnames(X)[k]] <- X[,k]
    }
  }
}
housesmod$X...1. <- NULL
housesmod["SalePrice"] <- houses$SalePrice
#Linear model with less variables than full model - They have been dropped with LASSO and AIC
model01 <- lm(SalePrice ~ ., data = housesmod, na.action='na.omit')
summary(model01)
model01 <- step(model01, direction = "both") #AIC
summary(model01)
update(model01)
summary(model01)
#Drop the columns that AIC rejected
housesmod <- data.frame(X[,1])
for (i in colnames(X)) {
  for (j in names(model01$coefficients)) {
    if (i == j){
      k <- match(j, colnames(X))
      housesmod[colnames(X)[k]] <- X[,k]
    }
  }
} 
housesmod$X...1. <- NULL
housesmod["SalePrice"] <- houses$SalePrice

model01 <- lm(SalePrice ~ ., data = housesmod, na.action='na.omit')
summary(model01)
model01 <- lm(SalePrice ~ .-Mas.Vnr.Area - Bsmt.ExposureNo - BsmtFin.Type.1GLQ, data = housesmod, na.action='na.omit')
summary(model01)

housesmod$Mas.Vnr.Area <- NULL
housesmod$Bsmt.ExposureNo <- NULL
housesmod$BsmtFin.Type.1GLQ <- NULL
model01 <- lm(SalePrice ~ ., data = housesmod, na.action='na.omit')
summary(model01)
assum_fun(model01)
#####FIXING THE ASSUMPTION #########
par(mfrow = c(1,1))
require(car)
#Find what method to use for fixing the normality
symbox(~ SalePrice, data=housesmod) #BOX-COX(-0.5) or log
summary(powerTransform(SalePrice ~ ., data = housesmod, family="yjPower")) #closes to 0. So, log!!!!

###########logmodel#############
logmodel <- lm(log(SalePrice) ~ ., data = housesmod, na.action='na.omit')
summary(logmodel)
##################No2-LASSO#######################
X <- model.matrix(logmodel)[,-1]
lasso <- glmnet(X, log(houses$SalePrice))
plot(lasso, xvar = "lambda", label = T)
lasso1 <- cv.glmnet(X, log(houses$SalePrice), alpha = 1)
plot(lasso1)
plot(lasso1$glmnet.fit, xvar = "lambda")
abline(v=log(c(lasso1$lambda.min, lasso1$lambda.1se)), lty =2)
m <- coef(lasso1, s = "lambda.1se")
m <- m[m[,1]!=0,]
#Drop the columns that LASSO rejected
housesmod1 <- data.frame(X[,1])
for (i in colnames(X)) {
  for (j in names(m)) {
    if (i == j){
      k <- match(j, colnames(X))
      housesmod1[colnames(X)[k]] <- X[,k]
    }
  }
}
housesmod1$X...1. <- NULL
housesmod1["SalePrice"] <- houses$SalePrice

logmodel <- lm(log(SalePrice) ~ ., data = housesmod1, na.action='na.omit')
summary(logmodel)
logmodel <- step(logmodel, direction = "both")
summary(logmodel)
update(logmodel)
summary(logmodel)

housesmod1 <- data.frame(X[,1])
for (i in colnames(X)) {
  for (j in names(logmodel$coefficients)) {
    if (i == j){
      k <- match(j, colnames(X))
      housesmod1[colnames(X)[k]] <- X[,k]
    }
  }
}
housesmod1$X...1. <- NULL
housesmod1["SalePrice"] <- houses$SalePrice

logmodel <- lm(log(SalePrice) ~ ., data = housesmod1, na.action='na.omit')
summary(logmodel)
assum_fun(logmodel)

logmodel <- lm(log(SalePrice) ~ .  + log(Gr.Liv.Area) + poly(BsmtFin.SF.1,2),
               data = housesmod1, na.action='na.omit')
summary(logmodel)
assum_fun(logmodel)
#center to zero the intercept
housesmod2 <- as.data.frame(scale(housesmod1, center = TRUE, scale = F))
housesmod2$SalePrice <- houses$SalePrice
#####cook's test for infuential points#######
critical.value <- 4/(nrow(housesmod2)- length(logmodel$coef))
par(mfrow = c(1,1))
plot(cooks.distance(logmodel), main='Data with influential point',
     ylab="Cook's distance", ylim=range(c(0,cooks.distance(logmodel), critical.value )) )
abline(h=critical.value,col=2,lty=2)
length(which(cooks.distance(logmodel) > critical.value))
#drop this outlier because it affects a lot the outcome of our assumptions
housesmod2 <- housesmod2[-1077,]
##################FINAL#####################
logmodel1 <- lm(log(SalePrice) ~ .  -Gr.Liv.Area -Fireplaces - Kitchen.AbvGr -BsmtFin.SF.1 - Heating.QCTA
               + I(Garage.Cars^4)  - Kitchen.QualTA, data = housesmod2, na.action='na.omit')
summary(logmodel1)
assum_fun(logmodel1)
houses[1077,]
#use bootstrap to confirm the coefficients' values
set.seed(3435) # for reproducibility
bootCase(logmodel1, B=999)
#Prediction with the test sample
#change the factor's level because model matrix remove the "Overall.Cond3" that we our model has
library(DMwR)
require(utils)
houses_test1$Overall.Cond <- factor(houses_test1$Overall.Cond, levels = c("No", "4", "3", "5", "6", "7", "8", "9"))
testmod <- lm(SalePrice ~., data = houses_test1, na.action = 'na.omit' )
X1 <- as.data.frame(model.matrix(testmod))
X1 <- X1[,which(colnames(X1) %in% colnames(housesmod2))]
X1["SalePrice"] <- houses_test1$SalePrice
colnames(X1) == colnames(housesmod2)
pred <- predict(logmodel1, newdata = X1)
regr.eval(pred, houses_test1$SalePrice) #measurements of the prediction
true_r2 <- 1-sum(logmodel1$residuals^2)/sum((log(houses$SalePrice)-mean(log(houses$SalePrice)))^2)
true_r2 #true R^2
#check multicollinearity
vif(logmodel1)
#cross validation
#leave one out CV
require(caret)
require(tidyverse)
train.controlLO <- trainControl(method = "LOOCV")
modello <- train(log(SalePrice) ~ . -Gr.Liv.Area -Fireplaces - Kitchen.AbvGr -BsmtFin.SF.1 - Heating.QCTA
               + I(Garage.Cars^4) - Kitchen.QualTA, data = housesmod2,
               method = "lm", trControl = train.controlLO)
modello
#k-fold 
set.seed(123) 
train.controlcv <- trainControl(method = "cv", number = 10)
modelcv <- train(log(SalePrice) ~ . -Gr.Liv.Area -Fireplaces - Kitchen.AbvGr -BsmtFin.SF.1 - Heating.QCTA
               + I(Garage.Cars^4)  - Kitchen.QualTA, data = housesmod2,
               method = "lm", trControl = train.controlcv)
modelcv
##################COMPARISONS################
##another model###
logm2 <- lm(log(SalePrice) ~ . +I(Gr.Liv.Area^3)- Kitchen.AbvGr -BsmtFin.SF.1 - Heating.QCTA
             + I(Garage.Cars^2) + I(Year.Remod.Add^2) - Kitchen.QualTA, data = housesmod2)
assum_fun(logm2)
summary(logm2)

modellol2 <- train(log(SalePrice) ~ . +I(Gr.Liv.Area^3)- Kitchen.AbvGr -BsmtFin.SF.1 - Heating.QCTA
                   + I(Garage.Cars^2) + I(Year.Remod.Add^2) - Kitchen.QualTA, data = housesmod2,
                  method = "lm", trControl = train.controlLO)
modellol2
modelcvl2 <- train(log(SalePrice) ~ . +I(Gr.Liv.Area^3)- Kitchen.AbvGr -BsmtFin.SF.1 - Heating.QCTA
                   + I(Garage.Cars^2) + I(Year.Remod.Add^2) - Kitchen.QualTA, data = housesmod2,
                  method = "lm", trControl = train.controlcv)
modelcvl2

predl2 <- predict(logm2, newdata = X1) #test 
regr.eval(predl2, houses_test1$SalePrice)
##logmodel-- initial
modellol <- train(log(SalePrice) ~ ., data = housesmod2,
                 method = "lm", trControl = train.controlLO)
modellol
modelcvl <- train(log(SalePrice) ~ . , data = housesmod2,
                 method = "lm", trControl = train.controlcv)
modelcvl

predl <- predict(logmodel, newdata = X1)
regr.eval(predl, houses_test1$SalePrice)
####null model
modellonul<- train(SalePrice ~ ., data = housesmod,
                  method = "lm", trControl = train.controlLO)
modellonul
modelcvnul <- train(SalePrice ~ . , data = housesmod,
                  method = "lm", trControl = train.controlcv)
modelcvnul

housesmod$Condition.2PosN <- NULL
model001 <- lm(SalePrice ~., data = housesmod)

X1 <- as.data.frame(model.matrix(testmod))
X1 <- X1[,which(colnames(X1) %in% colnames(housesmod))]
X1["SalePrice"] <- houses_test1$SalePrice
colnames(X1) == colnames(housesmod)
prednul <- predict(model001, newdata = X1)
regr.eval(prednul, houses_test1$SalePrice)

###Typical Profile - Centering to Mean####
center_scale <- function(x) {
  scale(x, scale = FALSE)
}
cm <- lm(SalePrice ~., data = as.data.frame(center_scale(housesmod)))
summary(cm)
assum_fun(cm)
