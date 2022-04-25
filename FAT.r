## Lab 1 - Regression

df = read.csv2("dataset.csv",sep=",")
names(df)
names(df) = c("X","Y")
names(df)
head(df)
plot(df$X,df$Y)
model1 = lm(Y~X,data = df)
summary(model1)## P- value < 0.05 = statistically significant 
abline(model1, col = "blue")

test_x = c(300,750,510)
test_df = data.frame(test_x)
R = predict(model1, X=test_df)
R
summary(R)

## For x = 1200

res= predict(model1, data.frame(X = 1200))
res
summary(res)


## Lab 2 - STationary time series

#Jasjeet Singh Siddhu 19BCE1423
setwd("D:/Rstudio/6th Sem/19BCE1423_Jasjeet Lab 2")
getwd()
rm(list = ls())
library(forecast)
library(tseries)
Palmoil = read.csv("Palmoil.csv")
class(Palmoil)
potime = ts(Palmoil$Dollar,start = 1, end = 128, frequency = 4)
class(potime)
plot(potime)

acf(potime)
pacf(potime)
adf.test(potime) # < 0.05 = accept alternate hypothesis and reject null hypo
# Here 0.01 = alternate hypo = stationary time series

mypo = auto.arima(potime, ic = "aic", trace = T)
mypo
acf(ts(mypo$residuals))
myfc = forecast(mypo,level = c(95),h=10*4)
myfc
plot(myfc)


## Lab 3 - Non stationary time series

#Jasjeet Singh Siddhu 19BCE1423
setwd("D:/Rstudio/6th Sem/19BCE1423_Jasjeet Lab 3")
getwd()
rm(list = ls())
library(forecast)
library(tseries)
data = read.csv("Tractor-sales.csv")
class(data)
data = ts(data$Number.of.Tractor.Sold,start = c(2003,1),frequency = 12)
class(data)

plot(data, xlab = "Years", ylab = 'Tractor Sales')

plot(diff(data),ylab = 'Differenced Tractor Sales')

ld = log10(data)
plot(ld, ylab = 'Log (Tractor Sales)')

dld = diff(ld)
plot(dld, ylab = 'Differenced Log (Tractor Sales)')

adf.test(dld)

par(mfrow = c(1,2)) # to analyze multiple graphs in a asingle window

acf(ts(dld),main = 'ACF Tractor Sales')
pacf(ts(dld), main = 'PACF Tractor Sales')

ARIMAfit = auto.arima(dld, ic = "aic", trace = T)

## ARIMA(0,0,1)(0,1,1) = means that can be solved by ARMA model alone or 0,1,1 arima model

summary(ARIMAfit)

fc = forecast(ARIMAfit, level = c(95), h = 3*12)

plot(fc)


## Lab 4 - One way anova 

#Jasjeet Singh Siddhu 19BCE1423
setwd("D:/Rstudio/6th Sem/19BCE1423_Jasjeet_Lab 4")
getwd()
rm(list = ls())
data = read.csv("onewayanova_machine.csv")
data
summary(data)
boxplot(data,ylab = "Diameter", xlab = "machines", col="green")
stacked_groups = stack(data)
names(stacked_groups) = c("Diameter","Machines")
stacked_groups
anova_results = aov(Diameter~Machines, data=stacked_groups)
summary(anova_results)
tk = TukeyHSD(anova_results)#Tukey Honest significance test
# to find significant difference between means of any 2 grps
#lwr, upr: the lower and upper end pointof confidence interval at 95%
# p-adj : p-valye after adjustment for multiple comparisons

tk
plot(tk,col="blue")


## Lab 5 - Logistic Regression

setwd("D:/Rstudio/6th Sem/19BCE1423_Jasjeet Lab 5")
getwd()
rm(list = ls())
data = read.csv("admission.csv")
head(data)
summary(data)
hist(data$admit,col="green")
data$admit=as.factor(data$admit)
summary(data)

library(caret)
train_ind = createDataPartition(data$admit,p=0.80,list=F)
train_ds = data[train_ind,]
test_ds=data[-train_ind,]
model = glm(admit~.,data = train_ds,family="binomial")
summary(model)


pred_train = predict(model,train_ds,type="response")
pred_train

pred_train = ifelse(pred_train >0.5,1,0)
tab1 = table(Predicted = pred_train, Actual = train_ds$admit)
tab1
1 - sum(diag(tab1))/sum(tab1)
pred_test = predict(model, test_ds, type = 'response')
pred_test
pred_test = ifelse(pred_test>0.5,1,0)
tab2 = table(Predicted = pred_test, Actual = test_ds$admit)
tab2
1-sum(diag(tab2))/sum(tab2)

## Lab 6 - Kmeans

setwd("D:/Rstudio/6th Sem/19BCE1423_Jasjeet Lab 6")
getwd()
rm(list = ls())
library(factoextra)
library(cluster)
df = USArrests
df = na.omit(df)
df = scale(df)
head(df)


# Determine the optimal no of clusters = k

#plot number of clusters vs total within sum of squares (Elbow method)

fviz_nbclust(df, kmeans, method = "wss")

set.seed(1)

km = kmeans(df, centers=4,nstart=25)

km

fviz_cluster(km,data=df)
final_data = cbind(USArrests, cluster= km$cluster)
head(final_data)

## Seeds dataset


df = read.csv("seeds_K Means.csv")
head(df)
df = na.omit(df)
df = df[-c(1,9)]
df = scale(df)
head(df)



fviz_nbclust(df, kmeans, method = "wss")

set.seed(1)

km = kmeans(df, centers=3,nstart=25)

km
fviz_cluster(km,data=df)


## Lab 7 - Kmeadoids clustering

library(factoextra)
library(cluster)
df <- USArrests

#remove rows with missing values
df <- na.omit(df)

#scale each variable to have a mean of 0 and sd of 1
df <- scale(df)

#view first six rows of dataset
head(df)
#to determine optimal k, 2 methods - elbow method and gap statistic
fviz_nbclust(df, pam, method = "wss")

set.seed(1)

#perform k-medoids clustering with k = 4 clusters
kmed <- pam(df, k = 4)

#view results
kmed
fviz_cluster(kmed, data = df)

final_data <- cbind(USArrests, cluster = kmed$cluster)

#view final data
head(final_data)

## Lab 8 - Gradient descent optimization

# first
# Gradient Descent - Optimization
#Minimize the function f(x) = 1.2 * (x-2)^2 + 3.2
#Basic calculus requires that we find the 1st derivative, and 
#solve for the value of x such that f'(x) = 0. 
#This is easy enough to do, f'(x) = 2*1.2*(x-2)
# step factor/learning rate = 0.05, initial x value= 0.1
#finds the values of x that minimize the function above, 
#and plots the progress of the algorithm with each iteration.
rm(list=ls())
# create a sequence of elements in a Vector
#to generate sequences when plotting the axes of figures or simulating data. 

xs <- seq(0,4,len=20) 
xs

# define the function we want to optimize

f <-  function(x) {1.2 * (x-2)^2 + 3.2}

# plot the function 
plot(xs , f (xs), type="l",xlab="x",ylab=expression(1.2(x-2)^2 +3.2)) 

# calculate the gradient df/dx

grad <- function(x){
  1.2*2*(x-2)
}

# df/dx = 2.4(x-2), if x = 2 then 2.4(2-2) = 0
# The actual solution we will approximate with gradient descent
# is  x = 2 as depicted in the plot below
# gradient descent implementation
x <- 0.1 # initialize the first guess for x-value
xtrace <- x  # store x -values for graphing purposes (initial)
ftrace <- f(x) # store y-values (function evaluated at x) for graphing purposes (initial)
stepFactor <- 0.01 # learning rate 'alpha'
for (step in 1:5000) {
  x <- x - stepFactor*grad(x) # gradient descent update
  xtrace <- c(xtrace,x) # update for graph
  ftrace <- c(ftrace,f(x)) # update for graph
}

lines ( xtrace , ftrace , type="b",col="blue") # type=b (both points & lines)
text (0.5,6, "Gradient Descent",col="red",pos= 4)

# print final value of x
print(x) # x converges to 2.0
text(2,4,"x=2",col="red",pos=1)
text(2,4,"(Global minimum)",col="red",pos=3)

# second
xs <- seq(-0.5,0.2,len=20) 
xs

# define the function we want to optimize

f <-  function(x) {3*x^2 + 2*x + 1}

# plot the function 
plot(xs , f (xs), type="l",xlab="x",ylab=expression(3*x^2 + 2*x + 1)) 

# calculate the gradient df/dx

grad <- function(x){
  6*x + 2
}

# df/dx = 2.4(x-2), if x = 2 then 2.4(2-2) = 0
# The actual solution we will approximate with gradient descent
# is  x = 2 as depicted in the plot below
# gradient descent implementation
x <- 0.1 # initialize the first guess for x-value
xtrace <- x  # store x -values for graphing purposes (initial)
ftrace <- f(x) # store y-values (function evaluated at x) for graphing purposes (initial)
stepFactor <- 0.01 # learning rate 'alpha'
for (step in 1:5000) {
  x <- x - stepFactor*grad(x) # gradient descent update
  xtrace <- c(xtrace,x) # update for graph
  ftrace <- c(ftrace,f(x)) # update for graph
}

lines ( xtrace , ftrace , type="b",col="blue") # type=b (both points & lines)
text (0.5,6, "Gradient Descent",col="red",pos= 1)

# print final value of x
print(x) # x converges to -0.333
text(-0.333,1,"x=-0.333",col="red",pos=1)
text(-0.333,1,"(Global minimum)",col="red",pos=3)


## Lab 9 - Hiearchical Clustering

# complete - 
rm(list=ls())
#install.packages("factoextra")
#install.packages("cluster")
library(factoextra)
library(cluster)
#**************
#Load and preparing the data
#**************
df <- USArrests  
df <- na.omit(df)
head(df) 
df <- scale(df) # scaling to normalize the values >>>> x-xbar/SD
head(df)
#********************

# Hierachical clustering "complete linkage"
#********************
dist_mat <- dist(df, method = 'euclidean')
hclust_complete <- hclust(dist_mat, method = 'complete')
plot(hclust_complete,cex=0.5, hang = -1) 
# hang = A negative value will cause the labels to hang down from 0.
# cex= font size 
cut<- cutree(hclust_complete, k = 4)
abline(h =4 , col = 'red')
rect.hclust(hclust_complete , k = 4, border = 2:5)

# single
dist_mat <- dist(df, method = 'euclidean')
hclust_complete <- hclust(dist_mat, method = 'single')
plot(hclust_complete,cex=0.5, hang = -1) 
# hang = A negative value will cause the labels to hang down from 0.
# cex= font size 
cut<- cutree(hclust_complete, k = 4)
abline(h =4 , col = 'red')
rect.hclust(hclust_complete , k = 4, border = 2:5)


## Lab 10 - Random Forest

mydata = read.csv("diabetes.csv")
#View(mydata)
mydata$Outcome = as.factor(mydata$Outcome)
index=sample(2,nrow(mydata), replace=TRUE,prob=c(0.9,0.1))
training=mydata[index==1,]
testing=mydata[index==2,]
RFM <- randomForest(Outcome ~ .,data=training, importance=T, proximity=T)
Dia_Pred=predict(RFM,testing)
testing$Out=Dia_Pred
View(testing)

CFM=table(testing$Outcome,testing$Out)
CFM

library(caret) 
confusionMatrix(CFM)