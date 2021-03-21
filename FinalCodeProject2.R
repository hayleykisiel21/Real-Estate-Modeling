# MY CODE
# HOUSE SALES IN KING COUNTY

# Predicting the price of a house in King George County 

#######################
## DATA AND PACKAGES ##
#######################

# libraries 
install.packages("car")
install.packages("gridExtra")
install.packages("caret")
install.packages("dplyr")
install.packages("broom")
install.packages("leaflet")
install.packages("glmnet")
install.packages("ROCR")
install.packages("olsrr")


library(leaps)
library(car)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ROCR)
library(glmnet)
library(caret)
library(dplyr)
library(broom)
library(leaflet)
library(olsrr)



# read in data 
data <- read.csv("kc_house_data.csv", header = TRUE)
data$price.log = (log(data$price))^.7
data$waterfront = as.factor(data$waterfront)
data$view = as.factor(data$view)
data$grade = as.factor(data$grade)
data$condition = as.factor(data$condition)

# df with log(price)
df <- subset(data,select=-c(date, id, zipcode, price))

attach(df)

#check for missing values (there are 0)
NA_values=data.frame(no_of_na_values=colSums(is.na(df)))
head(NA_values,18)

###################
## DATA ANALYSIS ##
###################

# price statistics with transformation

summary(data$price)
histogram(data$price,col="orange")
histogram(log(data$price)^1.2,col="orange")

# coorelation matrix between all variables 
cor_data=data.frame(df[,1:18])
cor_data=subset(cor_data, select = -c(6))
correlation=cor(cor_data)
par(mfrow=c(1, 1))
corrplot(correlation,method="color")

# according to the corrplot, price is positively correlated with:
# bedroom, bathroom, Sqft_living, view , grade, sqft_above, sqft_basement, lat, sqft_living 15

# scatterplots to determine relationships between highly correlated variables
p1=ggplot(data = df, aes(x = bedrooms, y = price.log)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Bedrooms and Price", x="bedrooms",y="Price")
p2=ggplot(data = df, aes(x = bathrooms, y = price.log)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Bathrooms and Price", x="bathrooms",y="Price")
p3=ggplot(data = df, aes(x = sqft_living, y = price.log)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Sqft_living and Price", x="Sqft_living",y="Price")
p4=ggplot(data = df, aes(x = sqft_above, y = price.log)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Sqft_above and Price", x="Sqft_above",y="Price")
p5=ggplot(data = df, aes(x = sqft_basement, y = price.log)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Sqft_basement and Price", x="Sqft_basement",y="Price")
p6=ggplot(data = df, aes(x = lat, y = price.log)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Latitude and Price", x="Latitude",y="Price")
p7=ggplot(data = df, aes(x = sqft_living15, y = price.log)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Sqft_living15 and Price", x="Sqft_living15",y="Price")
grid.arrange(p1,p2,p3,p4,p5,p6,p7,nrow=3)

# boxplots for categorical variables: view and grade

par(mfrow=c(1, 2))
boxplot(price.log~view,data=df,main="Different boxplots", xlab="view",ylab="price",col="orange",border="blue")
boxplot(price.log~grade,data=df,main="Different boxplots", xlab="grade",ylab="price",col="orange",border="blue")

# all have linear relationships

# looking at bedroom correlation
ggplot(data = df, aes(x = bedrooms, y = price.log)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Bedrooms and Price", x="bedrooms",y="Price")

# removed outlier with 33 bedrooms - clearly influenced slope 
df <- df[-c(15871),] 

# new bedroom correlation without outlier
ggplot(data = df, aes(x = bedrooms, y = price.log)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Bedrooms and Price", x="bedrooms",y="Price")

# both also have linear relationship

#########
## MAP ##                                                  
#########

coordinates_data <- dplyr::select(df, price.log, lat, long)
head(coordinates_data)

pal = colorNumeric("YlOrRd", domain = coordinates_data$price.log)
int_map <- coordinates_data %>%
  leaflet()%>%
  addProviderTiles(providers$OpenStreetMap.Mapnik)%>%
  addCircleMarkers(col = ~pal(price.log), opacity = 1.5, radius = 0.3) %>% 
  addLegend(pal = pal, values = ~price.log) 

int_map   
##############
##Simple Linear Regression
##############

## Before starting any linear analysis check to see regression assumptions are met on the original data 
data2 <- read.csv("kc_house_data.csv", header = TRUE)
orig<- lm((data2$price)~(data2$sqft_living))
boxplot((data2$price)~(data2$sqft_living)) #Looks exponential might need a log transformaiton
#Initial sssumption plots
par(mfrow=c(2,2))
plot((data2$sqft_living), (data2$price), main="Plot of price Against sqft_living") #Looks exponential might need a log transformaiton
abline(orig,col="red")
plot(orig$fitted.values,orig$residuals, main="Plot of residuals against fits")
abline(h=0,col="red")
acf(orig$residuals, main="ACF of Residuals",ylim=c(-0.012,0.1))
qqnorm(orig$residuals)
qqline(orig$residuals, col="red")
library(MASS)
boxcox(orig, lambda = seq(-0.2,.35,.025))
#First transformation; log of response
secondfit<- lm(log(data2$price)~(data2$sqft_living))
boxcox(secondfit, lambda = seq(.2,1.3,.025))
#First transformation assumption plots
par(mfrow=c(2,2))
plot((data2$sqft_living), log(data2$price), main="Plot of log(price) Against sqft_living") #Looks exponential might need a log transformaiton
abline(secondfit,col="red")
plot(secondfit$fitted.values,secondfit$residuals, main="Plot of residuals against fits")
abline(h=0,col="red")
acf(secondfit$residuals, main="ACF of Residuals",ylim=c(-0.012,0.1))
qqnorm(secondfit$residuals)
qqline(secondfit$residuals, col="red")
#second transformation; log of predictor variable
result<- lm(log(data2$price) ~log(data2$sqft_living))
boxplot(log(data2$price)~log(data2$sqft_living))
#Second transformation assumption plots 
plot(log(data2$sqft_living), log(data2$price), main="Plot of log(price) Against log(sqft_living)") #looks linear
abline(result,col="red")
plot(result$fitted.values,result$residuals, main="Plot of residuals against fits") #Looks great no pattern randomly scattered around 0
abline(h=0,col="red")
acf(result$residuals, main="ACF of Residuals",ylim=c(-0.012,0.1))
qqnorm(result$residuals)
qqline(result$residuals, col="red")
library(MASS)
boxcox(result, lambda = seq(-2,2,.025)) #Need a negative 1 exponent transformation to the price variable
#Third transformation plots
tf.price <- log(data2$price)** -1
result.tf <- lm(tf.price~log(data2$sqft_living))
boxcox(result.tf)
plot(log(data2$sqft_living), tf.price, main="Plot of price against bedrooms")
abline(result.tf,col="red")
plot(result.tf$fitted.values,result.tf$residuals, main="Plot of residuals against fits")
abline(h=0,col="red")
acf(result.tf$residuals, main="ACF of Residuals",ylim=c(-0.012,0.1))
qqnorm(result.tf$residuals)
qqline(result.tf$residuals, col="red")
## Simple Linear Regression with predictor: sqft_living; response: price
## Ho: b1 = 0. There is a not a linear relationship between measurement of house and price
## Ha: b1 != 0. There is a linear relationship between measurement of house and price
summary(result.tf)
## p-value < 2.2e-16
## Since p<0.05 we can reject the null hypothesis and accept the alternative
## hypothesis that there is a linear relationship between measurement of house and price
## Estimated linear regression equation for price against measurement of house
result.tf$coef
anova(result.tf)
## 95% confidence interval of slope
confint(result.tf,level = 0.95)
newdata<-data.frame(sqft_living = 1500)
## 95% confidence interval of mean response
predict.lm(result,newdata,level=.95,interval="confidence")
## Prediction Interval of a new response w/ spending power
predict.lm(result,newdata,level=.95,interval="predict")


#####################
## MODEL SELECTION ##
#####################

# regression model with all variables included
fit_all <- lm(price.log ~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+
                waterfront+view+condition+grade+sqft_above+sqft_basement+yr_built+
                yr_renovated+lat+long+sqft_living15+sqft_lot15)
summary(fit_all)
boxcox(fit_all)

# r-sq = .7695 with all significant variables 

# removing sqft_basement & sqft_above - has been effected because of possible multicollinearity
fit_all2 <- lm(price.log ~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+
                 waterfront+view+condition+grade+yr_built+
                 yr_renovated+lat+long+sqft_living15+sqft_lot15)
summary(fit_all2)
boxcox(fit_all2)
# r-sq = .7696 with all significant variables (same as before)
# can remove sqft_basement & sqft above

# intercept only model
regnull <- lm(price.log~1, data=df)
# model with all predictors
regfull <- lm(price.log~., data=df)

# forward selection
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")

# backward elimination
step(regnull, scope=list(lower=regnull, upper=regfull), direction="backward")

# step regresstion
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")

# equation found with forward/stepwise regression: 
fit <- lm(price.log ~ grade + lat + sqft_living + yr_built +
            view + bathrooms + sqft_living15 + condition + waterfront + 
            floors + sqft_lot + yr_renovated + bedrooms + long + sqft_lot15)

summary(fit)
par(mfrow=c(2,2))
plot(fit$fitted.values,fit$residuals, main="Residual Plot")
abline(h=0,col="red")
boxcox(fit, main = "BoxCox Plot for fit")
acf(price.log)
qqnorm(price.log)
# r-squared: .7695

#######################
## MULTICOLLINEARITY ##
#######################

# testing for multicollinearity 
vif(fit)

# multicollinearity does not effect the model however the model can be greatly reduced by removing those points 

###############
## SPLITTING ##
###############

set.seed(6021)
sample<-sample.int(nrow(df), floor(.50*nrow(df)), replace = F)
train<-df[sample, ]
test<-df[-sample, ]

##############
## OUTLIERS ##
##############

#finding outliers in response variables: 

##externally studentized residuals
ext.student.res<-rstudent(fit) 

par(mfrow=c(1,1))

n<-length(test$price.log)
p<-length(test)

##critical value using Bonferroni procedure
qt(1-0.05/(2*n), n-p-1)

sort(ext.student.res)

plot(ext.student.res,main="Externally Studentized Residuals", ylim=c(-5.5,5.5))
abline(h=qt(1-0.05/(2*n), n-p-1), col="red")
abline(h=-qt(1-0.05/(2*n), n-p-1), col="red")

ext.student.res[abs(ext.student.res)>qt(1-0.05/(2*n), n-p-1)]

# outliers: 327, 412, 2590, 12552, 18333, 18877

df <- df[-c(327, 412, 2590, 12552, 18333, 18877),]

##influential observations

# cooks distance - 0 outliers
HighLeverage.cd <- cooks.distance(fit) > qf(0.5, p, n - p)


# dffits - 310 outliers
HighLeverage.dffits <- dffits(fit) > 2*sqrt(p/n)
# removes outliers from model 
df.out <- df[!HighLeverage.dffits,]

# final model used with same variables as 'fit' but on the new dataset with outliers removed
fit.final <- lm(price.log ~ grade + lat + sqft_living + yr_built +
                view + bathrooms + sqft_living15 + condition + waterfront + 
                floors + sqft_lot + yr_renovated + bedrooms + long + sqft_lot15, data = df.out)

summary(fit) #R^2 - .7695
summary(fit.final) #R^2 - .7707
par(mfrow=c(2,2))
plot(fit.final$fitted.values,fit.final$residuals, main="Residual Plot")
abline(h=0,col="red")
boxcox(fit.final, main = "BoxCox Plot for fit")
acf(price.log)
qqnorm(price.log)
# no other influential variables

##################################
## Testing Regression Equation ##
##################################
#Split the Data
set.seed(6021)
sample<-sample.int(nrow(df), floor(.75*nrow(df)), replace = F)
train<-df[sample, ]
test<-df[-sample, ]

fit.test <- lm(price.log ~ grade + lat + sqft_living + yr_built +
                 view + bathrooms + sqft_living15 + condition + waterfront +
                 floors + sqft_lot + yr_renovated + bedrooms + long + sqft_lot15, data = train)
summary(fit.test) #R^2 -0.77
pred <- predict.lm(fit.test, test)
head(test$price.log)
head(pred)
actuals_preds <- data.frame(cbind(actuals=(test$price.log), predicteds=(pred))) # make actuals_predicteds dataframe.
head(actuals_preds)
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy # 87.86028%
#A simple correlation between the actuals and predicted values can be used as a form of accuracy measure.
#A higher correlation accuracy implies that the actuals and predicted values have similar directional movement,
#i.e. when the actuals values increase the predicteds also increase and vice-versa.
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy # => 98.96561%, min_max accuracy
#Actuals and predict both are in same dataset. Min_Max_accuracy will find out accuracy rate of each row.
#it can be considered accuracy rate of the model.
meandiff <- mean(abs(pred - test$price.log))
meanact <- mean(test$price.log)
mape <- meandiff/meanact
mape #On average the predicted values are different from the actual values by 1.044%
plot((pred), (test$price.log),
     xlab="predicted",ylab="actual", main = "Log of Price for Predicted v.s. Actual")
abline(a=0,b=1, col= ("red"))
