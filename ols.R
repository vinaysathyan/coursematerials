# Set working directory to where csv file is located
setwd("C:/Users/michaeljgrogan/Documents/a_documents/computing/data science/datasets")

# Read the data
mydata<- read.csv("ols_stock2.csv")
attach(mydata)

df<-data.frame(stock_return,dividend,earnings,debt_to_equity,marketcap)
attach(df)

# OLS regression - stock_return (dependent variable) and dividend + earnings + debt_to_equity (independent variables)
reg1 <- lm(stock_return ~ dividend + earnings + debt_to_equity)
summary(reg1)

plot(reg1)

#Install car library and regress independent variables
library(car)

#Variance Inflation Factor Test for multicollinearity
vif(reg1)

#Variance Inflation Factor
auxreg <- lm(dividend ~ earnings + debt_to_equity)
rsquared <- summary(auxreg)$r.square
vifstat <- 1/(1 - (rsquared))

#Breusch-Pagan Test for Heteroscedasticity
library(lmtest)
bptest(reg1)

stock_return_scaled=(stock_return/earnings)*1
reg2<-lm(stock_return_scaled ~ dividend + earnings + debt_to_equity)
bptest(reg2)
summary(reg2)