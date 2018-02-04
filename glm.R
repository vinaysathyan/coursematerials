# Set working directory to where csv file is located
setwd("C:/Users/michaeljgrogan/Documents/a_documents/computing/data science/datasets")

# Read the data
mydata<- read.csv("binomial_stock.csv")
attach(mydata)

# Logistic regression
dividend <- glm(dividend ~ years + earnings_estimates, data=mydata, family="binomial")
summary(dividend)

yearsprobability=dividend$coefficients[2]
oddsratio=exp(yearsprobability)

probability=(oddsratio/(1+oddsratio))
probability