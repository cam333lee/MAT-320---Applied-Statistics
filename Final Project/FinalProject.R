
#This program is created for the Final Project, MAT 320
install.packages("xlsx")
library(xlsx)
data = read.xlsx("ExchangeRateData.xlsx",sheetIndex = 1, header = T)
data
xrate = data$EXJPUS
xrate

install.packages("rmarkdown")
preCOVID = data[seq(1,36), 2]
preCOVID
duringCOVID = data[seq(37,63),2]


#Hypothesis Testing Two means (Two Tailed)
t.test(preCOVID, duringCOVID, mu = 0, var.equal = T, alternative = "two.sided", conf.level = 0.95)
p = 0.07258
cat("In this study, we assumed that the average exchange rate from pre-covid times vs. 
    covid times were not equal. Since p = ", p, "it is greater than 0.05 which means we cannot
    reject the idea that it is not equal.")

cat("With 95% confidence, we can conclude that evidence points to the value of the Japanese Yen has been greater during covid times when 
being compared to pre-covid times. Although it was assumed that the value of the Yen would be greater during
    non-covid times, it has been proven false and remains in a more positive state.")

#Hypothesis Testing Two means (Right-Tailed)
t.test(preCOVID, duringCOVID, mu = 0, var.equal = T, alternative = "greater", conf.level = 0.95)
p = 0.03629





#Hypothesis Testing Two means (Left-Tailed)
t.test(preCOVID, duringCOVID, mu = 0, var.equal = T, alternative = "less", conf.level = 0.95)
p = 0.9637

cat("This clearly shows ")





#Pre-Covid Exchange Rates
#Making Exchange Rate Plot
data = read.xlsx("ExchangeRateData.xlsx",sheetIndex = 1, header = T)
data
monPreCOVID = data[seq(1,36), 1]
monPreCOVID
monDurCOVID = data[seq(37,63),1]
preCOVID = data[seq(1,36), 2]
preCOVID
duringCOVID = data[seq(37,63),2]

plot(monPreCOVID, preCOVID, main = "Exchange Rates Before COVID", xlab = "Months", 
     ylab = "Exchange Rate", col = "Blue", type = "b")

#During-Covid Exchange Rates
plot(monDurCOVID,duringCOVID, main = "Exchange Rates During COVID", xlab = "Months", 
     ylab = "Exchange Rate", col = "Red", type = "b")



#Making U.S. Plot
usData <- read.xlsx("UnitedStatesCovidTrendsFINAL.xlsx", sheetIndex = 2, header = TRUE)
usData

month = usData[,1]
month
cases = usData[,2]
cases
plot(month, cases, main = "COVID Cases in the United States", 
     xlab ="Date", ylab = "Number of Cases", col = "blue", pch = 16, type = "b")







#Making scatterplot
x = usData[,1]
y = data1[,2]
plot(x, y, main = "Scatterplot of Percentage of Investments in Energy Securities vs. Tax Efficiency", 
     xlab ="Energy Securities", ylab = "Tax Efficiency", col = "blue", pch = 16, xlim = c(0,12))

#a.)Regression Equation
m = lm(y~x, data = data1)
print(m)

y.intercept = m$coefficients[1]
slope = m$coefficients[2]
cat("y-intercept = ", y.intercept, "slope = ", slope, "\n")
cat("The Regression Line Eq: Tax Efficiency", y.intercept,"+(", slope,") * Energy Securities\n")

#b.) Add regression line to plot
abline(reg = m, col = "red")



#Kizuku's code
##Plot of covid trends in Japan
dataJ = read.xlsx("JapanCovidTrends.xlsx",sheetIndex = 1, header = T)
dataJ
monthJ = dataJ[,1]
caseJ = dataJ[,2]
plot(monthJ,caseJ,main="Covid Cases vs Date in JP",
     xlab = "Date(monthly)",ylab="New Cases",
     type = "o", col='blue')

##Plot of covid trends in USA
dataUSA = read.xlsx("USACovidTrends.xlsx",sheetIndex = 1, header = T)
dataUSA
monthUSA = dataUSA[,1]
caseUSA = dataUSA[,2]
summary(caseUSA)
plot(monthUSA,caseUSA,main="Covid Cases vs Date in USA",
     xlab = "Date(monthly)",ylab="New Cases(in hundreds)",
     type = "o", col='blue')



###Linear regression line
#2021~
x_date = c(0:14)
y_exchange = data[seq(49,63,1),2]

m = lm(formula = y_exchange~x_date)
summary(m)

#checking if it's okay to use linear regression.
layout(matrix(data=1:4,nrow=2,ncol=2,byrow=T))
plot(m)
layout(matrix(data=1,nrow=1,ncol=1))#reset

y.intercept=m$coefficients[1]
slope=m$coefficients[2]
cat("y-intercept = ", y.intercept,"\t","slope = ", slope, "\n")
cat("The regression equeation: $1 = ",y.intercept," + (",slope,") * Number_of_months (JPY)\n")

#Plotting
plot(x_date,y_exchange,main="Exchange rate JPY-USD",
     xlab = "Date (monthly)",ylab="Exchange rate",
     pch = 16,col='blue')
abline(reg = m,col="red")

#2024
cat("$1 = ",y.intercept + slope * 48, "(JPY)\n")

