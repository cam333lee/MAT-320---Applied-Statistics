#Assignment 7
#Cameron Lee
#4/28/22

#Questions:
#15.1: 28*
#15.2: 58*, 60*, 64*, 66*
#15.3: 92*, 94*
#15.4: 128*, 130*
#16.3: 48*, 51*(Check assumptions of normal populations and equal population standard deviation), 63*

#************************Section 15.1*********************************
#***********************Question 28***********************************
data <- read.csv("15.28.txt", header = TRUE, sep = '\t')
data

x.energy = data[,1]
y.eff = data[,2]

m = lm(y.eff~x.energy, data = data)
summary(m)

#a.) Residual standard error
cat("According to the data, the residual standard error is equal to: ", 3.086)

#b.) Interpret part a results
cat("Very roughly speaking, on average, the predicted tax efficiency of each 
    mutual fund holding in the sample differes from the observed price by about 3.006")

#c.) Obtain a residual plot and a normal probability plot of the residuals 
layout(matrix(data = 1:4, nrow = 2, ncol = 2, byrow = TRUE))
plot(m)
layout(matrix(data = 1, nrow = 1, ncol = 1))

#d.) Decide whether you can reasonably consider assumptions 1-3 for regression inferences
#to be met by the variables under consideration (The answer here is subjective.)

#Assumptions: 1.) Population regression line, Equal standard deviation, Normal populations
#1.) Population regression line can be made
#2.) Standard deviation are similar to each other 
#3.) Population can be considered normal
cat("It appears to be reasonable.")


#************************Section 15.2*********************************
#***********************Question 58***********************************
#Hsub0: B1 = 0;
#HsubA: B1 /= 0
#a = 0.05
summary(m)
t.stat = -12.37 #From summary code
p.value = 0.0000017 #From summary code
sl = 0.05
t.cv = qt(sl/2, lower.tail = FALSE, df = length(data$ENERGY)-2)
cat("t.cv = ", t.cv, ",", -t.cv)
cat("t.stat =", t.stat)

cat("Reject the null hypothesis since p<=a, (0.0000017 < 0.05). At the 5% significance level, the data provides sufficient evidence
    to conclude that energy is a useful predictor for tax efficiency.")

#***********************Question 60***********************************
#Hsub0: B1 = 0;
#HsubA: B1 /= 0
#a = 0.01
data2 <- read.csv("15.60.txt", sep = '\t', header = TRUE)
data2

houseSize = data2[,1]
houseSize
housePrice = data2[,2]

m = lm(housePrice~houseSize, data = data2)

summary(m)
t.stat = 3.918 #From summary code
p.value = 0.00567 #From summary code
sl = 0.05
t.cv = qt(sl/2, lower.tail = FALSE, df = length(data2$SIZE)-2)
cat("t.cv = ", t.cv, ",", -t.cv)
cat("t.stat =", t.stat)

cat("Reject the null hypothesis since p<=a, (0.00567 < 0.05). At the 5% significance level, the data provides sufficient evidence
    to conclude that size (in square feet) is a useful predictor for price of a house.")


#***********************Question 64***********************************
#Confidence interval of 95%
m = lm(y.eff~x.energy, data = data)
summary(m)
level = 0.95
t.cvci1 = qt((1-level)/2, lower.tail = FALSE, df = length(data$ENERGY)-2)
slope = m$coefficients[2]
std.err1 = 3.086
LL = slope - t.cvci1*std.err1
UL = slope + t.cvci1*std.err1
cat("The", 100*level, "% CI is from", LL, "to", UL, '\n')
cat("We can be 95% confident that, for energy holdings, the decrease in mean quantity in tax efficiency 
per 1% increase in holdings is somewhere between -12.38 to 1.85")


#***********************Question 66***********************************
#Confidence interval of 99%
m = lm(housePrice~houseSize, data = data2)
summary(m)
level = 0.99
t.cvci2 = qt((1-level)/2, lower.tail = FALSE, df = length(data2$SIZE)-2)
slope = m$coefficients[2]
std.err2 = 59.62
LL = slope - t.cvci2*std.err2
UL = slope + t.cvci2*std.err2
cat("The", 100*level, "% CI is from", LL, "to", UL, '\n')
cat("We can be 99% confident that, for houses, the change in price
    per 1,000 sqare foot increase in size is between $-192,746 and $224,533")

#************************Section 15.3*********************************
#***********************Question 92***********************************
#*a.) Obtain a point estimate for the mean tax efficiency of all mutual fund portfolios with 6% of their investments 
#*#in energy securities

#linear regression line
m = lm(y.eff~x.energy, data = data)
y.intercept = m$coefficients[1]
slope = m$coefficients[2]
prediction1 = y.intercept + (slope * 6)
cat("The prediction estimate for 6% is: ", prediction1)

#b.) Interval for mean tax efficiency with 6% of their investments in  
predict(m, data.frame(x.energy = 6), interval = "confidence", conf.level = 0.95)
cat("We can be 95% certain that the mean price of all 6% mutual funds with investments 
    in energy securities is somewhere between 75.80585 and 83.37764")

#c.) Find predicted tax efficiency of a mutual fund portfolio with 6% of its investments in energy securities
cat("The prediction estimate for 6% is: ", prediction1)


#d.) predicted value t-prediction interval:
predict(m, data.frame(x.energy = 6), interval = "prediction", conf.level = 0.95)
cat("We can be 95% certain that the mutual fund portfolio with 6% of its investments in 
    energy securities will be somewhere between 73.61782 and 88.56567")
#e.) Draws graph showing both the 95% CI from part b and part d FINISH
#f.) FINISH


#***********************Question 94***********************************
#*a.) Determine a point estimate for the mean price of all 2800 sq. ft. Equestrian Estate homes.
houseSize = data2[,1]
houseSize
housePrice = data2[,2]

#Regression Equation
m = lm(housePrice~houseSize, data = data2)
print(m)

y.intercept = m$coefficients[1]
slope = m$coefficients[2]
prediction2 = y.intercept + (slope * 28)
cat("The prediction estimate for 2800 sq ft is: ", prediction2,"or $", prediction2 * 1000)

#b.) Interval for mean price of all 2800 Equestrian Estate homes 
predict(m, data.frame(houseSize = 28), interval = "confidence", conf.level = 0.99)
cat("We can be 99% certain that the mean price of all 2800 square foot homes will be
    somewhere between $534,343 and $635,8607")

#c.) Find the predicted price of a 2800 Equestrian Estate home
cat("The predicted price of a 2800 sq. ft. home is $", 1000*prediction2)

#d.) Determine a 99% prediction interval for the price of a 2800 sq. ft. Equestrian Estate home.
predict(m, data.frame(houseSize = 28), interval = "prediction", conf.level = 0.99)
cat("We can be 99% certain that the price of a 2800 sq. ft. home will be somewhere between 
    $435,262 and $734,9418")


#************************Section 15.4*********************************
#***********************Question 128***********************************
energyData <- read.csv("15.128.txt", header = TRUE, sep = '\t')
cor.test(energyData$ENERGY, energyData$EFFICIENCY, alternative = "less", conf.level = 0.975)
sl = 0.025
t.cvenergy = qt(sl, lower.tail = TRUE, df = length(energyData$ENERGY)-2)
cat("t.cvenergy =", t.cvenergy, '\n')
#Hsub0 = 0
#HsubA < 0
cat("p-value = 0.0000008509 which is less than 0.025")
cat("At the 2.5% significance level, the data does provide significant evidence
    to conclude that, for mutual fund holdings with investments in energy securities,
    percent of holdings and tax efficiency are negatively linearly correlated.")

#***********************Question 130***********************************
houseData <- read.csv("15.30.txt", header = TRUE, sep = '\t')
cor.test(houseData$SIZE, houseData$PRICE, alternative = "greater", conf.level = 0.995)
sl = 0.005
#ASK KIZUKU
t.cvhouse = qt(sl, lower.tail = FALSE, df = length(houseData$SIZE)-2)
cat("t.cvhouse =", t.cvhouse, '\n')
cat("p-value = 0.002882 which is less than 0.005")
cat("At the 0.5% significance level, the data does provide signicant evidence
    to conclude that, for houses, sq. footage and price are positively linearly correlated")

#************************Section 16.3*********************************
#***********************Question 48***********************************
dataMovie <-read.csv("16.48.txt", header = TRUE, sep = '\t')
dataMovie

datastar1 = dataMovie[,1]
datastar2 =  dataMovie[,2]
datastar3 = dataMovie[,3]
datastar4 = dataMovie[,4]
datastar1 = datastar1[is.na(datastar1) == FALSE]
datastar2 = datastar2[is.na(datastar2) == FALSE]
datastar3 = datastar3[is.na(datastar3) == FALSE]
datastar4 = datastar4[is.na(datastar4) == FALSE]

#boxplots
boxplot(datastar1, datastar2, datastar3, datastar4, names = c("1* or 1.5*", "2* or 2.5*", "3* or 3.5*", "4*"))

#make a data frame
stars<-c(rep("1* or 1.5*", length(datastar1)), rep("2* or 2.5*", length(datastar2)),
         rep("3* or 3.5*", length(datastar3)), rep("4*", length(datastar4)))
ratings<-c(datastar1, datastar2, datastar3, datastar4)
frameMovie<-data.frame(stars, ratings)

#boxplots
plot(ratings~stars, data = frameMovie)

#ANOVA

ratings.aov = aov(ratings~stars)
summary(ratings.aov)

ratings.aov

summary(lm(ratings~stars))
anova(lm(ratings~stars))


#Hsub0 = mu1 = mu2 = mu3 = mu4
#HsubA = not all means are equal 
#a = 0.01
cat("p-value = 0.002687")
cat("Since our p-value is less than 0.01, we can reject the null hypothesis")
cat("At the 1% significance level, the data provides sufficient evidence to conclude that 
    a difference exists in the mean running times among films in the four rating groups")


#***********************Question 51***********************************
dataStaph <-read.csv("16.51.txt", header = TRUE, sep = '\t')
dataStaph

dataA = dataStaph[,1]
dataB = dataStaph[,2]
dataC = dataStaph[,3]
dataD = dataStaph[,4]
dataE = dataStaph[,5]

dataA1 = dataA[is.na(dataA) ==FALSE]
dataB1 = dataB[is.na(dataB) ==FALSE]
dataC1 = dataC[is.na(dataC) ==FALSE]
dataD1 = dataD[is.na(dataD) ==FALSE]
dataE1 = dataE[is.na(dataE) ==FALSE]

#boxplot - checking standard deviations

boxplot(dataA1, dataB1, dataC1, dataD1, dataE1, names = c("Strain A", "Strain B", "Strain C", "Strain D", "Strain E"))

#checking for normalcy
qqnorm(dataA1)
qqline(dataA1)
qqnorm(dataB1)
qqline(dataB1)
qqnorm(dataC1)
qqline(dataC1)
qqnorm(dataD1)
qqline(dataD1)
qqnorm(dataE1)
qqline(dataE1)

#make a data frame
strain<-c(rep("Strain A", length(dataA1)), rep("Strain B", length(dataB1)), rep("Strain C", length(dataC1)), 
          rep("Strain D", length(dataD1)), rep("Strain E", length(dataE1)))
bac.count<-c(dataA1, dataB1, dataC1, dataD1, dataE1)

frameStrain <-data.frame(strain, bac.count)
plot(bac.count~strain, data = frameStrain)

#ANOVA
strain.aov = aov(bac.count~strain)
summary(strain.aov)
strain.aov

summary(lm(bac.count~strain))
anova(lm(bac.count~strain))

cat("p-value = 0.1027")
cat("Since our p-value is greater than 0.05, we cannot reject Hsub0")
cat("At the 5% significance level, the data does not provide sufficient evidence to conclude that there is a
    difference in mean bacteria counts among the five strains of the staph infection")
