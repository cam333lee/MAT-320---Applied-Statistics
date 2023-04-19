#Cameron Lee
#4/27/22

#Homework Numbers
#14.1: 24
#14.2: 58*, 60*, 64
#14.3: 98*, 100* (parts b, c, and d only)
#14.4: 144* 146*

#***********************Chapter 14 Section 2***********************
#*********************Question 58**********************************

data1 <- read.csv("14.58.txt", sep = '\t', header = TRUE)
data1

#Making scatterplot
x = data1[,1]
y = data1[,2]
plot(x, y, main = "Scatterplot of Percentage of Investments in Energy Securities vs. Tax Efficiency", 
     xlab ="Energy Securities (%)", ylab = "Tax Efficiency", col = "blue", pch = 16, xlim = c(0,12))

#a.)Regression Equation
m = lm(y~x, data = data1)
print(m)

y.intercept = m$coefficients[1]
slope = m$coefficients[2]
cat("y-intercept = ", y.intercept, "slope = ", slope, "\n")
cat("The Regression Line Eq: Tax Efficiency ", y.intercept,"+(", slope,") * Energy Securities\n")

#b.) Add regression line to plot
abline(reg = m, col = "red")


#c.) Because the slope of the line is negative, tax efficiency tends to decrease as 
#energy securities increase


#d.) Because x represents energy securities, and y represents tax efficiency,  
#the slope of -5.264 indicates that the tax efficiency decreases by 5.624 per 1%
#increase of the energy securities

#e.) predictor variable: x -> Energy Securities
    #response variable: y -> Tax Efficiency 

#ASK
#f.) outliers: none
    #potential influential observations: (10.6, 53.5)

#g.) Predict 5.0%, 7.4%

prediction1 = y.intercept + (slope * 5)
prediction2 = y.intercept + (slope * 7.4)

cat("The prediction for 5.0% is:", prediction1, "and the prediction for 7.4% is:", prediction2)

#make predictions
d = data.frame(x = c(5.0, 7.4))
y.hat = predict(m, newdata = d)
#Add predicted points to the plot
points(x = d$x, y = y.hat, pch = 8, cex = 1, col = "green")


boxplot(x, horizontal = TRUE)

#*********************Question 60**********************************
data2 <- read.csv("14.60.txt", sep = '\t', header = TRUE)
data2

#Making scatterplot
houseSize = data2[,1]
houseSize
housePrice = data2[,2]
plot(houseSize, housePrice, main = "Scatterplot of Size vs. Price for Equestrian Estates", 
     xlab ="Size (In Hundreds of Sq. Ft.)", ylab = "Price In Thousands ($)", col = "blue", pch = 16)

#a.)Regression Equation
m = lm(housePrice~houseSize, data = data2)
print(m)

y.intercept = m$coefficients[1]
slope = m$coefficients[2]
cat("y-intercept = ", y.intercept, "slope = ", slope, "\n")
cat("The Regression Line Eq: Price", y.intercept,"+(", slope,") * Size\n")

#b.) Add regression line to plot
abline(reg = m, col = "red")


#c.) Because the slope of the line is positive, the price of a house tends to increase as 
#the size of the house increases


#d.) Because x represents the size of a house in sq. feet, and y represents the price of a house,  
#the slope of 15.89352 indicates that the price of a house increases decreases by $15,893.52 per 100 
#square feet 

#e.) predictor variable: x -> Size
#response variable: y -> Price

#ASK
#f.) outliers: (30, 738)
#potential influential observations: (40, 804)

#g.) Predict 2600 SQ.FT.

prediction1 = y.intercept + (slope * 26)

cat("The prediction for 2600 sq. ft. is:", prediction1, "or $", prediction1*1000)


#NOT SURE WHAT IS WRONG HERE
#make predictions
d = data.frame(x = 26)
d
d$x
y.hatHouse= predict(m, newdata = d)
#Add predicted points to the plot
points(x = d$x, y = y.hatHouse, pch = 8, cex = 1, col = "green")


#*********************Question 64**********************************
#On Paper


#***********************Chapter 14 Section 3***********************
#*********************Question 98**********************************
#*
#*#b.) compute the coefficient of determination, r^2
dataEnergy <-read.csv("14.98.txt", sep = '\t', header = TRUE)
dataEnergy
rEnergy = cor(dataEnergy$ENERGY, dataEnergy$EFFICIENCY)
cat("Linear Correlation Coefficient =", rEnergy, "\n")
cat("Coefficient of Determination = ", rEnergy^2, "\n")

#c.) Determine the percentage of variation in the observed values of the
#response variable explained by the regression, and interpret your answer
cat("The coefficient of determination is equal to", (rEnergy^2)*100, "% which shows
that the variation in the Tax Efficiency Data is explained by percentage in Energy Securities.")

#d.) State how useful the regression equation appears to be for making predictions
cat("The regression equation appears to be extremely useful for making predictions.")


#Practice with 14.99
#b.) compute the coefficient of determination, r^2
data3 <-read.csv("14.99.txt", sep = '\t', header = TRUE)
data3

r = cor(data3$AGE, data3$PRICE)
cat("Linear Correlation Coefficient =", r, "\n")
cat("Coefficient of Determination = ", r^2, "\n")

#***********************Chapter 14 Section 3***********************
#*********************Question 100*********************************
#*#b.) compute the coefficient of determination, r^2
dataHouse <-read.csv("14.100.txt", sep = '\t', header = TRUE)
dataHouse
rHouse = cor(dataHouse$SIZE, dataHouse$PRICE)
cat("Linear Correlation Coefficient =", rHouse, "\n")
cat("Coefficient of Determination = ", rHouse^2, "\n")

#c.) Determine the percentage of variation in the observed values of the
#response variable explained by the regression, and interpret your answer
cat("The coefficient of determination is equal to", (rHouse^2)*100, "% which shows
that the variation in the Price Data is explained by size of the houses.")

#d.) State how useful the regression equation appears to be for making predictions
cat("The regression equation appears to be useful for making predictions.")

#***********************Chapter 14 Section 4***********************
#*********************Question 144**********************************
#Used the variables from the prior problem already
#a.) obtain the linear correlation coefficient
cat("Linear Correlation Coefficient =", rEnergy, "\n")

#b.) Interpret the value of r in terms of the linear relationship between the 
#two variables in question
cat("Suggests an extremely strong negative linear relationship between energy 
    securities and tax efficiency.")

#c.) Discuss the graphical interpretation of the value of r and verify that it
#is consistent with the graph you obtained in 14S.2
cat("Data points are closely clustered about the regression line")

#d.) square r and compare
cat("It will be the same answer as question 14.98; r^2 = ",rEnergy^2)

#*********************Question 146**********************************
#Used the variables from the prior problem already
#a.) obtain the linear correlation coefficient
cat("Linear Correlation Coefficient =", rHouse, "\n")

#b.) Interpret the value of r in terms of the linear relationship between the 
#two variables in question
cat("Suggests a strong positive linear relationship between size of house 
    and its price.")

#c.) Discuss the graphical interpretation of the value of r and verify that it
#is consistent with the graph you obtained in 14.2
cat("Data points are widely scattered about the regression line")

#d.) square r and compare
cat("It will be the same answer as question 14.100; r^2 = ",rHouse^2)
