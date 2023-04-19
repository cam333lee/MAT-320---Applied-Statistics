#Exam 3
#Cameron Lee

#******************************Question 1*************************************
#a.) Scatterplot and Regression Equation
hours = c(10, 11, 16, 9, 7, 15, 16, 10)
grade = c(75, 80, 92, 58, 46, 96, 89, 68)
plot(hours, grade, main = "Scatterplot of Lab Hours Recorded vs. Final Course Grade", 
     xlab = "Hours", ylab = "Grade", col = "blue")

#b.) Find the regression equation and draw the regression line on the scatter plot
#making a data.frame
data1 <- data.frame(hours, grade)
data1

hours = data1[,1]
grade = data1[,2]

#Linear regression line
m = lm(grade~hours, data = data1)
print(m)

y.intercept = m$coefficients[1]
slope = m$coefficients[2]
cat("y-intercept = ", y.intercept, "slope = ", slope, "\n")
cat("The Regression Line Eq: Final Grade", y.intercept,"+(", slope,") * Hours In Lab\n")

abline(reg = m, col = "red")

#c.) Predict a student's grade if they spend about 14 hours per week in the lab. 
#Reasonable? Explain the reasonableness. 

grade.14 = y.intercept + (slope * 14)
cat("If a student were to spend 14 hour in lab, then their predicted final
    grade should be: ", grade.14, "%")
cat("This is a reasonable prediction using the regression equation because
    it lies within the range of hours studied in the sample data. (14 lies
    within the range of 7-16 hours)")

#d.) Predict a student's grade if they spend about 20 hours per week in the lab. 
#Reasonable? Explain the reasonableness. 
grade.20 = y.intercept + (slope * 20)
cat("If a student were to spend 14 hour in lab, then their predicted final
    grade should be: ", grade.20, "%")
cat("This is not a resonable predicton using the regression equation because
    it lies outside of the range of hours studied in the sample data. 
    (20 lies out the range of 7-16 hours)")

#e.) Linear correlation (r) coefficient and the coefficient of determination (r^2)
rHours = cor(data1$hours, data1$grade)
cat("Linear Correlation Coefficient =", rHours, "\n")
cat("Coefficient of Determination = ", rHours^2, "\n")

#f.) Determine the "percentage of variation" in the response variable 
#explained by the regression
cat("The coefficient of determination is equal to", (rHours^2)*100, "% which shows
that the variation in student's grades are explained by hours spent in the lab.")

#g.) Describe the linear relationship between these two variables(strength and direction)
#, and decide whether this linear regression is useful for making predictions
cat("This linear regression is very useful in making grade predictions")
cat("The Linear Correlation Coefficient suggests an extremely strong positive linear 
    relationship between Lab Hours Recorded and Final Course Grades.")


#******************************Question 2*************************************
#a.) Create two types of residual plots that we discussed in class: 1.)
#residuals vs. x-values and a normal probability plot of the data
layout(matrix(data = 1:4, nrow = 2, ncol = 2, byrow = TRUE))
plot(m)
layout(matrix(data = 1, nrow = 1, ncol = 1))

cat("The normal qq-plot shows that the data is normally distributed and the residual 
    plot shows a randomness around the center line meaning that both of our 
    requirements provide evidence that our data is suited for linear regression.")

#b.) Find Se and interpret answer
summary(m)
cat("According to the data, the residual standard error is equal to: 6.811")
cat("Very roughly speaking, on average, the predicted final grade differs from the 
    observed hours spent studying in the lab by about 6.811%")

#c.) Using R results from the regression, determine whether the data provides sufficient
#evidence to conclude that the number of hours in lab is useful for predicting the grade
cat("As the summary statistics say, the p-value for the linear regression line
    is p = 0.0007363. That means that the significance level would have to be so
    miniscule in order for the regression line to be inappropriate for this
    range of data. Overall, the p-value shows that the data provides great evidence
    that the number of hours in lab is useful for predicting the grade.")

#d.) Determine a 99% CI for the mean grade of all computer science students who spend
#14 hours in lab every week

predict(m, data.frame(hours = 14), interval = "confidence", conf.level = 0.99)
cat("We can be 99% certain that the mean grade of all students who study 14 hours
    in lab will be somwhere between 78.90972% and 93.26993%s")

#e.) Determine a 99% prediction interval for the grade of a computer science student
#who spends 14 hours in lab every week. 
predict(m, data.frame(hours = 14), interval = "prediction", conf.level = 0.99)
cat("We can be 99% certain that a student who spends 14 hours in lab 
    will earn a grade between 67.94375% and 104.2359%")

#******************************Question 3*************************************
schoolA <- c(420, 390, 365, 462, 480, NA) #Created independent school data 
schoolB <- c(600, 540, 665, 510, 515, 480) #Need to have NA to have same length vectors
schoolC <- c(454, 385, 400, 466, NA, NA)
combined_g <- data.frame(cbind(schoolA, schoolB, schoolC)) #combining data into one data set
summary(combined_g) #summary of the combined data

Stacked_Groups <- stack(combined_g) #creating a full stack of all of the data
Stacked_Groups #running the full stack

#Attaining anova results by using the stacked data
Results <- aov(Stacked_Groups$values~Stacked_Groups$ind, data = Stacked_Groups)
Results
#summary provides us with some summary of the anova results
summary(Results)

#a.) Check for assumptions of normal populations
schoolAD <- combined_g[,1]
schoolBD <- combined_g[,2]
schoolCD <- combined_g[,3]

schoolAD

schoolAData = schoolAD[is.na(schoolAD) == FALSE]
schoolBData = schoolBD[is.na(schoolBD) == FALSE]
schoolCData = schoolCD[is.na(schoolCD) == FALSE]

schoolAData

#Testing for normalcy
qqnorm(schoolAData)
qqline(schoolAData)
qqnorm(schoolBData)
qqline(schoolBData)
qqnorm(schoolCData)
qqline(schoolCData)

#Testing for sd
boxplot(schoolAData, schoolBData, schoolCData, names = c("School A", "School B", "School C"))

#making a data.frame
school <-c(rep("School A", length(schoolAData)), rep("School B", length(schoolBData)), 
            rep("School C", length(schoolCData)))
test.scores<-c(schoolAData, schoolBData, schoolCData)

Frame1 <- data.frame(test.scores,school)
Frame1

#Second anova test
test.aov = aov(test.scores~school)
summary(test.aov)

cat("All of the data looks normally distributed and the standard deviation can be
    considered equal from the boxplot") 


#b.) Two degrees of freedom
cat("The two degrees of freedom are marked by the number of independent data 
    sets there are (minus 1) and the number of actual values there are combined while sub
    subtracting the number of data sets")
cat("In this case, that would be 2 and 12")

#c.) Actual definition of the test statistic F, what two values are used to compute
#the value
cat("The F-statistic can be defined by the ratio of the variation among sample means to the
    variation within the samples. In other words, it is the variation among the sample means
    which is called the treatment mean square divided by the error
    mean square which shows the variation within the samples." )
cat("F = MSTR/MSE")

#d.) Anova test

schoolA <- c(420, 390, 365, 462, 480, NA) #Created independent school data 
schoolB <- c(600, 540, 665, 510, 515, 480) #Need to have NA to have same length vectors
schoolC <- c(454, 385, 400, 466, NA, NA)
combined_g <- data.frame(cbind(schoolA, schoolB, schoolC)) #combining data into one data set
summary(combined_g) #summary of the combined data

Stacked_Groups <- stack(combined_g) #creating a full stack of all of the data
Stacked_Groups #running the full stack

#Attaining anova results by using the stacked data
Results <- aov(Stacked_Groups$values~Stacked_Groups$ind, data = Stacked_Groups)
Results
#summary provides us with some summary of the anova results
summary(Results)


cat("The comments are in chronological order to find the anova")

cat("p-value approach:")
cat("If we take a look at the summary(Results), we can see that the p-value is equal
    to 0.00365. Since the p-value is so little, it can be concluded that there is sufficient 
    evidence to conclude that a difference exists in mean SAT math scores between the three schools")


#e.)How can F-statistic be found
cat("The F-statistic can be found by taking the values from the Mean Sq column
    of the anova results and dividing the top number in that column by the bottom number")
cat("In this problem it would be", 29041/3125, "which is equal to the F-statistic the anova
    generated")

#f.) 
cat("Used the F-statistic, and both degrees of freedom to attain the value of the p-statistic
to the right side of the graph")
p.val = 1-pf(9.292, 2, 12)
cat("The p.val = ", p.val)



