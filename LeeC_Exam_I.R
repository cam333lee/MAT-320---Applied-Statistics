#Cameron Lee
#2/22/2022

#*************************Question 2****************
#a.) Relative Frequency Distribution
WeightData <- c(5,0,1,0,2,0,5,0,5,0,3,8,5,0,5,0,5,6,0,0,0,0,0,0,8,5,5,0,4,5,0,0,4,0,0,0,0,0,8,0,9,5,3,0,5,0,0,0,5,8)
length
yaxislab <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)

FDR = table(WeightData)
print("Frequency Distribution")
print(FDR)

RFDr = FDR/sum(FDR)
print("Relative Frequency Distribution")
print(RFDr)

#b.) Relative-Frequency Histogram
hist(WeightData,
     prob = TRUE,
     breaks = seq(-0.5, 10.5, 1),
     main = "Last Digit of Respondent Weights",
     xlab = "Last Digits",
     ylab = "Frequency",
     ylim = c(0,0.6),
     border = "green")

#FIX
#c.) Dotplot
x <- WeightData[order(WeightData)]
x
stripchart(x,
           method = "stack",
           #labels =  c(0, 30),  #ylim = c(0, 30)
           xlab = "Last Digits",
           ylab = "Frequency",
           main = "Last# Digit Respondent Weights")

#d.) These seem to be self-reported measurements as there is a clear
#    discrepancy in the data since more than 50%, (52%) of the data collected
#    has a reported last digit of 0. This is highly improbable and 
#    irregular. 

#e.) There is very little accuracy of the results of this question about weights. 
#    Since majority of the data ends in 0's or 5's it is clear that these measurements
#    were not properly conducted and should require supervision in order for results to truly be accurate



#*************************Question 3****************
#*a.) Five Number Summary (Min, Q1, Q2, Q3, Max)
data3 <- read.csv("Exam1File.txt", header = TRUE)
numData3 = data3[,1]

sort(numData3)

fiveNumSumD3 = quantile(numData3, c(0, 0.25, 0.5, 0.75, 1), type = 2)
cat("The five number summary of this data is: ", fiveNumSumD3)

#b.) Interquartile range
#The middle 50% of the number of words per speech are spread out over 
#

Q = quantile(numData3, c(0.25, 0.5, 0.75), type = 2)
Q1 = Q[1]
Q2 = Q[2]
Q3 = Q[3]
IQR = Q3 - Q1
cat("The IQR is: ", IQR)

#c.) Upper and Lower Limits (Outlier Boundaries)
#LL = Q1 - (1.5 * IQR)
#UL = Q3 + (1.5 * IQR)

LL = Q1 - (1.5 * IQR)
UL = Q3 + (1.5 * IQR)

cat("The lower limit is: ", LL)
cat("The upper limit is: ", UL)

#d.) Lower Limit Outliers
#Answer: None of these numbers are outliers as they
#        are still greater than -795

#e.) Upper Limit Outliers
#Answer: Both of these numbers are outliers as Harrison's and
#        Taft's speeches are greater than 5,141 words

#f.) Boxplot

boxplot(numData3, 
        horizontal = TRUE, 
        main = "Length of Presidential Speeches",
        xlab = "Number of Words Used")

#g.) Shape of the distribution
#Answer: The shape of the distribution can be characterized by right-skew. 
#        Since most of the data can be found to the left of the histogram,
#        the data is most definitely right-skewed. 

#h.) Percentile of Joe Biden's speech (z-formula rearranged)

#x = MU + z(stddev)
#opposite: x-MU/stddev

#number of values below r/(total number of scores) * 100

#Length of data
TlengthD = length(numData3)
TlengthD
NumValBelow = 0
for(val in numData3) {
  
  if(val < 2550) {
    
    NumValBelow = NumValBelow + 1
    
  }
  
}

print(NumValBelow)

#Number of data less than 2

Percentile = (NumValBelow / TlengthD) *100

cat("Joe Biden's speech is: ", round(Percentile, digits = 2), "% longer than other presidential inaguration speeches.")


#*************************Question 5****************
#a.) MU and STDDEV
p = 0.66
n = 35


MU = p*n
MU

STDDEV3 = sqrt(n*p*(1-p))
STDDEV3

cat("The mean of X is: ", MU, "and the standard deviation of X is: ", round(STDDEV3, digits = 4), ".")


#b.) Calculate the probability that X is between 20 and 25 inclusive
ProbsX1 = sum(dbinom(20:25, 35, 0.66))
ProbsX1 = round(ProbsX1 * 100, digits = 4)
cat("The probability that the number of students who are enrolling in college is: ", ProbsX1, "%.")

#c.) Normal Distribution (error checking) to approximate the probability X is between 20 and 25, inclusive
z20 = (19.5-MU)/STDDEV3
print(z20)
z25=(25.5-MU)/STDDEV3
print(z25)
Normal = round(pnorm(z25) - pnorm(z20), digits = 4)
cat("The normal probability is: ", Normal, "or", Normal*100, "%")


#d.) Poisson Distribution
#Are z scores only for normal curves? Yes
#lambda = 66
lambda = MU
x <- c(0:35)
poisnum <- dpois((0:35), MU)
PoisNum = round(poisnum, digits = 4)
data.frame(x, PoisNum)


sum(dpois(20:25, MU))
pDis = ppois(25, MU) - ppois(19, MU)
cat("The poisson distribution gives a probability of: ", round(pDis * 100, digits = 4), "%")

#e.) Comparing Answers (Comparing the two approximations to b (which one is closer?))
#The normal distribution clearly has a much better approximation than
#the poisson distribution. As the normal distribution has an approximation of
#70.46 percent, that is 0.3192 away from the actual answer. Unlike
#the normal, the poisson gives a very far away answer of 46.8844%
#This is 20% less than the number it is supposed to be!

#*************************Question 6****************
#a.) Sampling distribution
mean = 2.1
ss1 = 25
stddev6 = 1.8
sampstddev6 = stddev6 / sqrt(ss1)
cat("The sampling distribution of the problem is approximately normal with a sample standard deivation of: ", 
    sampstddev6, "and a mean of: ", mean)
#Type of sampling distribution is normal as the central limit theorem states
#that if you have a large enough sample it will become normally distributed

#b.) at least 2.5
#(P > 2.5, noninclusive )
#(Bar graph idea: must be less than  two to account for the whole value of 2.5? 2.5 -> 2.0)

AL25 = 1 - pnorm(2, mean, sampstddev6)
cat("The probability that the mean is at least 2.5 is: ", round(AL25, digits = 2), "%")
