#Question 1: Population Mean, Unknown Population Std. Dev (T-Test)
#Hsub0: mu = 98.6
#Hsuba: mu /= 98.6 -> Two-Tailed
ttest.1 <- function(data, mustart, con.level){
  
  print(data)
  
  print(t.test(data, mu = mustart, alternative = "two.sided", conf.level = con.level))
  #critical values
  t.cv = qt(1-(0.1/2), df = 106-1, lower.tail = FALSE)
  cat("The critical values are", t.cv, "&", -t.cv)
}

ttest.1(read.csv("body_temp.txt", header = TRUE), 98.6, 0.95)
print("Since the t-statistic is -6.6115, it falls outside of the rejection region
      and p < 0.05, so we can reject the claim that the average body temperature is different than 98.6 degrees Fahrenheit.")

#The value of the test statistic outside of the 
#rejection region and p is less than 0.05, so reject Hsub0

#*********************************************************************************************
#Question 2: Difference in Two Population Proportions 
#pop1 = Freshmen
#pop2 = Sophomores
#p1 = proportion of freshman who purchased used textbooks
#p2 = proportion of sophomores who purchased used textbooks

#Hsub0: P1 = P2
#HsubA: P1 /= P2


x1 = 73
n1 = 139
x2 = 65
n2 = 121
phat1 = x1/n1
phat2 = x2/n2


#Checking z statistic 
#pp = (x1+x2)/(n1+n2)
#z = (phat1-phat2)/(sqrt(pp*(1-pp)*(1/n1+1/n2)))
#p.value = pnorm(z)

#print(z)
#print(p.value)

prop.test(c(x1, x2), c(n1, n2), alternative = "two.sided", conf.level = 0.95, correct = F)

print("Since P is 0.8465 and is greater than a, we cannot reject the claim that the proportion of
      freshman and sophomores who buy used textbooks are the same.") 

#*********************************************************************************************
#Question 3: Population Standard Deviation 
#Hsub0: stddev = 0.0230
#HsubA: stddev > 0.0230

data1 <- read.csv("pennies.txt", header = TRUE)
x = data1[,1]
print(x)
n = length(x)
s = sd(x)
sd0 = 0.0230
sl = 0.05
chisqr = (n-1)/sd0^2*s^2
chi.cv = qchisq(sl, n-1, lower.tail = FALSE)
p.value = pchisq(chisqr, n-1, lower.tail = FALSE) #right-tailed
cat("X^2 = ", chisqr, " chi.cv = ", chi.cv, " p-value = ", p.value, '\n')
cat("At the 5% significance level, the data provides sufficient 
    evidence to conclude that the deviation in penny weights produced pre-1983
    exceeds the manufacturing specification of 0.0230 grams.")




#Practice using problem 11.23
data1 <- read.csv("11.23.txt", header = TRUE)
x = data1[,1]
print(x)
n = length(x)
s = sd(x)
sd0 = 0.27
sl = 0.01
chisqr = (n-1)/sd0^2*s^2
chi.cv = qchisq(sl, n-1, lower.tail = FALSE) #right-tailed 
p.value = pchisq(chisqr, n-1, lower.tail = FALSE) #right-tailed
cat("X^2 = ", chisqr, " chi.cv = ", chi.cv, " p-value = ", p.value, '\n')


#*********************************************************************************************
#Question 4: Non-Pooled T-Interval Procedure
#Assuming std. dev. is not equal

n1 = 30
x1 = 74.3
s1 = 12.87
n2 = 32
x2 = 88.62
s2 = 22.09
df = n1 + n2 - 1
df

lower.crit = qt(0.005, df, lower.tail = TRUE)
lower.crit
upper.crit = qt(0.005, df, lower.tail = FALSE)
upper.crit


lower.ci = (x1-x2) - (upper.crit * (sqrt((s1^2 / n1) + (s2^2 / n2)))) 
lower.ci

upper.ci = (x1-x2) + (upper.crit * (sqrt((s1^2 / n1) + (s2^2 / n2)))) 
upper.ci

cat("The confidence interval is from (", lower.ci, ",", upper.ci, ")")
cat("As for an interpretation, we are 99% confident that on average Group 1 test scores
    were about 2.2% to 26.4% less than Group B test scores.")
cat("Since the entire interval is negative, there is evidence to say that 
    there is a difference in exam scores between proctored and unproctored students.")
cat("In conclusion, because 0 is not included in the interval, there is 
    evidence that there is a significant difference in final exam scores between
    proctored and unproctored students.")



#Book example to find critical value
#qt(0.1/2, 17, lower.tail = FALSE) 

#*********************************************************************************************
#Question 5: Goodness of Fit
#Significance level = 0.05
obs = c(145, 198, 169, 172, 142, 152, 154, 139, 121, 118)
probs = c(1/10, 1/10, 1/10, 1/10, 1/10, 1/10, 1/10, 1/10, 1/10, 1/10)
chisq.test(obs, p = probs)
cat("p = ", 0.00006833, "which is significantly less than 0.05")
cat("At the 5% significance level, the data does provide sufficient evidence 
    to conclude that there was a difference in deaths when comparing the various deciles. ")


#*********************************************************************************************
#Question 6: Pooled T-Test
data3 <- read.csv("quarter_data.txt", header = TRUE, sep = '\t')
x1 = data3[,1]
x2 = data3[,2]
#x1 = x1[is.na(x1) == FALSE] #Even though they have the same amount of values, it's good to check
#x2 = x2[is.na(x2) == FALSE]
t.test(x1, x2, mu = 0, alternative = "two.sided", var.equal = TRUE, conf.level = 0.95)
cat("p is < ", 0.000000000000000022, "which is significantly less than 0.05")
cat("At the 5% significance level, the data does provide sufficient evidence
    to conclude that there is a significant difference in the average weight
    between the two types of quarters.")

