#Mathematics 23C Final Project
#Michel Nehme and Ramiz A. Razzak

# Data: Child income by education and parental income percentile
# Source: https://opportunityinsights.org/data/,  Cross-Sectional Child Income Statistics by College Tier and Parent Income Percentile
data = read.csv("Final project data.csv"); head(data)
# If you do not have these plotting libraries as packages on R, you will need to download them to produce our ggplot graph 
library(ggridges)
library(ggplot2)
library(e1071)
#All points will be marked in capital letters. 

#DATAFRAME
#AT LEAST TWO CATEGORICAL COLUMNS
#AT LEAST TWO NUMERIC COLUMNS
#AT LEAST TWENTY ROWS. 
#ADDITIONAL POINT: A DATA SET WITH LOTS OF COLUMNS, ALLOWING COMPARISON OF MANY DIFFERENT VARIABLES. 

#It is important to note that for our data analysis, we predominantly use mean data at each percentile/school tier; i.e., since
#there is no way to disaggregate Chetty's data over the hundreds of thousands of research subjects which he used, 
#we took means at each income percentile for each schooling tier as our relevant data points. 


#Selecting specifically for the intersection of the column of mean incomes of children, and the school tiers "Ivy Plus"
# and "Other elite schools (public and private)": 
ivy_elite <- subset(data,tier_name == "Ivy Plus" | tier_name == "Other elite schools (public and private)" ,
                   select=c(tier_name, k_mean)); ivy_elite
#ADDITIONAL POINT: A GRAPHICAL DISPLAY THAT IS DIFFERENT FROM TEXTBOOK/CLASS; NICELY LABELLED GRAPHICS USING GGPLOT
#This ggplot graph beautifully compares an estimated distribution for ivy vs. elite colleges based on the subsetted data we provided above
ggplot(ivy_elite, aes(x = k_mean, y = tier_name, fill =tier_name)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none") + 
  labs(x = "Yearly income ($)") + 
  labs(y = "School tier")

# We will begin by getting an overview of the average income per educational category by producing aot. 
# The data is split into 15 educational categories, so we will compute the mean income for each category and then barplot it with the category name
# The unique function will give us each category name only once
l <- unique(data$tier_name); l
# We will use this to produce a vector within which we can store the mean of each category
N <- length(l); N
# Now we take our vector for kid incomes and isolate it for further use 
kid_income <- data$k_mean; kid_income
avgs <- numeric(N)
# Now we iterate accross category names and compute the average income for each category, and insert that into a numeric vector to produce our barplot 
for(i in 1: length(l)) {
  category_i <- which(data$tier_name == l[i])
  category_avg <- mean(kid_income[category_i],na.rm=TRUE)
  avgs[i] <- category_avg
}

barplot(avgs,names.arg=l,xlab="Category",ylab="Average income",col="blue",
        main="Average income per educational category",border="red")
#A BARPLOT. 
# Here we receive a visual display of level of school attainment against average income in dollars. From a superficial observation of the 
#graph, we can see that there seems to be a relatively significant correlation between level of educational attaiment and level of child income,
#particularly at the top end. Ivy-league graduates clearly earn the most on average. 

#Let us continue our statistical analysis by observing the mean income for a kid who attended an Ivy-League institution. 
counts <- table(data$tier_name); counts
ivy <- which(data$tier == 1); ivy
#Selecting for average Ivy-League income at each parental wealth percentile: 
ivy_income <- data$k_mean[ivy]; ivy_income
ivy_mean <- mean(ivy_income);ivy_mean
# This turns out to be $119528.7

#Now we want the parental percentile for each ivy-leaguer to associate it to their income; that is, we want to see how the parental income percentile affects child income among ivy-leaguers
ivy_parpercentile <- data$par_pctile[ivy]; ivy_parpercentile
# There is one data point per percentile, as desired. Generating a data frame of ivy-league child income against parental wealth percentile:
df = data.frame("Parental Percentile" = ivy_parpercentile, "Child income" = ivy_income); df

#We have now isolated this data into a data frame for further evaluation at a later time. 
#Visual purposes to assess average distribution of incomes over Ivy-League students. 

#Here, we now produce a histogram in which we look at a density distribution for Ivy-Leaguers's income for visual purposes. 
#HISTOGRAM
hist(ivy_income, 
     main="Histogram for Ivy League Income", 
     xlab="Income ($)", 
     border="blue", 
     col="green",
     breaks =30,
     freq = FALSE
     )

#ADDITIONAL POINT: USE OF LINEAR REGRESSION; APPROPRIATE USE OF COVARIANCE/CORRELATION (CORRELATION)
#Now, we proceed to verify the relationship between parental income percentile and child income; this is a key evaluation of transmission of wealth
#and economic mobility, as we discuss further in our written documentation. 
PP <- df$Parental.Percentile
CI <- df$Child.income
#Here, we plot Ivy-League income against parental income percentile.  
plot(PP, CI, pch = ".", cex = 3, main="Ivy League Income based on parent income percentile", xlab="Parental Income Percentile",ylab="Child Income",col="blue")
# let's now compute the correlation between parental income percentile and child income
cor(PP,CI)
#According to conventions, this correlation can be classified as moderate since it exceeds 0.5; therefore, there is a non-negligeable relationship between
#child income and parental income percentile for Ivy-Leaguers.

#We now want to plot our linear regression onto our scatter plot of Ivy-Leaguer incomes against their parents' wealth percentile. 
#Using the function lm to generate a linear regression model between parental percentile and a child's income earnings as an Ivy-Leaguer: 
lm <- lm(CI ~ PP, data = data); lm
a = 96861.3
b = 453.4
abline(a,b, col = "red")
#The divergence of discrete points away from our linear regression line is admittedly quite significant, as can be seen, which makes sense given our only 
#'moderate-level' correlation statistic between parental income percentile and child income. Had we proceed with trimming, we may have received 
#slightly different results.



# We will now proceed to do a permutation test to statistically analyse whether there exists a statistically significant difference in incomes if you go to a ivy/elite vs. highly selective private
#/public school, across parentile income percentiles. The intended impact of this analysis is to gauge variation of income
#at top end schools, which presumably have similar academic standards - this may therefore give us an implicit insight 
#into the signalling effects of school names, ceteris paribus. 

#ADDITIONAL POINT: A CONVINCING DEMONSTRATION OF A RELATIONSHIP THAT MIGHT NOT HAVE BEEN STATISTICALLY SIGNIFICANT BUT TURNS OUT TO BE SO. 
#ADDITIONAL POINT: AN EXAMPLE WHERE PERMUTATION TESTS OR OTHER COMPUTATIONAL TECHNIQUES CLEARLY WORK BETTER THAN CLASSICAL METHODS (SEE FOLLOWING ANALYSIS AND COMPARE)

#Our initial hypothesis is that there ought not to be a statistically significant difference across these two categorisations:
#after all, the educational difference between ivy league schools and other top private/public schools which are highly selective despite
#potentially not being categorised as 'elite', like UC Berkeley, seems prima facie minimal, so let's see if our assumption is correct

#Creating a subset of our data which isolates the categories in which we are interested, and naming it aptly
ivy_hselec <- subset(data, tier_name == "Ivy Plus" | tier_name == "Other elite schools (public and private)" | 
                  tier_name == "Highly selective public" | tier_name == "Highly selective private" ,select=c(tier, k_mean)); ivy_hselec

#ADDITIONAL POINT: APPROPRIATE USE OF NOVEL STATISTICS 
skewness(ivy_hselec$k_mean[idx])
skewness(ivy_hselec$k_mean[idx2])
#The incomes across percentiles for Ivy/Elite students are less skewed to the right than are the incomes across percentiles for 
#highly selective public/private schools, as indicated by the former skewness statistic of 1.397877, and the latter of 2.201591.

#Setting up our permutation test: 
idx <- which(ivy_hselec$tier == "1" | ivy_hselec$tier == "2"); length(idx)
idx2 <- which(ivy_hselec$tier == "3", ivy_hselec$tier == "4" ); length(idx)
eliteavg <- mean(ivy_hselec$k_mean[idx]); eliteavg
hselective <- mean(ivy_hselec$k_mean[idx2]); hselective
obs.diff <- eliteavg - hselective; obs.diff
sample <- sample(ivy_hselec$tier); sample
sample_elite <- sum(ivy_hselec$k_mean*(sample == "1" | sample == "2"))/sum(sample == "1" | sample == "2"); sample_elite
sample_hselective <- sum(ivy_hselec$k_mean*(sample == "3" | sample == "4"))/sum(sample == "3" | sample == "4"); sample_hselective
(sample == "1" | sample == "2")
N <- 10000
diffs <- numeric(N) 
for ( i in 1:N){
  sample <- sample(ivy_hselec$tier); sample
  sample_elite <- sum(ivy_hselec$k_mean*(sample == "1" | sample == "2"))/sum(sample == "1" | sample == "2"); sample_elite
  sample_hselective <- sum(ivy_hselec$k_mean*(sample == "3" | sample == "4"))/sum(sample == "3" | sample == "4"); sample_hselective
  diffs[i] <- sample_elite - sample_hselective; diffs[i]
}
diffs
mean <- mean(diffs); mean
#Generating a histogram of differences between observed and expected values, which we expect to be normal by the central limit theorem:
hist(diffs, breaks = "FD",main="Differences between observed and expected values", xlab="Difference ($)",ylab="Frequency",col="blue")
#Now, let us generate a p-value from our permutation test to discern whether or not the observed differences give us reason to 
#reject our null hypothesis that there ought not to be a difference between income for elite/Ivy-League graduates and Highly Selective
#graduates: 
pvalue <- (sum(diffs >= obs.diff)+1)/(N+1); pvalue 
# Our p-value is incredibly low, which means it is statistically significant, and gives us reason to reject our null hypothesis, 
#which we did not expect to be the case!

#Let us verify the normality of histogram of differences between observed and expected values by overlaying a normal curve (physcially verifying that central limit theorem applies here): 
hist_1 <- hist(diffs, breaks = "FD", freq = FALSE, main="Differences between observed and expected values", xlab="Difference ($)",ylab="Frequency",col="blue")
mu = mean(diffs)
sd = sd(diffs)
curve(dnorm(x, mu, sd), add = TRUE, col = "red")
#PROBAILITY DENSITY GRAPH OVERLAID ONTO A HISTOGRAM. 
# This overlaid normal curve proves a beautiful fit, as expected! 

# This analysis has shown that, as described in point 9 of the bonus section, we have demonstrated a relationship that might not have been statistically
# significant (the difference in incomes based on ivy vs. top selective private and public institutions) us in fact very significant, as demonstrated
# by the very low p-value we received. 

#COMPARISON OF ANALYSIS BY CLASSICAL METHODS (CHI-SQUARED) AND SIMULATION METHODS (PERMUTATION TEST)
#Now, we will proceed to a similar analysis using a chi-square test, with our null hypothesis being that the income between ivy-leaguers and elite
#college-goers is identical to that of selective private and public colleges. Based on our permutation test, we expect a p-value that will 
#falsily this null hypothesis, and further value the point we've seemed to make.  


#Here, we are conducting a Chi-squared test to determine whether or not there is a statistically significant difference in income for those who graduate from
#ivy-league and elite schools and highly selective college-goers: 

#Creating indexes so that we can construct our data frame: we could have used 'subset' function, but we decided to show how to 
#construct a data frame from scratch using basic r functions, for the sake of analytical diversity!

ivyi <- which(data$tier =="1"); ivyi
elitei <- which(data$tier == "2"); elitei
selecpub <- which(data$tier == "3"); selecpub
selecpriv <- which(data$tier == "4"); selecpriv
IVY <- data$k_mean[ivyi]
ELITE <- data$k_mean[elitei]
SELECPUB <-data$k_mean[selecpub] 
SELECPRIV <- data$k_mean[selecpriv]
df <- data.frame(IVY, ELITE, SELECPUB,SELECPRIV); df

#A CONTINGENCY TABLE + ANALYSIS OF CONTINGENCY TABLE.  

#Here, we generate our contingency table by isolating the relevant data from our initial dataframe, and using it to take the means
#which subsequently form our contingency table: 
IVY1 <- as.numeric(IVY)
ELITE1 <- as.numeric(ELITE)
SELECPUB1 <- as.numeric(SELECPUB)
SELECPRIV1 <- as.numeric(SELECPRIV)
mean_ivy_elite <- (IVY1 + ELITE1)/2; mean_ivy_elite
mean_selecpub_selecpriv <- (SELECPUB1 + SELECPRIV1)/2; mean_selecpub_selecpriv



#Now we have a data frame composed of the averages of ivy and elite at each percentile, and the averages of highly selective public and private at every data point.
df1 <- data.frame(mean_ivy_elite, mean_selecpub_selecpriv); df1
#For our contingency table, we take the transpose of this data frame:
contingency_table < t(df1)

# Now we will begin the chi-square test by hand, since the built-in test doesn't allow for the proper amount of degrees of freedom
# We had to do this because the chi-square test assumes 100 degrees of freedom, when in reality we have 200
# We ill MANUALLY do the expected counts 
obs <- t(df1); obs
#These are our obersvations 
col_means = colMeans(obs); col_means
# Our expected is how much we expect to be in the bins, and since our null hypothesis is that the categorical distribution of 
# dollars is the same, we produce the following expected thing.
exp <- rbind(col_means, col_means)
#Manual computation of test statistic
test_stat = sum((obs-exp)^2 / exp); test_stat
p_chisq <- pchisq(q =848252.9 , df = 200)
pval = 1 - p_chisq; pval
#This means our p-value is EXTREMELY low, essentially zero, since the p-value is 1 - this value, so 1-1 is zero. 

#Let us now take the chi-statistic, to discern whether or not income is dependent on the distinction between ivy-league/elite schools, and highly selective schools. 
#Chi-square test takes in counts; here, we are taking counts as number of dollars, and our assumption is that the distribution
#of counts should be equal based on our null hypothesis. The idea is that number of dollars should be equally distributed, based on our null hypothesis. 
#Because they are percentiles, we have standardized categories into discrete bins as desired 
#IMPORTANT: We are considering these as counts in categorical columns; we discussed this with Joe, and insofar as we consider
# the dollars as counts in categorical bins, then this satisfies the requirements of a standard chi-square for independence. 
# While non-standard, this is a valid application of the chi-square test, wherein our null hypothesis is that ivy and non-ivy
#have the same earnings. 
# The degrees of freedom should be 200, since we do not know the row and column totals, so we computed the test by hand rather
#than with the built-in R function
# We spoke with Joe about this to verify the validity and the number of degrees of freedom.
#Our p-value is 1 - (the result of pchisq) since we are looking for the likelihood of a test statistic being
#MORE EXTREME than the one we received. 
#Therefore our p-value is near 0 (similarly to our permutation test result)
#Our p-value is incredibly small, indicating that, despite our initial hypothesis, going to an ivy/elite school seems to be related
#to a significantly larger income for graduating students on net across parental percentiles than for students who attended highly-selective public/private schools.
#This was, for Ramiz and I, counterintuitive , in that we presumed that highly selective schools generated similar earning potential to ivy-league/elite schools. 
#This will be extrapolated upon in our analysis! 
#By comparing our classic test vs. our permutation test, we see that both yield similar results, with extremely low p-values. Therefore, our 
#comparison concludes that both tests are, in this specific case, in agreement in terms of the results produced (that is, in rejecting
#the null hypothesis). 


#ADDITIONAL POINT: A CONVINCING DEMONSTRATION OF A RELATIONSHIP THAT MAY HAVE BEEN STATISTICALLY SIGNIFICANT BUT TURNS OUT NOT TO BE SO. 

#Comparing nonselective four-year public with two-year for-profit; we would expect nonselective four-year public students to earn more money on average, given that they have a) done more 
#years of schooling, and b) two-year for-profit schools are often predatory in their education practices and generally on net do not offer better education than public schools, if not often worse education, 
#while leaving students in debt traps. There is much media on this! In this test, however, we are just going to see whether there is compelling evidence 
#to suggest that the incomes are DIFFERENT for the two-types of students, because we do not want to be too presumptuous about our underlying conditions! Maybe the two-year students
#actually earn more than the four-year ones...
#For this reason, we are conducting a two-tail test:
#Let us see whether there is actually a statistically significant interaction between the two: 
comp <- subset(data, tier_name == "Nonselective four-year public" | tier_name == "Two-year for-profit", select=c(tier, k_mean)); comp
#Clearing out data which returns a non-result, so that we can use our data for statistical analysis (this is an intelligent move!): 
comp1 <- comp[complete.cases(comp),]; comp1
#Creating our indexes, where tier 7 corresponds to four-year public schools, and tier 11 to two-year for-profit institutions: 
idx1 <- which(comp1$tier == "7"); length(idx1)
idx2 <- which(comp1$tier == "11" ); length(idx2)
four_pub <- mean(comp1$k_mean[idx1]); four_pub
two_priv <- mean(comp1$k_mean[idx2]); two_priv
obs.diff <- four_pub - two_priv; obs.diff
sample <- sample(comp1$tier); sample
sample_four_pub <- sum(comp1$k_mean*(sample == "7")/sum(sample == "7")); sample_four_pub
sample_two_priv <- sum(comp1$k_mean*(sample == "11"))/sum(sample == "11"); sample_two_priv
N <- 10000
diffs <- numeric(N) 
for ( i in 1:N){
  sample <- sample(comp1$tier); sample
  sample_four_pub <- sum(comp1$k_mean*(sample == "7"))/sum(sample == "7"); sample_four_pub
  sample_two_priv <- sum(comp1$k_mean*(sample == "11"))/sum(sample == "11"); sample_two_priv
  diffs[i] <- sample_four_pub - sample_two_priv; diffs[i]
}

mean <- mean(diffs); mean
hist2 <- hist(diffs, breaks = "FD",main="Differences between observed and expected values", xlab="Difference ($)",ylab="Frequency",col="blue")
abline(v = obs.diff, col = "red")
#Two-tailed test, since we are testing for difference. 
pvalue <- 2*(sum(diffs <= obs.diff)+1)/(N+1); pvalue 

#A p-value above 0.1 indicates that our result is not statistically signifcant, where we might expect it to be, because we initially posited that it seems intuitive
#that four years of public schooling should yield better educational results, and therefore better earning capacity, than just two years of for-profit education. By our
#analysis, however, there is no statistically significant distinction between the two-types of schooling across percentiles for earning capacity (with a two-tail test), 
#which is somewhat surprising. 

#Thank you very much for reading through this project! It has been a real pleasure, and we hope you have a wonderful semester. Mich & Ramiz

#ADDITIONAL POINTS REMAINING: A ONE-PAGE DOCUMENT THAT DISCUSSES ETHICAL ISSUES RELATED TO COLLECTION; 
#A ONE PAGE DOCUMENT DISCUSSING ETHICAL ISSUES RELATING TO CONCLUSIONS; 
#TEAM CONSISTS OF EXACTLY TWO MEMBERS. 
#A VIDEO OF THE SHORT SCRIPT IS POSTED ON YOUTUBE: https://www.youtube.com/watch?v=AOiKLqNAioM&feature=youtu.be&fbclid=IwAR023pM_9zXY5Erz24uedas9bxxzAvSjlD5tcQ_TttWeAQKSFfK7gpTEhRI

#ADDITIONAL POINT (FOR THE BANTER): IMMEDIATELY DISBAND THE SEARCH COMMITTEE AND HIRE THEM, FOUR POINTS TO GRYFFINDOR!!!!


