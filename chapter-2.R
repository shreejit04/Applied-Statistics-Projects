---
  title: "chapter-2-freenwood"
author: "Shreejit Poudyal"
date: "2024-01-30"
output: html_document
---
  
  ### Loading Packages

library(tidyverse)
library(mosaic)
library(mosaicData)
library(yarrr)


### Loading Data

dd <- read_csv("https://www.math.montana.edu/courses/s217/documents/Walker2014_mod.csv")
mosaicdata <- data(HELPrct)


#### Problem 2.1
\textb{Overtake Distance Analysis} The tests for the overtake distance data were performed with two-sided alternatives and so two-sided areas used to find the p-values. Suppose that the researchers expected that the average passing distance would be less (closer) for the commute clothing than for the casual clothing group. Repeat obtaining the permutation-based p-value for the one-sided test for either the full or smaller sample data set. Hint: Your p-value should be just about half of what it was before and in the direction of the alternative.


B <- 1000
Ts_dd <- matrix(NA, nrow = B)

dd <- dd %>%
  filter(Condition %in% c("casual", "commute"))
for (b in (1:B)){
  lmP <- lm(Distance ~ shuffle(Condition), data = dd)
  Ts_dd[b] <- coef(lmP)[2]
}



summary(lmP)
summary(Ts_dd)


#### Problem 2.2. 
\textb{HELP Study Data Analysis} Load the HELPrct data set from the mosaicData package [Pruim et al., 2021a] (you need to install the mosaicData package once to be able to load it). The HELP study was a clinical trial for adult inpatients recruited from a detoxification unit. Patients with no primary care physician were randomly assigned to receive a multidisciplinary assessment and a brief motivational intervention or usual care and various outcomes were observed. Two of the variables in the data set are sex, a factor with levels male and female and daysanysub which is the time (in days) to first use of any substance post-detox. We are interested in the difference in mean number of days to first use of any substance post-detox between males and females. There are some missing responses and the following code will produce `favstats` with the missing values and then provide a data set that by applying the `drop_na()` function to the piped data set removes any observations with missing values.

HELPrct2 <- HELPrct %>% 
  select(daysanysub, sex)

HELPrct3 <- HELPrct2 %>% 
  drop_na()

favstats(daysanysub ~ sex, data = HELPrct2)
favstats(daysanysub ~ sex, data = HELPrct3)


#### 2.2.1. 
Based on the results provided, how many observations were missing for males and females? Missing values here likely mean that the subjects didn’t use any substances post-detox in the time of the study but might have at a later date – the study just didn’t run long enough. This is called censoring. What is the problem with the numerical summaries here if the missing responses were all something larger than the largest observation? \\

Answer: For males, 159 observations were missing and 50 for women were missing. If in case numerical summaries here if the missing responses were all something larger than the largest observation, that would provide biased estimates in favor the recorded values. Moreover, we would also be missing essential data points to be able to make correct conclusions from the data.

#### 2.2.2. 
Make a pirate-plot and a boxplot of daysanysub ~ sex using the HELPrct3 data set created above. Compare the distributions, recommending parametric or nonparametric inferences. 

pirateplot(daysanysub ~ sex, data = HELPrct3, inf.method = "ci", inf.disp = "line")

Average consumption of any substances post-detox by female subjects in the time of the study was higher than males. Furthermore, a greater proportion of women (compared to total women) seen to exceed the number of days for consumption of any substances post-detox compared to males. However, in numbers male population has a greater number. The average `daysanysub` interval for males and females differs significantly, with that of men being much lower.

#### 2.2.3. 
Generate the permutation results and write out the 6+ steps of the hypothesis test. 

B <- 1000
Tstar <- matrix(NA, nrow = B)

for (b in (1:B)){
  lmP <- lm(daysanysub ~ shuffle(sex), data = HELPrct3)
  Tstar[b] <- coef(lmP)[2]
}



summary(lmP)
summary(Tstar)


Steps of the hypothesis test.\\
* Identify research question/objective, and study data (significance and usage). \\
* What are ways we can understand/visualize the data? \\
* Identify appropriate model/test statistic that is needed for the particular study. \\
* Identify population \\
1. State the null and alternative hypothesis that the population be based on. \\
2. State any assumptions made for the analysis, and ensure any conditions for required distributions are met. \\
3. Calculate the test statistic and the p-value, where the p-value represents the probability of getting the observed test statistic or something more extreme when ${H_0}$ is true. \\
4. State the conclusion, where if p-value > $\alpha$, then we fail to reject the null hypothesis, or that we do not have enough evidence to support the ${H_a}$. Whereas, if p-value< $\alpha$ then we reject the null hypothesis or that there is enough evidence to conclude in favor of ${H_a}$.\\
5. Create reports based on findings/data. \\
6. Discuss implementation of results/scope in real world.\\

#### 2.2.4. 
Interpret the p-value for these results. \\
The p-value indicates that there is insufficient evidence to reject the null hypothesis, suggesting that the variable "sex" does not have a statistically significant effect on the response variable `daysanysub` in the linear regression model.

#### 2.2.5. 
Generate the parametric test results using lm, reporting the test-statistic, its distribution under the null hypothesis, and compare the p-value to those observed using the permutation approach. \\
The p-value obtained from lm test gives further evidence of insufficient evidence to reject the null hypothesis, suggesting that the variable "sex" does not have a statistically significant effect on the response variable `daysanysub` in the linear regression model due to the higher p-value compared to that of permutation results.\\


lm_test <- lm(daysanysub ~ sex, data = HELPrct3)
summary(lm_test)


#### 2.2.6. 
Make and interpret a 95% bootstrap confidence interval for the difference in the means.
The red vertical line represents the observed coefficient, and the blue dashed lines represent the confidence interval boundaries. Our bootstrapped distribution seems roughly symmetric and indicative of difference in consumption of any substances post-detox by female subjects in the time of the study between the two groups of sex in the study.

set.seed(1237)
HELPrct_bst <- resample(HELPrct2)
HELPrct_bst2 <- resample(HELPrct2)

lm1 <- lm(daysanysub ~ sex, data = HELPrct_bst)
Tobs <- coef(lm1)[2]; Tobs

B <- 1000
set.seed(1234)
Tstar_bst <- matrix(NA, nrow = B)
for (b in (1:B)){
  lmP <- lm(daysanysub ~ sex, data = resample(HELPrct_bst))
  Tstar_bst[b] <- coef(lmP)[2]
}

quantiles <- qdata(Tstar_bst, c(0.025,0.975))

tibble(Tstar_bst) %>% 
  ggplot(aes(x = Tstar_bst)) +
  geom_histogram(aes(y = ..ncount..), bins = 15, col = 1, fill = "skyblue", center = 0) +
  geom_density(aes(y = ..scaled..)) +
  theme_bw() +
  labs(y = "Density") +
  geom_vline(xintercept = Tobs, col = "red", lwd = 2) +
  geom_vline(xintercept = quantiles, col = "blue", lwd = 2, lty = 2) +
  stat_bin(aes(y = ..ncount.., label = ..count..), bins = 15,
           geom = "text", vjust = -0.75)

qdata(Tstar_bst, 0.025)
qdata(Tstar_bst, 0.975)


