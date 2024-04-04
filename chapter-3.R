
  
### Loading Packages
  
library(tidyverse)
library(multcomp)
library(tibble)
library(yarrr)
library(readr)
library(mosaic)


# 3.10 Practice problems

# 3.1. Cholesterol Analysis For the first practice problems, you will work with the cholesterol data set from the `multcomp` package that was used to generate the Tukey’s HSD results. To load the data set and learn more about the study, use the following code:
  
cholesterol <- as_tibble(cholesterol)
help(cholesterol)


## 3.1.1. Graphically explore the differences in the changes in Cholesterol levels for the five levels using pirate-plots.

pirateplot(response ~ trt, data = cholesterol, inf.method = "ci", inf.disp = "line")


## 3.1.2. Is the design balanced? Generate R output to support this assessment.

## Answer: The data to be balanced. The QQ-plot suggests that the data follows as the data points lie roughly on the line indicating a the linear model. Furthermore, the density plot suggests that the data depicts a roughly normal distribution.

favstats(response ~ trt, data = cholesterol)

ch_lm <- lm(response ~ trt, data = cholesterol)

cholesterol <- cholesterol %>% 
  mutate(res = residuals(ch_lm)) 

cholesterol %>% 
  ggplot(aes(sample = res)) +
  geom_qq() +
  stat_qq_line() +
  theme_bw() +
  labs(title = "QQ-Plot of residuals")

cholesterol %>% ggplot(mapping = aes(x = res)) +
  geom_density() +
  labs(x = "Residuals",
       y = "Density",
       title = "Density plot of residuals") +
  theme_bw()


## 3.1.3. Complete all 6+ steps of the hypothesis test using the parametric F-test, reporting the ANOVA table and the distribution of the test statistic under the null. When you discuss the scope of inference, make sure you note that the treatment levels were randomly assigned to volunteers in the study.

## $H_0$: $\mu_1$ = $\mu_2$ = $\mu_4$ = $\mu_D$ = $\mu_E$ 
## $H_a$: Not all $\mu$ are equal 


par(mfrow = c(2,2))
plot(ch_lm, pch = 16)


## Due to the lack of sufficient information, we can start by assuming that there was little interactions between the patients, and hence assume independence between different groups and that they are exclusive.

## The anova is a parametric approach. The anova generates the following p-value and F-statistic values.

anova(ch_lm)


## Given tbe p-value, we can conclude that there is strong evidence against the null hypothesis that all treatments will yield the same results.

## The various treatment groups for cholesterol patients exhibited a mean difference ranging from ~5 to ~22 in cholesterol reduction. However, due to the lack of additional information about the individuals included in this study, we are unable to establish a specific scope of inference.

## 3.1.4. Generate the permutation distribution and find the p-value. Compare the parametric p-value to the permutation test results.

## The p-value of the parametric and permutation tests are very similar with both values being less than 0.0001. 

Tobs <- anova(lm(response ~ trt, data = cholesterol))[1,4]; Tobs

B <- 1000
cholesterol_d <- matrix(NA, nrow = B)

for (b in (1:B)){
  cholesterol_d[b] <- anova(lm(response ~ shuffle(trt), data = cholesterol))[1,4]
}

pdata(cholesterol_d, Tobs, lower.tail = F)[[1]]

tibble(cholesterol_d) %>% 
  ggplot(aes(x = cholesterol_d)) +
  geom_histogram(aes(y = ..ncount..), bins = 25, col = 1, fill = "skyblue") +
  geom_density(aes(y = ..scaled..)) +
  theme_bw() +
  labs(y = "Density") +
  geom_vline(xintercept = Tobs, col = "red", lwd = 2) +
  stat_bin(aes(y = ..ncount.., label = ..count..), bins = 25, geom = "text", vjust = -0.75)


## 3.1.5. Perform Tukey’s HSD on the data set. Discuss the results – which pairs were detected as different and which were not? Bigger reductions in cholesterol are good, so are there any levels you would recommend or that might provide similar reductions?
  
Tm2 <- glht(ch_lm, linfct = mcp(trt = "Tukey"))
confint(Tm2)
plot(Tm2)


## 3.1.6. Find and interpret the CLD and compare that to your interpretation of results from 3.1.5.
## The Tukey plot suggests that drugE has the highest success rate. Thus, suggested drug would be drugE.


confint(ch_lm)


## 3.2. Sting Location Analysis These data come from [Smith, 2014] where the author experimented on himself by daily stinging himself five times on randomly selected body locations over the course of months. You can read more about this fascinating (and cringe inducing) study at https://peerj.com/articles/338/.

## The following code gets the data prepared for analysis by removing the observations he took each day on how painful it was to sting himself on his forearm before and after the other three observations that were of interest each day of the study. This is done with a negation (using $“!”$ of the `%in%` which identifies rows related to the two daily forearm locations (`Forearm` and `Forearm1`) to leave all the rows in the data set for any levels of `Body_Location` that were not in these two levels. This is easier than trying to list all 24 other levels, then `Body_Location` variable is re-factored to clean out its unused levels, and finally the reorder function is used to order the levels based on the sample mean pain rating – and the results of these steps are stored in the `sd_fixedR` tibble.
