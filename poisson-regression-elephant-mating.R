---
title: "Poisson regression"
---
  
### Loading Packages
  
library(tidyverse)
library(readr)
library(Sleuth2)
library(gridExtra)
library(lmtest)


### Load data

data <- case2201 %>%
  mutate(age_sq = Age^2)


#### Elephant mating

How does age affect male elephant mating patterns? An article by Poole (1989) investigated whether mating success in male elephants increases with age and whether there is a peak age for mating success. To address this question, the research team followed 41 elephants for one year and recorded both their ages and their number of matings. The data (Ramsey and Schafer 2002) is found in `elephant.csv`, and the variables are: 
  
  `MATINGS` = the number of matings in a given year 

`AGE` = the age of the elephant in years. \\

a. Create a histogram of MATINGS. Is there preliminary evidence that number of matings could be modeled as a Poisson response? Explain.

Answer: Yes. Since a poisson regression is used to model count data, it's important to check if the distribution of the response variable (MATINGS in this case) resembles that of a Poisson distribution. From the histogram, the distribution of MATINGS appears to be approximately right-skewed and is discrete, which is characteristic of count data. Additionally, the shape of the histogram somewhat resembles that of a Poisson distribution. However, further analysis, such as formal goodness-of-fit tests or examining overdispersion, may be necessary to confirm whether the MATINGS data can be appropriately modeled using a Poisson regression.

hist(data$Matings) 



    b. Plot MATINGS by AGE. Add a least squares line. Is there evidence that modeling matings using a linear regression with age might not be appropriate? Explain. (Hints: fit a smoother; check residual plots).
    
    Answer: Upon examining the relationship between matings and age, I observed that while the SLR model line indicates a (roughly) linear relationship, the smoother suggests a potential non-linear trend, possibly quadratic or higher-order. The line of best fit suggests a relationship but doesn't accurately represent the spread of our data.
{r, warning=FALSE}
# Create the first plot with a linear regression line
plot1 <- ggplot(data, aes(x = Age, y = Matings)) +
  geom_point() +
  geom_abline(slope = coef(lm(Matings ~ Age, data = data))[2], 
              intercept = coef(lm(Matings ~ Age, data = data))[1], 
              color = "red") +
  ggtitle("Linear Regression") +
  theme_classic()

# Create the second plot with loess smooth line
plot2 <- ggplot(data, aes(x = Age, y = Matings)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +
  ggtitle("Loess Smooth") +
  theme_classic()

plot(residuals(lm(Matings ~ Age, data = data)))

# Arrange the plots side by side
grid.arrange(plot1, plot2, ncol = 2)



c. For each age, calculate the mean number of matings. Take the log of each mean and plot it by AGE.

i. What assumption can be assessed with this plot?
  Answer: log_mean_matings appears to increase with age, or at least this trend is visually apparent.

ii. Is there evidence of a quadratic trend on this plot?
  Answer: Upon visual inspection of the histogram of MATINGS, there is not compelling evidence to suggest that the number of matings could be accurately modeled as a Poisson response. Instead, it appears that the relationship between the variables could potentially be simply linear. There is no clear indication of a quadratic trend in the plot. However, the dataset also contains only 19 points after aggregation.


df1 <- data %>%
  group_by(Age) %>%
  summarise(mean_matings = mean(Matings)) %>%
  mutate(log_mean_matings = log(mean_matings))

ggplot(df1, aes(x=Age, y=log_mean_matings, col=Age)) +
  geom_point() +
  theme_classic() +
  scale_color_fermenter()


d. Fit a Poisson regression model with a linear term for AGE. Exponentiate and then interpret the coefficient for AGE.

Answer: Exp(Age = 0.06869) ≈ 1.0711. For every one-unit increase in Age, the expected count of Matings increases by approximately 7.11%. Additionally, the coefficient for "Age" is statistically significant with a p-value much smaller than 0.05, indicating that there is strong evidence to suggest that the variable "Age" is associated with the number of Matings and is a useful statistic.

lm2 <- glm(Matings ~ Age, family=poisson, data=data)
summary(lm2)
exp(lm2$coeff[2])



e. Construct a 95% confidence interval for the slope and interpret in context (you may want to exponentiate endpoints).

Answer: 95% Confidence Interval for the slope of Age is (1.04, 1.10)

conf_interval <- confint(lm2, "Age", level = 0.95)
exp_conf_interval <- exp(conf_interval)
exp_conf_interval



f. Are the number of matings significantly related to age? Test with

i. a Wald test and
ii. a drop in deviance test.

The p-value for age in the Wald test is extremely small (p = 5.81e-07), indicating a highly significant relationship between age and the number of matings. Similarly, the p-value for the drop in deviance test is also small (p = 7.991e-07), providing a strong avidence of the significant association between matings and age. These results strongly suggest that age plays a crucial role in predicting the number of matings and should therefore be considered in the model rather than excluded. This is a great way to ensure the fitness of the model with age.

lm2$coef[2]/summary(lm2)$coefficients["Age", "Std. Error"]

waldtest <- glm(Matings ~ 1, family = poisson, data = data)
anova(lm2, waldtest, test = "Chisq")



g. Add a quadratic term in AGE to determine whether there is a maximum age for the number of matings for elephants. Is a quadratic model preferred to a linear model? To investigate this question, use

i. a Wald test and
ii. a drop in deviance test.

The p-value for the quadratic value of age in the second model confirming Wald test is 0.669, indicating that the linear model (with simply Age) performs significantly better than the quadratic model. Similarly, the p-value for the drop in deviance test is 0.6667 for the model with quadratic value of age, further supporting the notion that the quadratic term for age should be excluded from the model and doesnt provide any valuable results. Therefore, based on both tests, it is advisable to retain the linear model and exclude the quadratic term for age.

lm3 <- glm(Matings ~ Age + age_sq, family=poisson, data=data)
summary(lm3)

lm3$coef[2]/summary(lm3)$coefficients["age_sq", "Std. Error"] # Wald test

anova(lm2, lm3, test = "Chisq") # Drop in deviance test



    h. What can we say about the goodness-of-fit of the model with age as the sole predictor? Compare the residual deviance for the linear model to a $χ^2$ distribution with the residual model degrees of freedom.
    
    A p-value of 0.09426231 indicates that there is approximately a 9.43% chance of observing a deviance statistic as extreme as or more extreme than the one observed under the null hypothesis. Thus, we would fail to reject the null hypothesis. We do not have sufficient evidence to conclude that the model does not fit the data well at the 5% significance level.

1 - pchisq(lm2$deviance, lm2$df.residual)


    i. Fit the linear model using quasi-Poisson regression. (Why?)
        i. How do the estimated coefficients change?
        ii. How do the standard errors change?
        iii. What is the estimated dispersion parameter?
        iv. An estimated dispersion parameter greater than 1 suggests overdispersion. When adjusting for overdispersion, are you more or less likely to obtain a significant result when testing coefficients? Why?
        
        In the quasipoisson model, the standard error for coefficients increased and the respective p-values decreased. This suggessts more uncertainity in the quasipoisson model. The coefficients remain  the same.
        Since the difference in deviance is 0 and the associated p-value is 1, it indicates that there is no significant difference between the two models. The results suggest that both the regular Poisson regression model and the quasi-Poisson regression model provide similar estimates for the coefficients. However, the quasi-Poisson model accounts for potential overdispersion in the data, as indicated by the estimated dispersion parameter. The analysis of deviance further confirms that incorporating overdispersion does not significantly improve the model fit. Therefore, the quasi-Poisson model may be preferred when dealing with count data exhibiting overdispersion.

quasi_lm2 <- glm(Matings ~ Age, family = quasipoisson, data = data)
summary(quasi_lm2)



overdispersionCheck = sum(residuals(lm2, type = "pearson")^2 / lm2$df.res)
summary(quasi_lm2, dispersion = overdispersionCheck)
anova(lm2, quasi_lm2, test = "Chisq", dispersion = overdispersionCheck)
1 - pchisq(overdispersionCheck, lm2$df.residual)


