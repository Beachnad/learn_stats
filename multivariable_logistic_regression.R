#' LOGISTIC REGRESSION TUTORIAL
#' 
#' The following lesson comes from:
#' https://www.youtube.com/watch?v=C4N3_XJJ-jU
#' 
#' The lesson covers the basic on how to perform a logistic
#' regression from scratch.

library(dplyr)
library(ggplot2)
library(dbr)

# Data cleaning and prep
data <- read.csv('data/heart.csv', header=T) %>%
  rename(hd=target) %>%
  mutate(
    sex = as.factor(ifelse(sex == 0, 'F', 'M')),
    cp = as.factor(cp),
    fbs = as.factor(fbs),
    restecg = as.factor(restecg),
    exang = as.factor(exang),
    slope = as.factor(slope),
    ca = as.factor(as.integer(ca)),
    thal = as.factor(as.integer(thal)),
    hd = as.factor(ifelse(hd == 1, 'Unhealthy', 'Healthy'))
  )

# checking that structure that cleaning is done
str(data)

# Check that we have healthy and diseased samples for each gender
xtabs(~ hd + sex, data = data)

# check this for the other boolean and categorical variables
xtabs(~ hd + cp,      data = data)
xtabs(~ hd + fbs,     data = data)
xtabs(~ hd + restecg, data = data)  # not very many 2's
xtabs(~ hd + slope,   data = data)
xtabs(~ hd + ca,      data = data)       # not very many 4's
xtabs(~ hd + thal,    data = data)

# LOGISTIC REGRESSION ================

#' Starting simple
#' 
#' First, we are going to create a model that does
#' nothing more than use gender as an input in order
#' to predict heart disease (hd).

sex_only_logistic <- glm(hd ~ sex + trestbps, data=data, family='binomial')
summary(sex_only_logistic)

all_vars_logistic <- glm(hd ~ ., data=data, family='binomial')
summary(all_vars_logistic)

# McFadden pseudo R squared
ll.null <- all_vars_logistic$null.deviance / -2  # log-likelihood of null hypothesis
ll.proposed <- all_vars_logistic$deviance / -2   # log-likelihood pf prop hypothesis

# R squared of 0.569889  - This values represents the overall effect size,
#   where bigger == Better
#' ! IMPORANT NOTE ! 
#' There are many ways to calculate R squared for logistic regression,
#' over ten in fact. Research what is commonly used in your field
#' in order to determine what method to use. THe method below is
#' just one way of doing this.
(ll.null - ll.proposed) / ll.null
# very low p-value, so we know that our results were not dumb luck
1 - pchisq(2 * (ll.proposed - ll.null), df=(length(all_vars_logistic$coefficients) - 1) )

# VISUALIZING THE MODEL =========

# create dataframe that contains the predicted odds of a patient having heart disease
predicted.data <- data %>%
  mutate(prob.of.hd=all_vars_logistic$fitted.values,
         prob.of.hd.sex = sex_only_logistic$fitted.values) %>%
  arrange(prob.of.hd) %>%
  mutate(rank = row_number())


ggplot(predicted.data, aes(x=rank, y=prob.of.hd, color=hd)) +
  geom_point(alpha=1, stroke=1) +
  scale_color_manual(values=c('Unhealthy'='#ff6060', 'Healthy'='#11ff8d')) +
  xlab('Index') +
  ylab('Probability of Heart Disease') +
  db_dark_theme() %+replace%
  theme(legend.position = 'right')

predicted.data %>%
  arrange(prob.of.hd.sex) %>%
  mutate(rank=row_number()) %>%
  ggplot(aes(x=rank, y=prob.of.hd.sex, color=hd)) +
  geom_point(alpha=1, stroke=1) +
  xlab('Index') +
  ylab('Probability of Heart Disease') +
  db_dark_theme() %+replace%
  theme(legend.position = 'right')


# SUPPLEMENTAL EXPIRMENTATION ==========

#' ROC
#' ROC - Receiver operating characteristic:

library(pROC)
set.seed(420)

n_samples <- 100
weight <- sort(rnorm(n=n_samples, mean=172, sd=29))

obese <- ifelse((runif(n=n_samples)) < rank(weight)/100, 1, 0)

plot(x=weight, y=obese)
glm.fit = glm(obese ~ weight, family = binomial)
lines(weight, glm.fit$fitted.values)

# ADDITIONAL EXPIRMENTATION =========
#' All of the above as covered in the video lesson,
#' however, I wanted to explore a few more topics
#' to further my understanding and exapand upon
#' my ability to apply this method to a real project
library(MASS)

full.mod <- glm(hd ~ ., data = data, family = 'binomial')
summary(full.mod)

prob <- predict(full.mod, data, type='response')
pred.class <- ifelse(prob>0.5, 'Unhealthy', 'Healthy')
mean(pred.class == data$hd)

step.mod <- full.mod %>% stepAIC(trace=F)
summary(step.mod)
prob.step <- predict(step.mod, data, type='response')
step.pred.class <- ifelse(prob>0.5, 'Unhealthy', 'Healthy')
mean(step.pred.class == data$hd)
