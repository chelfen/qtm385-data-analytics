library(tidyverse)
library(scales)
library(gmodels)
library(lubridate)
library(plotly)
library(ggplot2)
library(car)
library(AER)
library(cobalt)
library(grf)
library(policytree)
library(DiagrammeR)

setwd('/Users/clairefenton/Desktop/ecomm_performance_data')

##### Milestone 1
m1 <- read_csv('milestone-1/ecomm_rev_2020.csv') 

### Exploration
nrow(m1)
length(unique(m1$CUSTID))

# Around 25% given display ad
mean(m1$ATTRIB_DISPLAY_AD)

# Average values based on given ad
m1 %>%
  group_by(ATTRIB_DISPLAY_AD) %>%
  summarise(avg_cart = mean(CART_TOTAL), avg_prev_checkouts = mean(PREVIOUS_CHECKOUTS), 
            avg_page_views = mean(PAGE_VIEWS), avg_decile = mean(ESTIMATED_INCOME_DECILE),
            avg_prod_views = mean(PRODUCT_VIEWS))
# Control for decile level too
m1 %>%
  group_by(ESTIMATED_INCOME_DECILE, ATTRIB_DISPLAY_AD) %>%
  summarise(avg_cart = mean(CART_TOTAL), avg_prev_checkouts = mean(PREVIOUS_CHECKOUTS), 
            avg_page_views = mean(PAGE_VIEWS), avg_prod_views = mean(PRODUCT_VIEWS))
# People who saw the ad tended to have more previous checkouts and a higher income level

# Visuals
hist(m1$PREVIOUS_CHECKOUTS)
hist(m1$PAGE_VIEWS)
hist(m1$PRODUCT_VIEWS)
hist(m1$CART_TOTAL)
hist(m1$ESTIMATED_INCOME_DECILE)

# Visuals split between ATTRIB and not ATTRIB
m1_ad <- m1 %>% filter(ATTRIB_DISPLAY_AD == 1)
m1_no_ad <- m1 %>% filter(ATTRIB_DISPLAY_AD == 0)
hist(m1_ad$PREVIOUS_CHECKOUTS)
hist(m1_ad$PAGE_VIEWS)
hist(m1_ad$PRODUCT_VIEWS)
hist(m1_ad$CART_TOTAL) # interesting
hist(m1_ad$ESTIMATED_INCOME_DECILE) # interesting

hist(m1_no_ad$PREVIOUS_CHECKOUTS)
hist(m1_no_ad$PAGE_VIEWS)
hist(m1_no_ad$PRODUCT_VIEWS)
hist(m1_no_ad$CART_TOTAL)
hist(m1_no_ad$ESTIMATED_INCOME_DECILE)

### Modeling effect of ATTRIB
model <- lm(CART_TOTAL ~ ATTRIB_DISPLAY_AD + PREVIOUS_CHECKOUTS + PAGE_VIEWS + factor(ESTIMATED_INCOME_DECILE) + PRODUCT_VIEWS, data=m1)
summary(model)
return <- coef(model)[['ATTRIB_DISPLAY_AD']] * sum(m1$ATTRIB_DISPLAY_AD)
return
roi <- (return - 10000)/10000
roi

##### Milestone 2

m2 <- read_csv('milestone-2/ecomm_rev_2020_incl_assn.csv') 

## 2 Stage Least Squares
s1 <- lm(ATTRIB_DISPLAY_AD ~ ASSIGNED_DISPLAY_AD + PREVIOUS_CHECKOUTS + PAGE_VIEWS + factor(ESTIMATED_INCOME_DECILE) + PRODUCT_VIEWS, data=m2)
summary(s1)
phat <- predict(s1, newdata=m2)
s2 <- lm(CART_TOTAL ~ phat + PREVIOUS_CHECKOUTS + PAGE_VIEWS + factor(ESTIMATED_INCOME_DECILE) + PRODUCT_VIEWS, data=m2, x=TRUE)
coef(s2)[["phat"]]

summary(s2)
## ROI
return <- coef(s2)[["phat"]] * nrow(m2)
return
roi <- (return - 10000)/10000
roi
## Same IV analysis
aeriv <- ivreg(CART_TOTAL ~ ATTRIB_DISPLAY_AD + PREVIOUS_CHECKOUTS + PAGE_VIEWS + factor(ESTIMATED_INCOME_DECILE) + PRODUCT_VIEWS | ASSIGNED_DISPLAY_AD + PREVIOUS_CHECKOUTS + PAGE_VIEWS + factor(ESTIMATED_INCOME_DECILE) + PRODUCT_VIEWS, data=m2)
summary(aeriv)
sqrt(vcovHC(aeriv))[2,2]
## 90% CI for effect
coef(aeriv)['ATTRIB_DISPLAY_AD'] + c(-1.96, 1.96)*sqrt(vcovHC(aeriv))[2,2]

##Covariates Balancing Test: ATTRIB_DISPLAY_AD unbalanced
treated <- m2 %>% filter(ATTRIB_DISPLAY_AD == 1)
control <- m2 %>% filter(ATTRIB_DISPLAY_AD == 0)

list_var <- names(m2)[3:6]
columns <- c("Variable", "Treated Mean","Control Mean",
             "Difference","T-stat","P-value") 
out <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(out) <- columns

for(i in 1:length(list_var)){
  t <- t.test(treated[,list_var[i]], control[,list_var[i]])
  res <- c(t$estimate[1],t$estimate[2],
           t$estimate[1] - t$estimate[2],
           t$statistic,t$p.value)
  res <- round(as.numeric(res),3)
  res <- c(list_var[i], res)
  out[nrow(out) + 1,] <- res
}
out

##Covariates Balancing Test: ASSIGNED_DISPLAY_AD (much better balance)
treated <- m2 %>% filter(ASSIGNED_DISPLAY_AD == 1)
control <- m2 %>% filter(ASSIGNED_DISPLAY_AD == 0)

list_var <- names(m2)[3:6]
columns <- c("Variable", "Treated Mean","Control Mean",
             "Difference","T-stat","P-value") 
out <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(out) <- columns

for(i in 1:length(list_var)){
  t <- t.test(treated[,list_var[i]], control[,list_var[i]])
  res <- c(t$estimate[1],t$estimate[2],
           t$estimate[1] - t$estimate[2],
           t$statistic,t$p.value)
  res <- round(as.numeric(res),3)
  res <- c(list_var[i], res)
  out[nrow(out) + 1,] <- res
}
out

## Using cobalt to check balance, but analysis of outputs unclear
bal.tab(m2[,3:6], continuous = 'std', treat = m2$ATTRIB_DISPLAY_AD, stats = c("m", "v", "ks"))

love.plot(m2[,3:6], treat = m2$ATTRIB_DISPLAY_AD, continuous = "std", binary = "std",
          stats = c("m"),
          thresholds = c(m = .1), line = TRUE)

## Matching
treated <- m2 %>% filter(ASSIGNED_DISPLAY_AD == 1)
control <- m2 %>% filter(ASSIGNED_DISPLAY_AD == 0)

mod.ps <- lm(CART_TOTAL ~ ATTRIB_DISPLAY_AD + PREVIOUS_CHECKOUTS + PAGE_VIEWS + factor(ESTIMATED_INCOME_DECILE), data = m2)
m2$ps <- predict(mod.ps, type = 'response')
treated$ps <- predict(mod.ps, type = 'response', newdata = treated)
control$ps <- predict(mod.ps, type = 'response', newdata = control)

k <- 1
treated_match <- c()
for(i in 1:nrow(treated)){
  d <- mahalanobis(as.matrix(control$ps),
                   as.matrix(treated[i,]$ps),
                   cov = var(m2$ps))
  d <- unname(d)
  treated_match[i] <- mean(control$CART_TOTAL[order(d)[1:k]])
}

control_match <- c()
for(i in 1:nrow(control)){
  d <- mahalanobis(as.matrix(treated$ps),
                   as.matrix(control[i,]$ps),
                   cov = var(m2$ps))
  d <- unname(d)
  control_match[i] <- mean(treated$CART_TOTAL[order(d)[1:k]])
}

ITE_treated <- treated$CART_TOTAL - treated_match
ITE_control <- control_match - control$CART_TOTAL
ITE <- c(ITE_control, ITE_treated)
#ATT
mean(ITE_treated)
mean(ITE_control)
mean(ITE)
## Looking at differences in avg cart total for each decile/attrib
treated %>%
  group_by(ESTIMATED_INCOME_DECILE, ATTRIB_DISPLAY_AD) %>%
  summarise(mean(CART_TOTAL))

### Cov balancing: attributed within the assigned group (not balanced)
yep <- treated %>% filter(ATTRIB_DISPLAY_AD == 1)
nope <- treated %>% filter(ATTRIB_DISPLAY_AD == 0)

list_var <- names(treated)[3:6]
columns <- c("Variable", "Treated Mean","Control Mean",
             "Difference","T-stat","P-value") 
out <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(out) <- columns

for(i in 1:length(list_var)){
  t <- t.test(yep[,list_var[i]], nope[,list_var[i]])
  res <- c(t$estimate[1],t$estimate[2],
           t$estimate[1] - t$estimate[2],
           t$statistic,t$p.value)
  res <- round(as.numeric(res),3)
  res <- c(list_var[i], res)
  out[nrow(out) + 1,] <- res
}
out

### Milestone 3
m3 <- read_csv('milestone-3/ecomm_rev_2020_omnipotent.csv')

m3 %>% filter(ASSIGNED_DISPLAY_AD == 1) %>% group_by(ATTRIB_DISPLAY_AD) %>% summarise(mean(CART_TOTAL_Y0), mean(CART_TOTAL_Y1), mean(CART_TOTAL))
m3 %>% filter(ASSIGNED_DISPLAY_AD == 0) %>% summarise(mean(CART_TOTAL_Y0), mean(CART_TOTAL_Y1), mean(CART_TOTAL))

## compute individual effects
m3 %>% group_by(CUSTID) %>% summarise(effect = CART_TOTAL_Y1 - CART_TOTAL_Y0) %>% ggplot(aes(x=effect)) + geom_histogram()

m3 %>% group_by(CUSTID) %>% summarise(effect = CART_TOTAL_Y1 - CART_TOTAL_Y0, ASSIGNED_DISPLAY_AD = ASSIGNED_DISPLAY_AD) %>% ggplot(aes(x=effect)) + geom_histogram()

m3 %>% group_by(CUSTID) %>% summarise(effect = CART_TOTAL_Y1 - CART_TOTAL_Y0) %>% ungroup() %>% summarise(mean(effect), median(effect))

m3 %>% group_by(CUSTID) %>% 
  summarise(effect = CART_TOTAL_Y1 - CART_TOTAL_Y0, ASSIGNED_DISPLAY_AD = ASSIGNED_DISPLAY_AD, ATTRIB_DISPLAY_AD = ATTRIB_DISPLAY_AD) %>% 
  ungroup() %>% 
  group_by(ASSIGNED_DISPLAY_AD, ATTRIB_DISPLAY_AD) %>% 
  summarise(mean(effect), median(effect))

## Policy tree
# Estimating and summarizing CATEs
Y <- m3$CART_TOTAL
W <- m3$ASSIGNED_DISPLAY_AD
decile <- m3$ESTIMATED_INCOME_DECILE
X <- m3[,c(3:6)]

cf <- causal_forest(X, Y, W, W.hat = 0.5, clusters = decile)
cf
# Use train/evaluation school split from above, but use non-missing units for policy_tree
train.rat <- 0.7
N <- nrow(m3)

trainon <- sample(1:N, size=ceiling(train.rat*N))
test <- (1:N)[-trainon]

train <- m3[trainon,]
eval <- m3[test,]

# Compute doubly robust scores
dr.scores <- get_scores(cf)
# Use as the ATE as a "cost" of program treatment to find something non-trivial
ate <- average_treatment_effect(cf)
cost <- ate[["estimate"]]
dr.rewards <- cbind(control=-dr.scores, treat=dr.scores - cost)

# Fit depth 2 tree on training subset
train <- sample(1:N, size=ceiling(train.rat*N))
test <- (1:N)[-trainon]


## Depth 2: 14%
tree <- policy_tree(X[train, ], dr.rewards[train, ], min.node.size = 100, depth = 2)
plot(tree, leaf.labels = c("dont treat", "treat"))

treat <- c(rep(0, nrow(m3)))
for (i in 1:nrow(m3)) {
  if (m3$PREVIOUS_CHECKOUTS[i] <= 2) {
    if (m3$ESTIMATED_INCOME_DECILE[i] == 10) {
      treat[i] <- 1
    }
  }
  else if (m3$PAGE_VIEWS[i] > 5){
    treat[i] <- 1
  }
}

mean(treat)

m3 %>%
  mutate(policy = treat) %>%
  group_by(CUSTID) %>%
  mutate(observed = ifelse(policy == 1, CART_TOTAL_Y1, CART_TOTAL_Y0)) %>%
  group_by(policy) %>%
  summarise(mean(PRODUCT_VIEWS), mean(ESTIMATED_INCOME_DECILE), mean(PAGE_VIEWS), mean(PREVIOUS_CHECKOUTS), mean(observed))


## Depth 3: ??%
tree <- policy_tree(X[trainon, ], dr.rewards[trainon, ], min.node.size = 100, depth = 3)
plot(tree, leaf.labels = c("dont treat", "treat"))

treat <- c(rep(0, nrow(m3)))
for (i in 1:nrow(m3)) {
  if (m3$PAGE_VIEWS[i] <= 6) {
    if (m3$PAGE_VIEWS[i] <= 3) {
      if (m3$ESTIMATED_INCOME_DECILE[i] <= 2) {
        treat[i] <- 1
      }
    } 
  }
  else if (m3$ESTIMATED_INCOME_DECILE[i] <= 6) {
    if (m3$ESTIMATED_INCOME_DECILE[i] > 3) {
      treat[i] <- 1
    }
  }
  else if (m3$ESTIMATED_INCOME_DECILE[i] > 8) {
    treat[i] <- 1
  }
}

mean(treat)

nrow(m3 %>% filter(CART_TOTAL_Y1 > CART_TOTAL_Y0))/nrow(m3)


m3 %>%
  mutate(policy = treat) %>%
  group_by(CUSTID) %>%
  mutate(observed = ifelse(policy == 1, CART_TOTAL_Y1, CART_TOTAL_Y0)) %>%
  group_by(policy) %>%
  summarise(mean(PRODUCT_VIEWS), mean(ESTIMATED_INCOME_DECILE), mean(PAGE_VIEWS), mean(PREVIOUS_CHECKOUTS), mean(observed), mean(CART_TOTAL_Y0), mean(CART_TOTAL_Y1))

  
m3 %>%
  filter(PAGE_VIEWS == 0)


