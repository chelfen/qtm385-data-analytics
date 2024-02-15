library(tidyverse)
library(scales)
library(gmodels)
library(lubridate)
library(plotly)
library(ggplot2)
library(car)
library(caret)

setwd('/Users/clairefenton/Desktop/Emory')

all.data <- read_delim('brand_acq_tnsactions.csv.gz', delim="\t") 

# Creates more workable samples so code runs better, delete for final product
cust_samp <- all.data %>% distinct(custKey) %>% sample_n(30000) %>% unlist() #same cust could have mult rows (transactions)
df <- all.data %>% filter(custKey %in% cust_samp)

# Reformatting the date key
df <- df %>%
  left_join(
    df %>%
      distinct(dtKey) %>%
      mutate(date = ymd(as.character(dtKey)))
  )

# Creating new columns: year, month, week and ymo
df <- df %>%
  mutate(
    year=year(date),
    month=month(date),
    week=week(date),
    ymo=ymd(sprintf("%04d-%02d-01", year, month)),
    ppu = gross/tnsCount,
    ppu = ifelse(is.na(ppu), 0, ppu),
    add_on = ifelse(orderType=='ADD-ON ORDER', 1, 0),
    business = ifelse(orderType=='BUSINESS ORDER', 1, 0),
    return = ifelse(orderType=='RETURN ORDER', 1, 0),
    retail = ifelse(orderType=='RETAIL ORDER', 1, 0),
    group = ifelse(orderType=='GROUP ORDER', 1, 0),
    influencer = ifelse(orderType=='INFLUENCER ORDER', 1, 0),
    promotional = ifelse(orderType=='PROMOTIONAL ORDER', 1, 0),
    unknown = ifelse(orderType=='UNKNOWN', 1, 0)
    )

resamp <- df %>%
  group_by(
    custKey, ymo
  ) %>%
  summarize(
    gross = sum(gross),
    num_sku = n(),
    avg_ppu = mean(ppu),
    avg_tnsCount = mean(tnsCount),
    avg_add_on = mean(add_on),
    avg_business = mean(business),
    avg_return = mean(return),
    avg_retail = mean(retail),
    avg_group = mean(group),
    avg_influencer = mean(influencer),
    avg_promotional = mean(promotional),
    avgg_unknown = mean(unknown)
  ) %>%
  arrange(custKey, ymo)


# Create an indicator for when each customer entered dataset,
# and the order of the purchase
resamp <- resamp %>%
  ungroup() %>%
  left_join(
    resamp %>% # arrange purchase months
      ungroup() %>%
      distinct(ymo) %>%
      arrange(ymo) %>%
      mutate(
        j = row_number()
      )
  ) %>% 
  group_by(custKey) %>%
  arrange(ymo) %>% # when did they come into the database and how many periods have we seen them for
  mutate(
    purchase_seq = row_number(),
    customer_entry = min(j),
    lifetime_seq = (j - customer_entry) + 1,
  )

nothing_changed <- resamp
resamp <- nothing_changed

resamp %>% select(customer_entry, lifetime_seq, timelag, j, purchase_seq) %>% group_by()

# average check size * number of checks per period * number of periods per lifetime

today <- max(resamp$j)

lifetime <- resamp %>%
  distinct(custKey, lifetime_seq) %>%
  group_by(custKey) %>%
  arrange(lifetime_seq) %>%
  summarize(
    # Summary lifetime info.
    firstdate=min(lifetime_seq),
    lastdate=max(lifetime_seq),
    lifetime=as.integer(lastdate-firstdate)+1,
    currentperiod=as.integer(today-lastdate),
    npurchases=n()
  ) %>%
  mutate(
    churned = as.integer(currentperiod > 8)
  )

lifetime

hist(lifetime$currentperiod) # apparently a lot of executives love to see this

gmodels::CrossTable(lifetime$churned) # 80% have churned (yikes)

gmodels::CrossTable(df.train$churned)

# now we can run a logistic regression model to compute churn probability
# Pr(y=1) = R + F + M + error

resamp <- resamp %>%
  left_join(
    lifetime %>%
      select(custKey, j=lastdate, churned)
  ) %>%
  mutate(
    churned = ifelse(is.na(churned), 0, churned)
  )

resamp <- resamp %>%
  group_by(custKey) %>%
  arrange(ymo) %>%
  mutate(
    # recency of purchase (R)
    # how recently did you buy something
    timelag = j - lag(j),
    timelag = ifelse(is.na(timelag), 0, timelag)
  )

gmodels::CrossTable(resamp$churned) # drops to around 3% here

max.lifetime <- resamp %>% group_by(custKey) %>% summarise(max = max(lifetime_seq))

train.rat <- 0.7
N <- nrow(resamp)

train <- sample(1:N, size=ceiling(train.rat*N))
test <- (1:N)[-train]

df.train <- resamp[train,]
df.test <- resamp[test,]

gmodels::CrossTable(resamp$churned) # drops to around 3% here

# omg the models!

### Regular Linear Model
glm.churn <- glm(churned ~ timelag + lifetime_seq + gross, data = df.train, family = binomial(link='logit'))
summary(glm.churn)

vif(glm.churn)

### Polynomial Models

# Quadratic on Timelag
glm.churn <- glm(churned ~ timelag + lifetime_seq + gross + timelag^2, data = df.train, family = binomial(link='logit'))
summary(glm.churn)

vif(glm.churn)

# No indications of multicollinearity among the present variables
for_correlation <- df.train %>% ungroup() %>% select(gross, lifetime_seq, timelag)

cor(for_correlation, method='pearson')

### Debating: do we include timelag*lifetime_seq variable?

# Not included
glm.churn <- glm(churned ~ factor(ymo) + timelag + lifetime_seq + gross + I(timelag^2), data = df.train, family = binomial(link='logit'))
summary(glm.churn)

df.test <- df.test %>%
  ungroup() %>%
  mutate(
    yhat = predict(glm.churn, df.test, type='response') # how do we interpret this y hat value?
  )

summary(df.test['yhat'])

hist(df.test$yhat, breaks=30)

df.train

# FULL MODEL
glm.churn_full <- glm(churned ~ factor(ymo) + timelag + I(timelag^2) + lifetime_seq + gross + I(gross^2) + avg_ppu + avg_business + avg_influencer + avg_retail + avg_group + avg_promotional + avg_add_on + (avg_group*avg_retail) + (avg_retail*avg_influencer), data = df.train, family = binomial(link='logit'))

summary(glm.churn_full)

#for_correlation <- df.train %>% ungroup() %>% select()
#cor(for_correlation, method='pearson')

vif(glm.churn_full)

df.test <- df.test %>%
  ungroup() %>%
  mutate(
    yhat = predict(glm.churn_full, df.test, type='response') # how do we interpret this y hat value?
  )

resamp <- resamp %>%
  ungroup() %>%
  mutate(
    yhat = predict(glm.churn_full, resamp, type='response') # how do we interpret this y hat value?
  )

#hist(df.test$yhat, breaks=30)

df.test %>% group_by(custKey) %>% mutate(max.life = max(lifetime_seq)) %>% filter(lifetime_seq == max.life, churned == 1) %>% select(custKey, churned, max.life)
df.test %>% group_by(custKey) %>% mutate(max.life = max(lifetime_seq)) %>% filter(lifetime_seq == max.life, churned == 0) %>% select(custKey, churned, max.life)

resamp %>% group_by(custKey) %>% mutate(max.life = max(lifetime_seq)) %>% filter(lifetime_seq == max.life, churned == 1) %>% select(custKey, churned, max.life)
resamp %>% group_by(custKey) %>% mutate(max.life = max(lifetime_seq)) %>% filter(lifetime_seq == max.life, churned == 0) %>% select(custKey, churned, max.life)


#resamp %>% filter(custKey == '00bb54d8a9b399b558c02f51c1106779') %>% select(churned) # why are they not churned? 

unchurned <- df.test %>% filter(churned == 0) %>% select(yhat) %>% summarise(mean = mean(yhat), sd = sd(yhat))

unchurned_yhat <- df.test %>% filter(churned == 0)

unchurned.mean <- as.numeric(unchurned['mean'][1])
unchurned.sd <- as.numeric(unchurned['sd'][1])

quantile(unchurned_yhat$yhat, .75)

upper <- unchurned.mean + 1.96 * (unchurned.sd / sqrt(nrow(df.test)))
lower <- unchurned.mean - 1.96 * (unchurned.sd / sqrt(nrow(df.test)))

df.test.predicted <- df.test %>% mutate(predicted = ifelse(yhat > quantile(unchurned_yhat$yhat, .75), 1, 0))

predicted <- factor(as.logical(unlist(df.test.predicted['predicted'])))
reference <- factor(as.logical(unlist(df.test.predicted['churned'])))

# Confusion Matrix
cf <- caret::confusionMatrix(data=predicted, reference=reference)
print(cf)


## Let's attempt to write this CLV function

#avg.month.gross <- resamp %>% group_by(j) %>% summarise(avg_gross = mean(gross))
#avg.chance <- resamp %>% group_by(j) %>% summarise(avg_chance = mean(yhat))

distinct.cust <- resamp %>% distinct(custKey)

clvs

clvs <- resamp %>% distinct(custKey) %>% mutate(clv = 0, churned = 0)

sample.keys <- distinct.cust[1:1000,]$custKey
count <- 0
for (key in clvs$custKey) { # clvs$custKey
  order.value <- 0
  discount <- 1
  cust.i <- resamp %>% filter(custKey == key)
  
  months.with.purchase <- cust.i$lifetime_seq
  
  for (t in months.with.purchase) {
    chance <- cust.i %>% ungroup() %>% filter(lifetime_seq == t) %>% select(yhat)
    discount <- discount * (1 + chance[[1]])
    gross <- cust.i %>% ungroup() %>% filter(lifetime_seq == t) %>% select(gross)
    order.value <- order.value + (gross$gross[[1]]/discount)
  }
  clvs.prac[clvs$custKey == key,]$clv <- order.value
  count <- count + 1
  if (count %% 10000 == 0) {
    print(count)
  }
}


hist(clvs[1:7920,]$clv, breaks=30)
