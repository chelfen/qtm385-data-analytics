library(tidyverse)
library(scales)
library(gmodels)
library(lubridate)
library(plotly)
library(ggplot2)
library(car)

setwd('/Users/clairefenton/Desktop/Emory')

df <- read_delim('vaccines_tnsactions.csv', delim="\t")

df %>% summarise(sum(doses*dose_price))

black <- c('CEFASIBAN', 'DILIAXOIIMOD', 'PERFLUSULFAN', 'SOMATOXMONAM')
red <- c('ARILATAPIDE', 'SULFAARELIN', 'VINIGESTDUTANT', 'GADOINABIMELINE')
green <- c('GADOAZOLINE', 'NIFURIARILATADINE', 'PREDACRINAT','NALIMORELIN', 'PREDSTATATRIPTAN')

#Month
df$month <-  format(df$date,"%m")
#Year
df$year <-  format(df$date,"%Y")
#Month-Year
df$month_yr <- floor_date(df$date, unit = "month")

colors <- c('Other' = 'black', 'Seasonal' = 'red', 'Consistent'='lightgreen')

df %>%
  mutate(cluster = ifelse(brand %in% black, 'Other', ifelse(brand %in% red, 'Seasonal', ifelse(brand %in% green, 'Consistent', NA)))) %>%
  filter(!is.na(cluster)) %>%
  group_by(customer_id, cluster, month_yr) %>%
  summarise(avg_doses = mean(doses)) %>%
  group_by(month_yr, cluster) %>%
  summarise(avg_avg_doses = mean(avg_doses)) %>%
  ggplot(aes(month_yr)) +   
  geom_line(aes(y = avg_avg_doses, color = cluster)) + 
  labs(x='Date (month-year)', y='Average doses purchased', title = 'Average Doses Purchased per Customer by Vaccine Cluster', color='Cluster') + 
  theme(plot.title = element_text(size = 24), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
  scale_color_manual(values = colors)


####
#Month
df$month <-  format(df$date,"%m")
#Year
df$year <-  format(df$date,"%Y")
#Month-Year
df$month_yr <- floor_date(df$date, unit = "month")
df$monetary <- df$doses*df$dose_price
#Create brand-month data
cust_month <- df %>%
  group_by(customer_id, month_yr) %>%
  summarize(doses = sum(doses),
            monetary = sum(monetary),
            no_brands = n_distinct(brand))  %>%
  filter(doses > 0) 

cust_sum <- cust_month %>% group_by(customer_id) %>% summarize(count = n()) %>% filter(count>1)
#Padding
cust_month <- cust_month %>%
  filter(customer_id %in% cust_sum$customer_id) %>%
  group_by(customer_id) %>%
  pad_by_time(month_yr, .pad_value = NA)

cust_month <- cust_month %>%
  group_by(customer_id) %>%
  arrange(month_yr) %>% mutate(recent_doses = lag(doses),
                               recent_monetary = lag(monetary)) %>% 
  mutate(recent_doses = ifelse(row_number() == 1, 0, na.locf(recent_doses)),
         recent_monetary = ifelse(row_number() == 1, 0, na.locf(recent_monetary))) %>%
  replace_na(list(doses = 0, monetary = 0, no_brands = 0))
####

table(df$doses)

sum(df$dose_price*df$doses) #total money

df %>%
  group_by(customer_id) %>%
  summarise(n=n()) %>%
  arrange(-n)


cust <- df %>%
  filter(customer_id == 
           "4764f0959d20f48994c134c9307d9ff3")

cust %>%
  group_by(brand) %>%
  summarize(n=n()) %>%
  arrange(-n)

cust <- cust %>%
  filter(brand %in% c("PREDACRINAT"))

cust %>%
  ggplot(aes(x=date, y=doses)) +
  geom_line()

dose <- cust %>%
  mutate(
    period = sprintf("%04d-%02d-%02d", year(date), month(date), 1)
  ) %>%
  group_by(period) %>%
  summarize(doses=sum(doses))

dose

# Running aggregate total over 3 months
dose %>%
  mutate(period = as.Date(period)) %>%
  ungroup() %>%
  mutate(
    doses2 = lag(doses) + doses,
    doses3 = lag(doses2) + doses
  ) %>%
  ggplot(aes(x=period, y=log(doses3))) +
  geom_line()

## Doing some state-level exploration

# States with most doses ordered
df %>%
  group_by(customer_state) %>%
  summarize(total_doses = sum(doses)) %>%
  arrange(-total_doses)

# Median doses
df %>%
  group_by(customer_state) %>%
  summarize(median_doses = median(doses)) %>%
  arrange(-median_doses) %>%
  print(n = 25)

# Average doses
df %>%
  group_by(customer_state) %>%
  summarize(mean_doses = mean(doses)) %>%
  arrange(-mean_doses) %>%
  print(n = 50) %>%
  ggplot(aes(x=mean_doses)) +
  geom_histogram()

# Where is the money?
df %>%
  mutate(money = doses*dose_price) %>%
  group_by(customer_state) %>%
  summarize(total_money = sum(money)) %>%
  arrange(-total_money) %>%
  print(n=15)

# Average order money
df %>%
  mutate(money = doses*dose_price) %>%
  group_by(customer_state) %>%
  summarize(mean_money = mean(money)) %>%
  arrange(-mean_money) %>%
  print(n=15)%>%
  ggplot(aes(x=mean_money)) +
  geom_histogram()

# Median order money
df %>%
  mutate(money = doses*dose_price) %>%
  group_by(customer_state) %>%
  summarize(median_money = median(money)) %>%
  arrange(-median_money) %>%
  print(n=15)

# Seeing the lag after each order for one state
df %>%
  filter(customer_state=='HI') %>%
  distinct(date) %>%
  mutate(lag =  as.numeric(difftime(date,lag(date), units = "days"))) %>%
  ggplot(aes(x=date, y=lag)) +
  geom_line()
 
# Mean lag 
df %>%
  group_by(customer_state) %>%
  distinct(date) %>%
  mutate(lag =  as.numeric(difftime(date,lag(date), units = "days"))) %>%
  filter(!is.na(lag)) %>%
  mutate(mean_lag = mean(lag)) %>%
  distinct(customer_state, mean_lag) %>%
  arrange(-mean_lag)

# Max lag 
df %>%
  group_by(customer_state) %>%
  distinct(date) %>%
  mutate(lag =  as.numeric(difftime(date,lag(date), units = "days"))) %>%
  filter(!is.na(lag)) %>%
  mutate(max_lag = max(lag)) %>%
  distinct(customer_state, max_lag) %>%
  arrange(-max_lag)
  
## Who's ordering?

# Number of customers
df %>%
  group_by(customer_state) %>%
  distinct(customer_id) %>%
  summarise(n = n()) %>%
  arrange(-n)

# Doses per customer
df %>%
  group_by(customer_state) %>%
  distinct(customer_id) %>%
  summarise(n = n()) %>%
  arrange(-n)%>%
  left_join(
    df %>%
      group_by(customer_state) %>%
      summarize(total_doses = sum(doses))
  ) %>%
  group_by(customer_state) %>%
  summarise(doses_per_cust = total_doses/n) %>%
  arrange(-doses_per_cust) %>%
  print(n=20) %>%
  ggplot(aes(x=doses_per_cust)) +
  geom_histogram()

# Money per customer
df %>%
  group_by(customer_state) %>%
  distinct(customer_id) %>%
  summarise(n = n()) %>%
  arrange(-n)%>%
  left_join(
    df %>%
      mutate(money = doses*dose_price) %>%
      group_by(customer_state) %>%
      summarize(total_money = sum(money))
  ) %>%
  group_by(customer_state) %>%
  summarise(money_per_cust = total_money/n) %>%
  arrange(-money_per_cust) %>%
  print(n=20) %>%
  ggplot(aes(x=money_per_cust)) +
  geom_histogram()

## What about specific vaccines?

# Just looking at predacrinat
cust <- df %>%
  filter(brand %in% c("PREDACRINAT"))

cust %>%
  mutate(
    period = sprintf("%04d-%02d-%02d", year(date), month(date), 1)
  ) %>%
  group_by(period) %>%
  summarize(doses=sum(doses)) %>%
  mutate(period = as.Date(period)) %>%
  ggplot(aes(x=period, y = doses)) +
  geom_line()

# Looking at all vaccines over time
df %>%
  mutate(
    period = sprintf("%04d-%02d-%02d", year(date), month(date), 1)
  ) %>%
  group_by(period, brand) %>%
  summarize(doses=sum(doses)) %>%
  mutate(period = as.Date(period)) %>%
  ggplot(aes(x=period, y = doses, color = brand)) +
  geom_line()

# Removing arilatapide to better look at the others
df %>%
  filter(brand != 'ARILATAPIDE') %>%
  mutate(
    period = sprintf("%04d-%02d-%02d", year(date), month(date), 1)
  ) %>%
  group_by(period, brand) %>%
  summarize(doses=sum(doses)) %>%
  mutate(period = as.Date(period)) %>%
  ggplot(aes(x=period, y = doses, color = brand)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 90))

###try to do aggregate by month


### Looking specifically at lag for segmentation

# Mean lag 


# Just large group
df %>%
  filter(customer_type=='LARGE GROUP') %>%
  group_by(customer_id) %>%
  distinct(date, customer_type) %>%
  mutate(lag =  as.numeric(difftime(date,lag(date), units = "days"))) %>%
  filter(!is.na(lag)) %>%
  mutate(mean_lag = mean(lag)) %>%
  distinct(customer_id, mean_lag, customer_type) %>%
  arrange(-mean_lag) %>%
  ggplot(aes(x=mean_lag)) +
  geom_histogram() +
  geom_vline(xintercept = 365) +
  geom_vline(xintercept = 182) +
  geom_vline(xintercept = 91) +
  geom_vline(xintercept = 30)
# Just physician (not appreciably different)
df %>%
  filter(customer_type!='LARGE GROUP') %>%
  group_by(customer_id) %>%
  distinct(date, customer_type) %>%
  mutate(lag =  as.numeric(difftime(date,lag(date), units = "days"))) %>%
  filter(!is.na(lag)) %>%
  mutate(mean_lag = mean(lag)) %>%
  distinct(customer_id, mean_lag, customer_type) %>%
  arrange(-mean_lag) %>%
  ggplot(aes(x=mean_lag)) +
  geom_histogram() +
  geom_vline(xintercept = 365) +
  geom_vline(xintercept = 182) +
  geom_vline(xintercept = 91) +
  geom_vline(xintercept = 30)

# Orders per month
df %>%
  mutate(
    period = sprintf("%04d-%02d-%02d", year(date), month(date), 1)
  ) %>%
  group_by(customer_id, period) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(customer_id) %>%
  summarise(mean_orders = mean(n)) %>%
  ggplot(aes(x=mean_orders)) +
  geom_histogram()

# Orders per month <= 10 (for zooming in b/c of skew)
df %>%
  mutate(
    period = sprintf("%04d-%02d-%02d", year(date), month(date), 1)
  ) %>%
  group_by(customer_id, period) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(customer_id) %>%
  summarise(mean_orders = mean(n)) %>%
  filter(mean_orders <= 10) %>%
  ggplot(aes(x=mean_orders)) +
  geom_histogram()

table(df$brand)

## Looking at brands over time with proper date breaks
df %>%
  filter(brand != 'ARILATAPIDE') %>%
  mutate(
    period = sprintf("%04d-%02d-%02d", year(date), month(date), 1)
  ) %>%
  group_by(period, brand) %>%
  summarize(doses=sum(doses)) %>%
  mutate(period = as.Date(period)) %>%
  ggplot(aes(x=period, y = doses, color = brand)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 90))

## Prices don't change over time
df %>%
  group_by(brand) %>%
  distinct(dose_price) %>%
  arrange(-dose_price)


## Segementing vaccines by price
expensive_brands <- c('DILBOLIPARCIL', 'PREDACRINAT', 'VINIGESTDUTANT', 'NIFURMANTADINE', 'NALIMORELIN')
cheap_brands <- c('DILIAXOIIMOD', 'PERFLUSULFAN','ARILATAPIDE', 'GADOINABIMELINE', 'BOLCOICAMSULE', 'SULFAARELIN', 'NIFURIARILATADINE')
average_brands <- c('CEFASIBAN', 'PREDSTATATRIPTAN', 'GADOAZOLINE', 'SOMATOXMONAM')

data_frame <- transform( 
  df, price_level= ifelse(brand %in% expensive_brands, 'expensive', ifelse(brand %in% cheap_brands, 'cheap', 'average')))

data_frame

data_frame %>%
  mutate(
    period = sprintf("%04d-%02d-%02d", year(date), month(date), 1)
  ) %>%
  group_by(period, price_level) %>%
  summarize(doses=sum(doses)) %>%
  mutate(period = as.Date(period)) %>%
  ggplot(aes(x=period, y = doses, color = price_level)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 90))


## State sales over time
df %>%
  mutate(
    period = sprintf("%04d-%02d-%02d", year(date), month(date), 1)
  ) %>%
  group_by(period, customer_state) %>%
  summarize(doses=sum(doses)) %>%
  mutate(period = as.Date(period)) %>%
  filter(customer_state %in% c('WY', 'OK', 'AZ', 'SD')) %>%
  ggplot(aes(x=period, y = doses, color = customer_state)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 90))


## Create regions using built-in States dataframe
regions <- data.frame(rownames(States), States$region)
names(regions) <- c('state', 'region')
regions[regions == 'CN'] <- 'CT'

vacc_states <- sort(unique(df$customer_state))
states <- rownames(States)

merged_df <- merge(df,
      regions,
      by.x = "customer_state",
      by.y = "state",
      all.x = T)

merged_df %>%
  mutate(
    period = sprintf("%04d-%02d-%02d", year(date), month(date), 1)
  ) %>%
  group_by(period, region) %>%
  summarize(doses=sum(doses)) %>%
  mutate(period = as.Date(period)) %>%
  ggplot(aes(x=period, y = doses, color = region)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 90))

## Aggregating completely over time, kind of a flop
df %>%
  filter(brand != 'ARILATAPIDE') %>%
  mutate(
    period = as.character.Date(sprintf("%04d-%02d-%02d", year(date), month(date), 1))
  ) %>%
  group_by(period, brand) %>%
  summarize(doses=sum(doses)) %>%
  mutate(period = as.Date(period)) %>%
  ggplot(aes(x=period, y = cumsum(doses), color = brand)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 90))

## Aggregating 
`%nin%` <- Negate(`%in%`)
dose <- df %>%
  group_by(brand) %>%
  mutate(
    period = sprintf("%04d-%02d-%02d", year(date), month(date), 1)
  ) %>%
  group_by(period, brand) %>%
  summarize(doses=sum(doses))

# Running aggregate total over 3 months (price level)
data_frame <- transform( 
  df, price_level= ifelse(brand %in% expensive_brands, 'expensive', ifelse(brand %in% cheap_brands, 'cheap', 'average'))) %>%
  mutate(period = sprintf("%04d-%02d-%02d", year(date), month(date), 1))

data_frame %>%
  mutate(period = as.Date(period)) %>%
  mutate(
    doses2 = lag(doses) + doses,
    doses3 = lag(doses2) + doses
  ) %>%
  group_by(price_level, period) %>%
  summarise(total = sum(doses3)) %>%
  ggplot(aes(x=period, y=total, color=price_level)) +
  geom_line()

# Aggregating 3 month period number of customers
merged_df %>%
  mutate(period = sprintf("%04d-%02d-%02d", year(date), month(date), 1)) %>%
  mutate(period = as.Date(period)) %>%
  group_by(period, region) %>%
  distinct(customer_id) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=period, y=n, color=region)) +
  geom_line()

merged_df %>%
  mutate(period = sprintf("%04d-%02d-%02d", year(date), month(date), 1)) %>%
  mutate(period = as.Date(period)) %>%
  group_by(period, region) %>%
  summarise(money = sum(dose_price * doses)) %>%
  ggplot(aes(x=period, y=money, color=region)) +
  geom_line()


