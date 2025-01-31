getwd()
setwd("C:/Users/sambi/OneDrive/Documents/Loyola Marymount University/Data Sets")

library(dplyr)

data = read.csv('group_project.csv')
dim(data)
str(data)
summary(data)

a = as.Date(data$Order.Date, format='%m/%d/%Y')
b = as.Date(data$Order.Date, format='%d-%m-%Y')

a[is.na(a)] = b[!is.na(b)]

data$Order.Date = a
summary(data$Order.Date)

data$days = weekdays(data$Order.Date)
table(data$days)
data$year = substring(data$Order.Date, 1, 4)
table(data$year)
data$month = substring(data$Order.Date, 6, 7)
table(data$month)
data$dates = substring(data$Order.Date, 9, 10)
table(data$dates)

# Customer Lifetime Values
analysis_date = max(data$Order.Date, na.rm=T)

data_rfm = data %>% 
  group_by(Customer.ID) %>% 
  summarize(
    Recency = as.numeric(difftime(analysis_date, max(Order.Date), units = "days")), 
    Frequency = n(), 
    Monetary = sum(Sales, na.rm=T)) %>%  
  ungroup()

data_rfm = data_rfm %>% mutate(Recency = ifelse(Recency == 0, 1, Recency))

data_rfm = data_rfm %>% 
  mutate(
    R_Score = (1/Recency) * 0.3, 
    F_Score  = Frequency * 0.3, 
    M_Score = Monetary * 0.4, 
    CLV_Score = R_Score + F_Score + M_Score)

data_rfm = data_rfm %>% arrange(desc(CLV_Score))
print(n=50, data_rfm)

data = data %>% left_join(data_rfm %>% select(Customer.ID, CLV_Score), by="Customer.ID")
str(data)

# Survival Analysis
today = Sys.Date()
cust_data = data %>% 
  group_by(Customer.ID) %>% 
  summarize(last_date = max(Order.Date), first_date = min(Order.Date))
cust_data = data.frame(cust_data)
cust_data$date_range = cust_data$last_date - cust_data$first_date
cust_data = cust_data %>% 
  mutate(survival_time = as.numeric(difftime(today, last_date, units = "days")))
summary(cust_data$survival_time)
cust_data = cust_data %>% mutate(event_status = ifelse(survival_time >= 3701, 1, 0))
table(cust_data$event_status)
data = data %>% select(-Region, -Ship.Date, -Postal.Code)

data = data %>% left_join(cust_data, by="Customer.ID")
write.csv(data, "C:/Users/sambi/OneDrive/Documents/Loyola Marymount University/Data Sets/superstore_updated_v4.csv", row.names=F)
