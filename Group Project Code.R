# Questions
# 1. Which product sub-categories generate the highest sales overall
# 2. What is the average shipping costs for orders shipped via different shipping modes (Standard Class, Second Class, etc.)
# 3. Which countries have the highest sales volume per sub-category
# 4. Which customer segments show the highest lifetime value, and how does their purchasing behavior differ per sub-category
# 5. What is the relationship between customer loyalty (repeat purchases) and avg order profitability grouping by category
# 6. How do shipping costs impact overall profitability and which regions and segments are most affected by high shipping cost?
# 7. Which segments are more sensitive to discounts and how does their sensitivity affect profitability?
# 8. How does the time between a customer's first and last purchase correlate with their overall profitability and order frequency?
# 9. What is the correlation between order priority (High, Medium, Low) and long-term CLV? Does prioritizing certain orders lead to higher retention/value?
# 10. Are there opportunities to cross-sell sub categories with others that offer high profitability to customers with high CLV?
# 11. Is there a statistically significant association between shipping mode and order priority?
# 12. Is there a significant difference in average sales between different product categories?
# 13. What is the survival time of products in terms of reorder frequency, and how does it vary by product category?


# Honorable Mention: Identify high CLV based on order freq, avg order value, and purchasing preferences? What distinguishes lower-value from higher?

library(lubridate)
library(knitr)
library(moments)
library(beanplot)
library(polycor)
library(markovchain)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(survminer)
library(survival)
library(survMisc)
library(corrplot)
library(multcomp)
library(car)
library(pseudo)
library(geepack)
library(ROCR)
library(arules)
library(arulesViz)
library(coin)

getwd()
setwd("C:/Users/sambi/OneDrive/Documents/Loyola Marymount University/Data Sets")
data = read.csv("superstore_updated_v4.csv")
str(data)
survival_col = with(data, Surv(time=date_range/30, event=event_status))
data$survival = survival_col
str(data)
options(digits=4)


# # Problem 3
# country_sales = data %>% group_by(Country, Sub.Category) %>% summarize(Total_Sales = sum(Sales, na.rm=T)) %>% ungroup() %>% arrange(desc(Total_Sales))
# top_country_sales = country_sales %>% group_by(Sub.Category) %>% top_n(2, Total_Sales)
# ggplot(top_country_sales, aes(x=reorder(Sub.Category, -Total_Sales), y=Total_Sales, fill=Country)) + geom_bar(stat="identity", position='dodge') + theme_minimal() + labs(title="Highest Sales Volume by Country per Sub-Category", x="Sub-Category", y="Total Sales", fill="Country") + theme(axis.text.x = element_text(angle=45, hjust=1))
# 
# 
# #Problem 5
# customer_loyalty = data %>% group_by(Customer.ID, Category) %>% summarize(Repeat_Purchases = n(), Average_Profit=mean(Profit, na.rm=T)) %>% ungroup()
# loyalty_by_category = customer_loyalty %>% group_by(Category) %>% summarize(Avg_Repeat_Purchases = mean(Repeat_Purchases, na.rm=T), Avg_Order_Profit = mean(Average_Profit, na.rm=T))
# ggplot(loyalty_by_category, aes(x=Avg_Repeat_Purchases, y=Avg_Order_Profit, color=Category)) + geom_point(size=4) + geom_smooth(method=lm, se=F, linetype="dashed", color="gray") + theme_minimal() + labs(title="Relationships of Customer Loyalty and Average Order Profitability", x="Average Repeat Purchases", y="Average Order Profit", color="Category") + theme(plot.title=element_text(hjust=0.5), axis.text.x=element_text(angle=45, hjust=1))
# 
# 
# # Problem 6  
# shipping_impact = data %>% group_by(Market, Segment) %>% summarize(Total_Profit = sum(Profit, na.rm=T), Total_Shipping_Cost=sum(Shipping.Cost, na.rm=T), Avg_Profit_Per_Order = mean(Profit, na.rm=T), Avg_Shipping_Cost_Per_Order = mean(Shipping.Cost, na.rm=T)) %>% mutate(Shipping_Profit_Ratio = Total_Shipping_Cost / (Total_Profit+1)) %>% ungroup()
# ggplot(shipping_impact, aes(x=reorder(Market, -Shipping_Profit_Ratio), y=Shipping_Profit_Ratio, fill=Segment)) + geom_bar(stat="identity", position="dodge", width=0.7) + theme_minimal() + labs(title = "Shipping Cost to Profit Ratio by Region and Segment", x = "Region", y = "Shipping Cost to Profit Ratio", fill = "Segment") + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
# 
# 
# #Problem 7
# discount_impact = data %>% group_by(Segment) %>% summarize(Avg_Discount=mean(Discount, na.rm=T), Total_Profit=sum(Profit, na.rm=T), Avg_Profit_Per_Order=mean(Profit, na.rm=T)) %>% mutate(Profit_Discount_Rate=Total_Profit/(Avg_Discount+0.01))
# ggplot(discount_impact, aes(x=Avg_Discount, y=Avg_Profit_Per_Order, color=Segment)) + geom_point(size=4) + geom_smooth(method="lm", se=F, linetype="dashed", color='gray') + theme_minimal() + labs(title="Discount Sensitivity and Profitability by Segment", x="Average Discount", y="Average Profit per Order", color="Segment") + theme(plot.title=element_text(hjust=0.5), axis.text.x = element_text(angle=45, hjust=1))
# ggplot(discount_impact, aes(x=reorder(Segment, -Profit_Discount_Rate), y=Profit_Discount_Rate, fill=Segment)) + geom_bar(stat="identity", show.legend=F, width=0.6) + geom_text(aes(label=round(Profit_Discount_Rate, 2)), vjust=-0.5, size=4) + theme_minimal() + labs(title="Profit to Discount Ratio by Segment", x="Segment", y="Profit to Discount Rate") + theme(axis.text.x=element_text(angle=45, hjust=1), plot.title=element_text(hjust=0.5))
# 
# 
# # Problem 8
# variable.names(data)
# customer_analysis = data %>% group_by(Customer.ID) %>% summarize(First_Purchase = min(Order.Date, na.rm=T), Last_Purchase=max(Order.Date, na.rm=T), Order_Frequency=n(), Total_Profit=sum(Profit, na.rm=T), date_range=date_range)
# variable.names(customer_analysis)
# ggplot(customer_analysis, aes(x=date_range, y=Total_Profit, color=Order_Frequency))+ geom_point(alpha=0.7) + geom_smooth(method="lm", se=F, linetype="dashed", color='yellow') + scale_size_continuous(range=c(2,10)) + theme_minimal() + labs(title="Correlation of Purchase Time, Profit, and Order Frequency", x="Days Between First/Last Purchase", y="Total Profit", color="Order Frequency") + theme(plot.title=element_text(hjust=0.3)) + scale_color_gradient(low="lightblue", high="darkblue")
# 
# 
# # Problem 9
# clv_analysis = data %>% group_by(Customer.ID) %>% summarize(Total_CLV = sum(CLV_Score, na.rm=T), Avg_Profit = mean(Profit, na.rm=T), Critical_Priority_Orders = sum(Order.Priority == "Critical", na.rm=T), High_Priority_Orders = sum(Order.Priority=="High", na.rm=T), Medium_Priority_Orders = sum(Order.Priority=="Medium", na.rm=T), Low_Priority_Orders = sum(Order.Priority=="Low", na.rm=T), Total_Orders = n()) %>% mutate(Critical_Priority_Ratio=Critical_Priority_Orders/Total_Orders, High_Priority_Ratio = High_Priority_Orders/Total_Orders, Medium_Priority_Ratio = Medium_Priority_Orders/Total_Orders, Low_Priority_Ratio=Low_Priority_Orders/Total_Orders)
# ggplot(clv_analysis, aes(x=High_Priority_Ratio, y=Total_CLV, color=Avg_Profit)) + geom_point(alpha=0.8) + geom_smooth(method='lm', se=F, linetype="dashed", color='blue') + scale_color_gradient(low="lightgreen", high='darkgreen') + scale_size_continuous(range=c(2, 10)) + theme_minimal() + labs(title="Correlation Between Order Priority and CLV", x="High Priority Order Ratio", y="Total CLV", color="Average Profit") + theme(plot.title=element_text(hjust=0.5), axis.text.x=element_text(angle=45, hjust=1))
# ggplot(clv_analysis, aes(x=Medium_Priority_Ratio, y=Total_CLV, color=Avg_Profit)) + geom_point(alpha=0.8) + geom_smooth(method='lm', se=F, linetype="dashed", color='red') + scale_color_gradient(low="yellow", high='darkorange') + scale_size_continuous(range=c(2, 10)) + theme_minimal() + labs(title="Correlation Between Order Priority and CLV", x="Medium Priority Order Ratio", y="Total CLV", color="Average Profit") + theme(plot.title=element_text(hjust=0.5), axis.text.x=element_text(angle=45, hjust=1))
# ggplot(clv_analysis, aes(x=Low_Priority_Ratio, y=Total_CLV, color=Avg_Profit)) + geom_point(alpha=0.8) + geom_smooth(method='lm', se=F, linetype="dashed", color='green') + scale_color_gradient(low="pink", high='darkred') + scale_size_continuous(range=c(2, 10)) + theme_minimal() + labs(title="Correlation Between Order Priority and CLV", x="Low Priority Order Ratio", y="Total CLV", color="Average Profit") + theme(plot.title=element_text(hjust=0.5), axis.text.x=element_text(angle=45, hjust=1))
# ggplot(clv_analysis, aes(x=Critical_Priority_Ratio, y=Total_CLV, color=Avg_Profit)) + geom_point(alpha=0.6) + geom_smooth(method='lm', se=F, linetype="dashed", color='yellow') + scale_color_gradient(low="lightblue", high='darkblue') + scale_size_continuous(range=c(2, 10)) + theme_minimal() + labs(title="Correlation Between Order Priority and CLV", x="Critical Priority Order Ratio", y="Total CLV", color="Average Profit") + theme(plot.title=element_text(hjust=0.5), axis.text.x=element_text(angle=45, hjust=1))
# 
# 
# # Problem 10
# customer_subcat_analysis = data %>% group_by(Customer.ID, Sub.Category) %>% summarize(CLV=sum(CLV_Score, na.rm=T), Total_Profit=sum(Profit, na.rm=T), Order_Count=n()) %>% ungroup()
# clv_by_customer = customer_subcat_analysis %>% group_by(Customer.ID) %>% summarize(Total_CLV=sum(CLV, na.rm=T), Avg_Profit_Per_Order=mean(Total_Profit, na.rm=T))
# clv_subcat_combined = merge(customer_subcat_analysis, clv_by_customer, by="Customer.ID")
# cross_sell_opportunities = clv_subcat_combined %>% filter(Total_CLV>median(Total_CLV)) %>% group_by(Sub.Category) %>% summarize(Avg_Profit = mean(Total_Profit, na.rm=T), Overall_CLV=sum(Total_CLV, na.rm=T), Order_Count=sum(Order_Count, na.rm=T)) %>% arrange(desc(Avg_Profit))
# cross_sell_opportunities = cross_sell_opportunities %>% arrange(desc(Overall_CLV)) %>% mutate(is_top_3_clv = row_number() <= 3)
# ggplot(cross_sell_opportunities, aes(x = reorder(Sub.Category, -Avg_Profit), y = Avg_Profit, fill = Overall_CLV)) + geom_bar(stat = "identity") + scale_fill_gradient(low = "lightblue", high = "darkblue") + theme_minimal() + labs(title = "Cross-Sell Opportunities by Sub-Category for High CLV Customers", x = "Sub-Category", y = "Average Profit", fill = "Comprehensive CLV") + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
# ggplot(cross_sell_opportunities, aes(x = reorder(Sub.Category, -Avg_Profit), y = Avg_Profit, fill = is_top_3_clv)) + geom_bar(stat = "identity") + scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "red")) + theme_minimal() + labs(title = "Cross-Sell Opportunities by Sub-Category for High CLV Customers", x = "Sub-Category", y = "Average Profit", fill = "Top 3 CLV") + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
# 
# 
# # Problem 11
# table_ship_order = table(data$Ship.Mode, data$Order.Priority)
# chi_sq_test = chisq.test(table_ship_order)
# print(chi_sq_test) # Ship Mode and Order Priority do have a statistically significant relationship
# 
# 
# # Problem 12
# anova_model = aov(Sales~Category, data=data)
# anova_summary = summary(anova_model)
# print(anova_summary) # Sales and Category do have a statistically significant relationship
# 
# 
# # Problem 13
# data_subset = data %>% filter(!is.na(survival_time))
# km_fit = survfit(Surv(survival_time, event_status)~Category, data=data_subset, na.action=na.exclude)
# ggsurvplot(km_fit, data=data_subset)
# survdiff(survival~Category, data=data_subset)
# survdiff(survival~Market, data=data_subset)
# market_fit = survfit(Surv(survival_time, event_status)~Market, data=data_subset, na.action=na.exclude)
# ggsurvplot(market_fit, data=data_subset)
# 
# 
# # Problem 14
# survdiff(survival~CLV_Group, data=clv_data) #Significant
# survdiff(survival~Ship.Mode, data=clv_data) #Not Significant
# survdiff(survival~Sub.Category, data=clv_data) #Significant
# survdiff(survival~Segment, data=clv_data) #Significant
# survdiff(survival~Country, data=clv_data) #Significant
# survdiff(survival~Quantity, data=clv_data) # Significant
# survdiff(survival~Sales, data=clv_data) # Significant
# survdiff(survival~Profit, data=clv_data) # Significant
# survdiff(survival~Discount, data=clv_data) # Significant
# survdiff(survival~Order.Priority, data=clv_data) #Not Significant
# summary(aov(CLV_Score~Order.Priority, data=clv_data))
# survdiff(survival~Segment+Category+Sub.Category+Market+CLV_Group, data=clv_data) #Significant
# 
# 
# # Customer Segmentation and Profitability Analysis - Analyze CLV by Segment
# segment_clv = data %>% group_by(Segment, Customer.ID) %>% summarize(Lifetime_Value = sum(CLV_Score)) %>% group_by(Segment) %>% summarize(Average_CLV = mean(Lifetime_Value)) %>% arrange(desc(Average_CLV))
# ggplot(segment_clv, aes(x=reorder(Segment, -Average_CLV), y= Average_CLV, fill=Segment)) + geom_bar(stat="identity", show.legend = T, width=0.6) + geom_text(aes(label=round(Average_CLV, 2)), vjust=-0.5, size=4) + labs(title="Average CLV by Customer Segment", x="Customer Segment", y="Average CLV") + theme(axis.text.x = element_text(angle=45, hjust=1))
# anova1 = aov(CLV_Score~Segment, data)
# print(summary(anova1)) #Statistically significant
# 
# 
# # Impact of Shipping Costs and Discounts on Profitability - Ship Cost/Profit by Market/Segment
# avg_ship_cost = data %>% group_by(Ship.Mode) %>% summarize(Average_Shipping_Cost = mean(Shipping.Cost))
# ggplot(avg_ship_cost, aes(x=reorder(Ship.Mode, -Average_Shipping_Cost), y=Average_Shipping_Cost)) + geom_bar(stat="identity", fill="skyblue", width=0.6) + geom_text(aes(label=round(Average_Shipping_Cost, 2)), vjust=-0.5, color="black") + labs(title="Average Shipping Cost by Shipping Mode", x="Shipping Mode", y="Average Cost") + theme(axis.text.x = element_text(angle=45, hjust=1))
# 
# 
# # Product Profitability by Category/Sub-Category - Sales analysis by product sub category
# sub_cat_sales = data %>% group_by(Category, Sub.Category) %>% summarize(Total_Sales = sum(Sales)) %>% arrange(desc(Total_Sales))
# sub_cat_profit = data %>% group_by(Category, Sub.Category) %>% summarize(Total_Profit = sum(Profit)) %>% arrange(desc(Total_Profit))
# ggplot(sub_cat_sales, aes(x=reorder(Sub.Category, -Total_Sales), y=Total_Sales, fill=Category)) + geom_bar(stat="identity") + labs(title="Sales by Product Sub Category", x="Sub Category", y="Total Sales") + theme(axis.text.x = element_text(angle=45, hjust=1))
# ggplot(sub_cat_profit, aes(x=reorder(Sub.Category, -Total_Profit), y=Total_Profit, fill=Category)) + geom_bar(stat="identity") + labs(title="Product Sub Category Profitability", x="Sub Category", y="Total Profit") + theme(axis.text.x=element_text(angle=45, hjust=1))
# 
# anova_sales = aov(Sales~Sub.Category, data)
# summary(anova_sales)
# anova_profit = aov(Profit~Sub.Category, data)
# summary(anova_profit) # Both are statistically significant
# 
# 
# Survival Analysis for Retention by Product Category
km_fit = survfit(survival~Category, data, na.action = na.exclude)
ggsurvplot(km_fit, data)
anova3 = aov(date_range/30~Category, data)
summary(anova3) # This is statistically significant




# # Market Basket Analysis
# data_matrix = data[, -1]
# summary(data_matrix)
# nrow(unique(data_matrix))
# length(unique(data_matrix$Order.ID))
# length(unique(data_matrix$Sub.Category))
# data_matrix_mini = data_matrix[, c("Order.ID", "Sub.Category")]
# write.csv(data_matrix_mini, "transactiondata", row.names = F)
# 
# transaction_data = read.transactions(file="transactiondata", format="single", sep=",", cols=c("Order.ID", "Sub.Category"), rm.duplicates=T, header=T)
# data_transactions = as(transaction_data, "transactions")
# summary(data_transactions)
# itemFrequencyPlot(data_transactions, topN=10, type="absolute")
# 
# data_rules = apriori(data_transactions, parameter=list(support=0.005, conf=0.2, target="rules"))
# summary(data_rules)
# 
# top_lift = head(data_rules, n=20, by="lift")
# inspect(top_lift)
# top_confidence = head(data_rules, n=20, by="confidence")
# inspect(top_confidence)
# 
# sorted_subset_rules = sort(subset(data_rules, lift > 1.3 & confidence > 0.24), decreasing=F)
# high_group = head(data_rules, by="lift", 10)
# plot(high_group, method="graph", control=list(type="items"), engine="igraph", vertex_color="lightblue", edge_color="darkred", vertex.label.color="black", alpha=0.75)
# inspect(high_group)







#Story Block 1 - Segment Analysis of Revenue, Average CLV, and other important metrics
variable.names(data)

segment_summary = data %>% group_by(Segment) %>% summarize(Total_Revenue = sum(Sales), Avg_Order_Value = mean(Sales), Order_Count = n_distinct(Order.ID), Customer_Count = n_distinct(Customer.ID), Total_Profit = sum(Profit), Profit_Margin = (Total_Profit / Total_Revenue)*100, Avg_CLV = mean(CLV_Score)) %>% arrange(desc(Total_Revenue))
segment_summary_long = tibble(Segment = rep(segment_summary$Segment, times=2), Metric = rep(c("Avg_Order_Value", "Profit_Margin"), each=nrow(segment_summary)), Value = c(segment_summary$Avg_Order_Value, segment_summary$Profit_Margin))


# Plot each Segment's revenue
ggplot(segment_summary, aes(x=reorder(Segment, Total_Revenue), y=Total_Revenue, fill=Segment)) + geom_bar(stat="identity") + geom_text(aes(label=scales::dollar(Total_Revenue)), vjust=-0.5) + theme(axis.text.x = element_text(angle=45, hjust=1)) + labs(title="Revenue by Customer Segment", x="Segment", y="Total Revenue") + scale_y_continuous(labels=scales::dollar_format())

revenue_anova = aov(Sales~Segment, data)
summary(revenue_anova)


# Plot each Segment's Average CLV
patterns = data %>% group_by(Customer.ID, Segment) %>% summarize(Purchase_Frequency = n(), Avg_Order_Value = mean(Sales), Total_Spent = sum(Sales), Last_Purchase = max(Order.Date), First_Purchase = min(Order.Date), AVG_CLV = mean(CLV_Score), Total_CLV=sum(CLV_Score))
view(patterns)
ggplot(patterns, aes(x=Segment, y=AVG_CLV, fill=Segment)) + geom_boxplot() + labs(title="Average Segment CLV - Boxplot", x="Segment", y="Customer Lifetime Value") + theme(axis.text.x=element_text(angle=45, hjust=1)) + scale_y_continuous(labels=scales::dollar_format())

aggregated_data = data %>% group_by(Segment) %>% summarize(CLV_Average = mean(CLV_Score), CLV_Sum = sum(CLV_Score), Sales_Sum = sum(Sales))
ggplot(aggregated_data, aes(x=reorder(Segment, CLV_Average), y=CLV_Average, fill=Segment)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=scales::dollar(CLV_Average)), vjust=-0.5, size=3.5) +  
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(
    title="Average Segment CLV",
    x="Segment",
    y="Customer Lifetime Value"
  ) +
  scale_y_continuous(labels=scales::dollar_format())

ggplot(aggregated_data, aes(x=reorder(Segment, CLV_Sum), y=CLV_Sum, fill=Segment)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=scales::dollar(CLV_Sum)), vjust=-0.5)+
  labs(title="Total Segment CLV", x="Segment", y="Customer Lifetime Value") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  scale_y_continuous(labels=scales::dollar_format())

clv_anova = aov(CLV_Score~Segment, data)
summary(clv_anova)

aggregated_data = aggregated_data %>% mutate(Segment = as.factor(Segment))
sum_clv_permutation_test = oneway_test(CLV_Sum~Segment, aggregated_data, distribution="approximate")
sum_clv_permutation_test

sum_sales_permuation_test = oneway_test(Sales_Sum~Segment, aggregated_data, distribution="approximate")
sum_sales_permuation_test

# Plot each Segment's metrics
ggplot(segment_summary_long, aes(x=Segment, y=Value, fill=Metric)) + geom_bar(stat="identity", position="dodge") + theme(axis.text.x = element_text(angle=45, hjust=1)) + labs(title="Key Metrics by Segment", x="Segment", y="Value") + facet_wrap(~Metric, scales="free_y")

# Analyze each Segment's CLV metrics
clv_threshold = quantile(data$CLV_Score, 0.75)
clv_analysis = data %>% 
  group_by(Segment) %>% 
  summarize(
    Avg_CLV = mean(CLV_Score), 
    Median_CLV = median(CLV_Score), 
    CLV_StdDev = sd(CLV_Score), 
    CLV_Skewness = skewness(CLV_Score), 
    High_Value_Customers = sum(CLV_Score > clv_threshold), 
    Retention_Rate = 
      n_distinct(Customer.ID[year==max(year)]) / 
      n_distinct(Customer.ID[year == min(year)])) %>% 
  arrange(desc(Avg_CLV))
view(clv_analysis)





# Story Block 2 - Customer Survival Analysis
survival_fit = survfit(survival~Segment, data=data)
ggsurvplot(survival_fit, data=data, risk.table=T, pval=T, conf.int=T, xlab="Time", ylab="Survival Probability", title="Customer Survival by Segment")

cox_model = coxph(survival~Segment, data=data)
summary(cox_model)

logrank_test = survdiff(survival~Segment, data=data)
logrank_test

survival_metrics = data %>% group_by(Segment) %>% summarize(Mean_Survival_Time = mean(date_range/30), Median_Survival_Time = median(date_range/30), Total_Customers = n_distinct(Customer.ID), Churned_Customers = sum(event_status==0), Retention_Rate = 1 - (sum(event_status == 0) / n_distinct(Customer.ID)), Avg_Sales = mean(Sales), Avg_Quantity = mean(Quantity), Avg_Discount = mean(Discount)) %>% arrange(desc(Retention_Rate))
view(survival_metrics)

predictor_data = data %>% mutate(Churn_Status = factor(ifelse(survival_time >= 3701, "Retained", "Churned"), levels=c("Retained", "Churned")))
# Sales Plot
ggplot(predictor_data, aes(x=Churn_Status, y=Sales, fill=Churn_Status)) + 
  geom_boxplot(outlier.shape = NA) + 
  facet_wrap(~Segment, scales="free_y") + 
  labs(title="Sales Impact on Customer Survival", x="Survival Status", y="Total Sales") + 
  scale_fill_manual(values=c("Retained" = "green", "Churned" = "red")) +
  coord_cartesian(ylim=c(0, quantile(predictor_data$Sales, 0.90, na.rm=T)))
# Quantity Plot
ggplot(predictor_data, aes(x = Churn_Status, y = Quantity, fill = Churn_Status)) + geom_boxplot() + facet_wrap(~Segment, scales = "free_y") + labs(title = "Quantity Impact on Customer Survival", x = "Survival Status", y = "Total Quantity") + scale_fill_manual(values = c("Retained" = "green", "Churned" = "red"))
# Discount Plot
ggplot(predictor_data, aes(x = Churn_Status, y = Discount, fill = Churn_Status)) + geom_boxplot() + facet_wrap(~Segment, scales = "free_y") + labs(title = "Discount Impact on Customer Survival", x = "Survival Status", y = "Discount Rate") + scale_fill_manual(values = c("Retained" = "green", "Churned" = "red"))

test_results = predictor_data %>% group_by(Segment) %>% summarize(Sales_p_value = wilcox.test(Sales[Churn_Status == "Retained"], Sales[Churn_Status == "Churned"])$p.value, Quantity_p_value = wilcox.test(Quantity[Churn_Status == "Retained"], Quantity[Churn_Status == "Churned"])$p.value, Discount_p_value = wilcox.test(Discount[Churn_Status == "Retained"], Discount[Churn_Status == "Churned"])$p.value)
test_results

summary_stats = predictor_data %>% group_by(Segment, Churn_Status) %>% summarize(Mean_Sales = mean(Sales), Median_Sales = median(Sales), Mean_Quantity = mean(Quantity), Median_Quantity = median(Quantity), Mean_Discount = mean(Discount), Median_Discount = median(Discount), Count = n()) %>% ungroup()
# print(summary_stats, n=Inf)
view(summary_stats)





# Story Block 3 - Market Basket Analysis
basket_data = data %>% group_by(Order.ID) %>% summarize(Items = list(unique(Sub.Category)))
transactions = as(basket_data$Items, "transactions")
summary(transactions)

rules = apriori(transactions, parameter=list(supp=0.005, conf=0.25))
summary(rules)
# Visualize the rules
plot(rules, method="graph", control=list(type="items"), engine="igraph", vertex_color="lightblue", edge_color="darkred", vertex.label.color="black", alpha=0.6)

sorted_rules = head(rules, n=10, by="lift")
inspect(sorted_rules)
plot(sorted_rules, method="graph", control=list(type="items"), engine="igraph", vertex_color="lightblue", edge_color="darkred", vertex.label.color="black", alpha=0.75)





# Story Block 4 - Predictive Modeling for CLV/Churn
customer_data = data %>% group_by(Customer.ID)
CLV_linear_model = lm(CLV_Score~Segment+Country+Market+Category+Sub.Category+Ship.Mode+Order.Priority+Quantity+Profit+survival, data=customer_data)
summary(CLV_linear_model)
outlierTest(CLV_linear_model)
no_outlier_customer_data = data[-c(20179), ] %>% group_by(Customer.ID)
CLV_linear_model = lm(CLV_Score~Segment+Country+Market+Category+Sub.Category+Ship.Mode+Order.Priority+Quantity+Profit+survival_time, data=no_outlier_customer_data)
summary(CLV_linear_model)
# Remove MarketEU, MarketLATAM, MarketCanada, MarketUS, Sub.CategorySupplies, Sub.CategoryTables
no_outlier_no_colinearity_customer_data = no_outlier_customer_data %>% filter(!(Market %in% c("EU", "LATAM", "Canada", "US") | Sub.Category %in% c("Supplies", "Tables")))
start = lm(CLV_Score~1, no_outlier_no_colinearity_customer_data)
clv_aic = step(start, scope=CLV_Score~Segment+Country+Market+Category+Sub.Category+Ship.Mode+Order.Priority+Sales+Discount+Quantity+Profit+Shipping.Cost+Order.Priority+date_range, direction="both", rm.na=T)

# Order Size = Quantity / Order Number
best_CLV_model = lm(CLV_Score ~ Market+Quantity+date_range+Profit+Segment+Order.Priority+Ship.Mode, no_outlier_no_colinearity_customer_data)
summary(best_CLV_model) # Adjusted R-Squared = 0.716
corrplot.mixed(corr=cor(no_outlier_no_colinearity_customer_data[, c("Quantity", "date_range", "Profit", "CLV_Score")], use="complete.obs"), upper="ellipse", tl.pos="lt")
vif(best_CLV_model)

coxph(survival~Discount+Segment+Market, data=no_outlier_no_colinearity_customer_data)
clv_top_25_percent = quantile(no_outlier_no_colinearity_customer_data$CLV_Score, 0.75)
clv_data = no_outlier_no_colinearity_customer_data %>% mutate(CLV_Group = ifelse(CLV_Score >= clv_top_25_percent, "Top 25%", "Bottom 75%"))
clv_group_summary = clv_data %>% group_by(CLV_Group) %>% summarize(Total_CLV = sum(CLV_Score))
clv_group_summary
variable.names(clv_data)
clv_fit = survfit(survival~CLV_Group, data=clv_data, na.action = na.exclude)
ggsurvplot(clv_fit, data=clv_data, mark.time=F)
t.test(date_range/30~CLV_Group, clv_data)


table(clv_data$CLV_Group)
clv_data = clv_data %>% mutate(Order.Date = as.Date(Order.Date, format="%Y-%m-%d"), last_date = as.Date(last_date, format="%Y-%m-%d"))
logistic_reg_data = clv_data %>% mutate(
  clv_probability = ifelse(CLV_Group =="Top 25%", 1, 0),
  Recency = as.numeric(difftime(max(Order.Date), last_date, units = "days")),
  Frequency = n(),
  Monetary = sum(Sales),
  Segment_Encoded = case_when(Segment == "Consumer" ~ 1, Segment == "Corporate" ~ 2, Segment == "Home Office" ~ 3, TRUE ~ 0),
  Category_Encoded = case_when(Category == "Furniture" ~ 1, Category=="Office Supplies" ~ 2, Category=="Technology" ~ 3, TRUE ~ 0),
  Subcat_Encoded = case_when(Sub.Category=="Accessories"~1, Sub.Category=="Appliances"~2, Sub.Category=="Art"~3, Sub.Category=="Binders"~4, Sub.Category=="Bookcases"~5, Sub.Category=="Chairs"~6, Sub.Category=="Copiers"~7, Sub.Category=="Envelopes"~8, Sub.Category=="Fasteners"~9, Sub.Category=="Furnishings"~10, Sub.Category=="Labels"~11, Sub.Category=="Machines"~12, Sub.Category=="Paper"~13, Sub.Category=="Phones"~14, Sub.Category=="Storage"~15, Sub.Category=="Supplies"~16, Sub.Category=="Tables"~17, TRUE~0),
  ShipMode_Encoded = case_when(Ship.Mode=="First Class"~1, Ship.Mode=="Same Day"~2, Ship.Mode=="Second Class"~3, Ship.Mode=="Standard Class"~4, TRUE~0),
  OrderPriority_Encoded = case_when(Order.Priority=="Critical"~1, Order.Priority=="High"~2, Order.Priority=="Medium"~3, Order.Priority=="Low"~4, TRUE~0),
  Market_Encoded = case_when(Market=="Africa"~1, Market=="APAC"~2, Market=="Canada"~3, Market=="EMEA"~4, Market=="EU"~5, Market=="LATAM"~6, Market=="US"~7, TRUE~0)
  )
set.seed(123)
traintestsplit_clv = sort(sample(nrow(logistic_reg_data), nrow(logistic_reg_data) * 0.7))
train_data_clv = logistic_reg_data[traintestsplit_clv, ]
test_data_clv = logistic_reg_data[-traintestsplit_clv, ]

clv_logreg = glm(clv_probability~ShipMode_Encoded+Segment_Encoded+Market_Encoded+Category_Encoded+Subcat_Encoded+Sales+Quantity+Discount+Profit+Shipping.Cost+OrderPriority_Encoded+date_range+survival_time, data=train_data_clv, family=binomial)
step_logreg = step(clv_logreg)
corrplot.mixed(corr=cor(train_data_clv[, c("Market_Encoded", "Sales", "Quantity", "Discount", "Profit", "date_range", "survival_time", "clv_probability")], use="complete.obs"), upper="ellipse", tl.pos="lt")
# remove Sales, survival_time, ShipMode_Encoded, Profit
corrplot.mixed(corr=cor(train_data_clv[, c("Market_Encoded", "Quantity", "Discount", "date_range", "clv_probability")], use="complete.obs"), upper="ellipse", tl.pos="lt")

best_clv_logreg = glm(clv_probability~Market_Encoded+Quantity+Discount+date_range, data=train_data_clv, family=binomial)
exp(coef(best_clv_logreg))

clv_prediction = predict(best_clv_logreg, test_data_clv, type="response")
final_clv_data = cbind(test_data_clv, clv_prediction)

model1_clv = table(final_clv_data$clv_probability, final_clv_data$...44 > 0.5)
accuracy_f1_clv = sum(diag(model1_clv)) / sum(model1_clv)
accuracy_f1_clv #0.737

new_clv_prediction = prediction(clv_prediction, test_data_clv$clv_probability)
evaluation_clv = performance(new_clv_prediction, "acc")
plot(evaluation_clv)

max_clv = which.max(slot(evaluation_clv, "y.values")[[1]])
best_accuracy_f1_clv = slot(evaluation_clv, "y.values")[[1]][max_clv]
clv_cutoff = slot(evaluation_clv, "x.values")[[1]][max_clv]
print(c(accuracy.rate=best_accuracy_f1_clv, cutoff=clv_cutoff)) # Best accuracy: 0.745

plot(performance(new_clv_prediction, "tpr", "fpr"), colorize=T, xlab="False Positive", ylab="True Positive", main="Simplified Logistic Regression for CLV Group Probability")
auc = performance(new_clv_prediction, "auc")@y.values[[1]]
auc # 0.7797 | 0.7733 | 0.7644


set.seed(123)
traintestsplit_churn = sort(sample(nrow(logistic_reg_data), nrow(logistic_reg_data) * 0.7))
train_data_churn = logistic_reg_data[traintestsplit_churn, ]
test_data_churn = logistic_reg_data[-traintestsplit_churn, ]

variable.names(logistic_reg_data)

churn_logreg = glm(event_status~Sales+Quantity+Discount+Profit+Shipping.Cost+CLV_Score+Segment_Encoded+Category_Encoded+Subcat_Encoded+ShipMode_Encoded+OrderPriority_Encoded+Market_Encoded, data=train_data_churn, family=binomial())
step_churn_logreg = step(churn_logreg)
corrplot.mixed(corr=cor(train_data_churn[, c("Sales", "Shipping.Cost", "CLV_Score", "Segment_Encoded", "ShipMode_Encoded", "Market_Encoded", "event_status")], use="complete.obs"), upper="ellipse", tl.pos="lt")
#remove Shipping.Cost, Market
corrplot.mixed(corr=cor(train_data_churn[, c("Sales","CLV_Score", "Segment_Encoded", "ShipMode_Encoded", "event_status")], use="complete.obs"), upper="ellipse", tl.pos="lt")

best_churn_logreg = glm(event_status~Sales+CLV_Score+Segment_Encoded+ShipMode_Encoded, data=train_data_churn, family=binomial())
exp(coef(best_churn_logreg))

churn_prediction = predict(best_churn_logreg, test_data_churn, type="response")
final_churn_data = cbind(test_data_churn, churn_prediction)
model1_churn = table(final_churn_data$event_status, final_churn_data$...44 > 0.5)
prop.table(model1_churn)
accuracy_f1_churn = sum(diag(model1_churn)) / sum(model1_churn)
accuracy_f1_churn #0.7922 | 0.7924 | 0.7922

new_churn_pred = prediction(churn_prediction, test_data_churn$event_status)
evaluation_churn = performance(new_churn_pred, "acc")
plot(evaluation_churn)

max_churn = which.max(slot(evaluation_churn, "y.values")[[1]])
best_accuracy_f1_churn = slot(evaluation_churn, "y.values")[[1]][max_churn]
churn_cutoff = slot(evaluation_churn, "x.values")[[1]][max_churn]
print(c(accuracy.rate=best_accuracy_f1_churn, cutoff=churn_cutoff)) # Best accuracy: 0.7960 | 0.7971 | 0.804

plot(performance(new_churn_pred, "tpr", "fpr"), colorize=T, xlab="False Positive", ylab="True Positive", main="Simplified Logistic Regression for Customer Churn Probability")
auc_churn = performance(new_churn_pred, "auc")@y.values[[1]]
auc_churn #0.804 | 0.8062 | 0.8147



# pseudo_churn_data = logistic_reg_data
# set.seed(123)
# 
# traintestsplit_pseudo = sort(sample(nrow(pseudo_churn_data), nrow(pseudo_churn_data) * 0.7))
# train_data_pseudo = pseudo_churn_data[traintestsplit_pseudo, ]
# test_data_pseudo = pseudo_churn_data[-traintestsplit_pseudo, ]
# gee_model = geeglm(event_status~Segment_Encoded+Category_Encoded+Subcat_Encoded+ShipMode_Encoded+OrderPriority_Encoded+Market_Encoded+Sales+Quantity+Discount+Profit+Shipping.Cost+date_range, data=train_data_pseudo, id=Customer.ID, family=binomial(), corstr="exchangeable")
# 
# pseudo_data = pseudosurv(train_data_pseudo$survival_time, train_data_pseudo$event_status, tmax=max(train_data_pseudo$survival_time))
# train_pseudo = cbind(train_data_pseudo, pseudo_data)
# pseudo_model = glm(pseudo.y~Segment_Encoded+Category_Encoded+Subcat_Encoded+ShipMode_Encoded+OrderPriority_Encoded+Market_Encoded+Sales+Quantity+Discount+Profit+Shipping.Cost+date_range, data=train_pseudo, family=quasibinomial())
# 
# gee_pred = predict(gee_model, test_data_pseudo, type="response")
# gee_roc = prediction(gee_pred, test_data_pseudo$event_status)
# plot(performance(gee_pred, "tpr", "fpr"), colorize=T, xlab="False Positive", ylab="True Positive")
# gee_auc = performance(gee_roc, "auc")@y.values[[1]]
# gee_auc
# 
# pseudo_pred = predict(pseudo_model, test_data_pseudo, type="response")
# pseudo_roc = prediction(pseudo_pred, test_data_pseudo$event_status)
# plot(performance(pseudo_pred, "tpr", "fpr"), colorize=T, xlab="False Positive", ylab="True Positive")
# pseudo_auc = performance(pseudo_roc, "auc")@y.values[[1]]
# pseudo_auc





