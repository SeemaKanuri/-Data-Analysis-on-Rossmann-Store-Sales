# Data-Analysis-on-Rossmann-Store-Sales

Developed a Data Product and GUI to analyze and visualization of annual sales based on the promotions, holidays, competitions and seasons by using R,  Rstudio, Rshiny(dplr, plotly,  ggplot2,  reshape, forecast) and python(numpy, sklearn, Pandas, Matplotlib).Employed various statistical tools, ANOVA, hypothesis to provide insights into the underlying causes for customer churn.

# Problem Statement

It’s a known fact that sales equal profits, with the increasing cost of doing business now a day’s retailers need to make profit on their sales. It is important to increase their sales and to save money by vigilantly planning their staff and inventory. In theory, if the cost of sales and investment in inventory is increased by 1%, profit should be increased in the double ratio. However, in reality ‘retailers’ don’t consider how much profit is made on each sale. In our project we have taken sales data of the Roassmann store which is a huge European drug store spanning over 3000 locations. We aim to provide strategic solution to Rossmann managers who are tasked to predict the reliable sales for these store (Source: www.kaggle.com). This will help the stores to save money if their sales are predictable and also enable store managers to create effective staff schedules and stock their inventory to increase profit in parallel to giving good customer service.
For instance, our analysis on the past data of the store will provide the insight of the sales trend for the next season if the sales are growing, by assigning more staff on those stores is regarded as good decision. On the other hand, focusing on stronger inventories in advance will help the managers to meet their customer demands which in turn increases productivity and profit. Our analysis will help the managers to optimize their profit with adequate staff and inventory without incurring great expense or time and consistent high level of service to their customers.

#  Data Sets

We have taken the historical data of up to 2 years 7 months i.e. from Jan 2013 to July 2015 for individual 1115 Rossmann stores (Source: www.kaggle.com). We using the dataset which contains two tables namely; 
train.csv which contain the data including daily sales information based on each date, feature of the day (promo, holiday, etc.) and number of customers. This data set contains total observations of 1017210 with each 9 variables as store, day of week, date, sales, customers, open, promo, state holiday, school holiday.
store.csv which contain the supplemental information about the store’s like competitions and promotions applied. There are total of 10 variables as store, store type, assortment, competition distance, competition open since month, promo2, promo2since week, promo2since year, promo interval with 1115 records.

# Immediate objectives and initial approach:

Analyze the distribution and types of all stores involved in long term promotion
Understanding the store pattern over the 2.7 years
Analyze store staff scheduling in comparative to the sales
Effect of single and consecutive promotion on sales
Analyze the distribution of promotion on each day of week or month
Effect of Competition Open Since Year on store sales
Drive incremental sales by expanding hours during busy seasons.
Effect of state holiday, school holiday on sales
Exploring and visualization the data
