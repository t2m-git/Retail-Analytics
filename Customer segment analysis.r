# Customer segment analysis task

# Load libraries
library(tidyverse)
library(ggplot2)
library(plotly)

# Load data
fcsv <- "https://raw.githubusercontent.com/multidis/hult-retail-analytics/main/customer_segmentation/datasets/customers_6.csv"
cust_rfm <- read_csv(fcsv)
cust_rfm


#1. As a part of an effort to increase the revenue from frequent shoppers, you are asked what is 
#the current average spending of the customers who are within the top 20% shopping frequency quantile 
#and made their most recent purchase during the past two weeks.

# Show top20% shopping frequency
frequncy_top20 <- quantile(cust_rfm$frequency, 0.8)

# Show top20% shopping frequency
recency_2w <- 14

# Show average spending of the customers within the top 20% shopping frequency quantile
# and made their most recent purchase during the past two weeks
mean(cust_rfm[which(cust_rfm$frequency >= frequncy_top20 & cust_rfm$recency <= recency_2w), ]$monetary)


#2. In order to plan customer retention efforts, you are asked how long ago, on average, was the last purchase 
#of most valuable shoppers (top 30% quantile in both frequency and spending).

# Show top30% shopping frequency
top30percent_frequency <- quantile(cust_rfm$frequency, 0.7)

# Show top30% shopping spending
top30percent_monetary <- quantile(cust_rfm$monetary, 0.7)

# Show mean recency among the most valueable shoppers(top 30% quantile in both frequency and spending) 
mean(cust_rfm[which(cust_rfm$frequency >= top30percent_frequency & cust_rfm$monetary >= top30percent_monetary), ]$recency)


#3. Please provide a sampling of 10 most valuable customers (CustomerID values) 
#for the organization. Support your answer (customer selection) with the relevant metrics.

#creating a function for min-max normalization
my_normalize <- function(x){
  
  my_min <- min(x, na.rm=T)
  my_max <- max(x, na.rm=T)
  min_max <- (x - my_min)/(my_max - my_min)
  return(min_max)
  
}#closing my_normalize

# Normalize data
cust_rfm$recency_normal <- my_normalize(x=cust_rfm$recency)
cust_rfm$frequency_normal <- my_normalize(x=cust_rfm$frequency)
cust_rfm$monetary_normal <- my_normalize(x=cust_rfm$monetary)

# Convert recency_normal into values that small value has high score
cust_rfm$monetary_normal <- 1 - cust_rfm$monetary_normal

cust_rfm$score <- (cust_rfm$frequency_normal * 2) + (cust_rfm$frequency_normal * 2) + cust_rfm$monetary_normal

# Get top 10 based on the score calculated previously
top10_score <- cust_rfm %>%
  arrange(desc(score)) %>%
  head(n=10)

# Show top 10 customers
View(top10_score$CustomerID)

#Please make sure to include your worksheets/code along with the answers in your team presentation 
#(and the final project if choosing this task).

# Additional data explorations
# explore customer metrics in the dataset
qplot(frequency, data=cust_rfm, binwidth=5)
quantile(cust_rfm$frequency, 0.7)
quantile(cust_rfm$frequency, 0.8)
qplot(recency, data=cust_rfm, binwidth=10)
quantile(cust_rfm$recency, 0.3)
qplot(monetary, data=cust_rfm, binwidth=1000)
qplot(monetary, data=cust_rfm, binwidth=500, xlim=c(0, 20000))
quantile(cust_rfm$monetary, 0.7)

# interactive scatter plot to explore most valuable customers
plot_ly(cust_rfm, x = ~frequency, y = ~monetary, color = ~recency,
        hoverinfo = "text", text = ~CustomerID) %>%
  add_markers()

# edit as needed: average values over customer subsets; this is just an example
cust_rfm %>%
  filter(frequency > quantile(cust_rfm$frequency, 0.7)) %>%
  filter(monetary > quantile(cust_rfm$monetary, 0.7)) %>%
  summarize(rec = mean(recency))
