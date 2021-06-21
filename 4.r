# Shopping cart analysis task

# load libraries
library(tidyverse)
#install.packages("arules")
library(arules)
#install.packages("arulesViz")
library(arulesViz)
library(RColorBrewer)

# read transactions dataset and convert to arules sparse matrix format
fcsv <- "https://raw.githubusercontent.com/multidis/hult-retail-analytics/main/shopping_cart/transactions_binary.csv"
df_trans <- read_csv(fcsv)
trans <- transactions(as.matrix(df_trans))
summary(trans)

# frequency plot
itemFrequencyPlot(trans, topN = 30)

# rules exceeding support and confidence thresholds
rules <- apriori(trans, parameter = list(support=0.001, confidence=0.5))

# most promising rules by lift (confidence-support scatterplot)
rules_sel <- head(sort(rules, by="lift"), 5)

# network representation
plot(rules_sel, method = "graph",  engine = "htmlwidget")

# rules for a selected product
rules_selprod <- subset(rules, subset = rhs %pin% "bottled water")
bottled_water <- inspect(head(sort(rules_selprod, by="lift"), 5))

summary <- rbind(whole_milk, other_vegetables, rolls, soda, yogurt, bottled_water)
summary
summary %>% sort(lift, decreasing = TRUE)
str(summary)
df_trans

#Step 1
#Identify top-30 most frequently bought products during the time period of the dataset.

# Identify top-30 most frequently bought products
col_sum <- colSums(df_trans)
View(col_sum)

top_30 <- col_sum %>%
      sort(decreasing = TRUE) %>%
      head(n=30)
View(top_30)

# frequency plot
itemFrequencyPlot(trans, topN = 30)


#Step 2
#Identify at least 5 most promising product association rules that involve top-N most frequently bought product, 
#with N being your team number.Support your conclusions with relevant metrics (support, confidence, lift).

# Top-6 most frequently bought product
top_6 <- col_sum %>%
  sort(decreasing = TRUE) %>%
  head(n=6)
View(top_6)


# top 5 rules in terms of lift
inspect(head(sort(rules, by ="lift"), 5))

# Show top 5 rules
plot(rules_sel, engine="plotly", control=list(col=brewer.pal(11,"Spectral")), main="")


#Please make sure to include a full, explicit listing of top product names and most promising association rules 
#in your presentation and assignment submission.

# Additional confirmations
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
