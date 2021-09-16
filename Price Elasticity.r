# Price elasticity task

# Load libraries
library(tidyverse)
library(ggplot2)

# load task dataset and product cost information
price_sales <- read_csv("https://raw.githubusercontent.com/multidis/hult-retail-analytics/main/price_elasticity/price_sales_weekly.csv")
cost <- read_csv("https://raw.githubusercontent.com/multidis/hult-retail-analytics/main/price_elasticity/product_cost.csv")

# show price_sales
str(price_sales)
View(price_sales)

# show cost
str(cost)
View(cost)

# keep final values in respective arrays
nprods <- 8
aElast <- rep(0, nprods)
aPopt <- rep(0, nprods)

# use product number as index
for (prod in 1:nprods) {
  # linear regression for sales vs. price
  fit <- lm(as.formula(paste(paste0("Q_P", prod), "~", paste0("price_P", prod))), data = price_sales)
  C1 <- fit$coefficients[1]
  C2 <- -fit$coefficients[2]
  
  # average price and sales
  Pav <- price_sales %>% summarize(Pav = mean(get(paste0("price_P", prod))))
  Qav <- price_sales %>% summarize(Qav = mean(get(paste0("Q_P", prod))))
  
  # price elasticity and optimal price
  aElast[prod] <- as.numeric(-C2 * Pav / Qav)
  aPopt[prod] <- as.numeric((C1 + C2*cost[prod, "cost"]) / (2*C2))
}

# 1.Estimate price elasticity for your selected products and rank the products 
#specifying which of those is the most and the least sensitive to price changes.

aElast

# Most sensitive - Product 3 -6.831348
min(aElast)

# Least sensitive - Product 8 0.7343663
max(aElast)

# 2.Propose a near-optimal price for selling each of your selected products using 
#the cost values provided in the following table:

aPopt

#[1] 2.148001 5.427585 2.286022 4.567088 7.462569 5.255232 3.657720 3.389826

# sample

# average price and sales
Pav <-  summarize(price_sales, Pav = mean(get(paste0("price_P", prod))))
Pav

Qav <- price_sales %>% summarize(Qav = mean(get(paste0("Q_P", prod))))
Qav

# estimate price elasticity
Elast <- -C2 * Pav / Qav
Elast

# find optimal price
Popt <- (C1 + C2*cost[prod, "cost"]) / (2*C2)
Popt


# visualize the linear fit
ggplot(price_sales, aes(x=price_P3, y=Q_P3)) + geom_point() +
  geom_smooth(method='lm', formula = y ~ x)

ggplot(price_sales, aes(x=price_P8, y=Q_P8)) + geom_point() +
  geom_smooth(method='lm', formula = y ~ x)
