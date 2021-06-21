# Shelf space analysis task

# Import libraries
library(tidyverse)
library(DT)
#install.packages("ROI")
#install.packages("ROI.plugin.glpk")
library(ROI)
library(ROI.plugin.glpk)
#install.packages("ompr")
#install.packages("ompr.roi")
library(ompr)
library(ompr.roi)

# product list
df_product_set <- read_csv("https://raw.githubusercontent.com/multidis/hult-retail-analytics/main/shelf_space_allocation/datasets/products_set_6.csv")
df_product_promote_set <- read_csv("https://raw.githubusercontent.com/multidis/hult-retail-analytics/main/shelf_space_allocation/datasets/products_promote_set_6.csv")

str(df_product_set)
str(df_product_promote_set)

df_product_promote_set

# Copy
df_product_set_3 <- df_product_set
str(df_product_set)
str(df_product_set_3)

# Initialize promotion
df_product_set_3$promo <- 0
df_product_promote_set$promo <- 1

# Rbind two dataframes
df_product <- rbind(df_product_set_3, df_product_promote_set)

# derived metrics
df_product_set <- df_product_set %>%
  mutate(monthly_profit = unit_margin * monthly_demand, .before = "category_id") %>%
  mutate(monthly_profit_per_width = unit_margin * monthly_demand / width, .before = "category_id")

df_product <- df_product %>%
  mutate(monthly_profit = unit_margin * monthly_demand, .before = "category_id") %>%
  mutate(monthly_profit_per_width = unit_margin * monthly_demand / width, .before = "category_id")

str(df_product)

#Step 1
#Select products for display striving to attain maximum profit per month with the shelf width of 3600 mm.
#Save your product selection worksheet as step 1 solution.

# Not use in this case
# Makes a dataframe corresponding to a size in df_product_set
#df_facing <- data.frame(flg_facing=logical(nrow(df_product_set)))

# Get optimal values
# Decision variables : x[i]
# Objective function : Maximum profit per month the sum of monthly_profit * x[i]
# Constraints : shelf width of 3600 mm the sum of width * x[i]

# the number of rows in the dataframe
n_rows <- nrow(df_product_set)

# Optimization by using glpk
model_1 <- MIPModel() %>%
  add_variable(x[i], i = 1:n_rows, type = "binary") %>%
  set_objective(sum_expr(df_product_set$monthly_profit[i] * x[i], i = 1:n_rows), "max") %>%
  add_constraint(sum_expr(df_product_set$width[i] * x[i], i = 1:n_rows) <= 3600) %>%
  solve_model(with_ROI(solver = "glpk"))

# Show the model
model_1

# Get the solution
df_facing <- get_solution(model_1, x[i])
facing <- df_facing$value

# Add a column and the values based on the solution
df_product_set$facing <- df_facing$value

# Show the results
# The sum of Monthly profit
sum(df_product_set[df_product_set$facing==1, ]$monthly_profit)
# The sum of width
sum(df_product_set[df_product_set$facing==1, ]$width)

# TODO verify a basic constraint: the total widths equal to or less than 3,600
df_product_set[which(facing==TRUE), ]
df_product_set %>% write_csv(file="./shelf_space_step1_solution.csv")


#Step 2
#You were asked by a store manager to display at least 3 products per brand (brands are provided in the brand_id column).
#Still strive to maximize the profit with this additional constraint in place, for the same shelf width.

# Extract unique values of brand ids
brands <- unique(df_product_set$brand_id)
str(brands)
brands

# use a binary mask: 1 where brand matches, 0 otherwise
# example: 1*(df_product_set$brand_id == brand_id)
nbprod_min <- 3
model_2 <- MIPModel() %>%
  add_variable(x[i], i = 1:n_rows, type = "binary") %>%
  set_objective(sum_expr(df_product_set$monthly_profit[i] * x[i], i = 1:n_rows), "max") %>%
  add_constraint(sum_expr(df_product_set$width[i] * x[i], i = 1:n_rows) <= 3600)
for (brand in brands) {
  model_2 <- model_2 %>% add_constraint(sum_expr(1*(df_product_set$brand_id == brand)[i] * x[i], i = 1:n_rows) >= nbprod_min)
}
model_2 <- model_2 %>% solve_model(with_ROI(solver = "glpk"))

# Extract results
df_product_set$facing2 <- get_solution(model_2, x[i])$value

# Show the results
# The sum of Monthly profit
sum(df_product_set[df_product_set$facing2==1, ]$monthly_profit)
# The sum of width
sum(df_product_set[df_product_set$facing2==1, ]$width)

# Set results
facing2 <- df_product_set$facing2

# TODO verify brand constraints: totals should be 3 or more
df_product_set[which(facing2==TRUE), ]
df_product_set %>% write_csv(file="shelf_space_step2_solution.csv")


#Step 3
#Being a good analyst, you have now diligently solved the task with the minimal number of products per brand requirement.
#Hoping to call it a day, you discover that your store manager is negotiating an agreement with another brand to promote
#their products (listed in products_promote_set_*.csv).
#The manager is asking you how much should the brand pay your store 
#if they want at least 8 products to be displayed on the same shelf you were working with. 
#Select the products to display enforcing that constraint while still keeping at least 3 products
#per brand for all other brands (listed in products_set_*.csv). 
#Once you find the best arrangement and estimate monthly profit, compare the profit to the previous step 
#to provide an answer to the store manager.

# Extract unique values of brand ids
brands3 <- unique(df_product$brand_id)
str(df_product)
str(brands3)
brands3

# the number of rows in the dataframe
n_rows <- nrow(df_product)

# use a binary mask: 1 where brand matches, 0 otherwise
# example: 1*(df_product_set$brand_id == brand_id)
promo_min <- 8
model_3 <- MIPModel() %>%
  add_variable(x[i], i = 1:n_rows, type = "binary") %>%
  set_objective(sum_expr(df_product$monthly_profit[i] * x[i], i = 1:n_rows), "max") %>%
  add_constraint(sum_expr(df_product$width[i] * x[i], i = 1:n_rows) <= 3600)
for (brand in brands3) {
  model_3 <- model_3 %>% add_constraint(sum_expr(1*(df_product$brand_id == brand)[i] * x[i], i = 1:n_rows) >= nbprod_min)
}
model_3 <- model_3 %>% add_constraint(sum_expr(df_product$promo[i] * x[i], i = 1:n_rows) >= promo_min)
model_3 <- model_3 %>% solve_model(with_ROI(solver = "glpk"))

# Extract results
df_product$facing3 <- get_solution(model_3, x[i])$value

# Show the results
# The sum of Monthly profit
sum(df_product[df_product$facing3==1, ]$monthly_profit)
# The sum of width
sum(df_product[df_product$facing3==1, ]$width)

# Set results
facing3 <- df_product$facing3

# TODO verify brand constraints: totals should be 3 or more
df_product[which(facing3==TRUE), ]
df_product %>% write_csv(file="./shelf_space_step3_solution.csv")
