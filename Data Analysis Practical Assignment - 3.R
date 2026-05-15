#ASSIGNMENT NO.3: Advanced Data Manipulation Techniques in R Recoding Variables 
# Create Data Frame 
sales_data <- data.frame( 
  OrderID = 1:200, 
  CustomerName  = sample(c("Rahul","Amit","Priya","Neha","Arjun","Sneha","Rohit","Kiran"),200,replace=TRUE), 
  City = sample(c("Pune","Mumbai","Delhi","Bangalore","Hyderabad"),200,replace=TRUE), 
  Product = sample(c("Laptop","Mobile","Tablet","Shoes","Watch"),200,replace=TRUE), 
  Category = sample(c("Electronics","Fashion"),200,replace=TRUE), 
  Quantity = sample(1:10,200,replace=TRUE), 
  Price = sample(seq(500,50000,500),200,replace=TRUE), 
  Discount = sample(c(0,5,10,15,20),200,replace=TRUE), 
  PaymentMethod = sample(c("Cash","Card","UPI"),200,replace=TRUE) 
) 

# Revenue Calculation 
sales_data$Revenue <- sales_data$Quantity * sales_data$Price 
# Discount Amount 
sales_data$DiscountAmount <- sales_data$Revenue * sales_data$Discount / 100 
# Final Amount 
sales_data$FinalAmount <- sales_data$Revenue - sales_data$DiscountAmount 

#Q1) Problems on Recoding Variables, Sorting and New Variable Creation  
#a) Create OrderSize (Small, Medium, Large) 
#b) Sort by Quantity descending 
#c) Sort by City and Revenue 
#d) Create Tax column 
#e) Create TotalBill column 


# a) Create OrderSize (Small, Medium, Large) 
sales_data$OrderSize <- ifelse( 
  sales_data$Quantity <= 3, "Small", 
  ifelse(sales_data$Quantity <= 7, "Medium", "Large") 
) 
print(sales_data$OrderSize) 

# b) Sort by Quantity descending 
sorted_quantity <- sales_data[order(-sales_data$Quantity), ] 
print(sorted_quantity) 

# c) Sort by City and Revenue 
sorted_city_revenue <- sales_data[order(sales_data$City, sales_data$Revenue), ] 
print(sorted_city_revenue) 

# d) Create Tax column (18% Tax) 
sales_data$Tax <- sales_data$FinalAmount * 0.18 
print(sales_data$Tax) 

# e) Create TotalBill column 
sales_data$TotalBill <- sales_data$FinalAmount + sales_data$Tax 
print(sales_data$TotalBill) 

# Display Updated Data 
head(sales_data) 

#Q2) Problems on dplyr select() and filter() 
#a) Select CustomerName and Product 
#b) Select numeric columns 
#c) Remove Discount column 
#d) Filter Pune customers 
#e) Filter multiple conditions (Price > 20000 and Quantity > 5) 
# Create Data Frame 

# Install and load dplyr 
install.packages("dplyr") 
library(dplyr) 

# a) Create Tax variable (18%) 
sales_data <- sales_data %>% 
  mutate(Tax = FinalAmount * 0.18) 
print(sales_data$Tax) 

# b) Create Profit variable 
sales_data <- sales_data %>% 
  mutate(Profit = FinalAmount * 0.20) 
print(sales_data$Profit) 

# c) Create OrderCategory 
sales_data <- sales_data %>% 
  mutate( 
    OrderCategory = ifelse( 
      Quantity <= 3, "Low", 
      ifelse(Quantity <= 7, "Medium", "High") 
    ) 
  ) 
print(sales_data$OrderCategory) 

# d) Create DiscountCategory 
sales_data <- sales_data %>% 
  mutate( 
    DiscountCategory = ifelse( 
      Discount == 0, "No Discount", 
      ifelse(Discount <= 10, "Low Discount", "High Discount") 
    ) 
  ) 
print(sales_data$DiscountCategory) 

# e) Create PaymentType variable 
sales_data <- sales_data %>% 
  mutate( 
    PaymentType = ifelse( 
      PaymentMethod == "Cash", "Offline", "Online" 
    ) 
  ) 
print(sales_data$PaymentType) 

# f) Sort by Revenue descending using arrange() 
sorted_revenue <- sales_data %>% 
  arrange(desc(Revenue)) 
print(sorted_revenue) 

#Q3) Problems on Using mutate() and arrange() 
#a. Create Tax variable 
#b. Create Profit variable 
#c. Create OrderCategory 
#d. Create DiscountCategory 
#e. Create PaymentType variable 
#f. Sort by Revenue descending by using arrange() 

# Load dplyr package 
library(dplyr) 

# Install and load dplyr 
install.packages("dplyr") 
library(dplyr) 

# a) Create Tax variable (18%) 
sales_data <- sales_data %>% 
  mutate(Tax = FinalAmount * 0.18) 
print(sales_data$Tax) 

# b) Create Profit variable 
sales_data <- sales_data %>% 
  mutate(Profit = FinalAmount * 0.20) 
print(sales_data$Profit) 

# c) Create OrderCategory 
sales_data <- sales_data %>% 
  mutate( 
    OrderCategory = ifelse( 
      Quantity <= 3, "Low", 
      ifelse(Quantity <= 7, "Medium", "High") 
    ) 
  ) 
print(sales_data$OrderCategory) 

# d) Create DiscountCategory 
sales_data <- sales_data %>% 
  mutate( 
    DiscountCategory = ifelse( 
      Discount == 0, "No Discount", 
      ifelse(Discount <= 10, "Low Discount", "High Discount") 
    ) 
  ) 
print(sales_data$DiscountCategory) 

# e) Create PaymentType variable 
sales_data <- sales_data %>% 
  mutate( 
    PaymentType = ifelse( 
      PaymentMethod == "Cash", "Offline", "Online" 
    ) 
  ) 
print(sales_data$PaymentType) 

# f) Sort by Revenue descending using arrange() 
sorted_revenue <- sales_data %>% 
  arrange(desc(Revenue)) 
print(sorted_revenue) 

#Q4) Problems on Using group_by() and summarise() 
#a. Total revenue by city 
#b. Average price by product 
#c. Total quantity sold by product 
#d. Maximum revenue by city 
#e. Minimum revenue by product 

# Install and load dplyr 
install.packages("dplyr") 
library(dplyr) 
# Create Revenue column 
sales_data$Revenue <- sales_data$Quantity * sales_data$Price 

# a) Total revenue by city 
total_revenue_city <- sales_data %>% 
  group_by(City) %>% 
  summarise(TotalRevenue = sum(Revenue)) 
print(total_revenue_city) 

# b) Average price by product 
average_price_product <- sales_data %>% 
  group_by(Product) %>% 
  summarise(AveragePrice = mean(Price)) 
print(average_price_product) 

# c) Total quantity sold by product 
total_quantity_product <- sales_data %>% 
  group_by(Product) %>% 
  summarise(TotalQuantity = sum(Quantity)) 
print(total_quantity_product) 

# d) Maximum revenue by city 
max_revenue_city <- sales_data %>% 
  group_by(City) %>% 
  summarise(MaxRevenue = max(Revenue)) 
print(max_revenue_city)

# e) Minimum revenue by product 
min_revenue_product <- sales_data %>% 
  group_by(Product) %>% 
  summarise(MinRevenue = min(Revenue)) 
print(min_revenue_product) 

#Q5) Problems on Customer Analysis 
#a. Count orders per customer 
#b. Total revenue per customer 
#c. Average order value 
#d. Top customer 
#e. Sort customers by revenue 

# Install and load dplyr 
install.packages("dplyr") 
library(dplyr) 
# Create Revenue column 
sales_data$Revenue <- sales_data$Quantity * sales_data$Price 

# a. Count orders per customer 
orders_per_customer <- sales_data %>% 
  group_by(CustomerName) %>% 
  summarise(OrderCount = n()) 
print(orders_per_customer) 

# b. Total revenue per customer 
revenue_per_customer <- sales_data %>% 
  group_by(CustomerName) %>% 
  summarise(TotalRevenue = sum(Revenue)) 
print(revenue_per_customer) 

# c. Average order value 
average_order_value <- sales_data %>% 
  group_by(CustomerName) %>% 
  summarise(AverageOrderValue = mean(Revenue)) 
print(average_order_value)

# d. Top customer 
top_customer <- sales_data %>% 
  group_by(CustomerName) %>% 
  summarise(TotalRevenue = sum(Revenue)) %>% 
  arrange(desc(TotalRevenue)) %>% 
  head(1) 
print(top_customer) 

# e. Sort customers by revenue 
sorted_customers <- sales_data %>% 
  group_by(CustomerName) %>% 
  summarise(TotalRevenue = sum(Revenue)) %>% 
  arrange(desc(TotalRevenue)) 
print(sorted_customers) 