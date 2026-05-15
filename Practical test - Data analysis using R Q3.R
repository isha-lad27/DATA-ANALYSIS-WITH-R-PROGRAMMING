#Q3) Statistical analysis and tables

data<-data.frame(
  Gender = c("Male", "Female", "Male", "Female", "Male", "Female"),
  Purchase = c("Yes", "No", "Yes", "Yes", "No", "Yes"),
  Amount = c(5000,7000,8000,6000,4000,9000)
)

#Questions

#1. Calculate mean and median of amount
mean(data$Amount)
median(data$Amount)

#2. Create frequency table for gender
table(data$Gender)

#3. Create a cross-tabulation of gender and purchase
table(data$Gender, data$Purchase)

#4. Find total purchase amount by gender
aggregate(Amount ~ Gender, data, sum)

#5. Count number of "Yes" purcahse
sum(data$Purchase == "Yes")