# Assignment 1 Solution - Introduction to R and Basic Operations

# Q1) Write an R program that: 
#     1.	Takes two numeric values from user. 
#     2.	Performs all arithmetic operations (+, −, *, /, power, modulus). 
#     3.	Stores the results in a vector. 
#     4.	Displays the maximum, minimum, and average value of the results. 

# Solution :

#Takes two numeric values from user.
a <- as.numeric(readline("Enter a First number :"))
b <- as.numeric(readline("Enter a second number :"))

# Arithmetic operations
sum_val <- a + b
diff_val <- a - b
prod_val <- a * b
div_val <- a / b
power_val <- a ^ b
mod_val <- a %% b

# Store results in vector
results <- c(sum_val, diff_val, prod_val, div_val, power_val, mod_val)

print(results)

#Displays the maximum, minimum, and average value of the results.
cat("Maximum:",max(results),"\n")
cat("Minimum:",min(results),"\n")
cat("Average:",mean(results),"\n")


# Q2) Create an R program that:
#     1. Generates 10 random numbers between 1 and 100.
#     2. Calculates square root and then rounded value, ceiling, and floor for each square root value.
#     3. Stores results in a data frame.

# Solution :

n <-as.numeric(readline("Enter how many numbers:"))

#validation check
if(is.na(n) || n<=0){
  stop("Invalid input.Please enter valid positive number.")
}else{nums <- sample(1:100,n)}

#Calculate square root and transformations
sqrt_vals <- sqrt(nums)
rounded <- round(sqrt_vals)
ceil_vals <- ceiling(sqrt_vals)
floor_vals <- floor(sqrt_vals)

#Store in data frame
df <- data.frame(
  Numbers = nums,
  SquareRoot = sqrt_vals,
  Rounded = rounded,
  Ceiling = ceil_vals,
  Floor = floor_vals
)
print(df)


# Q3) Write an R program that:
#     1. Takes a list of names.
#     2. Converts all names to uppercase.
#     3. Calculates the number of characters in each name.
#     4. Combines the name and its length into a formatted sentence.

# Solution :

n<-as.numeric(readline("Enter number of names: "))
names <- character(n)
for(i in 1:n){
  names[i] <- readline(paste("Enter name",i,":"))
}

upper_names <- toupper(names)
lengths <- nchar(upper_names)
result <- paste("Name:", upper_names, "-Length:", lengths)
print(result)


# Q4) Write an R program that:
#     1. Creates a vector of numbers from 1 to 200.
#     2. Finds numbers that are divisible by 3 and 5.
#     3. Calculates square root and square of those numbers.
#     4. Displays results in a table format.

# Solution :

nums <- 1:200
filtered <- nums[nums %% 3 == 0 & nums %% 5 == 0]

square_root<- sqrt(filtered)
square_val <- filtered^2

result  <- data.frame(
  Number = filtered,
  SquareRoot = square_root,
  Square = square_val
)

print(result)


# Q5) Write an R program to evaluate a quadratic equation 
#     for given a, b, c values and return real or complex roots.

# Formula : 
# For ax² + bx + c = 0
# Discriminant: D = b² − 4ac
# Roots: x = (-b ± √D) / 2a

# Solution :

a <- as.numeric(readline("Enter a: "))
b <- as.numeric(readline("Enter b: "))
c <- as.numeric(readline("Enter c: "))

D <- b^2 - 4*a*c

if (D > 0) {
  root1 <- (-b + sqrt(D)) / (2*a)
  root2 <- (-b - sqrt(D)) / (2*a)
  
  cat("Root 1 =", root1, "\n")
  cat("Root 2 =", root2, "\n")
  
} else if (D == 0) {
  root <- -b / (2*a)
  cat("Equal Root =", root, "\n")
} else {
    cat("Complex Roots\n")
}