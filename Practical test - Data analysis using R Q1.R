#Q1) Data Cleaning and transformation

df <- data.frame(
  name = c("Raj", " Amit", "Neha ", "raj", "Amit"),
  marks = c("80", "90", NA, "85", "90"),
  city = c("Pune", " pune", "Mumbai", "PUNE", "Mumbai")
)

# 1. Remove extra spaces from name column
df$name <- trimws(df$name)

cat("\nNames after removing spaces:\n")
print(df$name)

# 2. Convert all names to lowercase
df$name <- tolower(df$name)

cat("\nNames in lowercase:\n")
print(df$name)

# 3. Convert marks column to numeric and handle missing values
df$marks <- as.numeric(df$marks)

# Replace NA with mean marks
df$marks[is.na(df$marks)] <- mean(df$marks, na.rm = TRUE)

cat("\nMarks after handling missing values:\n")
print(df$marks)

# 4. Standardize city names (lowercase)
df$city <- tolower(trimws(df$city))

cat("\nStandardized City Names:\n")
print(df$city)

# 5. Filter students with marks > 80
high_marks <- subset(df, marks > 80)

cat("\nStudents with Marks > 80:\n")
print(high_marks)

# 6. Create Grade Column
df$grade <- ifelse(df$marks >= 85, "A", "B")

cat("\nFinal Data Frame with Grade:\n")
print(df)