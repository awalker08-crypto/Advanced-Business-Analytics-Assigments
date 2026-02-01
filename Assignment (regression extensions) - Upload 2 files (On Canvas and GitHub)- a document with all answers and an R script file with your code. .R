# Load required library
library(tidyverse)

# Load required library
library(readxl)

# Read the Excel file
wages <- read_excel("wages.xlsx")

# View the structure of the dataset
view(wages)

# Problem. 1.

# Question A. 

# Scatter Plot of Wage vs Age
plot(wages$Age, wages$Wage,
     xlab = "Age",
     ylab = "Hourly Wage",
     main = "Hourly Wage vs Age",
     pch = 19)

# Linear model
linear_model <- lm(Wage ~ Age, data = wages)

# Quadratic model
quadratic_model <- lm(Wage ~ Age + I(Age^2), data = wages)

# View results of both linear model and quadratic model
summary(linear_model)
summary(quadratic_model)

# Question B. 

# Estimate multiple regression model
multiple_model <- lm(Wage ~ Age + Educ, data = wages)

# View results
summary(multiple_model)

# Question C.

# Estimate quadratic mutiple regression model
quad_multiple_model <- lm(Wage ~ Age + I(Age^2) + Educ, data = wages)

# View results
summary(quad_multiple_model)

# Question D.

# Create a new data frame for individuals with 16 years of education
# at ages 30, 50, and 70
Sixteen_Edc_data <- data.frame(
  Age = c(30, 50, 70),
  Educ = c(16, 16, 16)
)


# Question E.

# Use the quadratic multiple regression model to predict hourly wages
# for the specified ages and education level.
# 1. = 30, 2.= 50, 3. = 70
predict(quad_multiple_model, newdata = Sixteen_Edc_data)

# Problem. 2.

# Question A.

AnnArbor <- read_excel("AnnArbor.xlsx")

# View the structure of the dataset
view(AnnArbor)


## Plot Rent against Number of Bedrooms
plot(AnnArbor$Beds, AnnArbor$Rent,
     xlab = "Number of Bedrooms",
     ylab = "Monthly Rent",
     main = "Rent vs Bedrooms")

# Plot Rent against Number of Bathrooms
plot(AnnArbor$Baths, AnnArbor$Rent,
     xlab = "Number of Bathrooms",
     ylab = "Monthly Rent",
     main = "Rent vs Bathrooms")

# Plot Rent against Square Footage
plot(AnnArbor$Sqft, AnnArbor$Rent,
     xlab = "Square Footage",
     ylab = "Monthly Rent",
     main = "Rent vs Square Footage")

# Question B.

# Estimate a multiple regression model with log-transformed square footage
rent_model <- lm(Rent ~ log(Sqft) + Beds + Baths, data = AnnArbor)

# View the regression results
summary(rent_model)

# Create a data frame for the rental to be predicted
new_rental <- data.frame(
  Sqft = 1600,
  Beds = 3,
  Baths = 2
)

# Predict monthly rent for the specified rental
predict(rent_model, newdata = new_rental)

