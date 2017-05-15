# Read in CSV data and look at top of dataset
full_data <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv", header = T)
head(full_data)

# Scale independent vars for PCA
scale.vars <- scale(full_data[, 1:20]) 
scale.vars[is.nan(scale.vars)] <- 0 
vars.income <- full_data[, 21]

# Apply PCA
vars.pca <- prcomp(scale.vars,
                 center = TRUE,
                 scale. = TRUE)

# Print PCA
print(vars.pca)

# Plot PCA
plot(vars.pca, type = "l")

# Summarize importance of each component
summary(vars.pca)

# Build a regression model based on top 5 PCs
income.modeltop5 <- lm(MonthlyIncome ~ Age+DistanceFromHome+Education+EmployeeNumber+EnvironmentSatisfaction, data=full_data)

# Build a regression model based on top 1 PCs
income.modeltop1 <- lm(MonthlyIncome ~ Age, data=full_data)

#Summarize model
summary(income.modeltop5)
summary(income.modeltop1)

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2))
plot(income.modeltop5, which = c(1, 2))
plot(income.modeltop1, which = c(1, 2))
