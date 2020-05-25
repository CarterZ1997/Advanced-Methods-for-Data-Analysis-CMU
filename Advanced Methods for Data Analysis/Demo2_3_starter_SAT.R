### Demo_2_3_starter: SAT Data ###
# First part of In-Class R Demo

###############################
# Read in the data
###############################
data = read.table("CASE1201.ASC", header = TRUE)
dim(data)
head(data)
attach(data)

###############################
# Exploratory Data Analysis
###############################

# Histograms
par(mfrow = c(2, 4))
hist(sat, main = "Histogram of SAT Scores", xlab = "Mean SAT Score", col = 1)
hist(takers, main = "Histogram of Takers", xlab = "Percentage of students tested", col = 2)
hist(income, main = "Histogram of Income", xlab = "Mean Household Income ($100s)", col = 3)
hist(years, main = "Histogram of Years", xlab = "Mean Years of Sciences and Humanities", col = 4)
hist(public, main = "Public Schools Percentage", xlab = "Percentage of Students in Public Schools", col = 5)
hist(expend, main = "Histogram of Expenditures", xlab = "Schooling Expenditures per Student ($100s)", col = 6)
hist(rank, main = "Histogram of Class Rank", xlab = "Median Class Ranking Percentile", col = 7)

# Check outlier
data[which(expend == max(expend)), ]

# Look at the data together
par(mfrow = c(1, 1))
plot(data[,-1]) #scatterplot matrix of ’data’, ignoring the first column.
round(cor(data[,-1]), 2)

# Rest of demo in-class (R code not included here in 'starter' code)

# Fit a full regression line to all the variables
# Check residuals
# Reduced model
# Sort SAT scores after adjusting for 'takers' and 'rank'
# Check residuals of reduced model
# Improve residuals and linear fit by transforming variables (such as using a log-transformation)