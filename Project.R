# Install packages if needed
#install.packages(c('tidyverse', 'lubridate', 'GGally', 'emmeans', 'ggplot2','Hmisc'))
# Load the necessary packages
library(tidyverse)
library(lubridate)
library(GGally)
library (emmeans)
library(ggplot2)
library(Hmisc)

# Read in the dataset
avocado <- read.csv(file.choose(), header = TRUE)

# Making copy of original data
df <- avocado

# avocado <- df
attach(avocado)

# View the first few rows of the dataset
head(avocado)

# Check the structure of the dataset
str(avocado)

# Summary statistics
summary(avocado[, c("AveragePrice", "Total.Volume", "X4046", "X4225", "X4770",
                         "Total.Bags", "Small.Bags","Large.Bags", "XLarge.Bags",
                         "type", "year", "region")])

# Converting date column from char to posixct
avocado$Date <- parse_date_time(avocado$Date, orders = c("%m/%d/%Y", "%Y-%m-%d"))


# Check for null values in all columns
colSums(is.na(avocado))


# Histogram of avocado prices
ggplot(avocado, aes(x = AveragePrice)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.5) +
  ggtitle("Distribution of Average Avocado Prices")


# Box plot of avocado prices by type
ggplot(avocado, aes(x = type, y = AveragePrice)) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  ggtitle("Average Avocado Prices by Type")


# Time series plot of avocado prices by year
avocado %>%
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d")) %>%
  ggplot(aes(x = Date, y = AveragePrice, color = factor(year))) +
  geom_line(size = 1) +
  labs(title = "Average Avocado Prices Over Time", x = "Date", y = "Average Price")


# Scatterplot of avocado prices and volume
ggplot(avocado, aes(x = Total.Volume, y = AveragePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Avocado Prices vs. Volume", x = "Total Volume", y = "Average Price")

# Price trends by type and region
avocado %>%
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d")) %>%
  ggplot(aes(x = Date, y = AveragePrice, color = type)) +
  geom_line() +
  facet_wrap(~region, ncol = 6, scales = "free_x") +
  labs(title = "Avocado Prices by Region and Type", x = "Date", y = "Average Price")

# Check size difference by price and or region
avocado %>%
  ggplot(aes(x = 4046, y = AveragePrice, color = region)) +
  geom_point() +
  labs(title = "Scatterplot of 4046 vs. Average Price by Region")

avocado %>%
  ggplot(aes(x = 4225, y = AveragePrice, color = region)) +
  geom_point() +
  labs(title = "Scatterplot of 4225 vs. Average Price by Region")

avocado %>%
  ggplot(aes(x = 4770, y = AveragePrice, color = region)) +
  geom_point() +
  labs(title = "Scatterplot of 4770 vs. Average Price by Region")


# If you want to compare specific regions with different sized avocados and avg price
ggpairs(avocado[avocado$region %in% c("GreatLakes", "NorthernNewEngland", "SouthCentral",
                                                 "Midsouth", "Southeast", "West", "Northeast"), 
                     c("X4046", "X4225", "X4770", "AveragePrice", "region")],
        lower = list(continuous = "points"), 
        upper = list(combo = "box"), 
        axisLabels = "show",
        title = "Scatterplot Matrix of Avocado Data")

# T-test for the average price of organic and conventional avocados
t.test(AveragePrice ~ type, data = avocado, subset = type %in% c("organic", "conventional"))

# ANOVA to test for differences in average price among regions
anova <- aov(AveragePrice ~ region, data = avocado)
summary(anova)

# ANOVA to test for differences in average price among years
avo_anova <- aov(AveragePrice ~ year, data = avocado)
summary(avo_anova)

# Chi-square test for association between region and type
table <- table(avocado$type, avocado$region)
chisq.test(table)

# Select relevant columns
vol_cols <- c("X4046", "X4225", "X4770")
price_col <- "AveragePrice"
vol_price_data <- avocado[, c(vol_cols, price_col)]

# Calculate Pearson correlation coefficients
correlations <- cor(vol_price_data, method = "pearson")

# View correlation matrix
correlations

ggpairs(avocado[, c(vol_cols, price_col)],
        lower = list(continuous = "points"), 
        upper = list(combo = "box"), 
        axisLabels = "show",
        title = "Scatterplot Matrix of Avocado Volume and Price Data")







###LINEAR MODELS
#Linear model to compare avg price and total bags sold
lm_model <- lm(AveragePrice ~ Total.Bags, data = avocado)
summary(lm_model)

ggplot(avocado, aes(x = Total.Bags, y = AveragePrice)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Average Price and Total Bags Sold",
       x = "Total Bags Sold", y = "Average Price")


#Linear model to compare avg price and total volume
lm_model <- lm(AveragePrice ~ Total.Volume, data = avocado)
summary(lm_model)

ggplot(avocado, aes(x = Total.Volume, y = AveragePrice)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Average Price and Total Volume",
       x = "Total Volume", y = "Average Price")

#Linear model to compare avg price and year
model_year <- lm(AveragePrice ~ year, data = avocado)
summary(model_year)

ggplot(avocado, aes(x = year, y = AveragePrice)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Average Price and Year",
       x = "Year", y = "Average Price")




###MULTIPLE LINEAR MODELS
#Are there significant differences in average prices between different regions, 
#after controlling for the type of avocado (conventional or organic) and the year?

# Fit multiple linear regression model
model <- lm(AveragePrice ~ type*region*year, data = avocado)

# Test for overall significance of the model
summary(model)

# Test for significance of individual coefficients
summary(model)$coefficients

# Test for interaction effect between type and region
summary(model)$coefficients["typeorganic:regionWest", c("Estimate", "Pr(>|t|)")]

# Get predicted average prices for each region, holding type and year constant
emmeans(model, ~ region, at = list(type = c("conventional", "organic"), year = mean(avocado$year)))

# Plot predicted average prices by region, holding type and year constant
ggplot(avocado, aes(x = region, y = AveragePrice, color = type)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar") +
  stat_summary(fun.data = mean_cl_normal, geom = "point", size = 2) +
  facet_wrap(~ year, ncol = 2) +
  labs(title = "Average Prices by Region, Type, and Year",
       x = "Region", y = "Average Price")


# Preprocess the data (convert categorical variables to numeric)
avocado$region <- as.numeric(factor(avocado$region))
avocado$year <- as.numeric(factor(avocado$year))
avocado$type <- ifelse(avocado$type == "conventional", 0, 1)

# Select the features and target variable
features <- c("Total.Volume", "X4046", "X4225", "X4770", "Total.Bags", "Small.Bags", "Large.Bags", "XLarge.Bags", "type", "year", "region")
X <- data[features]
y <- data["AveragePrice"]

# Train the multiple linear regression model
model <- lm(AveragePrice ~ X , data = avocado)

# Evaluate the model using summary statistics
summary(model)

model <- lm(AveragePrice ~ X4046 + X4225 + X4770, data = avocado)
summary(model)








###LOGISTIC MODELS (BAD)
# Convert the target variable to a binary variable
average_price <- mean(avocado$AveragePrice)
avocado$HighPrice <- ifelse(avocado$AveragePrice > average_price, 1, 0)

# Select the features and target variable
features <- c("Total.Volume", "X4046", "X4225", "X4770", "Total.Bags", "Small.Bags", "Large.Bags", "XLarge.Bags")
X <- data[features]
y <- data["HighPrice"]

# Train the logistic regression model
model <- glm(HighPrice ~ X, family = binomial(link = "logit"), data = avocado)
summary (model)

model <- glm(HighPrice ~ year, family = binomial(link = "logit"), data = avocado)
summary (model)
