#Assignment Activity 4: Visualise data to gather insights using R

###############################################################################

# The sales department of Turtle games prefers R to Python. 
# As part of data analysis in R, will explore 
# prepare the data set for analysis by utilizing basic statistics and plots. 

###############################################################################

# 1. Import the necessary package

# Import the tidyverse package.
library('tidyverse')

# Import the data set.
# You can choose how to import the CSV file.
turtle_sales <- read.csv(file.choose(), header=T)

# Set the working directory.
turtle_sales <- read.csv('turtle_sales.csv', header = TRUE)


# View the data frame.
head(turtle_sales)


################################################################################

# 2. Explore the data set

# Convert data frame to a tibble.
as_tibble(turtle_sales)


# Use the glimpse() function.
glimpse(turtle_sales)


# Use the summary() function.
summary(turtle_sales)

###############################################################################

# 3. Explore and Clean the data set

# Return a frequency table for the 'Genre' column.
table(turtle_sales$Platform)

## What we want to do:
# redundant columns (Ranking, Year, Genre, Publisher).
# Check why 'Product' is character.
# Convert 'Platform' to  (categorical variable).

# Remove  columns.
turtle_sales2 <- select(turtle_sales, -Ranking, -Year, -Genre, -Publisher )

# Check the new data frame.
head(turtle_sales2)

# View as a tibble.
as_tibble(turtle_sales2)

# View summary.
summary(turtle_sales2)

# Check the ageNum data.
summary(turtle_sales2$Platform)

###############################################################################

# 5. Visualise the data

# Explore the height data in a plot.
hist(turtle_sales2$Global_Sales,
          main="Frequency of Global Sales",
          xlab="Global Sales",
          col="darkmagenta")

# View top rows to see values.
head(turtle_sales2$Global_Sales)


High_Global_sale <- filter(turtle_sales2,
                           Global_Sales > 50)

head(High_Global_sale)

# Plot a bar chart by passing the x-variable and data source, then set the geom type:
qplot(NA_Sales, fill=Platform, data=turtle_sales2, geom='bar')
qplot(EU_Sales, fill=Platform, data=turtle_sales2, geom='bar')


qplot(Global_Sales, Platform, data=turtle_sales2, geom='boxplot')

#####################################################################

# To create a new element EU + NA sale. 
turtle_sales2 <- mutate(turtle_sales2, new_var=EU_Sales + NA_Sales)

# View first 10 rows of the data frame.
head(turtle_sales2, 10)
## new var, EU sale
qplot(Global_Sales, new_var, data=turtle_sales2, 
      ylab ="EU + NA Sale")

qplot(y=Global_Sales, data=turtle_sales2)

######Histogram.
# First pass the x-variable, then specify the data source. 
qplot(EU_Sales, data=turtle_sales2)

qplot(Global_Sales, bins=5, data=turtle_sales2)

#Global sales for different publishers
qplot(Year, Global_Sales, colour=Publisher, data=turtle_sales)

###############################################################################

# Data Wrangling and manipulation
# Determine Normality of data
# Plots and visualization on wrangled and manipulated data for insights
 
###############################################################################
# Install the necessary packages.
# Installing the tidyverse package is optional, as you've done this before.
# install.packages('tidyverse')
install.packages('skimr')
install.packages('DataExplorer') 

# Note that you only have to import the tidyverse package, as it consists of
# ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, and forcats. 
# However, for demonstration purposes, readr, dplyr and tidyr will be imported
# separately.

# The whole tidyverse package.
library(tidyverse)
# Import and read CSV file.
library(readr)
# Data wrangling.
library(dplyr)
# Data wrangling.
library(tidyr)
# Create statistical summaries.
library(skimr)
# Create a report as an HTML file.
library(DataExplorer)

# Find the min/max/mean sales per store:
# Specify the data source, set the margin for column,
# and add min/max/mean function.

apply(turtle_sales2, 2, min)
apply(turtle_sales2, 2, max)

# View the data frame.
head(turtle_sales2)

# Remove  non numeric columns.
turtle_sale_only <- select(turtle_sales2,-Product,-Platform)

# View the data frame.
head(turtle_sale_only)

apply(turtle_sale_only, 2, mean)

# Determine the impact on sales per product_id
aggregate(Global_Sales~Platform, turtle_sales2, sum)
aggregate(Global_Sales~Product, turtle_sales2, sum)
aggregate(Global_Sales~Product+Platform, turtle_sales2, sum)

# EU sale grouped by product
df_sale <- turtle_sales2 %>% group_by(Product, Platform) %>%
  summarise(sum_EU_Sale=sum(EU_Sales),
  .groups='drop') %>%
  arrange(desc(sum_EU_Sale))

# View the results.
df_sale

# Total Sale- global and NA/EU/Other
lapply(turtle_sale_only, sum)

# NA sale grouped by product
df_sale_NA <- turtle_sales2 %>% group_by(Product, Platform) %>%
  summarise(sum_NA_Sale=sum(NA_Sales),
            .groups='drop') %>%
  arrange(desc(sum_NA_Sale))

# View the results.
df_sale_NA

#Summary of sales
turtle_sale_only <- turtle_sale_only %>%
  rename("NA+EU_Sale" = "new_var" )

summary(turtle_sale_only)

# Boxplot to check quarterly range and skew
boxplot(turtle_sale_only)

# Histogram to check shape of data
plot(hist(turtle_sale_only$Global_Sales),
     main="Frequency of Global Sale values",
     xlab="Global Sale in million pounds",
     col="chocolate")

# compare data with normal distribution.
qqnorm(turtle_sale_only$Global_Sales)
qqline(turtle_sale_only$Global_Sales)

#Hypothesis test
shapiro.test(turtle_sale_only$Global_Sales)

#Check dimension
dim(turtle_sale_only)

#load library to check skewness
library(moments)
#install.packages("moments")

skewness(turtle_sale_only$Global_Sales)

#Kurtosis analysis
kurtosis(turtle_sale_only$Global_Sales)

# Determine the correlation for the whole data frame.
round (cor(turtle_sale_only),
       digits=2)

############# Exploring data with ggplot####
# Build a plot: Start with data, mapping and geom.
# Set the data source, add mapping elements.
# Import tidyverse.
library(tidyverse)

# Global sale grouped by Year
df_sale_plot <- turtle_sales %>% group_by(Year) %>%
  summarise(sum_Sale=sum(Global_Sales),
            .groups='drop') %>%
  arrange(desc(Year))

head(df_sale_plot)

#Scatterplot to indicate yearly global sale
ggplot (data = df_sale_plot, 
        mapping=aes(x = Year, y = sum_Sale)) +
  
  # Add a geom as point for scatterplot.
  # Set the colour to red.
  geom_point(color = 'red',
             # Set the alpha transparency to 0.5.
             alpha = 0.5,  
             # Set the point size to 1.5.
             size = 1.5)  +
    
  # Add the line-of-best-fit to the plot.
  geom_smooth(method = 'lm')

##Group variable relation

# Global sale grouped by Year and Genre
df_sale_plot1 <- turtle_sales %>% group_by(Year,Genre) %>%
  summarise(sum_Sale=sum(Global_Sales),
            .groups='drop') %>%
  arrange(desc(Year))

head(df_sale_plot1)

#Scatterplot to indicate yearly global sale by Genre
ggplot (data = df_sale_plot1, 
        mapping=aes(x = Year, y = sum_Sale,
        color = Genre)) +
  
  # Add a geom as point for scatterplot.
  # Set the colour to red.
  geom_point(color = 'red',
             # Set the alpha transparency to 0.5.
             alpha = 0.5,  
             # Set the point size to 1.5.
             size = 1.5)  +
  
  # Add the line-of-best-fit to the plot.
  #remove the confidence intervals
  geom_smooth(method = 'lm',
              se = FALSE,
              size = 1.5) +

# Add a scale layer for y.
scale_y_continuous(breaks = seq(0, 350, 50), "Global Sale") +

# Add labels for title, subtitle, and caption.
labs(title = "Relationship between yearly sale and Genre",
     subtitle = "Yearly sale from EU, NA and other region") 

###############################################################################

# This R script contains how to run
# regression analysis in R. 
#
#  - how to build a simple linear regression model in R
#  - how to perform log transformation on variables to improve model fit
#  - how to test the efficiency of regression models and make predictions.

###############################################################################

# Identify relationships between the data frame variables 
# Find a correlation.
cor(turtle_sale_only)

# Plot the relationship with base R graphics.
plot(turtle_sale_only$NA_Sales, turtle_sale_only$Global_Sales)
plot(turtle_sale_only$EU_Sales, turtle_sale_only$Global_Sales)

###############################################################################

# 4. Fit a simple linear regression model.

# Create a model with only one x variable.
model1 <- lm(Global_Sales~NA_Sales,
             data=turtle_sale_only)

# View the model.
model1

# View more outputs for the model - the full regression table.
summary(model1)

# Create a model with only one x variable.
model2 <- lm(Global_Sales~EU_Sales,
             data=turtle_sale_only)

# View the model.
model2

# View more outputs for the model - the full regression table.
summary(model2)

###############################################################################

# 5. Plot the model1.

# View residuals on a plot.
plot(model1$residuals)


# Plot the relationship with base R graphics.
plot(turtle_sale_only$NA_Sales, turtle_sale_only$Global_Sales)
coefficients(model1)


# Add line-of-best-fit.
abline(coefficients(model1))

###############################################################################

# 6. Create a log transformation.

# Complete a log transformation with dplyr's mutate() function.
turtle_sale_only <- mutate(turtle_sale_only, 
              logGlobal_Sales=log(Global_Sales))


# View new object with new variable.
head(turtle_sale_only)


# Create a new model using logIndex.
model_new <- lm(logGlobal_Sales~NA_Sales,
                data=turtle_sale_only)


# View full regression table.
summary(model_new)


# Plot the relationship between Global and NA sale.
plot(turtle_sale_only$NA_Sales, turtle_sale_only$logGlobal_Sales)


# Add a line-of-best fit to existing plot.
abline(coefficients(model_new))
###############################################################################

#Multiple linear Regression

###############################################################################

#Visualise the correlation

# Install the psych package.
install.packages('psych')

# Import the psych package.
library(psych)

turtle_sale_only <- select(turtle_sale_only, -logGlobal_Sales)

# Use the corPlot() function.
corPlot(turtle_sale_only, cex=2)

# Create a new object and 
# specify the lm function and the variables.
modela = lm(Global_Sales~NA_Sales+EU_Sales, data=turtle_sale_only)

# Print the summary statistics.
summary(modela)


# Load the new data file and view its structure.
Saletest <- read.csv(file.choose(), header=TRUE)

# View the data.
str(Saletest)

# Create a new object and specify  predict function.
predictTest = predict(modela, newdata=Saletest,
                      interval='confidence')

# Print the object.
predictTest
