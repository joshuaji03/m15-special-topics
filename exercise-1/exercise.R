# Exercise-1
# Implement code from this book chapter: http://r4ds.had.co.nz/many-models.html

# Packages
install.packages('modelr')
install.packages('tidyverse')
install.packages('gapminder')
library(gapminder)
library(modelr)
library(tidyverse)
library(ggplot2)

# Initial view of the data with ggplot
gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) + geom_line(alpha = 1/3)

# Look only at new zealand
nz <- filter(gapminder, country == "New Zealand")
nz %>% 
  ggplot(aes(year, lifeExp)) + geom_line() + ggtitle("Full data = ")
nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>%
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) +
  geom_line() +
  ggtitle("Linear trend +")


# Better yet, write your own function to accept a country as a parameter,
# and produce the same graphics

chn <- filter(gapminder, country == "China")
chn %>% 
  ggplot(aes(year, lifeExp)) + geom_line() + ggtitle("Full data = ")
chn_mod <- lm(lifeExp ~ year, data = chn)
chn %>%
  add_predictions(chn_mod) %>%
  ggplot(aes(year, pred)) +
  geom_line() +
  ggtitle("Linear trend +")

# Nest the data by country/continent
by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest()

# Define a statistical model, and store it in a function
country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

# Use the `map` functionality to run the same model for each country separately


# Add additional columns to store your residuals (distance between data and prediction)


# Unnest your residual


# Plot the residuals



# Plot residuals by continent


# Use `glance` to look at model quality


# Compare model quality to continent


# View country that have an r.squared value of less than .25

