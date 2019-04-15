# Script to run expected goals model

# Source data cleaning scripts and functions
source('~/GitHub/expected-goals-model/clean_stratadata.R')
source('~/GitHub/expected-goals-model/functions.R')

# Split data into training and testing data
split_data(chances)

# Log regression model
model <- glm(formula = icon~shotDist + shotAngle + type + primaryType + secondaryType + bodyPart + numDefPlayers + defPressure, family = binomial, data = training) 
prob <- predict(model, type = 'response')
