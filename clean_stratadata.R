# Clean StrataBet data

# Read in files
source('~/GitHub/expected-goals-model/read_stratadata.R')

library(tidyverse)

# Remove rows containing penalties, own goals or wining penalties
chances <- chances %>%
  filter(icon != 'owngoal' &
           icon != 'penawarded' &
           icon != 'Penalty Missed' &
           icon != 'Penalty Awarded' &
           type != 'Dangerous Moment' &
           type != 'Penalty' &
           type != 'Direct Corner' &
           chanceRating != 'Penalty' &
           chanceRating != '-')

# Create function to standardise variable names
rename_variables <- function(x) {
  switch(x,
         poorchance = 'Poor',
         fairlygoodchance = 'Fairly Good',
         goodchance = 'Good',
         verygoodchance = 'Very Good',
         greatchance = 'Great',
         superbchance = 'Superb',
         `Direct free kick` = 'Direct Free-Kick',
         `Open play` = 'Open Play',
         `Direct corner` = 'Direct Corner',
         `Shot/GK Save Rebound` = 'Shot (Opposition Rebound)',
         `Woodwork Rebound` = 'Shot (Woodwork Rebound)',
         `-` = 'No Assist',
         as.character(x)
  )
}

# Apply rename_variables function to columns
chances$chanceRating <- sapply(chances$chanceRating, rename_variables)
chances$type <- sapply(chances$type, rename_variables)
chances$primaryType <- sapply(chances$primaryType, rename_variables)

# Create yes or no goal column
training$icon <- ifelse(training$icon == "goal", "yes","no")

# Transform x-axis
# Co-ordinates = 0 to 272
training$location_x <- c(as.numeric(training$location_x)) + 136

# Calculate shot distance
find_distance <- function(x, y){
  x_off <- abs(136-x)
  y_off <- 0+y
    # Pythagoras' threorem
  distance <- sqrt((x_off*x_off)+(y_off*y_off))
  return(distance)
}

# Calculate shot angle
find_angle <- function(x, y){
    # Co-ordinates post to post
  d1 = 30
    # Distance from ball to post 1
  d2 = findDistance(x-15, y)
    # Distance from ball to post 2
  d3 = findDistance(x+15, y)
    # Cosine rule
  angle = acos(((d2*d2)+(d3*d3)-(d1*d1))/(2*d2*d3))
    # Convert radians to degrees
  angle <- (angle*180)/pi
  return(angle)
}

### CREATING VALUES FOR MODEL

  #divide shotDist by 4 to convert distance of co-ordinates to metres
training$shotDist <- find_distance(as.numeric(training$location_x), as.numeric(training$location_y))/4
training$shotAngle <- find_angle(as.numeric(training$location_x), as.numeric(training$location_y))

  #change class of data for model
training$icon <- as.factor(training$icon)
training$chanceRating <- as.factor(training$chanceRating)
training$type <- as.factor(training$type)
training$primaryType <- as.factor(training$primaryType)
training$secondaryType <- as.factor(training$secondaryType)
training$bodyPart <- as.factor(training$bodyPart)
training$numDefPlayers <- as.numeric(training$numDefPlayers)
training$defPressure <- as.numeric(training$defPressure)

### LOG REGRESSION MODEL

model <- glm(formula = icon~shotDist + shotAngle + type + primaryType + secondaryType + bodyPart + numDefPlayers + defPressure, family = binomial, data = training) 

prob <- predict(model, type = "response")
