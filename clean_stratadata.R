# Script to clean StrataBet data

# Read in files and functions
source('~/GitHub/expected-goals-model/read_stratadata.R')
source('~/GitHub/expected-goals-model/functions.R')

library(tidyverse)

# Remove rows containing penalties, own goals or wining penalties
chances <- chances %>% filter(!icon %in% c('owngoal', 'penawarded', 'Penalty Missed', 'Penalty Awarded'))
chances <- chances %>% filter(!type %in% c('Dangerous Moment', 'Penalty', 'Direct Corner'))
chances <- chances %>% filter(!chanceRating %in% c('Penalty', '-'))

# Apply rename_variables function to columns
chances$chanceRating <- sapply(chances$chanceRating, rename_variables)
chances$type <- sapply(chances$type, rename_variables)
chances$primaryType <- sapply(chances$primaryType, rename_variables)

# Create yes or no goal column
chances$icon <- ifelse(chances$icon == 'goal', 1,0)

# Convert location to numeric
chances$location_x <- as.numeric(chances$location_x)
chances$location_y <- as.numeric(chances$location_y)

# Transform x-axis
# Co-ordinates = 0 to 272
chances$location_x <- (chances$location_x + 136)

# Divide shotDist by 4 to convert distance of co-ordinates to metres
chances$shotDist <- find_distance(chances$location_x, chances$location_y)/4
chances$shotAngle <- find_angle(chances$location_x, chances$location_y)
