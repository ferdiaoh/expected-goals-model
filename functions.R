# Script with functions to clean and run expected goals model

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
         as.character(x))
}


# Split data into training and testing data
split_data <- function(x) {
  sample.size <- floor(0.7 * nrow(x))
  set.seed(1288)
  sample <- sample(seq_len(nrow(x)), size = sample.size)
  training <<- x[sample, ]
  testing <<- x[-sample, ]
}


# Calculate shot distance
find_distance <- function(x, y) {
  x_off <- abs(136-x)
  y_off <- 0+y
  # Pythagoras' threorem
  distance <- sqrt((x_off*x_off)+(y_off*y_off))
  return(distance)
}


# Calculate shot angle
find_angle <- function(x, y) {
  # Co-ordinates post to post
  d1 = 30
  # Distance from ball to post 1
  d2 = find_distance(x-15, y)
  # Distance from ball to post 2
  d3 = find_distance(x+15, y)
  # Cosine rule
  angle = acos(((d2*d2)+(d3*d3)-(d1*d1))/(2*d2*d3))
  # Convert radians to degrees
  angle <- (angle*180)/pi
  return(angle)
}
