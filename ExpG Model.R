### CREATING TRAINING AND TESTING DATA FRAMES

sample.size <- floor(0.7 * nrow(chances))

set.seed(101)
sample <- sample(seq_len(nrow(chances)), size = sample.size)

training <- chances[sample, ]
testing <- chances[-sample, ]

### CLEANING TRAINING DATA

  #exclude rows with own goals and penalties in icon column
training <- training[!training$icon=="owngoal" & !training$icon=="penawarded" & !training$icon=="Penalty Missed" & !training$icon=="Penalty Awarded",]

training$chanceRating <- ifelse(training$chanceRating=="-", "Penalty",
                                   ifelse(training$chanceRating=="fairlygoodchance", "Fairly Good",
                                          ifelse(training$chanceRating=="verygoodchance", "Very Good",
                                                 ifelse(training$chanceRating=="poorchance", "Poor",
                                                        ifelse(training$chanceRating=="greatchance", "Great", training$chanceRating)))))

  #exclude rows with penalties in chanceRating column
training <- training[!training$chanceRating=="Penalty",]

training$type <- ifelse(training$type=="Direct free kick", "Direct Free-Kick", 
                           ifelse(training$type=="Open play", "Open Play",
                                  ifelse(training$type=="Direct corner", "Direct Corner", training$type)))

  #exclude rows with dangerous moments (subjective measure of a chance ie. not a shot), penalties and direct corners in type column
training <-training[!training$type=="Dangerous Moment" & !training$type=="Penalty" & !training$type=="Direct Corner",]

training$primaryType <- ifelse(training$primaryType == "Shot/GK Save Rebound", "Shot (Opposition Rebound)",
                                  ifelse(training$primaryType == "Woodwork Rebound", "Shot (Woodwork Rebound)",
                                         ifelse(training$primaryType == "-", "No Assist", training$primaryType)))

training$icon <- ifelse(training$icon == "goal", "yes","no")

### TRANSFORMING X-AXIS

  #change negative co-ordinates to positive values by adding 136 to location_x
training$location_x <- c(as.numeric(training$location_x)) + 136

### CALCULATING SHOT DISTANCE

  #create function to calculate shot distance to centre of goal
findDistance<-function(x, y){
  x_off <- abs(136-x)
  y_off <- 0+y
    #pythagoras' threorem
  distance <- sqrt((x_off*x_off)+(y_off*y_off))
  return(distance)
}

###CALCULATING SHOT ANGLE

  #creates function to calculate shot angle to goal
findAngle<-function(x, y){
    #co-ordinates post to post
  d1 = 30
    #distance from ball to post 1
  d2 = findDistance(x-15, y)
    #distance from ball to post 2
  d3 = findDistance(x+15, y)
    #cosine rule
  angle = acos(((d2*d2)+(d3*d3)-(d1*d1))/(2*d2*d3))
    #convert radians to degrees
  angle <- (angle*180)/pi
  return(angle)
}

### CREATING VALUES FOR MODEL

  #divide shotDist by 4 to convert distance of co-ordinates to metres
training$shotDist <- findDistance(as.numeric(training$location_x), as.numeric(training$location_y))/4
training$shotAngle <- findAngle(as.numeric(training$location_x), as.numeric(training$location_y))

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
