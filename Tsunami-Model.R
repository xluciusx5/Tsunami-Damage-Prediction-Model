# Prerequisite
# install.packages("RColorBrewer") # Please install this package for correct visualization
library(RColorBrewer)

# Import tsunami dataset (in the .zip file)
tsunami = read.csv(file="tsunami_dataset.csv") 

tsunamiMagnitude = tsunami[!is.na(tsunami$EQ_MAGNITUDE),]; # Creating a new dataset without NAs in magnitude
tsunamiMagnitude2 = tsunamiMagnitude$EQ_MAGNITUDE; # Extracting cleaned magnitudes col
summary(tsunamiMagnitude2); sd(tsunamiMagnitude2) # Extracting mean and sd to generate our RGN magnitudes

# ---------------------------
# VARIABLE 1 - RGN MAGNITUDE
# Tsunami RGN magnitude function # Discrete numerical

RGNMagnitudes = function(n) rnorm(n, mean = 7.046676, sd = 0.7961247); # Normal distribution
    
    # Summary
    summary(RGNMagnitudes(100))
    hist(RGNMagnitudes(100), main = "RGN Magnitudes (100 Steps)", col= c("#f94144", "#ffe8d6", "#d8e2dc"))

# ----------------------------
# VARIABLE 2 - RGN WATER DEPTH
# Water depth function (generate n water depth values with range 1000 to 6000 meters) # Discrete numerical

waterDepth = function(n) round(runif(n , min = 1000 , max = 6000)) # Uniform distribution

    # Stats and plot 100 steps
    summary(waterDepth(100))
    sd(waterDepth(100)) 
    hist(waterDepth(100), col = brewer.pal(n = 9, name = "Blues"),
         xlab = "Depth", ylab = "Occurences",
         main = "Water depth of tsunami area (100 Steps)")

# -------------------------
# VARIABLE 3 - RGN DISTANCE
# Tsunami RGN distance to shore # Discrete numerical
    
tsunamiDistance = function(n) round(runif(n, min = 100 , max = 10000)); # Uniform distribution
  
    # Stats and plot 100 steps
    summary(tsunamiDistance(100))
    hist(tsunamiDistance(100), col= c("#AA4371", "#ffffff", "#333333"),
         ylab = "Occurences",
         xlab = "Distance to shore",
         main = "Distance to shore in meters (100 Steps)")

# ------------------------------
# VARIABLE 4 - RGN LUCK

# Luck - Second wave # Categorical
# Yes (1): No more than one wave
#  No (0): A second wave hits for RGN in range 0.5 - 1.2

# The function simply generates a number between 0 and 1 

luck = function(n) rbinom(n, size = 1, prob = 0.5); # Binomial distribution

# --------------------------------------- #
# Full model - Damage prediction function #
# --------------------------------------- #

# The function takes magnitude, water depth, distance and luck as parameters
# The returnDmg final parameter eliminates the string output and only returns the final damage value
    
damagePrediction = function(magnitude, waterDepth, distance, luck, returnDmg = FALSE)
{

  # ------------------------------------------------------------
  # Return damage conditionals (quick functions to return damage)
  # See after for detailed comments
  
  if(luck == 0 && returnDmg == TRUE) {
    g = 9.8
    tsunamiVelocity = sqrt(waterDepth * g)
    destructivePower = (tsunamiVelocity * distance * magnitude) / 10000; destructivePower
    secondWaveStrength = luck(1);
    damage = destructivePower / runif(1, min = 50 , max = 65);
    damage = damage + (secondWaveStrength * damage)
    return(damage)
  }
  if(luck == 1 && returnDmg == TRUE) {
    g = 9.8
    tsunamiVelocity = sqrt(waterDepth * g)
    destructivePower = (tsunamiVelocity * distance * magnitude) / 10000; destructivePower
    damage = destructivePower / runif(1, min = 50 , max = 65);
    return(damage)
  }
  
  # ------------------------
  # Tsunami magnitude string output
  print(paste0("The tsunami magnitude is ", format(round(magnitude, 1), nsmall = 1), ""))
  
  # ------------- RGN VELOCITY
  # Tsunami RGN velocity # Continuous numerical
  
  g = 9.8 # Earth's gravity in m/s^2 
  tsunamiVelocity = sqrt(waterDepth * g); # Actual tsunami velocity formula
  
  # String output
  print(paste0("Its velocity is ", format(round(tsunamiVelocity, 4), nsmall = 4), " m/s (", waterDepth, " m * 9.8 m/s)"))
  
  # ------------- RGN DESTRUCTIVE POWER
  # Destructive power RGN # Continuous numerical
  
  # Destructive power is generated from the product of velocity, distance, and magnitude variables / 10000

  # Scale
  # Weak (350) - Slightly damaging (650) - Destructive (950) - Devastating (1250)
  destructivePower = (tsunamiVelocity * distance * magnitude) / 10000;

  # Destructive power output
  if(destructivePower >= 1250) {
    print(paste0("The destructive power is ", format(round(destructivePower, 2), nsmall = 2), " (devastating - higher than 1250)."))
  } else if (destructivePower >= 950) {
    print(paste0("The destructive power is ", format(round(destructivePower, 2), nsmall = 2), " (destructive - 950 to 1249)."))
  } else if (destructivePower >= 350) {
    print(paste0("The destructive power is ", format(round(destructivePower, 2), nsmall = 2), " (slightly damaging - 350 to 949)."))
  } else {
    print(paste0("The destructive power is ", format(round(destructivePower, 2), nsmall = 2), " (weak - 0 to 349)."))
  }
  
  # Use print(summary(destructivePower to see the summary))
  summary(destructivePower)
  
  # Distance output
  print(paste0("The tsunami distance from the shore is ", distance, " m, or ", format(round((distance / 1000), 2), nsmall = 2), " km"))
  
  # ------------- PREDICTED DAMAGE (GBP)
  # We developed this formula to predict the most accurate real-life amount of damage expressed in millions (GBP)
  damage = destructivePower / runif(1, min = 50 , max = 65);

    # LUCK AFFECTING CHANCE OF SECOND WAVE
    # This statement determines whether a second wave is going to hit
    # If the luck value is 0, a hitting second wave will have 0.5 up to 1.2 additional damage
    if(luck == 0){
      secondWaveStrength = runif(1 , min = 0.5 , max = 1.2);
      damage = damage + (secondWaveStrength * damage)
      print(paste0("A second wave has hit. The damage was ", format(round(secondWaveStrength, 2), nsmall = 2), " times higher."))
    } else {
      print("No second wave hit. Luck was on your side.")
    }
  
  # Predicted damage string output
  print(paste0("With the above variables, ", format(round(damage, 2), nsmall = 2), " Millions in damage (GBP) were predicted"))
}

# FINAL RGN OUTPUT
# Output randomly generated simulation.
damagePrediction(RGNMagnitudes(1), waterDepth(1), tsunamiDistance(1), luck(1), FALSE)

# ------------------------------------------------------------------------------------------
# To predict the amount of damage of a determined tsunami for 1000 steps, use the code below

# Initialize empty array to contain damage values
damageArray = c()

# Loop model for 1000 steps
for(n in 0:1000) {
  # ------------------------
  # GENERATE YOUR OWN TSUNAMI
  damageArray[n] <- damagePrediction(magnitude = 7, waterDepth = 10000, distance = 6000, luck = 1, returnDmg = TRUE)
}

# Damage array values
# damageArray

# Summary and histogram of 1000 steps
summary(damageArray)
hist(damageArray, main = "Tsunami Full model (1000 Steps) \n Specific tsunami",
     col = brewer.pal(n = 9, name = "BuPu"),
     xlab = "Damage (in Millions)")
