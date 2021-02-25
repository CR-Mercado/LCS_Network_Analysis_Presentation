#' 0 - Data Simulation for LCS
#' 
#' Problem: A country has identified signs of widespread impact to 
#' child development, possibly a disease or toxic substance outbreak.

# Data will be at the child level and consist of
 #' Hospital where case was identified
 #' Child ID 
 #' Age
 #' Severity
 #' Last (up to 5) City, States of Travel 
 #' in form City_State_1, City_State_2, ... City_State_5

# Generating synthetic random data

set.seed(4) # reproducible randomization 

 # child id will be 1,000 random combinations of 5 letters and 5 numbers 
 
child_id <- NULL
for(i in 1:1000){ 
  child_id <- c(child_id, 
                paste0(sample(x = LETTERS, size = 5, replace = FALSE),
                       sample(0:9, 5),
                       collapse = "")
                )
}

 # age will be skewed, truncated, normal distributed between 1-12,
  # with an average of 6.

age <- abs ( rpois(n = 1000, lambda =  6) ) + 1
age[age > 12] <- 1

 # Severity will be poisson distributed with lambda = 1 and minimum of 1.
   # values greater than 3, made to be 1.

severity <- rpois(1000, 1) + 1
severity[severity > 3] <- 1
  
# Generate a data frame to hold the data 

outbreak_tbl <- data.frame(
  child_id = child_id, 
  age = age, 
  severity = severity
)

 # City, State will be randomized at the child level to result in between 2-5
 # cities per child. Only 10 cities across 3 states made available for simplicity.
 # a child will get a random number of locations, and then draw from these cities
 # such that the first few are disproportionately common. 

city_states <- c("Los Angeles, California", 
                 "San Diego, California",
                 "San Francisco, California",
                 "San Jose, California",
                 "Phoenix, Arizona",
                 "Las Vegas, Nevada",
                 "Houston, Texas",
                 "Dallas, Texas",
                 "New York City, New York",
                 "Washington, District of Columbia")

child_travel_history <- data.frame(
  child_id = child_id,
  city_state_1 = NA,
  city_state_2 = NA,
  city_state_3 = NA,
  city_state_4 = NA,
  city_state_5 = NA
)

# randomly select between 2-5 cities for a child to have visited
 # if > 5, make 2.
num_cities <- rpois(n = 1000, lambda = 2) + 2
num_cities[num_cities > 5] <- 2

# get a random distribution of cities to have visited 
city_number <- rpois(n = 1000, lambda = 3) + 1
city_number[city_number > 10] <- 1
city_probabilites <- table(city_number)/1000

# for each child
for(i in 1:1000){
  
  # prep 5 maximum cities   
  temp_cities <- rep(NA, 5)  
  
  # get both a random number of cities N and N random cities. 
  selected_cities <- sample(city_states, size = num_cities[i],
         replace = FALSE,
         prob = city_probabilites)
  
  # fill in the number of cities and assign it to that child's row 
  temp_cities[1:length(selected_cities)] <- selected_cities
  
  child_travel_history[i, 2:6] <- temp_cities
  
  }

child_outbreak_tbl <- merge(outbreak_tbl, child_travel_history, by = "child_id")

write.csv(child_outbreak_tbl, "child_outbreak_simulation.csv", row.names = FALSE)  
