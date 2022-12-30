rm(list = ls())
library(tidyverse)
library(collections)
options(scipen = 999)

input <-
  read.table(file = 'C:/Data/Play_hard/Advent_of_code_2022/input_day19.txt', header = FALSE, sep = ",")
head(input)
n_bp <- nrow(input)
quality <- matrix(0, n_bp, 3)
total_time <- 24

for (bp in 1:n_bp) {#####################iterate through all blueprints of the input


cost <- str_extract_all(input[bp, 1], pattern = "\\d+")[[1]]
cost_ore <- as.numeric(cost[2])
cost_clay <- as.numeric(cost[3])
cost_obsidian <- as.numeric(cost[4:5])
cost_geode <- as.numeric(cost[6:7])
resources <- rep(0, 4) # ore, clay, obsidian, geodes
robots <- c(1, rep(0, 3)) # collecting, ore, clay, obsidian, geodes
max_cost_ore <-
  max(cost_ore, cost_clay, cost_obsidian[1], cost_geode[1])

#Plan:
#1. Create recursive function, that executes next possible moves
#2. When a minute starts on can either wait or (given the resources are there) commission a robot
#3. After the decision, existing robots add their resources for the minute
#4. After resources are added, the state will be updated to the next resources and the new robots
#5. If several steps are possible, we have to iterate through them. To do so, I will assign case numbers to all 5 scenarios
#Case 1: Wait - will always be explored
#Case 2: Commission ore robot - explored if resources suffice
#Case 3: Commission clay robot - explored if resources suffice
#Case 4: Commission obsidian robot - explored if resources suffice
#Case 5: Commission geode robot - explored if resources suffice
#The state will contain: current minute, resources, robots, best so far geode score
#Base case will be when no more minutes are left
#current minute will be the minute that starts in that moment. When current minute is 24, the robots can still collect material

#ideas for pruning the moves
#implemented - if you decide against a certain type of robot, do not consider it again until after another type of robot was bought
#implemented - if the robots of a type are equal to the maximum cost in that currency, we don't need any more robots of this type
#implemented - add another base case which is, if the highscore cannot be beat even if every minute, we built a geode robot in every remaining move
#implemented - building a robot in the last minute does not make sense
#implemented - building a robot other than a geode robot in the previous to last minute does not make sense

robot_game <-
  function(current_minute,
           resources,
           robots,
           highscore,
           useless_moves) {
    cat("Starting function call for minute", current_minute, "with resources", resources, "and robots", robots, "\n Useless moves are set to", useless_moves, "\n")
    # check for base cases
    current_geodes <- resources[4]
    potential_geodes <-
      current_geodes + (robots[4] * ( total_time - current_minute+1)) + sum(seq(total_time - current_minute+1) - 1)
    if (current_minute > total_time) { # if time has run out
      cat("Reached base case with highscore of", current_geodes, "at start of minute", current_minute, " with resources", resources, "and robots",  robots, "\n\n")
       if (current_geodes > highscore) {
        highscore <- current_geodes
       }
      return(list(current_minute, resources, robots, highscore))
    } else if (potential_geodes < highscore) {
      # second type of base case in which the current highscore cannot be beat anymore
      cat("Reached base case. Leave current path because highscore cannot be beat anymore. \n\n")
      return(list(current_minute, resources, robots, highscore))
    } else if ((robots[2] == 0 & useless_moves[3] == TRUE) | (robots[3] == 0 & useless_moves[4] == TRUE) | (robots[4] == 0 & useless_moves[5] == TRUE)) { # base case if a robot is rendered a useless purchase, but there aren't jet any of that type
      cat("Reached base case because a needed resource will never be produced \n\n")
      return(list(current_minute, resources, robots, highscore))
      } else {
      # check possible next moves for their availability
      moves <- rep(FALSE, 5)
      moves[1] <- TRUE # move "wait" will always be an option
      if (current_minute <= total_time - 2) {
        if ((resources[1] >= cost_ore[[1]]) & (robots[1] < max_cost_ore) & (useless_moves[2] == FALSE)) {
          moves[2] <- TRUE
        }
        if ((resources[1] >= cost_clay[1]) &
            (robots[2] < cost_obsidian[2]) & (useless_moves[3] == FALSE)) {
          moves[3] <- TRUE
        }
        if ((resources[1] >= cost_obsidian[1]) &
            (resources[2] >= cost_obsidian[2]) & (robots[3] < cost_geode[2]) &
            (useless_moves[4] == FALSE)) {
          moves[4] <- TRUE
        }
      }
      if (current_minute <= total_time - 1) {
        if ((resources[1] >= cost_geode[1]) &
            (resources[3] >= cost_geode[2]) & (useless_moves[5] == FALSE)) {
          moves[5] <- TRUE
        }
      }
      cat("No base case reached. Possible next moves are:", moves, "\n")
      # existing robots add their resources
      resources <- resources + robots
      # update state for the different cases
       for (m in 1:5) {
         if (moves[m] == TRUE) {
          cat("Resources after minute", current_minute, "mining but before robot commissioning of that minute:", resources, "Robots before commissioning:", robots, "\n")
          #update resources and robots for the different cases
           next_resources <- resources
           next_robots <- robots
          if (m==1) {
            useless_moves <- useless_moves | moves
          } else if (m == 2) {
            next_resources[1] <- next_resources[1] - cost_ore
            next_robots[1] <- next_robots[1] + 1
            useless_moves <- rep(FALSE, 5)
          } else if (m == 3) {
            next_resources[1] <- next_resources[1] - cost_clay
            next_robots[2] <- next_robots[2] + 1
            useless_moves <- rep(FALSE, 5)
          } else if (m == 4) {
            next_resources[1] <- next_resources[1] - cost_obsidian[1]
            next_resources[2] <- next_resources[2] - cost_obsidian[2]
            next_robots[3] <- next_robots[3] + 1
            useless_moves <- rep(FALSE, 5)
          } else if (m == 5) {
            next_resources[1] <- next_resources[1] - cost_geode[1]
            next_resources[3] <- next_resources[3] - cost_geode[2]
            next_robots[4] <- next_robots[4] + 1
            useless_moves <- rep(FALSE, 5)
          }
          next_time <- current_minute + 1
          cat("We executed move", m, "during minute", current_minute, ". Resources are now", next_resources, "and robots are", next_robots, "\n")
          output <-
            robot_game(next_time, next_resources, next_robots, highscore, useless_moves)
          cat("Finished recursing over all moves following move", m, "in minute", current_minute, ". Resources are:", resources, ". Moving on to the next move for this minute if there is any.\n")
          highscore <- output[[4]]
          
        }
      }
    }
    return(output)
  }



start_minute <- 1
highscore <- 0
useless_moves <- rep(FALSE, 5)
main_output <- robot_game(start_minute, resources, robots, highscore, useless_moves)
highscore <- main_output[[4]]

quality[bp,1] <- highscore
quality[bp,2] <- highscore * bp
cat("Finished bp", bp, "\n")
}

sum(quality[,2])
