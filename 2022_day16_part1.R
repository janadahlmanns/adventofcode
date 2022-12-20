rm(list = ls())
library(tidyverse)
options(scipen = 999)

input <-
  read.table(file = 'C:/Data/Play_hard/Advent_of_code_2022/input_day16_ex.txt', header = FALSE, sep = "\n")
total_time <- 30
#head(input)
#nrow(input)

#Plan: go through all possible paths in the caves
# calculate pressure release along this path
# when time runs out, check if it a higher pressure release than the previous maximum, if so keep as maximum to compare to, if not, do nothing


pattern_valves <- "[:upper:]{2}"
pattern_flow <- "(?<=rate=)\\d+"

data <- data.frame(matrix(NA, nrow(input), 3))
neighbors_names <- list() # list of all neighbors of a cave as "names"
neighbors <- list() # list of all neighbors of a cave as numbers
for (i in 1:nrow(input)) {
  valves <- str_extract_all(input[i,1], pattern_valves)
  flow <- str_extract_all(input[i,1], pattern_flow)
  data[i,1] <- valves[[1]][1]
  data[i,2] <- as.numeric(flow)
  neighbors_names[[i]] <- valves[[1]][2:length(valves[[1]])]
}
for (i in 1:nrow(data)) {
  neighbors[[i]] <- list()
  for (n in 1:length(neighbors_names[[i]])) {
    neigh <- which(data$X1 == neighbors_names[[i]][n])
    neighbors[[i]][n] <- neigh
  }
}



takesteps <- function(n, path, ticker, valve_status, n_step) {
  #cat("Starting function call with path", path, ", and n=", n, "ticker = ", ticker, "\n")
  #open valve if there is enough time and if it isn't already open
  if ((ticker[n_step] < (total_time-1)) & (valve_status[n,1] == 0)) {
    # set valve status to open and increase time ticker by one
    valve_status[n,1] <- 1
    valve_status[n,2] <- ticker[n_step]+1
    ticker[n_step] <- ticker[n_step]+1
    #cat("Opened valve", n, "increased ticker to", ticker, "\n")
    }
  # check if it is a base case
  if ((ticker[n_step] >= total_time) | (sum(valve_status[,1]) == n_valves)) { # base case when time runs out or when all valves are open
    #cat ("Reached base case with path:", path, "\n")
    #calculate pressure release value
    pressure_release <- 0
    for (v in 1:n_valves) {
      pressure_release <- pressure_release + (valve_status[v,1]*(total_time-valve_status[v,2])*data[v,2])
    }
    #if this pressure release is higher than previous maximum replace paths_taken. If not, do nothing and move on.
    if (pressure_release > max_pressure_release) {
      max_pressure_release <<- pressure_release
      paths_taken[[1]]<<- path
      paths_taken[[2]]<<- valve_status
      ticker_taken[[1]] <<- ticker
      cat ("Reached base case with higher pressure release of", pressure_release, "with path:", path, "\n")
    }
    return(list(n, path, ticker, valve_status, n_step))
    
  } else {
    for (i in 1:data[n,3]) {
      #cat("Begin execution of for statement with path", path, " and n=", n, " (i=", i, "). Ticker =", ticker, " \n")
      next_n <- neighbors[[n]][[i]]
      next_step <- n_step+1
      next_path <- path
      next_path[n_step+1] <- next_n
      next_ticker <- ticker
      next_ticker[n_step+1] <- ticker[n_step] + 1 
      
      #cat("No base case -> Stepping into recursion with path:", next_path, ", and n=", next_n, ". (i=", i,"). And ticker ", next_ticker, " \n")
      output <- takesteps(next_n, next_path, next_ticker, valve_status, next_step)
      #cat("Returned from recursion with path", output[[2]], ", and n=", output[[1]], ", ticker", ticker,  "\n")
    }
    n <- output[[1]]
    path <- output[[2]]
    ticker <- output[[3]]
    valve_status <- output[[4]]
    n_step <- output[[5]]
    return(list(n, path, ticker, valve_status, n_step))
    }
}


path <- 1 # list of caves visited along the way
ticker <- 0 # timestamps of caves visited
paths_taken <- list() # store path of hitherto best pressure release
ticker_taken <- list() # store time stamps of hitherto best pressure release
max_pressure_release <- 0 # setup variable to store hitherto maximum pressure release
n_step <- 1 # index of current step along path through caves
valve_status <- matrix(0, nrow(data), 2) # 0 = closed, 1 = open, 2nd collumn is for timestamps when valves are opened
n_valves <- nrow(valve_status) # number of valves

# set valves with flow rate 0 to already open so that he doesn't spend needless time to open them
for (v in 1:n_valves) {
  if (data[v,2]==0) {
    valve_status[v,1] <- 1
    valve_status[v,2] <- 0
  }
  # also count number of cave neighbors once and use this in the recursive function
  data[v,3] <- length(neighbors[[v]])
}


mainfun_output <- takesteps(1, path, ticker, valve_status, n_step)


