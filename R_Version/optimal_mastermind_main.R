### DEFINE MASTERMIND SPACE ###
n <- 5 # Number of pins in password
c <- 5 # Number of possible pin colors

### SETUP ####
# Import required libraries
library(cli)

# Setup algorithm
space <- matrix(nrow=c^n, ncol=n) # Create an empty matrix to store all possible passwords

# Find and list all possible passwords
for (i in 1:n) {
  column_c <- c()
  for (j in 1:c) {
    column_c <- c(column_c, rep(j, c^(n-i)))
  }
  space[,i] <- rep(column_c, c^(i-1))
}

o_space <- space # Save an original un-reduced space

# Generate a random password
password <- space[sample(1:(c^n),1),]

### DEFINITIONS ###
### MAIN PROCESS
# The following functions calculate and implement the optimal mastermind strategy.

# Given a guess, return the number of correct colors in the wrong place and correct colors in the correct place.
get_response <- function(guess, pass) {
  half_pins <- 0
  full_pins <- 0
  for (i in 1:c) {
    half_pins <- half_pins + min(c(length(subset(guess, guess==i)), length(subset(pass, pass==i))))
  }
  
  for (i in 1:n) {
    if (guess[i]==pass[i]) {
      half_pins <- half_pins - 1
      full_pins <- full_pins + 1
    }
  }
  
  return(c(half_pins, full_pins))
}

# Remove all guesses in the space of possible guesses that aren't possible given a response.
trim_space <- function(guess, response, space) {
  correct_vec <- c()
  for (i in 1:length(space[,1])) {
    if (all(get_response(guess, space[i,])==response)) {
      correct_vec <- c(correct_vec, i)
    }
  }
  new_space <- space
  new_space <- new_space[correct_vec,,drop=FALSE]
  return(new_space)
}

# Find the informational value of guess, where higher values mean than an answer to this guess would be more informative.
info_of_guess <- function(guess, space) {
  responses <- matrix(ncol=2, nrow = length(space[,1]))
  for (i in 1:length(space[,1])) {
    responses[i,] <- get_response(guess, space[i,])
  }
  u <- unique(responses)
  t_info <- 0
  for (i in 1:length(u[,1])) {
    pi <- length(which(responses[,1]==u[i,1] & responses[,2]==u[i,2]))/length(responses[,1])
    t_info <- t_info - pi*log2(pi)
  }
  return(t_info)
}

# Select the best guess from all guesses based on the amount of information it provides about the password.
select_best_guess <- function(space, o_space) {
  info <- c()
  cli_progress_bar("Selecting Best Guess", total = length(o_space[,1]))
  for (i in 1:length(o_space[,1])) {
    info <- c(info, info_of_guess(o_space[i,], space))
    cli_progress_update()
    print(info)
    print(o_space[i,])
  }
  cli_progress_done()
  correct_values <- which(round(info, digits=6)==max(round(info, digits=6)))
  if (length(correct_values) > 1) {
    return(o_space[sample(correct_values, 1),]) 
  } else {
    return(o_space[correct_values[1],])
  }
}

### OPTIMIZE FIRST GUESS
# The following functions are designed to skip redundant calculations during the first guess, when no information on the password is available and checking every possible permutation is unnecessary, thus greatly reducing computational burden.

# Convert a number of repeated pins of different colors to matching sequences of pins
combo_to_guesses <- function(combos, total_colors) {
  guesses <- matrix(nrow = 0, ncol = sum(combos[[1]]))
  n_pins <- sum(combos[[1]])
  n_cols <- length(combos[[1]])
  for (combo in combos) {
    guess <- c()
    colors <- sample(1:total_colors, n_cols, replace = FALSE)
    for (i in 1:length(combo)) {
      num <- combo[i]
      guess <- c(guess, rep(colors[i], num))
    }
    guesses <- rbind(guesses, sample(guess))
  }
  rownames(guesses) <- NULL
  return(guesses)
}

# Find all possible combinations of repeated colors that fit within the defined space.
possible_combos <- function(n_pins) {
  new_combos <- list(c(rep(1, n_pins)))
  combos <- list()
  temp_combos <- list(NA)
  while (length(temp_combos)>0) {
    temp_combos <- list()
    for (i in new_combos) {
      n_ones <- length(which(i==1))
      if (n_ones > 1) {
        for (j in 2:n_ones) {
          temp <- c( i[which(i!=1)], j)
          if (j < n_ones) {
            temp <- c(temp, rep(1, (n_ones-j)))
          }
          temp_combos <- append(temp_combos, list(sort(temp)))
        }
      }
    }
    combos <- append(combos, new_combos)
    new_combos <- unique(temp_combos)
  }
  combos <- append(combos, new_combos)
  return(unique(combos))
}

# Return all possible combinations of repeated pins that don't use more colors than are available.
valid_combos <- function(n_pins, n_cols) {
  valid_combs <- list()
  possible_combs <- possible_combos(n_pins)
  for (i in possible_combs) {
    if (length(i) <= n_cols) {
      valid_combs <- append(valid_combs, list(i))
    }
  }
  return(valid_combs)
}

### OUTPUT PROCESS
### The following functions are used to inform the user of the program's progress.

# Neatly print a guess and the response of that guess.
print_output <- function(guess, response, it) {
  print(paste("GUESS #", it, sep=""))
  print(guess)
  print("RESPONSE")
  print(paste("Correct color, wrong spot: ", response[1]))
  print(paste("Correct color, correct spot: ", response[2]))
}

### RUN ALGORYTHM ###
guess <- c(0,0,0) # Create an empty first guess
# Using optimization for the first guess, calculate an optimal response, receive response, and adjust possible password space.
guess <- select_best_guess(space, combo_to_guesses(valid_combos(n, c), c))
response <- get_response(guess, password)
space <- trim_space(guess, response, space)

it <- 1 # Increment the number of guesses used.
print_output(guess, response, it) # Print info about the first guess

# Repeat procedure, without first-guess optimization, until correct password is found.
while (!all(guess == password) && length(space[,1])>1) {
  # Calculate guess, get response, and adjust possible password space
  guess <- select_best_guess(space, o_space)
  response <- get_response(guess, password)
  space <- trim_space(guess, response, space)
  
  it <- it + 1 # Increment the number of guesses used.
  print_output(guess, response, it) # Print info about the guess
}

# Identify if last guess was lucky and fell on correct password while password was still uncertain
if (!all(guess == space[1,])) {
  it<-it+1 # If guess was not lucky, increment the number of guesses (count one more guess, even if only one possible valid guess remains)
}

# Print the final guess and summary information for the entire game.
print("SUCCESS")
print("Final Guess:")
print(space[1,])
print("Password:")
password
print("Number of Guesses Used:")
print(it)
