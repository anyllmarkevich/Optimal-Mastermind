library(cli)

create_space <- function(c, n) {
  space <- matrix(nrow=c^n, ncol=n)
  
  for (i in 1:n) {
    column_c <- c()
    for (j in 1:c) {
      column_c <- c(column_c, rep(j, c^(n-i)))
    }
    space[,i] <- rep(column_c, c^(i-1))
  }
  return(space)
}

find_guess_structure <- function(guess) {
  guess_info <- c()
  u <- unique(guess)
  n_occurences <- c()
  for (j in u) {
    n_occurences <- c(n_occurences, length(which(guess==j)))
  }
  guess_info <- sort(n_occurences)
  return(guess_info)
}

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

select_best_guess <- function(space, o_space) {
  info <- c()
  cli_progress_bar("Selecting Best Guess", total = length(o_space[,1]))
  for (i in 1:length(o_space[,1])) {
    info <- c(info, info_of_guess(o_space[i,], space))
    cli_progress_update()
  }
  cli_progress_done()
  correct_values <- which(round(info, digits=6)==max(round(info, digits=6)))
  if (length(correct_values) > 1) {
    return(o_space[sample(correct_values, 1),]) 
  } else {
    return(o_space[correct_values[1],])
  }
}
### RUNTIME CODE ###

n_list <- c(5)
c_list <- c(6)

for (i in 1:length(n_list)) {
  n <- n_list[i]
  c <- c_list[i]
  space <- create_space(c, n)
  guess <- select_best_guess(space, combo_to_guesses(valid_combos(n, c), c))
  print("OPTIMUM FIRST GUESS FOR")
  print(paste("n=", n, sep = ""))
  print(paste("c=", c, sep = ""))
  print("Guess:")
  print(guess)
  print("Guess Structure:")
  print(find_guess_structure(guess))
}