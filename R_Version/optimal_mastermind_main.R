library(cli)
# This file is for getting info on repeating digits in first guesses, AND NOTHING ELSE
n <- 5
c <- 6

space <- matrix(nrow=c^n, ncol=n)

for (i in 1:n) {
  column_c <- c()
  for (j in 1:c) {
    column_c <- c(column_c, rep(j, c^(n-i)))
  }
  space[,i] <- rep(column_c, c^(i-1))
}

o_space <- space

password <- space[sample(1:(c^n),1),]
password


find_guess_structure <- function(guesses) {
  print(guesses)
  guess_info <- matrix(nrow = length(guesses[,1]), ncol = length(unique(guesses[1,])))
  print(length(unique(guesses[1,])))
  for (i in 1:length(guesses[,1])) {
    u <- unique(guesses[i,])
    n_occurences <- c()
    for (j in u) {
      n_occurences <- c(n_occurences, length(which(guesses[i,]==j)))
    }
    guess_info[i,] <- sort(n_occurences)
    #tryCatch(guess_info[i,] <- sort(n_occurences), finally = stop("Not all guesses were equivalent."))
  }
  for (i in 2:length(guess_info[,1])) {
    if (!all(guess_info[i,]==guess_info[i-1,])) {
      stop("Not all guesses were equivalent.")
    }
  }
  print("Structure:")
  print(guess_info[1,])
  #return(guess_info[1,])
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
  #print("Probabilities")
  #temp <- c()
  for (i in 1:length(u[,1])) {
    pi <- length(which(responses[,1]==u[i,1] & responses[,2]==u[i,2]))/length(responses[,1])
    #temp <- c(temp, pi)
    #print(pi)
    t_info <- t_info - pi*log2(pi)
  }
  #print("sum")
  #print(sum(temp))
  #print(t_info)
  
  return(t_info)
}

select_best_guess <- function(space, o_space, iter) {
  info <- c()
  cli_progress_bar("Selecting Best Guess", total = length(o_space[,1]))
  for (i in 1:length(o_space[,1])) {
    info <- c(info, info_of_guess(o_space[i,], space))
    cli_progress_update()
  }
  cli_progress_done()
  #print("Info")
  #print(info)
  #print("max")
  #print(max(info))
  #print("Which")
  #print(as.double(info)==as.double(4.052252))
  #print(str(info[722]))
  #print(info==max(info))
  #print(which(info==max(info)))
  #print(which.max(info))
  #print(info==4.052252)
  #print(round(info*1000000)==max(round(info*1000000)))
  #print("Values")
  #print(o_space[which(info==max(info)),])
  #print(round(info, digits=6))
  #print(round(info, digits=6)==max(round(info, digits=6)))
  #print("Values")
  #print(o_space[which(round(info, digits=6)==max(round(info, digits=6))),])
  if (iter==0) {
    print("First Guess Info")
    find_guess_structure(o_space[which(round(info, digits=6)==max(round(info, digits=6))),])
    print("End First Guess Info")
  }
  return(o_space[sample(which(round(info, digits=6)==max(round(info, digits=6))), 1),])
}

#select_best_guess(space, o_space)

guess <- c(0,0,0)
it <- 0
while (!all(guess == password) && length(space[,1])>1) {
  guess <- select_best_guess(space, o_space, it)
  #print("Guess")
  #print(guess)
  #print("Space")
  #print(space)
  response <- get_response(guess, password)
  space <- trim_space(guess, response, space)
  it <- it + 1
}
print("Success Space")
space
print("Password")
password
print("Iterations")
it






