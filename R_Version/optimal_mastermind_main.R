n <- 4
c <- 4

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

select_best_guess <- function(space, o_space) {
  info <- c()
  for (i in 1:length(o_space[,1])) {
    info <- c(info, info_of_guess(o_space[i,], space))
  }
  print("Info")
  print(info)
  print(o_space[which(info==max(info), 1),])
  return(o_space[sample(which(info==max(info)), 1),])
}

#select_best_guess(space, o_space)

guess <- c(0,0,0)
it <- 0
while (!all(guess == password) && length(space[,1])>1) {
  guess <- select_best_guess(space, o_space)
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
