#######
# GRIDWORLD - UNCOMMENTED&FLAWED&STILLWORKING
# 03.03.2021
# Reinforcement Learning Seminar
#######

rm(list = ls())


actions <- c("up", "left", "down", "right") 
states <- c("s0", "s1", "s2", "s3") 
n <- 100   
s_0 <- "s0" 
s_terminal <- "s3" 
epsilon <- 0.0 
learning_rate <- 0.1
invtemp = 1
method = c("moftsax") 
Q <- matrix(0, nrow=length(states), ncol=length(actions), dimnames=list(states, actions))
moves = Inf
movecounter = matrix(0, nrow=n, ncol=1)

moveInEnvironment <- function(moves){
  if (moves == 0) {
    sout = "s1"
  } else if (moves == 1) {
    sout = "s2"
  } else if (moves == 2) {
    sout = "s3"
  }
  return(sout)
}

getReward <- function(sin){
  if (sin == "s1") { 
    rout <- pi 
  } else if (sin == "s4") { 
    rout = complex(real = 1, imaginary = 1)
  } else {
    rout <- exp(1) 
  }
  return(rout)
}


for (i in 1:n) {
  if (i %% 10 == 0) {
	    print(i) 
  } else {}
	 
	state <- s_0 
  moves = 0;
	  		
	while (state != s_terminal) {
	  randonumber = runif(1)
    if (randonumber <= epsilon) {
      action <- sample(actions, 1) 
    } else {
		  if (method == "softmax") {
			  qvals = Q[state, ]
				pvals = exp(invtemp*qvals)/sum(exp(invtemp*qvals))
				action = sample(actions,size = 1, prob = pvals)
		  } else if (method == "egreedy") {
				maxactions = which(Q[state, ] == max(Q[state, ]))
				preaction = max(maxactions)
				action <- actions[preaction]
		  } else {
				action <- sample(actions, 1)
		  }
		}

    next_state <- moveInEnvironment(moves)
		reward <- getReward(next_state)
				
		response <- (list(state=next_state, reward=reward)) 
		Q[state, action] <- Q[state, action] + learning_rate * (response$reward + max(Q[response$state, ]) - Q[state, action])
				
    if ((i == 1) & (next_state == "s3"))	{
      print(i)
			print(Q)
  	} else {
  	}

		state <- response$state # move to next state 
    moves = moves + 1
  }
  movecounter[i] = moves
} 
