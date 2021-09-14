#######
# GRIDWORLD - UNCOMMENTED
# 03.03.2021
# Reinforcement Learning Seminar
#######

rm(list = ls()) #clear the workspace


actions <- c("up", "left", "down", "right") 
states <- c("s0", "s1", "s2", "s3") 
n <- 100   # specfy how many times you want to run the q-learning function
s_0 <- "s0" # start at initial state 
s_terminal <- "s3" # goal state
epsilon <- 0.1 # parameter for ε-greedy action selection
learning_rate <- 0.1 # LR
invtemp = 0.1
method = c("egreedy") 
movecounter = matrix(0, nrow=n, ncol=1)
Q <- matrix(0, nrow=length(states), ncol=length(actions), dimnames=list(states, actions))
moves = 0

moveInEnvironment <- function(sin,ain){
  sout = sin
  if (sin == "s0" && ain == "down") 
    sout <- "s1" 
  if (sin == "s1" && ain == "up") 
    sout <- "s0" 
  if (sin == "s1" && ain == "right") 
    sout <- "s2" 
  if (sin == "s2" && ain == "left") 
    sout <- "s1" 
  if (sin == "s2" && ain == "up") 
    sout <- "s3" 
  if (sin == "s3" && ain == "down") 
    sout <- "s2" 
  return(sout)
}

getReward <- function(sin){
  if (sin == "s3") { 
    rout <- 100 
  } else { 
    rout <- -1 
  } 
  return(rout)
}


for (i in 1:n) {
  if (i %% 100 == 0) {
	    print(i) 
  } else {}
	 
	state <- s_0 # set cursor to initial state 
    
  moves = 0;
	  		
	while (state != s_terminal) {

		  if (method == "softmax") {
			  qvals = Q[state, ]
				pvals = exp(invtemp*qvals)/sum(exp(invtemp*qvals))
				action = sample(actions,size = 1, prob = pvals)
		  } else if (method == "egreedy"){ 
		    randonumber = runif(1)
		    if (randonumber <= epsilon) {
		      action <- sample(actions, 1)  # pick random action ('sample' takes a sample of the specified size (here 1) from the elements of 'actions') 
		    } else {
		      maxactions = which(Q[state, ] == max(Q[state, ]))
		      action = sample(actions[maxactions],1)
		    }
		  } else {
				action <- sample(actions, 1)
				print("no method has been specified. Sampling random action...")
		  }

    next_state <- moveInEnvironment(state,action)
		reward <- getReward(next_state)
				
		response <- (list(state=next_state, reward=reward)) 
		Q[state, action] <- Q[state, action] + learning_rate * (response$reward + max(Q[response$state, ]) - Q[state, action])
				
    if ((i == 1) & (next_state == "s3"))	{  #	if (i %% 300 == 0) {
    	print(i)
			print(Q) # what does q-look like after first successful iteration (i==1)
  	} else {
  	}

		state <- response$state # move to next state 
    moves = moves + 1
  }  
  movecounter[i] = moves
} 


#print(actions[max.col(Q)])
print(Q)
#print(movecounter)
plot(1:n,movecounter)





