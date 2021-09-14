#######
# GRIDWORLD - COMMENTED&INCOMPLETE&NOTWORKING
# 03.03.2021
# Reinforcement Learning Seminar
#######

rm(list = ls()) #clear the workspace

### define the important variables
# using the  = c() command you can combine elements into a vector
actions <- ???
states <- ???
n <- ??? #number of runs from start to end
s_0 <- ??? #start start
s_terminal <- ??? # end state
learning_rate <- ??? # learning rate for learning rule
invtemp = ??? #inverse temperature for softmax choice rule
method = ??? #method that we want to use (softmax?)
  ### can you think of any other variable that might come in handy?
  # e.g. if we want to implement another method?

# use 'matrix' to create state-action value table with 0s in it. 
# use ?matrix to open a window that tells you how to use it:
Q <- matrix(0, ???) 


# this is a function that can be called later in the script.
# so far there is not much defined here.
# try to translate the rules from our environment into code
# what are the input arguments sin and ain? and output sout?
moveInEnvironment <- function(sin,ain){
  sout = sin
  if (sin == "s0" && ain == "down") 
    sout <- "s1"
  ???
  return(sout)
}

#this is also a function that we can call later
# you provide some state information and the function ...
# returns some reward
getReward <- function(sin){
  if (sin == "sonestate") { 
    rout <- ??? 
  } else if (sin == ???) { 
    rout <- ??? 
  } else { 
    rout <- ??? 
  } 
  return(rout)
}


#here we start to loop through the different runs
for (i in 1:n) {
  #print() is useful (not necessary)
  # here we use it to show at which iteration our agent is currently in
  if (i %% 100 == 0) {
	    print(i) 
  } else {}
	 
	state <- ???# in which initial state should we start? 
	
	## here the agent starts to move
	# but when should he stop?
	while (state ???) {

		  if (method == "softmax") {
			  qvals = Q[state, ] #get qvalues for the current state
				pvals = ??? #convert them into probabilities
				action = sample(actions,size = 1, prob = pvals) #sample from actions with specific probabilities
				# look what happens when you exchange pvals for qvals in the line above
		  } else if (method == "egreedy"){ 
		    ## e-greedy could be an alternative to softmax
		    #from what you know, how would you start?
		    # hint1: you need an if/else statement
		    # hint2: which.max() gives you the maximum, but be careful ;)
		    # to get you started: runif(1) gives you a random number between 0 and 1, uniform distribution
		    randonumber = runif(1)
		    
		    
		  } else {
				action <- sample(actions, 1)
		  }
    
	  ## remember the functions we defined in the beginning?
	  #here we finally use them.
	  #but what input variables are necessary?
    next_state <- moveInEnvironment(???)
		reward <- getReward(???)
		
		## data management
		# It is a good idea to store some variables for easier access later
		# this simply stores the variables state (e.g., s1) and reward (e.g. -1) in a list		
		response <- (list(state=next_state, reward=reward))
		
		## learning rule
		# something is still missing here
		# also: have a look how we use the variable response followed by $ to easily access
		Q[state, action] <- ??? (response$reward + max(Q[response$state, ]) - Q[state, action])
		
		
		## again some (unnecessary but helpful) plotting
		# you can again use the "%%" operator to 
    if ((i == 1) & (next_state == "s3"))	{
    	print(i)
			print(Q) # what does q-look like after first successful iteration (i==1)
  	} else {
  	}

		state <- response$state # move to next state 
    moves = moves + 1 #count 
  } #end of while loop (moves)
  movecounter[i] = moves
} # end of for loop (runs)

##print the final results and have a look
print(actions[max.col(Q)])
print(Q)
#print(movecounter)
plot(1:n,movecounter)





