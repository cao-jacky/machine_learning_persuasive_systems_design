
################## FUNCTION DEFINITIONS: ##################

# Function to generate observations
get_reward <- function(arm){
  if (arm == "A"){ # Arm A reward
    return(rbinom(1, 1, .4))
  }
  if (arm == "B"){ # Arm B reward
    return(rbinom(1,1, .6))
  }
  # Else
  print("No valid arm...")
  return(FALSE)
}

# The actual bandit policy (alter this)
choose_arm <- function(An=0, As=0, Bn=0, Bs=0){
  # Normally, do something with the counts...
  # For now, select a random arm
  sample(c("A", "B"), 1)
}


############### THE ACTUAL LOOP: ##################

T <- 1000 # The horizon
M <- 2 # The number of simulation runs
Result <- c() # Vector with the reward for each run

for(m in 1:M){ # For each simulation run
  
  # Initalize this simulation:
  An <- 0 # Number of plays of arm A
  As <- 0 # Number of successes of arm A
  Bn <- 0 # Number of plays of arm B
  Bs <- 0 # Number of successes of arm B
  cR <- 0 # Cumulative rewards for this run
  
  for(t in 1:T){ # For each timepoint
    
    # Choose action
    action <- choose_arm(An, As, Bn, Bs)
    
    # Get reward
    reward <- get_reward(action)
    
    # Store history
    if(action == "A"){
      An <- An + 1
      As <- As + reward
    }
    if(action == "B"){
      Bn <- Bn + 1
      Bn <- Bn + reward
    }
    cR <- cR + reward
  
  }
  
  # Store outcome of this run:
  Result <- c(Result, cR)
  
}

# Show results:
print(Result)

## ASSIGNMENT 2:
# Create new versions of the choose_arm() function (using the same signature)
# and see if you can improve on the average rewards...




