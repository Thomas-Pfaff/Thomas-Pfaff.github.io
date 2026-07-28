#######################################################################
###  Lotka-Volterra Predator-Prey
###  Euler's Method: Differential Equation Numerical Solution
###  By Thomas J. Pfaff 
###  For The Text Applied Calculus with R
#######################################################################

#######################################################################
#### Code to run Euler's Method and save the results.
#### Note: F reprsents foxes (predator) and R rabbits (prey)
#######################################################################

## Define starting values and variables.

t_initial <- 0
t_final <- 400
R_0 <- 200
b_1 <- 0.05
d_1 <- 0.005
F_0 <- 10
b_2 <- 0.0004
d_2 <- 0.04
delta_t <- 0.01

## Set up values for the For Loop.

t <- t_initial
R <- R_0
F <- F_0
number_steps <- (t_final - t_initial) / delta_t
t_data <- t # Initializing the time vector with starting value
R_data <- R # Initializing the Rabbit population vector with starting value
F_data <- F # Initializing the Fox population vector with starting value


## Run the For Loop.

for (i in 1:number_steps){
  R_prime <- b_1*R - d_1*R*F # The rabbit differential equation
  F_prime <- b_2*R*F - d_2*F # The fox differential equation

  R <- R_prime*delta_t + R # Rabbit microscope equation
  F <- F_prime*delta_t + F # Fox microscope equation
  t <- t + delta_t # Increments t to its next value

  if (round(R) == 0){R <- 0}  # Round the prey to 0 to avoid, say, half a rabbit reproducing
  t_data[i+1]<- t  # Add new value to the time vector
  R_data[i+1] <- R  # Add new value to the Rabbit vector
  F_data[i+1] <- F  # Add new value to the Fox vector
} 

######################################################################
#### Create a plot of the data
######################################################################

options(scipen = 999)
par(mar = c(5, 5, 5 ,5)) # Defines space around the graph. Number of lines for bottom, left, top, and right.
plot(t_data, R_data, type = "l", lwd = 2, xlab = "Time",
     ylab = "Prey (Rabbits)", cex.lab = 1.5, cex.axis = 1.5)
  title(paste("Predator Prey Output with\nR_0=", R_0, ", b_1=", b_1, ", d_1=",
               d_1, "\nF_0=", F_0, ", b_2=", b_2,", d_2=", d_2,sep="") )
  grid(NULL, NULL, col = "black", lwd = 3)
par(new  =TRUE) # Add new plot to the current graph
plot(t_data, F_data, type = "l", ylim = c(min(F_data), 1.5*max(F_data)),
     lwd = 2, axes = FALSE, xlab = "", ylab = "", col = "red2")   # Create second plot without axes
  axis(side = 4, at = pretty(c(min(F_data), 1.5*max(F_data))),
       col = "red2", cex.axis = 1.5, col.axis = "red2") # Add second axis
  mtext("Predators (Fox)", side = 4, line = 3, col = "red2", cex = 1.5) 
  grid(NULL, NULL, col = "red2", lwd = 3)


######################################################################
#### Find the final values.
######################################################################

options(digits = 8) # Sets number of digits of output
R_data[length(R_data)]
F_data[length(F_data)]

paste("The value of R at time t=", round(t,1), " is ", R_data[length(R_data)], sep="")
paste("The value of F at time t=", round(t,1), " is ", F_data[length(F_data)], sep="")


#######################################################################
#### Find the maximm value and time of first (if ties) maximum. 
#### Use min() and which.min() to find minimums.
#######################################################################


max(R_data) # Rabbit max
which.max(R_data) # Location of first rabbit max
t_data[which.max(R_data)] # Time value of first rabbit max

max(F_data) # Fox max
which.max(F_data) # Location of first fox max
t_data[which.max(F_data)] # Time value of first fox max

#######################################################################
#### Run the code below to see the first n values of the data.
#### This is particulalry useful to check work done by hand.
#######################################################################

rows <- 5
head(cbind(t_data,R_data,F_data), rows)

