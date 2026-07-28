############################################################################
###  SIR Model
###  Euler's Method: SIR Differential Equations Numerical Solution
###  By Thomas J. Pfaff
###  For The Text Applied Calculus with R
############################################################################

#######################################################################
#### Code to run Euler's Method and save the results.
#######################################################################

## Define starting values and variables.

t_initial <- 0
t_final <- 30
S_0 <- 50000
I_0 <- 2000
R_0 <- 500
mc <- 0.00001
d <- 14
delta_t <- 0.01

## Set up values for the For Loop.

t <- t_initial
S <- S_0
I <- I_0
R <- R_0
number_steps <- (t_final - t_initial) / delta_t
t_data <- t # Initializing the time vector with starting value
S_data <- S # Initializing the Susceptible population vector with starting value
I_data <- I # Initializing the Infected population vector with starting value
R_data <- R # Initializing the Recovered population vector with starting value

## Run the For Loop.

for(i in 1:number_steps){
  S_prime <- -mc*S*I    # The differential equations 
  I_prime <- mc*S*I - I / d
  R_prime <- I / d

  S <- S_prime*delta_t + S # Microscope equations
  I <- I_prime*delta_t + I 
  R <- R_prime*delta_t + R 
  t <- t + delta_t # Increments t to its next value

  t_data[i+1] <- t  # Add new value to the time vector
  S_data[i+1] <- S  # Add new value to the Susceptible vector
  I_data[i+1] <- I  # Add new value to the Infected vector
  R_data[i+1] <- R  # Add new value to the Recovered vector
}


######################################################################
#### Create a plot of the data
######################################################################

options(scipen = 999) # Disable scientific notation
par(mar = c(5, 4, 5, 6))
plot(t_data, S_data, type = "l", ylim = c(0, S_0+I_0+R_0), lwd = 2,
     xlab = "Time", ylab = "Population", cex.lab = 1.5, cex.axis = 1.25)
  title(paste("SIR Output with\nS_0=", S_0, ", I_0=", I_0, ", R_0=",
              R_0, "\nmc=", mc, ", d=", d, sep="") )
  abline(h = (S_0 + I_0 + R_0), lwd = 2, col = "gray50")
  grid(NULL, NULL, col = "black")
  lines(t_data, I_data, lwd = 2, col = "red2")
  lines(t_data, R_data, lwd = 2, col = "blue2")
  par(xpd = TRUE)
  legend("topright", c("S", "I", "R"), lwd = c(2, 2, 2), 
         col = c("black", "red2", "blue2"), inset = c(-0.2,0))
  par(xpd=FALSE)

######################################################################
#### Find the final values.
######################################################################

options(digits = 8) # Sets number of digits of output

S_data[length(S_data)]
I_data[length(I_data)]
R_data[length(R_data)]

paste("The value of S at time t=", round(t,1)," is ", S_data[length(S_data)], sep="")
paste("The value of I at time t=", round(t,1)," is ", I_data[length(I_data)], sep="")
paste("The value of R at time t=", round(t,1)," is ", R_data[length(R_data)], sep="")

#######################################################################
#### Find the maximm values and time of first maximum.
#### Use min() and which.min() to find minimums.
#######################################################################


max(S_data) # Susceptible max
which.max(S_data) # Location of susceptible max
t_data[which.max(S_data)] # Time value of susceptible max

max(I_data) # Infected max
which.max(I_data) # Location of infected max
t_data[which.max(I_data)] # Time value of infected max

max(R_data) # Recovered max
which.max(R_data) #  Location of recovered max
t_data[which.max(R_data)] # Time value of recovered max


#######################################################################
#### Run the code below to see the first n values of the data.
#### This is particulalry useful to check work done by hand.
#######################################################################

rows <- 5
head(cbind(t_data, S_data, I_data, R_data), rows)


