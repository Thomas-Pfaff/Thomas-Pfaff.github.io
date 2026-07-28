#######################################################################
###  Logistic Growth 
###  Euler's Method: Differential Equation Numerical Solution
###  By Thomas J. Pfaff
###  For The Text Applied Calculus with R
#######################################################################

#######################################################################
#### Code to run Euler's Method and save the results.
#######################################################################

## Define starting values and variables.

t_initial <- 0
t_final <- 20
y_0 <- 10
r <- 0.5
k <- 1000
delta_t <- 0.01

## Set up values for the For Loop.

t <- t_initial
y <- y_0
number_steps <- (t_final-t_initial) / delta_t
t_data <- t # Initializing the time vector with starting value
y_data <- y # Initializing the y or population vector with starting value

## Run the For Loop.

for (i in 1:number_steps){
  y_prime <- r*(1 - y / k)*y  # The Differential equation 
  y <- y_prime*delta_t + y # Microscope equation
  t <- t + delta_t # Increments t to its next value
  t_data[i + 1] <- t # Add new value to the time vector
  y_data[i + 1] <- y # Add new value to the population vector
} 

######################################################################
#### Create a plot of the data. 
######################################################################

par(mar = c(4, 5, 5, 2))
plot(t_data, y_data, ylim = c(0,max(y_data)), type = "l",
     lwd = 2, xlab = "Time", ylab = expression(P(t)),
     cex.lab = 1.5, cex.axis = 1.5)
  title(paste("Logistic Growth Output with\ny_0=", y_0,
              ", r=", r,", k=", k, "\nnumber of steps=", 
              number_steps, sep = ""))
  grid(NULL, NULL, col = "black")

######################################################################
#### Find the final value.
######################################################################

options(digits = 8) # Sets number of digits of output
y_data[length(y_data)]

paste("The value of y at time t=",round(t,1)," is ", 
      y_data[length(y_data)], sep = "")

#######################################################################
#### Run the code below to see the first n values of the data.
#### This is particulalry useful to check work done by hand.
#######################################################################

rows <- 5
head(cbind(t_data, y_data), rows)


