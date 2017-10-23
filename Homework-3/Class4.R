#  Class 4 - Numerical Differentiation

# Differentiate the function (x)^1/2 over the range of 0 < x < 2 using
# the forward, backward and central difference approaches. Compare to
# the exact solution. 
   
###  library -----------------  
library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(data.table)
library(RcppRoll)
library(ggplot2)
library(scales)

delx=0.2 #Set step size
f1=function(x)x^(1/2) #Define function f1(x)=x^(1/2)
x=seq(from=delx, to=2, by=delx) #Define range of x
deriv=(1/2)*x^(-1/2) #Known derivative of function X^(1/2) is (1/2)x^(-1/2)

### Forward Difference ----------
# Use the forward difference method: f'(x) = [(f(x+delx)-f(x)]/delx 
# to approximate the derivative to f(x)

x_fd=x+delx
f=f1(x)
f_prime_fd=((f1(x_fd)-f1(x))/delx) # Forward difference approximation of derivative
error_fd=deriv-f_prime_fd #Error between derivative and forward difference approximation

# Plot forward difference method against actual derivative
plot(x,f_prime_fd, main="f(x)=sqrt(x)", xlab="x", ylab="f/dx", col="2",
xlim=c(0,2),ylim=c(0,2)) 
points(x,deriv,col="1")
legend("topright",c("Deriv","Forward Diff"),col=c(1,2),pch=c(1,1))

### Backward Difference
# Repeat for the backward difference method 
x_bd=x-delx
f_prime_bd=(f1(x)-(f1(x_bd)))/delx
points(x,f_prime_bd,col="3")
legend("topright",c("Deriv","Forward Diff","Backward Diff"),col=c(1,2,3),pch=c(1,1,1))

# Compute error for backward difference
error_bd=deriv-f_prime_bd

### Central DIfference
# Repeat for the central difference method 
f_prime_cd=(f1(x_fd)-(f1(x_bd)))/(2*delx)
points(x,f_prime_cd,col="4")
legend("topright",c("Deriv","Forward Diff","Backward Diff","Central Diff"),col=c(1,2,3,4),pch=c(1,1,1,1))

# Compute error for backward difference
error_cd=deriv-f_prime_cd

### Compare error for each method
plot(x,error_fd, main="Error", xlab="x", ylab="Error", col="2",
     xlim=c(0,2),ylim=c(-0.3,0.3)) 
points(x,error_bd,col="3")
points(x,error_cd,col="4")
legend("top",c("Forward Diff","Backward Diff","Central Diff"),col=c(2,3,4),pch=c(1,1,1))
