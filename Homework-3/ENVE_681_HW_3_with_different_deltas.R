# Homework 3 ------------------
# 10/23/17
#------------------------------

library(tidyverse)
library(lubridate)
library(plotly)

# Define input variables
#------------------------------------------------------------------------------

# x_len = length in x direction in meters
x_len = 600

# y_len = length in y direction in meters
y_len = 1000

# del = change in distance along both x and y 
# (for simplicity the change is the same in both x & y directions)

del = 25
x_del = 10
y_del = 50

# R = recharge in cm/yr
R = 50

# Convert R to m/yr
R = R/(100*365)

# T = transmissivity  in m^2/day
T = 200

# x_nodes = number of nodes in x direction not including the border
x_nodes = x_len/x_del - 1

# y_nodes = number of nodes in y direction not including the border
y_nodes = y_len/y_del - 1

# total_nodes = total number of nodes in system
total_nodes = x_nodes * y_nodes


# Creating answer matrix
#------------------------------------------------------------------------------

# creating corner boundaries variables for matrix
corner_1 = 1
corner_2 = x_nodes
corner_3 = total_nodes
corner_4 = total_nodes - x_nodes + 1

# define answer matrix as rhs
# fill in initial matrix with general overall equation
rhs = matrix((-R/T), total_nodes,1)

# fill in conditions for top and bottom boundaries
for (i in 1:x_nodes) 
{
  rhs[i,1] = (-R/T) - (10/(y_del)^2)
  rhs[total_nodes- i, 1] = (-R/T) - (10/(y_del)^2)
}

# fill in conditions for side boundaries
for (i in 1:(y_nodes-1)) 
{
  rhs[x_nodes*i, 1] = (-R/T) - (10/(x_del)^2)
  rhs[(x_nodes*i) + 1 , 1] = (-R/T) - (10/(x_del)^2)
}

# fill in corner boundaries
rhs[corner_1,1] = (-R/T) - (10/(x_del)^2) - (10/(y_del)^2)
rhs[corner_2,1] = (-R/T) - (10/(x_del)^2) - (10/(y_del)^2)
rhs[corner_3,1] = (-R/T) - (10/(x_del)^2) - (10/(y_del)^2)
rhs[corner_4,1] = (-R/T) - (10/(x_del)^2) - (10/(y_del)^2)

# Creating coefficient matrix
#------------------------------------------------------------------------------

# define coeff as coefficient matrix with the intial diagnols as -4
coeff = diag(total_nodes) * -((2/(x_del)^2) + (2/(y_del)^2))

# create 2 offset diagonals as 1
diag(coeff[-1,]) = (1/(x_del)^2)
diag(coeff[,-1]) = (1/(x_del)^2)

# create offset diagnols with one (x_nodes + 1) rows down
diag(coeff[(x_nodes + 1):total_nodes,]) = (1/(y_del)^2)

# create offset diagnols with one (x_nodes + 1) columns over
diag(coeff[,(x_nodes + 1):total_nodes]) = (1/(y_del)^2)


# Modify coefficient matrix for boundary conditions
for (i in 1:(y_nodes - 1))
{
  coeff[(i * x_nodes),(i * x_nodes + 1)] = 0
  coeff[(i * x_nodes + 1),(i * x_nodes)] = 0
}

# Solving for h matrix
#------------------------------------------------------------------------------

# define h as matrix for h values of groundwater
h = solve(coeff,rhs)


# Reassemble h into matrix that matches nodes
z = matrix(h, x_nodes,y_nodes)

i = matrix(10 , x_nodes, 1)
j = matrix(10, 1, y_nodes + 2)

z = cbind(i, z)
z = cbind(z, i)
z = rbind(j, z)
z = rbind(z, j)

# graphing matrix
#------------------------------------------------------------------------------

plot_ly(z = matrix(z , x_nodes + 2 , y_nodes + 2)) %>%
  add_surface()


