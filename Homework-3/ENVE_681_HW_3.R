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
                                               
del = 100

# R = recharge in cm/yr
R = 50

# Convert R to m/yr
R = R/(100*365)

# T = transmissivity  in m^2/day
T = 200

# x_nodes = number of nodes in x direction not including the border
x_nodes = x_len/del - 1

# y_nodes = number of nodes in y direction not including the border
y_nodes = y_len/del - 1

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
rhs = matrix((-R*(del^2)/T), total_nodes,1)

# fill in conditions for top and bottom boundaries
for (i in 1:x_nodes) 
  {
  rhs[i,1] = (-R*(del^2)/T) - 10
  rhs[total_nodes- i, 1] = (-R*(del^2)/T) - 10
}

# fill in conditions for side boundaries
for (i in 1:(y_nodes-1)) 
  {
  rhs[x_nodes*i, 1] = (-R*(del^2)/T) - 10
  rhs[(x_nodes*i) + 1 , 1] = (-R*(del^2)/T) - 10
}

# fill in corner boundaries
rhs[corner_1,1] = (-R*(del^2)/T) - 20
rhs[corner_2,1] = (-R*(del^2)/T) - 20
rhs[corner_3,1] = (-R*(del^2)/T) - 20
rhs[corner_4,1] = (-R*(del^2)/T) - 20

# Creating coefficient matrix
#------------------------------------------------------------------------------

# define coeff as coefficient matrix with the intial diagnols as -4
coeff = diag(total_nodes) * -4

# create 2 offset diagonals as 1
diag(coeff[-1,]) = 1
diag(coeff[,-1]) = 1

# create offset diagnols with one (x_nodes + 1) rows down
diag(coeff[(x_nodes + 1):total_nodes,]) = 1

# create offset diagnols with one (x_nodes + 1) columns over
diag(coeff[,(x_nodes + 1):total_nodes]) = 1


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


#------------------------------------------------------------------------------
# BONUS
#------------------------------------------------------------------------------
# Same variable from previous section
# Creating answer matrix
#------------------------------------------------------------------------------

trans_rhs = matrix(rhs, x_nodes, y_nodes)

x_inlet = 200
y_inlet = 300

x_inlet_nodes = x_inlet/del 
y_inlet_nodes = y_inlet/del 

inlet_corner_x1 = x_inlet_nodes - (x_inlet_nodes - 1)
inlet_corner_y1 = y_nodes - y_inlet_nodes
inlet_corner_x2 = x_inlet_nodes + 1
inlet_corner_y2 = y_nodes

# Updating new boundary conditions

for (i in (inlet_corner_x1 + 1):x_inlet_nodes) 
  {
  trans_rhs[i, inlet_corner_y1] = (-R*(del^2)/T) - 10
}

for (j in (inlet_corner_y1 + 1):y_nodes) 
  {
  trans_rhs[x_inlet_nodes + 1, j] = (-R*(del^2)/T) - 10
}

# Update corners

trans_rhs[inlet_corner_x1, inlet_corner_y1] = (-R*(del^2)/T) - 20
trans_rhs[inlet_corner_x2, inlet_corner_y2] = (-R*(del^2)/T) - 20
# trans_rhs[1, y_nodes] = 10

# updating where inlet is

for (i in (inlet_corner_x1):x_inlet_nodes) 
  {
  for (j in (inlet_corner_y1 + 1):y_nodes) 
    {
    trans_rhs[i, j] = 10
  }
}

# making it a single column matrix

rhs_2 = matrix(trans_rhs, total_nodes, 1)


# Creating coefficient matrix
#------------------------------------------------------------------------------

coeff_2 <- coeff

# counter to start at correct row/column
b = y_nodes - y_inlet_nodes + 2

# Modify coefficient matrix for new boundary conditions
for (j in 1:y_inlet_nodes)
  {
  for (i in 1:x_inlet_nodes) 
    {
    coeff_2[(x_nodes*(b-j) + i),(x_nodes * (y_nodes - j) + i)] = 0
    coeff_2[(x_nodes * (y_nodes - j) + i), (x_nodes*(b-j) + i)] = 0
  }
}

for (j in 1:y_inlet_nodes)
{
  for (i in 1:x_inlet_nodes) 
  {
    coeff_2[(x_nodes*(y_nodes-j) + i),(x_nodes*(y_nodes-j) + i)] = 1
    coeff_2[(x_nodes*(y_nodes-j) + i + 1),(x_nodes*(y_nodes-j) + i)] = 0
    coeff_2[(x_nodes*(y_nodes-j) + i),(x_nodes*(y_nodes-j) + i + 1)] = 0
  }
}


# Solving for h matrix
#------------------------------------------------------------------------------

# define h as matrix for h values of groundwater
h_2 = solve(coeff_2,rhs_2)


# Reassemble h into matrix that matches nodes
z_2 = matrix(h_2, x_nodes,y_nodes)

i = matrix(10 , x_nodes, 1)
j = matrix(10, 1, y_nodes + 2)

z_2 = cbind(i, z_2)
z_2 = cbind(z_2, i)
z_2 = rbind(j, z_2)
z_2 = rbind(z_2, j)


# graphing matrix
#------------------------------------------------------------------------------

plot_ly(z = matrix(z_2 , x_nodes + 2 , y_nodes + 2)) %>%
  add_surface()


