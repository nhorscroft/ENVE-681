# Homework 5 Island Problem ------------------
# 11/13/17
# Libraries------------------------------

library(tidyverse)
library(lubridate)
library(plotly)


# Define input variables-------------------------------------------------------

# x_len = length in x direction in meters
x_len = 600

# y_len = length in y direction in meters
y_len = 1000

# del = change in distance along both x and y 
# (for simplicity the change is the same in both x & y directions)

del = 25
dx = 100
dy = 100
dx2 = dx^2
dy2 = dy^2


# n = time in days
n = 100


# dn = timestep in days
dn = 1

# R = recharge in cm/yr
R = 40

# Convert R to m/yr
R = R/(100*365)

# T = transmissivity  in m^2/day
T = 200

# Sy = specific yield
Sy = 0.8

# gamma = crank-nicholson/central difference value
gamma = 1/2


# r = T*dn/Sy
r = T*dn/Sy

# x_nodes = number of nodes in x direction not including the border
x_nodes = x_len/dx - 1

# y_nodes = number of nodes in y direction not including the border
y_nodes = y_len/dy - 1

# total_nodes = total number of nodes in system
total_nodes = x_nodes * y_nodes


# Creating initial answer matrix---------------------------------------------------

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
  rhs[i,1] = (-R/T) - (10/dy2)
  rhs[total_nodes- i, 1] = (-R/T) - (10/dy2)
}

# fill in conditions for side boundaries
for (i in 1:(y_nodes-1)) 
{
  rhs[x_nodes*i, 1] = (-R/T) - (10/dx2)
  rhs[(x_nodes*i) + 1 , 1] = (-R/T) - (10/dx2)
}

# fill in corner boundaries
rhs[corner_1,1] = (-R/T) - (10/dx2) - (10/dy2)
rhs[corner_2,1] = (-R/T) - (10/dx2) - (10/dy2)
rhs[corner_3,1] = (-R/T) - (10/dx2) - (10/dy2)
rhs[corner_4,1] = (-R/T) - (10/dx2) - (10/dy2)


# Creating inital coefficient matrix-------------------------------------------

# define coeff as coefficient matrix with the intial diagnols as -4
coeff = diag(total_nodes) * -((2/dx2) + (2/dy2))

# create 2 offset diagonals as 1
diag(coeff[-1,]) = (1/dx2)
diag(coeff[,-1]) = (1/dx2)

# create offset diagnols with one (x_nodes + 1) rows down
diag(coeff[(x_nodes + 1):total_nodes,]) = (1/dy2)

# create offset diagnols with one (x_nodes + 1) columns over
diag(coeff[,(x_nodes + 1):total_nodes]) = (1/dy2)


# Modify coefficient matrix for boundary conditions
for (i in 1:(y_nodes - 1))
{
  coeff[(i * x_nodes),(i * x_nodes + 1)] = 0
  coeff[(i * x_nodes + 1),(i * x_nodes)] = 0
}


# Solving for h initial matrix-------------------------------------------------

# define h as matrix for h values of groundwater
h_initial = solve(coeff,rhs)


# Reassemble h into matrix that matches nodes
z = matrix(h_initial, x_nodes,y_nodes)

i = matrix(10 , x_nodes, 1)
j = matrix(10, 1, y_nodes + 2)

z = cbind(i, z)
z = cbind(z, i)
z = rbind(j, z)
z = rbind(z, j)


# graphing initial matrix------------------------------------------------------

plot_ly(z = matrix(z , x_nodes + 2 , y_nodes + 2)) %>%
  add_surface()


# Creating coefficient matrix h & g--------------------------------------------

# h coefficient matrix
h = diag(total_nodes)* (1 - 2*gamma*r*((1/dx2) + (1/dy2)))

diag(h[-1,]) = (r*gamma/dx2)
diag(h[,-1]) = (r*gamma/dx2)

diag(h[(x_nodes + 1):total_nodes,]) = (r*gamma/dy2)
diag(h[,(x_nodes + 1):total_nodes]) = (r*gamma/dy2)

for (i in 1:(y_nodes - 1))
{
  h[(i * x_nodes),(i * x_nodes + 1)] = 0
  h[(i * x_nodes + 1),(i * x_nodes)] = 0
}

# g coefficient matrix

g = diag(total_nodes)* (1 + 2*r*gamma*((1/dx2) + (1/dy2)))

diag(g[-1,]) = (-r*gamma/dx2)
diag(g[,-1]) = (-r*gamma/dx2)

diag(g[(x_nodes + 1):total_nodes,]) = (-r*gamma/dy2)
diag(g[,(x_nodes + 1):total_nodes]) = (-r*gamma/dy2)

for (i in 1:(y_nodes - 1))
{
  g[(i * x_nodes),(i * x_nodes + 1)] = 0
  g[(i * x_nodes + 1),(i * x_nodes)] = 0
}

# Inital h_old and hh before loop----------------------------------------------

h_old = h_initial

hh = h_initial

# solving new matrix with timesteps--------------------------------------------

for (i in seq(1, n, by = dn))
{
  h_new = solve(g, h%*%h_old)
  hh = cbind(hh,h_new)
  h_old = h_new
}

z1 = matrix(hh[,2], x_nodes,y_nodes)

i = matrix(10 , x_nodes, 1)
j = matrix(10, 1, y_nodes + 2)

z1 = cbind(i, z1)
z1 = cbind(z1, i)
z1 = rbind(j, z1)
z1 = rbind(z1, j)

plot_ly(z = matrix(z1 , x_nodes + 2 , y_nodes + 2)) %>%
  add_surface()

z5 = matrix(hh[,6], x_nodes,y_nodes)
z5 = cbind(i, z5)
z5 = cbind(z5, i)
z5 = rbind(j, z5)
z5 = rbind(z5, j)

plot_ly(z = matrix(z5 , x_nodes + 2 , y_nodes + 2)) %>%
  add_surface()
