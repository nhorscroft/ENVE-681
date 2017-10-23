# This program solves the Laplace equation for a rectangular aquifer, 400m
# long (x-direction) by 200m long (y-direction) with fixed head values of
# 100m on the right side and 0 everywhere else. Grid spacing is 50m in both
# x- and y- directions

library(tidyverse)
library(plotly)

# Define variables
xlength = 400  # length in x- direction (m) 
ylength = 200  # length in y-direction (m)
del = 50       # delx = dely = 50m
xnodes=xlength/del-1
ynodes=ylength/del-1
totalnodes=xnodes*ynodes

# Define rhs matrix and define right boundary conditions
rhs=rep(0,totalnodes)
for (i in 1:ynodes)
{
  rhs[i*xnodes]=-100
}

# Define coefficient matrix
A=diag(totalnodes)*-4
diag(A[-1,])=1
diag(A[,-1])=1
diag(A[(xnodes+1):totalnodes,])=1
diag(A[,(xnodes+1):totalnodes])=1
# Modify coefficient matrix for boundary conditions
for (i in 1:(ynodes-1))
{
  A[i*xnodes,i*xnodes+1]=0
  A[i*xnodes+1,i*xnodes]=0
}

# Solve for h
h=solve(A,rhs)
# Reassemble h into matrix that matches nodes
z=matrix(h,nrow=xnodes,ncol=ynodes)

plot_ly(z=matrix(h,nrow=xnodes,ncol=ynodes)) %>%
  add_surface()

