# Homework 5 Transient 1D Problem ------------------
# 11/13/17
#---------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)


# Define input variables-------------------------------------------------------

# l = length (m)
l = 400

# dx = change in length (m)
dx = 10

# d = depth (m)
d = 4

# t = time to run model (days)
t = 3000

# dt = timestep (days)
dt = 1

# Ks = hydraulic conductivity (m/day)
Ks = 5

# Sy = specific yield
Sy = 0.8

# T = transmissivity (m2/day)
T = Ks*d

# gamma = crank-nicholson/central difference value
gamma = 1/2

# x_nodes = number of nodes in x direction not inluding the border
x_nodes = (l/dx) - 1

# total_nodes = total number of nodes in system
total_nodes = x_nodes

# r = (T*dt)/(Sy*(dx^2))
r = (T*dt)/(Sy*(dx^2))

# h_initial = initial head condition matrix at t = 0
h_initial = matrix(0.6, nrow = x_nodes, ncol = 1)

# Create coefficient matrix for 1D Transient FD/CD ----------------------------

# equation for 1D transient FD/CD: h(n+1, i) = rh(n, i+1) + (1-2r)h(n,i) + rh(n, i-1)

# W = matrix for 1D transient FD/CD model
W = diag(total_nodes) * (1-2*r)
diag(W[-1,]) = (r)
diag(W[,-1]) = (r)


# Solve for 1D transient FD/CD h_new  -----------------------------------------
h_old = h_initial
hh = h_initial

for(i in seq(1, t, by = dt))
  {
  h_new = W%*%h_old
  hh = cbind(hh, h_new)
  h_old = h_new
}

# Convert to data frame and graph for 1d transient FD/CD ----------------------

bc <- matrix(0, nrow = 1, ncol = t+1)

hh <- rbind(bc, hh)
hh <- rbind(hh, bc)


hh_graph_FD <- data.frame(x = seq(0, l, by = dx), 
                       hh[,1], hh[,251], hh[,501], 
                       hh[,1001], hh[,1501], hh[,3001]) %>% 
  rename("Initial" = hh...1.,
         "250 days" = hh...251.,
         "500 days" = hh...501.,
         "1000 days" = hh...1001.,
         "1500 days" = hh...1501.,
         "3000 days" = hh...3001.) %>% 
  gather(Time, head, "Initial":"3000 days") %>% 
  mutate(Time = factor(Time, levels = unique(Time)))

  
hh_graph_FD %>%  
  ggplot(aes(x = x, y = head, group = Time, colour = Time, shape = Time)) +
  geom_point(size = 2)+
  scale_shape_manual(values = c(0:5)) +
  scale_x_continuous(breaks = seq(0, l, by = 50)) +
  scale_y_continuous(breaks = seq(0, 0.65, by = 0.1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())+
  labs(x = "Distance (m)",
       y = "Head \n(m)",
       title = "Change in Water Table over Time",
       subtitle = "Explicit FD/CD 1D Boussinesq Equation")
  
# coefficient matrices for Crank-Nicholson/Central Difference -----------------

# equation for crank nicholoson/central difference: 

# (1+ 2rgamma)h(n+1, i) - rgamma(h(n+1, i+1) +  h(n+1, i-1) 
# = (1-2rgamma)h(n,i) + rgamma(h(n, i+1) + rh(n, i-1))   

# H = (1-2rgamma)h(n,i) + rgamma(h(n, i+1) + rh(n, i-1))
H = diag(total_nodes) * (1-2*r*gamma)
diag(H[-1,]) = (r*gamma)
diag(H[,-1]) = (r*gamma)



# G = (1+ 2rgamma)h(n+1, i) - rgamma(h(n+1, i+1) +  h(n+1, i-1)
G = diag(total_nodes) * (1+2*r*gamma)
diag(G[-1,]) = -(r*gamma)
diag(G[,-1]) = -(r*gamma)


# Solve for crank nicholson/central difference --------------------------------
h_old = h_initial
hh_2 = h_initial

for(i in seq(1, t, by = dt))
{
  h_new = solve(G, H%*%h_old)
  hh_2 = cbind(hh_2, h_new)
  h_old = h_new
}

hh_2 <- rbind(bc, hh_2)
hh_2 <- rbind(hh_2, bc)

# Convert to data frame and graph for 1d transient CN/CD ----------------------

hh_graph_CN <- data.frame(x = seq(0, l, by = dx), 
                       hh_2[,1], hh_2[,251], hh_2[,501], 
                       hh_2[,1001], hh_2[,1501], hh_2[,3001]) %>% 
  rename("Initial" = hh_2...1.,
         "250 days" = hh_2...251.,
         "500 days" = hh_2...501.,
         "1000 days" = hh_2...1001.,
         "1500 days" = hh_2...1501.,
         "3000 days" = hh_2...3001.) %>% 
  gather(Time, head, "Initial":"3000 days") %>% 
  mutate(Time = factor(Time, levels = unique(Time)))


hh_graph_CN %>%  
  ggplot(aes(x = x, y = head, group = Time, colour = Time, shape = Time)) +
  geom_point(size = 2)+
  scale_shape_manual(values = c(0:5)) +
  scale_x_continuous(breaks = seq(0, l, by = 50)) +
  scale_y_continuous(breaks = seq(0, 0.65, by = 0.1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())+
  labs(x = "Distance (m)",
       y = "Head \n(m)",
       title = "Change in Water Table over Time",
       subtitle = "Crank Nicolson/ Central Difference 1D Boussinesq Equation")




