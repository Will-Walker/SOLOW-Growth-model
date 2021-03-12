#This script replicates the solow model charts shown in the slides for macro lecture 2 12/03/2021

# packages ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(gridExtra)
library(reshape2)

# toy data ----------------------------------------------------------------

#capital stock rages from 0 to 400
K <- seq(from = 0, to = 400, by = 4)
#N (labour force) is fixed at 10
N <- rep(10, times = 100)

#x is arbitrarily set to 0.2
x <- 0.2

#merge test data
test_data <- as.data.frame(cbind(K,N))

# simple solow property demonstration  ------------------------------------

#show output assuming Y = K^(x)*N^(1-x)
#marginal product of capital: is first derivative of Y function with respect to K
#FOC = x*K^(x-1)*N^(1-x)
simulated_output <- test_data %>% 
  mutate(output = (K^x)*(N^(1-x)),
         FOC = x*K^(x-1)*N^(1-x)) 

#generate output line
output <- ggplot(simulated_output) +
  geom_line(aes(x = K, y = output )) +
  labs(x = "Capital (K)", y = "Output (Y)", title = "Output") + 
  theme_classic()

#show FOC line
MPK <- ggplot(simulated_output) +
  geom_line(aes(x = K, y = FOC)) +
  labs(x = "Capital (K)", y = "MPK (dY/dK)", title = "MPK") + 
  theme_classic()

#merge
grid.arrange(output,MPK, ncol = 1)
 
# convergence: steady state capital accumulation ---------------------------------------

#its amazing how sensitive the economy is to these parameters!

#set initial exogenous parameters:
depreciation <- 0.1 #this economies machines rust very slowly
savings_rate <- 0.9 #this economy is obsessed with saving
productivity <- 1 #meaningless index of "productivity" - this will rise gradually over time
productivity_growth <- 0.2
x <- 0.1 #this economy is not capital intensive
N <- 10 #N is fixed
experiment_length <- 75 # dont want the experiment to go on forever!

#Evolution of capital is described by:

#K(t+1) = (1 - depreciation)*K + savings_rate*productivity(t)*(K^(x))*(N^(1-x))

#productivity(t + 1) =  productivity(t) + productivity_growth

#to demonstrate convergence we set a couple different starting K
Initial_capital_stock_low <- 10 #K could evolve over time starting at 10
Initial_capital_stock_extreme <- 1000 #imagine that a warehouse full of tractors was sent back to the middle ages...

#run for low K economy

#limit number of repeats due to highly sensitive properties of economy
counter <- 0

while(counter < experiment_length) {
  
  counter <- counter + 1
  
  if(counter == 1) {
    
    K <- Initial_capital_stock_low
    
    K_low_start <- Initial_capital_stock_low
    
  } else {
    
    #exogenous tech growth
    productivity <- productivity + productivity_growth
    
    K <- (1 - depreciation)*K + savings_rate*productivity*(K^(x))*(N^(1-x))
    
  }
  
  K_low_start <- c(K_low_start, K)
  
}

#run for high K economy

#limit number of repeats due to highly sensitive properties of economy
counter <- 0

#initialize productivity
productivity <- 1

while(counter < experiment_length) {
  
  counter <- counter + 1
  
  if(counter == 1) {
    
    K <- Initial_capital_stock_extreme
    
    K_extreme_start <- Initial_capital_stock_extreme
    
  } else {
    
    #exogenous tech growth
    productivity <- productivity + productivity_growth
    
    K <- (1 - depreciation)*K + savings_rate*productivity*(K^(x))*(N^(1-x))
    
  }
  
  K_extreme_start <- c(K_extreme_start, K)
  
}

#merge runs and plot economies

K_convergance_data <- as.data.frame(cbind(K_low_start, K_extreme_start)) %>% 
  mutate(t = row_number()) %>% 
  melt(id.vars = "t") %>% 
  rename("Economy" = variable,
         "kt = (1-d)k + sAk^x" = value)

ggplot(K_convergance_data, aes(y = `kt = (1-d)k + sAk^x`, x = t, colour = Economy)) + 
  geom_line() + 
  labs(title = "Solow capital convergance") + 
  theme_classic() 









