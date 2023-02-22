#QTL exploration
library(dplyr)
#M21-199 min 100 for 6-17 and min 50 for 6-11
#approx 250 subjects
n <- 250
#drop out rate 
p <- 0.1
OE_data <- rbinom(1000,n,0.1)
#tolerance limit
quantile(OE_data,0.99)

#simulate 20 trials for OE difference chart
#function to substract calculate expectations
E_fun <- function(x,p){
  n <- order(x)
  return(x-n*p)
}

n_sim <- 20
my_data <- sapply(rep(n,n_sim),rbinom,size = 1,prob = 0.1) %>% 
  apply(2,cumsum) %>% 
  apply(2,E_fun,p = p)
my_data <- rbind(0,my_data)
plot_data <- data.frame(X1 = as.numeric(cbind(rep(1:(n+1),n_sim))),
                        X2 = as.character(rep(1:n_sim,each=n+1)), 
                        X3 = as.numeric(c(my_data)))
library(ggplot2)
ggp <- ggplot(plot_data, aes(X1, X3, col = X2)) +             # Create ggplot2 plot
  geom_line()
ggp          
