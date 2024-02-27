# Set the parameters for the AR(1) model
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
phi <- 0.6  # Autoregressive coefficient
sigma <- 0.5  # Standard deviation of the error term
n_years = 20
n <- 12*n_years    # Number of time points

# Simulate AR(1) time series data
set.seed(123)  # Set seed for reproducibility
ar1_data <- arima.sim(model = list(ar = phi), n = n, sd = sigma)

index <- 1:n
harm1 <- sin(2*pi*index/12)

# Plot the simulated time series data
plot(ar1_data, type = 'l', main = 'Simulated AR(1) Time Series', ylab = 'Case Counts')
points(harm1, type='l', col='red')

comb <- exp(2 +ar1_data + harm1)

case1 <- rpois(n=n,lambda=comb)
plot(case1, type='l')


ds1 <- cbind.data.frame(N_cases=case1,date=seq.Date(from=as.Date('2000-01-01'), length.out=n, by='month')) %>%
  mutate(monthN=month(date), epidemic_flag=-999, threshold=999) 

dates.test <- unique(ds1$date)[-c(1:60)] #exclude first 5 years

ds1.in <- ds1 %>% filter(!(date %in% dates.test)) 


for(i in 1:length(dates.test)){
  X=dates.test[i]
  ds1.in <- bind_rows(ds1.in, ds1[ds1$date==X,]) #add on latest observation 
  month.test <- month(X)
  ds1.test <- ds1.in %>%
    filter(date <= X & monthN == month.test & epidemic_flag <=0 ) %>% #select the same month for all years, excluding past epidemics
    mutate(ma5=rollapply(N_cases,width=5,mean,align='right',fill=NA, partial=T), #5 most recent NON EPIDEMIC years
           sd5=rollapply(N_cases,width=5,sd,align='right',fill=NA, partial=T),
           ma5_lag = lag(ma5),
           sd5_lag=lag(sd5)) %>%
    ungroup() %>%
    arrange(date) %>%
    filter(date==X) %>%
    dplyr::select(date,N_cases,ma5_lag, sd5_lag,threshold, epidemic_flag) 
  
 #forthe most recent date, determine if an epidemic has occurred
  ds1.in <- ds1.in %>%
    mutate( threshold= if_else(date==X,ds1.test$ma5_lag + 2*ds1.test$sd5_lag ,threshold),
            epidemic_flag = if_else(date==X,1*(ds1.test$N_cases>threshold),epidemic_flag)
            )

}

ds2 <- ds1.in %>%
  filter( epidemic_flag!=-999)
ggplot(ds2, aes(x=date, y=N_cases)) +
  geom_point(aes(color=epidemic_flag)) +
  theme_classic() +
  geom_line(aes(x=date, y=threshold))







