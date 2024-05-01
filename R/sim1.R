# Set the parameters for the AR(1) model
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(exactci)
phi <- 0.6  # Autoregressive coefficient
sigma <- 0.0001  # Standard deviation of the error term
n_years = 50
n <- 12*n_years    # Number of time points

# Simulate AR(1) time series data
set.seed(12345)  # Set seed for reproducibility
#ar1_data <- arima.sim(model = list(ar = phi), n = n, sd = sigma)

index <- 1:n
harm1 <- sin(2*pi*index/12)

# Plot the simulated time series data
plot(ar1_data, type = 'l', main = 'Simulated AR(1) Time Series', ylab = 'Case Counts')
points(harm1, type='l', col='red')

#comb <- exp(0 +ar1_data + harm1)
comb <- exp(1 + harm1) #harmonic only

case1 <- rpois(n=n,lambda=comb)
plot(case1, type='l')


ds1 <- cbind.data.frame(N_cases=case1,date=seq.Date(from=as.Date('2000-01-01'), length.out=n, by='month')) %>%
  mutate(monthN=month(date), 
         epidemic_flag=-999, 
         threshold=-999,
         
         epidemic_flag_poisson=-999,
         threshold_poisson=-999,
         
         epidemic_flag_quant=-999,
         threshold_quant=-999) 

dates.test <- unique(ds1$date)[-c(1:60)] #exclude first 5 years

ds1.in <- ds1.in %>% filter(!(date %in% dates.test)) 


#NOE TO FIX--THIS ISN"T CURRENTLY CORRECT--most.recent is taking 5 most recent, but it should be 5 most recent not including current time
for(i in 1:length(dates.test)){
  X=dates.test[i]
  month.test <- month(X)
  
  most.recent <- ds1.in %>%
    filter(date < X & monthN == month.test ) %>% #& epidemic_flag <=0 ) %>% #select the same month for all years, not including current month
    arrange(date) %>%
    mutate(order=row_number(),
           rev_order= max(order)- order+ 1) %>%
    filter(rev_order<=5)

  #mean with SD
  ds1.test_mean_sd <- most.recent %>%
    summarize(n_times=n(),
           ma5=mean(N_cases,na.rm=T), #5 most recent NON EPIDEMIC years
           sd5=sd(N_cases, na.rm=T)
           ) %>%
    mutate(date=X)  
  
  #quantile
  ds1.test_quant <- ds1.in %>%  #note uses whole data, not just most recent 5
    filter(date < X & monthN == month.test ) %>% #& epidemic_flag <=0 ) %>% #select the same month for all years, excluding past epidemics
    summarize(n_times=n(),
           mquant=quantile(N_cases, probs=0.95),
    ) %>%
    ungroup() %>%
    mutate(date=X)
  #poisson
  #pois.pred.interval= quantile(rpois(100000, lambda=mean.x), probs=0.975)

  #poisson prediction interval
  mod1.coef <-summary(glm(N_cases ~1, family='poisson', data=most.recent))$coefficients['(Intercept)',c('Estimate','Std. Error')]
  
    pois.ucl= quantile(rpois(1000, lambda=exp(mod1.coef['Estimate'] +rnorm(1000,mean=0, sd=mod1.coef['Std. Error']))), probs=0.975)
    
    #for the most recent date, determine if an epidemic has occurred
    ds1.in <- bind_rows(ds1.in, ds1[ds1$date==X,]) #add on latest observation 
    
    ds1.in <- ds1.in %>%
    mutate( threshold= if_else(date==X,ds1.test_mean_sd$ma5 + 2*ds1.test_mean_sd$sd5 ,threshold),
            epidemic_flag = if_else(date==X,1*(N_cases>threshold),epidemic_flag),
    
            threshold_poisson= if_else(date==X,pois.ucl ,threshold_poisson),
            epidemic_flag_poisson = if_else(date==X,1*(N_cases>threshold_poisson),epidemic_flag_poisson),
            
            threshold_quant= if_else(date==X,ds1.test_quant$mquant ,threshold_quant),
            epidemic_flag_quant = if_else(date==X,1*(N_cases>threshold_quant),epidemic_flag_quant),
            
            )

}

ds2_pois <- ds1.in %>%
  filter( epidemic_flag_poisson!=-999)
ggplot(ds2_pois, aes(x=date, y=N_cases)) +
  geom_line(col='gray') +
  theme_classic() +
  geom_line(aes(x=date, y=threshold_poisson))+
  geom_line(aes(x=date, y=threshold), col='red')+
  geom_line(aes(x=date, y=threshold_quant), col='blue')

mean(ds2_pois$epidemic_flag_pois)
mean(ds2_pois$epidemic_flag_quant)
mean(ds2_pois$epidemic_flag)





#check
y1 <-rpois(10000, lambda=2)
meany=mean(y1)
sy=sd(y1)
ucly= meany + 2*sy
mean(y1>ucly)




#test prediction intevral for confidence interval
x1 <- c(4,6,7,11,8)
mod1 <- lm(x1 ~1)
summary(mod1)

#mean(x1)
#sd(x1)/sqrt(5)
#empirical upper CI
mean(x1) + 1.96*sd(x1)/sqrt(5)

predict(mod1,  interval="confidence",level = 0.95)[1, "upr"] 
predict(mod1,  interval="predict",level = 0.95)[1, "upr"]

############manually calculate it
# Confidence level (e.g., 95%)
confidence_level <- 0.95

# Calculate the mean and standard deviation of the data
mean_value <- mean(x1)
sd_value <- sd(x1)

# Degrees of freedom for normal distribution
df <- length(x1) - 1

# Calculate standard error
standard_error <- sd_value / sqrt(length(x1))

# Calculate margin of error using the normal distribution
margin_of_error <- qnorm((1 + confidence_level) / 2, mean = 0, sd = 1) * standard_error

# Calculate prediction interval
lower_bound <- mean_value - margin_of_error
upper_bound <- mean_value + margin_of_error

# Print the results
cat("Mean:", mean_value, "\n")
cat("Prediction Interval:", lower_bound, "to", upper_bound, "\n")

#poisson variable
set.mean <- 20
set.seed(123)
x2 <- rpois(100000, lambda=set.mean)
mean.x <- mean(x2)
ucl.x2 <- mean.x + 1.96*sqrt(mean.x)
ucl2.x2 <- mean.x + 1.96*sd(x2)

#generate another dataset:
x3 <- rpois(100000, lambda=set.mean)


pois.pred.interval= quantile(rpois(100000, lambda=mean.x), probs=0.975)
mean(x3>ucl.x2) # about right for a 2 sideD CI
mean(x3>ucl2.x2) # about right for a 2 sideD CI
mean(x3>pois.pred.interval) #too conservative


set.seed(123)
hist.data <- rpois(n=5, lambda=3)

mod1 <- summary(glm(hist.data ~1, family='poisson'))
mod1.coef <- mod1$coefficients['(Intercept)',c('Estimate','Std. Error')]
#pois.ucl= quantile(rpois(1000, lambda=exp(mod1.coef['Estimate'] +rnorm(1000,mean=0, sd=mod1.coef['Std. Error']))), probs=0.975)
pois.ucl= quantile(rpois(1000, lambda=exp(mod1.coef['Estimate'] )), probs=0.975)
pois.ucl

new.data <- rpois(n=10000, lambda=3)

mean(new.data>pois.ucl)

pois_pred_int <- function(X){
  mod1 <- summary(glm(X ~1, family='poisson'))
  mod1.coef <- mod1$coefficients['(Intercept)',c('Estimate','Std. Error')]
}
