######
# ASSIGNMENT STATISTICS 40991
# Presented by: Juan David Garcia Gonzalez
# Due Date: October 24 2022
######

## Setting the Working Directory
setwd("C:/Users/Juan D Garcia/OneDrive - University College London/Desktop/Bocconi University/PhD Studies/Statistics_Bonetti/RScripts")
getwd()

###
# POINT 1)
###

## Given ascendant ordered samples (X1, X2, ..., X(n-1), Xn), compute 
## the following statistics:
# 1) X1: the minimum value of the sample.
# 2) X((n+1)/2): the median of the sample (for an uneven number of n). 
# Note: in this case all the n sample are uneven. 
# 3) Xn: the maximum value of the sample.
# 4) only for curiosity and learning reasons, the mean will be also calculated.

## Generating empty result vectors 
k <- 10000    # Number of values
resmin3 <- resmedian3 <- resmax3 <- resmean3 <- rep(NA,k)
resmin5 <- resmedian5 <- resmax5 <- resmean5 <- rep(NA,k)
resmin7 <- resmedian7 <- resmax7 <- resmean7 <- rep(NA,k)
resmin15 <- resmedian15 <- resmax15 <- resmean15 <- rep(NA,k)
resmin31 <- resmedian31 <- resmax31 <- resmean31 <- rep(NA,k)
resmin51 <- resmedian51 <- resmax51 <- resmean51 <- rep(NA,k)
resmin101 <- resmedian101 <- resmax101 <- resmean101 <- rep(NA,k)

## Generating the loops for many (in this case k is set to 10.000) iid 
## samples from the U(0,1) for n <- c(3,5,7,15,31,51,101)
n <- 101       # I have successively change n and name of variables
set.seed(40991)
for(i in 1:k)
  {
    gensample <- runif(n,0,1)
    resmin101[i] <- min(gensample)
    resmedian101[i] <- median(gensample)
    resmax101[i] <- max(gensample)
    resmean101[i] <- mean(gensample)
} 

## Saving the results into data frames to facilitate analysis
resmin <- as.data.frame(cbind(resmin3, resmin5, resmin7, resmin15,
                          resmin31, resmin51, resmin101))
resmedian <- as.data.frame(cbind(resmedian3, resmedian5, resmedian7,
                                 resmedian15, resmedian31, resmedian51, 
                                 resmedian101))
resmean <- as.data.frame(cbind(resmean3, resmean5, resmean7,
                                 resmean15, resmean31, resmean51, 
                                 resmean101))
resmax <- as.data.frame(cbind(resmax3, resmax5, resmax7, resmax15,
                              resmax31, resmax51, resmax101))

results <- as.data.frame(cbind(resmin, resmedian, resmean, resmax))
write.csv2(results, "1_Uniform.csv", row.names = FALSE)

## Visualization of results
# Defining legends, colors and number of plots
boxplotleg <- c("3","5","7","15","31","51","101")
boxplotcol <- c("gray80", "gray70", "gray60", "gray50",
                "gray40", "gray30", "gray20")
par(mfrow=c(1,2))

# Boxplots: Min&Max
boxplot(resmax,
        xlab = "n samples",
        ylab = "Max",
        main = "Maximum Value (Max) Results",
        names = boxplotleg, 
        col = boxplotcol)
abline(h=1, col = "darkblue", lwd = 2, lty = 2)
boxplot(resmin,
        xlab = "n samples",
        ylab = "Min",
        main = "Minimum Value (Min) Results",
        col = boxplotcol,
        names = boxplotleg)
abline(h=0, col = "darkblue", lwd = 2, lty = 2)

# Boxplots: Med&Mean
boxplot(resmedian,
        xlab = "n samples",
        ylab = "Med",
        main = "Median Value (Med) Results",
        names = boxplotleg, 
        col = boxplotcol)
abline(h=0.5, col = "darkblue", lwd = 2, lty = 2)
boxplot(resmean,
        xlab = "n samples",
        ylab = "Mean",
        main = "Mean Value (Mean) Results",
        col = boxplotcol,
        names = boxplotleg)
abline(h=0.5, col = "darkblue", lwd = 2, lty = 2)

# Visualization by histograms
# Note: vertical line at the E[U(0,1)] = 0.5
# Defining legends, colors and number of plots
par(mfrow=c(2,2))
hislabels <- c("MIN", "MAX", "MED")
hiscol <- c("darkblue", "darkgreen", "gray")

hist(resmin3, col = "darkblue", 
     ylim = c(0, 5000), xlim = c(0,1), main = "MIN (blue), MED (gray) & MAX (green) with different n
     
     n = 3", xlab = "")
hist(resmax3, add=TRUE, col = "darkgreen")
hist(resmedian3, add= TRUE, col= "gray")
abline(v=0.5, col = "gray", lwd = 2, lty = 2)

hist(resmin7, col = "darkblue", ylim = c(0, 5000),
     xlim = c(0,1), main = "n = 7", xlab = "")
hist(resmax7, add=TRUE, col = "darkgreen")
hist(resmedian7, add= TRUE, col= "gray")
abline(v=0.5, col = "gray", lwd = 2, lty = 2)

hist(resmin31, col = "darkblue", ylim = c(0, 5000), 
     xlim = c(0,1), main = "n = 31", xlab = "")
hist(resmax31, add=TRUE, col = "darkgreen")
hist(resmedian31, add= TRUE, col= "gray")
abline(v=0.5, col = "gray", lwd = 2, lty = 2)

hist(resmin101, col = "darkblue", ylim = c(0, 5000),
     xlim = c(0,1), main = "n = 101", xlab = "")
hist(resmax101, add=TRUE, col = "darkgreen")
hist(resmedian101, add= TRUE, col= "gray")
abline(v=0.5, col = "gray", lwd = 2, lty = 2)

## EXPONENTIAL DISTRIBUTION: Generating the loops for many (in this case k
# is set to 10.000) iid Samples from the Exp(lamda), for n <- c(3,5,7,15,31,51,101).

# Generating empty result vectors 
k <- 10000    # Number of values
resmin3 <- resmedian3 <- resmax3 <- resmean3 <- rep(NA,k)
resmin5 <- resmedian5 <- resmax5 <- resmean5 <- rep(NA,k)
resmin7 <- resmedian7 <- resmax7 <- resmean7 <- rep(NA,k)
resmin15 <- resmedian15 <- resmax15 <- resmean15 <- rep(NA,k)
resmin31 <- resmedian31 <- resmax31 <- resmean31 <- rep(NA,k)
resmin51 <- resmedian51 <- resmax51 <- resmean51 <- rep(NA,k)
resmin101 <- resmedian101 <- resmax101 <- resmean101 <- rep(NA,k)

## Generating the loops for k=10.000 and n <- c(3,5,7,15,31,51,101)
lambda <- 0.75  # lamda was set to 0.75
n <- 101        # I have successively change n and name of variables
set.seed(40991)
for(i in 1:k)
{
  gensample <- rexp(n,lambda)
  resmin101[i] <- min(gensample)
  resmedian101[i] <- median(gensample)
  resmax101[i] <- max(gensample)
  resmean101[i] <- mean(gensample)
} 

## Saving the results in data frames to facilitate analysis
resmin <- as.data.frame(cbind(resmin3, resmin5, resmin7, resmin15,
                              resmin31, resmin51, resmin101))
resmedian <- as.data.frame(cbind(resmedian3, resmedian5, resmedian7,
                                 resmedian15, resmedian31, resmedian51, 
                                 resmedian101))
resmean <- as.data.frame(cbind(resmean3, resmean5, resmean7,
                               resmean15, resmean31, resmean51, 
                               resmean101))
resmax <- as.data.frame(cbind(resmax3, resmax5, resmax7, resmax15,
                              resmax31, resmax51, resmax101))
results <- as.data.frame(cbind(resmin, resmedian, resmean, resmax))
write.csv2(results, "1_Exp.csv", row.names = FALSE)

## Visualization of results
# Boxplots: Max&Min
par(mfrow=c(1,2))
boxplot(resmax,
        xlab = "n samples",
        ylab = "Max",
        main = "Maximum Value (Max) Results",
        names = boxplotleg, 
        col = boxplotcol)
boxplot(resmin,
        xlab = "n samples",
        ylab = "Min",
        main = "Minimum Value (Min) Results",
        col = boxplotcol,
        names = boxplotleg)
abline(h=0, col = "darkblue", lwd = 2, lty = 2)

# Boxplots: Med&Mean
boxplot(resmedian,
        xlab = "n samples",
        ylab = "Med",
        main = "Median Value (Med) Results",
        names = boxplotleg, 
        col = boxplotcol)
abline(h=(log(2)/lambda), col = "darkblue", lwd = 2, lty = 2)
# Note: horizontal line at the Median[exp(lamda)] = (ln(2)/lambda) = 0.9241962
boxplot(resmean,
        xlab = "n samples",
        ylab = "Mean",
        main = "Mean Value (Mean) Results",
        col = boxplotcol,
        names = boxplotleg)
abline(h=(1/lambda), col = "darkblue", lwd = 2, lty = 2)
# Note: horizontal line at the E[exp(lamda)] = 1/lambda = 1.333333

# Visualization by histograms: Min, Max & Med
# Note 1: vertical lines at the Median[exp(lamda)] = (ln(2)/lambda)
# Note 2: xlim set to c(0,8), then information of the distribution of MAX is lost
par(mfrow=c(2,2))

hislabels <- c("MIN", "MAX", "MED")
hiscol <- c("darkblue", "darkgreen", "gray")

hist(resmin3, col = "darkblue", 
     ylim = c(0, 5000), xlim = c(0,8), main = "MIN (blue), MED (gray) & MAX (green) with different n
     
     n = 3", xlab = "")
hist(resmax3, add=TRUE, col = "darkgreen")
hist(resmedian3, add= TRUE, col= "gray")
abline(v=(log(2)/lambda), col = "gray", lwd = 2, lty = 2)

hist(resmin7, col = "darkblue", ylim = c(0, 5000),
     xlim = c(0,8), main = "n = 7", xlab = "")
hist(resmax7, add=TRUE, col = "darkgreen")
hist(resmedian7, add= TRUE, col= "gray")
abline(v=(log(2)/lambda), col = "gray", lwd = 2, lty = 2)

hist(resmin31, col = "darkblue", ylim = c(0, 5000), 
     xlim = c(0,8), main = "n = 31", xlab = "")
hist(resmax31, add=TRUE, col = "darkgreen")
hist(resmedian31, add= TRUE, col= "gray")
abline(v=(log(2)/lambda), col = "gray", lwd = 2, lty = 2)

hist(resmin101, col = "darkblue", ylim = c(0, 5000),
     xlim = c(0,8), main = "n = 101", xlab = "")
hist(resmax101, add=TRUE, col = "darkgreen")
hist(resmedian101, add= TRUE, col= "gray")
abline(v=(log(2)/lambda), col = "gray", lwd = 2, lty = 2)


###
# POINT 2
#
# POINT 2. (a)
###

## Generating the loops (k= 10.000) iid samples from the N(10,1)
## for n <- c(10,50,100, 1000), use (i.e. optim() function) to 
## compute the MLE for the mean and the sd (k=10.000)

## Generating empty result vectors
k <- 10000
resmean10 <- resmean50 <- resmean100 <- resmean1000 <- rep(NA,k)
ressd10 <- ressd50 <- ressd100 <- ressd1000 <- rep(NA,k)

## Defining the Loglikelihood function and its negative form
loglikeli <- function (param){
  negloglikeli <- -sum(dnorm(x, param[1], param[2], log = TRUE))
  return(negloglikeli)
}

## Sampling and optimizing
p <- c(20,5)           # par argument in optim () function. Discretional choice.
                       # par argument are the initial values for the parameters
n <- 1000                # I have successively change n and name of variables
set.seed(40991)
for(i in 1:k)
{
  x <- rnorm(n,10,1)
  res <-  optim(par = p, loglikeli, hessian = TRUE)
  resmean1000[i] <- res$par[1]      # Successively change the variable name
  ressd1000[i] <- res$par[2]        # Successively change the variable name
}
Hessian1000<- res$hessian           # successively change the variable name
Inverse1000<- solve(res$hessian)    # successively change the variable name

## Saving and organizing results to facilitate analysis
resmean <- cbind(resmean10, resmean50, resmean100, resmean1000)
ressd <- cbind(ressd10, ressd50, ressd100, ressd1000)
res_export <- as.data.frame(cbind(resmean, ressd))
write.csv2(res_export, file = "2a_Optim_Results_Normal.csv", row.names = FALSE)
# res_export <- read.csv2(file = "2a_Optim_Results_Normal.csv")

## Analyzing results with a summary
summary(res_export)

## Visualization of results

# ggplot visualization: comparing estimators with real values
install.packages("tidyverse")
library(tidyverse)
tab_resmeann <- rbind("n = 0010", "n = 0050", "n = 0100", "n = 1000")
tab_resmean1 <- rbind(mean(resmean10), mean(resmean50), 
          mean(resmean100), mean(resmean1000))
tab_resmean2 <- rbind(sd(resmean10), sd(resmean50), 
                      sd(resmean100), sd(resmean1000))
tab_ressd1 <- rbind(mean(ressd10), mean(ressd50), 
                      mean(ressd100), mean(ressd1000))
tab_ressd2 <- rbind(sd(ressd10), sd(ressd50), 
                      sd(ressd100), sd(ressd1000))
tab_resmean <- as.data.frame(cbind(tab_resmeann, tab_resmean1, 
                                   tab_resmean2, tab_ressd1, tab_ressd2))
write.csv2(tab_resmean, file = "2a_Results_Mean&Sd.csv", row.names = FALSE)
# Note: V1 = n size, V2 = mean(resmean), v3 = sd(resmean), V4 = mean(ressd), v5 = sd(ressd)

# Mean of resmean
ggplot(data=tab_resmean, aes(y=tab_resmean1, x=tab_resmeann, group=1)) +
  geom_line()+
  geom_point() + 
  ggtitle("MLE Mean") +
  xlab("n") +
  ylab("Mean") +
  theme_classic()

# Mean of ressd
ggplot(data=tab_resmean, aes(y=tab_ressd1, x=tab_resmeann, group=1)) +
  geom_line()+
  geom_point() + 
  ggtitle("MLE Standard Deviation") +
  xlab("n") +
  ylab("Mean") +
  theme_classic()

# Boxplots of Mean and Sd Estimators
# Defining legends and colors
boxplotleg <- c("10","50","100","1000")
boxplotcol <- c("gray80", "gray60", "gray40", "gray20")
par(mfrow=c(1,2))
boxplot(resmean,
        xlab = "n samples",
        ylab = "Mean",
        main = "MLE Mean Estimator",
        names = boxplotleg, 
        col = boxplotcol)
abline(h=10, col = "darkblue", lwd = 2, lty = 2)
boxplot(ressd,
        xlab = "n samples",
        ylab = "Sd",
        main = "MLE Standard Deviation Estimator",
        col = boxplotcol,
        names = boxplotleg)
abline(h=1, col = "darkblue", lwd = 2, lty = 2)


###
# POINT 2. (b)
###

# Note of the Hessian: an error when running the code for n=1
# Then, comparing the results from the varcov Matrix when 
# n <- c(10,100,1000)
Inverse10
Inverse50
Inverse100
Inverse1000

# Ploting the variance of estimators
varcov <- as.data.frame(rbind(Inverse10, Inverse50, Inverse100, Inverse1000))
var <- as.data.frame(rbind(diag(Inverse10), diag(Inverse50), 
                              diag(Inverse100), diag(Inverse1000)))
write.csv2(varcov, file = "2b_varcov.csv", row.names = FALSE)

plot(var$V1, type = "l", xaxt="n", col ="darkblue", 
     xlab = "n samples <-  c(10,50,100,1000), left to right", 
     ylab = "Variance of Estimators", ylim = c(0,0.04))
lines(var$V2, col = "darkgreen")
legend("topright", legend=c("var(MLE_mean)", "var(MLE_Sd"), 
       col=c("darkblue", "darkgreen"), lty = 1:2, cex=0.7, bty = "n")

###
# POINT 3
###

## Generating loops (k= 10.000) of iid samples from the Bernoulli(p)
## for n <- c(10,50,100, 1000), use (i.e. optim() function) to 
## compute the MLE for p (resp). Estimate the true confidence interval

## Generating empty result vectors 
# Note: resp_ber generate a vector of 0 and 1, in which xi=1 if the true
# parameter p (t_prob) falls into the confidence interval i. 0 otherwise.
k <- 10000
resp10 <- resp50 <- resp100 <- resp1000 <- rep(NA,k)
resp10_low90 <- resp50_low90 <- resp100_low90 <- resp1000_low90 <- rep(NA,k)
resp10_up90 <- resp50_up90 <- resp100_up90 <- resp1000_up90 <- rep(NA,k)
resp10_ber <- resp50_ber <- resp100_ber <- resp1000_ber <- rep(NA,k)

## Setting the initial parameters
t_prob <- 0.75     # True probability of sample success
p_initial <- 0.5  # par argument in optim () function. Discretional choice.
                  # par is the initial value for the parameter´s optimization.

## Defining the Loglikelihood function and its negative form
loglikeli <- function (param){
  negloglikeli <- -sum(dbinom(x, 1, param[1], log = TRUE))
  return(negloglikeli)
}

## Sampling and optimizing
# Note: For n=10 the method set was "Nelder-Mead" and Hessian = FALSE.
# Default BFGS method requires the gradient of the function being minimized.
# In some cases, it will try to use finite-differences to estimate it (error).
# Error under the default method could be because the vector x creates a
# discontinuity that prevents the numerical gradient from being properly formed. 

## n = 10
n <- 10                 
set.seed(40991)
for(i in 1:k)
{
  x <- rbinom(n, 1, prob = t_prob)
  res <-  optim(par = p_initial, loglikeli, method = "Nelder-Mead")
  resp10[i] <- res$par[1]
  resp10_low90[i] <- resp10[i] - qnorm(0.95)*sqrt((resp10[i]*(1-resp10[i]))/n)
  resp10_up90[i] <- resp10[i] + qnorm(0.95)*sqrt((resp10[i]*(1-resp10[i]))/n)
  resp10_ber[i] <- 1*(t_prob>resp10_low90[i])*(t_prob<resp10_up90[i])
}
# True confidence level at 99% CI
p10_ber <- mean(resp10_ber)
p10_99 <- c(p10_ber - qnorm(0.995)*sqrt(p10_ber*(1-p10_ber)/length(resp10_ber)),
         p10_ber, 
         p10_ber + qnorm(0.995)*sqrt(p10_ber*(1-p10_ber)/length(resp10_ber)))

## n = 50 
n <- 50              
set.seed(40991)
for(i in 1:k)
{
  x <- rbinom(n, 1, prob = t_prob)
  res <-  optim(par = p_initial, loglikeli, hessian = TRUE)
  resp50[i] <- res$par[1]
  resp50_low90[i] <- resp50[i] - qnorm(0.95)*sqrt((resp50[i]*(1-resp50[i]))/n)
  resp50_up90[i] <- resp50[i] + qnorm(0.95)*sqrt((resp50[i]*(1-resp50[i]))/n)
  resp50_ber[i] <- 1*(t_prob>resp50_low90[i])*(t_prob<resp50_up90[i])
}
# True confidence level at 99% CI
p50_ber <- mean(resp50_ber)
p50_99 <- c(p50_ber - qnorm(0.995)*sqrt(p50_ber*(1-p50_ber)/length(resp50_ber)),
            p50_ber, 
            p50_ber + qnorm(0.995)*sqrt(p50_ber*(1-p50_ber)/length(resp50_ber)))


## n = 100
n <- 100              
set.seed(40991)
for(i in 1:k)
{
  x <- rbinom(n, 1, prob = t_prob)
  res <-  optim(par = p_initial, loglikeli, hessian = TRUE)
  resp100[i] <- res$par[1]
  resp100_low90[i] <- resp100[i] - qnorm(0.95)*sqrt((resp100[i]*(1-resp100[i]))/n)
  resp100_up90[i] <- resp100[i] + qnorm(0.95)*sqrt((resp100[i]*(1-resp100[i]))/n)
  resp100_ber[i] <- 1*(t_prob>resp100_low90[i])*(t_prob<resp100_up90[i])
}
# True confidence level at 99% CI
p100_ber <- mean(resp100_ber)
p100_99 <- c(p100_ber - qnorm(0.995)*sqrt(p100_ber*(1-p100_ber)/length(resp100_ber)),
            p100_ber, 
            p100_ber + qnorm(0.995)*sqrt(p100_ber*(1-p100_ber)/length(resp100_ber)))

## n = 1000
n <- 1000              
set.seed(40991)
for(i in 1:k)
{
  x <- rbinom(n, 1, prob = t_prob)
  res <-  optim(par = p_initial, loglikeli, hessian = TRUE)
  resp1000[i] <- res$par[1]
  resp1000_low90[i] <- resp1000[i] - qnorm(0.95)*sqrt((resp1000[i]*(1-resp1000[i]))/n)
  resp1000_up90[i] <- resp1000[i] + qnorm(0.95)*sqrt((resp1000[i]*(1-resp1000[i]))/n)
  resp1000_ber[i] <- 1*(t_prob>resp1000_low90[i])*(t_prob<resp1000_up90[i])
}
# True confidence level at 99% CI
p1000_ber <- mean(resp1000_ber)
p1000_99 <- c(p1000_ber - qnorm(0.995)*sqrt(p1000_ber*(1-p1000_ber)/length(resp1000_ber)),
             p1000_ber, 
             p1000_ber + qnorm(0.995)*sqrt(p1000_ber*(1-p1000_ber)/length(resp1000_ber)))

## Saving the MLE results
resp <- cbind(resp10, resp50, resp100, resp1000)
resp_low90 <- cbind(resp10_low90, resp50_low90, 
                    resp100_low90, resp1000_low90)
resp_up90 <- cbind(resp10_up90, resp50_up90, 
                   resp100_up90, resp1000_up90)
resp_ber <- cbind(resp10_ber, resp50_ber, resp100_ber, resp1000_ber)
resp_export <- as.data.frame(cbind(resp, resp_low90, resp_up90,
                                   resp_ber))
write.csv2(resp_export, file = "3_Optim_Results_Bernoulli.csv", row.names = FALSE)

## Ploting the pMLE
boxplotleg <- c("10","50","100","1000")
boxplotcol <- c("gray80", "gray60", "gray40", "gray20")
boxplot(resp,
        xlab = "n samples",
        ylab = " ",
        main = "MLE p",
        col = boxplotcol,
        names = boxplotleg)
abline(h=t_prob, col = "darkblue", lwd = 2, lty = 2)

## Saving the True confidence level 99%CI results
plow_99 <- c(p10_ber - qnorm(0.995)*sqrt(p10_ber*(1-p10_ber)/length(resp10_ber)),
             p50_ber - qnorm(0.995)*sqrt(p50_ber*(1-p50_ber)/length(resp50_ber)),
             p100_ber - qnorm(0.995)*sqrt(p100_ber*(1-p100_ber)/length(resp100_ber)),
             p1000_ber - qnorm(0.995)*sqrt(p1000_ber*(1-p1000_ber)/length(resp1000_ber)))
p_99 <- c(p10_ber, p50_ber, p100_ber, p1000_ber)
pup_99 <- c(p10_ber + qnorm(0.995)*sqrt(p10_ber*(1-p10_ber)/length(resp10_ber)),
            p50_ber + qnorm(0.995)*sqrt(p50_ber*(1-p50_ber)/length(resp50_ber)),
            p100_ber + qnorm(0.995)*sqrt(p100_ber*(1-p100_ber)/length(resp100_ber)),
            p1000_ber + qnorm(0.995)*sqrt(p1000_ber*(1-p1000_ber)/length(resp1000_ber)))
n <- c(10, 50, 100, 1000)

p <- as.data.frame(cbind(n, plow_99, p_99, pup_99))
write.csv2(p, file = "3_True_p_99CI.csv", row.names = FALSE)
p

## ploting the 99% CI results
install.packages("ggplot2")
library(ggplot2)

ggplot(p_99_CI, aes(levels_p_99, p_99)) +        
  geom_point() +
  geom_errorbar(aes(ymin = plow_99, ymax = pup_99)) +
  xlab("n <- (10, 50, 100, 1000)") + 
  ylab("p") + 
  ggtitle("True Confidence Level 99%") + 
  theme_classic() 

###
# POINT 4
###

## n = 10
# Defining parameters
fit1_10 <- fit2_10 <- se1_10 <- se2_10 <- matrix(ncol = 2, nrow = k)
k <- 1000
n <- 10
# Generating data and lm results
for (i in 1:k){
  set.seed(40991+i)
  # Normal Errors
  Xi_10 <- rnorm(n, 5, 4)
  E1_10 <- rnorm(n, 0, 9)
  B0 <- 10
  B1 <- 2
  Y1_10 <- B0 + (B1*Xi_10) + E1_10
  fit1 <- lm(Y1_10 ~ Xi_10)
  fit1_10[i, ] <- fit1$coefficients
  se1_10[i, ] <- sqrt(diag(vcov((fit1))))
  # Exponential Errors (lambda = 1.2)
  E2_10 <- rexp(n, 1.2)
  Y2_10 <- B0 + (B1*Xi_10) + E2_10
  fit2 <- lm(Y2_10 ~ Xi_10)
  fit2_10[i, ] <- fit2$coefficients
  se2_10[i, ] <- sqrt(diag(vcov((fit2))))
}
summary(fit1_10)
summary(fit2_10)

## n = 50
# Defining parameters
fit1_50 <- fit2_50 <- se1_50 <- se2_50 <- matrix(ncol = 2, nrow = k)
k <- 1000
n <- 50
# Generating data and lm results
for (i in 1:k){
  set.seed(40991+i)
  # Normal Errors
  Xi_50 <- rnorm(n, 5, 4)
  E1_50 <- rnorm(n, 0, 9)
  B0 <- 10
  B1 <- 2
  Y1_50 <- B0 + (B1*Xi_50) + E1_50
  fit1 <- lm(Y1_50 ~ Xi_50)
  fit1_50[i, ] <- fit1$coefficients
  se1_50[i, ] <- sqrt(diag(vcov((fit1))))
  # Exponential Errors (lambda = 1.2)
  E2_50 <- rexp(n, 1.2)
  Y2_50 <- B0 + (B1*Xi_50) + E2_50
  fit2 <- lm(Y2_50 ~ Xi_50)
  fit2_50[i, ] <- fit2$coefficients
  se2_50[i, ] <- sqrt(diag(vcov((fit2))))
}
summary(fit1_50)
summary(fit2_50)

## n = 100
# Defining parameters
fit1_100 <- fit2_100 <- se1_100 <- se2_100 <- matrix(ncol = 2, nrow = k)
k <- 1000
n <- 100
# Generating data and lm results
for (i in 1:k){
  set.seed(40991+i)
  # Normal Errors
  Xi_100 <- rnorm(n, 5, 4)
  E1_100 <- rnorm(n, 0, 9)
  B0 <- 10
  B1 <- 2
  Y1_100 <- B0 + (B1*Xi_100) + E1_100
  fit1 <- lm(Y1_100 ~ Xi_100)
  fit1_100[i, ] <- fit1$coefficients
  se1_100[i, ] <- sqrt(diag(vcov((fit1))))
  # Exponential Errors (lambda = 1.2)
  E2_100 <- rexp(n, 1.2)
  Y2_100 <- B0 + (B1*Xi_100) + E2_100
  fit2 <- lm(Y2_100 ~ Xi_100)
  fit2_100[i, ] <- fit2$coefficients
  se2_100[i, ] <- sqrt(diag(vcov((fit2))))
}
summary(fit1_100)
summary(fit2_100)

## n = 1000
# Defining parameters
fit1_1000 <- fit2_1000 <- se1_1000 <- se2_1000 <- matrix(ncol = 2, nrow = k)
k <- 1000
n <- 1000
# Generating data and lm results
for (i in 1:k){
  set.seed(40991+i)
  # Normal Errors
  Xi_1000 <- rnorm(n, 5, 4)
  E1_1000 <- rnorm(n, 0, 9)
  B0 <- 10
  B1 <- 2
  Y1_1000 <- B0 + (B1*Xi_1000) + E1_1000
  fit1 <- lm(Y1_1000 ~ Xi_1000)
  fit1_1000[i, ] <- fit1$coefficients
  se1_1000[i, ] <- sqrt(diag(vcov((fit1))))
  # Exponential Errors (lambda = 1.2)
  E2_1000 <- rexp(n, 1.2)
  Y2_1000 <- B0 + (B1*Xi_1000) + E2_1000
  fit2 <- lm(Y2_1000 ~ Xi_1000)
  fit2_1000[i, ] <- fit2$coefficients
  se2_1000[i, ] <- sqrt(diag(vcov((fit2))))
}
summary(fit1_1000)
summary(fit2_1000)

## Saving the B0 and B1 samples and calculating their SE
# Note: V1: n=10, V2: n=50, V3: n=100, v4: n=1000
Reg1_B0 <- cbind(fit1_10[,1], fit1_50[,1], fit1_100[,1], fit1_1000[,1])
Reg2_B0 <- cbind(fit2_10[,1], fit2_50[,1], fit2_100[,1], fit2_1000[,1]) 
Reg1_B1 <- cbind(fit1_10[,2], fit1_50[,2], fit1_100[,2], fit1_1000[,2])
Reg2_B1 <- cbind(fit2_10[,2], fit2_50[,2], fit2_100[,2], fit2_1000[,2])
write.csv2(Reg1_B0, "4.Reg1_B0.csv", row.names = FALSE)
write.csv2(Reg2_B0, "4.Reg2_B0.csv", row.names = FALSE)
write.csv2(Reg1_B1, "4.Reg1_B1.csv", row.names = FALSE)
write.csv2(Reg2_B1, "4.Reg2_B1.csv", row.names = FALSE)

Reg1_B0_se <- cbind(se1_10[,1], se1_50[,1], se1_100[,1], se1_1000[,1])
Reg2_B0_se <- cbind(se2_10[,1], se2_50[,1], se2_100[,1], se2_1000[,1])
Reg1_B1_se <- cbind(se1_10[,2], se1_50[,2], se1_100[,2], se1_1000[,2])
Reg2_B1_se <- cbind(se2_10[,2], se2_50[,2], se2_100[,2], se2_1000[,2])

## Exploratory Analysis
# Note: V1: n=10, V2: n=50, V3: n=100, v4: n=1000
summary(Reg1_B0)
summary(Reg2_B0)
summary(Reg1_B1)
summary(Reg2_B1)

## Boxplots for analizing biasedness and consistency of B0 and B1
# Defining legends and colors
boxplotleg <- c("10","50","100","1000")
boxplotcol <- c("gray80", "gray60", "gray40", "gray20")

# B0
par(mfrow=c(1,2))
boxplot(Reg1_B0,
        xlab = "n samples",
        ylab = "B0",
        main = "Normal Errors",
        names = boxplotleg, 
        col = boxplotcol)
abline(h=10, col = "darkblue", lwd = 2, lty = 2)
boxplot(Reg2_B0,
        xlab = "n samples",
        ylab = "B0",
        main = "Exponential Errors",
        col = boxplotcol,
        names = boxplotleg)
abline(h=10, col = "darkblue", lwd = 2, lty = 2)
# New horizontal line with B0_true=10, plus (1/lambda) = (1/1.2)
abline(h=10+(1/1.2), col = "darkblue", lwd = 1.5, lty = 2)

# B1
par(mfrow=c(1,2))
boxplot(Reg1_B1,
        xlab = "n samples",
        ylab = "B1",
        main = "Normal Errors",
        names = boxplotleg, 
        col = boxplotcol)
abline(h=2, col = "darkblue", lwd = 2, lty = 2)
boxplot(Reg2_B1,
        xlab = "n samples",
        ylab = "B1",
        main = "Exponential Errors",
        col = boxplotcol,
        names = boxplotleg)
abline(h=2, col = "darkblue", lwd = 2, lty = 2)

## Boxplots of standard errors (SE)
# B0
par(mfrow=c(1,2))
boxplot(Reg1_B0_se,
        xlab = "n samples",
        ylab = "Standard errors (B0)",
        main = "Normal Errors",
        names = boxplotleg, 
        col = boxplotcol)
boxplot(Reg2_B0_se,
        xlab = "n samples",
        ylab = "Standard errors (B0)",
        main = "Exponential Errors",
        col = boxplotcol,
        names = boxplotleg)

# B1
par(mfrow=c(1,2))
boxplot(Reg1_B1_se,
        xlab = "n samples",
        ylab = "Standard errors (B1)",
        main = "Normal Errors",
        names = boxplotleg, 
        col = boxplotcol)
boxplot(Reg2_B1_se,
        xlab = "n samples",
        ylab = "Standard errors (B1)",
        main = "Exponential Errors",
        col = boxplotcol,
        names = boxplotleg)

## 95% intervals of B0 and B1
# Note: 95% CI are built on both the mean of estimators and standard errors
## Visualizing the 95% Confidence Intervals

#Reg1_B0
low_95_1B0 <- c(mean(Reg1_B0[,1]) - qnorm(0.95) * mean(Reg1_B0_se[,1]),
                 mean(Reg1_B0[,2]) - qnorm(0.95) * mean(Reg1_B0_se[,2]), 
                 mean(Reg1_B0[,3]) - qnorm(0.95) * mean(Reg1_B0_se[,3]), 
                 mean(Reg1_B0[,4]) - qnorm(0.95) * mean(Reg1_B0_se[,4]))
pe_95_1B0 <- c(mean(Reg1_B0[,1]),
              mean(Reg1_B0[,2]),
              mean(Reg1_B0[,3]), 
              mean(Reg1_B0[,4]))
up_95_1B0 <- c(mean(Reg1_B0[,1]) + qnorm(0.95) * mean(Reg1_B0_se[,1]),
               mean(Reg1_B0[,2]) + qnorm(0.95) * mean(Reg1_B0_se[,2]), 
               mean(Reg1_B0[,3]) + qnorm(0.95) * mean(Reg1_B0_se[,3]),
               mean(Reg1_B0[,4]) + qnorm(0.95) * mean(Reg1_B0_se[,4]))
n <- c(1,2,3,4)
p_95_1B0 <- as.data.frame(cbind(n, low_95_1B0, pe_95_1B0, up_95_1B0))

ggplot(p_95_1B0, aes(n, pe_95_1B0)) +      
  geom_point() +
  geom_errorbar(aes(ymin = low_95_1B0, ymax = up_95_1B0)) +
  xlab("n = c(10, 50, 100, 1000)") + 
  ylab("B0") + 
  ggtitle("95% Confidence Interval, Normal Errors") + 
  geom_hline(yintercept = 10, col = "blue", lty = 4) + 
  theme_classic() 

#Reg2_B0
low_95_2B0 <- c(mean(Reg2_B0[,1]) - qnorm(0.95) * mean(Reg2_B0_se[,1]),
                mean(Reg2_B0[,2]) - qnorm(0.95) * mean(Reg2_B0_se[,2]), 
                mean(Reg2_B0[,3]) - qnorm(0.95) * mean(Reg2_B0_se[,3]), 
                mean(Reg2_B0[,4]) - qnorm(0.95) * mean(Reg2_B0_se[,4]))
pe_95_2B0 <- c(mean(Reg2_B0[,1]),
               mean(Reg2_B0[,2]),
               mean(Reg2_B0[,3]), 
               mean(Reg2_B0[,4]))
up_95_2B0 <- c(mean(Reg2_B0[,1]) + qnorm(0.95) * mean(Reg2_B0_se[,1]),
               mean(Reg2_B0[,2]) + qnorm(0.95) * mean(Reg2_B0_se[,2]), 
               mean(Reg2_B0[,3]) + qnorm(0.95) * mean(Reg2_B0_se[,3]),
               mean(Reg2_B0[,4]) + qnorm(0.95) * mean(Reg2_B0_se[,4]))
n <- c(1,2,3,4)
p_95_2B0 <- as.data.frame(cbind(n, low_95_2B0, pe_95_2B0, up_95_2B0))

ggplot(p_95_2B0, aes(n, pe_95_2B0)) +      
  geom_point() +
  geom_errorbar(aes(ymin = low_95_2B0, ymax = up_95_2B0)) +
  xlab("n = c(10, 50, 100, 1000)") + 
  ylab("B0") + 
  ggtitle("95% Confidence Interval, Exponential Errors") + 
  geom_hline(yintercept = 10, col = "blue", lty = 4) +
  geom_hline(yintercept = 10 + (1/1.2), col = "blue", lty = 4) + 
  theme_classic() 
# Note: (1/lambda) = (1/1.2) is added in the second abline  

#Reg1_B1
low_95_1B1 <- c(mean(Reg1_B1[,1]) - qnorm(0.95) * mean(Reg1_B1_se[,1]),
                mean(Reg1_B1[,2]) - qnorm(0.95) * mean(Reg1_B1_se[,2]), 
                mean(Reg1_B1[,3]) - qnorm(0.95) * mean(Reg1_B1_se[,3]), 
                mean(Reg1_B1[,4]) - qnorm(0.95) * mean(Reg1_B1_se[,4]))
pe_95_1B1 <- c(mean(Reg1_B1[,1]),
               mean(Reg1_B1[,2]),
               mean(Reg1_B1[,3]), 
               mean(Reg1_B1[,4]))
up_95_1B1 <- c(mean(Reg1_B1[,1]) + qnorm(0.95) * mean(Reg1_B1_se[,1]),
               mean(Reg1_B1[,2]) + qnorm(0.95) * mean(Reg1_B1_se[,2]), 
               mean(Reg1_B1[,3]) + qnorm(0.95) * mean(Reg1_B1_se[,3]),
               mean(Reg1_B1[,4]) + qnorm(0.95) * mean(Reg1_B1_se[,4]))
n <- c(1,2,3,4)
p_95_1B1 <- as.data.frame(cbind(n, low_95_1B1, pe_95_1B1, up_95_1B1))

ggplot(p_95_1B1, aes(n, pe_95_1B1)) +      
  geom_point() +
  geom_errorbar(aes(ymin = low_95_1B1, ymax = up_95_1B1)) +
  xlab("n = c(10, 50, 100, 1000)") + 
  ylab("B1") + 
  ggtitle("95% Confidence Interval, Normal Errors") + 
  geom_hline(yintercept = 2, col = "blue", lty = 4) + 
  theme_classic() 

#Reg2_B1
low_95_2B1 <- c(mean(Reg2_B1[,1]) - qnorm(0.95) * mean(Reg2_B1_se[,1]),
                mean(Reg2_B1[,2]) - qnorm(0.95) * mean(Reg2_B1_se[,2]), 
                mean(Reg2_B1[,3]) - qnorm(0.95) * mean(Reg2_B1_se[,3]), 
                mean(Reg2_B1[,4]) - qnorm(0.95) * mean(Reg2_B1_se[,4]))
pe_95_2B1 <- c(mean(Reg2_B1[,1]),
               mean(Reg2_B1[,2]),
               mean(Reg2_B1[,3]), 
               mean(Reg2_B1[,4]))
up_95_2B1 <- c(mean(Reg2_B1[,1]) + qnorm(0.95) * mean(Reg2_B1_se[,1]),
               mean(Reg2_B1[,2]) + qnorm(0.95) * mean(Reg2_B1_se[,2]), 
               mean(Reg2_B1[,3]) + qnorm(0.95) * mean(Reg2_B1_se[,3]),
               mean(Reg2_B1[,4]) + qnorm(0.95) * mean(Reg2_B1_se[,4]))
n <- c(1,2,3,4)
p_95_2B1 <- as.data.frame(cbind(n, low_95_2B1, pe_95_2B1, up_95_2B1))

ggplot(p_95_2B1, aes(n, pe_95_2B1)) +      
  geom_point() +
  geom_errorbar(aes(ymin = low_95_2B1, ymax = up_95_2B1)) +
  xlab("n = c(10, 50, 100, 1000)") + 
  ylab("B1") + 
  ggtitle("95% Confidence Interval, Exponential Errors") + 
  geom_hline(yintercept = 2, col = "blue", lty = 4) + 
  theme_classic()

#####
# END
#####