#---------------------------------------------------------------------------
# MULTIVARIATE TIME SERIES
#---------------------------------------------------------------------------

#------------------------------------------------
#1. STATIONARITY OF MULTIVARIATE TIME SERIES
#------------------------------------------------


#Xt = 0.2X(t-1) + 0.4Y(t-2) + e_t^X
#Yt = 0.1X(t-1) + 0.3Y(t-1) + e_t^Y


A <- matrix(c(0.2,0.4,0.1,0.3), 2, 2, byrow=T)
A

eigen(A)

eigen(A)$values

#eigen values= 0.45615528,0.04384472

#eigen values' mod is strictly less than 1 therefore the process 
#is stationary.


#-----------------------------------------------
# 2. COINTEGRATED TIME SERIES
#-----------------------------------------------


#X and Y are cointegrated if the following conditions hold:

# 1. X and Y are I(1) random processes.
#    That is, the data is to be differenced once

# 2. There exists a non-zero cointegrating vector(alpha, beta) such
#    that (alpha)x + (beta)y is stationary.


setwd("C:\\Users\\Sanya Chauhan\\Desktop\\ME\\Actuarial Science\\CS2\\R Data Files\\TIME SERIES") 
xy <- read.table("cointegration.txt")
xy

x <- xy[,1]
y <- xy[,2]


PP.test(x)     #0.4925  #greater that 0.05 #hence, not stationary
dx<- diff(x)
PP.test(dx)    #0.01
 

PP.test(y)     #0.6912  #greater that 0.05 #hence, not stationary
dy<- diff(y)
PP.test(dy)    #0.01


#Both X and Y are random I(1) processes.


# To check second condition-

fin.coint <- function(coint){
  
  comb <- coint[1]*x + coint[2]*y
  test <- PP.test(comb)$p.value
  test
  
}

v <- c(1,1)

fit<- nlm(fin.coint,v)
fit 

alpha <- fit$estimate[1]
beta <- fit$estimate[2]


#To check is alpha*x +  beta*y is stationary or not

c(alpha, beta)

comb <- alpha*x + beta*y
PP.test(comb)
# pval = 0.01
# smaller that 0.05
#hence, stationary


#condition 2 holds

#Hence, X and Y are cointegrated

#------------------------------------------------------------
# END OF TIME SERIES
#------------------------------------------------------------