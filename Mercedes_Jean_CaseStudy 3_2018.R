#############################################
############# notes #########################
#############################################
testData <- rgamma ( n =300 , shape =3.5 , rate =1/6)
hist ( testData , freq = FALSE , breaks =seq (0 ,70 ,2) )

mean.data = mean(testData)
sd.data = sd(testData)

par( new = T)
x = seq(0, 70, 0.5)
y = dnorm(x, mean.data, sd.data)
plot(x,y, type ="l", xlim = c(0,70), ylim = c(0,0.05) )

testNorm = rnorm(100000, mean.data, sd.data)
ks.test(testData, testNorm)


hist(testData, freq = F, breaks = seq(0,70,2) , xlim = c(0,70), ylim = c(0,.05))
par(new = T)
x = seq(0,70,.05)
y = dchisq(x , mean.data)
plot(x,y, type = "l", xlim = c(0,70), ylim = c(0,0.05))

testchi = rchisq(100000, mean.data)
ks.test(testData,testchi)


gammaParameters = function ( data ) {
   theMean <- mean ( data )
   theVar <- var ( data )
   beta <- theMean / theVar
   alpha <- theMean ^2/ theVar
   return ( list ( alpha = alpha , beta = beta ) )
   }


gp = gammaParameters(testData)
gp$alpha
gp$beta

hist(testData, freq = F, breaks = seq(0,70,2), xlim = c(0,70), ylim = c(0,0.05))
par(new = T)
x = seq(0,70,0.5)
y = dgamma(x , shape = gp$alpha, rate = gp$beta)
plot(x,y, type = "l" , xlim = c(0,70), ylim = c(0,0.05))
testgamma = rgamma(100000, shape = gp$alpha, rate = gp$beta)
ks.test(testData, testgamma)

####################################
##########  PROBLEMS     ###########
####################################

#@@@@@@@@@@@@@@@@@@@@@@#
##@@@@@ PROBLEM 1  @@@@@#
###@@@@@@@@@@@@@@@@@@@@@@#

#install.packages("quantmod")
library(quantmod)
getSymbols("AMD") # getting AMD stock data

#@@@@@@@@@@@@@@@@@@@@@@#
##@@@@@ PROBLEM 2  @@@@@#
###@@@@@@@@@@@@@@@@@@@@@@#

amd.stock = data.frame(AMD)

#@@@@@@@@@@@@@@@@@@@@@@#
##@@@@@ PROBLEM 3  @@@@@#
###@@@@@@@@@@@@@@@@@@@@@@#

high = as.vector(as.numeric(amd.stock$AMD.High))
low = as.vector(as.numeric(amd.stock$AMD.Low))
close = as.vector(as.numeric(amd.stock$AMD.Close))
open = as.vector(as.numeric(amd.stock$AMD.Open))

#@@@@@@@@@@@@@@@@@@@@@@#
##@@@@@ PROBLEM 4  @@@@@#
###@@@@@@@@@@@@@@@@@@@@@@#

#Normal Distribution Parameters
high_mean = mean(high)
low_mean = mean(low)
close_mean = mean (close)
open_mean = mean(open)

high_sd = sd(high)
low_sd =  sd(low)
close_sd = sd(close)
open_sd = sd(open)


# Chi Squared Distribution Parameters 
high.x = length(high)
low.x = length(low)
close.x = length(close)
open.x = length(open)


#all of the means are calculated 
# degrees of freedom is the same as the means


#Gamma Distribution Parameters 
gp.high = gammaParameters(high)
  #$`alpha 2.135043 $beta 0.2900505
gp.low = gammaParameters(low)
  #$`alpha` 2.147759 $beta 0.3048093
gp.close = gammaParameters(close)
  #$`alpha` 2.137653 $beta 0.2968395
gp.open = gammaParameters(open)
  #$`alpha`2.140084 $beta 0.2968032

#@@@@@@@@@@@@@@@@@@@@@@#
##@@@@@ PROBLEM 5  @@@@@#
###@@@@@@@@@@@@@@@@@@@@@@#

#############################
#     HIGH          #########
#############################
par(mfrow = c(3,1))
hist(high, freq = F, xlim = c(0,50), ylim = 0.15) # hist of high 
#NOrmal
par( new = T)
x_high_norm = high
y_high_norm = dnorm(x_high_norm, high_mean, high_sd)
plot(x_high_norm,y_high_norm, type ="l", xlim = c(0,50), ylim = c(0,0.15) )



#Chi Squared
hist(high, freq = F) # hist of high 

par(new = T)
x_high_chi = high
y_high_chi = dchisq(x_high_chi , high_mean)
plot(x_high_chi,y_high_chi, type = "l", xlim = c(0,50), ylim = c(0,0.15))


#Gamma Dist 
hist(high, freq = F)
par(new = T)
x_high_gamma = high
y_high_gamma = dgamma(x_high_gamma , shape = gp.high$alpha, rate = gp.high$beta)
plot(x_high_gamma,y_high_gamma, type = "l" , xlim = c(0,50), ylim = c(0,0.15))


########################
#    LOW           #####
########################

hist(low, freq = F) # hist of high 
#NOrmal
par( new = T)
x_low_norm = low
y_low_norm = dnorm(x_low_norm, low_mean, low_sd)
plot(x_low_norm,y_low_norm, , type ="l", xlim = c(0,50), ylim = c(0,0.15) )



#Chi Squared
hist(low, freq = F) # hist of high 
par(new = T)
x_low_chi = low
y_low_chi = dchisq(x_low_chi , low_mean)
plot(x_low_chi,y_low_chi, type = "l", xlim = c(0,50), ylim = c(0,0.15))

#Gamma Dist 
hist(low, freq = F, breaks = seq(0,50,2), xlim = c(0,50), ylim = c(0,0.15))
par(new = T)
x_low_gamma = low
y_low_gamma = dgamma(x_low_gamma , shape = gp.low$alpha, rate = gp.low$beta)
plot(x_low_gamma,y_low_gamma, type = "l" , xlim = c(0,50), ylim = c(0,0.15))


##################################
#CLOSE 
##################################

par(mfrow = c(3,1))
########
#NOrmal#
########
hist(close, freq = F) # hist of close 
par( new = T)
x_close_norm = close
y_close_norm = dnorm(x_close_norm, close_mean, close_sd)
plot(x_close_norm,y_close_norm, type ="l", xlim = c(0,50), ylim = c(0,0.15) )


#############
#Chi Squared#
#############
hist(close, freq = F) # hist of high 
par(new = T)
x_close_chi = close
y_close_chi= dchisq(x_close_chi , close_mean)
plot(x_close_chi,y_close_chi, type = "l", xlim = c(0,50), ylim = c(0,0.15))

############
#Gamma Dist#
############
hist(close, freq = F)
par(new = T)
x_close_gamma = close
y_close_gamma = dgamma(x_close_gamma , shape = gp.close$alpha, rate = gp.close$beta)
plot(x_close_gamma,y_close_gamma, type = "l" , xlim = c(0,50), ylim = c(0,0.15))


#################################
# OPEN
#################################

hist(open, freq = F) # hist of high 
#NOrmal
par( new = T)
x_open_norm = open
y_open_norm = dnorm(x_open_norm, open_mean, open_sd)
plot(x_open_norm,y_open_norm,type ="l", xlim = c(0,50), ylim = c(0,0.15) )



#Chi Squared
hist(open, freq = F) # hist of high 
par(new = T)
x_open_chi = open
y_open_chi = dchisq(x_open_chi , open_mean)
plot(x_open_chi,y_open_chi, type = "l", xlim = c(0,50), ylim = c(0,0.15))

#Gamma Dist 
hist(open, freq = F)
par(new = T)
x_open_gamma = open
y_open_gamma = dgamma(x_open_gamma , shape = gp.open$alpha, rate = gp.open$beta)
plot(x_open_gamma,y_open_gamma, type = "l" , xlim = c(0,50), ylim = c(0,0.15))






##@@@@@ PROBLEM 6  @@@@@#

######
#HIGH#
######
hist(high)
high.testNorm = rnorm(100000, high_mean, high_sd)

ks.test(y_high_norm, high.testNorm)
#p-value <  2.2e-16; reject to use hist 


y_high_chi
high.testchi = rchisq(100000, high_mean)
ks.test(y_high_chi,high.testchi)
#p-value <2.2e-16
# We would reject the use of a chi squared 

y_high_gamma
high.testgamma = rgamma(100000, shape = gp.high$alpha, rate = gp.high$beta)
ks.test(y_high_gamma, high.testgamma)
# p-value = 2.998e-14 again, reject the use of a gamma 

#####
#LOW#
#####
low.testNorm = rnorm(100000, low_mean, low_sd)
ks.test(low, low.testNorm)
# p-value <2.2e-16
# data suggest that we should reject the use of a normal curve 
#for high 

low.testchi = rchisq(100000, low_mean)
ks.test(low, low.testchi)
#p-value <2.2e-16
# We would reject the use of a chi squared 

low.testgamma = rgamma(100000, shape = gp.low$alpha, rate = gp.low$beta)
ks.test(low, low.testgamma)
# p-value = 2.998e-14 again, reject the use of a gamma 

#######
#CLOSE#
#######
close.testNorm = rnorm(100000, close_mean, close_sd)
ks.test(close, close.testNorm)
# p-value <2.2e-16
# data suggest that we should reject the use of a normal curve 
#for high 

close.testchi = rchisq(100000, close_mean)
ks.test(close,close.testchi)
#p-value <2.2e-16
# We would reject the use of a chi squared 

close.testgamma = rgamma(100000, shape = gp.close$alpha, rate = gp.close$beta)
ks.test(close, close.testgamma)
# p-value = 2.998e-14 again, reject the use of a gamma 

######
#OPEN#
######
open.testNorm = rnorm(100000, open_mean, open_sd)
ks.test(open, open.testNorm)
# p-value <2.2e-16
# data suggest that we should reject the use of a normal curve 
#for high 

open.testchi = rchisq(100000, open_mean)
ks.test(open,open.testchi)
#p-value <2.2e-16
# We would reject the use of a chi squared 

open.testgamma = rgamma(100000, shape = gp.open$alpha, rate = gp.open$beta)
ks.test(open, open.testgamma)
# p-value = 1.125e-13 again, reject the use of a gamma 
