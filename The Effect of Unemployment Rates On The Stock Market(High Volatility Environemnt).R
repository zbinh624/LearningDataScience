# import data set and load useful packages

library(AER)
library(sandwich)
library(lmtest)
library(car)
library(MASS)

data = read.csv("fredgraph (1).csv")
newdata = read.csv("fredgraph (6).csv")
newdata$VIX = data$VIXCLS

#######################################################5
# QUADRATIC SPECIFICATION OF THE MODEL
#######################################################
quadratic.ols.fit = lm(SP500 ~ U2RATE + I(U2RATE^2), data = newdata)
summary(quadratic.ols.fit)
ttest1 <- coeftest(quadratic.ols.fit, vcov = vcovHC(quadratic.ols.fit, "HC1"))
ttest1
hatSP500.1 <-predict(quadratic.ols.fit) 
#######################################################
# CUBIC SPECIFICATION OF THE INCOME MODEL
#######################################################
cubic.ols.fit = lm(SP500 ~ U2RATE + I(U2RATE^2) + I(U2RATE^3), data = newdata)
summary(cubic.ols.fit)
ttest2 <- coeftest(cubic.ols.fit, vcov = vcovHC(cubic.ols.fit, "HC1"))
ttest2
hatSP500.2 <- predict(cubic.ols.fit) 
# F-test for the coefficients of U2RATE^2 and U2RATE^3
waldtest(cubic.ols.fit, c("I(U2RATE^2)","I(U2RATE^3)"), vcov = vcovHC(cubic.ols.fit, "HC1"))


#######################################################
# LOG SPECIFICATION OF THE INCOME MODEL
######################################################
## Check that the logmation is making the 
# distribution on U2RATE more similar to a normal

hist(newdata$U2)
hist(log(newdata$U2), breaks = 5)

log.ols.fit = lm(SP500 ~ log(U2RATE), data = newdata)
summary(log.ols.fit)
ttest.log <- coeftest(log.ols.fit, vcov = vcovHC(log.ols.fit, "HC1"))
ttest.log

hatSP500log <-predict(log.ols.fit) 

# Plot cubic vs log specification
order.avinc <- sort(newdata$U2RATE,index=T)
par(mfrow = c(1, 1))
plot(newdata$U2RATE,newdata$SP500)
lines(order.avinc$x,hatSP500.2[order.avinc$ix],col=3,lwd=2)
lines(order.avinc$x,hatSP500log[order.avinc$ix],col=4,lwd=2)
legend("topright",c("Cubic","Log"),lty=c(1,1),col=3:4,lwd=c(2,2),bty="n")


# all the model

plot(newdata$U2RATE,newdata$SP500)
abline(lm(SP500 ~ U2RATE, data = newdata))
lines(order.avinc$x,hatSP500.1[order.avinc$ix],col=2,lwd=2)
lines(order.avinc$x,hatSP500.2[order.avinc$ix],col=3,lwd=2)
lines(order.avinc$x,hatSP500log[order.avinc$ix],col=4,lwd=2)
legend("topright",c("Linear","Quadratic","Cubic","LOG"),col = 2:5,lty=c(1,1,1,1),lwd=c(2,2,2,2),bty="n")


#######################################################
# Generate nonlinearities by interacting variables
#######################################################
# Generate dummy for High VIX index

newdata$HVIX <- ifelse(newdata$VIX >= 20,1,0)

########################################################
# INTERACTION BETWEEN A BINARY VARIABLE AND NONLINEAR
# TRANSFORMATIONS OF A CONTINUOUS VARIABLE
########################################################

# Interaction between one continuous and one binary variable
ols.log.int.nl <- lm(SP500 ~ log(U2RATE) + HVIX + I(log(U2RATE)*HVIX) , data = newdata)
summary(ols.log.int.nl)

ttest.log.int.nl <- coeftest(ols.log.int.nl,vcov = vcovHC(ols.log.int.nl, "HC1"))
ttest.log.int.nl

# Plot the two regression lines for each value of HVIX (0 and 1)
order.U2RATE <- sort(newdata$U2RATE,index=T)
hatSP500.HVIX <- predict(ols.log.int.nl)[order.U2RATE$ix]
plot(newdata$U2RATE[newdata$HVIX == 0],newdata$SP500[newdata$HVIX == 0])
points(newdata$U2RATE[newdata$HVIX == 1],newdata$SP500[newdata$HVIX == 1],cex=0.5,col=4)
lines(order.U2RATE$x[newdata$HVIX[order.U2RATE$ix] == 0],hatSP500.HVIX[newdata$HVIX[order.U2RATE$ix] == 0],col=3,lwd=2)
lines(order.U2RATE$x[newdata$HVIX[order.U2RATE$ix] == 1],hatSP500.HVIX[newdata$HVIX[order.U2RATE$ix] == 1],col=4,lwd=2)
legend("topleft",c("HVIX = 0","HVIX = 1"),lty=c(1,1),col=3:4,lwd=c(2,2),bty="n")

# Test for the interaction coefficients to be 0
waldtest(ols.log.int.nl,c("I(log(U2RATE) * HVIX)"), vcov = vcovHC(ols.log.int.nl, "HC1"))


### FINAL MODEL
# Since it failed the F-test, we cannot reject the null hypothesis. The final model
# will not include the interaction term.

finalmodel =  lm(SP500 ~ log(U2RATE) + HVIX , data = newdata)
summary(finalmodel)

##Analysis of Variance Table
anova(finalmodel)

# Even though the F value for HVIX is not ideal, it's significant under alpha = 0.01.
# Constructing it helped us understand the effect of high VIX index on SP500 and 
# that's why decided to include it in the final model.

##Instrumental variable:
# Test for VIX as a instrumental variable
##Estimate the model in Two Stages
# First Stage: Obtain the predicted values of price
fs_tsls <- lm(log(U2RATE) ~ VIX,data = newdata)
summary(fs_tsls)
ttest.fs <- coeftest(fs_tsls,vcov = vcovHC(fs_tsls, "HC1"))

newdata$lravphat <- predict(fs_tsls)

# Second Stage: Plug predicted SP500 into the main model
ss_tsls <- lm(SP500 ~ lravphat,data = newdata)
summary(ss_tsls)

# Combine everything into a single command in R
fm <- ivreg(SP500 ~ log(U2RATE) |VIX,data = newdata)
summary(fm, vcov = sandwich, df = Inf)

### Instrument validity 
# 1. Instrument relevance
dfs_tsls <- lm(SP500 ~ log(U2RATE) + VIX, data = newdata)
waldtest(dfs_tsls, c("VIX"), vcov = vcovHC(dfs_tsls, "HC1"))

# low F score, the instrumental variable VIX is not relevant in the model.

## Characteristics of a instrumental variable 
# (i) It causes variation in the treatment variable;
# (ii) It does not have a direct effect on the outcome variable, only indirectly through the
# treatment variable. 
# In this case, we would have to find a variable that's correlated to unemployment rate,
# but does not affect the index of SP500 directly. As I've done above,
# we can use the waldtest to run the F test to test the validity of the instrumental variable.






