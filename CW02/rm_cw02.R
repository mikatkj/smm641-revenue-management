## Question 1: London congestion charge  ----
## loading libraries and data ----
library(nloptr)
cong <- read.csv('CongestionPricing.csv')

# explore the data
table(is.na(cong))
str(cong)
head(cong)
summary(cong)
#par(mfrow = c(1,2))
#hist(cong$Peak_WTP, main = 'WTP for Peak')
#hist(cong$Nonpeak_WTP, main = 'WTP for Nonpeak')

## Q1.a single charge ----
# no. of observations
n <- nrow(cong)
# no. of city drivers in total
total <- 192000
# max WTP for each person (add a new column)
cong$maxwtp <- apply(cong[,2:3], 1, max)
# upper bound for price search
maxprice <- max(cong[,2:3])
maxprice
# empty array variables
demand <- rep(0, maxprice)
revenue <- rep(0, maxprice)
# get the no. of people would buy at each price level
for (p in 1:maxprice){
    demand[p] <- sum(cong$maxwtp >= p)
    revenue[p] <- p * demand[p]
}
# find the best price
price.best <- which(revenue == max(revenue))
price.best
print(paste('If a single price is to be charged across both peak and nonpeak, the optimal price is', price.best))
# total revenue
cars <- round(demand[price.best] / n * total, 0)
revenue.best <- price.best * cars
revenue.best
# total level of emissions
speed <- 30 - 0.0625 * cars / 1000 # km/h
speed
co2 <- cars * ifelse(speed<25, 617.5-16.7*speed, 235.0-1.4*speed) # g/km in total
co2

# plot ----
# plot total demand VS price
xaxis <- 1:maxprice
plot(xaxis,   # search prices 
     demand/n*total,   # total demand
     pch = 16, type = "s", col = "blue", las = 1, xaxt = "n",
     xlab = "Price", ylab = "Demand")
xticks <- seq(0, maxprice)
axis(side = 1, at = xticks)
# plot total revenue vs price
xaxis <- 1:maxprice
plot(xaxis,   # search prices
     xaxis*(demand/n*total)/1000000,   # total revenue
     pch = 16, type = "s", col = "blue", las = 1, xaxt = "n",
     xlab = "Price", ylab = "Total Revenue (in £1,000,000)")
xticks <- seq(0, maxprice)
axis(side = 1, at = xticks) 
axis(side = 1, at = price.best) 
lines(c(price.best, price.best), c(0, revenue.best/1000000), lty=2)
axis(side = 2, at = round(revenue.best/1000000, 2), 
     las = 1, tick = FALSE)
lines(c(0, price.best), c(revenue.best/1000000, revenue.best/1000000), lty = 2)

## Q1.b peak period price strategy ----
# base price for nonpeak
base.price <- 7
# initalize variables
demand.nonpeak <- rep(0, maxprice)
demand.peak <- rep(0, maxprice)
revenue2 <- rep(0, maxprice)
# surplus among nonpeak periods
surplus.nonpeak <- cong$Nonpeak_WTP - base.price
# surplus among peak periods for different peak prices
surplus.peak <- matrix(0, n, maxprice) 
for(p in 1:maxprice){
    for(i in 1:n){
        surplus.peak[i,p] <- cong$Peak_WTP[i] - p 
    }
}
# have a look at surpluses for peak 
colnames(surplus.peak) <- paste0('p=', 1:maxprice)
head(surplus.peak)
# compare and decide whether choice peak/nonpeak/neither
for(p in 1:maxprice){
    demand.nonpeak[p] <- sum((surplus.nonpeak > surplus.peak[,p]) *
                                 (surplus.nonpeak >= 0))
    demand.peak[p] <- sum((surplus.nonpeak <= surplus.peak[,p]) *
                              (surplus.peak[,p] >= 0))
    revenue2[p] <- base.price * demand.nonpeak[p] + p * demand.peak[p]
}
# optimal price
price.best2 <- which(revenue2 == max(revenue2))
price.best2
print(paste('When nonpeak periods have a base price of £7, the optimal peak price is', price.best2))
# total revenue
revenue.best2 <- base.price*(demand.nonpeak[price.best2]/n*total) +
    price.best2*(demand.peak[price.best2]/n*total)
revenue.best2
# total level of emissions
cars2 <- round((demand.nonpeak[price.best2] + demand.peak[price.best2]) / 
                   n * total, 0)
cars2
speed2 <- 30 - 0.0625 * cars2 / 1000 # km/h
speed2
co2.ppp <- cars2 * ifelse(speed2<25, 617.5-16.7*speed2, 235.0-1.4*speed2) # g/km in total
co2.ppp

# compare (a) and (b) ----
print(paste('If a single price is to be charged across both peak and nonpeak, the optimal price is', 
            price.best))
print(paste('When nonpeak periods have a base price of £7, the optimal peak price is', 
            price.best2)) 
revenue.best2 - revenue.best   # total revenue increases
cars2 - cars   # no. of total cars decrease
speed2 - speed   # speed increases
co2.ppp - co2   # co2 emissions decreases

# plot ----
# plot nonpeak demand VS Peak Period Price
xaxis <- 1:maxprice
plot(xaxis,   # ppp
     demand.nonpeak/n*total,   # total demand for nonpeak
     pch = 16, type = "s", col = "blue", las = 1, xaxt = "n",
     xlab = "Peak Period Price", ylab = "Non-peak Period Demand")
xticks <- seq(0, maxprice)
axis(side = 1, at = xticks)
# plot peak demand VS Peak Period Price
xaxis <- 1:maxprice
plot(xaxis,   # ppp
     demand.peak/n*total,   # total demand for peak
     pch = 16, type = "s", col = "blue", las = 1, xaxt = "n",
     xlab = "Peak Period Price", ylab = "Peak Period Demand")
xticks <- seq(0, maxprice)
axis(side = 1, at = xticks)
# plot total revenue VS Peak Period Price
xaxis <- 1:maxprice
yaxis <- base.price*(demand.nonpeak[xaxis]/n*total) +
    xaxis*(demand.peak[xaxis]/n*total)
plot(xaxis,   # ppp
     yaxis/1000000,   # total revenue
     pch = 16, type = "s", col = "blue", las = 1, xaxt = "n",
     xlab = "Peak Period Price", ylab = "Total Revenue (in £1,000,000)")
xticks <- seq(0, maxprice)
axis(side = 1, at = xticks) 
axis(side = 1, at = price.best2) 
lines(c(price.best2, price.best2), c(0, revenue.best2/1000000), lty=2)
axis(side = 2, at = round(revenue.best2/1000000, 2), las = 1)
lines(c(0, price.best2), c(revenue.best2/1000000, revenue.best2/1000000), lty = 2)

## Q1.c minimize pollution and ensure a portion of revenue ----
# base price for nonpeak
base.price <- 7
# to find optimal revenue by optimization
eval_f <- function(x){
    peak.price <- x
    demand.np <- max(0, demand.nonpeak[peak.price])
    demand.p <- max(0, demand.peak[peak.price])
    car <- round((demand.np + demand.p) / n * total, 0)
    spd <- 30 - 0.0625 * car / 1000
    emi <- car * ifelse(spd<25, 617.5-16.7*spd, 235.0-1.4*spd) # g/km in total
    obj.func <- emi
    return(obj.func)
}
eval_g_ineq <- function(x){
    peak.price <- x
    demand.np <- max(0, demand.nonpeak[peak.price])
    demand.p <- max(0, demand.peak[peak.price])
    rev <- base.price*(demand.np/n*total) + peak.price*(demand.p/n*total)
    constraint <- c(-demand.np,
                    -demand.p,
                    # base.price - peak.price   # nonpeak <= peak price
                    1100000-rev)   # revenue >= 1,100,000
    return(constraint)
}
# initial values
x0 <- 10
# lower and upper bounds of control
lb <- 1
ub <- maxprice
opts <- list('algorithm' = 'NLOPT_LN_COBYLA',
             'xtol_rel'  = 1.0e-9,
             'maxeval'   = 1000)
result <- nloptr(x0 = x0, eval_f = eval_f, lb = lb, ub = ub,
                 eval_g_ineq = eval_g_ineq, opts = opts)
# print results
price.opt <- round(result$solution, 2)
emi.opt <- round(result$objective, 0)
rev.opt <- base.price*(demand.nonpeak[price.opt]/n*total) +
    price.opt*(demand.peak[price.opt]/n*total)
print(paste('The optimal peak price is:', price.opt))
print(paste('The minimum total level of emissions is:', emi.opt))
print(paste('The total revenue is:', rev.opt))


## Question 2: Sandwich shell space allocation  ----
## loading libraries and data ----
library(mlogit)
library(nloptr)
sand <- read.csv('SandwichChoice.csv')

# explore the data
table(is.na(sand))
str(sand)
head(sand)
table(sand$choice)

## pre-process data ----
# no. of observations
n <- nrow(sand)
# change structure of the choice data to prepare for the mlogit function
choice <- mlogit.data(sand, 
                      shape = 'wide',
                      varying = 2:6,
                      choice = 'choice')

## obtain parameters for MNL demand model by running MLE ----
# fit a MNL demand model
mle <- mlogit(choice ~ price, 
              data = choice, 
              reflevel = 'DNB') 
# have a look at the results
mle
summary(mle)
# obtain parameters for MNL model
mu <- -1/summary(mle)$coefficients[5]
print(paste('The shape parameter mu is:', round(mu, 2)))
util <- summary(mle)$coefficients[1:4] * mu
names(util) <- c('regular.avo', 'small.avo', 'regular.veg', 'small.veg')
print('The gross utilities (u_j) are:')
round(util,2)

## solve the nonlinear optimization problem ----
eval_f <- function(x){
    price.regular <- x[1]
    price.small <- x[2]
    price <- rep(c(price.regular,price.small), 2)
    # attraction values
    attraction <- exp((util - price) / mu)
    # sum of attraction values
    sum <- 1 + sum(attraction)
    # purchase probabilities
    prob <- attraction / sum
    prob.regular <- prob[1] + prob[3]
    prob.small <- prob[2] + prob[4]
    # expected profit
    profit <- n*prob.regular*(price.regular-2) +   # cost for regular is £2.00
        n*prob.small*(price.small-1)   # cost for small is £1.00
    # return objective function
    obj.func <- -profit
    return(obj.func)
}
eval_g_ineq <- function(x){
    price <- x
    price.regular <- price[1]
    price.small <- price[2]
    constraint <- c(-price.regular,
                    -price.regular,
                    1.5+price.small-price.regular)   # price.regular-price.small>=1.5
    return(constraint)
}
# initial values
x0 <- c(3,1)
# bounds
lb <- c(0,0)
ub <- c(10,10)
# options
opts <- list('algorithm' = 'NLOPT_LN_COBYLA',
             'xtol_rel'  = 1.0e-9,
             'maxeval'   = 1000)
result <- nloptr(x0 = x0, eval_f = eval_f, lb = lb, ub = ub,
                 eval_g_ineq = eval_g_ineq, opts = opts)

## display the results ----
price.optimal <- round(result$solution, 2)
print(paste('The optimal price for regular variant is', price.optimal[1]))
print(paste('The optimal price for small variant is', price.optimal[2]))
