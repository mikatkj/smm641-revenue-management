## 1. loading libraries and data ----
library(ggplot2)
library(lattice)
library(nloptr)
library(lpSolve)
CatFish <- read.csv('CatFish.csv')

## 2. data preprocessing ----
# specify no. of survey respondents and products
n1 <- max(CatFish$consumer_id)
n2 <- max(CatFish$product_id)
# specify product names
product.name <- c("Non-certified, live, large",
                  "Certified, smoked, large",  
                  "Certified, live, large",    
                  "Non-certified, live, medium",  
                  "Non-certified, smoked, large", 
                  "Certified, live, medium",      
                  "Non-certified, smoked, medium",
                  "Certified, smoked, medium")
CatFish <- CatFish[,c(1,3,4)]
# check data
# table(complete.cases(CatFish))
# str(CatFish)
# construct a WTP matrix
wtp <- data.frame(matrix(rep(0,n1*n2), nrow = n1, ncol = n2))
for(i in 1:n1){
    for(j in 1:n2){
        wtp[i,j] <- CatFish[(CatFish$consumer_id==i)&(CatFish$fish_product==product.name[j]),3]
    }
}
colnames(wtp) <- product.name
# calculate max WTPs
wtp$certified <- apply(wtp[,c(2,3,6,8)],1,max)
wtp$noncertified <- apply(wtp[,c(1,4,5,7)],1,max)
wtp$live <- apply(wtp[,c(1,3,4,6)],1,max)
wtp$smoked <- apply(wtp[,c(2,5,7,8)],1,max)
wtp$large <- apply(wtp[,c(1,2,3,5)],1,max)
wtp$medium <- apply(wtp[,c(4,6,7,8)],1,max)
wtp$max <- apply(wtp[,c(1:8)],1,max)

## plot WTPs ----
hist1 <- data.frame(Attribute = c(rep('certified',n1),rep('noncertified',n1)),
                    WTP = c(wtp$certified, wtp$noncertified))
hist2 <- data.frame(Attribute = c(rep('smoked',n1),rep('live',n1)),
                    WTP = c(wtp$smoked, wtp$live))
hist3 <- data.frame(Attribute = c(rep('large',n1),rep('medium',n1)),
                    WTP = c(wtp$large, wtp$medium))
# compare WTPs between certified and noncertified
ggplot(hist1, aes(x = WTP, color = Attribute, fill = Attribute)) +
    geom_histogram(binwidth = 100, position = "dodge", alpha = 0.5) +
    theme(legend.position = "top") +
    scale_color_manual(values = c("darkblue", "#E69F00")) +
    scale_fill_manual(values=c("darkblue", "#E69F00"))
# compare WTPs between live and smoked
ggplot(hist2, aes(x = WTP, color = Attribute, fill = Attribute)) +
    geom_histogram(binwidth = 100, position = "dodge", alpha = 0.5) +
    theme(legend.position = "top") +
    scale_color_manual(values = c("darkblue", "#E69F00")) +
    scale_fill_manual(values=c("darkblue", "#E69F00"))
# compare WTPs between large and medium
ggplot(hist3, aes(x = WTP, color = Attribute, fill = Attribute)) +
    geom_histogram(binwidth = 100, position = "dodge", alpha = 0.5) +
    theme(legend.position = "top") +
    scale_color_manual(values = c("darkblue", "#E69F00")) +
    scale_fill_manual(values=c("darkblue", "#E69F00"))

## 3. set certified/noncertified prices ----
# using non-parametric estimate to search prices from 50 to 2850 with an increment of 50
price.start <- 50
price.end <- 2850
increment <- 50
# prices
price.c <- seq(price.start, price.end, by = increment)
price.nc <- seq(price.start, price.end, by = increment)
price.points <- (price.end - price.start)/increment + 1   # = 57
# total no. of price combinations = 57^2
n.comb <- price.points^2   

# get WTPs for certified/noncertified from survey data
wtp.c <- wtp$certified
wtp.nc <- wtp$noncertified
# uncomment below to calculate the other attributes
# wtp.c <- wtp$large
# wtp.nc <- wtp$medium
# wtp.c <- wtp$smoked
# wtp.nc <- wtp$live

# compute surplus
surplus.c <- matrix(0, n1, price.points)
surplus.nc <- matrix(0, n1, price.points)
for(i in 1:price.points){
    surplus.c[,i] <- wtp.c - price.c[i]
    surplus.nc[,i] <- wtp.nc - price.nc[i]
}

# create a data.frame to record price for each combination with no. of clients who would choose which attribute
index <- 1:n.comb   # column1: index
price.certified <- rep(0, n.comb)   # column2: prices for certified
price.noncertified <- rep(0, n.comb)   # column3: prices for noncertified
demand.certified <- rep(0, n.comb)   # column4: clients who would buy certified
demand.noncertified <- rep(0, n.comb)   # column5: clients who would buy noncertified
for(i in 1:price.points){
    for(j in 1:price.points){
        idx <- (i-1)*price.points+j
        price.certified[idx] <- price.c[i]
        price.noncertified[idx] <- price.nc[j]
        demand.certified[idx] <- sum((surplus.c[,i]>surplus.nc[,j]) *
                                       (surplus.c[,i]>=0))
        demand.noncertified[idx] <- sum((surplus.nc[,j]>=surplus.c[,i]) *
                                    (surplus.nc[,j]>=0))
    }
}
df.comb <- data.frame(index, 
                      price.certified, price.noncertified, 
                      demand.certified, demand.noncertified)

# visualize revenue as a function of different prices ----
df.comb$revenue <- df.comb$price.certified * df.comb$demand.certified + 
    df.comb$price.noncertified * df.comb$demand.noncertified

# visualize revenue as a function of certified and noncertified prices
df.comb.certified <- df.comb
#trellis.par.set("axis.line",list(col=NA,lty=1,lwd=1))
plot.3d1 <- wireframe(revenue ~ price.certified * price.noncertified, 
                      data = df.comb.certified,
                      shade = FALSE, #scales = list(arrows = FALSE),
                      perspective = FALSE,
                      xlab = 'price.certified', ylab = 'price.noncertified')
plot.3d1

# uncomment for the other two attributes
# visualize revenue as a function of large and medium prices
# df.comb.large <- df.comb
# plot.3d2 <- wireframe(revenue ~ price.certified * price.noncertified,
#                       data = df.comb.large,
#                       shade = FALSE, #scales = list(arrows = FALSE),
#                       perspective = FALSE,
#                       
#                       xlab = 'price.large', ylab = 'price.medium')
# plot.3d2

# visualize revenue as a function of smoked and live prices
# df.comb.smoked <- df.comb
# plot.3d3 <- wireframe(revenue ~ price.certified * price.noncertified,
#                       data = df.comb.smoked,
#                       shade = FALSE, #scales = list(arrows = FALSE),
#                       perspective = FALSE,
#                       xlab = 'price.smoked', ylab = 'price.live')
# plot.3d3

# use "nloptr" to find optimal revenue by optimization ----
# run regression for certified/noncertified demands
fit.c <- lm(demand.certified ~ price.certified + price.noncertified, 
            data = df.comb)  # for certified demand
#summary(fit.c)
#coef(fit.c)
fit.nc <- lm(demand.noncertified ~ price.certified + price.noncertified, 
             data = df.comb)  # for noncertified demand
# define objective function
eval_f <- function(x){
    price.certified <- x[1]
    price.noncertified <- x[2]
    pred.c <- predict(fit.c, data.frame(price.certified, price.noncertified)) 
    pred.nc <- predict(fit.nc, data.frame(price.certified, price.noncertified))
    demand.c <- max(0, pred.c)
    demand.nc <- max(0, pred.nc)
    revenue <- price.certified * demand.c + price.noncertified * demand.nc
    obj.func <- -revenue
    return(obj.func)
}
# define inequality constraints
eval_g_ineq <- function(x){
    price.certified <- x[1]
    price.noncertified <- x[2]
    pred.c <- predict(fit.c, data.frame(price.certified, price.noncertified)) 
    pred.nc <- predict(fit.nc, data.frame(price.certified, price.noncertified))
    demand.c <- max(0, pred.c)
    demand.nc <- max(0, pred.nc)
    constraint <- c(-demand.c,
                    -demand.nc,
                    x[2] - x[1])   # assume noncertified<=certified
    return(constraint)
}
# initial values
x0 <- c(1500,1500) 
# lower and upper bounds of control
lb <- c(50,50)
ub <- c(2850,2850)
# options
opts <- list('algorithm' = 'NLOPT_LN_COBYLA',
             'xtol_rel'  = 1.0e-9,
             'maxeval'   = 1000)
# solve the optimization problem
result <- nloptr(x0 = x0, eval_f = eval_f, lb = lb, ub = ub,
                 eval_g_ineq = eval_g_ineq, opts = opts)

# get results for certified/noncertified prices
price.opt.certified <- result$solution
revenue.opt.certified <- -result$objective
# print(paste('Optimal certified price is:', round(price.opt.certified[1],2)))
# print(paste('Optimal noncertified price is:', round(price.opt.certified[2],2)))
# print(paste('Optimal revenue is:', round(revenue.opt.certified,2)))

# uncomment for the other two attributes
# get results for large/medium prices
# price.opt.large <- result$solution
# revenue.opt.large <- -result$objective
# print(paste('Optimal large price is:', round(price.opt.large[1],2)))
# print(paste('Optimal medium price is:', round(price.opt.large[2],2)))
# print(paste('Optimal revenue is:', round(revenue.opt.large,2)))

# get results for smoked/live prices
# price.opt.smoked <- result$solution
# revenue.opt.smoked <- -result$objective
# print(paste('Optimal smoked price is:', round(price.opt.smoked[1],2)))
# print(paste('Optimal live price is:', round(price.opt.smoked[2],2)))
# print(paste('Optimal revenue is:', round(revenue.opt.smoked,2)))

## 4. use MNL demand model to estimate price/demand relationship ----
# calculate parameter estimates for MNL model
wtp.mean <- apply(wtp[1:8], 2, mean)   # average WTPs across 8 products
wtp.var <- apply(wtp[1:8], 2, var)   # variances across 8 products
wtp.var.mean <- mean(wtp.var)
# prepare parameters for MNL model
util <- wtp.mean   # u_j - gross utilities
mu <- sqrt(wtp.var.mean*6)/pi   # mu - shape parameter
# initialize parameters
n3 <- 3   # set 3 prices for each product
price.diff <- 100   # set a price difference
wtp.median <- apply(wtp[1:8], 2, median)   # median of WTPs across 8 products
price.assort <- matrix(rep(0,n3*n2), n2, n3)   # total number of prices to set=n2*n3
attract <- matrix(rep(0,n3*n2), n2, n3)   # attraction values matrix
prob <- matrix(rep(0,n3*n2), n2, n3)   # probabilities matrix
# set prices for 8 products
for(i in 1:n2){
    price.assort[i,1] <- wtp.median[i] - price.diff
    price.assort[i,2] <- wtp.median[i]
    price.assort[i,3] <- wtp.median[i] + price.diff
}
# compute attraction values
for(i in 1:n2){
    attract[i,] <- exp((util[i] - price.assort[i,]) / mu)    #v_j - attraction values
}
# stack matrix to one column (price & attraction value for each alternative)
p <- c(price.assort[1,], price.assort[2,], price.assort[3,], price.assort[4,],
       price.assort[5,], price.assort[6,], price.assort[7,], price.assort[8,])
v <- c(attract[1,], attract[2,], attract[3,], attract[4,],
       attract[5,], attract[6,], attract[7,], attract[8,])

## 5. joint pricing and assortment optimization ----
# maximum assortment size 
size <- 8
# number of alternatives
n <- n3*n2
# attraction value for no purchase
v0 <- 1
# normalize attraction values (to have sum = 1)
v <- v / (sum(v) + v0)
# obtain 2 parameters for Markov Chain Choice model
a <- v   # a_i - first choice probabilities
b <- matrix(0, n, n)   # b_ij - the transition matrix
for (i in 1:n){
    for (j in 1:n){
        if(i != j) b[i,j] = v[j] / (1 - v[i])
    }
}

# construct LP to obtain optimal assortment ----
# objective function
obj.func <- c(p, rep(0, n), rep(0, n))

# balance equations
lhs11 <- diag(1, n, n)
lhs12 <- diag(1, n, n) - t(b)
lhs13 <- diag(0, n, n)
lhs1 <- cbind(lhs11, lhs12, lhs13)
# link indicator variables to purchase probabilities
lhs21 <- -diag(1, n, n)
lhs22 <- diag(0, n, n)
lhs23 <- diag(1, n, n)
lhs2 <- cbind(lhs21, lhs22, lhs23)
# cardinality
lhs31 <- rep(0, n)
lhs32 <- rep(0, n)
lhs33 <- rep(1, n)
lhs3 <- c(lhs31, lhs32, lhs33)
# pick at most 1 among subsets
lhs41 <- matrix(rep(0, n2*n), n2, n)
lhs42 <- matrix(rep(0, n2*n), n2, n)
lhs43 <- matrix(c(c(rep(1,3), rep(0,21)),
                  c(rep(0,3), rep(1,3), rep(0,18)),
                  c(rep(0,6), rep(1,3), rep(0,15)),
                  c(rep(0,9), rep(1,3), rep(0,12)),
                  c(rep(0,12), rep(1,3), rep(0,9)),
                  c(rep(0,15), rep(1,3), rep(0,6)),
                  c(rep(0,18), rep(1,3), rep(0,3)),
                  c(rep(0,21), rep(1,3))), nrow = n2, ncol = n, byrow = TRUE)
lhs4 <- cbind(lhs41, lhs42, lhs43)

const <- rbind(lhs1, lhs2, lhs3, lhs4)

# constraints - right hand side
rhs1 <- a
rhs2 <- rep(0, n)
rhs3 <- size
rhs4 <- rep(1, n2)
rhs <- c(rhs1, rhs2, rhs3, rhs4)

# constraint directions
dir1 <- rep('=', n)
dir2 <- rep('>=', n)
dir3 <- '='   # if must contain 8 products, set as '='; if maximum, set as '<='
dir4 <- rep('<=', n2)
const.dir <- c(dir1, dir2, dir3, dir4)
# binary variables (declare their indices)
binary.index <- (2*n + 1):(3*n)

# solve LP
lp1 <- lp('max', obj.func, 
          const, const.dir, rhs, 
          binary.vec = binary.index,
          compute.sens = TRUE)   # sensitivity
# optimal values for decision variable
lp1$solution
# optimal value for objective function
lp1$objval

# print optimal assortment
assort.best <- which(lp1$solution[1:n] > 0)
assort.best   # The optimal assortment
# print(paste0('The optimal expected revenue is:', round(lp1$objval*100, 3)))

## 6. markdown pricing ----
options(digits = 2) 
# initial WTP for 2 products which are chosen in the assortment
wtp1 <- wtp$'Certified, smoked, large'   # Certified, smoked, large
wtp2 <- wtp$'Non-certified, smoked, large'   # Non-certified, smoked, large
# inventory level
inv <- c(300,300)
# number of days to sell
day <- 5
# assume mean decrease in WTPs by day
wtp.decrease <- 50
# salvage price
price.salvage <- c(300,200)

# WTPs for all the days
wtp.md1 <- matrix(rep(0,n1*day), nrow = n1, ncol = day)
wtp.md2 <- matrix(rep(0,n1*day), nrow = n1, ncol = day)
for (i in 1:day){
    wtp.md1[,i] <- wtp1 - wtp.decrease * (i - 1)
    wtp.md2[,i] <- wtp2 - wtp.decrease * (i - 1)
}

# scenario1: a single price throughout the horizon ----
eval_f <- function(x){
    x <- round(x,0)
    price1 <- x[1]   # a single price for product1: Certified, smoked, large
    price2 <- x[2]   # a single price for product2: Non-certified, smoked, large
    # initialize demand vector
    d1 <- rep(0,day)
    d2 <- rep(0,day)
    surplus1 <- matrix(rep(0,n1*day), nrow = n1, ncol = day)
    surplus2 <- matrix(rep(0,n1*day), nrow = n1, ncol = day)
    for (i in 1:day){
        surplus1[,i] <- wtp.md1[,i] - price1
        surplus2[,i] <- wtp.md2[,i] - price2
        d1[i] <- sum(surplus1[,i] >= 0)
        d2[i] <- sum(surplus2[,i] >= 0)
    }
    # salvage 
    salvage1 <- inv[1] - sum(d1)
    salvage2 <- inv[2] - sum(d2)
    # expected revenue
    revenue <- price1*sum(d1) + price.salvage[1]*salvage1 + 
        price2*sum(d2) + price.salvage[2]*salvage2
    # set objective function
    obj.func <- -revenue
    return(obj.func)
}
eval_g_ineq <- function(x){
    x <- round(x,0)
    price1 <- x[1]   # a single price for product1: Certified, smoked, large
    price2 <- x[2]   # a single price for product2: Non-certified, smoked, large
    # initialize demand vector
    d1 <- rep(0,day)
    d2 <- rep(0,day)
    surplus1 <- matrix(rep(0,n1*day), nrow = n1, ncol = day)
    surplus2 <- matrix(rep(0,n1*day), nrow = n1, ncol = day)
    for (i in 1:day){
        surplus1[,i] <- wtp.md1[,i] - price1
        surplus2[,i] <- wtp.md2[,i] - price2
        d1[i] <- sum(surplus1[,i] >= 0)
        d2[i] <- sum(surplus2[,i] >= 0)
    }
    # salvage 
    salvage1 <- inv[1] - sum(d1)
    salvage2 <- inv[2] - sum(d2)
    # enforce constraints
    constraint <- c(-salvage1,   # salvage>=0
                    -salvage2,
                    -d1,   # demand>=0
                    -d2)
    return(constraint)
}
# initial values
x0 <- c(1000,1000)
# lower and upper bounds of control
lb <- price.salvage
ub <- c(2850,2850)
# options
opts <- list( "algorithm" = "NLOPT_LN_COBYLA",
              "xtol_rel"  = 1.0e-9,
              "maxeval"   = 1000)
result.single <- nloptr(x0=x0, eval_f=eval_f, lb=lb, ub=ub,
                        eval_g_ineq=eval_g_ineq, opts=opts)
price.opt.single <- result.single$solution
revenue.opt.single <- - result.single$objective

# scenario2: optimal pricing markdown strategy ----
eval_f <- function(x){
    x <- round(x,0)
    price1 <- x[1:day]   # md prices for product1: Certified, smoked, large
    price2 <- x[(day+1):(day*2)]   # md prices for product2: Non-certified, smoked, large
    # initialize demand vector
    d1 <- rep(0,day)
    d2 <- rep(0,day)
    surplus1 <- matrix(rep(0,n1*day), nrow = n1, ncol = day)
    surplus2 <- matrix(rep(0,n1*day), nrow = n1, ncol = day)
    for (i in 1:day){
        surplus1[,i] <- wtp.md1[,i] - price1[i]
        surplus2[,i] <- wtp.md2[,i] - price2[i]
        d1[i] <- sum(surplus1[,i] >= 0)
        d2[i] <- sum(surplus2[,i] >= 0)
    }
    # salvage 
    salvage1 <- inv[1] - sum(d1)
    salvage2 <- inv[2] - sum(d2)
    # expected revenue
    revenue <- sum(price1*d1) + price.salvage[1]*salvage1 + 
        sum(price2*d2) + price.salvage[2]*salvage2
    # set objective function
    obj.func <- -revenue
    return(obj.func)
}
eval_g_ineq <- function(x){
    x <- round(x,0)
    price1 <- x[1:day]   # md prices for product1: Certified, smoked, large
    price2 <- x[(day+1):(day*2)]   # md prices for product2: Non-certified, smoked, large
    # initialize demand vector
    d1 <- rep(0,day)
    d2 <- rep(0,day)
    surplus1 <- matrix(rep(0,n1*day), nrow = n1, ncol = day)
    surplus2 <- matrix(rep(0,n1*day), nrow = n1, ncol = day)
    for (i in 1:day){
        surplus1[,i] <- wtp.md1[,i] - price1[i]
        surplus2[,i] <- wtp.md2[,i] - price2[i]
        d1[i] <- sum(surplus1[,i] >= 0)
        d2[i] <- sum(surplus2[,i] >= 0)
    }
    # salvage 
    salvage1 <- inv[1] - sum(d1)
    salvage2 <- inv[2] - sum(d2)
    # enforce constraints
    constraint <- c(-salvage1,   # salvage>=0
                    -salvage2,
                    -d1,   # demand>=0
                    -d2,
                    diff(price1),
                    diff(price2))
    return(constraint)
}
# initial values
x0 <- c(rep(1000,day), rep(1000,day))
# lower and upper bounds of control
lb <- c(rep(price.salvage[1],day), rep(price.salvage[2],day))
ub <- c(rep(2850,day), rep(2850,day))
# options
opts <- list( "algorithm" = "NLOPT_LN_COBYLA",
              "xtol_rel"  = 1.0e-9,
              "maxeval"   = 1000)
result.md <- nloptr(x0=x0, eval_f=eval_f, lb=lb, ub=ub,
                    eval_g_ineq=eval_g_ineq, opts=opts)
price.opt.md <- result.md$solution
revenue.opt.md <- - result.md$objective
