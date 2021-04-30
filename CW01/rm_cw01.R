## loading libraries ----
library(lpSolve)
library(scales)

## Question 1: croissants allocation ----
## protection level for lunch ----
# calculate optimal protection level for high-fare demand (numerical inspection)
# using Poisson distribution
mL <- 50   # expected demand for breakfast
mH <- 20   # expected demand for lunch
pL <- 1.00   # price for breakfast
pH <- 1.50   # price for lunch
capacity <- 50   # capacity of croissants
revenue <- rep(0, capacity+1)   # expected revenue
for(i in 1:(capacity+1)){
    protect <- i-1
    avail.low <- capacity - protect   # croissants available for breakfast after protection
    revenue[i] <- 0
    for(dL in 0:100){
        # demand for breakfast
        sold.low <- min(avail.low, dL)
        avail.high <- capacity - sold.low   # remaining croissants for high-fare
        for(dH in 0:100){
            # demand for lunch
            sold.high <- min(avail.high, dH)
            revenue.iter <- pL*sold.low + pH*sold.high
            revenue[i] <- revenue[i] + 
                revenue.iter*dpois(dL,mL)*dpois(dH,mH)
        }
    }
}

## Q1.a Revenue based on FCFS ----
revenue.FCFS = revenue[1]   # set protection level=0
print(paste('The expected revenue based on FCFS:', round(revenue.FCFS, 2)))

## Q1.b Optimal protection for lunch ----
index.best <- which(revenue == max(revenue))
protect.best <- index.best - 1
revenue.optimal <- max(revenue)
print(paste('The optimal protection level for lunch:', protect.best))

## Q1.c Maximum revenue ----
print(paste('The maximum expected revenue:', round(revenue.optimal, 2)))

improve <- round((revenue.optimal - revenue.FCFS)/revenue.FCFS*100, 1)
sprintf('The percent improvement compared to revenue based on FCFS: %s%%', improve)

## plots ----
# plotting expected revenue VS protection level
xaxis <- 0:capacity
plot(xaxis, revenue, pch = 16, cex = 0.5, las = 1, xaxt = 'n',
     xlab = 'Croissants Protected for Lunch', ylab = 'Expected Revenue (in £)',
     cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
# show the optimal value
xticks <- seq(0, capacity, by = 10)
axis(side = 1, at = xticks)
axis(side = 1, at = protect.best)
lines(c(protect.best, protect.best), c(0, revenue.optimal), lty = 2)
axis(side = 2, at = round(revenue.optimal, 2), las = 1)
lines(c(0, protect.best), c(revenue.optimal, revenue.optimal), lty = 2)


## Question 2: Seats allocation ----
# Q2.a Optimal expected total revenue ----
# set up a DP recursion
N1 <- 100   # day 1 seats availability
N2 <- 100   # day 2 seats availability
TT <- 300   # length of time horizon

prob <- c(1/10,1/5,1/10,1/5,1/10,1/5)   # probabilities of request arrival for each of 6 products
prob.noArrival <- 1 - sum(prob)
prob.noArrival # probability of no request=0.1
price <- c(150,100,120,80,250,150)   # prices for 6 products

# initialize arrays for value function and actions
# for the value function V(x1, x2, t)
v <- array(rep(0, len=(N1+1)*(N2+1)*(TT+1)), dim=c(N1+1, N2+1, TT+1))
# for the actions, i.e. accept1(x1, x2, t)
accept1 <- accept2 <- accept3 <- accept4 <- accept5 <- accept6 <- 
    array(rep(0, len=(N1+1)*(N2+1)*(TT+1)), dim=c(N1+1, N2+1, TT+1))

# terminal values
for(i in 1:(N1+1)){
    for(j in 1:(N2+1)){
        v[i,j,1] <- 0   # seats are worthless at the end of horizon
    }
}

# DP algorithm
for(t in 2:(TT+1)){
    for(i in 1:(N1+1)){
        for(j in 1:(N2+1)){
            
            # for no request arrival
            v.arrival0 <- v[i,j,t-1]
            
            # for request for product1 (high-fare for day1)
            v.arrival1 <- v[i,j,t-1]
            accept1[i,j,t] <- 0
            if(i>1){
                v.arrival1 <- max(price[1]+v[i-1,j,t-1], v[i,j,t-1])
                if(price[1]+v[i-1,j,t-1] > v[i,j,t-1]){
                    accept1[i,j,t] <- 1
                }
            }
            # for request for product2 (low-fare for day1)
            v.arrival2 <- v[i,j,t-1]
            accept2[i,j,t] <- 0
            if(i>1){
                v.arrival2 <- max(price[2]+v[i-1,j,t-1], v[i,j,t-1])
                if(price[2]+v[i-1,j,t-1] > v[i,j,t-1]){
                    accept2[i,j,t] <- 1
                }
            }
            
            # for request for product3 (high-fare for day2)
            v.arrival3 <- v[i,j,t-1]
            accept3[i,j,t] <- 0
            if(j>1){
                v.arrival3 <- max(price[3]+v[i,j-1,t-1], v[i,j,t-1])
                if(price[3]+v[i,j-1,t-1] > v[i,j,t-1]){
                    accept3[i,j,t] <- 1
                }
            }
            # for request for product4 (low-fare for day2)
            v.arrival4 <- v[i,j,t-1]
            accept4[i,j,t] <- 0
            if(j>1){
                v.arrival4 <- max(price[4]+v[i,j-1,t-1], v[i,j,t-1])
                if(price[4]+v[i,j-1,t-1] > v[i,j,t-1]){
                    accept4[i,j,t] <- 1
                }
            }
            
            # for request for product5 (high-fare for day1+2)
            v.arrival5 <- v[i,j,t-1]
            accept5[i,j,t] <- 0
            if(i>1){
                if(j>1){
                    v.arrival5 <- max(price[5]+v[i-1,j-1,t-1], v[i,j,t-1])
                    if(price[5]+v[i-1,j-1,t-1] > v[i,j,t-1]){
                        accept5[i,j,t] <- 1
                    }
                }
            }
            # for request for product6 (low-fare for day1+2)
            v.arrival6 <- v[i,j,t-1]
            accept6[i,j,t] <- 0
            if(i>1){
                if(j>1){
                    v.arrival6 <- max(price[6]+v[i-1,j-1,t-1], v[i,j,t-1])
                    if(price[6]+v[i-1,j-1,t-1] > v[i,j,t-1]){
                        accept6[i,j,t] <- 1
                    }
                }
            }
            
            # obtain the total value function from 6 products
            v[i,j,t] <- prob.noArrival*v.arrival0 + 
                prob[1]*v.arrival1 + prob[2]*v.arrival2 + 
                prob[3]*v.arrival3 + prob[4]*v.arrival4 +
                prob[5]*v.arrival5 + prob[6]*v.arrival6
        }
    }
}

# calculate maximum revenue, given capacity levels and remaining time
# for N1=100, N2=100 and TT=300 periods to go
revenue.optimal.seat <- v[101,101,301]
revenue.optimal.seat

# Q2.b Expected revenue on FCFS ----
# initialize arrays for value function and actions
v <- array(rep(0, len=(N1+1)*(N2+1)*(TT+1)), dim=c(N1+1, N2+1, TT+1))
accept1 <- accept2 <- accept3 <- accept4 <- accept5 <- accept6 <- 
    array(rep(0, len=(N1+1)*(N2+1)*(TT+1)), dim=c(N1+1, N2+1, TT+1))

# terminal values
for(i in 1:(N1+1)){
    for(j in 1:(N2+1)){
        v[i,j,1] <- 0   # seats are worthless at the end of horizon
    }
}

# DP algorithm
for(t in 2:(TT+1)){ 
    for(i in 1:(N1+1)){ 
        for(j in 1:(N2+1)){
            
            # for no request arrival 
            v.arrival0 <- v[i,j,t-1] 
            
            # for request for product1 (high-fare for day1)
            v.arrival1 <- v[i,j,t-1] 
            accept1[i,j,t] <- 0
            if(i>1){ 
                v.arrival1 <- price[1]+v[i-1,j,t-1]
                accept1[i,j,t] <- 1
            }
            # for request for product2 (low-fare for day1)
            v.arrival2 <- v[i,j,t-1] 
            accept2[i,j,t] <- 0
            if(i>1){ 
                v.arrival2 <- price[2]+v[i-1,j,t-1]
                accept2[i,j,t] <- 1
            }
            
            # for request for product3 (high-fare for day2)
            v.arrival3 <- v[i,j,t-1]
            accept3[i,j,t] <- 0
            if(j>1){
                v.arrival3 <- price[3]+v[i,j-1,t-1]
                accept3[i,j,t] <- 1
            }
            # for request for product4 (low-fare for day2)
            v.arrival4 <- v[i,j,t-1]
            accept4[i,j,t] <- 0
            if(j>1){
                v.arrival4 <- price[4]+v[i,j-1,t-1]
                accept4[i,j,t] <- 1
            }
            
            # for request for product5 (high-fare for day1+2)
            v.arrival5 <- v[i,j,t-1]
            accept5[i,j,t] <- 0
            if(i>1){
                if(j>1){
                    v.arrival5 <- price[5]+v[i-1,j-1,t-1]
                    accept5[i,j,t] <- 1
                }
            }
            # for request for product6 (low-fare for day1+2)
            v.arrival6 <- v[i,j,t-1]
            accept6[i,j,t] <- 0
            if(i>1){
                if(j>1){
                    v.arrival6 <- price[6]+v[i-1,j-1,t-1]
                    accept6[i,j,t] <- 1
                }
            }
            
            # obtain the total value function from 6 products
            v[i,j,t] <- prob.noArrival*v.arrival0 + 
                prob[1]*v.arrival1 + prob[2]*v.arrival2 + 
                prob[3]*v.arrival3 + prob[4]*v.arrival4 +
                prob[5]*v.arrival5 + prob[6]*v.arrival6
            
        }
    }
}

# FCFS revenue
# for N1=100, N2=100 and TT=300 periods to go
revenue.fcfs <- v[101,101,301]
revenue.fcfs 

# compare optimal revenue with revenue obtained from FCFS
compare <- round((revenue.optimal.seat - revenue.fcfs)/revenue.fcfs*100, 1)
sprintf('The percent improvement compared to revenue based on FCFS: %s%%', compare)


# Q2.c LP bid prices heuristics ----
# step1: obtain bid prices from solving LP ----
# (1) 6 decision variables
# x1~x6 for each of the six products

# (2) objective function coefficients
obj.fun <- price   # revenue contribution of each decision var

# (3) constraint coef
capacity.day1 <- c(1,1,0,0,1,1)
capacity.day2 <- c(0,0,1,1,1,1)
demand <- diag(1, 6, 6)
constr <- rbind(capacity.day1, capacity.day2, demand)

# (4) constraint directions
constr.dir <- c(rep('<=', 2), rep('<=', 6))

# (5) right-hand side values
exp.demand <- prob * 300  #  mean demand for each product
rhs <- c(100,100, exp.demand)  # seats capacities and mean demands

# solve LP
sol <- lp('max', obj.fun, constr, constr.dir, rhs, compute.sens = TRUE)
# optimal solution
sol$solution
for(i in 1:6){
    print(paste("Allocate", sol$solution[i], 'seats for product', i))
}
# optimal objective function value
sol$objval
print(paste('The optimal revenue ($) is:', sol$objval, '(UPPER BOUND)'))

# obtain bid prices for heuristics
bidprices <- sol$duals[1:2]
print(paste('The bid price for Day 1 ticket:', bidprices[1]))
print(paste('The bid price for Day 2 ticket:', bidprices[2]))

# step2: determine a heuristic policy using those bid prices and implement ----
# that is to accept all the products except product6 (which is low-fare for day1+2)

# implement bid price heuristic
v <- array(rep(0, len=(N1+1)*(N2+1)*(TT+1)), dim=c(N1+1, N2+1, TT+1))
accept1 <- accept2 <- accept3 <- accept4 <- accept5 <- accept6 <- 
    array(rep(0, len=(N1+1)*(N2+1)*(TT+1)), dim=c(N1+1, N2+1, TT+1))
# terminal values
for(i in 1:(N1+1)){
    for(j in 1:(N2+1)){
        v[i,j,1] <- 0
    }
}
# DP algorithm
for(t in 2:(TT+1)){ 
    for(i in 1:(N1+1)){ 
        for(j in 1:(N2+1)){
            
            # for no request arrival 
            v.arrival0 <- v[i,j,t-1] 
            
            # for request for product1 (high-fare for day1)
            v.arrival1 <- v[i,j,t-1] 
            accept1[i,j,t] <- 0
            if(i>1){ 
                v.arrival1 <- price[1]+v[i-1,j,t-1]
                accept1[i,j,t] <- 1
            }
            # for request for product2 (low-fare for day1)
            v.arrival2 <- v[i,j,t-1] 
            accept2[i,j,t] <- 0
            if(i>1){ 
                v.arrival2 <- price[2]+v[i-1,j,t-1]
                accept2[i,j,t] <- 1
            }
            
            # for request for product3 (high-fare for day2)
            v.arrival3 <- v[i,j,t-1]
            accept3[i,j,t] <- 0
            if(j>1){
                v.arrival3 <- price[3]+v[i,j-1,t-1]
                accept3[i,j,t] <- 1
            }
            # for request for product4 (low-fare for day2)
            v.arrival4 <- v[i,j,t-1]
            accept4[i,j,t] <- 0
            if(j>1){
                v.arrival4 <- price[4]+v[i,j-1,t-1]
                accept4[i,j,t] <- 1
            }
            
            # for request for product5 (high-fare for day1+2)
            v.arrival5 <- v[i,j,t-1]
            accept5[i,j,t] <- 0
            if(i>1){
                if(j>1){
                    v.arrival5 <- price[5]+v[i-1,j-1,t-1]
                    accept5[i,j,t] <- 1
                }
            }
            # for request for product6 (low-fare for day1+2)
            v.arrival6 <- v[i,j,t-1]
            accept6[i,j,t] <- 0

            # obtain the total value function from 6 products
            v[i,j,t] <- prob.noArrival*v.arrival0 + 
                prob[1]*v.arrival1 + prob[2]*v.arrival2 + 
                prob[3]*v.arrival3 + prob[4]*v.arrival4 +
                prob[5]*v.arrival5 + prob[6]*v.arrival6
            
        }
    }
}

# bid price heuristic revenue
revenue.bid <- v[101,101,301]  
print(paste('The expected revenue ($) from the bid-price heuristic is:', 
            format(round(revenue.bid, 2), nsmall = 2)))
(revenue.bid - revenue.fcfs)/revenue.fcfs * 100
(revenue.bid - revenue.optimal.seat)/revenue.optimal.seat * 100

# Q2.d Design our own heuristic solution ----
# initialize arrays for value function and actions
v <- array(rep(0, len=(N1+1)*(N2+1)*(TT+1)), dim=c(N1+1, N2+1, TT+1))
accept1 <- accept2 <- accept3 <- accept4 <- accept5 <- accept6 <- 
    array(rep(0, len=(N1+1)*(N2+1)*(TT+1)), dim=c(N1+1, N2+1, TT+1))

# terminal values
for(i in 1:(N1+1)){
    for(j in 1:(N2+1)){
        v[i,j,1] <- 0   # seats are worthless at the end of horizon
    }
}

# DP algorithm
for(t in 2:(TT+1)){
    for(i in 1:(N1+1)){
        for(j in 1:(N2+1)){
            
            # for no request arrival
            v.arrival0 <- v[i,j,t-1]
            
            # for request for product1 (high-fare for day1)
            v.arrival1 <- v[i,j,t-1]
            accept1[i,j,t] <- 0
            if(i>1){
                v.arrival1 <- price[1]+v[i-1,j,t-1]
                accept1[i,j,t] <- 1
            }
            # for request for product2 (low-fare for day1)
            v.arrival2 <- v[i,j,t-1]
            accept2[i,j,t] <- 0
            if(i>1){
                v.arrival2 <- max(price[2]+v[i-1,j,t-1], v[i,j,t-1])
                if(price[2]+v[i-1,j,t-1] > v[i,j,t-1]){
                    accept2[i,j,t] <- 1
                }
            }
            
            # for request for product3 (high-fare for day2)
            v.arrival3 <- v[i,j,t-1]
            accept3[i,j,t] <- 0
            if(j>1){
                v.arrival3 <- price[3]+v[i,j-1,t-1]
                accept3[i,j,t] <- 1
            }
            # for request for product4 (low-fare for day2)
            v.arrival4 <- v[i,j,t-1]
            accept4[i,j,t] <- 0
            if(j>1){
                v.arrival4 <- max(price[4]+v[i,j-1,t-1], v[i,j,t-1])
                if(price[4]+v[i,j-1,t-1] > v[i,j,t-1]){
                    accept4[i,j,t] <- 1
                }
            }
            
            # for request for product5 (high-fare for day1+2)
            v.arrival5 <- v[i,j,t-1]
            accept5[i,j,t] <- 0
            if(i>1){
                if(j>1){
                    v.arrival5 <- price[5]+v[i-1,j-1,t-1]
                    accept5[i,j,t] <- 1
                }
            }
            # for request for product6 (low-fare for day1+2)
            v.arrival6 <- v[i,j,t-1]
            accept6[i,j,t] <- 0
            if(i>1){
                if(j>1){
                    v.arrival6 <- max(price[6]+v[i-1,j-1,t-1], v[i,j,t-1])
                    if(price[6]+v[i-1,j-1,t-1] > v[i,j,t-1]){
                        accept6[i,j,t] <- 1
                    }
                }
            }
            
            # obtain the total value function from 6 products
            v[i,j,t] <- prob.noArrival*v.arrival0 + 
                prob[1]*v.arrival1 + prob[2]*v.arrival2 + 
                prob[3]*v.arrival3 + prob[4]*v.arrival4 +
                prob[5]*v.arrival5 + prob[6]*v.arrival6
        }
    }
}

# obtain our own heuristic revenue
revenue.heur <- v[101,101,301] 
revenue.heur
print(paste('The expected total revenue of heuristic approach:', 
            round(revenue.heur, 2)))

# Q2.e Compare revenues from a and d ----
decrease.heur <- round((revenue.optimal.seat - revenue.heur)/
                           revenue.optimal.seat*100, 3)
sprintf('The percent decrease compared to the optimal revenue: %s%%',
        decrease.heur)

# compare all other revenues with 'upper-lower' bound
library(scales)
print(paste('Bid-price heuristic achieves:',
            percent((revenue.bid - revenue.fcfs)/(sol$objval - revenue.fcfs)),
            'of the revenue potential from FCFS to LP solution.'))
print(paste('Our heuristic achieves:',
            percent((revenue.heur - revenue.fcfs)/(sol$objval - revenue.fcfs)),
            'of the revenue potential from FCFS to LP solution.'))
print(paste('DP achieves:',
            percent((revenue.optimal.seat - revenue.fcfs)/(sol$objval - revenue.fcfs)),
            'of the revenue potential from FCFS to LP solution.'))


## Question 3: car rental ----
# set up a DP recursion
N1 <- 45   # day 1 cars availability
N2 <- 45   # day 2 cars availability
N3 <- 45   # day 3 cars availability
TT <- 100   # length of time horizon

ph <- 0.04
pl <- 0.12
prob <- c(rep(c(ph,pl), 6))   # probabilities of request arrival for each of 12 products
prob.noArrival <- 1 - sum(prob)   # probability of no request=0.04
price <- c(49,45, 44,40, 42,38, 90,85, 83,78, 130,120)   # prices for 12 products

# set up LP
# objective function coefficients
obj.fun <- price
# constraint coef
capacity.day1 <- c(1,1,0,0,0,0,1,1,0,0,1,1)
capacity.day2 <- c(0,0,1,1,0,0,1,1,1,1,1,1)
capacity.day3 <- c(0,0,0,0,1,1,0,0,1,1,1,1)
demand <- diag(1, 12, 12)
constr <- rbind(capacity.day1, capacity.day2, capacity.day3, demand)
# constraint directions
constr.dir <- c(rep('<=', 3), rep('<=', 12))
# right-hand side values
exp.demand <- prob * 250  #  mean demand for each product

# FCFS ----
# initialize arrays for value function and actions
v <- array(rep(0, len=(N1+1)*(N2+1)*(N3+1)*(TT+1)), 
           dim=c(N1+1, N2+1, N3+1, TT+1))
for(i in 1:12) { 
    nam <- paste("accept", i, sep = "")
    assign(nam, v)
}

# terminal values
for(i in 1:(N1+1)){
    for(j in 1:(N2+1)){
        for(k in 1:(N3+1)){
            v[i,j,k,1] <- 0   # cars are worthless at the end of horizon
        }
    }
}
# DP algorithm
for(t in 2:(TT+1)){ 
    for(i in 1:(N1+1)){ 
        for(j in 1:(N2+1)){
            for(k in 1:(N3+1)){
                
                # for no request arrival 
                v.arrival0 <- v[i,j,k,t-1] 
                
                # request for product1 (high-fare for day1)
                v.arrival1 <- v[i,j,k,t-1] 
                accept1[i,j,k,t] <- 0
                if(i>1){ 
                    v.arrival1 <- price[1]+v[i-1,j,k,t-1]
                    accept1[i,j,k,t] <- 1
                }
                # request for product2 (low-fare for day1)
                v.arrival2 <- v[i,j,k,t-1] 
                accept2[i,j,k,t] <- 0
                if(i>1){ 
                    v.arrival2 <- price[2]+v[i-1,j,k,t-1]
                    accept2[i,j,k,t] <- 1
                }
                
                # request for product3 (high-fare for day2)
                v.arrival3 <- v[i,j,k,t-1] 
                accept3[i,j,k,t] <- 0
                if(j>1){ 
                    v.arrival3 <- price[3]+v[i,j-1,k,t-1]
                    accept3[i,j,k,t] <- 1
                }
                # request for product4 (low-fare for day2)
                v.arrival4 <- v[i,j,k,t-1] 
                accept4[i,j,k,t] <- 0
                if(j>1){ 
                    v.arrival4 <- price[4]+v[i,j-1,k,t-1]
                    accept4[i,j,k,t] <- 1
                }
                
                # request for product5 (high-fare for day3)
                v.arrival5 <- v[i,j,k,t-1] 
                accept5[i,j,k,t] <- 0
                if(k>1){ 
                    v.arrival5 <- price[5]+v[i,j,k-1,t-1]
                    accept5[i,j,k,t] <- 1
                }
                # request for product6 (low-fare for day3)
                v.arrival6 <- v[i,j,k,t-1] 
                accept6[i,j,k,t] <- 0
                if(k>1){ 
                    v.arrival6 <- price[6]+v[i,j,k-1,t-1]
                    accept6[i,j,k,t] <- 1
                }
                
                # combo products
                # request for product7 (high-fare for day1+2)
                v.arrival7 <- v[i,j,k,t-1]
                accept7[i,j,k,t] <- 0
                if(i>1){
                    if(j>1){
                        v.arrival7 <- price[7]+v[i-1,j-1,k,t-1]
                        accept7[i,j,k,t] <- 1
                    }
                }
                # request for product8 (low-fare for day1+2)
                v.arrival8 <- v[i,j,k,t-1]
                accept8[i,j,k,t] <- 0
                if(i>1){
                    if(j>1){
                        v.arrival8 <- price[8]+v[i-1,j-1,k,t-1]
                        accept8[i,j,k,t] <- 1
                    }
                }
                
                # request for product9 (high-fare for day2+3)
                v.arrival9 <- v[i,j,k,t-1]
                accept9[i,j,k,t] <- 0
                if(j>1){
                    if(k>1){
                        v.arrival9 <- price[9]+v[i,j-1,k-1,t-1]
                        accept9[i,j,k,t] <- 1
                    }
                }
                # request for product10 (low-fare for day2+3)
                v.arrival10 <- v[i,j,k,t-1]
                accept10[i,j,k,t] <- 0
                if(j>1){
                    if(k>1){
                        v.arrival10 <- price[10]+v[i,j-1,k-1,t-1]
                        accept10[i,j,k,t] <- 1
                    }
                }
                
                # request for product11 (high-fare for day1+2+3)
                v.arrival11 <- v[i,j,k,t-1]
                accept11[i,j,k,t] <- 0
                if(i>1){
                    if(j>1){
                        if(k>1){
                            v.arrival11 <- price[11]+v[i-1,j-1,k-1,t-1]
                            accept11[i,j,k,t] <- 1
                        }
                    }
                }
                # request for product12 (low-fare for day1+2+3)
                v.arrival12 <- v[i,j,k,t-1]
                accept12[i,j,k,t] <- 0
                if(i>1){
                    if(j>1){
                        if(k>1){
                            v.arrival12 <- price[12]+v[i-1,j-1,k-1,t-1]
                            accept12[i,j,k,t] <- 1
                        }
                    }
                }
                
                
                # obtain the total value function from 12 products
                v[i,j,k,t] <- prob.noArrival*v.arrival0 + 
                    prob[1]*v.arrival1 + prob[2]*v.arrival2 + 
                    prob[3]*v.arrival3 + prob[4]*v.arrival4 +
                    prob[5]*v.arrival5 + prob[6]*v.arrival6 +
                    prob[7]*v.arrival7 + prob[8]*v.arrival8 + 
                    prob[9]*v.arrival9 + prob[10]*v.arrival10 + 
                    prob[11]*v.arrival11 + prob[12]*v.arrival12
            }
        }
    }
}

rev.fcfs <- v[N1+1,N2+1,N3+1,TT+1] 
rev.fcfs

# bid price heuristic ----
# calculate bid prices based on LP
rhs <- c(N1,N2,N3, exp.demand)  # cars capacities and mean demands
# solve LP
sol.lp <- lp('max', obj.fun, constr, constr.dir, rhs, compute.sens = TRUE)
# optimal solution
rev.lp <- sol.lp$solution
rev.lp
# optimal objective function value
sol.lp$objval
# obtain bid prices for heuristics
bidprices.lp <- sol.lp$duals[1:3]
bidprices.lp

# heuristic rules:
# ACCEPT: product1/2 (high/low day1), product3/4 (high/low day2), product5/6 (high/low day3),
# product7/8 (high/low day1+2), product9/10 (high/low day2+3), product11 (high day1+2+3)
# REJECT: product12 (low day1+2+3)

# initialize arrays for value function and actions
v <- array(rep(0, len=(N1+1)*(N2+1)*(N3+1)*(TT+1)), 
           dim=c(N1+1, N2+1, N3+1, TT+1))
for(i in 1:12) { 
    nam <- paste("accept", i, sep = "")
    assign(nam, v)
}

# terminal values
for(i in 1:(N1+1)){
    for(j in 1:(N2+1)){
        for(k in 1:(N3+1)){
            v[i,j,k,1] <- 0   # cars are worthless at the end of horizon
        }
    }
}
# DP algorithm
for(t in 2:(TT+1)){ 
    for(i in 1:(N1+1)){ 
        for(j in 1:(N2+1)){
            for(k in 1:(N3+1)){
                
                # for no request arrival 
                v.arrival0 <- v[i,j,k,t-1] 
                
                # request for product1 (high-fare for day1)
                v.arrival1 <- v[i,j,k,t-1] 
                accept1[i,j,k,t] <- 0
                if(i>1){ 
                    v.arrival1 <- price[1]+v[i-1,j,k,t-1]
                    accept1[i,j,k,t] <- 1
                }
                # request for product2 (low-fare for day1)
                v.arrival2 <- v[i,j,k,t-1] 
                accept2[i,j,k,t] <- 0
                if(i>1){
                    v.arrival2 <- price[2]+v[i-1,j,k,t-1]
                    accept2[i,j,k,t] <- 1
                }
                
                # request for product3 (high-fare for day2)
                v.arrival3 <- v[i,j,k,t-1] 
                accept3[i,j,k,t] <- 0
                if(j>1){ 
                    v.arrival3 <- price[3]+v[i,j-1,k,t-1]
                    accept3[i,j,k,t] <- 1
                }
                # request for product4 (low-fare for day2)
                v.arrival4 <- v[i,j,k,t-1] 
                accept4[i,j,k,t] <- 0
                if(j>1){
                    v.arrival4 <- price[4]+v[i,j-1,k,t-1]
                    accept4[i,j,k,t] <- 1
                }
                
                # request for product5 (high-fare for day3)
                v.arrival5 <- v[i,j,k,t-1] 
                accept5[i,j,k,t] <- 0
                if(k>1){ 
                    v.arrival5 <- price[5]+v[i,j,k-1,t-1]
                    accept5[i,j,k,t] <- 1
                }
                # request for product6 (low-fare for day3)
                v.arrival6 <- v[i,j,k,t-1] 
                accept6[i,j,k,t] <- 0
                if(k>1){
                    v.arrival6 <- price[6]+v[i,j,k-1,t-1]
                    accept6[i,j,k,t] <- 1
                }
                
                # combo products
                # request for product7 (high-fare for day1+2)
                v.arrival7 <- v[i,j,k,t-1]
                accept7[i,j,k,t] <- 0
                if(i>1){
                    if(j>1){
                        v.arrival7 <- price[7]+v[i-1,j-1,k,t-1]
                        accept7[i,j,k,t] <- 1
                    }
                }
                # request for product8 (low-fare for day1+2)
                v.arrival8 <- v[i,j,k,t-1]
                accept8[i,j,k,t] <- 0
                if(i>1){
                    if(j>1){
                        v.arrival8 <- price[8]+v[i-1,j-1,k,t-1]
                        accept8[i,j,k,t] <- 1
                    }
                }
                
                # request for product9 (high-fare for day2+3)
                v.arrival9 <- v[i,j,k,t-1]
                accept9[i,j,k,t] <- 0
                if(j>1){
                    if(k>1){
                        v.arrival9 <- price[9]+v[i,j-1,k-1,t-1]
                        accept9[i,j,k,t] <- 1
                    }
                }
                # request for product10 (low-fare for day2+3)
                v.arrival10 <- v[i,j,k,t-1]
                accept10[i,j,k,t] <- 0
                if(j>1){
                    if(k>1){
                        v.arrival10 <- price[10]+v[i,j-1,k-1,t-1]
                        accept10[i,j,k,t] <- 1
                    }
                }
                
                # request for product11 (high-fare for day1+2+3)
                v.arrival11 <- v[i,j,k,t-1]
                accept11[i,j,k,t] <- 0
                if(i>1){
                    if(j>1){
                        if(k>1){
                            v.arrival11 <- price[11]+v[i-1,j-1,k-1,t-1]
                            accept11[i,j,k,t] <- 1
                        }
                    }
                }
                # request for product12 (low-fare for day1+2+3)
                v.arrival12 <- v[i,j,k,t-1]
                accept12[i,j,k,t] <- 0
                # if(i>1){
                #     if(j>1){
                #         if(k>1){
                #             v.arrival12 <- price[12]+v[i-1,j-1,k-1,t-1]
                #             accept12[i,j,k,t] <- 1
                #         }
                #     }
                # }
                
                
                # obtain the total value function from 12 products
                v[i,j,k,t] <- prob.noArrival*v.arrival0 + 
                    prob[1]*v.arrival1 + prob[2]*v.arrival2 + 
                    prob[3]*v.arrival3 + prob[4]*v.arrival4 +
                    prob[5]*v.arrival5 + prob[6]*v.arrival6 +
                    prob[7]*v.arrival7 + prob[8]*v.arrival8 + 
                    prob[9]*v.arrival9 + prob[10]*v.arrival10 + 
                    prob[11]*v.arrival11 + prob[12]*v.arrival12
            }
        }
    }
}

rev.bid <- v[N1+1,N2+1,N3+1,TT+1] 
rev.bid

# DP optimal ----
# initialize arrays for value function and actions
v <- array(rep(0, len=(N1+1)*(N2+1)*(N3+1)*(TT+1)), 
           dim=c(N1+1, N2+1, N3+1, TT+1))
for(i in 1:12) { 
    nam <- paste("accept", i, sep = "")
    assign(nam, v)
}

# terminal values
for(i in 1:(N1+1)){
    for(j in 1:(N2+1)){
        for(k in 1:(N3+1)){
            v[i,j,k,1] <- 0   # cars are worthless at the end of horizon
        }
    }
}
# DP algorithm
for(t in 2:(TT+1)){ 
    for(i in 1:(N1+1)){ 
        for(j in 1:(N2+1)){
            for(k in 1:(N3+1)){
                
                # for no request arrival 
                v.arrival0 <- v[i,j,k,t-1] 
                
                # request for product1 (high-fare for day1)
                v.arrival1 <- v[i,j,k,t-1] 
                accept1[i,j,k,t] <- 0
                if(i>1){ 
                    v.arrival1 <- max(price[1]+v[i-1,j,k,t-1], v[i,j,k,t-1])
                    if(price[1]+v[i-1,j,k,t-1] > v[i,j,k,t-1]){
                        accept1[i,j,k,t] <- 1
                    }
                }
                # request for product2 (low-fare for day1)
                v.arrival2 <- v[i,j,k,t-1] 
                accept2[i,j,k,t] <- 0
                if(i>1){ 
                    v.arrival2 <- max(price[2]+v[i-1,j,k,t-1], v[i,j,k,t-1])
                    if(price[2]+v[i-1,j,k,t-1] > v[i,j,k,t-1]){
                        accept2[i,j,k,t] <- 1
                    }
                }
                
                # request for product3 (high-fare for day2)
                v.arrival3 <- v[i,j,k,t-1] 
                accept3[i,j,k,t] <- 0
                if(j>1){ 
                    v.arrival3 <- max(price[3]+v[i,j-1,k,t-1], v[i,j,k,t-1])
                    if(price[3]+v[i,j-1,k,t-1] > v[i,j,k,t-1]){
                        accept3[i,j,k,t] <- 1
                    }
                }
                # request for product4 (low-fare for day2)
                v.arrival4 <- v[i,j,k,t-1] 
                accept4[i,j,k,t] <- 0
                if(j>1){ 
                    v.arrival4 <- max(price[4]+v[i,j-1,k,t-1], v[i,j,k,t-1])
                    if(price[4]+v[i,j-1,k,t-1] > v[i,j,k,t-1]){
                        accept4[i,j,k,t] <- 1
                    }
                }
                
                # request for product5 (high-fare for day3)
                v.arrival5 <- v[i,j,k,t-1] 
                accept5[i,j,k,t] <- 0
                if(k>1){ 
                    v.arrival5 <- max(price[5]+v[i,j,k-1,t-1], v[i,j,k,t-1])
                    if(price[5]+v[i,j,k-1,t-1] > v[i,j,k,t-1]){
                        accept5[i,j,k,t] <- 1
                    }
                }
                # request for product6 (low-fare for day3)
                v.arrival6 <- v[i,j,k,t-1] 
                accept6[i,j,k,t] <- 0
                if(k>1){ 
                    v.arrival6 <- max(price[6]+v[i,j,k-1,t-1], v[i,j,k,t-1])
                    if(price[6]+v[i,j,k-1,t-1] > v[i,j,k,t-1]){
                        accept6[i,j,k,t] <- 1
                    }
                }
                
                # combo products
                # request for product7 (high-fare for day1+2)
                v.arrival7 <- v[i,j,k,t-1]
                accept7[i,j,k,t] <- 0
                if(i>1){
                    if(j>1){
                        v.arrival7 <- max(price[7]+v[i-1,j-1,k,t-1], v[i,j,k,t-1])
                        if(price[7]+v[i-1,j-1,k,t-1] > v[i,j,k,t-1]){
                            accept7[i,j,k,t] <- 1
                        }
                    }
                }
                # request for product8 (low-fare for day1+2)
                v.arrival8 <- v[i,j,k,t-1]
                accept8[i,j,k,t] <- 0
                if(i>1){
                    if(j>1){
                        v.arrival8 <- max(price[8]+v[i-1,j-1,k,t-1], v[i,j,k,t-1])
                        if(price[8]+v[i-1,j-1,k,t-1] > v[i,j,k,t-1]){
                            accept8[i,j,k,t] <- 1
                        }
                    }
                }
                
                # request for product9 (high-fare for day2+3)
                v.arrival9 <- v[i,j,k,t-1]
                accept9[i,j,k,t] <- 0
                if(j>1){
                    if(k>1){
                        v.arrival9 <- max(price[9]+v[i,j-1,k-1,t-1], v[i,j,k,t-1])
                        if(price[9]+v[i,j-1,k-1,t-1] > v[i,j,k,t-1]){
                            accept9[i,j,k,t] <- 1
                        }
                    }
                }
                # request for product10 (low-fare for day2+3)
                v.arrival10 <- v[i,j,k,t-1]
                accept10[i,j,k,t] <- 0
                if(j>1){
                    if(k>1){
                        v.arrival10 <- max(price[10]+v[i,j-1,k-1,t-1], v[i,j,k,t-1])
                        if(price[10]+v[i,j-1,k-1,t-1] > v[i,j,k,t-1]){
                            accept10[i,j,k,t] <- 1
                        }
                    }
                }
                
                # request for product11 (high-fare for day1+2+3)
                v.arrival11 <- v[i,j,k,t-1]
                accept11[i,j,k,t] <- 0
                if(i>1){
                    if(j>1){
                        if(k>1){
                            v.arrival11 <- max(price[11]+v[i-1,j-1,k-1,t-1], v[i,j,k,t-1])
                            if(price[11]+v[i-1,j-1,k-1,t-1] > v[i,j,k,t-1]){
                                accept11[i,j,k,t] <- 1
                            }
                        }
                    }
                }
                # request for product12 (low-fare for day1+2+3)
                v.arrival12 <- v[i,j,k,t-1]
                accept12[i,j,k,t] <- 0
                if(i>1){
                    if(j>1){
                        if(k>1){
                            v.arrival12 <- max(price[12]+v[i-1,j-1,k-1,t-1], v[i,j,k,t-1])
                            if(price[12]+v[i-1,j-1,k-1,t-1] > v[i,j,k,t-1]){
                                accept12[i,j,k,t] <- 1
                            }
                        }
                    }
                }
                
                
                # obtain the total value function from 12 products
                v[i,j,k,t] <- prob.noArrival*v.arrival0 + 
                    prob[1]*v.arrival1 + prob[2]*v.arrival2 + 
                    prob[3]*v.arrival3 + prob[4]*v.arrival4 +
                    prob[5]*v.arrival5 + prob[6]*v.arrival6 +
                    prob[7]*v.arrival7 + prob[8]*v.arrival8 + 
                    prob[9]*v.arrival9 + prob[10]*v.arrival10 + 
                    prob[11]*v.arrival11 + prob[12]*v.arrival12
            }
        }
    }
}

rev.dp <- v[N1+1,N2+1,N3+1,TT+1]  
rev.dp

## test different terminal values ----
Revenue_FCFS=rep(0,8)
for(n in 1:8){
    discount_price=seq(0,35,5)
    m=discount_price[n]
    
    # FCFS ----
    # initialize arrays for value function and actions
    v <- array(rep(0, len=(N1+1)*(N2+1)*(N3+1)*(TT+1)), dim=c(N1+1, N2+1, N3+1,TT+1))
    for(i in 1:12) { 
        nam <- paste("accept", i, sep = "")
        assign(nam, v)
    }
    
    # terminal values
    for(i in 1:(N1+1)){
        for(j in 1:(N2+1)){
            for(k in 1:(N3+1)){
                v[i,j,k,1] <- m*(i-1)+m*(j-1)+m*(k-1)   # cars are worthless at the end of horizon
            }
        }
    }
    # DP algorithm
    for(t in 2:(TT+1)){ 
        for(i in 1:(N1+1)){ 
            for(j in 1:(N2+1)){
                for(k in 1:(N3+1)){
                    
                    # for no request arrival 
                    v.arrival0 <- v[i,j,k,t-1] 
                    
                    # request for product1 (high-fare for day1)
                    v.arrival1 <- v[i,j,k,t-1] 
                    accept1[i,j,k,t] <- 0
                    if(i>1){ 
                        v.arrival1 <- price[1]+v[i-1,j,k,t-1]
                        accept1[i,j,k,t] <- 1
                    }
                    # request for product2 (low-fare for day1)
                    v.arrival2 <- v[i,j,k,t-1] 
                    accept2[i,j,k,t] <- 0
                    if(i>1){ 
                        v.arrival2 <- price[2]+v[i-1,j,k,t-1]
                        accept2[i,j,k,t] <- 1
                    }
                    
                    # request for product3 (high-fare for day2)
                    v.arrival3 <- v[i,j,k,t-1] 
                    accept3[i,j,k,t] <- 0
                    if(j>1){ 
                        v.arrival3 <- price[3]+v[i,j-1,k,t-1]
                        accept3[i,j,k,t] <- 1
                    }
                    # request for product4 (low-fare for day2)
                    v.arrival4 <- v[i,j,k,t-1] 
                    accept4[i,j,k,t] <- 0
                    if(j>1){ 
                        v.arrival4 <- price[4]+v[i,j-1,k,t-1]
                        accept4[i,j,k,t] <- 1
                    }
                    
                    # request for product5 (high-fare for day3)
                    v.arrival5 <- v[i,j,k,t-1] 
                    accept5[i,j,k,t] <- 0
                    if(k>1){ 
                        v.arrival5 <- price[5]+v[i,j,k-1,t-1]
                        accept5[i,j,k,t] <- 1
                    }
                    # request for product6 (low-fare for day3)
                    v.arrival6 <- v[i,j,k,t-1] 
                    accept6[i,j,k,t] <- 0
                    if(k>1){ 
                        v.arrival6 <- price[6]+v[i,j,k-1,t-1]
                        accept6[i,j,k,t] <- 1
                    }
                    
                    # combo products
                    # request for product7 (high-fare for day1+2)
                    v.arrival7 <- v[i,j,k,t-1]
                    accept7[i,j,k,t] <- 0
                    if(i>1){
                        if(j>1){
                            v.arrival7 <- price[7]+v[i-1,j-1,k,t-1]
                            accept7[i,j,k,t] <- 1
                        }
                    }
                    # request for product8 (low-fare for day1+2)
                    v.arrival8 <- v[i,j,k,t-1]
                    accept8[i,j,k,t] <- 0
                    if(i>1){
                        if(j>1){
                            v.arrival8 <- price[8]+v[i-1,j-1,k,t-1]
                            accept8[i,j,k,t] <- 1
                        }
                    }
                    
                    # request for product9 (high-fare for day2+3)
                    v.arrival9 <- v[i,j,k,t-1]
                    accept9[i,j,k,t] <- 0
                    if(j>1){
                        if(k>1){
                            v.arrival9 <- price[9]+v[i,j-1,k-1,t-1]
                            accept9[i,j,k,t] <- 1
                        }
                    }
                    # request for product10 (low-fare for day2+3)
                    v.arrival10 <- v[i,j,k,t-1]
                    accept10[i,j,k,t] <- 0
                    if(j>1){
                        if(k>1){
                            v.arrival10 <- price[10]+v[i,j-1,k-1,t-1]
                            accept10[i,j,k,t] <- 1
                        }
                    }
                    
                    # request for product11 (high-fare for day1+2+3)
                    v.arrival11 <- v[i,j,k,t-1]
                    accept11[i,j,k,t] <- 0
                    if(i>1){
                        if(j>1){
                            if(k>1){
                                v.arrival11 <- price[11]+v[i-1,j-1,k-1,t-1]
                                accept11[i,j,k,t] <- 1
                            }
                        }
                    }
                    # request for product12 (low-fare for day1+2+3)
                    v.arrival12 <- v[i,j,k,t-1]
                    accept12[i,j,k,t] <- 0
                    if(i>1){
                        if(j>1){
                            if(k>1){
                                v.arrival12 <- price[12]+v[i-1,j-1,k-1,t-1]
                                accept12[i,j,k,t] <- 1
                            }
                        }
                    }
                    
                    
                    # obtain the total value function from 12 products
                    v[i,j,k,t] <- prob.noArrival*v.arrival0 + 
                        prob[1]*v.arrival1 + prob[2]*v.arrival2 + 
                        prob[3]*v.arrival3 + prob[4]*v.arrival4 +
                        prob[5]*v.arrival5 + prob[6]*v.arrival6 +
                        prob[7]*v.arrival7 + prob[8]*v.arrival8 + 
                        prob[9]*v.arrival9 + prob[10]*v.arrival10 + 
                        prob[11]*v.arrival11 + prob[12]*v.arrival12
                    
                    rev.fcfs <- v[N1+1,N2+1,N3+1,TT+1] 
                    Revenue_FCFS[n]=Revenue_FCFS[n]+rev.fcfs
                }
            }
        }
        
    }
    
}
Revenue_FCFS
# bid price heuristic ----
# calculate bid prices based on LP

Revenue_bid=rep(0,8)
for(n in 1:8){
    discount_price=seq(0,35,5)
    m=discount_price[n]
    
    v <- array(rep(0, len=(N1+1)*(N2+1)*(N3+1)*(TT+1)), dim=c(N1+1, N2+1, N3+1,TT+1))
    for(i in 1:12) { 
        nam <- paste("accept", i, sep = "")
        assign(nam, v)
    }
    
    # terminal values
    for(i in 1:(N1+1)){
        for(j in 1:(N2+1)){
            for(k in 1:(N3+1)){
                v[i,j,k,1] <- m*(i-1)+m*(j-1)+m*(k-1)   # cars are worthless at the end of horizon
            }
        }
    }
    # DP algorithm
    for(t in 2:(TT+1)){ 
        for(i in 1:(N1+1)){ 
            for(j in 1:(N2+1)){
                for(k in 1:(N3+1)){
                    
                    # for no request arrival 
                    v.arrival0 <- v[i,j,k,t-1] 
                    
                    # request for product1 (high-fare for day1)
                    v.arrival1 <- v[i,j,k,t-1] 
                    accept1[i,j,k,t] <- 0
                    if(i>1){ 
                        v.arrival1 <- price[1]+v[i-1,j,k,t-1]
                        accept1[i,j,k,t] <- 1
                    }
                    # request for product2 (low-fare for day1)
                    v.arrival2 <- v[i,j,k,t-1] 
                    accept2[i,j,k,t] <- 0
                    if(i>1){ 
                        v.arrival2 <- price[2]+v[i-1,j,k,t-1]
                        accept2[i,j,k,t] <- 1
                    }
                    
                    # request for product3 (high-fare for day2)
                    v.arrival3 <- v[i,j,k,t-1] 
                    accept3[i,j,k,t] <- 0
                    if(j>1){ 
                        v.arrival3 <- price[3]+v[i,j-1,k,t-1]
                        accept3[i,j,k,t] <- 1
                    }
                    # request for product4 (low-fare for day2)
                    v.arrival4 <- v[i,j,k,t-1] 
                    accept4[i,j,k,t] <- 0
                    if(j>1){ 
                        v.arrival4 <- price[4]+v[i,j-1,k,t-1]
                        accept4[i,j,k,t] <- 1
                    }
                    
                    # request for product5 (high-fare for day3)
                    v.arrival5 <- v[i,j,k,t-1] 
                    accept5[i,j,k,t] <- 0
                    if(k>1){ 
                        v.arrival5 <- price[5]+v[i,j,k-1,t-1]
                        accept5[i,j,k,t] <- 1
                    }
                    # request for product6 (low-fare for day3)
                    v.arrival6 <- v[i,j,k,t-1] 
                    accept6[i,j,k,t] <- 0
                    if(k>1){ 
                        v.arrival6 <- price[6]+v[i,j,k-1,t-1]
                        accept6[i,j,k,t] <- 1
                    }
                    
                    # combo products
                    # request for product7 (high-fare for day1+2)
                    v.arrival7 <- v[i,j,k,t-1]
                    accept7[i,j,k,t] <- 0
                    if(i>1){
                        if(j>1){
                            v.arrival7 <- price[7]+v[i-1,j-1,k,t-1]
                            accept7[i,j,k,t] <- 1
                        }
                    }
                    # request for product8 (low-fare for day1+2)
                    v.arrival8 <- v[i,j,k,t-1]
                    accept8[i,j,k,t] <- 0
                    if(i>1){
                        if(j>1){
                            v.arrival8 <- price[8]+v[i-1,j-1,k,t-1]
                            accept8[i,j,k,t] <- 1
                        }
                    }
                    
                    # request for product9 (high-fare for day2+3)
                    v.arrival9 <- v[i,j,k,t-1]
                    accept9[i,j,k,t] <- 0
                    if(j>1){
                        if(k>1){
                            v.arrival9 <- price[9]+v[i,j-1,k-1,t-1]
                            accept9[i,j,k,t] <- 1
                        }
                    }
                    # request for product10 (low-fare for day2+3)
                    v.arrival10 <- v[i,j,k,t-1]
                    accept10[i,j,k,t] <- 0
                    if(j>1){
                        if(k>1){
                            v.arrival10 <- price[10]+v[i,j-1,k-1,t-1]
                            accept10[i,j,k,t] <- 1
                        }
                    }
                    
                    # request for product11 (high-fare for day1+2+3)
                    v.arrival11 <- v[i,j,k,t-1]
                    accept11[i,j,k,t] <- 0
                    if(i>1){
                        if(j>1){
                            if(k>1){
                                v.arrival11 <- price[11]+v[i-1,j-1,k-1,t-1]
                                accept11[i,j,k,t] <- 1
                            }
                        }
                    }
                    # request for product12 (low-fare for day1+2+3)
                    v.arrival12 <- v[i,j,k,t-1]
                    accept12[i,j,k,t] <- 0
                    ##if(i>1){
                    ##if(j>1){
                    ##if(k>1){
                    ##v.arrival12 <- price[12]+v[i-1,j-1,k-1,t-1]
                    ##accept12[i,j,k,t] <- 1
                    ##}
                    ##}
                    ##}
                    
                    
                    # obtain the total value function from 12 products
                    v[i,j,k,t] <- prob.noArrival*v.arrival0 + 
                        prob[1]*v.arrival1 + prob[2]*v.arrival2 + 
                        prob[3]*v.arrival3 + prob[4]*v.arrival4 +
                        prob[5]*v.arrival5 + prob[6]*v.arrival6 +
                        prob[7]*v.arrival7 + prob[8]*v.arrival8 + 
                        prob[9]*v.arrival9 + prob[10]*v.arrival10 + 
                        prob[11]*v.arrival11 + prob[12]*v.arrival12
                    
                    rev.bid <- v[N1+1,N2+1,N3+1,TT+1] 
                    Revenue_bid[n]=Revenue_bid[n]+rev.bid
                }
            }
        }
        
    }
    
}
Revenue_bid
# DP optimal ----
# initialize arrays for value function and actions

Revenue_dp=rep(0,8)
for(n in 1:8){
    discount_price=seq(0,35,5)
    m=discount_price[n]
    
    v <- array(rep(0, len=(N1+1)*(N2+1)*(N3+1)*(TT+1)), 
               dim=c(N1+1, N2+1, N3+1, TT+1))
    for(i in 1:12) { 
        nam <- paste("accept", i, sep = "")
        assign(nam, v)
    }
    
    # terminal values
    for(i in 1:(N1+1)){
        for(j in 1:(N2+1)){
            for(k in 1:(N3+1)){
                v[i,j,k,1] <- m*(i-1)+m*(j-1)+m*(k-1)   # cars are worthless at the end of horizon
            }
        }
    }
    # DP algorithm
    for(t in 2:(TT+1)){ 
        for(i in 1:(N1+1)){ 
            for(j in 1:(N2+1)){
                for(k in 1:(N3+1)){
                    
                    # for no request arrival 
                    v.arrival0 <- v[i,j,k,t-1] 
                    
                    # request for product1 (high-fare for day1)
                    v.arrival1 <- v[i,j,k,t-1] 
                    accept1[i,j,k,t] <- 0
                    if(i>1){ 
                        v.arrival1 <- max(price[1]+v[i-1,j,k,t-1], v[i,j,k,t-1])
                        if(price[1]+v[i-1,j,k,t-1] > v[i,j,k,t-1]){
                            accept1[i,j,k,t] <- 1
                        }
                    }
                    # request for product2 (low-fare for day1)
                    v.arrival2 <- v[i,j,k,t-1] 
                    accept2[i,j,k,t] <- 0
                    if(i>1){ 
                        v.arrival2 <- max(price[2]+v[i-1,j,k,t-1], v[i,j,k,t-1])
                        if(price[2]+v[i-1,j,k,t-1] > v[i,j,k,t-1]){
                            accept2[i,j,k,t] <- 1
                        }
                    }
                    
                    # request for product3 (high-fare for day2)
                    v.arrival3 <- v[i,j,k,t-1] 
                    accept3[i,j,k,t] <- 0
                    if(j>1){ 
                        v.arrival3 <- max(price[3]+v[i,j-1,k,t-1], v[i,j,k,t-1])
                        if(price[3]+v[i,j-1,k,t-1] > v[i,j,k,t-1]){
                            accept3[i,j,k,t] <- 1
                        }
                    }
                    # request for product4 (low-fare for day2)
                    v.arrival4 <- v[i,j,k,t-1] 
                    accept4[i,j,k,t] <- 0
                    if(j>1){ 
                        v.arrival4 <- max(price[4]+v[i,j-1,k,t-1], v[i,j,k,t-1])
                        if(price[4]+v[i,j-1,k,t-1] > v[i,j,k,t-1]){
                            accept4[i,j,k,t] <- 1
                        }
                    }
                    
                    # request for product5 (high-fare for day3)
                    v.arrival5 <- v[i,j,k,t-1] 
                    accept5[i,j,k,t] <- 0
                    if(k>1){ 
                        v.arrival5 <- max(price[5]+v[i,j,k-1,t-1], v[i,j,k,t-1])
                        if(price[5]+v[i,j,k-1,t-1] > v[i,j,k,t-1]){
                            accept5[i,j,k,t] <- 1
                        }
                    }
                    # request for product6 (low-fare for day3)
                    v.arrival6 <- v[i,j,k,t-1] 
                    accept6[i,j,k,t] <- 0
                    if(k>1){ 
                        v.arrival6 <- max(price[6]+v[i,j,k-1,t-1], v[i,j,k,t-1])
                        if(price[6]+v[i,j,k-1,t-1] > v[i,j,k,t-1]){
                            accept6[i,j,k,t] <- 1
                        }
                    }
                    
                    # combo products
                    # request for product7 (high-fare for day1+2)
                    v.arrival7 <- v[i,j,k,t-1]
                    accept7[i,j,k,t] <- 0
                    if(i>1){
                        if(j>1){
                            v.arrival7 <- max(price[7]+v[i-1,j-1,k,t-1], v[i,j,k,t-1])
                            if(price[7]+v[i-1,j-1,k,t-1] > v[i,j,k,t-1]){
                                accept7[i,j,k,t] <- 1
                            }
                        }
                    }
                    # request for product8 (low-fare for day1+2)
                    v.arrival8 <- v[i,j,k,t-1]
                    accept8[i,j,k,t] <- 0
                    if(i>1){
                        if(j>1){
                            v.arrival8 <- max(price[8]+v[i-1,j-1,k,t-1], v[i,j,k,t-1])
                            if(price[8]+v[i-1,j-1,k,t-1] > v[i,j,k,t-1]){
                                accept8[i,j,k,t] <- 1
                            }
                        }
                    }
                    
                    # request for product9 (high-fare for day2+3)
                    v.arrival9 <- v[i,j,k,t-1]
                    accept9[i,j,k,t] <- 0
                    if(j>1){
                        if(k>1){
                            v.arrival9 <- max(price[9]+v[i,j-1,k-1,t-1], v[i,j,k,t-1])
                            if(price[9]+v[i,j-1,k-1,t-1] > v[i,j,k,t-1]){
                                accept9[i,j,k,t] <- 1
                            }
                        }
                    }
                    # request for product10 (low-fare for day2+3)
                    v.arrival10 <- v[i,j,k,t-1]
                    accept10[i,j,k,t] <- 0
                    if(j>1){
                        if(k>1){
                            v.arrival10 <- max(price[10]+v[i,j-1,k-1,t-1], v[i,j,k,t-1])
                            if(price[10]+v[i,j-1,k-1,t-1] > v[i,j,k,t-1]){
                                accept10[i,j,k,t] <- 1
                            }
                        }
                    }
                    
                    # request for product11 (high-fare for day1+2+3)
                    v.arrival11 <- v[i,j,k,t-1]
                    accept11[i,j,k,t] <- 0
                    if(i>1){
                        if(j>1){
                            if(k>1){
                                v.arrival11 <- max(price[11]+v[i-1,j-1,k-1,t-1], v[i,j,k,t-1])
                                if(price[11]+v[i-1,j-1,k-1,t-1] > v[i,j,k,t-1]){
                                    accept11[i,j,k,t] <- 1
                                }
                            }
                        }
                    }
                    # request for product12 (low-fare for day1+2+3)
                    v.arrival12 <- v[i,j,k,t-1]
                    accept12[i,j,k,t] <- 0
                    if(i>1){
                        if(j>1){
                            if(k>1){
                                v.arrival12 <- max(price[12]+v[i-1,j-1,k-1,t-1], v[i,j,k,t-1])
                                if(price[12]+v[i-1,j-1,k-1,t-1] > v[i,j,k,t-1]){
                                    accept12[i,j,k,t] <- 1
                                }
                            }
                        }
                    }
                    
                    
                    # obtain the total value function from 12 products
                    v[i,j,k,t] <- prob.noArrival*v.arrival0 + 
                        prob[1]*v.arrival1 + prob[2]*v.arrival2 + 
                        prob[3]*v.arrival3 + prob[4]*v.arrival4 +
                        prob[5]*v.arrival5 + prob[6]*v.arrival6 +
                        prob[7]*v.arrival7 + prob[8]*v.arrival8 + 
                        prob[9]*v.arrival9 + prob[10]*v.arrival10 + 
                        prob[11]*v.arrival11 + prob[12]*v.arrival12
                    
                    rev.dp <- v[N1+1,N2+1,N3+1,TT+1] 
                    Revenue_dp[n]=Revenue_dp[n]+rev.dp
                }
            }
        }
    }
}
Revenue_dp