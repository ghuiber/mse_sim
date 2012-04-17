# http://www.foreignpolicy.com/articles/2012/02/27/the_body_counter?page=full
# simulate MSE = Multiple Systems Estimation

# SOME HOUSEKEPING FIRST

# pretty picture comes from here
library("ggplot2")

# true population size
population <- 150000 

# number of guesses you can take to estimate
# maximum-likelihood mean, standard deviation
# of your final guess
n <- 10

# this many draws will help you simulate 
# the uncertainty of your guess
sims <- 10^4

# define a function that will render big numbers with comma
# separators for thousands. the original version is here: 
# https://stat.ethz.ch/pipermail/r-help/2010-November/259488.html
commaUS <- function(x) {
   sprintf("%s", formatC(x, format="fg", big.mark = ","))
}

# THE PIECES OF THE ACTUAL SIMULATION

# make one guess, with daily catches of up to x:
# -- if x=100, the daily catch is between 0 and 99
# -- if x=1000, the daily catch is between 0 and 999.
mymse <- function(x) {
   day1 <- round(runif(1)*x)       # count of fish caught on day 1
   day2 <- round(runif(1)*x)       # count of fish caught on day 2
   pond <- c(1:population)
   day1 <- sample(pond,day1)               # list of fish caught on day 1
   day2 <- sample(pond,day2)               # list of fish caught on day 2
   overlap <- length(intersect(day1,day2)) # count of fish caught twice
   guess <- length(day1)*length(day2)
   if(overlap>0) {
      guess <- guess/overlap
   }
   return(round(guess))
}

# assume that the fish population guesses for a given pond 
# are distributed N(mu,sigma). this is the log likelihood 
# function of a given set of y guesses:
myll <- function(par, y) {
   mu <- par[1]
   sigma <- par[2]    
   l <- length(y)*log(sigma^2)
   a <- 1/sigma^2
   ll <- sum((y-mu)^2)  
   return(-(l+a*ll)/2)
}

# collect n guesses of catches limited to 
# a given maxcatch, then estimate mu, sigma
# and make sims draws from N(mu, sigma)
mysim <- function(n,maxcatch) {
   # 1. make n guesses of fish in the pond
   a <- apply(as.matrix(1:n),1,function(x) mymse(maxcatch))
   # 2. get the maximum likelihood estimates of mu, sigma
   # that might have produced the n guesses:
   opt <- optim(par=c(1,1), fn = myll, control = list(fnscale = -1), 
          y=a, method = "BFGS", hessian = TRUE) 
   # 3. model the uncertainty around these estimates
   b <- rnorm(sims,mean=opt$par[1],sd=opt$par[2])
   return(b)
}   

# THE FINAL PICTURE

# now make some comparisons of the effect of
# maxcatch with a given number of guesses n
# on the population estimate and its precision
maxes  <- c(500,1000,10000,20000)
simspic <- matrix(0,sims,length(maxes))
for(i in 1:length(maxes)) {
   simspic[,i] <- mysim(n,maxes[i])
}

# Set up a stacked 2-column matrix where the second
# column will be used for grouping these guesses
x.1 <- cbind(simspic[,1],maxes[1])
x.2 <- cbind(simspic[,2],maxes[2])
x.3 <- cbind(simspic[,3],maxes[3])
x.4 <- cbind(simspic[,4],maxes[4])

# now plot the guesses with the
# catch limits set in maxes
d <- as.data.frame(rbind(x.1,x.2,x.3,x.4))
p <- qplot(V1, colour=factor(V2), data=d, geom="density", xlab="Fish in the pond")
t <- paste("Precision of the population estimate with",n,"guesses",sep=" ")
p <- p + scale_colour_discrete(name = "Catch limit") + opts(title=t)
p
