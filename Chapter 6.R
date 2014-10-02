#Chapter 6


Tran.matrix <- matrix(c(.4,.1,.5,
                        .2,.2,.6,
                        .3,.4,.3),3,3,byrow=TRUE)

#build vector of ones
e <- rep(1,3)
e

#Build Matrix of ones
E <- matrix(1,3,3)
E

# Identity matrix
I <- diag(1,3,3)

# Function to higher powers
`%^%` <- function(x,N){
  x.n <- x
  for (i in 1:N){
    x.n <- x.n%*%x
  }
  return(x.n)
}

# Find limiting distribution of Ergodic Chain starting points
Pie <- e%*%solve((I+E-Tran.matrix))
Pie

# Alt, note Tran to a high power
Tran.matrix%^%32

################################
#### Problem 6.14 ##############
################################


Tran.matrix <- matrix(c(.915,.010,.040,.010,.025,.000,
                        .006,.946,.014,.015,.015,.004,
                        .013,.050,.845,.057,.032,.003,
                        .002,.017,.014,.948,.014,.005,
                        .015,.000,.000,.045,.932,.008,
                        .000,.000,.000,.030,.000,.970),6,6,byrow=TRUE)

#build vector of ones
e <- rep(1,6)
e

#Build Matrix of ones
E <- matrix(1,6,6)
E

# Identity matrix
I <- diag(1,6,6)

# Find limiting distribution of Ergodic Chain starting points
Pie <- e%*%solve((I+E-Tran.matrix))
Pie

# Every time we look at a piece of land there is a six percent chance it is
# water, a nineteen percent chance it is forest, a seven percent chance it is 
# grassland, a thirty seven percent chance it is agricultural, a seventeen
# percent chance it is residential, and a fourteen percent chance it is
# commercial/residential.