#Chapter 6

#build vector of ones
e <- rep(1,3)
e

#Build Matrix of ones
E <- matrix(1,3,3)
E

# Identity matrix
I <- diag(1,3,3)

Tran.matrix <- matrix(c(.4,.1,.5,
                        .2,.2,.6,
                        .3,.4,.3),3,3,byrow=TRUE)
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
