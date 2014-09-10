
## Steve Bronder
## Download Rstudio
## Page 66
## Example Markov Chain in Action
## Stages of Retiopathic Degenration

##############
#Problem One##
##############

#Create 4x4 matrix using concactenate function
# When building matrix use spaces for good code

Tran.matrix.1 <- matrix(c(.1,.3,.2,.4
                       ,.2,.3,.2,.3
                       ,.3,.3,.1,.3
                       ,.2,.1,.4,.3),4,4,byrow=TRUE)


# Create a special funciton to automate this process
# can use %^% as a matrix power
`%^%` <- function(x,N){
  x.n <- x
  for (i in 1:N){
    x.n <- x.n%*%x
  }
  return(x.n)
}

#sub a. B-A-B-C-B-A
path <- .2*.3*.2*.3*.2

path

#sub b. Find probabilities given X0 = B

Init.space.1.0 <- c(0,1,0,0)

Init.space.1.1<- Init.space.1.0%*%Tran.matrix.1

A1 <- Init.space.1.1[1]

A1
Init.space.1.3 <- Init.space.1.0%*%(Tran.matrix.1%^%3)

C3 <- Init.space.1.3[3]

C3
Init.space.1.4 <- Init.space.1.0%*%(Tran.matrix.1%^%4)

B4 <- Init.space.1.4[2]

B4

Init.space.1.5 <- Init.space.1.0%*%(Tran.matrix.1%^%5)

B5 <- Init.space.1.5[2]

B5

# sub c. Find Tran matrix to fifth power
Tran.matrix.1.5<- Tran.matrix.1%^%5

Tran.matrix.1.5

# sub d. Find A5 given X0=B

A5 <- Init.space.1.5[1]

# sub e. Find pie5 given pie0

Init.matrix.1.e <- c(.2,.4,.3,.1)

Init.matrix.1.e5 <- Init.matrix.1.e%*%Tran.matrix.1.5

Init.matrix.1.e5

####################################################

##############
#Problem Nine#
##############


Tran.matrix.9 <- matrix(c(.62,.11,.05,.15,.05,.00
                         ,.00,.47,.00,.32,.21,.00
                         ,.00,.18,.45,.09,.09,.18
                         ,.00,.00,.00,.50,.50,.00
                         ,.00,.00,.00,.33,.67,.00
                         ,.00,.00,.00,.00,.00,1.0),6,6,byrow=TRUE)


Init.state.9 <- c(47/123,24/123,14/123,20/123,12/123,6/123)

Tran.matrix.9.2<-Tran.matrix.9%^%2

Init.state.2<- Init.state.9%*%Tran.matrix.9.2

Init.state.2


#####################################################

###############
##Problem 23 ##
###############

Tran.matrix.taxi <- matrix(c(.4,.1,.5
                            ,.2,.2,.6
                            ,.3,.4,.3),3,3,byrow=TRUE)

H <- matrix(c(.30476,.63130,.06394
              ,.30476,-.41424,.10948
              ,.30476,-.19602,-.10874),3,3,byrow=TRUE)

J <- matrix(c(00001,.00000,.000000
           ,00000,.17913,.000000
           ,00000,.00000,-.27913),3,3,byrow=TRUE)

Tran.matrix.taxi.b<- H%*%J%*%solve(H)

Tran.matrix.taxi.b

Tran.matrix.taxi.3 <- Tran.matrix.taxi%^%3
Tran.matrix.taxi.3

Tran.matrix.taxi.inf <- Tran.matrix.taxi%^%30
Tran.matrix.taxi.inf

Init.state.taxi <- c(.3,.4,.3)

Init.state.taxi.inf <- Init.state.taxi%*%Tran.matrix.taxi.inf
Init.state.taxi.inf

