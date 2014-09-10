
## Steve Bronder
## Download Rstudio
## Page 66
## Example Markov Chain in Action
## Stages of Retiopathic Degenration

#Create 5x5 matrix using concactenate function
# When using concactenate in matrix allow space relative to matrix size

Tran.matrix <- matrix(c(.65,.27,.06,.03,.00
                       ,.16,.59,.20,.04,.01
                       ,.04,.40,.44,.12,.01
                       ,.00,.13,.38,.38,.13
                       ,.00,.00,.00,.00,1.0),5,5,byrow=TRUE)

# Take fifth power of Tran.matrix 
# Use Matrix Multiplication %*%

Tran.matrix.two <- Tran.matrix%*%Tran.matrix

Tran.matrix.four <- Tran.matrix.two%*%Tran.matrix.two


# Create a special funciton to automate this process
# can use %^% as a matrix power
`%^%` <- function(x,N){
  x.n <- x
  for (i in 1:N){
    x.n <- x.n%*%x
  }
return(x.n)
  }
    
  
# Get initial distribution pie to the zero from page 66
init.dist0 <- c(.337,.449,.194,.2,0)

# Find initial distribution to the fourth
# mulitply by transition matrix of step four
init.dist4 <- init.dist0%*%Tran.matrix.four


#########################################
#########################################
### Page 68: taxi Cab ###################
#########################################


# Get iniital condition

init.cond <- c(.3,.4,.3)

# Input one hour transition matrix

Tran.matrix.taxi <- matrix(c(.4,.1,.5,.2,.2,.6,.3,.4,.3),3,3,byrow=TRUE)

# Use previous function to find trans matrix to eight

Tran.matrix.eight <- Tran.matrix.taxi%^%8

init.cond.eight <- init.cond%*%Tran.matrix.eight


