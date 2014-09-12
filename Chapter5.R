###############
## Chapter 5##
##############

# Create Identity

Identity <- diag(1,4,4)

# Solve for U fundemental matrix

Full.matrix <- matrix(c(1,0,0,0,0,0,
                        0,1,0,0,0,0,
                       .5,0,0,.5,0,0,
                       0,.333,.333,0,.333,0,
                       .333,0,0,.333,0,.333,
                       0,.5,0,0,.5,0),6,6,byrow=TRUE)

# Take Tran part of full matrix
Tran.matrix <- Full.matrix[3:6,3:6]
U <- solve((Identity-Tran.matrix))



#################
## Problem 5.2 ##
#################

## Make Full Matrix
Full.matrix <- matrix(c(1,0,0,0,
                        0,1,0,0,
                        .0204,0,.6373,.3423,
                        0,.0101,.0377,.9522),4,4,byrow=TRUE)

# Pull out Tran
Tran.matrix <- Full.matrix[3:4,3:4]
# Make identity
Identity <- diag(1,2,2)
# Use solve to find U.
#  [1,1] is expected number of times you'll go to blood from blood
U.matrix <- solve((Identity-Tran.matrix))

#Sum over rows.
# [1,1] expected number of times to go to blood
apply(U.matrix,1,sum)

#########
# Find A#
#########

### Pull out Transient to recurrent portion portion of matrix (S)

S.matrix <- Full.matrix[3:4,1:2]

# [1,1] is prob of leaving from urine giving you started at blood
# 22 percent chance of radioactive stuff exiting from urine
A.matrix <- U.matrix%*%S.matrix


######################################################################
######################################################################

###################
## Problem 5.3   ##
###################

# Make Full matrix

Full.matrix <- matrix(c(1,0,0,0,0,
                        0,1,0,0,0,
                        .1,.3,0,.45,.15,
                        .15,.1,0,.25,.5,
                        .2,.05,0,.35,.4))

# Use above as example
