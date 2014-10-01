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

Full.matrix <- matrix(c(1.00,0.00,0.00,0.00,0.00,
                        0.00,1.00,0.00,0.00,0.00,
                        0.10,0.30,0.00,0.45,0.15,
                        0.15,0.10,0.00,0.25,0.50,
                        0.20,0.05,0.00,0.35,0.40),5,5,byrow=TRUE)



Tran.matrix <- Full.matrix[3:5,3:5]
# Make identity
Identity <- diag(1,3,3)
# Use solve to find U.
#  [1,1] is expected number of times you'll go to blood from blood
U.matrix <- solve((Identity-Tran.matrix))

U.matrix

apply(U.matrix,1,sum)


#########
# Find A#
#########

### Pull out Transient to recurrent portion portion of matrix (S)

S.matrix <- Full.matrix[3:5,1:2]

# [1,1] is prob of leaving from urine giving you started at blood
# 22 percent chance of radioactive stuff exiting from urine
A.matrix <- U.matrix%*%S.matrix

A.matrix

################################
##### Problem 5.4 ##############
################################

Full.matrix <- matrix(c(1,0,0,0,
                        1/15,14/15,0,0,
                        0,4/15,11/15,0,
                        0,0,3/5,2/5),4,4,byrow=TRUE)


Tran.matrix <- Full.matrix[2:4,2:4]

# Make identity

Identity <- diag(1,3,3)

# Use solve to find U.

#  [1,1] is expected number of times you'll go to blood from blood

U.matrix <- solve((Identity-Tran.matrix))

U.matrix

# Expected value of times to visit each step

apply(U.matrix,1,sum)

################################
################################
######## Problem 5.15 ##########
################################
################################

Full.matrix <- matrix(c(1.00,0.00,0.00,0.00,0.00,0.00,0.00,
                        0.29,0.71,0.00,0.00,0.00,0.00,0.00,
                        0.14,0.26,0.59,0.00,0.00,0.00,0.00,
                        0.04,0.24,0.37,0.35,0.00,0.00,0.00,
                        0.02,0.35,0.14,0.10,0.29,0.00,0.00,
                        0.16,0.14,0.18,0.07,0.18,0.27,0.00,
                        0.20,0.13,0.00,0.00,0.00,0.23,0.43)
                        ,7,7,byrow=TRUE)

Tran.matrix <- Full.matrix[2:7,2:7]
                        

# Make identity
Identity <- diag(1,6,6)

# Use solve to find U.

U.matrix <- solve((Identity-Tran.matrix))

U.matrix

apply(U.matrix,1,sum)

############################
### Problem 5.16 ###########
############################

Full.matrix <- matrix(c(1.00,0.00,0.00,0.00,0.00,0.00,
                        0.00,0.25,0.48,0.27,0.00,0.00,
                        0.00,0.25,0.62,0.13,0.00,0.00,
                        0.00,0.00,0.00,0.00,0.99,0.01,
                        0.18,0.00,0.00,0.00,0.70,0.12,
                        0.00,0.00,0.00,0.00,0.74,0.26)
                      ,6,6,byrow=TRUE)

Tran.matrix <- Full.matrix[2:6,2:6]


# Make identity
Identity <- diag(1,5,5)
# Use solve to find U.
#  [1,1] is expected number of times you'll go to blood from blood
U.matrix <- solve((Identity-Tran.matrix))

U.matrix

ans.1 <- U.matrix[1,1]
ans.1


ans.2 <- U.matrix[1,2]
ans.2

ans.3<-apply(U.matrix,1,sum)[4]
ans.3

############################
### Problem 5.28 ###########
############################

#>> tmat = [0,p,0;q,0,p;0,q,0]

#tmat =
  
# [ 0, p, 0]
# [ q, 0, p]
# [ 0, q, 0]

#>> Id = [1,0,0;0,1,0;0,0,1]

#Id =
  
# 1     0     0
# 0     1     0
# 0     0     1

#>> inv(Id-tmat)

#ans =
  
#  [ (p*q - 1)/(2*p*q - 1), -p/(2*p*q - 1),      -p^2/(2*p*q - 1)]
#  [        -q/(2*p*q - 1), -1/(2*p*q - 1),        -p/(2*p*q - 1)]
#  [      -q^2/(2*p*q - 1), -q/(2*p*q - 1), (p*q - 1)/(2*p*q - 1)]

#a Expected number of services in the game

# 4 * q^4 + 4 * p^4 + 5 * (q * p^4 * 4) + 5 * (p * q^4 * 4) + 6p^2 * q^2 * 
# [4 + (2/(1 - 2 * q * p))] + 4 * p^2 * q^3 * 
# [5 + (1-p * q + p + p^2)/(1 - 2 * p * q)] + 4 * p^3 * q^2 * 
# [5 + (1-p*q + q + q^2)/(1 - 2*p*q)]

#b The probability of A's winning in the game
# P(A wins) = p^4 + 4 * p^4 * q + (4 * p^2 * q^3 *p^3)/(1 - 2 * p * q) +
# (6 * p^2 * q^2 * p^2)/(1 - 2 * p * q) + 
# (4 * q^2 * p^3 * p*(1 - p * q))/(1 - 2 * p * q)
#
#i p = 0
# Ans P(A wins) = 0 
#
#ii p = .25
# Ans P(A wins) = 13/256
#
#iii p = .5
# Ans P(A wins) = 1/2
#
#iv p = .75
# Ans P(A wins) = 243/256
#
#v p = 1
# Ans P(A wins) = 1


############################
### Problem 5.49 ###########
############################

# A. f32 and f42 will both be 1

Full.matrix <- matrix(c(1.00,0.00,0.00,0.00,
                        0.30,0.20,0.40,0.10,
                        0.00,0.40,0.10,0.50,
                        0.00,0.30,0.50,0.20)
                      ,4,4,byrow=TRUE)

Tran.matrix <- Full.matrix[2:4,2:4]


# Make identity
Identity <- diag(1,3,3)
# Use solve to find U.

U.matrix <- solve((Identity-Tran.matrix))

U.matrix

apply(U.matrix,1,sum)

# Now do problem b.

Full.matrix <- matrix(c(1.00,0.00,0.00,
                        0.40,0.10,0.50,
                        0.30,0.50,0.20)
                      ,3,3,byrow=TRUE)

Tran.matrix <- Full.matrix[2:3,2:3]


Identity <- diag(1,2,2)

# Use solve to find U.
U.matrix <- solve((Identity-Tran.matrix))
U.matrix

#Probs from 3 and 4 to 2
apply(U.matrix,1,sum)
