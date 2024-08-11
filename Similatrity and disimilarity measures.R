# calculate Minkowski distance
#from scratch
# Custom function to calculate Minkowski distance
calculateMinkowskiDistance <- function(vect1, vect2, p) {
  # Initializing answer variable as 0
  answer <- as.integer(0)
  # Iterating over the length of the vector
  # Using for-in loop
  for (index in 0 : length(vect1))
  {
    # temp stores the absolute difference raised to power p
    temp = as.integer(abs(vect1[index] - vect2[index]) ^ p)
    # Updating answer variable
    answer = sum(temp, answer)
  }
  # The final answer would be answer raised to
  # power 1 / p
  answer = answer ^ (1 / p)
  # Return the answer
  return(answer)
}
# Initializing a vector
vect1 <- c(1, 3, 5, 7)
# Initializing another vector
vect2 <- c(2, 4, 6, 8)
# Set p equal to 4
p <- as.integer(1)
# Call the function to calculate MinkowskiDistance
distance = calculateMinkowskiDistance(vect1, vect2, p)
# Print the calculated distance
print(paste("The Minkowski distance between vect1 and vect2 having the value of p =",p, "is", distance ))
# Set p equal to 5
p <- as.integer(2)

# Call the function to calculate MinkowskiDistance
distance = calculateMinkowskiDistance(vect1, vect2, p)

# Print the calculated distance
print(paste("The Minkowski distance between vect1 and vect2 having the value of p =",p, "is", distance ))

# Set p equal to 5
p <- as.integer(3)

# Call the function to calculate MinkowskiDistance
distance = calculateMinkowskiDistance(vect1, vect2, p)

# Print the calculated distance
print(paste("The Minkowski distance between vect1 and vect2 having the value of p =",p, "is", distance ))
# Set p equal to 5
p <- as.integer(4)
# Call the function to calculate MinkowskiDistance
distance = calculateMinkowskiDistance(vect1, vect2, p)

# Print the calculated distance
print(paste("The Minkowski distance between vect1 and vect2 having the value of p =",p, "is", distance ))


#cosine similarity from scratch
df <- data.frame(t(data.frame(c1=rnorm(100),
                              c2=rnorm(100),
                              c3=rnorm(100),
                              c4=rnorm(100),
                              c5=rnorm(100),
                              c6=rnorm(100))))
apply_cosine_similarity <- function(df){
  cos.sim <- function(df, ix) 
  {
    A = df[ix[1],]
    B = df[ix[2],]
    return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
  }   
  n <- nrow(df) 
  cmb <- expand.grid(i=1:n, j=1:n) 
  C <- matrix(apply(cmb,1,function(cmb){ cos.sim(df, cmb) }),n,n)
  C
}
apply_cosine_similarity(df)
#cosine similarity using build in function
library(lsa)
## the cosinus measure between two vectors
vec1 = c( 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
vec2 = c( 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0 )

cosine(vec1,vec2)
## the cosine measure for all document vectors of a matrix
vec3 = c( 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0 )
matrix = cbind(vec1,vec2, vec3)
cosine(matrix)
#contingency matrix
table(vec1,vec2)
