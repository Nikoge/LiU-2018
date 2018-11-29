# To run the iterations, first run Jose's code. Then this code once from top to bottom. Then for each 
# iterators run from line 8 to the end, again and again.
# Create an empty matrix to store all Bernouilli probabilities
m <- matrix(NA, nrow = 1000, ncol = 3)

#Here I create the Bernouilli probabilities, lecture 1b, slide 16. I use 3 loops to do it for the three distributions
# not very efficient, but it works.
for(each in 1:nrow(x)){
  row <- x[each,]
  vec <- c()
  for (i in 1:10) {
    a <- mu[1,i]^row[i]
    b <- a * ((1-mu[1,i])^(1-row[i]))
    vec[i] <- b
    c <- prod(vec)
  }
  m[each, 1] <- c
}

for(each in 1:nrow(x)){
  row <- x[each,]
  vec <- c()
  for (i in 1:10) {
    a <- mu[2,i]^row[i]
    b <- a * ((1-mu[2,i])^(1-row[i]))
    vec[i] <- b
    c <- prod(vec)
  }
  m[each, 2] <- c
}

for(each in 1:nrow(x)){
  row <- x[each,]
  vec <- c()
  for (i in 1:10) {
    a <- mu[3,i]^row[i]
    b <- a * ((1-mu[3,i])^(1-row[i]))
    vec[i] <- b
    c <- prod(vec)
  }
  m[each, 3] <- c
}
#output Bernoulli probabilities
m

# Here I create a empty matrix, to store all values for the numerator of the formula on the bottom of
# slide 9, lecture 1b.
m2 <- matrix(NA, ncol = 3, nrow = 1000)

# m2 stores all the values for the numerator of the formula on the bottom of slide 9, lecture 1b.
for (i in 1:1000){
  a <- pi * m[i,]
  m2[i,] <- a
}
m2

# Sum m2 to get the denominator of the formula on the bottom of slide 9, lecture 1b.
m2_sum <- rowSums(m2)
m_final <- m2 / m2_sum
# m_final is p(Znk | Xn, u, pi), the result of the formula on the bottom of slide 9, lecture 1b
m_final


# Create the numerator for pi, slide 9, lecture 1b.
numerator_pi <- colSums(m_final)

# Create new values for pi, stored in the vector pi_new
pi_new <- numerator_pi / N
pi_new

# Here I try to create the numerator for the formula of mu, on slide 9, lecture 1b, I run three
# seperate loops for each distribution. Again, not very efficient, but easier to overview.
mnew <- matrix(NA, nrow = 1000, ncol = 10)
mnew2 <- matrix(NA, nrow = 1000, ncol = 10)
mnew3 <- matrix(NA, nrow = 1000, ncol = 10)

for (i in 1:1000){
row <- x[i,] * m_final[i,1]
mnew[i,] <- row
row <- row/numerator_pi[1]
}

for (i in 1:1000){
  row <- x[i,] * m_final[i,2]
  mnew2[i,] <- row
  row <- row/numerator_pi[2]
}

for (i in 1:1000){
  row <- x[i,] * m_final[i,3]
  mnew3[i,] <- row
  row <- row/numerator_pi[3]
}

# All the nominators have to be divided by the denominators, this is the same as the numerator for pi.
# Therefore divide by the respective elements of numerator_pi
mnew <- colSums(mnew)/numerator_pi[1]
mnew2 <- colSums(mnew2)/numerator_pi[2]
mnew3 <- colSums(mnew3)/numerator_pi[3]

# Rowbind these vectors to get a mu_new matrix, of 3x10 dimensions.
mu_new <- rbind(mnew, mnew2, mnew3)
mu_new

# Now, to create the iterations, I have to run the code again and again, and specifying mu as new the new values
# created for mu. Same goes for the other variables.
mu <- mu_new
pi <- pi_new

