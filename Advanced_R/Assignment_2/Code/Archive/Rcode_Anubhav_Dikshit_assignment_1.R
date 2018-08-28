#**********************************************************************************************************************************#
#*                                                        ASSIGNMENT 1                                                            *#
#*STUDENT NAME: ANUBHAV DIKSHIT                                                                                                   *#
#*LIUID: anudi287                                                                                                                 *#
#**********************************************************************************************************************************#

cat("\014") # Clear console
rm(list = ls()) # clear workspace
gc() #Garbage collector
name <- "Anubhav Dikshit"
liuid <- "anudi287"
lab_path <- ("https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml")
set_assignment(lab_path)

#**********************************************************************************************************************************#
#*                                            LOADING THE LIBRARY IF NOT FOUND INSTALL                                            *#
#**********************************************************************************************************************************#

if (!require(markmyassignment)){install.packages(markmyassignment)} 
library(markmyassignment)

#**********************************************************************************************************************************#
#*                                                   ASSIGNMENT 1.1 (vECTORS)                                                     *#
#**********************************************************************************************************************************#

# 1.1.1 my_num_vector
my_num_vector <- function()
  {
  A <- log10(11)
  B <- cos(pi/5)
  C <- exp(pi/3)
  D <- (1173 %% 7)/19
  final <- as.vector(c(A,B,C,D))
return(final)
  } 


# 1.1.2 filter my vector
filter_my_vector <- function(x,leq)
{
    x[x>=leq] <- NA
    return(x)
}

# 1.1.3 dot prod, refered to DK mathematics website
dot_prod <- function(a,b)
{
  c = sum(a*b)
  return(c)
}

# 1.1.4 approx e(N)
approx_e <- function(N)
{
  temp <- sum(1/factorial(0:N))
  return(temp)
}


# test_value <- round(exp(1), 4) # rounding the exponent to 5 decimal point and storing it
#  
#  for(i in 1:100)
#    {
#    temp_in <- round(approx_e(i),4)
#    if(temp_in == test_value) 
#      {
#      return(i) 
#      break
#      }
# }

#**********************************************************************************************************************************#
#*                                                   ASSIGNMENT 1.2 (MATRICES)                                                    *#
#**********************************************************************************************************************************#

# 1.2.1 my magic matrix()
my_magic_matrix <- function()
{
my_matrix = matrix(data = c(4,3,8,9,5,1,2,7,6), nrow = 3, ncol = 3, byrow = FALSE)
return(my_matrix)
}

# 1.2.2 calculate elements(A)
calculate_elements <- function(A)
{
  return(nrow(A) * ncol(A))
}

# 1.2.3 row to zero
row_to_zero <- function(A,i)
{
 A[i,] <- 0
 return(A)
}

# 1.2.4 add elements to matrix
add_elements_to_matrix <- function(A, x, i, j)
{
  A[i,j] <- A[i,j] + x
  return(A)
}

#**********************************************************************************************************************************#
#*                                                   ASSIGNMENT 1.3 (LISTS)                                                       *#
#**********************************************************************************************************************************#

# 1.3.1 my_magic_list()
my_magic_list <- function()
{
temp <- list(info = "my own list", c(1.04139, 0.80902, 2.84965, 0.21053), matrix(data = c(4,3,8,9,5,1,2,7,6), nrow = 3, ncol = 3, 
                                                                                 byrow = FALSE))
return(temp)
}

# 1.3.2 change_info()
change_info <- function(list_name, text_to_replace)
{
  list_name$info <- text_to_replace
  return(list_name)
}

# 1.3.3 add note()
add_note <- function(list_name, new_element)
{
  list_name$note <- new_element
  return(list_name)
}

# 1.3.4 add note()
sum_numeric_parts <- function(list_name)
{
  unlisted_list_name <- unlist(list_name)
  
  return(list_name)
}




#**********************************************************************************************************************************#
#*                                                   RUNNING ALL ASSIGNMENTS                                                      *#
#**********************************************************************************************************************************#

# print answer
my_num_vector()
filter_my_vector(x = c(2, 9, 2, 4, 102), leq = 4)
dot_prod(a = c(3,1,12,2,4), b = c(1,2,3,4,5))
approx_e(7) # N equal to 7 is approximate equal to round(exp(1), 5), refer to line 60 onwards

my_magic_matrix()
mat <- my_magic_matrix()
calculate_elements(cbind(mat,mat))
row_to_zero(mat, i = 2)
add_elements_to_matrix(A = mat, x = -2, i = 1:3, j = 2:3)

my_magic_list()
a_list <- my_magic_list()
change_info(list_name = a_list, text_to_replace = "Some new info")
add_note(list_name = a_list, new_element = "This is a magic list!")


