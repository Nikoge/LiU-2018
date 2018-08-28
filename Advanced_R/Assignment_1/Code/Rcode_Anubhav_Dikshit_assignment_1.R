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
lab_path <- (path = "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml")

#**********************************************************************************************************************************#
#*                                            LOADING THE LIBRARY IF NOT FOUND INSTALL                                            *#
#**********************************************************************************************************************************#

if (!require(markmyassignment)){install.packages(markmyassignment)} 
library(markmyassignment)
set_assignment(lab_path)

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
change_info <- function(x, text)
{
  x$info <- text
  return(x)
}

# 1.3.3 add note()
add_note <- function(x, note)
{
  x$note <- note
  return(x)
}

# 1.3.4 sum numeric parts()
sum_numeric_parts <- function(x)
{
  unlisted_list_name <- as.numeric(unlist(x, recursive = TRUE, use.names = FALSE))
  unlisted_list_name[is.na(unlisted_list_name)] <- 0
  unlisted_list_name_sum <- sum(unlisted_list_name)
  return(unlisted_list_name_sum)
}

#**********************************************************************************************************************************#
#*                                                   ASSIGNMENT 1.4 (DATA FRAMES)                                                       *#
#**********************************************************************************************************************************#

# 1.4.1 my data.frame()
my_data.frame <- function(x)
{
df <- data.frame(id = c(1,2,3), name = c("John", "Lisa", "Azra"), income = c(7.30, 0.00, 15.21), rich = c(FALSE, FALSE, TRUE))
  return(df)
}

# 1.4.2 sort head()
sort_head <- function(df, var.name, n)
{
  df <- df[order(df[[var.name]], decreasing = TRUE),]
  return(data.frame(head(df,n)))
}

# 1.4.3 add median variable(df, j)
add_median_variable <- function(df, j)
{
  median_of_column <- median(df[[j]])
  df$compared_to_median <- ifelse(df[[j]] > median_of_column, "Greater", ifelse(df[[j]] < median_of_column, "Smaller", "Median"))
  return(data.frame(df))
}

# 1.4.4 add median variable(df, j)
analyze_columns <- function(df, j)
{
#df = iris
#j = 1:2
mean_column = mean(df[[j[[1]]]])
median_column = median(df[[j[[1]]]])
sd_column = sd(df[[j[[1]]]])
first_list = list(c(mean_column, median_column, sd_column))

mean_column_2 = mean(df[[j[[2]]]])
median_column_2 = median(df[[j[[2]]]])
sd_column_2 = sd(df[[j[[2]]]])
second_list = list(c(mean_column_2, median_column_2, sd_column_2))


df2 = df[, c(j[[1]],j[[2]])]
df2 = cor(df2)
correlation_matrix_df = list(df2)

final = c(first_list, 
          second_list, 
          correlation_matrix_df)

names(final) <- c(eval(colnames(df2)[1]), eval(colnames(df2)[2]), "correlation_matrix")
return(final)
}

#**********************************************************************************************************************************#
#*                                                   RUNNING ALL ASSIGNMENTS                                                      *#
#**********************************************************************************************************************************#

# vectors
my_num_vector()
filter_my_vector(x = c(2, 9, 2, 4, 102), leq = 4)
dot_prod(a = c(3,1,12,2,4), b = c(1,2,3,4,5))
approx_e(7) # N equal to 7 is approximate equal to round(exp(1), 5), refer to line 60 onwards

# matrix
my_magic_matrix()
mat <- my_magic_matrix()
calculate_elements(cbind(mat,mat))
row_to_zero(mat, i = 2)
add_elements_to_matrix(A = mat, x = -2, i = 1:3, j = 2:3)

# list
my_magic_list()
a_list <- my_magic_list()
change_info(x = a_list, text = "Some new info")
add_note(list_name = a_list, new_element = "This is a magic list!")
sum_numeric_parts(x = a_list[2])

# dataframes
my_data.frame()
sort_head(df = iris, var.name = "Petal.Length", n = 5)
head(add_median_variable(df = faithful, 1))
tail(add_median_variable(df = faithful, 1))
analyze_columns(df = faithful, 1:2)
analyze_columns(df = iris, c(4,1))


mark_my_assignment()
