#**********************************************************************************************************************************#
#*                                                        ASSIGNMENT 2                                                            *#
#*STUDENT NAME: ANUBHAV DIKSHIT                                                                                                   *#
#*LIUID: anudi287                                                                                                                 *#
#**********************************************************************************************************************************#

cat("\014") # Clear console
rm(list = ls()) # clear workspace
gc() #Garbage collector
name <- "Anubhav Dikshit"
liuid <- "anudi287"
lab_path <- (path = "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab2.yml")

#**********************************************************************************************************************************#
#*                                            LOADING THE LIBRARY IF NOT FOUND INSTALL                                            *#
#**********************************************************************************************************************************#

if (!require(markmyassignment)){install.packages(markmyassignment)} 
library(markmyassignment)
set_assignment(lab_path)

#**********************************************************************************************************************************#
#*                                                   ASSIGNMENT 1.1 (CONDITIONAL STATEMENTS)                                      *#
#**********************************************************************************************************************************#

# 1.1.1 Sheldon game

sheldon_game <- function(player1, player2)
{
  if(is.numeric(player1) == TRUE & is.numeric(player2) == TRUE)
  {
    stop("error")
  }else{
  if(player1 == player2) 
  {
    result = "Draw!"
  }
  
  else if(player1 == "scissors")
  {
    result = ifelse(player2 == "paper", "Player 1 wins!",
                    ifelse(player2 == "lizard", "Player 1 wins!",
                           ifelse(player2 == "rock", "Player 2 wins!",
                                  ifelse(player2 == "spock", "Player 2 wins!", "Draw!"))))
  }
  else if(player1 == "paper")
  {
    result = ifelse(player2 == "rock", "Player 1 wins!",
                    ifelse(player2 == "spock", "Player 1 wins!",
                           ifelse(player2 == "lizard", "Player 2 wins!",
                                  ifelse(player2 == "scissors", "Player 2 wins!", "Draw!"))))
  }
  else if(player1 == "rock")
  {
    result = ifelse(player2 == "scissors", "Player 1 wins!",
                    ifelse(player2 == "lizard", "Player 1 wins!",
                           ifelse(player2 == "spock", "Player 2 wins!",
                                  ifelse(player2 == "paper", "Player 2 wins!", "Draw!"))))
  }
  else if(player1 == "lizard")
  {
    result = ifelse(player2 == "paper", "Player 1 wins!",
                    ifelse(player2 == "spock", "Player 1 wins!",
                           ifelse(player2 == "scissors", "Player 2 wins!",
                                  ifelse(player2 == "rock", "Player 2 wins!", "Draw!"))))
  }
  else if(player1 == "spock")
  {
    result = ifelse(player2 == "rock", "Player 1 wins!",
                    ifelse(player2 == "scissors", "Player 1 wins!",
                           ifelse(player2 == "lizard", "Player 2 wins!",
                                  ifelse(player2 == "paper", "Player 2 wins!", "Draw!"))))
  }
  else{
    result = "Draw!"    
  }
    }

  return(result)
}

#**********************************************************************************************************************************#
#*                                                   ASSIGNMENT 1.2 (FOR LOOPS)                                                   *#
#**********************************************************************************************************************************#

# 1.2.1 my moving median(), sourced from stackexchnage https://stats.stackexchange.com/questions/3051/mean-of-a-sliding-window-in-r

my_moving_median <- function(n, x, ...)
{
   if(is.numeric(x) == TRUE & is.numeric(n) == TRUE) 
   {
     length_of_vector <- length(x)
     position <- seq(from = 1, to = (length_of_vector-n), by=1)
     temp <- vector(length = length(position))
     for(i in 1:length(position)){
       temp[i] <- median(x[position[i]:(position[i]+n)], ...)
     }
   } else {stop("error")}
  return(temp)
}

# 1.2.2 for mult table

for_mult_table <- function(from, to)
{

  if(is.numeric(from) == TRUE & is.numeric(to) == TRUE) 
  {
    mat_row = seq(from=from, to = to, by = 1)
    mat_col = mat_row
    temp <- matrix(data = 0, nrow=length(mat_row), ncol=length(mat_col))
colnames(temp) <- as.list(mat_row)
rownames(temp) <- as.list(mat_col)  
    for(i in 1:length(mat_row)){
      for(j in 1:length(mat_col)){
        temp[i,j] <- mat_row[i] * mat_col[j] 
            }
      }
  } else {stop("error")}
  return(temp)
}



#**********************************************************************************************************************************#
#*                                                   ASSIGNMENT 1.3 (WHILE LOOPS)                                                   *#
#**********************************************************************************************************************************#

# 1.3.1 find_cumsum

find_cumsum <- function(x, find_sum)
{
  if(is.numeric(x) == TRUE & is.numeric(find_sum) == TRUE) {
    i = 1
    temp = 0
    while(temp < find_sum)
      {
      temp <- temp + x[i]
      i <- i+1
      if(i > length(x)){return(temp)}
    }
  }else{stop("error")
    }
  return(temp)
    }






#**********************************************************************************************************************************#
#*                                                   RUNNING ALL ASSIGNMENTS                                                      *#
#**********************************************************************************************************************************#


sheldon_game("lizard", "spock")
sheldon_game("rock", "rock")
my_moving_median(x = c(5,1,2,2,5,NA,8,9,9), n=2)
for_mult_table(from = 10, to = 12)
find_cumsum(x=1:10, find_sum=500)


mark_my_assignment()