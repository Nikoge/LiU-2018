#**********************************************************************************************************************************#
#*                                                        ASSIGNMENT 3                                                            *#
#*STUDENT NAME: ANUBHAV DIKSHIT                                                                                                   *#
#*LIUID: anudi287                                                                                                                 *#
#**********************************************************************************************************************************#

cat("\014") # Clear console
rm(list = ls()) # clear workspace
gc() #Garbage collector
name <- "Anubhav Dikshit"
liuid <- "anudi287"
lab_path <- (path = "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab3.yml")
setwd("C:/Users/anubh/OneDrive/Documents/GitHub/LiU/LiU-2018/Advanced_R/Assignment_3")

#**********************************************************************************************************************************#
#*                                            LOADING THE LIBRARY IF NOT FOUND INSTALL                                            *#
#**********************************************************************************************************************************#

if (!require(markmyassignment)){install.packages(markmyassignment)} 
library(markmyassignment)
set_assignment(lab_path)

#**********************************************************************************************************************************#
#*                                                   ASSIGNMENT 1.1 (CODE)                                                        *#
#**********************************************************************************************************************************#

# 1.1.1 euclidean()

euclidean <- function(a,b){
if(a == 0 | b == 0 | is.na(a) | is.na(b)){stop("incorrect inputs")}
if(a > b){
  temp <- b
  b <- a
  a <- temp
}
    r <- b%%a
    return(ifelse(r, euclidean(a, r), a))
  }  


# create this function and run the following
usethis::create_package("~/Rassignment3")
devtools::load_all(".")
usethis::use_readme_md()
usethis::use_test()
covr::report()
