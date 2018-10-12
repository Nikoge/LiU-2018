# ------------------------------------------------------------------------------
# DATA IMPORT ASG. 2
# ------------------------------------------------------------------------------

# IMPORTANT: sep = ";" was required to read the data correctly
df = read.csv("Oilcoal.csv", sep = ";")

# IMPORTANT: converting factor variables to numeric is required
# head(df[, which(sapply(df, is.factor))])

df$Coal = as.numeric(df$Coal)
df$Oil = as.numeric(df$Oil)
df$Marker.size = as.numeric(df$Marker.size)



# ------------------------------------------------------------------------------
# DATA PREP ASG. 2, TASK 5
# ------------------------------------------------------------------------------

# Idea: for each year, loop through each country and take the Coal value 

old <- function(){
# Initialize empty data frame
df_new = as.data.frame(matrix(nrow = length(unique(df$Year)), ncol = length(unique(df$Country)) + 1))
colnames(df_new) = c("Year", as.character(unique(df$Country)))
rownames(df_new) = 1965:2009

# Loop through df_new and pull the correct Coal value from original df
k = 0
for (i in 1965:2009) {
  k = k + 1
  for (j in as.character(unique(df$Country))) {
    df_new[k, j] = df[which(df$Country == j & df$Year == i), "Coal"]
    df_new[k, "Year"] = i
  }
}
return(df_new)}
## New solution

library("reshape2")

new <- function()
df_new3 <- dcast(df[,c("Country", "Year", "Coal")], formula = Year~Country, value.var = "Coal")

# odering columns like your older soultion
df_new3 <- df_new3[names(df_new)]
# checking if all elements are the same
all.equal(df_new3, df_new)
# the only difference is due to the mean of rows
 return(new)
}
library("microbenchmark")
microbenchmark(old(), new())