# Task 1.1
# Loading the library and the data.
library(plotly)
library(tidyverse)
df = read.csv("aegypti_albopictus.csv")
head(df)

# Transforming the dataframe.
# Getting the year.
# Removing the missing values.
# Getting only data that represents points.
df_2004 = df[df$YEAR == 2004, ]
df_2004 = drop_na(df_2004, "YEAR")
df_2004 = df_2004[df_2004$LOCATION_TYPE == "point", ]

df_2013 = df[df$YEAR == 2013, ]
df_2013 = drop_na(df_2013, "YEAR")
df_2013 = df_2013[df_2013$LOCATION_TYPE == "point", ]

p = plot_mapbox(lat=df_2004$Y,
                lon=df_2004$X,
                mode='scattermapbox')

p