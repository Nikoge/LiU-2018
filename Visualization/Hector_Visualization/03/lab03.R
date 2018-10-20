# Task 1.1
library(plotly)
library(tidyverse)
df = read.csv("aegypti_albopictus.csv")

# Transforming the dataframe.
# Getting the year.
# Removing the missing values.
# Getting only data that represents points.
df_2004 = df[df$YEAR == 2004, ]
df_2013 = df[df$YEAR == 2013, ]

# Creating both plots (2004, 2013).
p_2004 = plot_mapbox(lat=df_2004$Y,
                     lon=df_2004$X,
                     mode='scattermapbox',
                     color=df_2004$VECTOR) %>%
  layout(title='Mosquito’s Population in 2004',
         font=list(color='white'),
         plot_bgcolor='#191A1A', 
         paper_bgcolor='#191A1A',
         mapbox=list(style='dark'),
         legend=list(orientation='h',
                     font=list(size=8)),
         margin=list(l=25, r=25,
                     b=25, t=25,
                     pad=2))

p_2013 = plot_mapbox(lat=df_2013$Y,
                     lon=df_2013$X,
                     mode='scattermapbox',
                     color=df_2013$VECTOR) %>%
  layout(title='Mosquito’s Population in 2013',
         font=list(color='white'),
         plot_bgcolor='#191A1A', 
         paper_bgcolor='#191A1A',
         mapbox=list(style='dark'),
         legend=list(orientation='h',
                     font=list(size=8)),
         margin=list(l=25, r=25,
                     b=25, t=25,
                     pad=2))

p_2004
p_2013

# Task 1.2 
# Calculating the number of
# mosquitoes per country.
df$view = 1
df_agg = aggregate(df$view,
                   by=list(COUNTRY_ID=df$COUNTRY_ID, COUNTRY=df$COUNTRY),
                   FUN=sum)

# Defining the plot.
g = list(projection=list(type='equirectangular'))

p = plot_geo() %>%
  add_trace(z=df_agg$x,
            color=df_agg$x,
            colors='Blues',
            text=df_agg$COUNTRY,
            locations=df_agg$COUNTRY_ID) %>%
  layout(title="Number of mosquitoes per country",
         geo=g) %>%
  colorbar(title="Number of Mosquitos")

p

# Task 1.3
# Equidistant plot.
g = list(projection=list(type='equidistant'))

p = plot_geo() %>%
  add_trace(z=log(df_agg$x),
            color=log(df_agg$x),
            colors='Blues',
            text=df_agg$COUNTRY,
            locations=df_agg$COUNTRY_ID) %>%
  layout(title="Log(Number of mosquitoes per country)",
         geo=g) %>%
  colorbar(title="Number of Mosquitos")

p

# Conic equal area plot.
g = list(projection=list(type='conic equal area'))

p = plot_geo() %>%
  add_trace(z=log(df_agg$x),
            color=log(df_agg$x),
            colors='Blues',
            text=df_agg$COUNTRY,
            locations=df_agg$COUNTRY_ID) %>%
  layout(title="Log(Number of mosquitoes per country)",
         geo=g) %>%
  colorbar(title="Number of Mosquitos")

p

# Task 1.4
# Creating a boolean mask to get
# the data points that corresponds
# to Brazil in 2013.
mask = df$COUNTRY == 'Brazil' & df$YEAR == 2013
df_br13 = df[mask, ]

# Creating the intervals for the coordinates.
df_br13$X1 = cut_interval(df_br13$X, 100)
df_br13$Y1 = cut_interval(df_br13$Y, 100)

# Creating a column of ones so
# we can group them and count
# observations among the intervals.
df_br13$count = 1

# Getting the mean of X1, Y1
# and sum of N by intervals.
df_grouped = df_br13 %>%
  group_by(X1, Y1) %>%
  summarise(X1_mean=mean(X),
            Y1_mean=mean(Y),
            nobs=sum(count))


g = list(scope="south america")
p = plot_mapbox(lat=df_grouped$Y1_mean,
                lon=df_grouped$X1_mean,
                mode='scattermapbox',
                text=df_grouped$nobs,
                color=df_grouped$nobs) %>%
  layout(title='Mean Mosquito’s Population in 2013 (Brazil)',
         font=list(color='white'),
         plot_bgcolor='#191A1A', 
         paper_bgcolor='#191A1A',
         mapbox=list(style='dark', mapbox=g),
         legend=list(orientation='h',
                     font=list(size=8)),
         margin=list(l=25, r=25,
                     b=25, t=25,
                     pad=2))

p

library(dplyr)
library(ggplot2)
library(sf)

## TASK 2.1

## Load data
df = read.csv(file="income_households.csv", fileEncoding="ISO-8859-1", sep=",")
## Load map
rds = readRDS("gadm36_SWE_2_sf.rds")

## Create 
grouped_df = df %>%
  group_by(region) %>%
  summarise(Young = mean(X2016[age=="18-29 years"]), Adult = mean(X2016[age=="30-49 years"]), Senior = mean(X2016[age=="50-64 years"]))


## TASK 2.2
violin_plot_age <- df %>%
  filter(age!="18+ years") %>%
  droplevels() %>%
  plot_ly(
    x = ~age,
    y = ~X2016,
    split = ~age,
    type = 'violin',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    )
  ) %>% 
  layout(
    xaxis = list(
      title = "Mean Income in Thousands"
    ),
    yaxis = list(
      title = "Age",
      zeroline = F
    )
  )
ggplotly(violin_plot_age)



## TASK 2.3
library(akima)
attach(grouped_df)
s=interp(Young,Adult,Senior, duplicate = "mean")
detach(grouped_df)
plot_ly(x=~s$x, y=~s$y, z=~s$z, type="surface") %>%
  layout(
    scene = list(
      xaxis = list(
        title = "Young"
      ),
      yaxis = list(
        title = "Adult"
      ),
      zaxis = list(
        title = "Senior"
      )
    )
  )




## TASK 2.4
# Set rownames of grouped_df as like rds NAME_1 countie names to use same indexing
# split all regions
grouped_df = as.data.frame(grouped_df)
splitted_region = strsplit(as.character(grouped_df$region) , " ")
# get all words except first and last
splitted_region = sapply(splitted_region, FUN=function(el){
  paste(el[2:(length(el)-1)], collapse=" ")
})
# fix the Orebro label
splitted_region[which(splitted_region=="Örebro")]="Orebro"
rownames(grouped_df) = splitted_region
# Set Young means to rds
rds$Young=grouped_df[rds$NAME_1, "Young"]
rds$Young[is.na(rds$Young)]=0
# Set Adult means to rds
rds$Adult=grouped_df[rds$NAME_1, "Adult"]
rds$Adult[is.na(rds$Adult)]=0

# young map
choropleth_map_1 = plot_ly() %>%
  add_sf(data=rds, split=~NAME_1, color=~Young, showlegend=F, alpha=1) %>%
  layout(
    title = "Mean of Young"
  )
# adult map
choropleth_map_2 = plot_ly() %>%
  add_sf(data=rds, split=~NAME_1, color=~Adult, showlegend=F, alpha=1)%>%
  layout(
    title = "Mean of Adult"
  )
subplot(choropleth_map_1, choropleth_map_2)


## TASK 2.5
# Add Liu Location
li_lat = 58.409814
li_long = 15.624525
choropleth_map = choropleth_map_1 %>%
  add_markers(
    y = li_lat, x = li_long,
    size = 5, color = rgb(1,0,0), text = "Linköping"
  ) 

choropleth_map
