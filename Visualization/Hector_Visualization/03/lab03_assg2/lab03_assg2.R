library(dplyr)
library(ggplot2)
library(plotly)
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
subplot(choropleth_map_1, choropleth_map_2, titleX = TRUE, titleY = TRUE)


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
