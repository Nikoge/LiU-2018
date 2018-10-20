library(plotly)

### TASK 2.1

# read data
df_oilcoal = read.csv("Oilcoal.csv", sep=";", dec = ',', header = T)

# country sizes
c_sizes = data.frame(Country = c("Brazil", "China", "France", "Germany", "India", "Japan", "United Kingdom", "US"), 
                     Size = c(8516000, 9597000, 643801, 357386, 3287000, 377972, 242495, 9834000))

# normalize 3-5 the size of countries
c_sizes$Size = round((10-4)*(c_sizes$Size-min(c_sizes$Size))/(max(c_sizes$Size)-min(c_sizes$Size))+4)

# set sizes in dataset
df_oilcoal$Size = sapply(df_oilcoal$Country, FUN=function(x){
  return(c_sizes$Size[which(c_sizes$Country == x)])
})

p_t_2_1 = df_oilcoal %>%
  plot_ly(
    x = ~Coal,
    y = ~Oil,
    frame = ~Year,
    size = ~Size,
    # marker = list(size = ~Size, opacity = 0.5),
    text = ~Country, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>%
  layout(showlegend = FALSE) %>%
  add_markers(color=~Country)
p_t_2_1

### TASK 2.2
## Crete only similar countries! and find historical facts that affect these countries!

p_t_2_2 = df_oilcoal %>%
  filter(Country == c("Germany", "France")) %>%
  plot_ly(
    x = ~Coal,
    y = ~Oil,
    frame = ~Year,
    # marker = list(size = ~Marker.size, opacity = 0.5),
    text = ~Country,
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers',
    size = ~Size
  ) %>%
  layout(showlegend = FALSE) %>%
  add_markers(color=~Country)
p_t_2_2

### TASK 2.3
# calculate proportion of fuel consumption related to oil 
df_oilcoal$p_oil = df_oilcoal$Oil / (df_oilcoal$Oil + df_oilcoal$Coal) * 100

# creating new df for line
ndf_oil_p = data.frame(Country = df_oilcoal$Country, Year = df_oilcoal$Year, p_oil = df_oilcoal$p_oil)
zeros = data.frame(Country = df_oilcoal$Country, Year = df_oilcoal$Year, p_oil = 0)
ndf_oil_p = rbind(ndf_oil_p, zeros)

p_t_2_3 = ndf_oil_p %>%
  group_by(Country) %>%
  plot_ly(
    x = ~Country,
    y = ~p_oil,
    frame = ~Year,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'rgb(150, 150, 220)', width = 60)
  ) %>%
  layout(title = "Proportion of Fuel Consumption Related to Oil",
         xaxis = list(title = "Country"),
         yaxis = list(title = "Proportion"),
         showlegend = FALSE
  ) 
p_t_2_3

### TASK 2.4
p_t_2_4 = p_t_2_3 %>%
  animation_opts(
    500, easing = "elastic", redraw = F
  )
p_t_2_4


### TASK 2.4
library(tourr)

# create df, Country as column name, Years are row names
years = as.character(unique(df_oilcoal$Year))
m = sapply(levels(df_oilcoal$Country), FUN= function(country){
  tmp_coal = df_oilcoal %>% filter(Country==country) %>% arrange(Year) %>% select(Country, Year, Coal)
  tmp_coal$Coal
})
new_df = data.frame(m)
rownames(new_df) = years

mat = rescale(new_df)
set.seed(123)
tour <- new_tour(mat, grand_tour(), NULL)

steps <- c(0, rep(1/15, 200))
Projs<-lapply(steps, function(step_size){  
  step <- tour(step_size)
  if(is.null(step)) {
    .GlobalEnv$tour<- new_tour(mat, guided_tour(cmass), NULL)
    step <- tour(step_size)
  }
  step
})

# projection of each observation
tour_dat <- function(i) {
  step <- Projs[[i]]
  proj <- center(mat %*% step$proj)
  data.frame(x = proj[,1], y = proj[,2], state = rownames(mat))
}

# projection of each variable's axis
proj_dat <- function(i) {
  step <- Projs[[i]]
  data.frame(
    x = step$proj[,1], y = step$proj[,2], variable = colnames(mat)
  )
}

stepz <- cumsum(steps)

# tidy version of tour data

tour_dats <- lapply(1:length(steps), tour_dat)
tour_datz <- Map(function(x, y) cbind(x, step = y), tour_dats, stepz)
tour_dat <- dplyr::bind_rows(tour_datz)

# tidy version of tour projection data
proj_dats <- lapply(1:length(steps), proj_dat)
proj_datz <- Map(function(x, y) cbind(x, step = y), proj_dats, stepz)
proj_dat <- dplyr::bind_rows(proj_datz)

ax <- list(
  title = "", showticklabels = FALSE,
  zeroline = FALSE, showgrid = FALSE,
  range = c(-1.1, 1.1)
)

# for nicely formatted slider labels
options(digits = 3)
tour_dat <- highlight_key(tour_dat, ~state, group = "A")
tour <- proj_dat %>%
  plot_ly(x = ~x, y = ~y, frame = ~step, color = I("black")) %>%
  add_segments(xend = 0, yend = 0, color = I("gray80")) %>%
  add_text(text = ~variable) %>%
  add_markers(data = tour_dat, text = ~state, ids = ~state, hoverinfo = "text") %>%
  layout(xaxis = ax, yaxis = ax, showlegend = FALSE)#%>%animation_opts(frame=0, transition=0, redraw = F)
tour

df_tseries = new_df
df_tseries$state<-rownames(new_df)

df_oilcoal %>%
  group_by(Country) %>%
  plot_ly(x = ~Year, y = ~Coal, color=~Country, type = 'scatter', mode = 'lines')




