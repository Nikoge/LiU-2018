#**********************************************************************************************************************************#
#*                                                        ASSIGNMENT 1                                                            *#
#*STUDENT NAME: ANUBHAV DIKSHIT                                                                                                   *#
#*LIUID: anudi287                                                                                                                 *#
#**********************************************************************************************************************************#

cat("\014") # Clear console
rm(list = ls()) # clear workspace
gc() #Garbage collector

setwd("C:/Users/anubh/OneDrive/Documents/GitHub/LiU/LiU-2018/Visualization/Assignment_1")

#**********************************************************************************************************************************#
#*                                            LOADING THE LIBRARY IF NOT FOUND INSTALL                                            *#
#**********************************************************************************************************************************#

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, readr, gridExtra, data.table, plotly, shiny)

#**********************************************************************************************************************************#
#*                                                         LOADING THE INPUTS                                                     *#
#**********************************************************************************************************************************#

# Assignment 2.1

data_senic <- read_table2("Input/SENIC.txt", 
                          col_names = FALSE)

# Fixing the column names

setnames(data_senic, old = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"), 
                     new = c("ID", "length_of_stay", "avg_age", "avg_risk", "culturing_ratio", "chest_xray_ratio",
                           "avg_beds", "school_affil", "region", "avg_daily_census", "avg_nurses", "facility_per"))

# Fixing the datatypes

data_senic$school_affil <- factor(data_senic$school_affil, levels = c(1, 2),labels = c("Yes", "No"))
data_senic$region <- factor(data_senic$region, levels = c(1, 2, 3, 4),labels = c("NE", "NC", "S", "W"))

#**********************************************************************************************************************************#
#*                                      CREATING FUNCTION TO CALCULATE THE QUALITLE HIGHLIGHT THE OUTLIERS                        *#
#**********************************************************************************************************************************#

quant_cal <- function(x){
  #x <-  mtcars$hp
  q1 <- as.numeric(quantile(x, probs = c(0.25)))
  q3 <- as.numeric(quantile(x, probs = c(0.75)))
  iqr <- q3-q1
  upper_value <- q3+(1.5*iqr)
  lower_value <- q1-(1.5*iqr)
  outliers_index <- which(x < lower_value | x > upper_value)
  #which(x %in% boxplot(x)$out) # must be equal to above
  return(outliers_index)
}

#**********************************************************************************************************************************#
#*                                                 PLOTTING THE DENSITY PLOTS                                                     *#
#**********************************************************************************************************************************#

# Assignment 2.3

# Adding outlier to base layer

ggplot(data=data_senic, aes(x=avg_risk)) + geom_density() + geom_point(data = data_senic[quant_cal(data_senic$avg_risk), c("avg_risk")], aes(y = 0, x = avg_risk), shape=18, size=2) 

# Doing this for all columns

# subsetting numeric columns
data_senic_numeric <- data_senic[,c("ID", "length_of_stay", "avg_age", "avg_risk", "culturing_ratio", "chest_xray_ratio",
                                     "avg_beds", "avg_daily_census", "avg_nurses", "facility_per")]

# all plots, using a for loop would be easier, will think of writing this under a function
p1 <- ggplot(data=data_senic_numeric, aes(x=length_of_stay)) + geom_density() + geom_point(data = data_senic_numeric[quant_cal(data_senic_numeric$length_of_stay), c("length_of_stay")], aes(y = 0, x = length_of_stay), shape=18, size=2) 
p2 <- ggplot(data=data_senic_numeric, aes(x=avg_age)) + geom_density() + geom_point(data = data_senic_numeric[quant_cal(data_senic_numeric$avg_age), c("avg_age")], aes(y = 0, x = avg_age), shape=18, size=2) 
p3 <- ggplot(data=data_senic_numeric, aes(x=culturing_ratio)) + geom_density() + geom_point(data = data_senic_numeric[quant_cal(data_senic_numeric$culturing_ratio), c("culturing_ratio")], aes(y = 0, x = culturing_ratio), shape=18, size=2) 
p4 <- ggplot(data=data_senic_numeric, aes(x=chest_xray_ratio)) + geom_density() + geom_point(data = data_senic_numeric[quant_cal(data_senic_numeric$chest_xray_ratio), c("chest_xray_ratio")], aes(y = 0, x = chest_xray_ratio), shape=18, size=2) 
p5 <- ggplot(data=data_senic_numeric, aes(x=avg_beds)) + geom_density() + geom_point(data = data_senic_numeric[quant_cal(data_senic_numeric$avg_beds), c("avg_beds")], aes(y = 0, x = avg_beds), shape=18, size=2) 
p6 <- ggplot(data=data_senic_numeric, aes(x=avg_daily_census)) + geom_density() + geom_point(data = data_senic_numeric[quant_cal(data_senic_numeric$avg_daily_census), c("avg_daily_census")], aes(y = 0, x = avg_daily_census), shape=18, size=2) 
p7 <- ggplot(data=data_senic_numeric, aes(x=avg_nurses)) + geom_density() + geom_point(data = data_senic_numeric[quant_cal(data_senic_numeric$avg_nurses), c("avg_nurses")], aes(y = 0, x = avg_nurses), shape=18, size=2) 
p8 <- ggplot(data=data_senic_numeric, aes(x=facility_per)) + geom_density() + geom_point(data = data_senic_numeric[quant_cal(data_senic_numeric$facility_per), c("facility_per")], aes(y = 0, x = facility_per), shape=18, size=2) 
p9 <- ggplot(data=data_senic_numeric, aes(x=avg_risk)) + geom_density() + geom_point(data = data_senic_numeric[quant_cal(data_senic_numeric$avg_risk), c("avg_risk")], aes(y = 0, x = avg_risk), shape=18, size=2) 

# final output
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p9) # facility_per does not contain any outlier

#**********************************************************************************************************************************#
#*                                      PLOT INFECTION RISK VS. NUMBER OF NURSES                                                  *#
#**********************************************************************************************************************************#

plot(p7) # nurse
plot(p9) # risk

ggplot(data = data_senic, aes(x = avg_nurses, y = avg_risk)) + geom_point(aes(color = avg_beds))

#**********************************************************************************************************************************#
#*                                      CONVERTING GGPLOT INTO PLOTLY                                                             *#
#**********************************************************************************************************************************#

# zooming and subseting
ggplotly(p9)

#**********************************************************************************************************************************#
#*                                      PIPLING AND  PLOTLY                                                                       *#
#**********************************************************************************************************************************#

data = data_senic_numeric[quant_cal(data_senic_numeric$avg_risk), c("avg_risk")]

data_senic %>% select(avg_risk) %>% plot_ly(x = ~avg_risk, type = "histogram") %>% 
  add_markers(x = data$avg_risk, y = 0, showlegend = FALSE, marker = list(color = "red", symbol = c('diamond')))

#**********************************************************************************************************************************#
#*                                      SHINY APP FOR PLOTS                                                                        *#
#**********************************************************************************************************************************#
# NEED TO COMPLETE FROM HERE

# all plots, using a for loop would be easier, will think of writing this under a function
p1 <- ggplot(data=data_senic_numeric, aes(x=length_of_stay)) + geom_density() + geom_point(data = data_senic_numeric[quant_cal(data_senic_numeric$length_of_stay), c("length_of_stay")], aes(y = 0, x = length_of_stay), shape=18, size=2) 
p2 <- ggplot(data=data_senic_numeric, aes(x=avg_age)) + geom_density() + geom_point(data = data_senic_numeric[quant_cal(data_senic_numeric$avg_age), c("avg_age")], aes(y = 0, x = avg_age), shape=18, size=2) 
p3 <- ggplot(data=data_senic_numeric, aes(x=culturing_ratio)) + geom_density() + geom_point(data = data_senic_numeric[quant_cal(data_senic_numeric$culturing_ratio), c("culturing_ratio")], aes(y = 0, x = culturing_ratio), shape=18, size=2) 
p4 <- ggplot(data=data_senic_numeric, aes(x=chest_xray_ratio)) + geom_density() + geom_point(data = data_senic_numeric[quant_cal(data_senic_numeric$chest_xray_ratio), c("chest_xray_ratio")], aes(y = 0, x = chest_xray_ratio), shape=18, size=2) 
p5 <- ggplot(data=data_senic_numeric, aes(x=avg_beds)) + geom_density() + geom_point(data = data_senic_numeric[quant_cal(data_senic_numeric$avg_beds), c("avg_beds")], aes(y = 0, x = avg_beds), shape=18, size=2) 
p6 <- ggplot(data=data_senic_numeric, aes(x=avg_daily_census)) + geom_density() + geom_point(data = data_senic_numeric[quant_cal(data_senic_numeric$avg_daily_census), c("avg_daily_census")], aes(y = 0, x = avg_daily_census), shape=18, size=2) 
p7 <- ggplot(data=data_senic_numeric, aes(x=avg_nurses)) + geom_density() + geom_point(data = data_senic_numeric[quant_cal(data_senic_numeric$avg_nurses), c("avg_nurses")], aes(y = 0, x = avg_nurses), shape=18, size=2) 
p8 <- ggplot(data=data_senic_numeric, aes(x=facility_per)) + geom_density() + geom_point(data = data_senic_numeric[quant_cal(data_senic_numeric$facility_per), c("facility_per")], aes(y = 0, x = facility_per), shape=18, size=2) 
p9 <- ggplot(data=data_senic_numeric, aes(x=avg_risk)) + geom_density() + geom_point(data = data_senic_numeric[quant_cal(data_senic_numeric$avg_risk), c("avg_risk")], aes(y = 0, x = avg_risk), shape=18, size=2) 






ui <- fluidPage(
  sliderInput(inputId="ws", label="Choose bandwidth size", value=0.01, min=0.1, max=1),
  checkboxGroupInput("variable", "Variables to plot:",
                     c("Cylinders" = "cyl",
                       "Transmission" = "am",
                       "Gears" = "gear")),,
  plotOutput("densPlot")
)

server <- function(input, output) {
  
  output$densPlot <- renderPlot({
    ggplot(iris, aes(x=Sepal.Length,fill=Species))+
      stat_density(alpha=0.8, bw=input$ws, position="identity")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



library(shiny)
library(ggplot2)
library(data.table)



# Assignment 2.1

data_senic <- read_table2("Input/SENIC.txt", col_names = FALSE)

# Fixing the column names

setnames(data_senic, old = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"), 
         new = c("ID", "length_of_stay", "avg_age", "avg_risk", "culturing_ratio", "chest_xray_ratio",
                 "avg_beds", "school_affil", "region", "avg_daily_census", "avg_nurses", "facility_per"))

# Fixing the datatypes

data_senic$school_affil <- factor(data_senic$school_affil, levels = c(1, 2),labels = c("Yes", "No"))
data_senic$region <- factor(data_senic$region, levels = c(1, 2, 3, 4),labels = c("NE", "NC", "S", "W"))

#**********************************************************************************************************************************#
#*                                      CREATING FUNCTION TO CALCULATE THE QUALITLE HIGHLIGHT THE OUTLIERS                        *#
#**********************************************************************************************************************************#

quant_cal <- function(x){
  #x <-  mtcars$hp
  q1 <- as.numeric(quantile(x, probs = c(0.25)))
  q3 <- as.numeric(quantile(x, probs = c(0.75)))
  iqr <- q3-q1
  upper_value <- q3+(1.5*iqr)
  lower_value <- q1-(1.5*iqr)
  outliers_index <- which(x < lower_value | x > upper_value)
  #which(x %in% boxplot(x)$out) # must be equal to above
  return(outliers_index)
}



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("Density",
                  "Bandwidth:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    checkboxInput("checkbox", label = "Choice A", value = TRUE),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  
  output$distPlot <- renderPlot({
    x    <- data_senic[[]]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
})
