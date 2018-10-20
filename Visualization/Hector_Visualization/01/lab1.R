# TASK 1
# Importing the data that we are going to use
# and taking a look at it so that we know it 
# was imported properly.
df = read.table("SENIC.txt")
head(df)

# Changing the names of the columns.
df_names = c("ID")

for (i in 1:11){
  df_names = c(df_names, c(paste("X", as.character(i), sep="")))
}

names(df) = df_names
head(df)

# TASK 2
outliers = function(vec){
  # Stop condition for the outliers function.
  if (!(is.vector(vec) & is.numeric(vec))){
    print("ERROR: Either the object is not a vector or is not numeric.")
    stop()
  }
  
  # Getting the length of the vector in
  # order to construct the indices.
  n = length(vec)
  idxs = 1:n # Indices.
  
  
  # Getting the quantiles of the vector
  # to calculate outliers.
  quantiles = quantile(vec)
  q1 = quantiles[[2]]  # First quantile.
  q3 = quantiles[[4]]  # Third quantile.
  
  # Constructing the boolean mask that
  # is going to be used to construct the
  # indices.
  mask = (vec > q3 + 1.5 * (q3 - q1)) | (vec < q1 - 1.5 * (q3 - q1))
  idxs = idxs[mask]
  
  return(idxs)
}



# TASK 3
library(ggplot2)
outliers_idxs = outliers(df[, "X3"])
Y = rep(0, length(outliers_idxs))
X = df[outliers_idxs, "X3"]

g = ggplot() + geom_point(aes(x=X, y=Y), shape=5, size=5) + geom_density(aes(df$X3)) + xlab("X3")
print(g)

# TASK 4
library("grid")
library("gridExtra")

density_outliers = function(vec, name, bw="nrd0")
{
  outliers_idxs = outliers(vec)
  Y = rep(0, length(outliers_idxs))
  X = vec[outliers_idxs]
  
  g = ggplot() +  stat_density(aes(vec), bw=bw) + geom_point(aes(x=X, y=Y), shape=5, size=3) + xlab(name)
  
  return(g)
}

graphs = list()
counter = 1

for (name in df_names[2:length(df_names)]){
  graphs[[counter]] = density_outliers(df[, name], name)
  counter = counter + 1
}

grid.arrange(grobs=graphs, ncol=4)

# TASK 5 : TODO:
# g = ggplot(aes(x = df$X3, y = df$X10, color=df$X6))
g = ggplot() + geom_point(aes(x=df$X3, y=df$X10), shape=1, size=3, color=df$X6)
g
# TASK 6
library("plotly")
ggplotly(g)

# TASK 7
p = plot_ly() %>% 
  add_histogram(x = df[, "X3"]) %>%
  add_trace(x=X, y=Y, mode="markers", type="scatter", marker=list(symbol="diamond", size=10)) %>%
  layout(bargap = 0.05)
p

# TASK 8
library(shiny)

# Creating a vector for the names
# of the variables so it's easier
# for the user to read.
feature_names = c("Length of Stay",
                   "Age",
                   "Infection Risk",
                   "Routine Culturing Ratio",
                   "Routine Chests X-ray Ratio",
                   "Number of Beds",
                   "Medical School Affiliation",
                   "Region",
                   "Average Daily Census",
                   "Number of Nurses",
                   "Avialable Facilities & Services")

checkbox_list = list()

for (i in 1:length(feature_names)){
  checkbox_list[[i]] = checkboxInput(df_names[(i + 1)], feature_names[i], FALSE)
}

# Creating the UI for the shiny app.
ui = fluidPage(
  sliderInput(inputId="ws", label="Choose bandwidth size", value=1, min=0.1, max=1),
  checkbox_list,
  plotOutput("densPlot")
)

# Server side functions.
server = function(input, output) {
  output$densPlot <- renderPlot({
    graphs = list()
    counter = 1
    for (name in df_names[2:length(df_names)]){
      if (input[[name]] == TRUE)
      {
        graphs[[counter]] = density_outliers(df[, name], name, bw=input$ws)
        counter = counter + 1 
      }
    }
    if(length(graphs)>0){
      g = grid.arrange(grobs=graphs)
      g
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

