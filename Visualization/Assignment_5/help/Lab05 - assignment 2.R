#-------- Uploading and cleansing data

library(plotly)
library(crosstalk)
library(GGally)
library(htmltools)
olive <- read.csv("olive.csv")
FUN <- function(x){
if(x==1){
  'North'}else if(x==2){
    'South'} else{
      'ardinia island'}
}
olive <- olive %>% select(2:11) %>% mutate('Region' = lapply(X = olive$Region,FUN = FUN))
sh_olive <- SharedData$new(olive)

#-------- 2.1 An interactive scatter plot of the eicosenoic against linoleic

sh_olive %>% plot_ly() %>% add_markers(x = ~eicosenoic
,y = ~linoleic, color = I("palegreen2"), hoverinfo = 'text',
 text = ~paste0('eicosenoic: ',eicosenoic)) %>% highlight(on = 'plotly_selected'
,dynamic = TRUE,color = c("red3","deepskyblue","gold","hotpink")
,opacityDim = I(1), off = 'plotly_deselect') 

#------- 2.2 Linking the scatterplot to a bar chart and using slider for the filtering
#--- Scatter plot

scatter <- sh_olive %>% plot_ly() %>% add_markers(x = ~eicosenoic
                                                  ,y = ~linoleic, color = I("mediumaquamarine"))
#--- Bar chart

barchart <- sh_olive %>% plot_ly(x = ~Region) %>% add_histogram(
                                                  color = I("mediumaquamarine")) %>% layout(barmode = 'overlay')
#--- 

bscols(widths = c(3,NA), filter_slider(id = "FS",label = "values of stearic"
                         ,sharedData = sh_olive, column = ~stearic)
,subplot(scatter,barchart) %>% highlight(on = 'plotly_select'
                               ,off = 'plotly_deselect',persistent = TRUE
                               ,dynamic = TRUE, color = c("red3","deepskyblue","gold","hotpink")
                               ,opacityDim = I(1)) %>% hide_legend()
      )

#------ 2.3 linked scatter plots

scatter1 <- sh_olive %>% plot_ly() %>% add_markers(x = ~eicosenoic
                                       ,y = ~linoleic, color = I("palegreen2")
                                       ,hoverinfo = 'text'
                                       ,text = ~paste0('eicosenoic: ',eicosenoic,'<br>'
                                                       ,'linoleic: ',linoleic)
                                       )

scatter2 <- sh_olive %>% plot_ly() %>% add_markers(x = ~arachidic
                                       ,y = ~linoleic, color = I("lightskyblue")
                                       ,hoverinfo = 'text'
                                       ,text = ~paste0('arachidic: ',arachidic,'<br>'
                                                       ,'linoleic: ',linoleic)
                                       )

subplot(scatter1,scatter2,shareY = TRUE) %>% highlight(on = 'plotly_select',off = 'plotly_deselect'
                                         ,persistent = TRUE,dynamic = TRUE
                                         ,opacityDim = I(1)
                                         ,color = c("red3","deepskyblue","gold","hotpink") 
                                         )

#------- 2.4
#---Parallel coordinates chart

parcoord <- olive %>% ggparcoord(columns = c(3:10),scale = 'uniminmax')
d_parcood <- plotly_data(ggplotly(parcoord)) %>% group_by(.ID)
sh_parcood <- SharedData$new(d_parcood, key = ~.ID, group = "olive")
parcoord4 <- sh_parcood %>% plot_ly(x = ~variable, y = ~value, color = I("slategray3")) %>% add_lines(
line = list(width = 0.4)) %>% add_markers(marker=list(size=0.3), text=~.ID, hoverinfo="text")

#--- 3D_scatter plot

scatter_d <- olive
scatter_d$.ID <- 1:nrow(olive)
sh_scatter <- SharedData$new(scatter_d, key = ~.ID, group = "olive")
#---- Creating dropboxes
#---X
ButtonsX <- list()
for (i in 3:10){
  ButtonsX[[i-2]]= list(method = "restyle",
                        args = list( "x", list(olive[[i]])),
                        label = colnames(olive)[i])
}
#---Y
ButtonsY <- list()
for (i in 3:10){
  ButtonsY[[i-2]]= list(method = "restyle",
                        args = list( "y", list(olive[[i]])),
                        label = colnames(olive)[i])
}
#---Z
ButtonsZ <- list()
for (i in 3:10){
  ButtonsZ[[i-2]]= list(method = "restyle",
                        args = list( "z", list(olive[[i]])),
                        label = colnames(olive)[i])
}

scatter4 <- sh_scatter %>% plot_ly(x = ~eicosenoic, y= ~linoleic
                                   , z = ~arachidic, color = I("slategray3")) %>%
  add_markers() %>%
  layout( updatemenus = list(
           list(y = 1, buttons = ButtonsX),
           list(y = 0.8, buttons = ButtonsY),
           list(y = 0.6, buttons = ButtonsZ)
         )  )


#--- Bar chart

barchart4 <- sh_scatter %>% plot_ly(x = ~Region
                          , color = I("slategray3")) %>% add_histogram() %>% layout(barmode = 'overlay')


#--- Creating linked plots
bscols(
parcoord4 %>% highlight(on = 'plotly_select'
                        ,persistent = TRUE,dynamic = TRUE
                        ,opacityDim = I(1)
                        ,color = c("red3","deepskyblue","gold","aquamarine3") 
                        ) %>% hide_legend()
,scatter4 %>% highlight(on = 'plotly_click'
                        ,persistent = TRUE,dynamic = TRUE
                        ,opacityDim = I(1)
                        ,color = c("red3","deepskyblue","gold","aquamarine3") 
                        ) %>% hide_legend()
,barchart4 %>% highlight(on = 'plotly_click'
                         ,persistent = TRUE,dynamic = TRUE
                         ,opacityDim = I(1)
                         ,color = c("red3","deepskyblue","gold","aquamarine3") 
                        ) %>% hide_legend()

)























