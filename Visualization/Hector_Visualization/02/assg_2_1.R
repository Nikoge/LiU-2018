####
## Visualization Assignment 02.1
####
library(ggplot2)
library(grid)
library(gridExtra)

# importing data
df_olive = read.table("olive.csv", sep=",", header=TRUE)


## TASK 1.1
# discretized the values to 4 groups
linolenic_4_group = cut_interval(df_olive$linolenic, n=4)

g_normal = ggplot() +
  geom_point(aes(x = df_olive$palmitic, y = df_olive$oleic, color=df_olive$linolenic)) +
  labs(x="Palmitic", y="Oleic", color="Linolenic")+
  theme(legend.position = "bottom",
        legend.text = element_text(size=4),
        legend.title = element_text(size=6),
        legend.key.size = unit(0.8,"line")) 


g_grouped = ggplot() +
  geom_point(aes(x = df_olive$palmitic, y = df_olive$oleic, color=linolenic_4_group)) +
  labs(x="Palmitic", y="Oleic", color="Linolenic Grouped") +
  theme(legend.position = "bottom",
        legend.text = element_text(size=6),
        legend.title = element_text(size=6),
        legend.key.size = unit(0.6,"line"))


grid.arrange(grobs=list(g_normal, g_grouped))

## TASK 1.2
# discretized the values to 4 groups
linolenic_4_group = cut_interval(df_olive$linolenic, n=4)

g_size = ggplot() +
  geom_point(aes(x = df_olive$palmitic, y = df_olive$oleic, size=(linolenic_4_group)), alpha = 0.6, shape='*') +
  labs(x="Palmitic", y="Oleic", size="Linolenic", title="Fig3") +
  theme(legend.position = "bottom",
        legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        legend.key.size = unit(0.8,"line"))


g_angle = ggplot(df_olive, aes(x=palmitic, y=oleic)) +
  geom_point(size=0.5, alpha=0.4) +
  geom_spoke(aes(angle = as.numeric(linolenic_4_group), radius=55), alpha=0.6) +
  labs(x="Palmitic", y="Oleic", color="Linolenic", title="Fig4")

grid.arrange(grobs=list(g_grouped,g_size), ncol=2)


## TASK 1.3
# factorize the numeric values to leveled values
factorized_region = factor(df_olive$Region, levels=c(1,2,3), labels=c("1","2","3"))

g_region = ggplot() +
  geom_point(aes(x = df_olive$oleic, y = df_olive$eicosenoic, color=df_olive$Region)) +
  labs(x="Oleic", y="Eicosenoic", color="Region") +
  theme(legend.position = "bottom",
        legend.text = element_text(size=6),
        legend.title = element_text(size=6),
        legend.key.size = unit(0.6,"line"))

g_region_fact = ggplot() +
  geom_point(aes(x = df_olive$oleic, y = df_olive$eicosenoic, color=factorized_region)) +
  labs(x="Oleic", y="Eicosenoic", color="Region") +
  theme(legend.position = "bottom",
        legend.text = element_text(size=6),
        legend.title = element_text(size=6),
        legend.key.size = unit(0.6,"line"))

grid.arrange(g_region, g_region_fact)

## TASK 1.4
# discretized the values to 3 groups
linoleic_3_group = cut_interval(df_olive$linoleic, n=3)
palmitic_3_group = cut_interval(df_olive$palmitic, n=3)
palmitoleic_3_group = cut_interval(df_olive$palmitoleic, n=3)

g_all = ggplot() +
  geom_point(aes(x = df_olive$oleic, y = df_olive$eicosenoic, 
                 color = linoleic_3_group, 
                 shape = palmitic_3_group,
                 size = palmitoleic_3_group)) + 
  labs(x="Oleic", y="Eicosenoic", color="Linoleic", shape="Palmitic", size="Palmitoleic")

## TASK 1.5
g_all_region = ggplot() +
  geom_point(aes(x = df_olive$oleic, y = df_olive$eicosenoic, 
                 color = df_olive$Region,
                 shape = palmitic_3_group,
                 size = palmitoleic_3_group)) +
  labs(x="Oleic", y="Eicosenoic", color="Region", shape="Palmitic", size="Palmitoleic")
               
## TASK 1.6
library(plotly)
library(dplyr)

pc_areas = df_olive %>%
  group_by(Area) %>%      #group by Area
  summarise(sum = sum(palmitic, palmitoleic, stearic, oleic,linoleic, arachidic, eicosenoic)) %>%      # sum of all columns
  plot_ly(labels = ~Area, values = ~sum, type = 'pie', textinfo="none") %>% #create piechart
  layout(title = 'Proportions of Oils Coming From Different Areas in Italy',
         showlegend = FALSE)

#### or

pie_data = df_olive %>%
  group_by(Area) %>%      #group by Area
  summarise(sum = sum(palmitic, palmitoleic, stearic, oleic,linoleic, arachidic, eicosenoic)) # sum of all cols

pie_chart = plot_ly(labels = pie_data$Area, values = pie_data$sum, type = 'pie', textinfo="none") %>% #create piechart
  layout(title = 'Proportions of Oils Coming From Different Areas in Italy',
         showlegend = FALSE)



## TASK 1.7
density_contour = ggplot(df_olive, aes(x=linoleic, y=eicosenoic)) +
  geom_density_2d() +
  labs(title= "2D-Density Contour Plot", x="Linoleic", y="Eicosenoic")

scatt = ggplot(df_olive, aes(x=linoleic, y=eicosenoic)) +
  geom_point() +
  labs(title="Scatter Plot", x="Linoleic", y="Eicosenoic")

grid.arrange(grobs=list(density_contour, scatt), ncol=2)
