library(ggplot2)

# load data
df = read.csv(file="adult.csv", sep=",", header = FALSE)
#change colnames
colnames(df) = c(
  "age",
  "workclass",
  "fnlwgt",
  "education",
  "education-num",
  "marital-status",
  "occupation",
  "relationship",
  "race",
  "sex",
  "capital-gain",
  "capital-loss",
  "hours-per-week",
  "native-country",
  "income-level"
)

## TASK 2.1
# age vs hours-per-week colored by income-level
t_2_1_a = ggplot(df, aes(
  x = `hours-per-week`,
  y = age,
  color = `income-level`
)) +
  geom_point()
# age vs hours-per-week facet by income-level
t_2_1_b = ggplot(df, aes(y=age, x=`hours-per-week`)) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(facets = ~`income-level`)

## TASK 2.2
# bonus: calculate mean
means = df %>%
  group_by(`income-level`) %>%
  summarise(m = mean(age))
# density plot of age grouped by the Income level
t_2_2_a = ggplot(df, aes(x=age, fill=`income-level`)) +
  geom_density(alpha=0.4) +
  geom_vline(data=means, aes(xintercept=m, color=`income-level`),
             linetype="dashed")
# add the mean lines
t_2_2_a = t_2_2_a +
  geom_vline(data=means, aes(xintercept=m, color=`income-level`),
             linetype="dashed")

# bonus: calculate mean
means = df %>%
  group_by(`income-level`, `marital-status`) %>%
  summarise(m = mean(age))
# trellis plot condition on Marital Status , density plot of age grouped by the Income level
t_2_2_b = ggplot(df, aes(x=age, fill=`income-level`)) +
  geom_density(alpha=0.4) +
  facet_wrap(facets = ~`marital-status`)
# add the mean lines
t_2_2_b = t_2_2_b + geom_vline(data=means, aes(xintercept=m, color=`income-level`),
                     linetype="dashed")

## TASK 2.3

# filter out capital loss equal zero
n_df = df[df[,"capital-loss"]!=0,]

# x axis title
axx = list(
  title = 'Ordered Education Num'
)

# y axis title
axy = list(
  title = 'Age'
)

# z axis title
axz = list(
  title = 'Capital Loss'
)
# 3D-scatter plot of Education-num vs Age vs Captial Loss 
t_2_3_a = plot_ly(n_df, x = ~`education-num`, y = ~`age`, z = ~`capital-loss`, size=0.5) %>%
  add_markers() %>%
  layout(scene = list(xaxis = axx,
                      yaxis = axy,
                      zaxis = axz))

cut_n_df = n_df
cut_n_df$age_bins = cut_number(n_df$age, 6)

t_2_3_b = ggplot(cut_n_df, aes(x = `capital-loss`, y = `education-num`)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  facet_wrap(facets = ~age_bins) +
  scale_fill_distiller(palette= "Spectral", direction=1) 


## TASK 2.4
cut_n_df = n_df
cut_n_df$age_cut = cut_number(n_df$age, 4)

Agerange = lattice::equal.count(n_df$age, 4, overlap=0.1)
L<-matrix(unlist(levels(Agerange)), ncol=2, byrow = T)
L1<-data.frame(Lower=L[,1],Upper=L[,2], Interval=factor(1:nrow(L)))
index=c()
Class=c()
for(i in 1:nrow(L)){
  Cl=paste("[", L1$Lower[i], ",", L1$Upper[i], "]", sep="")
  ind=which(cut_n_df$age>=L1$Lower[i] &cut_n_df$age<=L1$Upper[i])
  index=c(index,ind)
  Class=c(Class, rep(Cl, length(ind)))
}
df_shingle = cut_n_df[index,]
df_shingle$Class = as.factor(Class)

t_2_4_a = ggplot(cut_n_df, aes(x = `capital-loss`, y = `education-num`)) +
  geom_point() +
  facet_wrap(facets = ~age_cut)

t_2_4_b = ggplot(df_shingle, aes(x = `capital-loss`, y = `education-num`))+ 
  geom_point()+
  facet_wrap(~Class, labeller = "label_both")
