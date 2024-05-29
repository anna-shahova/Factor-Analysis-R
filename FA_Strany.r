data <- read.csv("C:/Users/VTB/DESKTOP/R/Country-data.csv",header= TRUE , stringsAsFactors= FALSE)
head(data)
str(data)

#Name of the country,
#Death of children under five years of age per 1000 live births,
#Exports of goods and services; Exports of goods and services given as %age of the Total GDP;
#Imports of goods and services, Given as %age of the Total GDP;
#Net income per person;
#The measurement of the annual growth rate of the Total GDP;
#The average number of years a newborn child would live if the current mortality patterns are to remain the same;
#The number of children born to each woman if the current age-fertility rates remain the same.

data %>% mutate(country = as_factor(country)) %>% 
  select(-country) -> data2

summary(data2)

apply(data2, 2, sd)

round(cor(data2), 2)

corrplot(cor(data2), diag = FALSE)

GGally::ggpairs(data2)

pca <- prcomp(data2, scale. = TRUE)
pca

summary(pca)

pca$rotation[,1:5]


factanal(data2, factors = 5)
fan <- factanal(data2, factors = 3, scores = 'regression') 
fan
factanal(data2, factors = 2)

plot_ly(x = fan$scores[,1], y = fan$scores[,2], z = fan$scores[,3], color = data$country,
        type = 'scatter3d', mode = 'markers') %>% 
  layout(scene = list(
    xaxis = list(title = 'Factor 1'),
    yaxis = list(title = 'Factor 2'),
    zaxis = list(title = 'Factor 3')
  ))
