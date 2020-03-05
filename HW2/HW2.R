# Homework 2
library(tidyverse)

insurance <- read.csv(file.choose())
 
insurance <- insurance[1:9,] # cleans up table, only the first 9 rows have entries

insurance_long <- insurance %>%
  gather(
    key = "city",
    value = "rate",
    Atlanta:Philadelphia
  )

ggplot(
  data = insurance_long,
  aes(
    x = city,
    y = rate
  )
) + stat_boxplot(
  geom = "errorbar"
) + geom_boxplot(
  fill = "lightblue"
  
) + theme_classic()

model <- aov(
  formula =  rate ~ city,
  data = insurance_long
)

summary.aov(model)

TukeyHSD(model)

bonferroni <- pairwise.t.test(
  insurance_long$rate,
  insurance_long$city,
  p.adjust= "bonferroni"
)

bonferroni

