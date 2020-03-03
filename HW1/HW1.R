library(tidyverse)

##### Problem 1 - Badlands data

badlands <- read.csv(file.choose())

names(badlands) <- c("Lifeform","PreBurn","Year1Post", "Year2Post")

badlands_long <- badlands %>%
  gather(
    key = "burn_stage",
    value = "measure",
    PreBurn:Year2Post
  )

ggplot(
  data = badlands_long,
  aes(
    x = burn_stage,
    y = measure
  )
) + stat_boxplot(
  geom = "errorbar"
) + geom_boxplot(
  fill = "lightblue"
  
) + theme_classic()


t.test(x=badlands$PreBurn, y = badlands$Year1Post, paired= TRUE)

t.test(x=badlands$PreBurn, y = badlands$Year2Post, paired= TRUE)

badlands$Year1Dif <- badlands$PreBurn - badlands$Year1Post

badlands$Year2Dif <- badlands$PreBurn - badlands$Year2Post

t.test(x = badlands$Year1Dif)

t.test(x = badlands$Year2Dif)

##### Problem 2 - Chicken data

chicken <- read.csv(file.choose())

chicken_long <- chicken %>%
  gather(
    key = "group",
    value = "weight",
    Control:High.dose,
    factor_key = TRUE
  )

ggplot(
  data = chicken_long,
  aes(
    x = group,
    y=weight
  )
) +
  stat_boxplot (
    geom = "errorbar"
  ) +
  geom_boxplot(
    fill = "lightblue"
    
  ) + theme_classic()

t.test(y=chicken$Control, x = chicken$Low.dose, paired= FALSE)

t.test(y=chicken$Control, x = chicken$High.dose, paired= FALSE)

t.test(x=chicken$Control, y = chicken$Low.dose, paired= FALSE, 
       alternative = "less")

t.test(x=chicken$Control, y = chicken$High.dose, paired= FALSE, 
       alternative = "less")
