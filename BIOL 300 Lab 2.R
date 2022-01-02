library(readr)
library(dplyr)
library(ggplot2)
temp <- read_csv("Desktop/BIOLOGY/select_temp.csv")

# creating subsets of data
cool_pop = filter(temp, population == "cool") %>%
  select(select_temp)
warm_pop = filter(temp, population == "warm") %>%
  select(select_temp)

# performing independent t.test
t.test(cool_pop, warm_pop)

# graph
boxplot(temp$select_temp~temp$population)
ggplot(temp, aes(x = population, y = select_temp)) +
  geom_boxplot() +
  geom_point() +
  labs(
    title = "Select Temperature vs Population",
    y = "Select Temperature (Celsius)",
    x = "Population"
  )

