library(readr)
library(dplyr)
library(readr)
library(ggplot2)
hg <- read_csv("Desktop/hornworm growth.csv")
# View(hg)

# removing NA from the dataset and columns for Day1 and Day4
hg = hg %>% 
  select(-c(Day1, Day4)) %>%
  na.omit()

# creating a new vector of the difference in mass from Day 0 and Day 5
diffInDays = hg %>% mutate(diff = Day5 - Day0)

# grouping by species
jasmine = filter(diffInDays, Species == "Jasmine")
milkweed = filter(diffInDays, Species == "Milkweed")
tomato = filter(diffInDays, Species == "Tomato")

# creating a boxplot of the difference in mass from day 0 and day 5
boxplot(diffInDays$diff ~ diffInDays$Species, xlab = "Species", ylab = "Difference in Mass (g)", main = "Difference in Hornworm Mass from Day 0 to Day 5")
  




  

