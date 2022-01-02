Data <- read.csv("Downloads/BIOL300_Lab3Data...csv")
Q3 <- read.csv("Desktop/BIOLOGY/BIOL300_Lab3Data - Question 3.csv")

# creating a subset of data, first removing rows with NA then limiting to birds with at least 5 visitations
D5 = Data %>%
  group_by(species) %>%
  select(-c(number_conspecifics, number_heterospecifics, heterospecific_birds)) %>%
  na.omit() %>%
  filter(n() >= 5)

# creating subsets of teh data by creating dataframes based on the different species
BJ = filter(D5, species == "Blue Jay")
CG = filter(D5, species == "Common Grackle")
Es = filter(D5, species == "European Starling")
MD = filter(D5, species == "Mourning Dove" | "Mourning dove")
RWB = filter(D5, species == "Red-winged Blackbird")

# creating a formula to find the mean impact of context score
mean.ICS = function(x) {
  alone = x$pecks_alone / x$duration_alone
  not.alone = x$pecks_notalone / x$duration_notalone
  mean(alone - not.alone)
}

# creating a vector of the different imapct of context scores
mean.ICSbirds = c(mean.ICS(BJ), mean.ICS(CG), mean.ICS(ES), mean.ICS(MD), mean.ICS(RWB))

# refining the supplemental question 3 by creating a matrix of the species with >5 occurences and their beak size
Q3sup = Q3 %>%
  filter(species == "Blue Jay" | species == "Common Grackle" | species == "European Starling" | species == "Mourning Dove" |
           species == "Red-winged Blackbird") %>%
  select(species, beak_size)

# creating a dataframe of the supplemental information and the mean impact of context score
graph.df = data.frame(Q3sup, mean.ICSbirds)

# plot impact score vs beak size
ggplot(graph.df, aes(x = beak_size, y = mean.ICSbirds, colour = species)) +
  geom_point() +
  labs(
    title = "Impact Score vs Beak Size",
    x = "Beak Size",
    y = "Mean Impact Score (Pecks/Second)"
  )

# performing a linear regression on the different supplemental information
myfit1 = lm(mean.ICSbirds~Q3sup$beak_size)
summary(myfit1)
myfit2 = lm(mean.ICSbirds~Q3sup$dominance_score)
summary(myfit2)
myfit3 = lm(mean.ICSbirds~Q3sup$weight)
summary(myfit3)
# in performing a linear regression, we make the following assumptions: that teh beak size nad the impact score have a linear 
  # relationship, and teh vectors of both have a normal distribution
# from the linear regression we are able to determine that there is a realtionship between the impact score and the beak size
  
