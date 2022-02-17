library(readr)
library(dplyr)
library(ggplot2)
Q3Data <- read_csv("Downloads/Population Module - Lab Assignment - Extended Experiment Data - Population Module - Lab Assignment - Extended Experiment Data.csv")
View(Q3Data)
Day14 <- read_csv("Downloads/Above-groundGrowthDay14DataFINAL_Section004 - Above-groundGrowthDay14DataFINAL_Section004.csv")
head(Day14)

## LAB 3 - COMPARING THE EFFECT OF SALT CONCENTRATION ON STEM DIAMETER OF DIFFERENT TYPES OF B RAPA 
# making stem diameter numeric
Day14$stemDiameter = as.numeric(Day14$stemDiameter)

# creating a function to calculate the mean stem diameter
average_fxn = function(x){
  mean(x, na.rm = TRUE)
}

# creating a new subset with the average stem diameter
avgDay14 = aggregate(Day14$stemDiameter, list(Day14$variety, Day14$maternal_line, Day14$salt_conc), FUN = average_fxn)
names(avgDay14) = c("variety", "maternal_line", "salt_conc", "stem_diameter")
View(avgDay14)

# growth tolerance
avgDatawide = reshape(data = avgDay14, idvar = c("variety", "maternal_line"), timevar = "salt_conc", direction = "wide")
View(avgDatawide)
filter(avgDatawide, variety == "Standard")

# tolerance
avgDatawide$stemdiameter = (avgDatawide$stem_diameter.90/avgDatawide$stem_diameter.0) * 100

#boxplot
boxplot(stemdiameter ~ variety, data = avgDatawide, ylab = "Salt Tolerance (%)", xlab = "Variety", main = "Variety vs Salt Tolerance (%)")
head(avgDatawide)
View(avgDatawide)

# setting variety as a factor
lmstem = lm(stemdiameter ~ variety, data = avgDatawide)
summary(aov(lmstem))
# as the P-value is <0.05, the results are not statistically significant

# CREATING GRAPHS
# standard
standard_data = subset(avgDatawide, variety == "Standard")
corr_standard = cor.test(standard_data$stem_diameter.0, standard_data$stem_diameter.90)
plot(stem_diameter.90 ~ stem_diameter.0, data = standard_data, main = "Standard B. Rapa", xlab = "Stem Diameter (mm) in 0 mM NaCl", ylab = "Stem Diameter (mm) in 90 mM NaCl", pch = 19)
length(t(standard_data))
# petite
petite_data = subset(avgDatawide, variety == "Petite")
corr_petite = cor.test(petite_data$stem_diameter.0, petite_data$stem_diameter.90)
plot(stem_diameter.90 ~ stem_diameter.0, data = petite_data, main = "Petite B. Rapa", xlab = "Stem Diameter (mm) in 0 mM NaCl", ylab = "Stem Diameter (mm) in 90 mM NaCl", pch = 19)
length(t(petite_data))
# purple hairy
ph_data = subset(avgDatawide, variety == "Purple Hairy")
corr_ph = cor.test(ph_data$stem_diameter.0, ph_data$stem_diameter.90)
plot(stem_diameter.90 ~ stem_diameter.0, data = ph_data, main = "Purple Hairy B. Rapa", xlab = "Stem Diameter (mm) in 0 mM NaCl", ylab = "Stem Diameter (mm) in 90 mM NaCl", pch = 19)
length(t(ph_data))


## LAB 4 - CREATING GRAPHS TO COMPARE THE EFFECT OF SALT CONCENTRATION GIVEN STEM DIAMETER, NOS OF LEAVES, AND NOS OF FLOWERS
# creating a subset of data for stem diameter through removing NA and making salt concentration a factor
stemdiam = Q3Data %>%
  select(variety, salt_conc, day, stemDiameter) %>%
  na.omit()
stemdiam$salt_conc = factor(stemdiam$salt_conc)

# creating a data frame of variety, day, salt concentration, average stem diameter, and standard deviation of stem diameter
avg_stemdiam = aggregate(stemdiam$stemDiameter, list(stemdiam$variety, stemdiam$day, stemdiam$salt_conc), mean)
names(avg_stemdiam) = c("variety", "day", "salt_conc", "avg_stemDiameter")
sd_stemdiam = aggregate(stemdiam$stemDiameter, list(stemdiam$variety, stemdiam$day, stemdiam$salt_conc), sd)
names(sd_stemdiam) = c("variety", "day", "salt_conc", "sd_stemDiameter")
StemDiam <- merge(avg_stemdiam, sd_stemdiam)
View(StemDiam)

# plotting the stem diameter given the day and salt concentration
ggplot(StemDiam, aes(x = day, y = avg_stemDiameter, linetype = salt_conc, color = salt_conc)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin=avg_stemDiameter-sd_stemDiameter, ymax=avg_stemDiameter+sd_stemDiameter), width=2,
                position=position_dodge(0.05)) +
  labs(title = "Day vs Average Stem Diameter for Standard B. Rapa",
       x = "Day",
       y = "Average Stem Diameter (mm)", 
       color = "Salt Concentration (mM NaCl)")

# NUMLEAVES
# creating a subset of data for nos of leaves through removing NA and making salt concentration a factor
leaves = Q3Data %>%
  select(variety, salt_conc, day, numLeaves) %>%
  na.omit()
leaves$salt_conc = factor(leaves$salt_conc)

# creating a data frame of variety, day, salt concentration, and the average nos of leaves
avg_leaves = aggregate(leaves$numLeaves, list(leaves$variety, leaves$day, leaves$salt_conc), mean)
names(avg_leaves) = c("variety", "day", "salt_conc", "avg_numLeaves")
sd_leaves = aggregate(leaves$numLeaves, list(leaves$variety, leaves$day, leaves$salt_conc), sd)
names(sd_leaves) = c("variety", "day", "salt_conc", "sd_numLeaves")
Leaves <- merge(avg_leaves, sd_leaves)

# plotting the stem diameter given the day and salt concentration
ggplot(Leaves, aes(x = day, y = avg_numLeaves, linetype = salt_conc, color = salt_conc)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin=avg_numLeaves-sd_numLeaves, ymax=avg_numLeaves+sd_numLeaves), width=2,
                position=position_dodge(0.05)) +
  labs(title = "Day vs Average Number of Leaves for Standard B. Rapa",
       x = "Day",
       y = "Average Number of Leaves", 
       color = "Salt Concentration (mM NaCl)")

# NUMFLOWERS
# creating a subset of data for nos of flowers through removing NA and making salt concentration a factor
flowers = Q3Data %>%
  select(variety, salt_conc, day, numFlowers) %>%
  na.omit()
flowers$salt_conc = factor(flowers$salt_conc)

# creating a dataframe of variety, day, salt concentration, and the average nos of flowers
avg_flowers = aggregate(flowers$numFlowers, list(flowers$variety, flowers$day, flowers$salt_conc), mean)
names(avg_flowers) = c("variety", "day", "salt_conc", "avg_numFlowers")
sd_flowers = aggregate(flowers$numFlowers, list(flowers$variety, flowers$day, flowers$salt_conc), sd)
names(sd_flowers) = c("variety", "day", "salt_conc", "sd_numFlowers")
Flowers <- merge(avg_flowers, sd_flowers)

# plotting the stem diameter given the day and salt concentration
ggplot(Flowers, aes(x = day, y = avg_numFlowers, linetype = salt_conc, color = salt_conc)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin=avg_numFlowers-sd_numFlowers, ymax=avg_numFlowers+sd_numFlowers), width=2,
                position=position_dodge(0.05)) +
  labs(title = "Day vs Average Number of Flowers for Standard B. Rapa",
       x = "Day",
       y = "Average Number of Flowers", 
       color = "Salt Concentration (mM NaCl)")
