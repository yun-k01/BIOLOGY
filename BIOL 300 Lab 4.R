library(readr)
library(dplyr)
library(ggplot2)
clientsize <- read_csv("Downloads/BIOL300_client_size.csv")
clientdensity <- read_csv("Downloads/BIOL300_client_density.csv")
temp <- read_csv("Downloads/BIOL300_temperature.csv")

# creating subsets of data for cheating clients given the different contexts
size_jolt = filter(clientsize, client_response == "jolt")
density_jolt = filter(clientdensity, client_response == "jolts")
temp_jolt = filter(temp, client_response == "jolt")
size_nojolt = filter(clientsize, client_response == "no_jolt")
density_nojolt = filter(clientdensity, client_response == "no_jolts")
temp_nojolt = filter(temp, client_response == "no_jolt")

# writing a function to calculate cheating
# here x is the cheating cleints and y is the non-cheating clients
cheatingrate = function(x, y) {
  a = select(x, num_clients)
  b = select(y, num_clients)
  cheating = a / (a + b)
  return(as.numeric(unlist(cheating)))
}

# calculating cheating rate for each context
size.cheating = cheatingrate(size_jolt, size_nojolt)
density.cheating = cheatingrate(density_jolt, density_nojolt)
temp.cheating = cheatingrate(temp_jolt, temp_nojolt)

# creating dataframes to include species, wrasse individual, context, and cheating, then making the context a numeric attribute
size.df = tibble(species = size_jolt$species, wrasse_individual = size_jolt$wrasse_individual, client_size = size_jolt$client_size, cheating_rate = size.cheating)
  size.df$client_size_num = unclass(as.factor(size.df$client_size))
density.df = tibble(species = density_jolt$species, wrasse_individual = density_jolt$wrasse_individual, client_density = density_jolt$client_density, 
                    cheating_rate = density.cheating)
  density.df$client_density_num = unclass(as.factor(density.df$client_density))
temp.df = tibble(species = temp_jolt$species, wrasse_individual = temp_jolt$wrasse_individual, temperature = temp_jolt$temperature, cheating_rate = temp.cheating)
  temp.df$temperature_num = unclass(as.factor(temp.df$temperature))
  View(size.df)
# graphing cheating rate given context of size for the different fish species and sizes
avg_size <- aggregate(size.df$cheating_rate, list(size.df$species, size.df$client_size_num), mean)
names(avg_size) <- c("species", "size", "avg_cheatingrate")
sd_size <- aggregate(size.df$cheating_rate, list(size.df$species, size.df$client_size_num), sd)
names(sd_size) <- c("species", "size", "sd_cheatingrate")
avg_size <- merge(avg_size, sd_size)

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)  
plot(avg_cheatingrate ~ size, ylim = range(c(avg_cheatingrate-sd_cheatingrate, avg_cheatingrate+sd_cheatingrate)), ylab = "Cheating Rate", xlab =
        "Size (cm)", main = "Cheating Rate vs Client Size", type = "p", data = avg_size)
with(subset(avg_size, avg_size$species == "L. bicolor"), points(avg_cheatingrate ~ size, pch = 19))
with(subset(avg_size, species == "L. bicolor"), lines(avg_cheatingrate ~ size, lty = 1))
with(subset(avg_size, species == "L. dimidiatus"), lines(avg_cheatingrate ~ size, lty = 2))
with(avg_size, arrows(x1 = size, y1 = avg_cheatingrate-sd_cheatingrate, x0 = size, y0 =avg_cheatingrate+sd_cheatingrate, length=0.05, angle=90, code=3))
legend("topright", inset=c(-0.5,0), legend=c("L. bicolor","L. dimidiatus"), pch=c(16,1), title="Species")

# graphing cheating rate given context of density for the different fish species and sizes
avg_density <- aggregate(density.df$cheating_rate, list(density.df$species, density.df$client_density_num), mean)
names(avg_density) <- c("species", "density", "avg_cheatingrate")
sd_density <- aggregate(density.df$cheating_rate, list(density.df$species, density.df$client_density_num), sd)
names(sd_density) <- c("species", "density", "sd_cheatingrate")
avg_density <- merge(avg_density, sd_density)

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)  
plot(avg_cheatingrate ~ density, ylim = range(c(avg_cheatingrate-sd_cheatingrate, avg_cheatingrate+sd_cheatingrate)), ylab = "Cheating Rate", xlab =
       "Client Density", main = "Cheating Rate vs Client Density", type = "p", data = avg_density)
with(subset(avg_density, avg_density$species == "L. bicolor"), points(avg_cheatingrate ~ density, pch = 19)) # changes symbol for “OJ” group
with(subset(avg_density, species == "L. bicolor"), lines(avg_cheatingrate ~ density, lty = 1)) # adds lines for “OJ” group
with(subset(avg_density, species == "L. dimidiatus"), lines(avg_cheatingrate ~ density, lty = 2)) # adds lines for “VC” group
with(avg_density, arrows(x1 = density, y1 = avg_cheatingrate-sd_cheatingrate, x0 = density, y0 =avg_cheatingrate+sd_cheatingrate, length=0.05, angle=90, code=3))
legend("topright", inset=c(-0.5,0), legend=c("L. bicolor","L. dimidiatus"), pch=c(16,1), title="Species")

# graphing cheating rate given context of water temperature for different fish species and sizes
avg_temp <- aggregate(temp.df$cheating_rate, list(temp.df$species, temp.df$temperature_num), mean)
names(avg_temp) <- c("species", "temperature", "avg_cheatingrate")
sd_temperature <- aggregate(temp.df$cheating_rate, list(temp.df$species, temp.df$temperature_num), sd)
names(sd_temperature) <- c("species", "temperature", "sd_cheatingrate")
avg_temp <- merge(avg_temp, sd_temperature)
View(temp.df)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)  
plot(avg_cheatingrate ~ temperature, ylim = range(c(avg_cheatingrate-sd_cheatingrate, avg_cheatingrate+sd_cheatingrate)), ylab = "Cheating Rate", xlab =
       "Temperature (Celsius)", main = "Cheating Rate vs Water Temperature", type = "p", data = avg_temp)
with(subset(avg_temp, avg_temp$species == "L. bicolor"), points(avg_cheatingrate ~ temperature, pch = 19))
with(subset(avg_temp, species == "L. bicolor"), lines(avg_cheatingrate ~ temperature, lty = 1))
with(subset(avg_temp, species == "L. dimidiatus"), lines(avg_cheatingrate ~ temperature, lty = 2))
with(avg_temp, arrows(x1 = temperature, y1 = avg_cheatingrate-sd_cheatingrate, x0 = temperature, y0 =avg_cheatingrate+sd_cheatingrate, length=0.05, angle=90, 
                      code=3))
legend("topright", inset=c(-0.5,0), legend=c("L. bicolor","L. dimidiatus"), pch=c(16,1), title="Species")

# calculating two-way anova to compare the cheating rates of the different species given the context of size
size.lm <- lm(size.df$cheating_rate ~ size.df$client_size * size.df$species)
summary(aov(size.lm))
  # Assumptions
  hist(residuals(size.lm))
  shapiro.test(residuals(size.lm))
  plot(size.lm,1)
  with(data = size.df, bartlett.test(split(cheating_rate, list(client_size, species))))
  
# calculating two-way anova to compare cheating rates of the different species given the context of density
density.lm <- lm(density.df$cheating_rate ~ density.df$client_density * density.df$species)
summary(aov(density.lm))
  # assumptions
  hist(residuals(density.lm))
  shapiro.test(residuals(density.lm))
  plot(density.lm,1)
  with(data = density.df, bartlett.test(split(cheating_rate, list(client_density, species))))
  
# calculating two-way anova to compare cheating rates of the different species given the context of temperature
temp.lm <- lm(temp.df$cheating_rate ~ temp.df$temperature * temp.df$species)
summary(aov(temp.lm))
  # assumptions
  hist(residuals(temp.lm))
  shapiro.test(residuals(temp.lm))
  plot(temp.lm,1)
  with(data = temp.df, bartlett.test(split(cheating_rate, list(temperature, species))))
  
