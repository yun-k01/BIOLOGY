library(vegan)
library(readr)
library(dplyr)
library(ggplot2)
chlorophyll <- read_csv("Desktop/BIOLOGY/ChlorophyllFINAL - Section 004 - ChlorophyllFINAL - Section 004.csv")
zooplankton <- read_csv("Desktop/BIOLOGY/ZooplanktonFINAL - Section 004 - ZooplanktonFINAL - Section 004.csv")
View(chlorophyll)
## LAB 3
# POST SALT
post.zooplankton = subset(zooplankton, sample_type == "Post-salt")
post.zooplankton$speciesrichness = specnumber(post.zooplankton[, 5:13])
post.zooplankton$shannondiversity = diversity(post.zooplankton[, 5:13], index = "shannon")
post.zooplankton$evenness = post.zooplankton$shannondiversity/post.zooplankton$speciesrichness

# linear regression
postmyfit = lm(shannondiversity ~ cl_conc, data = post.zooplankton)
summary(postmyfit) # not statistically significant

# graph postsalt
plot(shannondiversity ~ cl_conc, data = post.zooplankton, xlab = "Chloride Concentration", ylab = "Shannon Diversity", main = "Zooplankton Diversity Post-Salt ")

# PRE SALT
pre.zooplankton = subset(zooplankton, sample_type == "Pre-salt")
pre.zooplankton$speciesrichness = specnumber(pre.zooplankton[, 5:13])
pre.zooplankton$shannondiversity = diversity(pre.zooplankton[, 5:13], index = "shannon")
pre.zooplankton$evenness = pre.zooplankton$shannondiversity/pre.zooplankton$speciesrichness

# linear regression
premyfit = lm(shannondiversity ~ cl_conc, data = pre.zooplankton)
summary(premyfit) # not statistically significant

# graph postsalt
plot(shannondiversity ~ cl_conc, data = pre.zooplankton, xlab = "Chloride Concentration", ylab = "Shannon Diversity", main = "Zooplankton Diversity Pre-Salt")


## LAB 4
extended_data <- read_csv("Downloads/Ecosystem Module Lab Assignment - Extended Experiment Data - Ecosystem Module Lab Assignment - Extended Experiment Data.csv")

# subsetting data to isolate for post-salt samples
postdata = subset(extended_data, sample_type == "Post-salt")
postzooplankton = subset(zooplankton, sample_type == "Post-salt")

# creating a vector of chlorophyll concentration
cl.conc = postzooplankton$cl_conc / postzooplankton$volume

# GRAPH 1 - Chlorophyll
# statistical analyses on chlorophyll from the old data
post.chlorophyll = subset(chlorophyll, sample_type == "Post-salt")
chlmyfit1 = lm(chl_total ~ cl_conc, data = post.chlorophyll)
summary(chlmyfit1) # statistically significant, thus include line of best fit

# graph chlorophyll from old data
plot(chl_total ~ cl_conc, data = post.chlorophyll, ylab = "Total Chlorophyll/L", xlab = "Chloride Concentration (mg Cl/L)", main = "Chlorophyll from Mesocosm Experiment ")
abline(chlmyfit1, col = "red", lwd = 2)

# statistical analyses on extended data
chlmyfit2 = lm(chl_total ~ cl_conc, data = postdata)
summary(chlmyfit2) # not statistically significant

# finding median and sd for error bars
avg_chl = aggregate(postdata$chl_total, list(postdata$cl_conc), mean)
names(avg_chl) = c("cl_conc", "avg_chl_total")
sd_chl = aggregate(postdata$chl_total, list(postdata$cl_conc), sd)
names(sd_chl) = c("cl_conc", "sd_chl_total")
totalChlData <- merge(avg_chl, sd_chl)

# graph chlorophyll from extended data as line graph with error bars
ggplot(totalChlData, aes(x = cl_conc, y = avg_chl_total)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymax = avg_chl_total+sd_chl_total, 
                    ymin = avg_chl_total-sd_chl_total), 
                width = 25, position = position_dodge(1)) +
  labs(x = "Chloride Concentration (mg Cl/L)",
       y = "Total Chlorophyll /L",
       title = "Chlorophyll in Replicated Data")

# GRAPH 2 - Adult Daphnia Abundance
# statistical analyses on daphnia from the old data
daphmyfit1 = lm(daph.conc ~ cl_conc, data = postzooplankton)
summary(daphmyfit1) # statistically significant, thus include line of best fit

# vectors of daphnia concentration
daph.conc = postzooplankton$daphnia / postzooplankton$volume

# graph daphnia from old data
plot(daph.conc ~ cl.conc, data = postzooplankton, ylab = "Adult Daphnia /L", xlab = "Chloride Concentration (mg Cl/L)", main = "Daphnia from Mesocosm Experiment")
abline(daphmyfit1, col = "red", lwd = 2)

# statistical analyses on extended data
daphmyfit2 = lm(daphnia ~ cl_conc, data = postdata)
summary(daphmyfit2) # statistically significant

# finding median and sd for error bars
avg_daph = aggregate(postdata$daphnia, list(postdata$cl_conc), mean)
names(avg_daph) = c("cl_conc", "avgDaph")
sd_daph = aggregate(postdata$daphnia, list(postdata$cl_conc), sd)
names(sd_daph) = c("cl_conc", "sdDaph")
daphData <- merge(avg_daph, sd_daph)

# graph daphnia from extended data as line graph with error bars
ggplot(daphData, aes(x = cl_conc, y = avgDaph)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymax = avgDaph+sdDaph, 
                    ymin = avgDaph-sdDaph), 
                width = 25, position = position_dodge(1)) +
  labs(x = "Chloride Concentration (mg Cl/L)",
       y = "Adult Daphnia /L",
       title = "Daphnia from Replicated Experiment")


# GRAPH 3 - Adult Copepod Abundance
# vectors of copepod concentration
cope.conc = (postzooplankton$cyclopoids + postzooplankton$calanoids) / postzooplankton$volume

# statistical analyses on daphnia from the old data
copemyfit1 = lm(cope.conc ~ cl_conc, data = postzooplankton)
summary(copemyfit1) # statistically significant, thus include line of best fit

# graph copepod from old data
plot(cope.conc ~ cl.conc, data = postzooplankton, ylab = "Copepods /L", xlab = "Chloride Concentration (mg Cl/L)", main = "Copepods from Mesocosm Experiment")
abline(copemyfit1, col = "red", lwd = 2)

# statistical analyses on extended data
copemyfit2 = lm(cyclopoids + calanoids ~ cl_conc, data = postdata)
summary(copemyfit2) # statistically significant

# finding median and sd for error bars
avg_cope = aggregate(postdata$cyclopoids + postdata$calanoids, list(postdata$cl_conc), mean)
names(avg_cope) = c("cl_conc", "avgCope")
sd_cope = aggregate(postdata$cyclopoids + postdata$calanoids, list(postdata$cl_conc), sd)
names(sd_cope) = c("cl_conc", "sdCope")
copeData <- merge(avg_cope, sd_cope)
View(copeData)

# graph copepods from extended data as line graph with error bars
ggplot(copeData, aes(x = cl_conc, y = avgCope)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymax = avgCope+sdCope, 
                    ymin = avgCope-sdCope), 
                width = 25, position = position_dodge(1)) +
  labs(x = "Chloride Concentration (mg Cl/L)",
       y = "Copepods /L",
       title = "Copepods from Replicated Experiment")


# GRAPH 4 - Shannon’s Biodiversity Index
postdata$speciesrichness = specnumber(postdata[, 5:13])
postdata$shannondiversity = diversity(postdata[, 5:13], index = "shannon")
postdata$evenness = postdata$shannondiversity/postdata$speciesrichness

# linear regression
postmyfit2 = lm(shannondiversity ~ cl_conc, data = postdata)
summary(postmyfit2) # not statistically significant

# finding median and sd for error bars
avg_diversity = aggregate(postdata$shannondiversity, list(postdata$cl_conc), mean)
names(avg_diversity) = c("cl_conc", "avgDiv")
sd_diversity = aggregate(postdata$shannondiversity, list(postdata$cl_conc), sd)
names(sd_diversity) = c("cl_conc", "sdDiv")
divData <- merge(avg_diversity, sd_diversity)

# graph shannon diversity from extended data as line graph with error bars
ggplot(divData, aes(x = cl_conc, y = avgDiv)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymax = avgDiv+sdDiv, 
                    ymin = avgDiv-sdDiv), 
                width = 25, position = position_dodge(1)) +
  labs(x = "Chloride Concentration (mg Cl/L)",
       y = "Shannon Diversity",
       title = "Zooplankton Diversity from Replicated Experiment")