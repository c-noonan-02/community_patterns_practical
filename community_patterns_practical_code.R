# erase environment
rm(list=ls())

getwd()

# import relevant packages
library(tidyverse)
library(vegan)
library(betapart)

# 1. LOAD THE DATABASE AND FORMAT THE DATA

# import data set
data_all <- read.csv("PaciFlora_data/Species_list_full_2905.csv",sep=";")
# remove all NAs
data_all <- data_all[-which(is.na(data_all$species)),]
data_all <- data_all[-which(is.na(data_all$island)),]

# create 6 data frames from data_all, one for each archipelago, and one
# combining these all into one
data_society <- data_all[which(data_all$islandgroup=="Society"),]
data_hawaiian <- data_all[which(data_all$islandgroup=="Hawaiian"),]
data_samoa <- data_all[which(data_all$islandgroup=="Samoa"),]
data_marquesas <- data_all[which(data_all$islandgroup=="Marquesas"),]
data_fiji <- data_all[which(data_all$islandgroup=="Fiji"),]
data_combined <- rbind(data_society,data_hawaiian,data_samoa,data_marquesas,data_fiji)

# check the lengths of each of these data frames- i.e. number of archipelagos
length(unique(data_society$island))
length(unique(data_hawaiian$island))
length(unique(data_samoa$island))
length(unique(data_marquesas$island))
length(unique(data_fiji$island))

# we need site-by-species data frames
# need to create one for each data frame

# create temporary data frame with 3 columns
data_society_new <- data_society[,c("species","island")]
data_society_new$presence <- 1
data_hawaiian_new <- data_hawaiian[,c("species","island")]
data_hawaiian_new$presence <- 1
data_samoa_new <- data_samoa[,c("species","island")]
data_samoa_new$presence <- 1
data_marquesas_new <- data_marquesas[,c("species","island")]
data_marquesas_new$presence <- 1
data_fiji_new <- data_fiji[,c("species","island")]
data_fiji_new$presence <- 1
data_combined_new <- data_combined[,c("species","island")]
data_combined_new$presence <- 1

# use function pivot_wider()
data_society_newer <- data_society_new %>% 
  pivot_wider(names_from=species,values_from=c(presence))
list0 <- as.list(rep(0,ncol(data_society_newer)))
names(list0) <- names(data_society_newer)

data_hawaiian_newer <- data_hawaiian_new %>% 
  pivot_wider(names_from=species,values_from=c(presence))
list0 <- as.list(rep(0,ncol(data_hawaiian_newer)))
names(list0) <- names(data_hawaiian_newer)

data_samoa_newer <- data_samoa_new %>% 
  pivot_wider(names_from=species,values_from=c(presence))
list0 <- as.list(rep(0,ncol(data_samoa_newer)))
names(list0) <- names(data_samoa_newer)

data_marquesas_newer <- data_marquesas_new %>% 
  pivot_wider(names_from=species,values_from=c(presence))
list0 <- as.list(rep(0,ncol(data_marquesas_newer)))
names(list0) <- names(data_marquesas_newer)

data_fiji_newer <- data_fiji_new %>% 
  pivot_wider(names_from=species,values_from=c(presence))
list0 <- as.list(rep(0,ncol(data_fiji_newer)))
names(list0) <- names(data_fiji_newer)

data_combined_newer <- data_combined_new %>% 
  pivot_wider(names_from=species,values_from=c(presence))
list0 <- as.list(rep(0,ncol(data_combined_newer)))
names(list0) <- names(data_combined_newer)

# replace NAs with 0s
data_society_newer <- as.data.frame(data_society_newer %>% replace_na(list0))
data_hawaiian_newer <- as.data.frame(data_hawaiian_newer %>% replace_na(list0))
data_samoa_newer <- as.data.frame(data_samoa_newer %>% replace_na(list0))
data_marquesas_newer <- as.data.frame(data_marquesas_newer %>% replace_na(list0))
data_fiji_newer <- as.data.frame(data_fiji_newer %>% replace_na(list0))
data_combined_newer <- as.data.frame(data_combined_newer %>% replace_na(list0))

# rename rows with island names from first column
row.names(data_society_newer) <- data_society_newer$island
row.names(data_hawaiian_newer) <- data_hawaiian_newer$island
row.names(data_marquesas_newer) <- data_marquesas_newer$island
row.names(data_fiji_newer) <- data_fiji_newer$island
row.names(data_combined_newer) <- data_combined_newer$island

# delete first columns
data_society_newer <- data_society_newer[,-1]
data_hawaiian_newer <- data_hawaiian_newer[,-1]
row.names(data_samoa_newer) <- data_samoa_newer$island
data_samoa_newer <- data_samoa_newer[,-1]
data_marquesas_newer <- data_marquesas_newer[,-1]
data_fiji_newer <- data_fiji_newer[,-1]
data_combined_newer <- data_combined_newer[,-1]

# retrieve the dimensions of the dataframes
# returns the number of rows and columns
dim(data_society_newer)
dim(data_marquesas_newer)
dim(data_hawaiian_newer)
dim(data_fiji_newer)
dim(data_samoa_newer)
dim(data_combined_newer)


# 2. RICHNESS PATTERNS

# compute gamma and alpha diversity for each archipelago from the species-by-site
# data frames

# gamma diversity
ncol(data_society_newer)
ncol(data_hawaiian_newer)
ncol(data_samoa_newer)
ncol(data_marquesas_newer)
ncol(data_fiji_newer)
ncol(data_combined_newer)

# alpha diversity
mean(rowSums(data_society_newer))
mean(rowSums(data_hawaiian_newer))
mean(rowSums(data_samoa_newer))
mean(rowSums(data_marquesas_newer))
mean(rowSums(data_fiji_newer))
mean(rowSums(data_combined_newer))

# which is the most invaded archipelago? Does it correspond with your hypothesis
# Hawaiian contains most invasive species

# Hawaiian archipelago has larger islands
# Hawaiian archipelago is furthest from mainlands - more isolated. Likely means
# niches are more likely to be left open if the native species goes extinct. 


# 3. SPECIES ACCUMULATION CURVES

# use the specaccum function in the vegan package to plot the SACs
SAC_society <- specaccum(data_society_newer)
SAC_hawaiian <- specaccum(data_hawaiian_newer)
SAC_samoa <- specaccum(data_samoa_newer)
SAC_marquesas <- specaccum(data_marquesas_newer)
SAC_fiji <- specaccum(data_fiji_newer)
SAC_combined <- specaccum(data_combined_newer)

Estim_society <- poolaccum(data_society_newer)
Estim_hawaiian <- poolaccum(data_hawaiian_newer)
Estim_samoa <- poolaccum(data_samoa_newer)
Estim_marquesas <- poolaccum(data_marquesas_newer)
Estim_fiji <- poolaccum(data_fiji_newer)
Estim_combined <- poolaccum(data_combined_newer)

par(mfrow=c(2,3))
plot(SAC_society$richness,pch=1,lty=1,lwd=2,type="b",col="blue",ylim=c(0,max(rowMeans(Estim_society$chao))),ylab="Richness",main="Society")
points(3:nrow(data_society_newer),rowMeans(Estim_society$chao),pch=2,lty=2,lwd=2,type="b",col="lightblue")
plot(SAC_hawaiian$richness,pch=1,lty=1,lwd=2,type="b",col="blue",ylim=c(0,max(rowMeans(Estim_hawaiian$chao))),ylab="Richness",main="Hawai'i")
points(3:nrow(data_hawaiian_newer),rowMeans(Estim_hawaiian$chao),pch=2,lty=2,lwd=2,type="b",col="lightblue")
plot(SAC_samoa$richness,pch=1,lty=1,lwd=2,type="b",col="blue",ylim=c(0,max(rowMeans(Estim_samoa$chao))),ylab="Richness",main="Samoa")
points(3:nrow(data_samoa_newer),rowMeans(Estim_samoa$chao),pch=2,lty=2,lwd=2,type="b",col="lightblue")
plot(SAC_marquesas$richness,pch=1,lty=1,lwd=2,type="b",col="blue",ylim=c(0,max(rowMeans(Estim_marquesas$chao))),ylab="Richness",main="Marquesas")
points(3:nrow(data_marquesas_newer),rowMeans(Estim_marquesas$chao),pch=2,lty=2,lwd=2,type="b",col="lightblue")
plot(SAC_fiji$richness,pch=1,lty=1,lwd=2,type="b",col="blue",ylim=c(0,max(rowMeans(Estim_fiji$chao))),ylab="Richness",main="Fiji")
points(3:nrow(data_fiji_newer),rowMeans(Estim_fiji$chao),pch=2,lty=2,lwd=2,type="b",col="lightblue")
plot(SAC_combined$richness,pch=1,lty=1,lwd=2,type="b",col="blue",ylim=c(0,max(rowMeans(Estim_combined$chao))),ylab="Richness",main="All data")
points(3:nrow(data_combined_newer),rowMeans(Estim_combined$chao),pch=2,lty=2,lwd=2,type="b",col="lightblue")
par(mfrow=c(1,1))
# SACs in blue
# Chao2 in light blue

# are these SACs saturating?
# No - still increasing. But beginning to level off?

# Does not make sense to use Chao2 because we cannot add anymore sites (islands)
# than what exist. 

# calculate the ratios between estimated and observed 
last(rowMeans(Estim_society$chao))/last(SAC_society$richness)
last(rowMeans(Estim_hawaiian$chao))/last(SAC_hawaiian$richness)
last(rowMeans(Estim_samoa$chao))/last(SAC_samoa$richness)
last(rowMeans(Estim_marquesas$chao))/last(SAC_marquesas$richness)
last(rowMeans(Estim_fiji$chao))/last(SAC_fiji$richness)
last(rowMeans(Estim_combined$chao))/last(SAC_combined$richness)
# not yet - estimates are higher than the observed

