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

