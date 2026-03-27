# Excessive switching in OCD and paranoia arises from different deficits in belief-updating
# Author: Charlotte Freeland, PhD
# Bayesian Gaussian Graphical Modeling of OCD, paranoia & PRL task behavior
# Data examined was collected and published in Suthaharan et. al, 2021


###################
rm(list=ls())

# Install packages
if(!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
if(!require(stringr)) {install.packages("stingr")}; library(stringr)
if(!require(tidyverse)) {install.packages("tidyverse")}; library(tidyverse)
if(!require(corrplot)) {install.packages("corrplot")}; library(corrplot)
if(!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
if(!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
if(!require(sjPlot)) {install.packages("sjPlot")}; library(sjPlot)
if(!require(rstatix)) {install.packages("rstatix")};library(rstatix)
if (!require(scales)) {install.packages("scales")}; library(scales)

# # # # Generate correlated normal variables # # # #
if (!require(MASS)) {install.packages('MASS')}; library(MASS)

# # # # Bayesian Gaussian Graphical Model # # # #
if (!require(BGGM)) {install.packages('BGGM')}; library(BGGM)
if (!require(qgraph)) {install.packages('qgraph')}; library(qgraph)


#### DATA WRANGLING #####
# Import Data 
data1 <- read.csv("pandemicPRL.csv")  # 1010 obs of 173 variables

# Source data wrangling and cleaning script
source("wrangle_data.R")

result <- wrangleData("pandemicPRL.csv")

# Generate clean and uncleaned dataframes
data2 <- result$data2     # filtered by date collected, no outliers removed
data3 <- result$data3     # clean df with 5 outliers removed from 2 variables


################
##### BGGM #####


# FIGURE 4 ----------------------------------------------------------------

# scale each continuous variable
dims <- data3[,c("rgpts_score", "docs_adjusted_score", "wsr_mean", "lsr_mean")]
for (i in 1:ncol(dims)) {
  dims[,i] <- scale(dims[,i])[1:nrow(dims)]
}

# correct labels
colnames(dims) <- c("Paranoia", "OCD", "WSR", "LSR")

# remove nas
dims <- dims[rowSums(is.na(dims))==0,]

# estimate network
beh.net <- estimate(dims, iter = 50000)

# select cool connections
beh.sel.net <- BGGM::select(beh.net, cred = 0.95)

# visualize network
figure4 <- qgraph(beh.sel.net$pcor_adj, labels = c("Paranoia", "OCD", "WSR", "LSR"),
       borders = T,
       layout = "spring",
       # layout = "circular",
       palette = "colorblind")

# BGGM network summary
summary04 <- summary(BGGM::select(explore(dims, mixed_type = rep(1, ncol(dims))), 
                                  alternative = "exhaustive")) 
summary04
# Save summary statistics as text file
summary04_output <- capture.output(summary04)

writeLines(summary04_output, "04-bggm-summary.txt")

############


# FIGURE 6 ----------------------------------------------------------------

# scale each continuous variable
dims <- data3[,c("rgpts_score", "docs_adjusted_score", 
                 "mu02_mean", "mu03_mean", "kappa2_mean",
                 "omega2_mean", "omega3_mean")]
for (i in 1:ncol(dims)) {
  dims[,i] <- scale(dims[,i])[1:nrow(dims)]
}

# correct labels
colnames(dims) <- c("Paranoia", "OCD", "Mu2", "Mu3", "Kappa", "Omega2", "Omega3")

# remove nas
dims <- dims[rowSums(is.na(dims))==0,]

# estimate network
beh.net <- estimate(dims, iter = 50000)

# select cool connections
beh.sel.net <- BGGM::select(beh.net, cred = 0.95)

# visualize network
qgraph(beh.sel.net$pcor_adj, labels = c("Paranoia", "OCD", "Mu2", "Mu3", "Kappa", "Omega2", "Omega3"),
       borders = T,
       layout = "spring",
       # layout = "circular",
       palette = "colorblind")

summary06 <- summary(BGGM::select(explore(dims, mixed_type = rep(1, ncol(dims))), alternative = "exhaustive")) 

summary06
# Save summary statistics as text file
summary06_output <- capture.output(summary06)

writeLines(summary06_output, "06-bggm-summary.txt")

##########

# SUPPLEMENTARY FIGURE 7 --------------------------------------------------

############
# scale each continuous variable
dims <- data3[,c("rgpts_score",  "bai_score", "bdi_score",
                 "mu03_mean", "kappa2_mean",
                 "omega2_mean", "omega3_mean")]
for (i in 1:ncol(dims)) {
  dims[,i] <- scale(dims[,i])[1:nrow(dims)]
}

# correct labels
colnames(dims) <- c("Paranoia", "Anxiety", "Depression",
                    "Mu3", "Kappa", "Omega2", "Omega3")

# remove nas
dims <- dims[rowSums(is.na(dims))==0,]

# estimate network
beh.net <- estimate(dims, iter = 50000)

# select cool connections
beh.sel.net <- BGGM::select(beh.net, cred = 0.95)

# visualize network
qgraph(beh.sel.net$pcor_adj, labels = c("Paranoia", "Anxiety", "Depression",
                                        "Mu3", "Kappa", "Omega2", "Omega3"),
       borders = T,
       layout = "spring",
       # layout = "circular",
       palette = "colorblind")

summaryS7 <- summary(BGGM::select(explore(dims, mixed_type = rep(1, ncol(dims))), alternative = "exhaustive")) 

summaryS7
# Save summary statistics as text file
summaryS7_output <- capture.output(summaryS7)

writeLines(summaryS7_output, "S7-bggm-summary.txt")

############
# scale each continuous variable
dims <- data3[,c("docs_adjusted_score",  "bai_score", "bdi_score",
                 "mu03_mean", "kappa2_mean",
                 "omega2_mean", "omega3_mean")]
for (i in 1:ncol(dims)) {
  dims[,i] <- scale(dims[,i])[1:nrow(dims)]
}

# correct labels
colnames(dims) <- c("OCD", "Anxiety", "Depression",
                     "Mu3", "Kappa", "Omega2", "Omega3")

# remove nas
dims <- dims[rowSums(is.na(dims))==0,]

# estimate network
beh.net <- estimate(dims, iter = 50000)

# select cool connections
beh.sel.net <- BGGM::select(beh.net, cred = 0.95)

# visualize network
qgraph(beh.sel.net$pcor_adj, labels = c("OCD", "Anxiety", "Depression",
                                        "Mu3", "Kappa", "Omega2", "Omega3"),
       borders = T,
       layout = "spring",
       # layout = "circular",
       palette = "colorblind")

summaryS7b <- summary(BGGM::select(explore(dims, mixed_type = rep(1, ncol(dims))), alternative = "exhaustive")) 
summaryS7b
# Save summary statistics as text file
summaryS7b_output <- capture.output(summaryS7b)

writeLines(summaryS7_output, "S7b-bggm-summary.txt")

############
