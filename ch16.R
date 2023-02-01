# Chapter 16 Design of experiment
getwd()
setwd("./Documents/RUMINO2023/theRbook3e")
dataPath = "./dataSet/"

# data set: splityield for 16.2.1
dat = read.table(paste0(dataPath, "splityield.txt"), header = T)

head(dat)

mod <- aov(yield ~ fertilizer*density*irrigation, data = dat)
summary(mod)

mod2 <- aov(yield ~ fertilizer*density*irrigation +
              Error(block/irrigation/density), data = dat)
summary(mod2)

unique(dat$density)

library(tidyverse)

dat %>%
  group_by(irrigation, density) %>%
  summarise(n = n())
