rm(list=ls())

library(dplyr)
library(ggplot2)
library(magrittr)
library(ggpubr)


dat <- PlantGrowth
str(dat)

levels(dat$group)
dat$group <- ordered(dat$group, levels = c( "ctrl","trt1","trt2"))

## Descriptive analysis
group_by(dat, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE),
    median = median(weight, na.rm = TRUE)
  )

## Graph
ggboxplot(dat, x = "group", y = "weight", 
          color = "group",
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")

ggline(dat, x = "group", y = "weight", 
       add = c("mean_se", "jitter"), 
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")


## Anova
mod <- lm(weight~group, data =dat)
dat.aov <- aov(mod)

summary(mod)
summary(dat.aov)
