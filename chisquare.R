getwd()
setwd('~/Desktop')

library(MASS)
str(survey)
tbl <- table(survey$Smoke, survey$Exer)

chisq.test(tbl)

new_tbl <- cbind(tbl[,"Freq"], tbl[,"None"] + tbl[,"Some"])
colnames(new_tbl) <- c("Freq", "Non_Freq")
new_tbl
chisq.test(new_tbl)



# Chi square goodness of fit test

df <- list(100, 10, 20, 40)
cat <- c("orange", "blue", 'purple', 'red')
dat <- data.frame(df)
colnames(dat) <- cat

chisq.test(dat)

# Chi squre Test of Independence & Homogeneity

normal <- list(20,30,40,50)
peanut <- list(60,70,80,90)
white <- list(100,110,120,130)
dat2 <- rbind(normal, peanut,white)
dat2 <- data.frame(dat2)
colnames(dat2) <- cat

chisq.test(dat2) ## Not working, maybe each cateory is list. Think of each category as a vector

orange <- c(20,60, 100)
blue <- c(30,70,110)
purple <- c(40,80,120)
red <- c(50,90, 130)
dat3 <- data.frame(orange, blue, purple, red)
chisq.test(dat3) ## Working
