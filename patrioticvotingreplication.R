#####CALL LIBRARIES#####

rm(list=ls())

library(foreign)
library(zeligverse)
library(Matching)
library(MASS)
library(ordinal)
library(Hmisc)
palette(colors())






#####IMPORT DATA#####

setwd("~/Dropbox/Academic Papers/Patriotism Voting/Data")  #Set working directory

ssi <- read.csv("ssivotingreplication.csv", header = TRUE)  #Import SSI data
head(ssi)  #Check first few entries of main data
nrow(ssi) #Check for number of entries in main data

mturk <- read.csv("mturkvotingreplication.csv", header = TRUE)
head(mturk)  #Check first few entries of main data
nrow(mturk) #Check for number of entries in main data





#####CREATE NATIONAL IDENTITY VARIABLE (FOOTNOTE 4 & PG. 12)#####

#SSI
ssi.natcorr <- cor.test(ssi$american, ssi$connected) #Examine corrleation between national identity items
print(ssi.natcorr)

ssi$natid <- ssi$american + ssi$connected #Create addative national identity variable

ssi.natdist <- prop.table(table(ssi$natid)) #Note that the distribution is skewed
print(ssi.natdist)

summary(ssi$natid)

#Create quartile scale based on the table, ensuring that each level comes as close to 25% as possible
# First quartile <= 7 (27%)
# Second quartile = 8 (20%)
# Third quartile = 9 (29%)
# Fourth quartile = 10 (23$)

ssi$natidc <- as.numeric(ifelse(ssi$natid <= 7, 1, ifelse(ssi$natid == 8, 2, ifelse(ssi$natid == 9, 3, ifelse(ssi$natid == 10, 4, ""))))) #Create collapsed variable


#MTURK
mturk.natcorr <- cor.test(mturk$american, mturk$connected) #Examine corrleation between national identity items
print(mturk.natcorr)

mturk$natid <- mturk$american + mturk$connected #Create addative national identity variable

mturk.natdist <- prop.table(table(mturk$natid)) #Note that the distribution is skewed
print(mturk.natdist)

#Create quartile scale based on the table, ensuring that each level comes as close to 25% as pomturkble
# First quartile <= 6 (27%)
# Second quartile = 7 (17%)
# Third quartile = 8 (24%)
# Fourth quartile >= 9 (31%)

mturk$natidc <- as.numeric(ifelse(mturk$natid <= 6, 1, ifelse(mturk$natid == 7, 2, ifelse(mturk$natid == 8, 3, ifelse(mturk$natid >= 9, 4, ""))))) #Create collapsed variable






#####VOTE CHOICE PERCENTAGES (TABLE 3)#####

#SSI

ssi.votetable1 <- table(ssi$treatname, ssi$voter3) # View wins and losses by condition
ssi.votetable1

ssi.votetable2 <- prop.table(table(ssi$treatname, ssi$voter3), 1)*100 #View proportion of wins and losses by condition
ssi.votetable2

ssi.repbenny <- ssi.votetable2[4,3] - ssi.votetable2[2,3] #Get change in proportion of Republican wins
print(ssi.repbenny)

n1 <- sum(ssi.votetable1[2,]) #Get Republican wins in each condition for proportion test
n2 <- sum(ssi.votetable1[4,])
x1 <- ssi.votetable1[2,3]
x2 <- ssi.votetable1[4,3]

ssi.repprop <- prop.test(x = c(x1, x2), n = c(n1, n2)) #Conduct two-proportions Z-Test
print(ssi.repprop)



ssi.dembenny <- ssi.votetable2[3,1] - ssi.votetable2[2,1] #Get change in proportion of Democratic wins
print(ssi.dembenny)

n1 <- sum(ssi.votetable1[2,]) #Get Democratic wins in each condition for proportion test
n2 <- sum(ssi.votetable1[3,])
x1 <- ssi.votetable1[2,1]
x2 <- ssi.votetable1[3,1]

ssi.demprop <- prop.test(x = c(x1, x2), n = c(n1, n2)) #Conduct two-proportions Z-Test
print(ssi.demprop)






#MTURK

mturk.propsub <- drop.levels(subset(mturk, treatname == "Control" | treatname == "DemPat" | treatname == "RepPat"))

mturk.votetable1 <- table(mturk.propsub$treatname, mturk.propsub$voter3) # View wins and losses by condition
mturk.votetable1

mturk.votetable2 <- prop.table(table(mturk.propsub$treatname, mturk.propsub$voter3), 1)*100 #View proportion of wins and losses by condition
mturk.votetable2

mturk.repbenny <- mturk.votetable2[3,3] - mturk.votetable2[1,3] #Get change in proportion of Republican wins
print(mturk.repbenny)

n1 <- sum(mturk.votetable1[1,]) #Get Republican wins in each condition for proportion test
n2 <- sum(mturk.votetable1[3,])
x1 <- mturk.votetable1[1,3]
x2 <- mturk.votetable1[3,3]

mturk.repprop <- prop.test(x = c(x1, x2), n = c(n1, n2)) #Conduct two-proportions Z-Test
print(mturk.repprop)



mturk.dembenny <- mturk.votetable2[2,1] - mturk.votetable2[1,1] #Get change in proportion of Democratic wins
print(mturk.dembenny)

n1 <- sum(mturk.votetable1[1,]) #Get Democratic wins in each condition for proportion test
n2 <- sum(mturk.votetable1[2,])
x1 <- mturk.votetable1[1,1]
x2 <- mturk.votetable1[2,1]

mturk.demprop <- prop.test(x = c(x1, x2), n = c(n1, n2)) #Conduct two-proportions Z-Test
print(mturk.demprop)



#####SSI VOTE CHOICE REGRESSIONS (TABLE 4)#####

ssi$voter <- as.factor(ssi$voter) #Convert variables to factors

test1 <- clm(voter ~ treatname, data = ssi, link = "logit")
summary(test1)
nobs(test1)

test2 <- clm(voter ~ treatname*natidc, data = ssi, link = "logit")
summary(test2)
nobs(test2)

test3 <- clm(voter ~ treatname*pid7, data = ssi, link = "logit")
summary(test3)
nobs(test3)

test4 <- clm(voter ~ treatname*natidc + treatname*pid7, data = ssi, link = "logit")
summary(test4)
nobs(test4)

test5 <- clm(voter ~ treatname*natidc*pid7, data = ssi, link = "logit")
summary(test5)
nobs(test5)







#####SSI VOTE PROBABILITIES (FIGURE 1)#####

ssi$voter3 <- as.factor(ssi$voter3) #Convert variables to factors
ssi$voted3 <- as.factor(ssi$voted3)

ssi.predict <- ssi

ssi.predict$bintreat <- as.numeric(ifelse(ssi.predict$treatment == 1, 0, ifelse(as.numeric(ssi.predict$treatment) > 1, 1, ""))) #Create binary indicator for treatment

ssi.conrep <- subset(ssi.predict, treatname == "Control" | treatname == "RepPat") #Create subsets
ssi.condem <- subset(ssi.predict, treatname == "Control" | treatname == "DemPat")


ssi.rep <- clm(voter3 ~ bintreat*natidc + bintreat*pid7, data = ssi.conrep, link = "logit")
summary(ssi.rep)
nobs(ssi.rep)

datapredict <- expand.grid(bintreat = c(0, 1), natidc = c(1, mean(ssi$natidc, na.rm = TRUE), 4), pid7 = mean(ssi$pid7, na.rm = TRUE)) #Create new data to generate predicted probabilities
predict.rep <- predict(ssi.rep, newdata = datapredict, type = "prob", se.fit = TRUE)
predict.repfit <- predict.rep$fit[,3]
predict.replow <- predict.rep$fit[,3] - 1.96*predict.rep$se.fit[,3]
predict.rephigh <- predict.rep$fit[,3] + 1.96*predict.rep$se.fit[,3]
predict.rep <- data.frame(cbind(predict.repfit, predict.replow, predict.rephigh))
names(predict.rep) <- c("fit", "lower", "upper")
print(predict.rep)



ssi.dem <- clm(voted3 ~ bintreat*natidc + bintreat*pid7, data = ssi.condem, link = "logit")
summary(ssi.dem)
nobs(ssi.dem)

datapredict <- expand.grid(bintreat = c(0, 1), natidc = c(1, mean(ssi$natidc, na.rm = TRUE), 4), pid7 = mean(ssi$pid7, na.rm = TRUE)) #Create new data to generate predicted probabilities
predict.dem <- predict(ssi.dem, newdata = datapredict, type = "prob", se.fit = TRUE)
predict.demfit <- predict.dem$fit[,3]
predict.demlow <- predict.dem$fit[,3] - 1.96*predict.dem$se.fit[,3]
predict.demhigh <- predict.dem$fit[,3] + 1.96*predict.dem$se.fit[,3]
predict.dem <- data.frame(cbind(predict.demfit, predict.demlow, predict.demhigh))
names(predict.dem) <- c("fit", "lower", "upper")
print(predict.dem)








#6.5x4.5
par(family = c("serif"))
par(mfrow = c(1,2))
par(mar = c(3, 0, 3, .6), oma = c(2, 2.75, 0, 0))
par(mgp=c(2.75,1,0))

x <- barplot(predict.rep$fit, main = "Republican Candidate", space = c(0, 0, 1, 0, 1, 0), ylim = c(0, .8), col = c("grey78", "grey48"), cex.main = .9, cex.axis = .8, cex.lab = .8, yaxt = "n")
box()
axis(2, at = c(0, .2, .4, .6, .8), c("", "", "", "", ""))
text(-1.7, .4, "Predicted Probability", srt = 90, cex = .8, xpd = NA)
text(c(-.95, -.95, -.95, -.95, -.95), c(0, .2, .4, .6, .8), c("0.0", "0.2", "0.4", "0.6", "0.8"), srt = 90, cex = .8, xpd = NA)
lines(c(x[1],x[1]), c(c(predict.rep$lower[1], predict.rep$upper[1])))
lines(c(x[1]-.1, x[1]+.1), c(predict.rep$lower[1], predict.rep$lower[1]))
lines(c(x[1]-.1, x[1]+.1), c(predict.rep$upper[1], predict.rep$upper[1]))
lines(c(x[2],x[2]), c(c(predict.rep$lower[2], predict.rep$upper[2])))
lines(c(x[2]-.1, x[2]+.1), c(predict.rep$lower[2], predict.rep$lower[2]))
lines(c(x[2]-.1, x[2]+.1), c(predict.rep$upper[2], predict.rep$upper[2]))
lines(c(x[3],x[3]), c(c(predict.rep$lower[3], predict.rep$upper[3])))
lines(c(x[3]-.1, x[3]+.1), c(predict.rep$lower[3], predict.rep$lower[3]))
lines(c(x[3]-.1, x[3]+.1), c(predict.rep$upper[3], predict.rep$upper[3]))
lines(c(x[4],x[4]), c(c(predict.rep$lower[4], predict.rep$upper[4])))
lines(c(x[4]-.1, x[4]+.1), c(predict.rep$lower[4], predict.rep$lower[4]))
lines(c(x[4]-.1, x[4]+.1), c(predict.rep$upper[4], predict.rep$upper[4]))
lines(c(x[5],x[5]), c(c(predict.rep$lower[5], predict.rep$upper[5])))
lines(c(x[5]-.1, x[5]+.1), c(predict.rep$lower[5], predict.rep$lower[5]))
lines(c(x[5]-.1, x[5]+.1), c(predict.rep$upper[5], predict.rep$upper[5]))
lines(c(x[6],x[6]), c(c(predict.rep$lower[6], predict.rep$upper[6])))
lines(c(x[6]-.1, x[6]+.1), c(predict.rep$lower[6], predict.rep$lower[6]))
lines(c(x[6]-.1, x[6]+.1), c(predict.rep$upper[6], predict.rep$upper[6]))
mtext(side = 1, at = c(1, 4, 7), c("Weak National\nIdentification", "Average National\nIdentification", "Strong National\nIdentification"), xpd = NA, cex = .8, line = 1)

par(mar = c(3, .35, 3, .25))
x <- barplot(predict.dem$fit, main = "Democratic Candidate", space = c(0, 0, 1, 0, 1, 0), ylim = c(0, .8), col = c("grey78", "grey48"), cex.main = .9, cex.axis = .8, cex.lab = .8, yaxt = "n")
box()
axis(2, at = c(0, .2, .4, .6, .8), c("", "", "", "", ""))
lines(c(x[1],x[1]), c(c(predict.dem$lower[1], predict.dem$upper[1])))
lines(c(x[1]-.1, x[1]+.1), c(predict.dem$lower[1], predict.dem$lower[1]))
lines(c(x[1]-.1, x[1]+.1), c(predict.dem$upper[1], predict.dem$upper[1]))
lines(c(x[2],x[2]), c(c(predict.dem$lower[2], predict.dem$upper[2])))
lines(c(x[2]-.1, x[2]+.1), c(predict.dem$lower[2], predict.dem$lower[2]))
lines(c(x[2]-.1, x[2]+.1), c(predict.dem$upper[2], predict.dem$upper[2]))
lines(c(x[3],x[3]), c(c(predict.dem$lower[3], predict.dem$upper[3])))
lines(c(x[3]-.1, x[3]+.1), c(predict.dem$lower[3], predict.dem$lower[3]))
lines(c(x[3]-.1, x[3]+.1), c(predict.dem$upper[3], predict.dem$upper[3]))
lines(c(x[4],x[4]), c(c(predict.dem$lower[4], predict.dem$upper[4])))
lines(c(x[4]-.1, x[4]+.1), c(predict.dem$lower[4], predict.dem$lower[4]))
lines(c(x[4]-.1, x[4]+.1), c(predict.dem$upper[4], predict.dem$upper[4]))
lines(c(x[5],x[5]), c(c(predict.dem$lower[5], predict.dem$upper[5])))
lines(c(x[5]-.1, x[5]+.1), c(predict.dem$lower[5], predict.dem$lower[5]))
lines(c(x[5]-.1, x[5]+.1), c(predict.dem$upper[5], predict.dem$upper[5]))
lines(c(x[6],x[6]), c(c(predict.dem$lower[6], predict.dem$upper[6])))
lines(c(x[6]-.1, x[6]+.1), c(predict.dem$lower[6], predict.dem$lower[6]))
lines(c(x[6]-.1, x[6]+.1), c(predict.dem$upper[6], predict.dem$upper[6]))
mtext(side = 1, at = c(1, 4, 7), c("Weak National\nIdentification", "Average National\nIdentification", "Strong National\nIdentification"), xpd = NA, cex = .8, line = 1)


legend(-4.95, -.18, c("No Patriotic Appeal     ", "Patriotic Appeal"), horiz = TRUE, fill = c("grey78", "grey48"), xpd = NA, cex = .8, bty = "n", x.intersp = .75)




#####SSI VOTE PROBABILITY COMPARISIONS (DISCUSSION OF FIGURE 1)#####


manual.onett <- function(m, sd, n){
  t <- ((m - 0)/(sd/sqrt(n)))
  df <- n - 1
  output <- 2*pt(-abs(t), df)
}



manual.twott <- function(m1, m2, sd1, sd2, n1, n2){
  t <- (((m1 - m2) - 0) /sqrt(((sd1^2)/n1) + ((sd2^2)/n2)))
  df <- n1 + n2 - 2
  output <- 2*pt(-abs(t), df)
}

  

ssi$voter3 <- as.factor(ssi$voter3) #Convert variables to factors
ssi$voted3 <- as.factor(ssi$voted3)

ssi.predict <- ssi

ssi.predict$bintreat <- as.numeric(ifelse(ssi.predict$treatment == 1, 0, ifelse(as.numeric(ssi.predict$treatment) > 1, 1, ""))) #Create binary indicator for treatment

ssi.conrep <- subset(ssi.predict, treatname == "Control" | treatname == "RepPat") #Create subsets
ssi.condem <- subset(ssi.predict, treatname == "Control" | treatname == "DemPat")


ssi.rep <- zelig(voter3 ~ bintreat*natidc + bintreat*pid7, data = ssi.conrep, model = "ologit")
summary(ssi.rep)

control <- setx(ssi.rep, bintreat = 0, natidc = 1, pid7 = mean(ssi$pid7, na.rm = TRUE))
treatment <- setx(ssi.rep, bintreat = 1, natidc = 1, pid7 = mean(ssi$pid7, na.rm = TRUE))
lownat <- sim(ssi.rep, x = control, x1 = treatment)
print(lownat)

test1 <- manual.onett(m = .069, sd = .056 * sqrt(1000), n = 1000)
print(test1)



ssi.rep <- zelig(vote3 ~ bintreat*natidc + bintreat*pid7, data = ssi.conrep, model = "ologit")
summary(ssi.rep)

control <- setx(ssi.rep, bintreat = 0, natidc = mean(ssi$natidc, na.rm = TRUE), pid7 = mean(ssi$pid7, na.rm = TRUE))
treatment <- setx(ssi.rep, bintreat = 1, natidc = mean(ssi$natidc, na.rm = TRUE), pid7 = mean(ssi$pid7, na.rm = TRUE))
mednat <- sim(ssi.rep, x = control, x1 = treatment)
print(mednat)

test2 <- manual.onett(m = .042, sd = .032 * sqrt(1000), n = 1000)
print(test2)



ssi.rep <- zelig(voter3 ~ bintreat*natidc + bintreat*pid7, data = ssi.conrep, model = "ologit")
summary(ssi.rep)

control <- setx(ssi.rep, bintreat = 0, natidc = 4, pid7 = mean(ssi$pid7, na.rm = TRUE))
treatment <- setx(ssi.rep, bintreat = 1, natidc = 4, pid7 = mean(ssi$pid7, na.rm = TRUE))
highnat <- sim(ssi.rep, x = control, x1 = treatment)
print(highnat)

test3 <- manual.onett(m = .162, sd = .063 * sqrt(1000), n = 1000)
print(test3)







ssi.dem <- zelig(voted3 ~ bintreat*natidc + bintreat*pid7, data = ssi.condem, model = "ologit")
summary(ssi.dem)

control <- setx(ssi.dem, bintreat = 0, natidc = 1, pid7 = mean(ssi$pid7, na.rm = TRUE))
treatment <- setx(ssi.dem, bintreat = 1, natidc = 1, pid7 = mean(ssi$pid7, na.rm = TRUE))
lownat <- sim(ssi.dem, x = control, x1 = treatment)
print(lownat)

test1 <- manual.onett(m = .156, sd = .047 * sqrt(1000), n = 1000)
print(test1)


ssi.dem <- zelig(voted3 ~ bintreat*natidc + bintreat*pid7, data = ssi.condem, model = "ologit")
summary(ssi.dem)

control <- setx(ssi.dem, bintreat = 0, natidc = mean(ssi$natidc, na.rm = TRUE), pid7 = mean(ssi$pid7, na.rm = TRUE))
treatment <- setx(ssi.dem, bintreat = 1, natidc = mean(ssi$natidc, na.rm = TRUE), pid7 = mean(ssi$pid7, na.rm = TRUE))
mednat <- sim(ssi.dem, x = control, x1 = treatment)
print(mednat)

test2 <- manual.onett(m = .126, sd = .034 * sqrt(1000), n = 1000)
print(test2)



ssi.dem <- zelig(voted3 ~ bintreat*natidc + bintreat*pid7, data = ssi.condem, model = "ologit")
summary(ssi.dem)

control <- setx(ssi.dem, bintreat = 0, natidc = 4, pid7 = mean(ssi$pid7, na.rm = TRUE))
treatment <- setx(ssi.dem, bintreat = 1, natidc = 4, pid7 = mean(ssi$pid7, na.rm = TRUE))
highnat <- sim(ssi.dem, x = control, x1 = treatment)
print(highnat)

test3 <- manual.onett(m = .096, sd = .059 * sqrt(1000), n = 1000)
print(test3)



ssi.rep <- zelig(voter3 ~ bintreat*natidc + bintreat*pid7, data = ssi.conrep, model = "ologit")
summary(ssi.rep)

control <- setx(ssi.rep, bintreat = 0, natidc = 4, pid7 = mean(ssi$pid7, na.rm = TRUE))
treatment <- setx(ssi.rep, bintreat = 1, natidc = 4, pid7 = mean(ssi$pid7, na.rm = TRUE))
highnat.rep <- sim(ssi.rep, x = control, x1 = treatment)

ssi.dem <- zelig(voted3 ~ bintreat*natidc + bintreat*pid7, data = ssi.condem, model = "ologit")
summary(ssi.dem)

control <- setx(ssi.dem, bintreat = 0, natidc = 4, pid7 = mean(ssi$pid7, na.rm = TRUE))
treatment <- setx(ssi.dem, bintreat = 1, natidc = 4, pid7 = mean(ssi$pid7, na.rm = TRUE))
highnat.dem <- sim(ssi.dem, x = control, x1 = treatment)

print(highnat.rep)
print(highnat.dem)


test1 <- manual.twott(m1 = .162, sd1 = .060 * sqrt(1000), n1 = 1000, m2 = .090, sd2 = .055, n2 = 1000)
print(test1)








# MTURK REPUBLICAN RACIAL PREDICTED PROBABILITIES
#####MTURK VOTE CHOICE REGRESSIONS (TABLE 5)#####

mturk$voter <- as.factor(mturk$voter) #Convert variables to factors
mturk$voted <- as.factor(mturk$voted)
mturk$treatment <- as.factor(mturk$treatment)

#Analyses for Republican Candidate

mturk.rep <- subset(mturk, treatname == "Control" | treatname == "RepPat" | treatname == "RepRace" | treatname ==  "RepBoth")

test1 <- clm(voter ~ treatname, data = mturk.rep, link = "logit")
summary(test1)
nobs(test1)


test2 <- clm(voter ~ treatname*natidc, data = mturk.rep, link = "logit")
summary(test2)
nobs(test2)


test3 <- clm(voter ~ treatname*natidc + treatname*pid7, data = mturk.rep)
summary(test3)
nobs(test3)


test4 <- clm(as.factor(voter) ~ treatname*rrscale, data = mturk.rep)
summary(test4)
nobs(test4)


test5 <- clm(as.factor(voter) ~ treatname*rrscale + treatname*pid7, data = mturk.rep)
summary(test5)
nobs(test5)


test6 <- clm(as.factor(voter) ~ treatname*natidc + treatname*rrscale + treatname*pid7, data = mturk.rep)
summary(test6)
nobs(test6)


#Analyses for Democratic Candidate

mturk.dem <- subset(mturk, treatname == "Control" | treatname == "DemPat" | treatname == "DemRace" | treatname ==  "DemBoth")

test1 <- clm(voted ~ treatname, data = mturk.dem, link = "logit")
summary(test1)
nobs(test1)


test2 <- clm(voted ~ treatname*natidc, data = mturk.dem, link = "logit")
summary(test2)
nobs(test2)


test3 <- clm(voted ~ treatname*natidc + treatname*pid7, data = mturk.dem)
summary(test3)
nobs(test3)


test4 <- clm(voted ~ treatname*rrscale, data = mturk.dem)
summary(test4)
nobs(test4)


test5 <- clm(voted ~ treatname*rrscale + treatname*pid7, data = mturk.dem)
summary(test5)
nobs(test5)


test6 <- clm(voted ~ treatname*natidc + treatname*rrscale + treatname*pid7, data = mturk.dem)
summary(test6)
nobs(test6)


# MTURK REPLICATION PREDICTED PROBABILITIES

#####MTURK REPUBLICAN RACIAL VOTE PROBABILITIES (FIGURE 2)####

mturk$voter3 <- as.factor(mturk$voter3) #Convert variables to factors

mturk.predict <- mturk

test <- clm(voter3 ~ treatname*natidc + treatname*rrscale + treatname*pid7, data = mturk)

mturk.predict$bintreat <- ifelse(mturk.predict$treatment == 1, 0, ifelse(as.integer(mturk.predict$treatment) > 1, 1, "")) #Create binary indicator for treatment
mturk.predict$bintreat <- as.integer(mturk.predict$bintreat)

mturk.reppatdat <- na.omit(subset(mturk.predict, treatname == "Control" | treatname == "RepPat", select = c(bintreat, natidc, voter3, voter, pid7, weightstrim, treatname, rrscale))) #Create subsets
mturk.repracedat <- na.omit(subset(mturk.predict, treatname == "Control" | treatname == "RepRace", select = c(bintreat, natidc, voter3, voter, pid7, weightstrim,treatname,  rrscale)))
mturk.repbothdat <- na.omit(subset(mturk.predict, treatname == "Control" | treatname == "RepBoth", select = c(bintreat, natidc, voter3, voter,pid7, weightstrim, treatname, rrscale)))


mturk.reppat <- zelig(voter3 ~ bintreat*natidc + bintreat*rrscale + bintreat*pid7, data = mturk.reppatdat, model = "ologit")
summary(mturk.reppat)

control <- setx(mturk.reppat, bintreat = 0, natidc = 1, rrscale = 4, pid7 = 4)
treatment <- setx(mturk.reppat, bintreat = 1, natidc = 1, rrscale = 4, pid7 = 4)
lownat.lowrace <- sim(mturk.reppat, x = control, x1 = treatment)
print(lownat.lowrace)
lownat.lowrace <- extract.fd(lownat.lowrace)[3,]
lownat.lowrace

control <- setx(mturk.reppat, bintreat = 0, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 4, pid7 = 4)
treatment <- setx(mturk.reppat, bintreat = 1, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 4, pid7 = 4)
mednat.lowrace <- sim(mturk.reppat, x = control, x1 = treatment)
print(mednat.lowrace)
mednat.lowrace <- extract.fd(mednat.lowrace)[3,]
mednat.lowrace

control <- setx(mturk.reppat, bintreat = 0, natidc = 4, rrscale = 4, pid7 = 4)
treatment <- setx(mturk.reppat, bintreat = 1, natidc = 4, rrscale = 4, pid7 = 4)
highnat.lowrace <- sim(mturk.reppat, x = control, x1 = treatment)
print(highnat.lowrace)
highnat.lowrace <- extract.fd(highnat.lowrace)[3,]
highnat.lowrace


control <- setx(mturk.reppat, bintreat = 0, natidc = 1, rrscale = 12, pid7 = 4)
treatment <- setx(mturk.reppat, bintreat = 1, natidc = 1, rrscale = 12, pid7 = 4)
lownat.medrace <- sim(mturk.reppat, x = control, x1 = treatment)
print(lownat.medrace)
lownat.medrace <- extract.fd(lownat.medrace)[3,]
lownat.medrace

control <- setx(mturk.reppat, bintreat = 0, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 12, pid7 = 4)
treatment <- setx(mturk.reppat, bintreat = 1, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 12, pid7 = 4)
mednat.medrace <- sim(mturk.reppat, x = control, x1 = treatment)
print(mednat.medrace)
mednat.medrace <- extract.fd(mednat.medrace)[3,]
mednat.medrace

control <- setx(mturk.reppat, bintreat = 0, natidc = 4, rrscale = 12, pid7 = 4)
treatment <- setx(mturk.reppat, bintreat = 1, natidc = 4, rrscale = 12, pid7 = 4)
highnat.medrace <- sim(mturk.reppat, x = control, x1 = treatment)
print(highnat.medrace)
highnat.medrace <- extract.fd(highnat.medrace)[3,]
highnat.medrace


control <- setx(mturk.reppat, bintreat = 0, natidc = 1, rrscale = 20, pid7 = 4)
treatment <- setx(mturk.reppat, bintreat = 1, natidc = 1, rrscale = 20, pid7 = 4)
lownat.highrace <- sim(mturk.reppat, x = control, x1 = treatment)
print(lownat.highrace)
lownat.highrace <- extract.fd(lownat.highrace)[3,]
lownat.highrace

control <- setx(mturk.reppat, bintreat = 0, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 20, pid7 = 4)
treatment <- setx(mturk.reppat, bintreat = 1, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 20, pid7 = 4)
mednat.highrace <- sim(mturk.reppat, x = control, x1 = treatment)
print(mednat.highrace)
mednat.highrace <- extract.fd(mednat.highrace)[3,]
mednat.highrace

control <- setx(mturk.reppat, bintreat = 0, natidc = 4, rrscale = 20, pid7 = 4)
treatment <- setx(mturk.reppat, bintreat = 1, natidc = 4, rrscale = 20, pid7 = 4)
highnat.highrace <- sim(mturk.reppat, x = control, x1 = treatment)
print(highnat.highrace)
highnat.highrace <- extract.fd(highnat.highrace)[3,]
highnat.highrace

pat.lowrace <- c(lownat.lowrace$ev, mednat.lowrace$ev, highnat.lowrace$ev)
pat.lowrace.low <- c(lownat.lowrace$lower, mednat.lowrace$lower, highnat.lowrace$lower)
pat.lowrace.high <- c(lownat.lowrace$upper, mednat.lowrace$upper, highnat.lowrace$upper)


pat.medrace <- c(lownat.medrace$ev, mednat.medrace$ev, highnat.medrace$ev)
pat.medrace.low <- c(lownat.medrace$lower, mednat.medrace$lower, highnat.medrace$lower)
pat.medrace.high <- c(lownat.medrace$upper, mednat.medrace$upper, highnat.medrace$upper)


pat.highrace <- c(lownat.highrace$ev, mednat.highrace$ev, highnat.highrace$ev)
pat.highrace.low <- c(lownat.highrace$lower, mednat.highrace$lower, highnat.highrace$lower)
pat.highrace.high <- c(lownat.highrace$upper, mednat.highrace$upper, highnat.highrace$upper)





mturk.reprace <- zelig(voter3 ~ bintreat*natidc + bintreat*rrscale + bintreat*pid7, data = mturk.repracedat, model = "ologit")
summary(mturk.reprace)

control <- setx(mturk.reprace, bintreat = 0, natidc = 1, rrscale = 4, pid7 = 4)
treatment <- setx(mturk.reprace, bintreat = 1, natidc = 1, rrscale = 4, pid7 = 4)
lownat.lowrace <- sim(mturk.reprace, x = control, x1 = treatment)
print(lownat.lowrace)
lownat.lowrace <- extract.fd(lownat.lowrace)[3,]
lownat.lowrace

control <- setx(mturk.reprace, bintreat = 0, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 4, pid7 = 4)
treatment <- setx(mturk.reprace, bintreat = 1, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 4, pid7 = 4)
mednat.lowrace <- sim(mturk.reprace, x = control, x1 = treatment)
print(mednat.lowrace)
mednat.lowrace <- extract.fd(mednat.lowrace)[3,]
mednat.lowrace

control <- setx(mturk.reprace, bintreat = 0, natidc = 4, rrscale = 4, pid7 = 4)
treatment <- setx(mturk.reprace, bintreat = 1, natidc = 4, rrscale = 4, pid7 = 4)
highnat.lowrace <- sim(mturk.reprace, x = control, x1 = treatment)
print(highnat.lowrace)
highnat.lowrace <- extract.fd(highnat.lowrace)[3,]
highnat.lowrace


control <- setx(mturk.reprace, bintreat = 0, natidc = 1, rrscale = 12, pid7 = 4)
treatment <- setx(mturk.reprace, bintreat = 1, natidc = 1, rrscale = 12, pid7 = 4)
lownat.medrace <- sim(mturk.reprace, x = control, x1 = treatment)
print(lownat.medrace)
lownat.medrace <- extract.fd(lownat.medrace)[3,]
lownat.medrace

control <- setx(mturk.reprace, bintreat = 0, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 12, pid7 = 4)
treatment <- setx(mturk.reprace, bintreat = 1, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 12, pid7 = 4)
mednat.medrace <- sim(mturk.reprace, x = control, x1 = treatment)
print(mednat.medrace)
mednat.medrace <- extract.fd(mednat.medrace)[3,]
mednat.medrace

control <- setx(mturk.reprace, bintreat = 0, natidc = 4, rrscale = 12, pid7 = 4)
treatment <- setx(mturk.reprace, bintreat = 1, natidc = 4, rrscale = 12, pid7 = 4)
highnat.medrace <- sim(mturk.reprace, x = control, x1 = treatment)
print(highnat.medrace)
highnat.medrace <- extract.fd(highnat.medrace)[3,]
highnat.medrace


control <- setx(mturk.reprace, bintreat = 0, natidc = 1, rrscale = 20, pid7 = 4)
treatment <- setx(mturk.reprace, bintreat = 1, natidc = 1, rrscale = 20, pid7 = 4)
lownat.highrace <- sim(mturk.reprace, x = control, x1 = treatment)
print(lownat.highrace)
lownat.highrace <- extract.fd(lownat.highrace)[3,]
lownat.highrace

control <- setx(mturk.reprace, bintreat = 0, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 20, pid7 = 4)
treatment <- setx(mturk.reprace, bintreat = 1, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 20, pid7 = 4)
mednat.highrace <- sim(mturk.reprace, x = control, x1 = treatment)
print(mednat.highrace)
mednat.highrace <- extract.fd(mednat.highrace)[3,]
mednat.highrace

control <- setx(mturk.reprace, bintreat = 0, natidc = 4, rrscale = 20, pid7 = 4)
treatment <- setx(mturk.reprace, bintreat = 1, natidc = 4, rrscale = 20, pid7 = 4)
highnat.highrace <- sim(mturk.reprace, x = control, x1 = treatment)
print(highnat.highrace)
highnat.highrace <- extract.fd(highnat.highrace)[3,]
highnat.highrace

race.lowrace <- c(lownat.lowrace$ev, mednat.lowrace$ev, highnat.lowrace$ev)
race.lowrace.low <- c(lownat.lowrace$lower, mednat.lowrace$lower, highnat.lowrace$lower)
race.lowrace.high <- c(lownat.lowrace$upper, mednat.lowrace$upper, highnat.lowrace$upper)

race.medrace <- c(lownat.medrace$ev, mednat.medrace$ev, highnat.medrace$ev)
race.medrace.low <- c(lownat.medrace$lower, mednat.medrace$lower, highnat.medrace$lower)
race.medrace.high <- c(lownat.medrace$upper, mednat.medrace$upper, highnat.medrace$upper)

race.highrace <- c(lownat.highrace$ev, mednat.highrace$ev, highnat.highrace$ev)
race.highrace.low <- c(lownat.highrace$lower, mednat.highrace$lower, highnat.highrace$lower)
race.highrace.high <- c(lownat.highrace$upper, mednat.highrace$upper, highnat.highrace$upper)



mturk.repboth <- zelig(voter3 ~ bintreat*natidc + bintreat*rrscale + bintreat*pid7, data = mturk.repbothdat, model = "ologit")
summary(mturk.repboth)

control <- setx(mturk.repboth, bintreat = 0, natidc = 1, rrscale = 4, pid7 = 4)
treatment <- setx(mturk.repboth, bintreat = 1, natidc = 1, rrscale = 4, pid7 = 4)
lownat.lowrace <- sim(mturk.repboth, x = control, x1 = treatment)
print(lownat.lowrace)
lownat.lowrace <- extract.fd(lownat.lowrace)[3,]
lownat.lowrace

control <- setx(mturk.repboth, bintreat = 0, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 4, pid7 = 4)
treatment <- setx(mturk.repboth, bintreat = 1, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 4, pid7 = 4)
mednat.lowrace <- sim(mturk.repboth, x = control, x1 = treatment)
print(mednat.lowrace)
mednat.lowrace <- extract.fd(mednat.lowrace)[3,]
mednat.lowrace

control <- setx(mturk.repboth, bintreat = 0, natidc = 4, rrscale = 4, pid7 = 4)
treatment <- setx(mturk.repboth, bintreat = 1, natidc = 4, rrscale = 4, pid7 = 4)
highnat.lowrace <- sim(mturk.repboth, x = control, x1 = treatment)
print(highnat.lowrace)
highnat.lowrace <- extract.fd(highnat.lowrace)[3,]
highnat.lowrace


control <- setx(mturk.repboth, bintreat = 0, natidc = 1, rrscale = 12, pid7 = 4)
treatment <- setx(mturk.repboth, bintreat = 1, natidc = 1, rrscale = 12, pid7 = 4)
lownat.medrace <- sim(mturk.repboth, x = control, x1 = treatment)
print(lownat.medrace)
lownat.medrace <- extract.fd(lownat.medrace)[3,]
lownat.medrace

control <- setx(mturk.repboth, bintreat = 0, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 12, pid7 = 4)
treatment <- setx(mturk.repboth, bintreat = 1, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 12, pid7 = 4)
mednat.medrace <- sim(mturk.repboth, x = control, x1 = treatment)
print(mednat.medrace)
mednat.medrace <- extract.fd(mednat.medrace)[3,]
mednat.medrace

control <- setx(mturk.repboth, bintreat = 0, natidc = 4, rrscale = 12, pid7 = 4)
treatment <- setx(mturk.repboth, bintreat = 1, natidc = 4, rrscale = 12, pid7 = 4)
highnat.medrace <- sim(mturk.repboth, x = control, x1 = treatment)
print(highnat.medrace)
highnat.medrace <- extract.fd(highnat.medrace)[3,]
highnat.medrace


control <- setx(mturk.repboth, bintreat = 0, natidc = 1, rrscale = 20, pid7 = 4)
treatment <- setx(mturk.repboth, bintreat = 1, natidc = 1, rrscale = 20, pid7 = 4)
lownat.highrace <- sim(mturk.repboth, x = control, x1 = treatment)
print(lownat.highrace)
lownat.highrace <- extract.fd(lownat.highrace)[3,]
lownat.highrace

control <- setx(mturk.repboth, bintreat = 0, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 20, pid7 = 4)
treatment <- setx(mturk.repboth, bintreat = 1, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 20, pid7 = 4)
mednat.highrace <- sim(mturk.repboth, x = control, x1 = treatment)
print(mednat.highrace)
mednat.highrace <- extract.fd(mednat.highrace)[3,]
mednat.highrace

control <- setx(mturk.repboth, bintreat = 0, natidc = 4, rrscale = 20, pid7 = 4)
treatment <- setx(mturk.repboth, bintreat = 1, natidc = 4, rrscale = 20, pid7 = 4)
highnat.highrace <- sim(mturk.repboth, x = control, x1 = treatment)
print(highnat.highrace)
highnat.highrace <- extract.fd(highnat.highrace)[3,]
highnat.highrace

both.lowrace <- c(lownat.lowrace$ev, mednat.lowrace$ev, highnat.lowrace$ev)
both.lowrace.low <- c(lownat.lowrace$lower, mednat.lowrace$lower, highnat.lowrace$lower)
both.lowrace.high <- c(lownat.lowrace$upper, mednat.lowrace$upper, highnat.lowrace$upper)

both.medrace <- c(lownat.medrace$ev, mednat.medrace$ev, highnat.medrace$ev)
both.medrace.low <- c(lownat.medrace$lower, mednat.medrace$lower, highnat.medrace$lower)
both.medrace.high <- c(lownat.medrace$upper, mednat.medrace$upper, highnat.medrace$upper)

both.highrace <- c(lownat.highrace$ev, mednat.highrace$ev, highnat.highrace$ev)
both.highrace.low <- c(lownat.highrace$lower, mednat.highrace$lower, highnat.highrace$lower)
both.highrace.high <- c(lownat.highrace$upper, mednat.highrace$upper, highnat.highrace$upper)





#6.5x8.65
par(family = c("serif"))
par(mfrow = c(3, 3))
par(mar = c(2, .4, 4.25, .4), oma = c(0, 3.5, 0, .1))

x <- barplot(pat.lowrace, ylab = "Marginal Probability", ylim = c(-.7, .7), col = c("grey78", "grey48", "grey30"), cex.main = 1, cex.axis = 1, cex.lab = 1.1, xpd = NA)
box()
title("Low Racial Resentment", line = .53, cex.main = 1.1)
abline(h = 0, lwd = 1.5)
text(c(.7, 1.9, 3.1), -.60, c("Weak\nNatID", "Average\nNatID", "Strong\nNatID"), xpd = NA, cex = 1.05, line = 1.25)
lines(c(x[1], x[1]), c(pat.lowrace.low[1], pat.lowrace.high[1]))
lines(c(x[1]-.05, x[1]+.05), c(pat.lowrace.low[1], pat.lowrace.low[1]))
lines(c(x[1]-.05, x[1]+.05), c(pat.lowrace.high[1], pat.lowrace.high[1]))
lines(c(x[2], x[2]), c(pat.lowrace.low[2], pat.lowrace.high[2]))
lines(c(x[2]-.05, x[2]+.05), c(pat.lowrace.low[2], pat.lowrace.low[2]))
lines(c(x[2]-.05, x[2]+.05), c(pat.lowrace.high[2], pat.lowrace.high[2]))
lines(c(x[3], x[3]), c(pat.lowrace.low[3], pat.lowrace.high[3]))
lines(c(x[3]-.05, x[3]+.05), c(pat.lowrace.low[3], pat.lowrace.low[3]))
lines(c(x[3]-.05, x[3]+.05), c(pat.lowrace.high[3], pat.lowrace.high[3]))


x <- barplot(pat.medrace, ylab = "", yaxt = "n", ylim = c(-.7, .7), col = c("grey78", "grey48", "grey30"), cex.main = 1, cex.axis = .93, cex.lab = .97)
box()
title("Average Racial Resentment", line = .53, cex.main = 1.1)
title("Republican Patriotic Appeal", cex.main = 1.4, line = 3, xpd = NA)
abline(h = 0, lwd = 1.5)
text(c(.7, 1.9, 3.1), -.60, c("Weak\nNatID", "Average\nNatID", "Strong\nNatID"), xpd = NA, cex = 1.05, line = 1.25)
lines(c(x[1], x[1]), c(pat.medrace.low[1], pat.medrace.high[1]))
lines(c(x[1]-.05, x[1]+.05), c(pat.medrace.low[1], pat.medrace.low[1]))
lines(c(x[1]-.05, x[1]+.05), c(pat.medrace.high[1], pat.medrace.high[1]))
lines(c(x[2], x[2]), c(pat.medrace.low[2], pat.medrace.high[2]))
lines(c(x[2]-.05, x[2]+.05), c(pat.medrace.low[2], pat.medrace.low[2]))
lines(c(x[2]-.05, x[2]+.05), c(pat.medrace.high[2], pat.medrace.high[2]))
lines(c(x[3], x[3]), c(pat.medrace.low[3], pat.medrace.high[3]))
lines(c(x[3]-.05, x[3]+.05), c(pat.medrace.low[3], pat.medrace.low[3]))
lines(c(x[3]-.05, x[3]+.05), c(pat.medrace.high[3], pat.medrace.high[3]))


x <- barplot(pat.highrace, ylab = "", yaxt = "n", ylim = c(-.7, .7), col = c("grey78", "grey48", "grey30"), cex.main = 1, cex.axis = .93, cex.lab = .97)
box()
title("High Racial Resentment", line = .53, cex.main = 1.1)
abline(h = 0, lwd = 1.5)
text(c(.7, 1.9, 3.1), -.60, c("Weak\nNatID", "Average\nNatID", "Strong\nNatID"), xpd = NA, cex = 1.05, line = 1.25)
lines(c(x[1], x[1]), c(pat.highrace.low[1], pat.highrace.high[1]))
lines(c(x[1]-.05, x[1]+.05), c(pat.highrace.low[1], pat.highrace.low[1]))
lines(c(x[1]-.05, x[1]+.05), c(pat.highrace.high[1], pat.highrace.high[1]))
lines(c(x[2], x[2]), c(pat.highrace.low[2], pat.highrace.high[2]))
lines(c(x[2]-.05, x[2]+.05), c(pat.highrace.low[2], pat.highrace.low[2]))
lines(c(x[2]-.05, x[2]+.05), c(pat.highrace.high[2], pat.highrace.high[2]))
lines(c(x[3], x[3]), c(pat.highrace.low[3], pat.highrace.high[3]))
lines(c(x[3]-.05, x[3]+.05), c(pat.highrace.low[3], pat.highrace.low[3]))
lines(c(x[3]-.05, x[3]+.05), c(pat.highrace.high[3], pat.highrace.high[3]))



x <- barplot(race.lowrace, ylab = "Marginal Probability", xpd = NA, ylim = c(-.7, .7), col = c("grey78", "grey48", "grey30"), cex.main = 1, cex.axis = 1, cex.lab = 1.1)
box()
title("Low Racial Resentment", line = .53, cex.main = 1.1)
abline(h = 0, lwd = 1.5)
text(c(.7, 1.9, 3.1), -.60, c("Weak\nNatID", "Average\nNatID", "Strong\nNatID"), xpd = NA, cex = 1.05, line = 1.25)
lines(c(x[1], x[1]), c(race.lowrace.low[1], race.lowrace.high[1]))
lines(c(x[1]-.05, x[1]+.05), c(race.lowrace.low[1], race.lowrace.low[1]))
lines(c(x[1]-.05, x[1]+.05), c(race.lowrace.high[1], race.lowrace.high[1]))
lines(c(x[2], x[2]), c(race.lowrace.low[2], race.lowrace.high[2]))
lines(c(x[2]-.05, x[2]+.05), c(race.lowrace.low[2], race.lowrace.low[2]))
lines(c(x[2]-.05, x[2]+.05), c(race.lowrace.high[2], race.lowrace.high[2]))
lines(c(x[3], x[3]), c(race.lowrace.low[3], race.lowrace.high[3]))
lines(c(x[3]-.05, x[3]+.05), c(race.lowrace.low[3], race.lowrace.low[3]))
lines(c(x[3]-.05, x[3]+.05), c(race.lowrace.high[3], race.lowrace.high[3]))



x <- barplot(race.medrace, yaxt = "n", ylab = "", ylim = c(-.7, .7), col = c("grey78", "grey48", "grey30"), cex.main = 1, cex.axis = .93, cex.lab = .97)
box()
title("Average Racial Resentment", line = .53, cex.main = 1.1)
title("Republican Racial Appeal", cex.main = 1.4, line = 3, xpd = NA)
abline(h = 0, lwd = 1.5)
text(c(.7, 1.9, 3.1), -.60, c("Weak\nNatID", "Average\nNatID", "Strong\nNatID"), xpd = NA, cex = 1.05, line = 1.25)
lines(c(x[1], x[1]), c(race.medrace.low[1], race.medrace.high[1]))
lines(c(x[1]-.05, x[1]+.05), c(race.medrace.low[1], race.medrace.low[1]))
lines(c(x[1]-.05, x[1]+.05), c(race.medrace.high[1], race.medrace.high[1]))
lines(c(x[2], x[2]), c(race.medrace.low[2], race.medrace.high[2]))
lines(c(x[2]-.05, x[2]+.05), c(race.medrace.low[2], race.medrace.low[2]))
lines(c(x[2]-.05, x[2]+.05), c(race.medrace.high[2], race.medrace.high[2]))
lines(c(x[3], x[3]), c(race.medrace.low[3], race.medrace.high[3]))
lines(c(x[3]-.05, x[3]+.05), c(race.medrace.low[3], race.medrace.low[3]))
lines(c(x[3]-.05, x[3]+.05), c(race.medrace.high[3], race.medrace.high[3]))




x <- barplot(race.highrace, yaxt = "n", ylab = "", ylim = c(-.7, .7), col = c("grey78", "grey48", "grey30"), cex.main = 1, cex.axis = .93, cex.lab = .97)
box()
title("High Racial Resentment", line = .53, cex.main = 1.1)
abline(h = 0, lwd = 1.5)
text(c(.7, 1.9, 3.1), -.60, c("Weak\nNatID", "Average\nNatID", "Strong\nNatID"), xpd = NA, cex = 1.05, line = 1.25)
lines(c(x[1], x[1]), c(race.highrace.low[1], race.highrace.high[1]))
lines(c(x[1]-.05, x[1]+.05), c(race.highrace.low[1], race.highrace.low[1]))
lines(c(x[1]-.05, x[1]+.05), c(race.highrace.high[1], race.highrace.high[1]))
lines(c(x[2], x[2]), c(race.highrace.low[2], race.highrace.high[2]))
lines(c(x[2]-.05, x[2]+.05), c(race.highrace.low[2], race.highrace.low[2]))
lines(c(x[2]-.05, x[2]+.05), c(race.highrace.high[2], race.highrace.high[2]))
lines(c(x[3], x[3]), c(race.highrace.low[3], race.highrace.high[3]))
lines(c(x[3]-.05, x[3]+.05), c(race.highrace.low[3], race.highrace.low[3]))
lines(c(x[3]-.05, x[3]+.05), c(race.highrace.high[3], race.highrace.high[3]))




x <- barplot(both.lowrace, ylab = "Marginal Probability", xpd = NA, ylim = c(-.7, .7), col = c("grey78", "grey48", "grey30"), cex.main = 1, cex.axis = 1, cex.lab = 1.1)
box()
title("Low Racial Resentment", line = .53, cex.main = 1.1)
abline(h = 0, lwd = 1.5)
text(c(.7, 1.9, 3.1), -.60, c("Weak\nNatID", "Average\nNatID", "Strong\nNatID"), xpd = NA, cex = 1.05, line = 1.25)
lines(c(x[1], x[1]), c(both.lowrace.low[1], both.lowrace.high[1]))
lines(c(x[1]-.05, x[1]+.05), c(both.lowrace.low[1], both.lowrace.low[1]))
lines(c(x[1]-.05, x[1]+.05), c(both.lowrace.high[1], both.lowrace.high[1]))
lines(c(x[2], x[2]), c(both.lowrace.low[2], both.lowrace.high[2]))
lines(c(x[2]-.05, x[2]+.05), c(both.lowrace.low[2], both.lowrace.low[2]))
lines(c(x[2]-.05, x[2]+.05), c(both.lowrace.high[2], both.lowrace.high[2]))
lines(c(x[3], x[3]), c(both.lowrace.low[3], both.lowrace.high[3]))
lines(c(x[3]-.05, x[3]+.05), c(both.lowrace.low[3], both.lowrace.low[3]))
lines(c(x[3]-.05, x[3]+.05), c(both.lowrace.high[3], both.lowrace.high[3]))





x <- barplot(both.medrace, yaxt = "n", ylab = "", ylim = c(-.7, .7), col = c("grey78", "grey48", "grey30"), cex.main = 1, cex.axis = .93, cex.lab = .97)
box()
abline(h = 0, lwd = 1.5)
title("Average Racial Resentment", line = .53, cex.main = 1.1)
title("Republican Patriotic + Racial Appeal", cex.main = 1.4, line = 3, xpd = NA)
lines(c(x[1], x[1]), c(both.medrace.low[1], both.medrace.high[1]))
lines(c(x[1]-.05, x[1]+.05), c(both.medrace.low[1], both.medrace.low[1]))
lines(c(x[1]-.05, x[1]+.05), c(both.medrace.high[1], both.medrace.high[1]))
lines(c(x[2], x[2]), c(both.medrace.low[2], both.medrace.high[2]))
lines(c(x[2]-.05, x[2]+.05), c(both.medrace.low[2], both.medrace.low[2]))
lines(c(x[2]-.05, x[2]+.05), c(both.medrace.high[2], both.medrace.high[2]))
lines(c(x[3], x[3]), c(both.medrace.low[3], both.medrace.high[3]))
lines(c(x[3]-.05, x[3]+.05), c(both.medrace.low[3], both.medrace.low[3]))
lines(c(x[3]-.05, x[3]+.05), c(both.medrace.high[3], both.medrace.high[3]))




abline(h = 0, lwd = 1.5)
text(c(.7, 1.9, 3.1), -.60, c("Weak\nNatID", "Average\nNatID", "Strong\nNatID"), xpd = NA, cex = 1.05, line = 1.25)

x <- barplot(both.highrace, yaxt = "n", ylab = "", ylim = c(-.7, .7), col = c("grey78", "grey48", "grey30"), cex.main = 1, cex.axis = .93, cex.lab = .97)
box()
title("High Racial Resentment", line = .53, cex.main = 1.1)
abline(h = 0, lwd = 1.5)
text(c(.7, 1.9, 3.1), -.60, c("Weak\nNatID", "Average\nNatID", "Strong\nNatID"), xpd = NA, cex = 1.05, line = 1.25)
lines(c(x[1], x[1]), c(both.highrace.low[1], both.highrace.high[1]))
lines(c(x[1]-.05, x[1]+.05), c(both.highrace.low[1], both.highrace.low[1]))
lines(c(x[1]-.05, x[1]+.05), c(both.highrace.high[1], both.highrace.high[1]))
lines(c(x[2], x[2]), c(both.highrace.low[2], both.highrace.high[2]))
lines(c(x[2]-.05, x[2]+.05), c(both.highrace.low[2], both.highrace.low[2]))
lines(c(x[2]-.05, x[2]+.05), c(both.highrace.high[2], both.highrace.high[2]))
lines(c(x[3], x[3]), c(both.highrace.low[3], both.highrace.high[3]))
lines(c(x[3]-.05, x[3]+.05), c(both.highrace.low[3], both.highrace.low[3]))
lines(c(x[3]-.05, x[3]+.05), c(both.highrace.high[3], both.highrace.high[3]))






######MTURK DEMOCRATIC RACIAL PREDICTED PROBABILITIES (FIGURE 3)##########

mturk$voted3 <- as.factor(mturk$voted3) #Convert variables to factors

mturk.predict <- mturk

mturk.predict$bintreat <- ifelse(mturk.predict$treatment == 1, 0, ifelse(as.integer(mturk.predict$treatment) > 1, 1, "")) #Create binary indicator for treatment
mturk.predict$bintreat <- as.integer(mturk.predict$bintreat)

mturk.dempatdat <- na.omit(subset(mturk.predict, treatname == "Control" | treatname == "DemPat", select = c(bintreat, natidc, voted3, voter, pid7, treatname, rrscale))) #Create subsets
mturk.demracedat <- na.omit(subset(mturk.predict, treatname == "Control" | treatname == "DemRace", select = c(bintreat, natidc, voted3, voter, pid7, treatname,  rrscale)))
mturk.dembothdat <- na.omit(subset(mturk.predict, treatname == "Control" | treatname == "DemBoth", select = c(bintreat, natidc, voted3, voter,pid7, treatname, rrscale)))


mturk.dempat <- zelig(as.factor(voted3) ~ bintreat*natidc + bintreat*rrscale + bintreat*pid7, data = mturk.dempatdat, model = "ologit")
summary(mturk.dempat)

control <- setx(mturk.dempat, bintreat = 0, natidc = 1, rrscale = 4, pid7 = 4)
treatment <- setx(mturk.dempat, bintreat = 1, natidc = 1, rrscale = 4, pid7 = 4)
lownat.lowrace <- sim(mturk.dempat, x = control, x1 = treatment)
print(lownat.lowrace)
lownat.lowrace <- extract.fd(lownat.lowrace)[3,]
lownat.lowrace

control <- setx(mturk.dempat, bintreat = 0, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 4, pid7 = 4)
treatment <- setx(mturk.dempat, bintreat = 1, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 4, pid7 = 4)
mednat.lowrace <- sim(mturk.dempat, x = control, x1 = treatment)
print(mednat.lowrace)
mednat.lowrace <- extract.fd(mednat.lowrace)[3,]
mednat.lowrace

control <- setx(mturk.dempat, bintreat = 0, natidc = 4, rrscale = 4, pid7 = 4)
treatment <- setx(mturk.dempat, bintreat = 1, natidc = 4, rrscale = 4, pid7 = 4)
highnat.lowrace <- sim(mturk.dempat, x = control, x1 = treatment)
print(highnat.lowrace)
highnat.lowrace <- extract.fd(highnat.lowrace)[3,]
highnat.lowrace


control <- setx(mturk.dempat, bintreat = 0, natidc = 1, rrscale = 12, pid7 = 4)
treatment <- setx(mturk.dempat, bintreat = 1, natidc = 1, rrscale = 12, pid7 = 4)
lownat.medrace <- sim(mturk.dempat, x = control, x1 = treatment)
print(lownat.medrace)
lownat.medrace <- extract.fd(lownat.medrace)[3,]
lownat.medrace

control <- setx(mturk.dempat, bintreat = 0, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 12, pid7 = 4)
treatment <- setx(mturk.dempat, bintreat = 1, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 12, pid7 = 4)
mednat.medrace <- sim(mturk.dempat, x = control, x1 = treatment)
print(mednat.medrace)
mednat.medrace <- extract.fd(mednat.medrace)[3,]
mednat.medrace

control <- setx(mturk.dempat, bintreat = 0, natidc = 4, rrscale = 12, pid7 = 4)
treatment <- setx(mturk.dempat, bintreat = 1, natidc = 4, rrscale = 12, pid7 = 4)
highnat.medrace <- sim(mturk.dempat, x = control, x1 = treatment)
print(highnat.medrace)
highnat.medrace <- extract.fd(highnat.medrace)[3,]
highnat.medrace


control <- setx(mturk.dempat, bintreat = 0, natidc = 1, rrscale = 20, pid7 = 4)
treatment <- setx(mturk.dempat, bintreat = 1, natidc = 1, rrscale = 20, pid7 = 4)
lownat.highrace <- sim(mturk.dempat, x = control, x1 = treatment)
print(lownat.highrace)
lownat.highrace <- extract.fd(lownat.highrace)[3,]
lownat.highrace

control <- setx(mturk.dempat, bintreat = 0, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 20, pid7 = 4)
treatment <- setx(mturk.dempat, bintreat = 1, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 20, pid7 = 4)
mednat.highrace <- sim(mturk.dempat, x = control, x1 = treatment)
print(mednat.highrace)
mednat.highrace <- extract.fd(mednat.highrace)[3,]
mednat.highrace

control <- setx(mturk.dempat, bintreat = 0, natidc = 4, rrscale = 20, pid7 = 4)
treatment <- setx(mturk.dempat, bintreat = 1, natidc = 4, rrscale = 20, pid7 = 4)
highnat.highrace <- sim(mturk.dempat, x = control, x1 = treatment)
print(highnat.highrace)
highnat.highrace <- extract.fd(highnat.highrace)[3,]
highnat.highrace

pat.lowrace <- c(lownat.lowrace$ev, mednat.lowrace$ev, highnat.lowrace$ev)
pat.lowrace.low <- c(lownat.lowrace$lower, mednat.lowrace$lower, highnat.lowrace$lower)
pat.lowrace.high <- c(lownat.lowrace$upper, mednat.lowrace$upper, highnat.lowrace$upper)


pat.medrace <- c(lownat.medrace$ev, mednat.medrace$ev, highnat.medrace$ev)
pat.medrace.low <- c(lownat.medrace$lower, mednat.medrace$lower, highnat.medrace$lower)
pat.medrace.high <- c(lownat.medrace$upper, mednat.medrace$upper, highnat.medrace$upper)


pat.highrace <- c(lownat.highrace$ev, mednat.highrace$ev, highnat.highrace$ev)
pat.highrace.low <- c(lownat.highrace$lower, mednat.highrace$lower, highnat.highrace$lower)
pat.highrace.high <- c(lownat.highrace$upper, mednat.highrace$upper, highnat.highrace$upper)





mturk.demrace <- zelig(as.factor(voted3) ~ bintreat*natidc + bintreat*rrscale + bintreat*pid7, data = mturk.demracedat, model = "ologit")
summary(mturk.demrace)

control <- setx(mturk.demrace, bintreat = 0, natidc = 1, rrscale = 4, pid7 = 4)
treatment <- setx(mturk.demrace, bintreat = 1, natidc = 1, rrscale = 4, pid7 = 4)
lownat.lowrace <- sim(mturk.demrace, x = control, x1 = treatment)
print(lownat.lowrace)
lownat.lowrace <- extract.fd(lownat.lowrace)[3,]
lownat.lowrace

control <- setx(mturk.demrace, bintreat = 0, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 4, pid7 = 4)
treatment <- setx(mturk.demrace, bintreat = 1, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 4, pid7 = 4)
mednat.lowrace <- sim(mturk.demrace, x = control, x1 = treatment)
print(mednat.lowrace)
mednat.lowrace <- extract.fd(mednat.lowrace)[3,]
mednat.lowrace

control <- setx(mturk.demrace, bintreat = 0, natidc = 4, rrscale = 4, pid7 = 4)
treatment <- setx(mturk.demrace, bintreat = 1, natidc = 4, rrscale = 4, pid7 = 4)
highnat.lowrace <- sim(mturk.demrace, x = control, x1 = treatment)
print(highnat.lowrace)
highnat.lowrace <- extract.fd(highnat.lowrace)[3,]
highnat.lowrace


control <- setx(mturk.demrace, bintreat = 0, natidc = 1, rrscale = 12, pid7 = 4)
treatment <- setx(mturk.demrace, bintreat = 1, natidc = 1, rrscale = 12, pid7 = 4)
lownat.medrace <- sim(mturk.demrace, x = control, x1 = treatment)
print(lownat.medrace)
lownat.medrace <- extract.fd(lownat.medrace)[3,]
lownat.medrace

control <- setx(mturk.demrace, bintreat = 0, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 12, pid7 = 4)
treatment <- setx(mturk.demrace, bintreat = 1, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 12, pid7 = 4)
mednat.medrace <- sim(mturk.demrace, x = control, x1 = treatment)
print(mednat.medrace)
mednat.medrace <- extract.fd(mednat.medrace)[3,]
mednat.medrace

control <- setx(mturk.demrace, bintreat = 0, natidc = 4, rrscale = 12, pid7 = 4)
treatment <- setx(mturk.demrace, bintreat = 1, natidc = 4, rrscale = 12, pid7 = 4)
highnat.medrace <- sim(mturk.demrace, x = control, x1 = treatment)
print(highnat.medrace)
highnat.medrace <- extract.fd(highnat.medrace)[3,]
highnat.medrace


control <- setx(mturk.demrace, bintreat = 0, natidc = 1, rrscale = 20, pid7 = 4)
treatment <- setx(mturk.demrace, bintreat = 1, natidc = 1, rrscale = 20, pid7 = 4)
lownat.highrace <- sim(mturk.demrace, x = control, x1 = treatment)
print(lownat.highrace)
lownat.highrace <- extract.fd(lownat.highrace)[3,]
lownat.highrace

control <- setx(mturk.demrace, bintreat = 0, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 20, pid7 = 4)
treatment <- setx(mturk.demrace, bintreat = 1, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 20, pid7 = 4)
mednat.highrace <- sim(mturk.demrace, x = control, x1 = treatment)
print(mednat.highrace)
mednat.highrace <- extract.fd(mednat.highrace)[3,]
mednat.highrace

control <- setx(mturk.demrace, bintreat = 0, natidc = 4, rrscale = 20, pid7 = 4)
treatment <- setx(mturk.demrace, bintreat = 1, natidc = 4, rrscale = 20, pid7 = 4)
highnat.highrace <- sim(mturk.demrace, x = control, x1 = treatment)
print(highnat.highrace)
highnat.highrace <- extract.fd(highnat.highrace)[3,]
highnat.highrace

race.lowrace <- c(lownat.lowrace$ev, mednat.lowrace$ev, highnat.lowrace$ev)
race.lowrace.low <- c(lownat.lowrace$lower, mednat.lowrace$lower, highnat.lowrace$lower)
race.lowrace.high <- c(lownat.lowrace$upper, mednat.lowrace$upper, highnat.lowrace$upper)

race.medrace <- c(lownat.medrace$ev, mednat.medrace$ev, highnat.medrace$ev)
race.medrace.low <- c(lownat.medrace$lower, mednat.medrace$lower, highnat.medrace$lower)
race.medrace.high <- c(lownat.medrace$upper, mednat.medrace$upper, highnat.medrace$upper)

race.highrace <- c(lownat.highrace$ev, mednat.highrace$ev, highnat.highrace$ev)
race.highrace.low <- c(lownat.highrace$lower, mednat.highrace$lower, highnat.highrace$lower)
race.highrace.high <- c(lownat.highrace$upper, mednat.highrace$upper, highnat.highrace$upper)



mturk.demboth <- zelig(as.factor(voted3) ~ bintreat*natidc + bintreat*rrscale + bintreat*pid7, data = mturk.dembothdat, model = "ologit")
summary(mturk.demboth)

control <- setx(mturk.demboth, bintreat = 0, natidc = 1, rrscale = 4, pid7 = 4)
treatment <- setx(mturk.demboth, bintreat = 1, natidc = 1, rrscale = 4, pid7 = 4)
lownat.lowrace <- sim(mturk.demboth, x = control, x1 = treatment)
print(lownat.lowrace)
lownat.lowrace <- extract.fd(lownat.lowrace)[3,]
lownat.lowrace

control <- setx(mturk.demboth, bintreat = 0, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 4, pid7 = 4)
treatment <- setx(mturk.demboth, bintreat = 1, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 4, pid7 = 4)
mednat.lowrace <- sim(mturk.demboth, x = control, x1 = treatment)
print(mednat.lowrace)
mednat.lowrace <- extract.fd(mednat.lowrace)[3,]
mednat.lowrace

control <- setx(mturk.demboth, bintreat = 0, natidc = 4, rrscale = 4, pid7 = 4)
treatment <- setx(mturk.demboth, bintreat = 1, natidc = 4, rrscale = 4, pid7 = 4)
highnat.lowrace <- sim(mturk.demboth, x = control, x1 = treatment)
print(highnat.lowrace)
highnat.lowrace <- extract.fd(highnat.lowrace)[3,]
highnat.lowrace


control <- setx(mturk.demboth, bintreat = 0, natidc = 1, rrscale = 12, pid7 = 4)
treatment <- setx(mturk.demboth, bintreat = 1, natidc = 1, rrscale = 12, pid7 = 4)
lownat.medrace <- sim(mturk.demboth, x = control, x1 = treatment)
print(lownat.medrace)
lownat.medrace <- extract.fd(lownat.medrace)[3,]
lownat.medrace

control <- setx(mturk.demboth, bintreat = 0, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 12, pid7 = 4)
treatment <- setx(mturk.demboth, bintreat = 1, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 12, pid7 = 4)
mednat.medrace <- sim(mturk.demboth, x = control, x1 = treatment)
print(mednat.medrace)
mednat.medrace <- extract.fd(mednat.medrace)[3,]
mednat.medrace

control <- setx(mturk.demboth, bintreat = 0, natidc = 4, rrscale = 12, pid7 = 4)
treatment <- setx(mturk.demboth, bintreat = 1, natidc = 4, rrscale = 12, pid7 = 4)
highnat.medrace <- sim(mturk.demboth, x = control, x1 = treatment)
print(highnat.medrace)
highnat.medrace <- extract.fd(highnat.medrace)[3,]
highnat.medrace


control <- setx(mturk.demboth, bintreat = 0, natidc = 1, rrscale = 20, pid7 = 4)
treatment <- setx(mturk.demboth, bintreat = 1, natidc = 1, rrscale = 20, pid7 = 4)
lownat.highrace <- sim(mturk.demboth, x = control, x1 = treatment)
print(lownat.highrace)
lownat.highrace <- extract.fd(lownat.highrace)[3,]
lownat.highrace

control <- setx(mturk.demboth, bintreat = 0, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 20, pid7 = 4)
treatment <- setx(mturk.demboth, bintreat = 1, natidc = mean(mturk$natidc, na.rm = TRUE), rrscale = 20, pid7 = 4)
mednat.highrace <- sim(mturk.demboth, x = control, x1 = treatment)
print(mednat.highrace)
mednat.highrace <- extract.fd(mednat.highrace)[3,]
mednat.highrace

control <- setx(mturk.demboth, bintreat = 0, natidc = 4, rrscale = 20, pid7 = 4)
treatment <- setx(mturk.demboth, bintreat = 1, natidc = 4, rrscale = 20, pid7 = 4)
highnat.highrace <- sim(mturk.demboth, x = control, x1 = treatment)
print(highnat.highrace)
highnat.highrace <- extract.fd(highnat.highrace)[3,]
highnat.highrace

both.lowrace <- c(lownat.lowrace$ev, mednat.lowrace$ev, highnat.lowrace$ev)
both.lowrace.low <- c(lownat.lowrace$lower, mednat.lowrace$lower, highnat.lowrace$lower)
both.lowrace.high <- c(lownat.lowrace$upper, mednat.lowrace$upper, highnat.lowrace$upper)

both.medrace <- c(lownat.medrace$ev, mednat.medrace$ev, highnat.medrace$ev)
both.medrace.low <- c(lownat.medrace$lower, mednat.medrace$lower, highnat.medrace$lower)
both.medrace.high <- c(lownat.medrace$upper, mednat.medrace$upper, highnat.medrace$upper)

both.highrace <- c(lownat.highrace$ev, mednat.highrace$ev, highnat.highrace$ev)
both.highrace.low <- c(lownat.highrace$lower, mednat.highrace$lower, highnat.highrace$lower)
both.highrace.high <- c(lownat.highrace$upper, mednat.highrace$upper, highnat.highrace$upper)







#6.5x8.65
par(family = c("serif"))
par(mfrow = c(3, 3))
par(mar = c(2, .4, 4.25, .4), oma = c(0, 3.5, 0, .1))

x <- barplot(pat.lowrace, ylab = "Marginal Probability", ylim = c(-.7, .7), col = c("grey78", "grey48", "grey30"), cex.main = 1, cex.axis = 1, cex.lab = 1.1, xpd = NA)
box()
title("Low Racial Resentment", line = .53, cex.main = 1.1)
abline(h = 0, lwd = 1.5)
text(c(.7, 1.9, 3.1), -.60, c("Weak\nNatID", "Average\nNatID", "Strong\nNatID"), xpd = NA, cex = 1.05, line = 1.25)
lines(c(x[1], x[1]), c(pat.lowrace.low[1], pat.lowrace.high[1]))
lines(c(x[1]-.05, x[1]+.05), c(pat.lowrace.low[1], pat.lowrace.low[1]))
lines(c(x[1]-.05, x[1]+.05), c(pat.lowrace.high[1], pat.lowrace.high[1]))
lines(c(x[2], x[2]), c(pat.lowrace.low[2], pat.lowrace.high[2]))
lines(c(x[2]-.05, x[2]+.05), c(pat.lowrace.low[2], pat.lowrace.low[2]))
lines(c(x[2]-.05, x[2]+.05), c(pat.lowrace.high[2], pat.lowrace.high[2]))
lines(c(x[3], x[3]), c(pat.lowrace.low[3], pat.lowrace.high[3]))
lines(c(x[3]-.05, x[3]+.05), c(pat.lowrace.low[3], pat.lowrace.low[3]))
lines(c(x[3]-.05, x[3]+.05), c(pat.lowrace.high[3], pat.lowrace.high[3]))


x <- barplot(pat.medrace, ylab = "", yaxt = "n", ylim = c(-.7, .7), col = c("grey78", "grey48", "grey30"), cex.main = 1, cex.axis = .93, cex.lab = .97)
box()
title("Average Racial Resentment", line = .53, cex.main = 1.1)
title("Democratic Patriotic Appeal", cex.main = 1.4, line = 3, xpd = NA)
abline(h = 0, lwd = 1.5)
text(c(.7, 1.9, 3.1), -.60, c("Weak\nNatID", "Average\nNatID", "Strong\nNatID"), xpd = NA, cex = 1.05, line = 1.25)
lines(c(x[1], x[1]), c(pat.medrace.low[1], pat.medrace.high[1]))
lines(c(x[1]-.05, x[1]+.05), c(pat.medrace.low[1], pat.medrace.low[1]))
lines(c(x[1]-.05, x[1]+.05), c(pat.medrace.high[1], pat.medrace.high[1]))
lines(c(x[2], x[2]), c(pat.medrace.low[2], pat.medrace.high[2]))
lines(c(x[2]-.05, x[2]+.05), c(pat.medrace.low[2], pat.medrace.low[2]))
lines(c(x[2]-.05, x[2]+.05), c(pat.medrace.high[2], pat.medrace.high[2]))
lines(c(x[3], x[3]), c(pat.medrace.low[3], pat.medrace.high[3]))
lines(c(x[3]-.05, x[3]+.05), c(pat.medrace.low[3], pat.medrace.low[3]))
lines(c(x[3]-.05, x[3]+.05), c(pat.medrace.high[3], pat.medrace.high[3]))


x <- barplot(pat.highrace, ylab = "", yaxt = "n", ylim = c(-.7, .7), col = c("grey78", "grey48", "grey30"), cex.main = 1, cex.axis = .93, cex.lab = .97)
box()
title("High Racial Resentment", line = .53, cex.main = 1.1)
abline(h = 0, lwd = 1.5)
text(c(.7, 1.9, 3.1), -.60, c("Weak\nNatID", "Average\nNatID", "Strong\nNatID"), xpd = NA, cex = 1.05, line = 1.25)
lines(c(x[1], x[1]), c(pat.highrace.low[1], pat.highrace.high[1]))
lines(c(x[1]-.05, x[1]+.05), c(pat.highrace.low[1], pat.highrace.low[1]))
lines(c(x[1]-.05, x[1]+.05), c(pat.highrace.high[1], pat.highrace.high[1]))
lines(c(x[2], x[2]), c(pat.highrace.low[2], pat.highrace.high[2]))
lines(c(x[2]-.05, x[2]+.05), c(pat.highrace.low[2], pat.highrace.low[2]))
lines(c(x[2]-.05, x[2]+.05), c(pat.highrace.high[2], pat.highrace.high[2]))
lines(c(x[3], x[3]), c(pat.highrace.low[3], pat.highrace.high[3]))
lines(c(x[3]-.05, x[3]+.05), c(pat.highrace.low[3], pat.highrace.low[3]))
lines(c(x[3]-.05, x[3]+.05), c(pat.highrace.high[3], pat.highrace.high[3]))



x <- barplot(race.lowrace, ylab = "Marginal Probability", xpd = NA, ylim = c(-.7, .7), col = c("grey78", "grey48", "grey30"), cex.main = 1, cex.axis = 1, cex.lab = 1.1)
box()
title("Low Racial Resentment", line = .53, cex.main = 1.1)
abline(h = 0, lwd = 1.5)
text(c(.7, 1.9, 3.1), -.60, c("Weak\nNatID", "Average\nNatID", "Strong\nNatID"), xpd = NA, cex = 1.05, line = 1.25)
lines(c(x[1], x[1]), c(race.lowrace.low[1], race.lowrace.high[1]))
lines(c(x[1]-.05, x[1]+.05), c(race.lowrace.low[1], race.lowrace.low[1]))
lines(c(x[1]-.05, x[1]+.05), c(race.lowrace.high[1], race.lowrace.high[1]))
lines(c(x[2], x[2]), c(race.lowrace.low[2], race.lowrace.high[2]))
lines(c(x[2]-.05, x[2]+.05), c(race.lowrace.low[2], race.lowrace.low[2]))
lines(c(x[2]-.05, x[2]+.05), c(race.lowrace.high[2], race.lowrace.high[2]))
lines(c(x[3], x[3]), c(race.lowrace.low[3], race.lowrace.high[3]))
lines(c(x[3]-.05, x[3]+.05), c(race.lowrace.low[3], race.lowrace.low[3]))
lines(c(x[3]-.05, x[3]+.05), c(race.lowrace.high[3], race.lowrace.high[3]))



x <- barplot(race.medrace, yaxt = "n", ylab = "", ylim = c(-.7, .7), col = c("grey78", "grey48", "grey30"), cex.main = 1, cex.axis = .93, cex.lab = .97)
box()
title("Average Racial Resentment", line = .53, cex.main = 1.1)
title("Democratic Racial Appeal", cex.main = 1.4, line = 3, xpd = NA)
abline(h = 0, lwd = 1.5)
text(c(.7, 1.9, 3.1), -.60, c("Weak\nNatID", "Average\nNatID", "Strong\nNatID"), xpd = NA, cex = 1.05, line = 1.25)
lines(c(x[1], x[1]), c(race.medrace.low[1], race.medrace.high[1]))
lines(c(x[1]-.05, x[1]+.05), c(race.medrace.low[1], race.medrace.low[1]))
lines(c(x[1]-.05, x[1]+.05), c(race.medrace.high[1], race.medrace.high[1]))
lines(c(x[2], x[2]), c(race.medrace.low[2], race.medrace.high[2]))
lines(c(x[2]-.05, x[2]+.05), c(race.medrace.low[2], race.medrace.low[2]))
lines(c(x[2]-.05, x[2]+.05), c(race.medrace.high[2], race.medrace.high[2]))
lines(c(x[3], x[3]), c(race.medrace.low[3], race.medrace.high[3]))
lines(c(x[3]-.05, x[3]+.05), c(race.medrace.low[3], race.medrace.low[3]))
lines(c(x[3]-.05, x[3]+.05), c(race.medrace.high[3], race.medrace.high[3]))




x <- barplot(race.highrace, yaxt = "n", ylab = "", ylim = c(-.7, .7), col = c("grey78", "grey48", "grey30"), cex.main = 1, cex.axis = .93, cex.lab = .97)
box()
title("High Racial Resentment", line = .53, cex.main = 1.1)
abline(h = 0, lwd = 1.5)
text(c(.7, 1.9, 3.1), -.60, c("Weak\nNatID", "Average\nNatID", "Strong\nNatID"), xpd = NA, cex = 1.05, line = 1.25)
lines(c(x[1], x[1]), c(race.highrace.low[1], race.highrace.high[1]))
lines(c(x[1]-.05, x[1]+.05), c(race.highrace.low[1], race.highrace.low[1]))
lines(c(x[1]-.05, x[1]+.05), c(race.highrace.high[1], race.highrace.high[1]))
lines(c(x[2], x[2]), c(race.highrace.low[2], race.highrace.high[2]))
lines(c(x[2]-.05, x[2]+.05), c(race.highrace.low[2], race.highrace.low[2]))
lines(c(x[2]-.05, x[2]+.05), c(race.highrace.high[2], race.highrace.high[2]))
lines(c(x[3], x[3]), c(race.highrace.low[3], race.highrace.high[3]))
lines(c(x[3]-.05, x[3]+.05), c(race.highrace.low[3], race.highrace.low[3]))
lines(c(x[3]-.05, x[3]+.05), c(race.highrace.high[3], race.highrace.high[3]))




x <- barplot(both.lowrace, ylab = "Marginal Probability", xpd = NA, ylim = c(-.7, .7), col = c("grey78", "grey48", "grey30"), cex.main = 1, cex.axis = 1, cex.lab = 1.1)
box()
title("Low Racial Resentment", line = .53, cex.main = 1.1)
abline(h = 0, lwd = 1.5)
text(c(.7, 1.9, 3.1), -.60, c("Weak\nNatID", "Average\nNatID", "Strong\nNatID"), xpd = NA, cex = 1.05, line = 1.25)
lines(c(x[1], x[1]), c(both.lowrace.low[1], both.lowrace.high[1]))
lines(c(x[1]-.05, x[1]+.05), c(both.lowrace.low[1], both.lowrace.low[1]))
lines(c(x[1]-.05, x[1]+.05), c(both.lowrace.high[1], both.lowrace.high[1]))
lines(c(x[2], x[2]), c(both.lowrace.low[2], both.lowrace.high[2]))
lines(c(x[2]-.05, x[2]+.05), c(both.lowrace.low[2], both.lowrace.low[2]))
lines(c(x[2]-.05, x[2]+.05), c(both.lowrace.high[2], both.lowrace.high[2]))
lines(c(x[3], x[3]), c(both.lowrace.low[3], both.lowrace.high[3]))
lines(c(x[3]-.05, x[3]+.05), c(both.lowrace.low[3], both.lowrace.low[3]))
lines(c(x[3]-.05, x[3]+.05), c(both.lowrace.high[3], both.lowrace.high[3]))





x <- barplot(both.medrace, yaxt = "n", ylab = "", ylim = c(-.7, .7), col = c("grey78", "grey48", "grey30"), cex.main = 1, cex.axis = .93, cex.lab = .97)
box()
abline(h = 0, lwd = 1.5)
title("Average Racial Resentment", line = .53, cex.main = 1.1)
title("Democratic Patriotic + Racial Appeals", cex.main = 1.4, line = 3, xpd = NA)
lines(c(x[1], x[1]), c(both.medrace.low[1], both.medrace.high[1]))
lines(c(x[1]-.05, x[1]+.05), c(both.medrace.low[1], both.medrace.low[1]))
lines(c(x[1]-.05, x[1]+.05), c(both.medrace.high[1], both.medrace.high[1]))
lines(c(x[2], x[2]), c(both.medrace.low[2], both.medrace.high[2]))
lines(c(x[2]-.05, x[2]+.05), c(both.medrace.low[2], both.medrace.low[2]))
lines(c(x[2]-.05, x[2]+.05), c(both.medrace.high[2], both.medrace.high[2]))
lines(c(x[3], x[3]), c(both.medrace.low[3], both.medrace.high[3]))
lines(c(x[3]-.05, x[3]+.05), c(both.medrace.low[3], both.medrace.low[3]))
lines(c(x[3]-.05, x[3]+.05), c(both.medrace.high[3], both.medrace.high[3]))




abline(h = 0, lwd = 1.5)
text(c(.7, 1.9, 3.1), -.60, c("Weak\nNatID", "Average\nNatID", "Strong\nNatID"), xpd = NA, cex = 1.05, line = 1.25)

x <- barplot(both.highrace, yaxt = "n", ylab = "", ylim = c(-.7, .7), col = c("grey78", "grey48", "grey30"), cex.main = 1, cex.axis = .93, cex.lab = .97)
box()
title("High Racial Resentment", line = .53, cex.main = 1.1)
abline(h = 0, lwd = 1.5)
text(c(.7, 1.9, 3.1), -.60, c("Weak\nNatID", "Average\nNatID", "Strong\nNatID"), xpd = NA, cex = 1.05, line = 1.25)
lines(c(x[1], x[1]), c(both.highrace.low[1], both.highrace.high[1]))
lines(c(x[1]-.05, x[1]+.05), c(both.highrace.low[1], both.highrace.low[1]))
lines(c(x[1]-.05, x[1]+.05), c(both.highrace.high[1], both.highrace.high[1]))
lines(c(x[2], x[2]), c(both.highrace.low[2], both.highrace.high[2]))
lines(c(x[2]-.05, x[2]+.05), c(both.highrace.low[2], both.highrace.low[2]))
lines(c(x[2]-.05, x[2]+.05), c(both.highrace.high[2], both.highrace.high[2]))
lines(c(x[3], x[3]), c(both.highrace.low[3], both.highrace.high[3]))
lines(c(x[3]-.05, x[3]+.05), c(both.highrace.low[3], both.highrace.low[3]))
lines(c(x[3]-.05, x[3]+.05), c(both.highrace.high[3], both.highrace.high[3]))


#####SSI DEMOGRAPHICS (FOOTNOTE 2 & PG. 11, OA-A)#####

ssi.age <- mean(ssi$age, na.rm = TRUE)
print(ssi.age) #46.05

ssi.gender <- prop.table(table(ssi$female))
print(ssi.gender) #62.67% female

ssi.income <- median(ssi$income, na.rm = TRUE)
print(ssi.income) #45-65K

ssi.education <- median(ssi$education, na.rm = TRUE)
print(ssi.education) #Associate degree

ssi.married <- prop.table(table(ssi$married))
print(ssi.married) #51.06% married

ssi.children <- prop.table(table(ssi$children))
print(ssi.children) #34.27% have kids under 18

ssi.attend <- mean(ssi$attend, na.rm = TRUE)
print(ssi.attend) #2.42

ssi.evang <- prop.table(table(ssi$bornagain))
print(ssi.evang) #32% evangelical

ssi.pid <- mean(ssi$pid7, na.rm = TRUE)
print(ssi.pid) #3.98

ssi.ideology <- mean(ssi$ideology, na.rm = TRUE)
print(ssi.ideology) #4.06

ssi.natid <- mean(ssi$natid, na.rm = TRUE)
print(ssi.natid) #8.20



#####MTURK DEMOGRAPHICS (FOOTNOTE 7 & PG. 16, OA-A)#####

mturk.age <- mean(mturk$age, na.rm = TRUE)
print(mturk.age) #38.51

mturk.gender <- prop.table(table(mturk$female))
print(mturk.gender) #60.02% female

mturk.income <- median(mturk$income, na.rm = TRUE)
print(mturk.income) #45-65K

mturk.education <- median(mturk$education, na.rm = TRUE)
print(mturk.education) #Bachelor's degree

mturk.married <- prop.table(table(mturk$married))
print(mturk.married) #51.36% married

mturk.children <- prop.table(table(mturk$children))
print(mturk.children) #40.13% have kids under 18

mturk.attend <- mean(mturk$attend, na.rm = TRUE)
print(mturk.attend) #2.05

mturk.evang <- prop.table(table(mturk$bornagain))
print(mturk.evang) #26.56% evangelical

mturk.pid <- mean(mturk$pid7, na.rm = TRUE)
print(mturk.pid) #3.95

mturk.ideology <- mean(mturk$ideology, na.rm = TRUE)
print(mturk.ideology) #3.82

mturk.natid <- mean(mturk$natid, na.rm = TRUE)
print(mturk.natid) #7.43

mturk.resent <- mean(mturk$rrscale, na.rm = TRUE)
print(mturk.resent) #11.68



#####2016 ANES NATIONAL DEMOGRAPHICS (FOOTNOTES 7 & 11, OA-A)#####

anes <- read.csv("ANES White Distributions.csv")
head(anes)

anes$pid3 <- ifelse(anes$pid <= 3, 1, ifelse(anes$pid == 4, 2, ifelse(anes$pid >= 5, 3, "")))

prop.table(table(ssi$pid3)) #40/40/20
prop.table(table(anes$pid3)) #40/50/10

dem <- subset(anes, pid3 == 1)
rep <- subset(anes, pid3 == 3)
ind <- subset(anes, pid3 == 2)


anes.age <- aggregate(anes$age, by = list(anes$pid3), weights = anes$weights, weighted.mean)
print(anes.age)
anes.age <- (50.07*.404 + 48.37 * .185 + 52.07 * .411)/100
print(anes.age) #50.6

anes.gender <- wtd.table(anes$gender, weights = anes$weights)
print(anes.gender)
total <- 1310.076 + 1418.093
anes.gender <- (1418.093/total)
print(anes.gender) #52.2%

anes.income <- wtd.quantile(anes$income, weight = anes$weights, na.rm = TRUE)
print(anes.income) #45-50K

anes.education <- wtd.quantile(anes$educ, weight = anes$weights, na.rm = TRUE)
print(anes.education) #Some college, but no degree

anes.married <- wtd.table(anes$marital, weights = anes$weights)
print(anes.married)
total <- 1148.06 + 1580.11
anes.married <- 1580.11/total
print(anes.married) #58% married

anes.children <- wtd.table(anes$children, weights = anes$weights)
print(anes.children)
total <- 1822.28 + 904.89
anes.children <- 904.89/total
print(anes.children) #33% children

anes.attend <- wtd.mean(anes$attend, na.rm = TRUE)
print(anes.attend) #2.50

anes.bornagain1 <- wtd.table(dem$bornagain, weights = dem$weights)
print(anes.bornagain1)
total1 <- 976.5686 + 72.9846
anes.bornagain1 <- (72.9846/total1)*40
anes.bornagain2 <- wtd.table(rep$bornagain, weights = rep$weights)
print(anes.bornagain2)
total2 <- 971.4717 + 346.3693
anes.bornagain2 <- (346.3693/total2)*40
anes.bornagain3 <- wtd.table(ind$bornagain, weights = ind$weights)
print(anes.bornagain3)
total3 <- 312.0216 + 48.7536
anes.bornagain3 <- (48.7536/total3)*20
anes.bornagain <- anes.bornagain1 + anes.bornagain2 + anes.bornagain3
print(anes.bornagain) #52.33

anes.pid <- wtd.mean(anes$pid, weights = anes$weights, na.rm = TRUE)
print(anes.pid) #4.22

anes.ideology <- wtd.mean(anes$ideology, weights = anes$weights, na.rm = TRUE)
print(anes.ideology) #4.23

#####SSI BALANCE TESTING (FOOTNOTE 9, OA-B TABLE B1 )#####

#Create subset for analysis
ssi.balsub <- subset(ssi, select = c(treatname, treatment, age, female, married, children, education, income, pid7, ideology, attend, bornagain, natid))

#Prepare data for balance tesing
ssi.balsub$bintreat <- ifelse(ssi.balsub$treatment == "", "", ifelse(ssi.balsub$treatment == 1, 0, 1))

ssi.contreppat <- subset(ssi.balsub, treatment == 1 | treatment == 3) #Born again different
ssi.contdempat <- subset(ssi.balsub, treatment == 1 | treatment == 2) #Ideology different

ssi.contreppat <- droplevels.data.frame(ssi.contreppat)
ssi.contdempat <- droplevels.data.frame(ssi.contdempat)

#Perform balance tests
ssi.balcontreppat <- MatchBalance(bintreat ~ age + female + income + education + bornagain + attend + ideology + pid7 + married + children + natid, data = ssi.contreppat, nboots = 1000, na.omit) #Ignore warnings
ssi.balcontdempat <- MatchBalance(bintreat ~ age + female + income + education + bornagain + attend + ideology + pid7 + married + children + natid, data = ssi.contdempat, nboots = 1000, na.omit) #Ignore warnings


#####MTURK BALANCE TESTING (FOOTNOTE 9, OA-B TABLE B2)#####

#Create subset for analysis
mturk.balsub <- subset(mturk, select = c(treatname, treatment, age, female, married, children, education, income, pid7, ideology, attend, bornagain, natid, rrscale))

#Prepare data for balance testing
mturk.balsub$bintreat <- ifelse(mturk.balsub$treatment == "", "", ifelse(mturk.balsub$treatment == 1, 0, 1))

#Create treatment subsets for balance testing 
mturk.contdempat <- subset(mturk.balsub, treatment == 1 | treatment == 2) 
mturk.contdemrace <- subset(mturk.balsub, treatment == 1 | treatment == 3)
mturk.contdemboth <- subset(mturk.balsub, treatment == 1 | treatment == 4) 
mturk.contreppat <- subset(mturk.balsub, treatment == 1 | treatment == 5) 
mturk.contreprace <- subset(mturk.balsub, treatment == 1 | treatment == 6) 
mturk.contrepboth <- subset(mturk.balsub, treatment == 1 | treatment == 7)

mturk.contdempat <- droplevels.data.frame(mturk.contdempat)
mturk.contdemrace <- droplevels.data.frame(mturk.contdemrace)
mturk.contdemboth <- droplevels.data.frame(mturk.contdemboth)
mturk.contreppat <- droplevels.data.frame(mturk.contreppat)
mturk.contreprace <- droplevels.data.frame(mturk.contreprace)
mturk.contrepboth <- droplevels.data.frame(mturk.contrepboth)

#Perform balance tests
mturk.balcontdempat <- MatchBalance(bintreat ~ age + female + income + education + bornagain + attend + ideology + pid7 + married + children  + natid + rrscale, data = mturk.contdempat, nboots = 1000, na.omit)
mturk.balcontdemrace <- MatchBalance(bintreat ~ age + female + income + education + bornagain + attend + ideology + pid7 + married + children + natid + rrscale, data = mturk.contdemrace, nboots = 1000, na.omit)
mturk.balcontdemboth <- MatchBalance(bintreat ~ age + female + income + education + bornagain + attend + ideology + pid7 + married + children  + natid + rrscale, data = mturk.contdemboth, nboots = 1000, na.omit)
mturk.balcontreppat <- MatchBalance(bintreat ~ age + female + income + education + bornagain + attend + ideology + pid7 + married + children + natid + rrscale, data = mturk.contreppat, nboots = 1000, na.omit)
mturk.balcontreprace <- MatchBalance(bintreat ~ age + female + income + education + bornagain + attend + ideology + pid7 + married + children + natid + rrscale, data = mturk.contreprace, nboots = 1000, na.omit) 
mturk.balcontrepboth <- MatchBalance(bintreat ~ age + female + income + education + bornagain + attend + ideology + pid7 + married + children + natid + rrscale, data = mturk.contrepboth, nboots = 1000, na.omit)


#####VARIABLES THAT CORRELATE WITH NATIONAL IDENTITY (FOOTNOTE 7)#####

#SSI
ssi.natcorr1 <- cor.test(ssi$natid, ssi$pid7) #Examine corrleation between national identity items
print(ssi.natcorr1)

ssi.natcorr2 <- cor.test(ssi$natid, ssi$ideology)
print(ssi.natcorr2)



#MTURK

mturk.natcorr1 <- cor.test(mturk$natid, mturk$pid7) #Examine corrleation between national identity items
print(mturk.natcorr1)

mturk.natcorr2 <- cor.test(mturk$natid, mturk$ideology)
print(mturk.natcorr2)

mturk.natcorr3 <- cor.test(mturk$natid, mturk$rrscale)
print(mturk.natcorr3)






#####DISTRIBUTION OF NATIONAL IDENTITY (OA-C)#####

#Review distribution of the national identity scale, overall and by partisanship
ssi.dem <- subset(ssi, pid3 == 1)
ssi.rep <- subset(ssi, pid3 == 3)
ssi.ind <- subset(ssi, pid3 == 2)

mturk.dem <- subset(mturk, pid3 == 1)
mturk.rep <- subset(mturk, pid3 == 3)
mturk.ind <- subset(mturk, pid3 == 2)



d1 <- density(ssi$natid, na.rm = TRUE, adjust = 1.45)
d2 <- density(ssi.dem$natid, na.rm = TRUE, adjust = 1.45)
d3 <- density(ssi.rep$natid,na.rm = TRUE, adjust = 1.45)
d4 <- density(ssi.ind$natid,na.rm = TRUE, adjust = 1.45)

d5 <- density(mturk$natid, na.rm = TRUE, adjust = 1.45)
d6 <- density(mturk.dem$natid, na.rm = TRUE, adjust = 1.45)
d7 <- density(mturk.rep$natid, na.rm = TRUE, adjust = 3)
d8 <- density(mturk.ind$natid, na.rm = TRUE, adjust = 1.45)

par(family = "serif")
par(mfrow = c(2, 1))
par(mar=c(3.1,4.1,4.1,2.1), oma = c(4, 0, 0,0))

plot(d1, lwd = 2, ylim = c(0, .4), xlim = c(2, 10), main = "SSI Sample (2015)", xlab = "", cex.main = .95, cex.lab = .9, cex.axis = .9)
lines(d2, lty = 6)
lines(d3, lty = 2)
lines(d4, lty = 3)

plot(d5, lwd = 2, ylim = c(0, .4), xlim = c(2, 10), main = "SSI Sample (2015)", xlab = "", cex.main = .95, cex.lab = .9, cex.axis = .9)
lines(d6, lty = 6)
lines(d7, lty = 2)
lines(d8, lty = 3)

legend(3.5, -.15, lwd = c(2, 1, 1, 1), lty = c(1, 2, 6, 3), c("All Respondents", "Republicans", "Democrats", "Independents"), xpd = NA, ncol = 2, cex = .9)








######SSI VOTE CHOICE FOR PARTISANS (OA-F)######

#Organize data
ssi$voter <- as.factor(ssi$voter)

ssi.replow <- subset(ssi, pid3 == 3 & natidc <= 2)
ssi.rephigh <- subset(ssi, pid3 == 3 & natidc >= 3)
ssi.demlow <- subset(ssi, pid3 == 1 & natidc <=2)
ssi.demhigh <- subset(ssi, pid3 == 1 & natidc >= 3)

#Run analyses
replow <- clm(voter ~ treatname, data = ssi.replow, link = "logit")
summary(replow)
nobs(replow)


rephigh <- clm(voter ~ treatname, data = ssi.rephigh, link = "logit")
summary(rephigh)
nobs(rephigh)



demlow <- clm(voter ~ treatname, data = ssi.demlow, link = "logit")
summary(demlow)
nobs(demlow)


demhigh <- clm(voter ~ treatname, data = ssi.demhigh, link = "logit")
summary(demhigh)
nobs(demhigh)




#####VOTE CHOICE IDEOLOGY ROBUSTNESS CHECK (OA-G)#####

ssi$voter <- as.factor(ssi$voter) #Convert variables to factors

test1 <- clm(voter ~ treatname*ideology, data = ssi, link = "logit")
summary(test1)
nobs(test1)

test2 <- clm(voter ~ treatname*natidc + treatname*ideology, data = ssi, link = "logit")
summary(test2)
nobs(test2)

test3 <- clm(voter ~ treatname*natidc*ideology, data = ssi, link = "logit")
summary(test3)
nobs(test3)













#####VOTE CHOICE POSTESTIMATION MODELS (OA-H)#####

ssi$voter3 <- as.factor(ssi$voter3) #Convert variables to factors

test1 <- clm(voter3 ~ treatname*natidc + treatname*pid7, data = ssi, link = "logit")
summary(test1)
nobs(test1)



mturk$voter3 <- as.factor(mturk$voter3) #Convert variables to factors
mturk.rep <- subset(mturk, treatname == "Control" | treatname == "RepPat" | treatname == "RepRace" | treatname ==  "RepBoth")

test2 <- clm(voter3 ~ treatname*natidc + treatname*rrscale + treatname*pid7, data = mturk.rep, link = "logit")
summary(test2)
nobs(test2)




mturk$voter3 <- as.factor(mturk$voter3) #Convert variables to factors
mturk.dem <- subset(mturk, treatname == "Control" | treatname == "DemPat" | treatname == "DemRace" | treatname ==  "DemBoth")

test3 <- clm(voter3 ~ treatname*natidc + treatname*rrscale + treatname*pid7, data = mturk.dem, link = "logit")
summary(test3)
nobs(test3)











#####MTURK VOTE CHOICE REGRESSION COMPARISIONS#####

mturk$voter <- as.factor(mturk$voter) #Convert variables to factors
mturk$voted <- as.factor(mturk$voted)
mturk$treatment <- as.factor(mturk$treatment)

#Analyses for Republican Candidate
mturk.rep1 <- subset(mturk, treatname == "RepPat" | treatname ==  "RepBoth")
mturk.rep2 <- subset(mturk, treatname == "RepRace" | treatname ==  "RepBoth")

test1 <- clm(voter ~ treatment, data = mturk.rep1, link = "logit")
summary(test1)
nobs(test1)

test2 <- clm(voter ~ treatment*natidc + treatment*rrscale + treatment*pid7, data = mturk.rep1, link = "logit")
summary(test2)
nobs(test2)

test3 <- clm(voter ~ treatment, data = mturk.rep2, link = "logit")
summary(test3)
nobs(test3)

test4 <- clm(voter ~ treatment*natidc + treatment*rrscale + treatment*pid7, data = mturk.rep2, link = "logit")
summary(test4)
nobs(test4)




mturk.dem1 <- subset(mturk, treatname == "DemPat" | treatname ==  "DemBoth")
mturk.dem2 <- subset(mturk, treatname == "DemRace" | treatname ==  "DemBoth")

test1 <- clm(voted ~ treatment, data = mturk.dem1, link = "logit")
summary(test1)
nobs(test1)

test2 <- clm(voted ~ treatment*natidc + treatment*rrscale + treatment*pid7, data = mturk.dem1, link = "logit")
summary(test2)
nobs(test2)

test3 <- clm(voted ~ treatment, data = mturk.dem2, link = "logit")
summary(test3)
nobs(test3)

test4 <- clm(voted ~ treatment*natidc + treatment*rrscale + treatment*pid7, data = mturk.dem2, link = "logit")
summary(test4)
nobs(test4)
