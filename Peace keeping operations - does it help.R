##################
# 1: Exercise 9  #
################## 

# a)
pko_data <- read.table(file = "pko.tsv", sep = "\t", header = TRUE, stringsAsFactors = FALSE) 
summary(pko_data)

pko_data <- subset(pko_data, !is.na(GDP))

pko_data$Income <- factor(pko_data$Income, 
                          levels = c('Low', 'MiddleLow', "MiddleHigh", "High"), 
                          labels = c('Low', 'MiddleLow', "MiddleHigh", "High"))


# b) Here, GDP is the response variable, and perhaps we are taking the log one step too early. In 
# practice (and I did this) you would most likely (unless you are really convinced a log-transform)
# decide to do this based on the residual plot from one of the (e.g. first) estimated models. 
with(pko_data, hist(GDP, col = 'grey'))

pko_data$logGDP <- with(pko_data, log(GDP))
with(pko_data, hist(logGDP, col = 'grey'))

# How to add something meaningful on the axis of the histogram 
with(pko_data, hist(logGDP, col = 'grey', axes = FALSE, xlab = "GDP"))
axis(side = 2)
tmpGDP <- c(10, 100, 1000, 10000, 100000)
axis(side = 1, at = log(tmpGDP), labels = format(tmpGDP, scientific = FALSE))


# c) I have not shown you the `what.max (...)` command, but there are several other ways to find this.
# We will discuss the other questions in class.
pko_data[with(pko_data, which.max(logGDP)), ]

View(pko_data[with(pko_data, which(logGDP > log(30000))), ])


# d) From this, GDP seems to be quite stable for 1985-2000, but from 2005 to 2015 there appears to be 
# some increase. Do you think that there is something, in addition to time, that causes this?
pko_data$Year <- as.factor(pko_data$Year)
pko_data$Year

with(pko_data, boxplot(logGDP ~ Year))


# e) It seems that populous countries generally have higher GDP. Also, the plot of Population and logGDP 
# does not look that informative, however, maybe we are fooled by India and China?
par(mfrow = c(1, 2))
with(pko_data, plot(Population, logGDP))
with(pko_data, lines(smooth.spline(Population, logGDP, df = 7), col = 'red'))

pko_data$logPopulation <- log(pko_data$Populatio)
with(pko_data, plot(logPopulation, logGDP))
with(pko_data, lines(smooth.spline(logPopulation, logGDP, df = 7), col = 'red'))

# How to add something meaningful on the axis of the scatter plot 
par(mfrow = c(1, 2))
with(pko_data, plot(Population, logGDP, ylab = 'GDP', axes = FALSE))
box(bty = 'o')
axis(side = 1)
axis(side = 2, at = log(tmpGDP), labels = format(tmpGDP, scientific = FALSE))
with(pko_data, lines(smooth.spline(Population, logGDP, df = 7), col = 'red'))

pko_data$logPopulation <- log(pko_data$Populatio)
with(pko_data, plot(logPopulation, logGDP, axes = FALSE, xlab = 'Population', ylab = 'GDP', main = 'log-transformed variables'))
box(bty = 'o')
axis(side = 2, at = log(tmpGDP), labels = format(tmpGDP, scientific = FALSE))
tmpPop <- c(1000, 10000, 100000, 1000000)
axis(side = 1, at = log(tmpPop), labels = format(tmpPop, scientific = FALSE))
with(pko_data, lines(smooth.spline(logPopulation, logGDP, df = 7), col = 'red'))



# f) We will discuss this in more details in the class.
par(mfrow = c(1, 2))
with(pko_data, plot(Battledeaths, logGDP))
with(pko_data, lines(smooth.spline(Battledeaths, logGDP, df = 7), col = 'red'))

pko_data$logBattledeaths <- log(pko_data$Battledeaths + 1)
with(pko_data, plot(logBattledeaths, logGDP))
with(pko_data, lines(smooth.spline(logBattledeaths, logGDP, df = 7), col = 'red'))

pko_data$zeroBattledeaths <- 'No'
pko_data[with(pko_data, which(Battledeaths < 1)), 'zeroBattledeaths'] <- 'Yes'


# g) We will discuss this in class. 


# h) 1) The residual plot appear to indicate no systematic errors in the model, and most variables are
# significant, we will still keep Year in the model (since this is included to correct for time) 2)+3)
# A 10% increase in the number of battle deaths results in a (1 + 10/100)^(-0.11) \approx 0.99 change in 
# GDP. This is a small increase, perhaps it is more meaningful to talk about the effect of small conflict vs. 
# war, therefore, it may make more sense to look at the effect of a 10 or 100 times increase in the number 
# of battle deaths, i.e. (10)^beta_logBat \approx 0.77 or (100)^beta_logBat \approx 0.60. 4).  This might 
# have been more precisely formulated, and what I have in mind is that increasing a number, say X, by 10 times 
# is the same as multiplying X by 10. The expected effect of zero battledeaths (given everything else equal) 
# is given by exp(beta_zeroBat) \approx 2.32. This is a high number, check which countries are included here.
pko_lm_h <- with(pko_data, lm(logGDP ~ logPopulation + logBattledeaths + zeroBattledeaths + Year))
summary(pko_lm_h)

beta_logBat <- pko_lm_h$coefficients[3]
beta_zeroBat <- pko_lm_h$coefficients[4]
(10)^beta_logBat
(100)^beta_logBat
exp(beta_zeroBat)

par(mfrow = c(1, 1))
with(pko_lm_h, plot(fitted.values, residuals))


# i) It seems that the PKO mandate variable is more an indicator of how serious the war was, than the PKO 
# mandate itself. 1)--2) ok. 3) We do not need to include a zeroPKOBudget since this is already explained by 
# a PKO mandate of zero.
par(mfrow = c(1, 2))
with(pko_data, boxplot(logGDP ~ PKO, horizontal = TRUE))
with(pko_data, boxplot(logBattledeaths ~ PKO, horizontal = TRUE))

pko_data[with(pko_data, which(PKO == 1)), 'PKO'] <- 1 
pko_data[with(pko_data, which(PKO == 2)), 'PKO'] <- 1 
pko_data[with(pko_data, which(PKO == 3)), 'PKO'] <- 2
pko_data[with(pko_data, which(PKO == 4)), 'PKO'] <- 2
pko_data$PKO <- as.factor(pko_data$PKO)

par(mfrow = c(1, 2))
with(pko_data, plot(PKOBudget, logGDP))

pko_data$logPKOBudget <- log(pko_data$PKOBudget + 1)
with(pko_data, plot(logPKOBudget, logGDP))


# j) 1) The model improved, in terms of R^2_adj, and the residual plot looks fine. We will have a more 
# complete discussion in class, but that most dramatic change is in the effect and p-value of 
# zeroBattledeaths. 2) The expected effect on GDP from a 100 precent increase in the PKO budget 
# (1 + 100/100)^beta_logPKOBud \approx 1.16 3) A doubling is the same as a 100 precent increase. 4) Look at 
# the answer from i) 
pko_lm_j <- with(pko_data, lm(logGDP ~ logPopulation + logBattledeaths + zeroBattledeaths + logPKOBudget + PKO + Year))
summary(pko_lm_j)

beta_logPKOBud <- pko_lm_j$coefficients[5]
(1 + 100/100)^beta_logPKOBud
2^beta_logPKOBud

par(mfrow = c(1, 1))
with(pko_lm_j, plot(fitted.values, residuals))


# k)
pko_data_sub <- subset(pko_data, Income != 'High')
pko_data_sub <- subset(pko_data_sub, Income != 'Low')


# l) 1) No major differences, remember that we can not compare R^2_adj directly. Note that the effect of 
# logPopulation has changed quite a lot. 2) And by a doubling of the PKO budget, we get an expected change in 
# GDP of (2)^beta_logPKOBud \appeox 1.13; which is not that far from what we got in j)
pko_lm_l <- with(pko_data_sub, lm(logGDP ~ logPopulation + logPKOBudget + PKO + Year))
summary(pko_lm_l)

beta_logPKOBud <- pko_lm_l$coefficients[3]
(2)^beta_logPKOBud

with(pko_lm_l, plot(fitted.values, residuals))


# m) I guess there are several solutions here, this was my choice for a split Conflict > 2 and War > 1, there 
# is not much pattern in the box plots, therefore, some guesswork is required. Perhaps a more data driven approach 
# is to try different splits, and see what is "best", then it is important to avoid overfitting (more on this 
# in class). 
par(mfrow = c(1, 2))
with(pko_data_sub, boxplot(logGDP ~ Conflict))
with(pko_data_sub, boxplot(logGDP ~ War))

pko_data_sub$ConflictNew <- 'No'
pko_data_sub[with(pko_data_sub, which(Conflict > 2)), 'ConflictNew'] <- 'Yes'

pko_data_sub$WarNew <- 'No'
pko_data_sub[with(pko_data_sub, which(War > 1)), 'WarNew'] <- 'Yes'

pko_data_sub$ConflictNew <- as.factor(pko_data_sub$ConflictNew)
pko_data_sub$WarNew <- as.factor(pko_data_sub$WarNew)


# n)
summary(with(pko_data_sub, lm(logGDP ~ PKOBudget + Battledeaths + Conflict + ConflictNew + War + WarNew + Population + logPopulation + zeroBattledeaths + logBattledeaths + logPKOBudget + PKO + Year)))
summary(with(pko_data_sub, lm(logGDP ~ PKOBudget + Battledeaths + Conflict + ConflictNew + War + Population + logPopulation + zeroBattledeaths + logBattledeaths + logPKOBudget + PKO + Year)))
summary(with(pko_data_sub, lm(logGDP ~ PKOBudget + Battledeaths + Conflict + ConflictNew + War + Population + logPopulation + logBattledeaths + logPKOBudget + PKO + Year)))
summary(with(pko_data_sub, lm(logGDP ~ PKOBudget + Battledeaths + Conflict + ConflictNew + War + Population + logPopulation + logPKOBudget + PKO + Year)))
summary(with(pko_data_sub, lm(logGDP ~ PKOBudget + Battledeaths + Conflict + ConflictNew + Population + logPopulation + logPKOBudget + PKO + Year)))
summary(with(pko_data_sub, lm(logGDP ~ PKOBudget + Battledeaths + Conflict + Population + logPopulation + logPKOBudget + PKO + Year)))
summary(with(pko_data_sub, lm(logGDP ~ PKOBudget + Conflict + Population + logPopulation + logPKOBudget + PKO + Year)))
summary(with(pko_data_sub, lm(logGDP ~ Conflict + Population + logPopulation + logPKOBudget + PKO + Year)))
summary(with(pko_data_sub, lm(logGDP ~ Population + logPopulation + logPKOBudget + PKO + Year)))

# This is the model that I got, based on how you solved m) it is possible you got another answer. 
# We will discuss this more in the class. 
pko_lm_n <- with(pko_data_sub, lm(logGDP ~ Population + logPopulation + logPKOBudget + PKO + Year))

beta_logPKOBud <- pko_lm_n$coefficients[4]
(2)^beta_logPKOBud

par(mfrow = c(1, 1))
with(pko_lm_n, plot(fitted.values, residuals))

# o) If time was an common cause, what would you expect the plot to look like?
with(pko_data_sub, plot(logPKOBudget, logGDP, pch = 21, bg = Year, cex = 2)) 


# p)




