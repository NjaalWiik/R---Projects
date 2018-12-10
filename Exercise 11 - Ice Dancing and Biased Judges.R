###################
# 1: Exercise 11 #
################### 


# 0) 
dance_data  <- read.table(file = 'IceDance.tsv', sep = '\t', header = TRUE, stringsAsFactors = FALSE)
summary(dance_data)


# 1a) Preparation
dance_data$SameNat <- 'No'
dance_data[with(dance_data, which(JudgeNat == PairNat)), "SameNat"] <- "Yes"
dance_data$SameNat <- as.factor(dance_data$SameNat)

dance_data$Segment <- factor(dance_data$Segment, levels = c('SD', 'FD'), labels = c('SD', 'FD'))
dance_data$Favorite <- as.factor(dance_data$Favorite)
dance_data$Both <- as.factor(dance_data$Both)

dance_data$PairID <- as.factor(dance_data$PairID)
dance_data$JudgeID <- as.factor(dance_data$JudgeID)


# 1b) Exploration and summary 
summary(dance_data)

with(dance_data, boxplot(JudgeScore ~ Segment, horizontal = TRUE))
with(dance_data, boxplot(JudgeScore ~ Favorite, horizontal = TRUE))

dance_data_SD <- subset(dance_data, Segment == 'SD')
dance_data_FD <- subset(dance_data, Segment == 'FD')

par(mfrow = c(1, 2))
with(dance_data_SD, boxplot(JudgeScore ~ PairID))
with(dance_data_FD, boxplot(JudgeScore ~ PairID))

par(mfrow = c(1, 2))
with(dance_data_SD, boxplot(JudgeScore ~ JudgeID))
with(dance_data_FD, boxplot(JudgeScore ~ JudgeID))

# This is not that informative
par(mfrow = c(1, 1))
with(dance_data, boxplot(JudgeScore ~ Both, horizontal = TRUE))
with(dance_data, boxplot(JudgeScore ~ SameNat, horizontal = TRUE))

# However, this is quite revealing 
with(dance_data, table(JudgeRank, SameNat))
with(dance_data, boxplot(JudgeRank ~ SameNat, horizontal = TRUE))



# 2a) Baseline model 
dance_lm <- with(dance_data, lm(JudgeScore ~ SameNat + Segment))
summary(dance_lm)

par(mfrow = c(1, 2))
with(dance_lm, plot(fitted.values, residuals))
with(dance_lm, hist(residuals))



# 2b) Model development
dance_lm <- with(dance_data, lm(JudgeScore ~ SameNat + Segment + Favorite))
summary(dance_lm)

par(mfrow = c(1, 2))
with(dance_lm, plot(fitted.values, residuals))
with(dance_lm, hist(residuals))

# Including Favourite in the model is (most likely) not enough, should include PairID, to correct for potential omitted-variable bias
dance_lm <- with(dance_data, lm(JudgeScore ~ SameNat + Segment + PairID))
summary(dance_lm)

with(dance_lm, plot(fitted.values, residuals))
with(dance_lm, hist(residuals))

# Add colour in order to try to see the pattern in the residual plot
with(dance_lm, plot(fitted.values, residuals, col = dance_data$Segment))
with(dance_lm, hist(residuals))

# There is a pattern in the residual plot, this is not here for each segment, i.e.
dance_SD_lm <- with(dance_data_SD, lm(JudgeScore ~ SameNat + PairID))
summary(dance_SD_lm)
with(dance_SD_lm, plot(fitted.values, residuals))

dance_FD_lm <- with(dance_data_FD, lm(JudgeScore ~ SameNat + PairID))
summary(dance_FD_lm)
with(dance_FD_lm, plot(fitted.values, residuals))



# 2b) Try to make the group more homogeneous by removing those that did not 
# qualify for FD segment. 
dance_data_sub <- subset(dance_data, Both == 'Yes')

dance_lm_sub <- with(dance_data_sub, lm(JudgeScore ~ SameNat + Segment + PairID))
summary(dance_lm_sub)
with(dance_lm_sub, plot(fitted.values, residuals, col = dance_data_sub$Segment))



# 2c) and 3) Final Model, a log-transformation solves the problem, maybe because of 
# the difference in the length of the segments and how skaters can, or can not, choose 
# elements to be included; will discuss this more in class.
dance_lm_sub <- with(dance_data_sub, lm(log(JudgeScore) ~ SameNat + Segment + PairID))
summary(dance_lm_sub)
with(dance_lm_sub, plot(fitted.values, residuals))

exp(dance_lm_sub$coefficients[2])



# 4) Will removing the highest and lowest score make the system more robust?
dance_data_sub_truc <- subset(dance_data_sub, JudgeRank < 9) 
dance_data_sub_truc <- subset(dance_data_sub_truc, JudgeRank > 1) 

dance_data_lm_sub_truc <- with(dance_data_sub_truc, lm(log(JudgeScore) ~ SameNat + PairID + Segment))
print(summary(dance_data_lm_sub_truc))

exp(dance_data_lm_sub_truc$coefficients[2])

dance_data_sub_truc <- subset(dance_data_sub, JudgeRank < 8) 
dance_data_sub_truc <- subset(dance_data_sub_truc, JudgeRank > 2) 

dance_data_lm_sub_truc <- with(dance_data_sub_truc, lm(log(JudgeScore) ~ SameNat + PairID + Segment))
print(summary(dance_data_lm_sub_truc))

exp(dance_data_lm_sub_truc$coefficients[2])

