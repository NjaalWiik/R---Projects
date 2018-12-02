##################
# 1: Exercise 3  #
################## 

# b) Reading data into R.  
brain_data <- read.table(file = 'BrainSize.tsv', sep = '\t', header = TRUE)


# c) Remove rows (row 2 and 21) both of which had missing values (i.e. height and/or weight was equal to -1)
brain_data <- brain_data[-c(2, 21), ]


# d) Converts the values of height and weights
brain_data$HeightCm <- with(brain_data, round(2.54*Height))
brain_data$WeightKg <- with(brain_data, round(0.45*Weight, 1))


# e) Scatter plot and correlation. Note that since the values "in the middle" are missing, this may give 
# an artificial high correlation if interpreted as the correlation for the whole population. Regardless, 
# the correlation is very high, almost 0.95, indicating that all of these different IQ scores measure something 
# related to each other
par(mfrow = c(1, 2))
with(brain_data, plot(PIQ, FSIQ))
with(brain_data, plot(VIQ, FSIQ))

with(brain_data, cor(PIQ, FSIQ))
with(brain_data, cor(VIQ, FSIQ))


# f) 


# g) The histogram below is bimodal by design, this is not necessary a problem, but it is something that we should 
# keep in mind. Again, by design, the populations of males and females are (with respect to FSIQ) quite similar, 
# however, this is not the case for the MRI counts. The reason is that men in general are larger than women, and 
# that body size is positively correlated with brain size
with(brain_data, hist(FSIQ))

par(mfrow = c(1, 2))
with(brain_data, boxplot(FSIQ ~ Gender, horizontal = TRUE))
with(brain_data, boxplot(MRI ~ Gender, horizontal = TRUE))


# h) If you look at this plot only in relation to the y-axis, the distribution of the white and black points is 
# essentially what is summarised in the first box plot above. The distribution  of the points with respect to the 
# x-axis is what is summarised in the second box plot.
par(mfrow = c(1, 1))
with(brain_data, plot(MRI, FSIQ, pch = 21, bg = Gender == 'Male'))
legend('bottomright', legend = c('Male','Female'), pt.bg = c(1, 0), pch = 21)


# i) From the summary we have that \beta_0 \approx 14.24 and \hat \beta_1 \approx 0.00011
brain_lm <- with(brain_data, lm(FSIQ ~ MRI))
summary(brain_lm)


# j) The estimated line is in the middle and quite far from all data points, which is a bit disturbing if this is 
# a model that is meant to represent data.
abline(brain_lm, col = 'red')


# k) Imagine pair individuals, where the only difference is that one has a MIR count that is 100000 higher than the 
# other. Then, on average, the person with the largest brain will have a FSIQ that is 100000*0.00011 = 11 higher.


# l) Split and plot 
brain_data_low  <- subset(brain_data, FSIQ <  130)
brain_data_high <- subset(brain_data, FSIQ >= 130)

par(mfrow = c(1, 2))
with(brain_data_low, plot(MRI, FSIQ, main = 'Brain Size and Intelligence (low)'))
with(brain_data_high, plot(MRI, FSIQ, main = 'Brain Size and Intelligence (high)'))


# m) The average MRI and FSIQ in the two groups 
with(brain_data_low, c(mean(MRI), mean(FSIQ)))
with(brain_data_high, c(mean(MRI), mean(FSIQ)))


# n)

# Low FSIQ Group
# 1) Correlation 
with(brain_data_low, cor(MRI, FSIQ))

# 2) Linear model 
brain_low_lm <- with(brain_data_low, lm(FSIQ ~ MRI))

# 3) Plot 
with(brain_data_low, plot(MRI, FSIQ, pch = 21, bg = 'grey'))
abline(brain_low_lm, col = 'red')

# 4) Summary
summary(brain_low_lm)

# High FSIQ Group
# 1) Correlation 
with(brain_data_high, cor(MRI, FSIQ))

# 2) Linear model 
brain_high_lm <- with(brain_data_high, lm(FSIQ ~ MRI))

# 3) Plot 
with(brain_data_high, plot(MRI, FSIQ, pch = 21, bg = 'grey'))
abline(brain_high_lm, col = 'red')

# 4) Summary
summary(brain_high_lm)


# o) Now, in the low group, \hat \beta_1 x 100000 = 6.485e-05*100000 = 6.485 \approx 6, and in the high group, 
# \hat \beta_1 x 100000 = 2.751e-05*100000 = 2.751 \approx 3. If we trust the model, the effect of brain size, on average, 
# is more than twice as strong for the low FSIQ group. However, if the two individuals with an MRI of more than 9500000 are
# removed from the low group, the effect og MRI in the low group drops down to \hat \beta_1 x 100000 = 4.301e-05*100000 =  
# 4.301 \approx 4.0


# p) 


# q) This is not possible since we used the full scale intelligence score FSIQ to split data and build the model, which we 
# do not know for a new individual. Therefore, this model is useless for predicting (anything), however, we may use it to 
# explain the effect of brain size on intelligence. 


# r) Yes. 


# s) When calculating the score for \bar x, replace \hat \beta_0 by \bar y - \hat \beta_1 \bar x, this should give you the 
# result.  


# t) Height or weight? Both are highly correlated with MRI, but height looks slightly better; which is also seen from the 
# calculated sample correlation. 
with(brain_data, plot(HeightCm, MRI))
with(brain_data, cor(HeightCm, MRI))

with(brain_data, plot(WeightKg, MRI))
with(brain_data, cor(WeightKg, MRI))


# u) If you compare the R2 scores, with the models using MRI, both are worse (in terms of R2) at describing data. 

# Low FSIQ Group
# 1) Linear model 
brain_low_height_lm <- with(brain_data_low, lm(FSIQ ~ HeightCm))
summary(brain_low_height_lm)

# 2) Plot 
with(brain_data_low, plot(HeightCm, FSIQ, pch = 21, bg = 'grey'))
abline(brain_low_height_lm, col = 'red')

# High FSIQ Group
# 1) Linear model 
brain_high_height_lm <- with(brain_data_high, lm(FSIQ ~ HeightCm))
summary(brain_high_height_lm)

# 2) Plot 
with(brain_data_high, plot(HeightCm, FSIQ, pch = 21, bg = 'grey'))
abline(brain_high_height_lm, col = 'red')
