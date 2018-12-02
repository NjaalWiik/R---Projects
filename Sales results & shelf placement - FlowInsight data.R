##################
# 1: Exercise 4  #
################## 

# c) Reading data. 
setwd("/Users/wiik/OneDrive/Njål J. Wiik/Utdanning/Handelshøyskolen BI/MSc in Business Analysis/Applied Data Analytics/Lecture 4")
flow_data <- read.table(file = 'FlowInsights.tsv', sep = '\t', stringsAsFactors = FALSE, header = TRUE)


# d) There should only be one missing data point, one row has sales equal to -1. Also, there are some 
# quite saml and large values, however, we will look more into this below.
summary(flow_data)
flow_data <- subset(flow_data, Sales > -1)


# e) Frist we will correct the typoes. There are several ways to do this, make sure you understand the 
# different components in the code below. 
with(flow_data, unique(Product))

flow_data[with(flow_data, which(Product == 'Shampo')), 'Product'] <- 'Shampoo'
flow_data[with(flow_data, which(Product == 'Shamoo')), 'Product'] <- 'Shampoo'
flow_data[with(flow_data, which(Product == 'Syling')), 'Product'] <- 'Styling'
with(flow_data, unique(Product))

# This removes one row with a missing product name. 
flow_data <- flow_data[-with(flow_data, which(Product == 'NULL')), ]
with(flow_data, unique(Product))


# f) This converts products names to factors. 
flow_product_name <- c('Shampoo', 'Conditioner', 'Soap', 'Styling')
flow_data[, 'Product'] <- with(flow_data, factor(x = Product, 
                                                 levels = flow_product_name, 
                                                 labels = flow_product_name))


# g) We will only keep rows with `Facings` < 4 and `Facings` > 1. I think that having 1 or more than 3 
# facings may be a sign that there is something special about the product. In general, we would like to 
# remove data/products/rows that does not belong to the 'normal' population. For example, very popular 
# products will not be affected by location neither. Note, do not remove too much.
par(mfrow = c(1, 1))
with(flow_data, hist(Facings))
flow_data <-  subset(x = flow_data, subset = Facings < 4)
flow_data <-  subset(x = flow_data, subset = Facings > 1)

# Does not seem to be anything special here.
with(flow_data, hist(Size))

# There are a few products with very high price. 
with(flow_data, hist(Price))
flow_data <-  subset(x = flow_data, subset = Price < 150)

# There are a some products with very high sales numbers.
with(flow_data, hist(Sales))
flow_data <-  subset(x = flow_data, subset = Sales < 20)


# h)
with(flow_data, plot(X, Y, pch = 19, col = Product, xlim = c(0, 210), ylim = c(0, 192)))
legend('topright', pch = 19, legend = c('Shampoo', 'Conditioner', 'Soap', 'Styling'), col = 1:4, cex = 0.60)
abline(v = 105)


# i) Removing all soap (too few) and styling products (all located in the same place). Also, removing all 
# products on the right half of the shelf since these (Shampoo and Conditioner) are hidden below Styling.
flow_data <- subset(flow_data, Product != 'Soap')
flow_data <- subset(flow_data, Product != 'Styling')
flow_data <- subset(flow_data, X < 105)


# j) Plot of the remaining products. The size of the points are related to the number of sales. There
# is a good spread (acorss the shelf) of high and low sales numbers. 
with(flow_data, plot(X, Y, pch = 19, cex = 0.2*Sales, xlim = c(0, 210), ylim = c(0, 192)))


# k) Scatter plots. Nothing special here (but we did not know this until we plotted).
par(mfrow = c(1, 3))
with(flow_data, plot(Size, Sales))
with(flow_data, plot(Price, Sales))
with(flow_data, plot(Y, Sales))

# l) Estimate the baseline model. At the bottom we expect to see \hat \beta_0 + \hat \beta_1 * 0 = 2.214180 
# + 0.011631*0 \approx 2.3 sales and at the top \hat \beta_0 + \hat \beta_1 * 192 = 2.246130 + 0.011631*192 
# = 4.5 sales. Note that these numbers will depend on the data that included (you may therefore get different
# numbers). 
flow_lm_baseline <- with(flow_data, lm(Sales ~ Y))
summary(flow_lm_baseline)


# m) Estimate the model with height and size. Note that you get the quartiles for all covariates from 
# the summary command. Also, look at R2 (or R2_adj) to see if the model improved. The effective range
# of Y is \hat \beta_1 * (Q1_Y, Q3_Y) = 0.015986*(45.70, 149.76) = (0.7, 2.4) and for Size it is 
# \hat \beta_2 * (Q1_Size, Q3_Size) = 355.676499*(0.008925, 0.012100) = (3.2, 4.3). Therefore, within the 
# range of the data, location can add up to 1.7 extra sales but size only 1.1. 
flow_lm_size <- with(flow_data, lm(Sales ~ Y + Size))
summary(flow_lm_size)
summary(flow_data)


# n) 


# o) The final model with and without X, compare p-values and R2_adj to decide. 
flow_lm_final <- with(flow_data, lm(Sales ~ Y + Size + Price + X))
summary(flow_lm_final)

flow_lm_final <- with(flow_data, lm(Sales ~ Y + Size + Price))
summary(flow_lm_final)


# p) The residual plot for the model in o). What is happening with the error when we predict the largest 
# sales numbers?
par(mfrow = c(1, 1))
with(flow_lm_final, plot(fitted.values, residuals))


# q)
# The line below creates a list/vector of the values 0, 1, 2, ..., 192
Y_seq <- 0:192

# In R, we can use the predict(...) function to make predictions, instead of doing this 
# by hand. In order for this to work, we need to store these points (those that we want 
# to predict) in a data frame with the same column names as the data frame used for data.
flow_pred <- data.frame(Y = Y_seq, Size = 0.01, Price = 55)

# The main arguments for the predict(...) function is the model `flow_lm_final`, the data 
# frame of where we want to make predictions `flow_pred`. In addition, we may add 
# interval = 'confidence' to get a confidence interval at the level `level = 0.95`.
flow_lm_final_pred <- predict(flow_lm_final, flow_pred, interval = 'confidence', level = 0.95)

# The matplot(...) function is a 'matrix' plot function. This is useful if we have values for 
# several lines that we want to plot together. Here, the result of the predict(...) function 
# above is a matrix with best guess and corresponding uncertainty (upper and lower confidence 
# levels) for that best guess (stored as three columns in a matrix).
par(mfrow = c(1, 1))
matplot(Y_seq, flow_lm_final_pred, xlab = 'Height', ylab ='Number of sales', type = 'l', col = 'black', lty = c(1, 2, 2))
legend('topleft', lty = c(1, 2), col = 'black', legend = c('Prediction', '95% confidence interval'))


# r)


# s) Yes, e.g. brand.


# t) If you trust the analysis, is the effect of location strong enough to be important? Remember that 
# this is only for one store over two weeks.


# u) Scatter plot with line 
with(flow_lm_final, plot(fitted.values, residuals))
abline(a = 0, b = -1)


# v) Estimating and plotting the Poisson regression model 
flow_pois      <- with(flow_data, glm(Sales ~ Y + Size + Price, family = 'poisson'))
flow_pois_pred <- predict(flow_pois, flow_pred, type = 'response')

matplot(Y_seq, flow_lm_final_pred, xlab = 'Height', ylab ='Number of sales', type = 'l', col = 'black', lty = c(1, 2, 2))
lines(Y_seq, flow_pois_pred, col = 'blue')
