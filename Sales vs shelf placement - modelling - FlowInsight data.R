##################
# 1: Exercise 5  #
################## 

# a) Read data, cleaning and preprocessing From Exercise 4: 

# Reading data. 
flow_data <- read.table(file = 'FlowInsightsBrand.tsv', sep = '\t', stringsAsFactors = FALSE, header = TRUE)

# Remove one row with a missing value 
flow_data <- subset(flow_data, Sales > -1)

# Correct misspellings in the product names and  
flow_data[with(flow_data, which(Product == 'Shampo')), "Product"] <- "Shampoo"
flow_data[with(flow_data, which(Product == 'Shamoo')), "Product"] <- "Shampoo"
flow_data[with(flow_data, which(Product == 'Syling')), "Product"] <- "Styling"

# Remove one product with a missing product name
flow_data <- flow_data[-with(flow_data, which(Product == 'NULL')), ]

# Convert products names to factors. 
flow_product_name <- c('Shampoo', 'Conditioner', 'Soap', 'Styling')
flow_data[, 'Product'] <- with(flow_data, factor(x = Product, 
                                                 levels = flow_product_name, 
                                                 labels = flow_product_name))

# More preprocessing and removal of some `extreme` values
flow_data <-  subset(x = flow_data, subset = Facings < 4)
flow_data <-  subset(x = flow_data, subset = Facings > 1)
flow_data <-  subset(x = flow_data, subset = Price < 150)
flow_data <-  subset(x = flow_data, subset = Sales < 20)

# Remove Soap and Styling products from the dataset
flow_data <- subset(flow_data, Product != 'Soap')
flow_data <- subset(flow_data, Product != 'Styling')


# b) Construction of dummy variable.
flow_data$Below <- 1
flow_data[with(flow_data, which(X < 105)), 'Below'] <- 0
flow_data[with(flow_data, which(Y > 90)), 'Below'] <- 0


# c) Plot the shelf. If we succeed in c) then the only points we should see are those in the 
# lower right corner (i.e. below the styling segment)
with(flow_data, plot(X, Y, pch = 19, col = Below, xlim = c(0, 210), ylim = c(0, 192)))


# d) Here, we include Below in the model form Exercise 4. The model does not appear to improve 
# much, if we compare R^2 and R^2_adj, there is a small improvement, but not by much. Also, the 
# p-value for the coefficient for Below is quite large. If we compare to the other effects, it 
# is tempting to say that it is of practical importance, i.e. on average we expect to sell half 
# a product less if it is placed below the styling segment. 
flow_lm_d <- with(flow_data, lm(Sales ~ Y + Size + Price))
summary(flow_lm_d)

flow_lm_d <- with(flow_data, lm(Sales ~ Y + Size + Price + Below))
summary(flow_lm_d)


# e) Do the calculations with Below = 0 and Below = 1 and verify that you get those formulas. 


# f) Here, we convert a dummy variable to a factor (which is essentially the same thing) and 
# if done right, the only part that should change is that it now says BelowYes in the summary 
# of the lm-object. 
flow_data[, 'Below'] <- with(flow_data, factor(x = Below, levels = c(0, 1),   labels = c('No', 'Yes')))

flow_lm_f <- with(flow_data, lm(Sales ~ Y + Size + Price + Below))
summary(flow_lm_f)


# g) If we include Product into the model, then 1) R^2 and R^2_adj increases, 2) in the model 
# ProductConditioner is the average difference in sales between Shampoo (reference group) and 
# Conditioner, 3) it is of both practical and statistical importance, 4) now the effect Below 
# decreased and is now of statistical importance, 4) No (compare with the other models). 
flow_lm_g <- with(flow_data, lm(Sales ~ Y  + Size + Price + Below + Product))
summary(flow_lm_g)


# h) We will talk about this in class.


# i) This should be clear from the table.
with(flow_data, table(Brand))


# j) Rename and combine brands with few products.
flow_data[with(flow_data, which(Brand == 'LUSINE')), 'Brand'] <- 'FEW'
flow_data[with(flow_data, which(Brand == 'NEUTRAL')), 'Brand'] <- 'FEW'
flow_data[with(flow_data, which(Brand == 'SANEX')), 'Brand'] <- 'FEW'
with(flow_data, table(Brand))


# k) Convert Brand to factor, and then estimate the corresponding linear model. 
flow_data[, 'Brand'] <- with(flow_data, as.factor(Brand))

# 1) Yes, compare R_2 and R^2_adj, 2) all of these are compared to the reference group, 
# which is the brand missing from the summary (AUSSIE), 3) HEAD&SHOULDERS, SUNSILK and 
# maybe FEW, 4) we can not say (with sufficient amount of certainty) that these, on average, 
# sells differently than than AUSSIE, 5) the effect is perhaps driven by brand?
flow_lm_k <- with(flow_data, lm(Sales ~ Y + Size + Price + Product + Below + Brand))
summary(flow_lm_k)


# l)


# m) Model with a quadratic term, and the answer to both questions are no.
flow_lm_m <- with(flow_data, lm(Sales ~ Y + I(Y^2) + Size + Price + Product + Below + Brand))
summary(flow_lm_m)


# n) Residual plot, and it is still a funnel, and we still make the largest mistakes for the 
# highest predictions; this is not so good.
with(flow_lm_k, plot(fitted.values, residuals))


# o)


# p)

