### Linear Regression###


library(dplyr)
# Loading data 
salesraw <- read.csv(file = "/Users/dengyuxin/Downloads/summer-products-with-rating-and-performance_2020-08.csv")

# Step 1 Preparing the data
# Dropping unnecessary columns
salesraw1 <- select( salesraw, -c('title', 'title_orig','tags','urgency_text',
                                  'product_color','merchant_title','merchant_name',
                                  'merchant_info_subtitle','merchant_id','merchant_profile_picture',
                                  'product_url','product_picture','product_id','theme','crawl_month',
                                  "product_variation_size_id","product_variation_inventory",
                                  "shipping_option_name","has_urgency_banner","rating_five_count",
                                  "rating_four_count","rating_three_count","rating_two_count","rating_one_count"))
##Checking unique values in each column
unique(salesraw1$currency_buyer)
salesraw2 <- select( salesraw1, -c("currency_buyer"))
colnames(salesraw2)

# Changing data type
# Adjust data type (Int -> Factor)
salesraw2$uses_ad_boosts <- as.factor(salesraw2$uses_ad_boosts) 
salesraw2$badge_local_product <- as.factor(salesraw2$badge_local_product)
salesraw2$badge_product_quality <- as.factor(salesraw2$badge_product_quality)
salesraw2$badge_fast_shipping <- as.factor(salesraw2$badge_fast_shipping)
salesraw2$merchant_has_profile_picture <- as.factor(salesraw2$merchant_has_profile_picture)
# Adjust data type (Character -> Factor)
salesraw2$origin_country <- as.factor(salesraw2$origin_country)


# Missing Value
# Test Missing Value
colSums(is.na(salesraw2)) # no missing value
str(salesraw2)

# Add a new column
sales <- salesraw2                          
sales <- cbind(price_difference = salesraw2$retail_price-salesraw2$price, salesraw2) 
str(sales)


# Step 2 Linear Regression

# OLS Model
# Build the model
linreg_model<-lm(formula=units_sold~.,data=sales)
summary(linreg_model)

# Drop redundant variables
sales1 <- select(sales, -c('retail_price', 'badge_fast_shipping'))
linreg_model1<-lm(formula=units_sold~.,data=sales1)
summary(linreg_model1)
colnames(salesraw2)
plot(residuals(linreg_model1),main="Residuals")


# Detecting multicollinearity
car::vif(linreg_model1)

# Dealing with multicollinearity
linreg_model2 <- lm(units_sold ~. -badges_count, data = sales1)
summary(linreg_model2)
car::vif(linreg_model2)


# QQ Plot
par(mfrow=c(2,2)) 
plot(linreg_model2)

#
x <- cbind(price=sales1$price, retail_price=sales1$retail_price, uses_ad_boosts=sales1$uses_ad_boosts, 
           rating=sales1$rating, rating_count=sales1$rating_count,
           badge_fast_shipping=sales1$badge_fast_shipping,badge_local_product=sales1$badge_local_product,
           badge_product_quality=sales1$badge_product_quality, shipping_option_price=sales1$shipping_option_price,
           shipping_is_express=sales1$shipping_is_express,countries_shipped_to=sales1$countries_shipped_to,
           inventory_total=sales1$inventory_total, origin_country=sales1$origin_country,
           merchant_has_profile_picture=sales1$merchant_has_profile_picture,
           merchant_rating= sales1$merchant_rating,merchant_rating_count=sales1$merchant_rating_count)
head(x)
y <- sales1[,3]
head(y)


library(dplyr)
library(glmnet)
#install.packages("ggplot2")

#Regression coefficient curve
par(mfrow=c(1,1))
model_lasso <- glmnet(x, y, alpha = 1)
plot(model_lasso, xvar = "lambda", label = TRUE)

#Regression coefficient curve
model_lasso <- glmnet(x, y, alpha = 1)
plot(model_lasso, xvar = "norm", label = TRUE)

#Cross-validation
cv_fit <- cv.glmnet(x, y, alpha = 1, nlambda=800)
plot(cv_fit)

#Get the estimated beta matrix by using the value of lambda that minimizes CVM
cv_fit$lambda.min
sales_minlambda <- glmnet(x,y,alpha=1,lambda=cv_fit$lambda.min)
sales_minlambda$beta


#Get the estimated beta matrix by using the value of lambda that minimizes CVM
cv_fit$lambda.1se
sales_minlambda <- glmnet(x,y,alpha=1,lambda=cv_fit$lambda.1se)
sales_minlambda$beta



