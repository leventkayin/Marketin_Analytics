library(data.table)
library(dplyr)
library(rpart)
library(tibble)
library(tidyr)
library(lubridate)

setwd('C:/Users/qw004/Desktop/MKT 680/mid-term project/Pernalonga')
products <- fread('product_table.csv')
trans <- fread('transaction_table.csv')


#Function for standardization
standardize <- function(x) {
  x <- (x-mean(x))/sd(x)
  return(x)
}


#Find out the kinds of products that Huggies produces
huggies <- products %>% filter(brand_desc == 'HUGGIES') %>% group_by(subcategory_id, category_id) %>% summarise(num = n())

others <- products %>% filter((subcategory_id == 93367 & category_id == 95720) | (subcategory_id == 93431 & category_id == 95732), brand_desc != 'HUGGIES')

all <- products %>% filter((subcategory_id == 93367 & category_id == 95720) | (subcategory_id == 93431 & category_id == 95732))


#Find all relevant transactions
trans_new <- trans %>% filter(prod_id %in% all$prod_id)


#Get all relevant features for clustering
trans_clust <- trans_new %>% select(starts_with('tran_prod'))

clust <- kmeans(trans_clust, centers = 6, iter.max = 20)

trans_fin <- trans_new %>% cbind(clust$cluster)
names(trans_fin)[13] <- 'cluster'


#Customer metrics
cust_val <- trans_new %>% group_by(cust_id) %>% summarise(val = sum(tran_prod_paid_amt))
cust_val
cust_amt <- trans_new %>% group_by(cust_id) %>% summarise(amt = sum(tran_prod_sale_qty))
cust_tier <- merge(trans_new, cust_amt, by = 'cust_id') %>% 
  merge(cust_val, by = 'cust_id') %>% 
  group_by(cust_id) %>% 
  mutate(tier = val/amt)


#cluster the customers based on their total value and average price of products they buy
cluster1_cust <- cust_tier %>% group_by(cust_id) %>%
  summarise(amt = mean(amt), val = mean(val), tier = mean(tier))
cluster1_cust <- apply(cluster1_cust[,-1], 2, FUN = standardize) %>% cbind(cluster1_cust[,1])

num <- 6
cluster1 <- cluster1_cust %>% kmeans(centers = num, iter.max = 20)

##############################################################################
remove(trans_new)
##############################################################################


#Merge the cluster number with the transaction data
trans_fin1 <- cbind(cluster1_cust, cluster1$cluster) %>% merge(trans_fin, by = 'cust_id')
names(trans_fin1)[5] <- 'cust_cluster'


#Compare all the clusters
this <- trans_fin1 %>% summary() 
this <- this[4,1:17]%>% as.data.frame()
for(i in 1:num){
  that <- trans_fin1 %>% filter(cust_cluster == i) %>% summary() 
  that <- that[4,1:17] %>% as.data.frame()
  this <- that[,1] %>% cbind(this)
  names(this)[1] <- i
}

trans_fin1$cust_cluster <- trans_fin1$cust_cluster %>% as.factor()
trans_fin1$store_id <- trans_fin1$store_id %>% as.factor()


#Building logistic regression
attach(trans_fin1) %>% suppressMessages()
LR <- lm(tran_prod_paid_amt ~ cust_cluster + amt + tier + val, data = trans_fin1)
pred <- predict(LR, trans_fin1)
LR %>% summary()
pred %>% summary()


for(i in unique(trans_fin1$cust_cluster)){
  LR1 <- lm(tran_prod_paid_amt ~ tran_prod_discount_amt, data = trans_fin1%>%filter(cust_cluster == i))
  cat('The cluster ', i, ' has a summary as follows: 
      ')
  print(LR1 %>% summary())
}
#From there we basically established that discount has everything to do with the customer's final paid amount. 

####################################################################################
remove(LR)
remove(LR1)
####################################################################################


#Further manipulation on customer churn
churn <- function(x) {
  y <- unique(x)
  if(length(y)>1){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

with_discount <- trans_fin1 %>% filter(tran_prod_discount_amt < 0)
this <- trans_fin1 %>% group_by(cust_id) %>% summarise(funs = churn(prod_id))
trans_fin2 <- trans_fin1 %>% merge(this, by = 'cust_id')

suppressMessages(attach(trans_fin2))
lm(funs ~ tran_prod_discount_amt, data = trans_fin2) %>% summary()


#Label all the transactions for the churn
trans_fin3 <- data.frame()
for(i in this$cust_id){
  m <- trans_fin2 %>% filter(cust_id == i) %>% arrange(tran_dt)
  m[1,18] = 0
  
  if(nrow(m) > 1){
    for(k in 1:(nrow(m)-1)){
      if(m[k,9] != m[k+1,9]){
        m[k+1,18] = 1
      }else{
        m[k+1,18] = 0
      }
    }
  }else{
    m[,18] = 0
  }
  
  trans_fin3 <- trans_fin3 %>% rbind(m)
}

glm(funs ~ tran_prod_discount_amt, family = 'binomial', data = trans_fin3) %>% summary()


#Now we incorporate the customers' buying habits into account
diaper_cust_buying <- trans %>% filter(cust_id %in% this$cust_id)
diaper_cust_buying_sim <- diaper_cust_buying %>% select(cust_id, prod_id, starts_with('tran')) %>% merge(products, by = 'prod_id')
diaper_cust_buying_sim <- diaper_cust_buying_sim %>% select(-subcategory_id,-sub_category_desc,-category_desc,-brand_desc,-category_desc_eng)

###########################################################################
remove(trans)
remove(diaper_cust_buying)
###########################################################################

with_other_prod <- diaper_cust_buying_sim %>% 
  mutate(count = 1) %>% 
  group_by(cust_id,category_id) %>% 
  summarise(count = n()) %>% 
  spread(category_id, count)

with_other_prod[is.na(with_other_prod)] <- 0


#Now merge this info with the transaction data
trans_fin4 <- trans_fin3 %>% full_join(with_other_prod, by = 'cust_id')
trans_fin4$tran_dt <- trans_fin4$tran_dt %>% ymd()
trans_fin4$cluster <- trans_fin4$cluster %>% as.factor()
trans_fin4$cust_id <- trans_fin4$cust_id %>% as.factor()
trans_fin4$tran_id <- trans_fin4$tran_id %>% as.factor()
trans_fin4$tran_dt <- trans_fin4$tran_dt %>% as.factor()
trans_fin4$store_id <- trans_fin4$store_id %>% as.factor()
trans_fin4$prod_unit <- trans_fin4$prod_unit %>% as.factor()
trans_fin4$prod_id <- trans_fin4$prod_id %>% as.factor()
trans_fin4$funs <- trans_fin4$funs %>% as.factor()
trans_fin4 <- trans_fin4 %>% select(-prod_unit)


#Split data into training set and test set
testind <- sample(seq(1,nrow(trans_fin4)), size = 0.25*nrow(trans_fin4))
test <- trans_fin4[testind,]
rest <- trans_fin4[-testind,]
valind <- sample(seq(1,nrow(rest)), size = 0.33333333333*nrow(rest))
val <- rest[valind,]
train <- rest[-valind,]



#The Naive Bayes model
library(e1071)

NB_train <- train %>% select(-ends_with('id')) 
NB_val <- val %>% select(-ends_with('id')) 
NB_test <- test  %>% select(-ends_with('id')) 
NB <- naiveBayes(funs ~ ., data = NB_train)
pred_NB <- predict(NB, NB_val)

#Logistic Regression
LR <- glm(funs ~ ., family = binomial, data = train)
pred_LR <- predict(LR, val)

#Decision tree
library(rpart)
DT <- rpart(funs ~ ., data = train)
pred_DT <- predict(DT, val)
for(i in 1:nrow(pred_DT)) {
  if(pred_DT[i,2] >= 0.5){
    pred_DT[i,2] = 1
  }else{
    pred_DT[i,2] = 0
  }
}
pred_DT <- pred_DT[,-1]


#Performance metrics
library(pROC)
library(caret)
confusionMatrix(pred_DT,val$funs)
F_meas(pred_NB,val$funs)


####################################################################################
new <- trans_fin4 %>% 
  select(cust_id,tran_prod_sale_amt,funs)

new1 <- new
new1$funs <- new1$funs %>% as.numeric()

new2 <- new1 %>%  group_by(cust_id) %>% 
  summarise(prod_sale = sum(tran_prod_sale_amt), funs_ = sum(funs)-n())

for(i in 1:nrow(new2)){
  if(new2[i,3] > 0){
    new2[i,3] = 1
  }else{
    new2[i,3] = 0
  }
}
####################################################################################

#Manipulation for pyspark
total_paid_each <- trans %>% group_by(cust_id, prod_id) %>% summarise(tot = sum(tran_prod_paid_amt))


####################################################################################
setwd('C:/Users/qw004/Desktop/MKT 680/mid-term project')
write.csv(trans_fin3, file = 'transactions.csv')
write.csv(trans_fin4, file = 'transactions_large.csv')
write.csv(val, file = 'validation.csv')
write.csv(train, file = 'training.csv')
write.csv(test, file = 'test.csv')
write.csv(total_paid_each, file = 'final1.csv')
####################################################################################


####################################################################################
#Post processing
final2 <- fread('mycsv.csv', header = TRUE)
final3 <- final2 %>% filter(prod_id%in%all$prod_id)
f <- final3 %>% separate(col, c('cust_id','rating'), ', ')
f2 <- f %>% separate(cust_id, c('null','cust_id'), '=')
f3 <- f2 %>% separate(rating, c('null2','rating'), '=')
f4 <- gsub(')', replacement = '', x = f3$rating)
f4 <- data.frame(rating = f4)
f5 <- f3 %>% select(-rating) %>% bind_cols(f4)
f6 <- f5 %>% select(-V1, -null, -null2)


#Isolate the Huggies products for targeted emailing
huggies_prod <- products %>% filter(brand_desc == 'HUGGIES')
final4 <- f6 %>% filter(prod_id%in%huggies_prod$prod_id)


#Exclude customers who have bought private label in the past
private_prod <- products %>% filter(brand_desc == 'PRIVATE LABEL')
private_cust <- trans_fin4 %>% filter(prod_id%in%private_prod$prod_id)
final4 <- final4 %>% filter(!cust_id %in% private_cust$cust_id)
product_ids <- final4$prod_id %>% unique()


#Slice the data up by product id into 7 parts
out1 = final4 %>% filter(prod_id == product_ids[1])
out2 = final4 %>% filter(prod_id == product_ids[2])
out3 = final4 %>% filter(prod_id == product_ids[3])
out4 = final4 %>% filter(prod_id == product_ids[4])
out5 = final4 %>% filter(prod_id == product_ids[5])
out6 = final4 %>% filter(prod_id == product_ids[6])
out7 = final4 %>% filter(prod_id == product_ids[7])


#Write them into csv files
write.csv(out1, file = 'out1.csv')
write.csv(out2, file = 'out2.csv')
write.csv(out3, file = 'out3.csv')
write.csv(out4, file = 'out4.csv')
write.csv(out5, file = 'out5.csv')
write.csv(out6, file = 'out6.csv')
write.csv(out7, file = 'out7.csv')