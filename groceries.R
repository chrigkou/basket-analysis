
groceries.src<-read.csv("GroceriesInitial.csv",header=TRUE,sep=",", na.strings=c("","NA"))
library(ggplot2)
p<-ggplot(groceries.src,aes(x=1,y=basket_value,fill=1))+geom_boxplot()
outlier_values<-boxplot.stats(groceries.src$basket_value)$out# οι τιμές των out-lier


#discretize the basket value
library(infotheo)
value<-groceries.src$basket_value
hist(value, breaks = 100, main = "Data")
library(Hmisc)
groceries.src$basket_values<-groceries.src$basket_value
groceries.src$basket_value<-cut2(groceries.src$basket_value,g = 3)
#name the categories
levels(groceries.src$basket_value)<-c("_low","_medium","_high") 

#put all items from a row in a list
groceries.src$Item <-gsub(',NA','',with(groceries.src,paste(groceries.src$Item_1,groceries.src$Item_2,groceries.src$Item_3,groceries.src$Item_4,groceries.src$Item_5,groceries.src$Item_6,groceries.src$Item_7,groceries.src$Item_8,groceries.src$Item_9,groceries.src$Item_10,groceries.src$Item_11,groceries.src$Item_12,groceries.src$Item_13,groceries.src$Item_14,groceries.src$Item_15,groceries.src$Item_16,groceries.src$Item_17,groceries.src$Item_18, groceries.src$Item_19,groceries.src$Item_20,groceries.src$Item_21,groceries.src$Item_22,groceries.src$Item_23,groceries.src$Item_24,groceries.src$Item_25,groceries.src$Item_26,groceries.src$Item_26,groceries.src$Item_27,groceries.src$Item_28,groceries.src$Item_29,groceries.src$Item_30,groceries.src$Item_31,groceries.src$Item_32,sep = ",")))

for (i in 1:32)
{
  name<-paste(c("Item", i), collapse= "_")
  groceries.src[,name]<-NULL
}

#select some products
cho <- list("citrus fruit","tropical fruit","whole milk","other vegetables","rolls/buns","chocolate","bottled water","yogurt","sausage","root vegetables","pastry","soda","cream")

for (food in cho){
  
  temp <- list()
  for (i in groceries.src$Item){
    temp <- rbind(temp, grepl(food, i))
  }
  groceries.src <- cbind(groceries.src,unlist(temp))
  names(groceries.src)[names(groceries.src) == "unlist(temp)"] <- food
  
}
#convert basket value to binary form
groceries.mod<-cbind(groceries.src, model.matrix( ~ 0 +basket_value, groceries.src))
groceries.mod$Item<-NULL
groceries.mod$basket_value<-NULL
groceries.mod$basket_value_high<-as.logical(groceries.mod$basket_value_high)
groceries.mod$basket_value_low<-as.logical(groceries.mod$basket_value_low)
groceries.mod$basket_value_medium<-as.logical(groceries.mod$basket_value_medium)


#View(groceries.mod)
write.csv(groceries.mod,"GroceriesProcessed.csv", row.names = TRUE)



#find rules that describe the baskets
require(arules)
groceries.pro<-read.csv("GroceriesProcessed.csv",header=TRUE,sep=",")
groceries.pro$X<-NULL

#rules without basket value
trans2 <- as(groceries.pro[,4:16], "transactions") 
#inspect(trans2)
itemFrequencyPlot(trans2, topN=20)
rules <- apriori(trans2, parameter = list(supp = 0.05, conf = 0.05,target="rules"))

#rules with basket value
trans <- as(groceries.pro[,4:19], "transactions") 
#inspect(trans)
itemFrequencyPlot(trans, topN=20)
rules <- apriori(trans, parameter = list(supp = 0.05, conf = 0.05,target="rules"))

# Show the top 5 rules, but only 2 digits
options(digits=2)
inspect(rules[1:4])

inspect(rules[1:20])

df_basket <- as(rules,"data.frame")
View(df_basket)


#groceries.pro <- scale(groceries.pro) # standardize variables 
fit <- kmeans(groceries.pro[,2:3], 5) # 5 cluster solution
# get cluster means
aggregate(groceries.pro[,2:3],by=list(fit$cluster),FUN=mean)


fit$centers  #means
fit$size    #size of each cluster
plot(groceries.pro[,2:3], col=fit$cluster ,main="Affiliation of observations") 


groceries.pro <- data.frame(groceries.pro, fit$cluster) 
groceries.pro$fit.cluster<-as.factor(groceries.pro$fit.cluster)
groceries.pro<-cbind(groceries.pro, model.matrix( ~ 0 +fit.cluster, groceries.pro))

for (i in 1:5){
  name<-paste(c("fit.cluster", i), collapse= "")
  groceries.pro[,name]<-as.logical(groceries.pro[,name])
  
}
groceries.pro$fit.cluster<-NULL


cols <- c(4:16, 20:24)
trans3 <- as(groceries.pro[,cols], "transactions") 
inspect(trans3)
itemFrequencyPlot(trans3, topN=20)
rules <- apriori(trans3, parameter = list(supp = 0.05, conf = 0.01,target="rules"))
df_basket <- as(rules,"data.frame")

