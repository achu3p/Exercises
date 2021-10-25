library(data.table)
library(ggplot2)
library(tidyverse)
library(knitr)
#library(stringr)
#library(DT)

#read data
aisles <- read_csv("C:/Users/achu3/Documents/instacart-market-basket-analysis/aisles.csv/aisles.csv")
departments <- read_csv("C:/Users/achu3/Documents/instacart-market-basket-analysis/departments.csv/departments.csv")
orders <- read_csv("C:/Users/achu3/Documents/instacart-market-basket-analysis/orders.csv/orders.csv")
products <- read_csv("C:/Users/achu3/Documents/instacart-market-basket-analysis/products.csv/products.csv")
order_products_train <- read_csv("C:/Users/achu3/Documents/instacart-market-basket-analysis/order_products__train.csv/order_products__train.csv")
order_products_prior <- read_csv("C:/Users/achu3/Documents/instacart-market-basket-analysis/order_products__prior.csv/order_products__prior.csv")

#data preview
head(aisles)
head(departments)
head(products)
head(orders) #glimpse(orders)
head(order_products_train) #1 = reorder; 0 = not
#select(mutate(order_products_train,reordered_yn=ifelse(reordered==1,"yes","no")),order_id,reordered,reordered_yn)
head(order_products_prior) #^same structure

#missing data
#what variables should be factors, if any?
#merge product, aisle, dept




#num of unique customers
#num of orders avg by ea customer

#When do people order? day, time
##time
orders%>%ggplot(aes(x=order_hour_of_day))+
  geom_histogram(stat="count")
##day - no info on days of week = what number
orders%>%ggplot(aes(x=order_dow))+
  geom_histogram(stat="count")

#When do people reorder?
orders%>%ggplot(aes(x=days_since_prior_order))+
  geom_histogram(stat="count")

#how many prior orders? relation to reorder

#num items people order
order_products_train%>%
  group_by(order_id)%>%
  summarise(num_items=last(add_to_cart_order))%>%
  ggplot(aes(x=num_items))+
  geom_histogram(stat="count")
#ls(order_products_train)
#head(order_products_train)

#aisle/department locations
#unique products in aisle/dept (# of, type?), popular products in aisle/dept
#num of products in aisle/dept...more choices = more/less order?
#popular aisles, depts (bestselling)

#most ordered - relation to reorder

bestsell <- order_products_train%>%
  group_by(product_id)%>%
  summarise(bestcount=n())%>%
  top_n(10,wt=bestcount)%>%
  left_join(select(products,product_id,product_name),by="product_id")%>%
  arrange(desc(bestcount))
print(bestsell)
##

#how often reorder
freqreorder <- order_products_train%>%
  group_by(reordered)%>%
  summarise(count=n())%>%
  mutate(reordered=as.factor(reordered))%>%
  mutate(proportion=count/sum(count))
print(freqreorder)

freqreorder%>%
  ggplot(aes(x=reordered,y=count,fill=reordered))+
  geom_bar(stat="identity")

#most reordered - need proportion
bestresell <- order_products_train%>%
  group_by(product_id)%>%
  summarise(proportion_reordered=mean(reordered),n=n())%>%
  filter(n>=40)%>% #######n>=40 significant 
  top_n(10,wt=proportion_reordered)%>%
  left_join(products,by="product_id")%>%
  arrange(desc(proportion_reordered))
print(bestresell)

bestresell%>%
  ggplot(aes(x=reorder(product_name,-proportion_reordered),y=proportion_reordered))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.title.x=element_blank()+coord_cartesian(ylim=c(0,.95))) ##adjust coord

#frequency of order, reorder 
#distb of order count by user
#days to next order, next reorder

#time of last order vs probability of reorder
order_products_train%>%
  left_join(orders,by="order_id")%>%
  group_by(days_since_prior_order)%>%
  summarise(mean_reorder=mean(reordered))%>%
  ggplot(aes(x=days_since_prior_order,y=mean_reorder))+
  geom_bar(stat="identity")
##association btwn num of orders & prob of reorder

#product types - organic, for ex.
## % product organic v not
products2 <- products%>%
  mutate(organic=ifelse(str_detect(str_to_lower(products$product_name),'organic'),
                        "organic","not organic"),organic=as.factor(organic))

#t <- order_products_train%>%
#  left_join(products,by="product_id")%>%
#  group_by('organic')%>%
#  summarize(count=n())%>%
#  mutate(proportion=count/sum(count))
#print(t)
#kable(t)