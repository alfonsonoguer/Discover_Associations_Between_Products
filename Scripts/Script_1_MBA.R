# Library -----------------------------------------------------------------
pacman::p_load(dplyr,tidyr,plyr,ggplot2,caret,
               RColorBrewer,corrplot,GGally,reshape, viridis,plotly,
               arules, reshape2)

# Data --------------------------------------------------------------------

items <- read.csv(file = "Data/lineitems.csv", sep = ";")

# items: contains the information of all the products for each order:
# individual id inside every order id, the order id, product quantity for every
# product in each order id, the Stock Keeping Unit or sku, the unit price and 
# the date that the product has been added into the order id.

orders <- read.csv("Data/orders_translated.csv", sep = ";")

# orders: contains the information from any transaction that has been made
# in the online shop: order id, the time that has been created, how much to
# customer paid and the "its" state. 

transactions <- read.csv("Data/trans.csv", header = FALSE, na.strings=c("","NA"))

# transactions: represents all the transactions that has been made between
# 2017-01-01 00:07:19 and 2018-03-14 13:58:36. 
# To be sure their are going to be insightful for our analysis, 
# we asked to our client to provide the ones that has equal or more than
# 2 products.


# Coments -----------------------------------------------------------------

#  by analizing the data, and speaking with the client, the only transactions
# that we care about are the completed ones, so that filters down the data,
# a lot.



# Prepocess ---------------------------------------------------------------


orders_completed<- orders[orders$state == "Completed",] 

# now we filter down the items to the completed orders.

itemsorders2plus <- items %>% group_by(id_order) %>% filter(n()>1)

itema2plusinorderscompl <- itemsorders2plus[itemsorders2plus$id_order %in% 
                                              orders_completed$id_order,]

tospread <- select(itema2plusinorderscompl, id_order, sku)
tospread$number <-NA
numberofitems <- tospread %>%
  group_by(id_order) %>%
  filter(n()>1) %>%
  summarise(n=n())
numberofitems <- as.data.frame(numberofitems)
for(i in numberofitems$id_order){
  # which(tospread %>% filter( id_order == 246018) )
  # hit a block
  tospread[tospread$id_order == i, "number"] <- 
    c(1:numberofitems[which(numberofitems$id_order == i),2])
  
}
partitiontospread

test <- dcast(data = tospread, id_order + value.number ~ sku)


items_of_completed_orders <- items[items$id_order %in% orders_completed$id_order,]

# and we only want orders with 2 or more products
items_of_completed_orders_2ormore<-items_of_completed_orders[duplicated(
  items_of_completed_orders$id_order), ]

length(unique(items_of_completed_orders_2ormore$id_order))


orders_two_or_more <- items %>%
  group_by(id_order) %>%
  filter(n() > 1 )

test <- merge.data.frame(items_of_completed_orders_2ormore, )

temp <- items %>%
              group_by(id_order) %>%
              filter(n() > 1 )

nrow(items %>%
              group_by(id_order))

length(unique(items$id_order))

orders_completed <- orders %>% filter(state=="Completed")

length(which(orders_completed$id_order %in% orders_two_or_more$id_order))

