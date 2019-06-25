# Library -----------------------------------------------------------------
pacman::p_load(dplyr,tidyr,plyr,ggplot2,caret,
               RColorBrewer,corrplot,reshape, viridis,plotly,
               arules, reshape2, arulesViz)

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
transactions <- read.csv("Data/trans.csv",header = FALSE, na.strings=c("","NA"))

transactions <- transactions[-1,]

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
# contamos las apariciones
for(i in numberofitems$id_order){
  # which(tospread %>% filter( id_order == 246018) )
  # hit a block
  tospread[tospread$id_order == i, "number"] <- 
    c(1:numberofitems[which(numberofitems$id_order == i),2])
  
}




names(tospread) <- c("id_order", "value", "clasifier")

hopethisworks <- dcast(data = tospread, id_order ~ clasifier)

names(hopethisworks)[-1] <- names(transactions)

# comentar con dani -------------------------------------------------------

transacionsconidorder<-merge(x = hopethisworks, y = transactions,
                             by = names(transactions))
nrow(hopethisworks)
nrow(transactions)
nrow(transacionsconidorder)
nrow(unique(transacionsconidorder))
nrow(unique(hopethisworks))
nrow(unique(transactions))

# continuar con mi vida Epic 2 -------------------------------------------------

trans2<-read.transactions("Data/trans2.csv",format = "basket" , sep = ",")

inspect(trans2)
length(trans2)
table(size(trans2))
LIST(trans2)
itemLabels(trans2)

itemFrequency(trans2)
itemFrequencyPlot(trans2, names = TRUE, topN = 20)
image(sample(trans2, 10000))
  
RulesName<- apriori (trans2, parameter = list(supp = 0.0005, conf = 0.5))
summary(RulesName)
inspect(sort(RulesName,by = "lift", 5))

sortedrulesbylift<- sort(RulesName,by = "lift")

inspect(sortedrulesbylift)[1:10,]

table(is.redundant(RulesName))

plot(RulesName)
plot(RulesName, engine = "interactive")
plot(RulesName, method = "two-key plot")

inspectDT(RulesName)

# Epic 3 categories -------------------------------------------------------


Sku_category<-read.csv("Data/sku_category.csv")
head(Sku_category)
trans3@itemInfo$category <- trans3@itemInfo$labels

table(trans3@itemInfo$category %in% Sku_category$sku)

head(trans3@itemInfo)
apply(trans3@itemInfo[,2],1:2, function(x){
  return (Sku_category[which(Sku_category[,1] %in% x),2])
  
}
)
Sku_category$manual_categories <- as.character(Sku_category$manual_categories)

trans3@itemInfo[-which(trans3@itemInfo$category %in% Sku_category$sku), ]<- NA
trans3@itemInfo <- na.omit(trans3@itemInfo)

for ( i in 1:nrow(trans3@itemInfo)){
  holder <-  which(Sku_category$sku %in% trans3@itemInfo$labels[i])
  trans3@itemInfo$category[i] <- Sku_category$manual_categories[holder]
}
head(trans3@itemInfo)
