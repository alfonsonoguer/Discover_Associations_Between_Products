# Library -----------------------------------------------------------------
pacman::p_load(dplyr,tidyr,plyr,ggplot2,caret,
               RColorBrewer,corrplot,reshape, viridis,plotly,
               arules, reshape2, arulesViz,shinythemes,shiny)

# Data --------------------------------------------------------------------

items <- read.csv(file = "Data/lineitems.csv", sep = ";")
{}
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

items$unit_price <- as.numeric(items$unit_price)

# now we filter down the items to the completed orders.

benefit <- items %>% group_by(sku) %>% mutate(product_quantity*unit_price)

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


# continuar con mi vida Epic 2 ------------------------------------------------

trans2<-read.transactions("Data/trans2.csv",format = "basket" , sep = ",")

inspect(trans2)
length(trans2)
table(size(trans2))
LIST(trans2)
itemLabels(trans2)

itemFrequency(trans2)
itemFrequencyPlot(trans2, names = TRUE, topN = 20)
image(sample(trans2, 10000))
  
RulesName<- apriori (trans2, parameter = list(supp = 0.0007, conf = 0.65))
summary(RulesName)
inspect(sort(RulesName,by = "count"))

sortedrulesbylift<- sort(RulesName,by = "lift")

inspect(sortedrulesbylift)[1:2,]

table(is.redundant(RulesName))

plot(RulesName)
plot(RulesName, engine = "interactive")
plot(RulesName, method = "two-key plot")

inspectDT(RulesName)

# Epic 3 categories -------------------------------------------------------

trans3 <- read.transactions("Data/trans2.csv",format = "basket" , sep = ",")

Sku_category<-read.csv("Data/sku_category.csv")
Sku_brand<-read.csv("Data/Brands.csv",sep = ";")
head(Sku_category)
head(Sku_brand)
Sku_brand<-unique(Sku_brand)

nrow(Sku_category)
nrow(Sku_brand)


skus <- merge(x = Sku_category, y = unique(Sku_brand), by= "sku",
              all.x = TRUE)

# c(1:5) %in% c(3:4)

table(skus$sku %in% trans3@itemInfo$labels)
skus <- skus[unique(skus$sku), ]
skus <- unite(skus,2:3,col = "category_brand",remove = FALSE)

testmerge <- merge(x = skus, y = trans3@itemInfo, by.x= "sku",by.y = "labels",
              all.y = TRUE)

# apply(testlevel, 2, function(x){levels(x)})
testmerge <- testmerge <- apply(testmerge, 2, as.character)
testmerge[is.na(testmerge)]<-"other"
apply(testmerge,2,as.factor)
testmerge <- as.data.frame(testmerge)
names(testmerge)[1] <- names(trans3@itemInfo)[1]



trans4 <- trans3

trans4@itemInfo<- testmerge


Rulesaggtrans4 <- apriori (aggregate(trans4, "category_brand"),
                       parameter = list(supp = 0.002, conf = 0.5))

Rules4agregated <- aggregate(RulesName4, "category_brand")
  
table(trans3@itemInfo$labels %in% skus$sku)
table(skus$sku %in% trans3@itemInfo$labels)
# trans3@itemInfo$category <- NA
# 
# holder <- 0

# for ( i in 1:nrow(trans3@itemInfo)){
#   holder <-  which(Sku_brand$sku %in% trans3@itemInfo$labels[i])
#   if(length(holder)==0){
#     holder <- "other"
#   }
#   trans3@itemInfo$category[i] <- Sku_category$manual_categories[holder]
# }


# trans3@itemInfo$labels <- as.factor(trans3@itemInfo$labels)


# which ( Sku_category$sku == "SPH0016" )

names(Sku_category)[1] <- "labels"
names(Sku_brand)[1] <- "labels"
# temporaryDF <- trans3@itemInfo
# nrow(Sku_brand)
nrow(trans3@itemInfo)
salidamerge <- merge(x = trans3@itemInfo,
                         y = unique(Sku_brand), by= "labels",all.x = TRUE,)

trans2@itemInfo[2103,]


df<- trans3@itemInfo

df$labels <- substr(df$labels, 0, 3)
nrow(df)

trans3@itemInfo <- df
nrow(trans3@itemInfo) - nrow(trans2@itemInfo)

# othercategories <- as.factor("Other brands")
 nrow(trans3@itemInfo)

NAposition <- which(is.na(trans3@itemInfo$brand))
na.omit(trans3@itemInfo)
DF <- trans3@itemInfo[NAposition, ] 
DF[,2] <- "Other brands"  

head(trans3@itemInfo)
# failed miserably
# apply(trans3@itemInfo[,2],1:2, function(x){
#   return (Sku_category[which(Sku_category[,1] %in% x),2])
#   
# }
# )
# Sku_category$manual_categories<-as.character(Sku_category$manual_categories)

# trans3@itemInfo[-which(trans3@itemInfo$category %in%
#                 Sku_category$sku), ]<- NA
# trans3@itemInfo <- na.omit(trans3@itemInfo)

# for ( i in 1:nrow(trans3@itemInfo)){
#   holder <-  which(Sku_category$sku %in% trans3@itemInfo$labels[i])
#   if(length(holder)==0){
#     holder <- "other"
#     }
#   trans3@itemInfo$category[i] <- Sku_category$manual_categories[holder]
# }
# head(trans3@itemInfo)

trans3agregated <- aggregate(trans3,by = "labels")

RulesName2 <- apriori (trans3,
                       parameter = list(supp = 0.0009, conf = 0.5, minlen = 2))

Rules2agregated <- apriori (trans3agregated,
                       parameter = list(supp = 0.001, conf = 0.5, minlen = 2))


subsetedrules <- subset(Rules2agregated, rhs %in% "APP")
sortedrules <- sort(subsetedrules, decreasing = TRUE,
                    na.last = NA, by = "lift")


subsetedrules <- subset(Rules2agregated, lhs %in% "APP")
sortedrules <- sort(subsetedrules, decreasing = TRUE,
                    na.last = NA, by = "confidence")
inspect(head(sortedrules,10))



inspect(head(sortedrules,10))

inspect(Rules2agregated)

inspect(Rules2agregated)

# funcion para mostrar las reglas que nos interesan -----------------------

# subset(Rules2agregated, in.out %in% category)
# inspect(sort(subset(Rules2agregated, in.out %in% category),
#               decreasing = TRUE, na.last = NA, by = "support")
        # )

subsetedrules <- subset(Rules2agregated, rhs %in% "app")
sortedrules <- sort(subsetedrules, decreasing = TRUE,
                    na.last = NA, by = "confidence")
inspect(head(sortedrules,5))


toprules<- function(rules,category,in_or_out,rules_amount){
  
  if (in_or_out == "in") {
    
    subsetedrules <- subset(rules, lhs %in% category)
    sortedrules <- sort(subsetedrules, decreasing = TRUE,
                        na.last = NA, by = "confidence")
    inspect(head(sortedrules,rules_amount))
    }
  if (in_or_out == "out") {
    
    subsetedrules <- subset(rules, rhs %in% category)
    sortedrules <- sort(subsetedrules, decreasing = TRUE,
                        na.last = NA, by = "confidence")
    inspect(head(sortedrules,rules_amount))
    }
  else{print("chose category as imput or output")}
  

}




# epic4 -------------------------------------------------------------------


trans4 <- read.transactions("Data/trans2.csv",format = "basket" , sep = ",")
Sku_category<-read.csv("Data/sku_category.csv")
#        sku manual_categories
# 1 8MO0001       accessories
# 2 8MO0002       accessories
# 3 8MO0003       accessories


# tempbrand<- substr(Sku_category$sku, 1, 3)
# tempbrand$brand <- substr(tempbrand$sku, 1, 3)
# 
# tempbrand <- Sku_category

Sku_brand<-read.csv("Data/Brands.csv", sep = ";")
#   sku   brand
# 1 RAI0001   RAI
# 2 APP0017   APP
# 3 APP0019   APP

# 
# df$filname <- substr(df$filname, 0, 3)
# nrow(Sku_category)
# nrow(Sku_brand)

# merge las dos DF de sku_ por sku with category being the main

skus <- merge(x = Sku_category, y = unique(Sku_brand), by= "sku",
              all.x = TRUE)
# table(is.na(skus))

# names(skus)[1] <- names(trans4@itemInfo)[1]

# creamos una columna nueva "category_brand" with category and brand
# lo metemos en skus

skus <- unite(skus,2:3,col = "category_brand",remove = FALSE)

# merge the skus and the transaction DF with it being the main and we toke the 
# result outside
testmerge <- merge(x = skus, y = trans4@itemInfo, by.x= "sku",by.y = "labels",
                   all.y = TRUE)

# limpiamos los NAs 

testmerge <- apply(testmerge, 2, as.character)
testmerge[is.na(testmerge)]<-"other"
apply(testmerge,2,as.factor)
testmerge <- as.data.frame(testmerge)
names(testmerge)[1] <- names(trans4@itemInfo)[1]

# we return the DF to the transaction matrix

trans4@itemInfo<- testmerge
# we generate rules aggregated by what we want
Rulesaggtrans4 <- apriori (aggregate(trans4, "category_brand"),
                           parameter = list(supp = 0.002, conf = 0.5))
