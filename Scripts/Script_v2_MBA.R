# Library -----------------------------------------------------------------
pacman::p_load(dplyr,tidyr,plyr,ggplot2,caret,
               RColorBrewer,corrplot,reshape, viridis,plotly,
               arules, reshape2, arulesViz,shinythemes,shiny)

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

# now we filter down the items to the completed orders.

orders_completed<- orders[orders$state == "Completed",] 

items$unit_price <- as.numeric(items$unit_price)


# testing how to use the summaryze/mutate functions on a gruop by
# benefit <- items %>% group_by(sku) %>% summarize(sku,benefit = 
                                                # product_quantity*unit_price)


itemsorders2plus <- items %>% group_by(id_order) %>% filter(n()>1)

itema2plusinorderscompl <- itemsorders2plus[itemsorders2plus$id_order %in% 
                                              orders_completed$id_order,]



# Epic 2 apriory, rules and transaction matrix ------------------------------

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

# Epic 3 we ad categories to the transaction matrix -------------------------

# we dont want to tamper with the previous results so we re-read the data.
trans3 <- read.transactions("Data/trans2.csv",format = "basket" , sep = ",")
Sku_category<-read.csv("Data/sku_category.csv")

# we could do the same substracting the first 3 characters of the sku instead
# of reading a new CSV
Sku_brand<-read.csv("Data/Brands.csv",sep = ";")
head(Sku_category)
head(Sku_brand)
Sku_brand<-unique(Sku_brand)
nrow(Sku_category)
nrow(Sku_brand)

# we make a new dataframe with all the relevant information of the 3 CSV
# we have read previously
skus <- merge(x = Sku_category, y = unique(Sku_brand), by= "sku",
              all.x = TRUE)

# c(1:5) %in% c(3:4)

table(skus$sku %in% trans3@itemInfo$labels)
skus <- skus[unique(skus$sku), ]

# we make a new colum "brand_category" for more granularity.

skus <- unite(skus,2:3,col = "category_brand",remove = FALSE)
# we filter the skus down to only the ones of the transacction matrix,
# we just dont care about all the others.
testmerge <- merge(x = skus, y = trans3@itemInfo, by.x= "sku",by.y = "labels",
              all.y = TRUE)

# we change the na to other to do that we need to change the factors to char 
# and because we like to have the categories as factors we turnit back
testmerge <- testmerge <- apply(testmerge, 2, as.character)
testmerge[is.na(testmerge)]<-"other"
apply(testmerge,2,as.factor)
testmerge <- as.data.frame(testmerge)
names(testmerge)[1] <- names(trans3@itemInfo)[1]


# playing with aggregate and sort and subset 

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



# we work with a diferent transaction matrix because we dont want to mess with 
# the "original" data
trans4 <- trans3

trans4@itemInfo<- testmerge

# this is the best way i have fount to obtain 
# the rules aggreagated by "something"

Rulesaggtrans4 <- apriori (aggregate(trans4, "category_brand"),
                           parameter = list(supp = 0.002, conf = 0.5))

Rules4agregated <- aggregate(RulesName4, "category_brand")




# Function for showing rules -----------------------

# its kinda useless because later I found that 
# ruleExplorer does this more completely and dinamically

# working out what i want the function to do before y make the function


# subset(Rules2agregated, in.out %in% category)
# inspect(sort(subset(Rules2agregated, in.out %in% category),
#               decreasing = TRUE, na.last = NA, by = "support")
        # )


# subsetedrules <- subset(Rules2agregated, rhs %in% "app")
# sortedrules <- sort(subsetedrules, decreasing = TRUE,
#                     na.last = NA, by = "confidence")
# inspect(head(sortedrules,5))


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




# epic4 redundant I added to the epic 3 because they were complementary -------


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


plot<-ggplot(data=testmerge , aes(x=manual_categories,
                                  fill= manual_categories)) + 
  geom_bar()
  plotly::ggplot(data=testmerge , aes(x=manual_categories,
                                      fill= manual_categories)) + 
    geom_bar()

  
