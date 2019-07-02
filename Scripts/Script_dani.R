items <- read.csv(file = "Data/lineitems.csv", sep = ";")

# items: contains the information of all the products for each order:
# individual id inside every order id, the order id, product quantity for every
# product in each order id, the Stock Keeping Unit or sku, the unit price and 
# the date that the product has been added into the order id.

orders <- read.csv("Data/orders_translated.csv", sep = ";")

# orders: contains the information from any transaction that has been made
# in the online shop: order id, the time that has been created, how much to
# customer paid and the "its" state. 

# primero filtramos por completed
start <-items[items$id_order %in% orders$id_order[orders$state=="Completed"],]

# items$count <- FALSE
# for (i in unique(items$id_order)) {
#   temp <- items$id_order %in% i
#   items[temp,8]<- ifelse(sum( temp) > 1,TRUE,FALSE)
# }
# 
# temp <- items$id_order %in% 299539
# items[temp,8]<- ifelse(sum( temp) > 1,TRUE,FALSE)
# 
# 
# funci<- function(comparacion,lista){
#   return (ifelse(sum(lista %in% comparacion) > 1,TRUE,FALSE))
# }
# 
# items$count <- sapply(items$id_order, function(x){funci(x,items$id_order)})

# "LGE0037" %in% items$sku
# which(items$sku %in% "LGE0037")




DFclustering <- start %>% group_by(sku) %>% 
                           summarise(number_transactions =n(),
                                     total_sales = 
                                        sum(product_quantity),
                                     average_sales_by_transaction = 
                                       mean(product_quantity),
                                     average_price_by_product = 
                                      mean(unit_price),
                                     amount_of_transactions = 
                                       length(unique(id_order))
                                     )

DFclustering
# sacamos una lista que nos da las posiciones de todas las transacciones por sku
# con estas posiciones sacamos un vector de id_order que nos permite calcular 
# sociability by sku

lista_posiciones_start<-sapply(DFclustering$sku,
                               function(x){which(start$sku %in% x)})

start$sku[1]

# here we have the Id_orders by the position 

lista_id_orders_by_SKU 





lista_id_orders_by_SKU  <- lapply(lista_posiciones_start,
                                  function(x){start$id_order[x]})
# comprobacion
start$id_order[lista_posiciones_start[[1]]][1] == lista_id_orders_by_SKU[[1]][1]


# por cada id_order hay un elemento , si se repiten hay mas de un elemento
info_transacciones <- start %>% group_by(id_order) %>% summarise(elementos=n(),
                                                                 acompañantes = 
                                                                   n() - 1)
info_transacciones <- info_transacciones  %>% mutate(solitaria = ifelse(acompañantes==0,TRUE,FALSE))


# proceso
# temp <- info_transacciones[info_transacciones$id_order %in% 
#                              lista_id_orders_by_SKU [[1]],"solitaria"][[1]]
# 
# social <- length(temp) - sum(temp) 
# loners <- sum(temp)
# if (loners == 0) {
#   loners = 1
# }
#   sociability <-   social/loners
hateyouDani <- function(lista_id_orders_by_SKU){
  
  temp <- info_transacciones[info_transacciones$id_order %in% 
                               lista_id_orders_by_SKU,"solitaria"][[1]]
  
  social <- length(temp) - sum(temp) 
  loners <- sum(temp)
  if (loners == 0) {
    loners = 1
  }
  sociability <-   social/loners
  return(sociability)
}

DFclustering <- as.data.frame(DFclustering)
  
DFclustering$sociability <- sapply(lista_id_orders_by_SKU,FUN = hateyouDani)


head(DFclustering)

# para_cluster <- start %>% group_by(sku) %>% summarise(apariciones = n(),
#                                       media_repe = mean(product_quantity),
#                                       total_vendido_qa = sum(product_quantity),
#                                       transacciones = length(unique(id_order))) %>% 
#                             mutate(total_vendido = apariciones * media_repe)

por_orden <- items %>% group_by(id_order) %>% summarise(acompañantes = n()-1)
por_orden$acompañantes <- as.numeric(as.character(por_orden$acompañantes))
# for(i in para_cluster$sku){which(items$sku %in% i))}
lista<-sapply(para_cluster$sku[1:100],function(x){which(items$sku %in% x)})

lista_transacciones_por_sku <- sapply(DFclustering$sku, which())



# sku_1_acomp <- as.vector(
#   por_orden[
#     which(
#       por_orden$id_order %in% items$id_order[
# 
#                 lista[[1]]
#       ]
#     )
#   ,2]
# )
# 
# adanilecaigobien <-  c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,1,1,0)
# which(adanilecaigobien == 0)
# sum(adanilecaigobien == 0)
