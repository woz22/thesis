#PURPOSE: create column of pre-treatment tree cover values for each year of each subplot

library(dplyr)
library(ggplot2)
library(tidyr)

data <- read.csv("C:/Users/User/Desktop/create_tc_col/tc.csv", header = TRUE) 
head(data, n = 20)
str(data)

#remove unneeded columns
data <- data[, c("subplot_id", "year", "dwd_fuel_10h", "tree_cover_ttl", "scode")]
str(data)

#vectors of sites where year of implementation = 2006, 2007, or 2008
imp06 <- c('ON', 'BC', 'WB', 'MC')
imp07 <- c('BM', 'DR', 'SV', 'GR', 'SC')
imp08 <- c('SR')

#create column for year of treatment implementation
data$y_imp <- NA                         #initialize column
data$y_imp[data$scode %in% imp06] <- 6
data$y_imp[data$scode %in% imp07] <- 7
data$y_imp[data$scode %in% imp08] <- 8

#create column for year since treatment (yst): year of monitoring - implementation year
data$yst <- data$year - data$y_imp

head(data, n = 20)
print(data[which(is.na(data$yst)),])

#create pre-treatment tree cover column based on the value in tree_cover_ttl when year = 6, fo each subplot
data$pre_tc <- NA

for (i in unique(data$subplot_id)) {                            #for each unique subplot_id
  pre_tree_c <- data$tree_cover_ttl[data$subplot_id == i][1]    #take the tree cover value from the first year of data collection
  data$pre_tc[data$subplot_id == i] <- pre_tree_c               #put those tree cover values in a column
}


#error check 
print(data[which(is.na(data$pre_tc)),])
print(data[which(data$pre_tc <1),])

summary(data$pre_tc)




#################ALTERNATE APPROACH USING DPLYR AND RELATIONAL TABLES
scode <- c('ON', 'BC', 'WB', 'MC','BM', 'DR', 'SV', 'GR', 'SC', 'SR')
y_imp2 <- c(6, 6, 6, 6, 7, 7, 7, 7, 7, 8)
relation <- data.frame(scode, y_imp2)
head(relation, n = 10)

data1 <- left_join(data, relation, by = "scode")
sample_n(data1, size = 50)

#error check
which(data1$y_imp - data1$y_imp2 != 0)
which(is.na(data1$y_imp2))


#create pre_tc with alternative method
data1$pre_tc2 <- NA
for (i in unique(data1$subplot_id)) {                                                                    #for each unique subplot_id
  pre_tree_c2 <- data1$tree_cover_ttl[data1$subplot_id == i][which.min(data1$year)]                      #take the tree cover value from the first year of data collection
  data1$pre_tc2[data1$subplot_id == i] <- pre_tree_c2                                                    #put those tree cover values in a column
}
head(data1, n = 20)

data1$pre_tc2 <- NA
for (i in unique(data1$subplot_id)) {                                                                    #for each unique subplot_id
  pre_tree_c2 <- data1$tree_cover_ttl[data1$subplot_id == i][which.min(data1$yst)]                       #take the tree cover value from the first year of data collection
  data1$pre_tc2[data1$subplot_id == i] <- pre_tree_c2                                                    #put those tree cover values in a column
}
head(data1, n = 20)
str(data1)

data1$yst <- as.integer(data1$yst)
data1$pre_tc2 <- NA
for (i in unique(data1$subplot_id)) {                                                                    #for each unique subplot_id
  pre_tree_c2 <- data1$tree_cover_ttl[data1$subplot_id == i][which.min(data1$yst)]                       #take the tree cover value from the first year of data collection
  data1$pre_tc2[data1$subplot_id == i] <- pre_tree_c2                                                    #put those tree cover values in a column
}
head(data1, n = 20)


which(is.na(data1$yst))















#how to figure out if this works? how to see if there are subplots where there is no-pretreatment data or no data from year 6 or 7 or 8?


for (i in unique(data$subplot_id)) {
  if(data$year[data$subplot_id == i][1] > 6){
    print(data$subplot_id[i])
  }
}


for (i in unique(data1$subplot_id)) {                                                                    #for each unique subplot_id
  pre_year <- min(data1$yst[data1$subplot_id == i])
  pre_tree_c2 <- data1$tree_cover_ttl[data1$subplot_id == i][pre_year]      #take the tree cover value from the first year of data collection
  data1$pre_tc2[data1$subplot_id == i] <- pre_tree_c2                                                    #put those tree cover values in a column
}

View(data1)
for (i in unique(data1$subplot_id)) {                                                                    #for each unique subplot_id
  pre_year <- min(data1$yst[data1$subplot_id == i])
  pre_tree_c2 <- data1$tree_cover_ttl[data1$subplot_id == i][pre_year]      #take the tree cover value from the first year of data collection
  data1$pre_tc2[data1$subplot_id == i] <- pre_tree_c2                                                    #put those tree cover values in a column
}

######
#create new column titled 'pre_tc' where for each subplot, 
#the value of tree_cover_ttl when year = 6 is pasted for every yst in that plot

data$tmp <- NA
for (i in unique(data$subplot_id)) {
  ugh <- data$tree_cover_ttl[data$subplot_id == i & data$year == 6]
  data$tmp[data$subplot_id == i] <- ugh
}


