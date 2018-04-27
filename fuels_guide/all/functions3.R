library(dplyr)
library(tidyr)
library(xlsx)

d <- read.csv('C:\\Users\\User\\Documents\\GitHub\\thesis\\fuels_guide\\all\\data_20180416145329.csv', header = TRUE)

#create columns of data that are sums of two columns: total forb cover (perennial + annual), total herb fuel load (live + dead), tree_density (>5<50 + >50)
d$cover_herb_forb <- with(d, fol_cover_pt_pforb + fol_cover_pt_aforb)
d$density_tree_JUOS <- with(d, tree_dns_5_50_JUOS + tree_dns_gt50_JUOS)
d$density_tree_PIED <- with(d, tree_dns_5_50_PIED + tree_dns_gt50_PIED)

#calculate herb bulk density
d$avg_grass_ht_meters <- d$avg_grass_ht/100
d$herb_load_ttl <- (d$herb_fuel_live_wd + d$herb_fuel_dead_wd) * 0.0001   #total herb load converted into kg/m^2 from kg/ha
d$bulkDns_herb <- with(d, herb_load_ttl/avg_grass_ht_meters)     #only takes into account grasses, not forbs!

#select columns of data that you need and standardize naming convention
data <- select(d, c(subplot_id, scode, rcode, treatment, sp_phase, year,  
                    cover_tree_JUOS = tree_cvr_JUOS, 
                    cover_tree_PIED = tree_cvr_PIED, 
                    cover_shrub_ARTRW8 = can_cover_ARTRW8, 
                    cover_shrub_CHVI8 = can_cover_CHVI8, 
                    cover_shrub_PUST = can_cover_PUST, 
                    cover_herb_pgrass = fol_cover_pt_pgrass, 
                    cover_herb_agrass = fol_cover_pt_agrass, 
                    cover_herb_forb, 
                    cover_ltrDuff_iLitter = litter_cover, 
                    cover_bground_bground = bare_gnd_cover,
                    density_tree_JUOS, 
                    density_tree_PIED, 
                    density_shrub_ARTRW8 = shrub_dns_ARTRW8, 
                    density_shrub_CHVI8 = shrub_dns_CHVI8,
                    density_shrub_PUST = shrub_dns_PUST,
                    height_shrub_ARTRW8 = shrub_ht_avg_ARTRW8,
                    height_shrub_CHVI8 = shrub_ht_avg_CHVI8,
                    height_shrub_PUST = shrub_ht_avg_PUST,
                    height_herb_grass = grass_ht_avg,
                    height_herb_forb = forb_ht_avg,
                    loading_shrub_ARTRW8 = shrub_bio_ttl_ARTRW8,
                    loading_shrub_CHVI8 = shrub_bio_ttl_CHVI8,
                    loading_shrub_PUST = shrub_bio_ttl_PUST,
                    loading_herb_live = herb_fuel_live_wd,
                    loading_herb_dead = herb_fuel_dead_wd,
                    loading_dwd_10h = dwd_fuel_10h,
                    loading_dwd_100h = dwd_fuel_100h,
                    loading_dwd_1000hS = dwd_fuel_1000h_s,
                    loading_dwd_1000hR = dwd_fuel_1000h_r,
                    loading_ltrDuff_iLitter = ispace_litter_load_wd,
                    loading_ltrDuff_trLitterDuff = tree_ltr_duff_ld,
                    bulkDns_shrub_ARTRW8 = shrub_bd_ARTRW8,
                    bulkDns_shrub_CHVI8 = shrub_bd_CHVI8,
                    bulkDns_shrub_PUST = shrub_bd_PUST,
                    bulkDns_herb_total = bulkDns_herb))


#data cleaning and subsetting function
clean_subset <- function(data, region, phase, treatment1, scode, y_imp){
  data <- filter(data, year != 'NA' | sp_phase != 'NA')   #remove rows where year = NA or sp_phase = NA
  data <- distinct(data)                                  #remove duplicate rows
  
  #remove rows where year != 10 years since treatment
  #but first create relational dataframe with site code (scode) and year of implementation
  #scode <- c('GR', 'ON', 'SC')                         #vector of sites
  #y_imp <- c(7, 6, 7)                                  #vector of year of implementation for corresponding sites
  
  relation <- data.frame(scode, y_imp)                 #create relational dataframe
  
  data <- left_join(data, relation, by = 'scode')      #join data and relational dataframes by scode
  data$yst <- data$year - data$y_imp                   #create year since treatment column (yst)
  data <- filter(data, yst == 10)                      #subset data where yst = 10
  
  data <- filter(data,                                 #subset data based on variables passed to clean_subset function
                 rcode == region, 
                 sp_phase == phase, 
                 treatment1 == as.factor(treatment))
  data <- select(data, -c(subplot_id, scode, rcode, treatment, sp_phase, year, y_imp, yst))
}


scode <- c('GR', 'ON', 'SC') #specify site codes
y_imp <- c(7, 6, 7)          #specify year of implementation for sites      
data <- clean_subset(data, region = 'JP', phase = '2', treatment1 = 'CO', scode = scode, y_imp = y_imp)

###################################################################################
stats <- function(data){
  stats <- function(x){c(round(min(x, na.rm = TRUE), 6),
                              round(mean(x, na.rm = TRUE), 6),
                              round(max(x, na.rm = TRUE), 6))
  }
  fuel <- as.data.frame(sapply(data, stats))
  fuel$stat <- c('min', 'mean', 'max')
  
  #format table
  fuel_long <- fuel %>%
    gather(key = component, value = value, -stat) %>%
    spread(key = stat, value = value) %>%
    separate(col = component, into = c("Variable", "Category", "Component"), sep = "\\_")
  
  #sort table
  fuel_order <- fuel_long[c('Variable', 'Category', 'Component', 'min', 'mean', 'max')]
  fuel_order$Variable <- factor(fuel_order$Variable, 
                               levels = c('cover', 'density', 'height', 'loading', 'bulkDns'))
  fuel_order$Category <- factor(fuel_order$Category, 
                               levels = c('tree', 'shrub', 'herb', 'dwd', 'ltrDuff', 'bground'))
  fuel_order <- arrange(fuel_order, Variable, Category)
  
  
  #make column of conversion factors
  d <- fuel_order 
  d$conversion <- 'NA'
  d$conversion[d$Variable == 'cover'] <- 1
  d$conversion[d$Variable == 'density'] <- 0.404686      #from #/ha to #/acre
  d$conversion[d$Variable == 'height'] <- 0.393701       #from cm to in
  d$conversion[d$Variable == 'loading'] <- 0.000446090   #from kg/ha to tons/acre
  d$conversion[d$Variable == 'bulkDns'] <- 0.0624280     #from kg/m^3 to lbs/ft^3
  
  d$conversion <- as.numeric(d$conversion)               #coerce data type of conversion from character to numeric
  d[,c('min', 'mean', 'max')] <- d[,c('min', 'mean', 'max')] * d$conversion      #convert to correct units
  d <- select(d, -conversion)                            #remove conversion column
 
  #make columns min mean and max of type numeric
  d[,c('min','mean','max')] <- lapply(d[,c('min','mean','max')], as.numeric)
  
  #round columns
  d[,c('min','mean','max')] <- round(d[,c('min','mean','max')], 4)
  return(d)
}

d <- stats(data)

#final order/arrangement
d$Component <- factor(d$Component, levels = c('JUOS', 'PIED', 'ARTRW8', 'CHVI8', 'PUST', 'pgrass', 'agrass', 'grass', 'forb', 'iLitter', 'bground',
                                              'live', 'dead', '10h', '100h', '1000hS', '1000hR', 'trLitterDuff', 'total'))
d <- arrange(d, Variable, Category, Component)

write.xlsx(d, file = 'C:\\Users\\User\\Documents\\GitHub\\thesis\\fuels_guide\\all\\fuels_guide_table_tidy.xlsx', 
           sheetName = 'Sheet3', 
           append = TRUE)

