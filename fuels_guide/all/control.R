library(tidyr)
library(dplyr)

###############RE-DOWNLOAD DATA FOR ALL TREATMENTS THEN FILTER??
#################then you would have to add filter for treatment
######################for mastication cbind bruce's masticated dwd fuel loading
###BEWARE--MULTIPLE TREE COVER AND GRASS HEIGHT VARIABLES
#use str(data, list.len = ncol(data)) to see all variables in dataset

#read in data and name it data
data <- read.csv('C:\\Users\\User\\Documents\\GitHub\\thesis\\fuels_guide\\all\\data_20180416145329.csv', header = TRUE)

#1 remove rows where year == NA, or sp_phase == NA
data <- filter(data, year != 'NA' | sp_phase != 'NA')

#remove duplicate rows
data <- distinct(data)

#2 remove rows where year != 10 years since treatment
#but first create relational dataframe with site code (scode) and year of implementation
scode <- c('GR', 'ON', 'SC')
y_imp <- c(7, 6, 7)

relation <- data.frame(scode, y_imp)   #then join data and relational dataframes by scode

data <- left_join(data, relation, by = 'scode')
data$yst <- data$year - data$y_imp     #create year since treatment column (yst)
data <- filter(data, yst == 10)    #select data where yst = 10

#select data where pre-treatment woodland phase (sp_phase) = 2 and treatment = control
#data <- filter(data, sp_phase == 2)

#select treatment and region
#data <- filter(data, treatment == 'ME' & rcode == 'JP')
data <- filter(data, rcode == 'JP')


###WHAT TREE HEIGHT(S)?
d <- data %>%
  group_by(sp_phase, treatment) %>%
  summarise( 
            "cover_tree_JUOS_min" = round(min(tree_cvr_JUOS, na.rm = TRUE), 4),
            "cover_tree_JUOS_mean"= round(mean(tree_cvr_JUOS, na.rm = TRUE), 4),
            "cover_tree_JUOS_max"= round(max(tree_cvr_JUOS, na.rm = TRUE), 4),
            "cover_tree_PIED_min" = round(min(tree_cvr_PIED, na.rm = TRUE), 4),
            "cover_tree_PIED_mean"= round(mean(tree_cvr_PIED, na.rm = TRUE), 4),
            "cover_tree_PIED_max"= round(max(tree_cvr_PIED, na.rm = TRUE), 4),
            "cover_shrub_ARTRW8_min" = round(min(can_cover_ARTRW8, na.rm = TRUE), 4),
            "cover_shrub_ARTRW8_mean"= round(mean(can_cover_ARTRW8, na.rm = TRUE), 4),
            "cover_shrub_ARTRW8_max"= round(max(can_cover_ARTRW8, na.rm = TRUE), 4),
            "cover_shrub_CHVI8_min" = round(min(can_cover_CHVI8, na.rm = TRUE), 4),
            "cover_shrub_CHVI8_mean"= round(mean(can_cover_CHVI8, na.rm = TRUE), 4),
            "cover_shrub_CHVI8_max"= round(max(can_cover_CHVI8, na.rm = TRUE), 4),
            "cover_shrub_PUST_min" = round(min(can_cover_PUST, na.rm = TRUE), 4),
            "cover_shrub_PUST_mean"= round(mean(can_cover_PUST, na.rm = TRUE), 4),
            "cover_shrub_PUST_max"= round(max(can_cover_PUST, na.rm = TRUE), 4),
            "cover_herb_pgrass_min" = round(min(fol_cover_pt_pgrass, na.rm = TRUE), 4),
            "cover_herb_pgrass_mean"= round(mean(fol_cover_pt_pgrass, na.rm = TRUE), 4),
            "cover_herb_pgrass_max"= round(max(fol_cover_pt_pgrass, na.rm = TRUE), 4),
            "cover_herb_agrass_min" = round(min(fol_cover_pt_agrass, na.rm = TRUE), 4),
            "cover_herb_agrass_mean"= round(mean(fol_cover_pt_agrass, na.rm = TRUE), 4),
            "cover_herb_agrass_max"= round(max(fol_cover_pt_agrass, na.rm = TRUE), 4),
            "cover_herb_forb_min" = round(min(fol_cover_pt_pforb + fol_cover_pt_aforb, na.rm = TRUE), 4),
            "cover_herb_forb_mean"= round(mean(fol_cover_pt_pforb + fol_cover_pt_aforb, na.rm = TRUE), 4),
            "cover_herb_forb_max"= round(max(fol_cover_pt_pforb + fol_cover_pt_aforb, na.rm = TRUE), 4),
            "cover_ltrDuff_ilitter_min" = round(min(litter_cover, na.rm = TRUE), 4),
            "cover_ltrDuff_ilitter_mean"= round(mean(litter_cover, na.rm = TRUE), 4),
            "cover_ltrDuff_ilitter_max"= round(max(litter_cover, na.rm = TRUE), 4),
            "cover_ltrDuff_bareground_min" = round(min(bare_gnd_cover, na.rm = TRUE), 4),
            "cover_ltrDuff_bareground_mean"= round(mean(bare_gnd_cover, na.rm = TRUE), 4),
            "cover_ltrDuff_bareground_max"= round(max(bare_gnd_cover, na.rm = TRUE), 4),
            "density_tree_JUOS_min" = round(min(tree_dns_5_50_JUOS + tree_dns_gt50_JUOS, na.rm = TRUE) * 0.404686, 4),
            "density_tree_JUOS_mean"= round(mean(tree_dns_5_50_JUOS + tree_dns_gt50_JUOS, na.rm = TRUE) * 0.404686, 4),
            "density_tree_JUOS_max"= round(max(tree_dns_5_50_JUOS + tree_dns_gt50_JUOS, na.rm = TRUE) * 0.404686, 4),
            "density_tree_PIED_min" = round(min(tree_dns_5_50_PIED + tree_dns_gt50_PIED, na.rm = TRUE) * 0.404686, 4),
            "density_tree_PIED_mean"= round(mean(tree_dns_5_50_PIED + tree_dns_gt50_PIED, na.rm = TRUE) * 0.404686, 4),
            "density_tree_PIED_max"= round(max(tree_dns_5_50_PIED + tree_dns_gt50_PIED, na.rm = TRUE) * 0.404686, 4),
            "density_shrub_ARTRW8_min" = round(min(shrub_dns_ARTRW8, na.rm = TRUE) * 0.404686, 4),
            "density_shrub_ARTRW8_mean"= round(mean(shrub_dns_ARTRW8, na.rm = TRUE) * 0.404686, 4),
            "density_shrub_ARTRW8_max"= round(max(shrub_dns_ARTRW8, na.rm = TRUE) * 0.404686, 4),
            "density_shrub_CHVI8_min" = round(min(shrub_dns_CHVI8, na.rm = TRUE) * 0.404686, 4),
            "density_shrub_CHVI8_mean"= round(mean(shrub_dns_CHVI8, na.rm = TRUE) * 0.404686, 4),
            "density_shrub_CHVI8_max"= round(max(shrub_dns_CHVI8, na.rm = TRUE) * 0.404686, 4),
            "density_shrub_PUST_min" = round(min(shrub_dns_PUST, na.rm = TRUE) * 0.404686, 4),
            "density_shrub_PUST_mean"= round(mean(shrub_dns_PUST, na.rm = TRUE) * 0.404686, 4),
            "density_shrub_PUST_max"= round(max(shrub_dns_PUST, na.rm = TRUE) * 0.404686, 4),
            "height_shrub_ARTRW8_min" = round(min(shrub_ht_avg_ARTRW8, na.rm = TRUE) * 0.393701, 4),
            "height_shrub_ARTRW8_mean"= round(mean(shrub_ht_avg_ARTRW8, na.rm = TRUE) * 0.393701, 4),
            "height_shrub_ARTRW8_max"= round(max(shrub_ht_avg_ARTRW8, na.rm = TRUE) * 0.393701, 4),
            "height_shrub_CHVI8_min" = round(min(shrub_ht_avg_CHVI8, na.rm = TRUE) * 0.393701, 4),
            "height_shrub_CHVI8_mean"= round(mean(shrub_ht_avg_CHVI8, na.rm = TRUE) * 0.393701, 4),
            "height_shrub_CHVI8_max"= round(max(shrub_ht_avg_CHVI8, na.rm = TRUE) * 0.393701, 4),
            "height_shrub_PUST_min" = round(min(shrub_ht_avg_PUST, na.rm = TRUE) * 0.393701, 4),
            "height_shrub_PUST_mean"= round(mean(shrub_ht_avg_PUST, na.rm = TRUE) * 0.393701, 4),
            "height_shrub_PUST_max"= round(max(shrub_ht_avg_PUST, na.rm = TRUE) * 0.393701, 4),
            "height_herb_grass_min" = round(min(grass_ht_avg, na.rm = TRUE) * 0.393701, 4),
            "height_herb_grass_mean"= round(mean(grass_ht_avg, na.rm = TRUE) * 0.393701, 4),
            "height_herb_grass_max"= round(max(grass_ht_avg, na.rm = TRUE) * 0.393701, 4),
            "height_herb_forb_min" = round(min(forb_ht_avg, na.rm = TRUE) * 0.393701, 4),
            "height_herb_forb_mean"= round(mean(forb_ht_avg, na.rm = TRUE) * 0.393701, 4),
            "height_herb_forb_max"= round(max(forb_ht_avg, na.rm = TRUE) * 0.393701, 4),
            "loading_shrub_ARTRW8_min" = round(min(shrub_bio_ttl_ARTRW8, na.rm = TRUE) * 0.000446090, 4),
            "loading_shrub_ARTRW8_mean"= round(mean(shrub_bio_ttl_ARTRW8, na.rm = TRUE) * 0.000446090, 4),
            "loading_shrub_ARTRW8_max"= round(max(shrub_bio_ttl_ARTRW8, na.rm = TRUE) * 0.000446090, 4),
            "loading_shrub_CHVI8_min" = round(min(shrub_bio_ttl_CHVI8, na.rm = TRUE) * 0.000446090, 4),
            "loading_shrub_CHVI8_mean"= round(mean(shrub_bio_ttl_CHVI8, na.rm = TRUE) * 0.000446090, 4),
            "loading_shrub_CHVI8_max"= round(max(shrub_bio_ttl_CHVI8, na.rm = TRUE) * 0.000446090, 4),
            "loading_shrub_PUST_min" = round(min(shrub_bio_ttl_PUST, na.rm = TRUE) * 0.000446090, 4),
            "loading_shrub_PUST_mean"= round(mean(shrub_bio_ttl_PUST, na.rm = TRUE) * 0.000446090, 4),
            "loading_shrub_PUST_max"= round(max(shrub_bio_ttl_PUST, na.rm = TRUE) * 0.000446090, 4),
            "loading_herb_live_min" = round(min(herb_fuel_live_wd, na.rm = TRUE) * 0.000446090, 4),
            "loading_herb_live_mean"= round(mean(herb_fuel_live_wd, na.rm = TRUE) * 0.000446090, 4),
            "loading_herb_live_max"= round(max(herb_fuel_live_wd, na.rm = TRUE) * 0.000446090, 4),
            "loading_herb_dead_min" = round(min(herb_fuel_dead_wd, na.rm = TRUE) * 0.000446090, 4),
            "loading_herb_dead_mean"= round(mean(herb_fuel_dead_wd, na.rm = TRUE) * 0.000446090, 4),
            "loading_herb_dead_max"= round(max(herb_fuel_dead_wd, na.rm = TRUE) * 0.000446090, 4),
            "loading_herb_total_min" = round(min(herb_fuel_live_wd + herb_fuel_dead_wd, na.rm = TRUE) * 0.000446090, 4),
            "loading_herb_total_mean"= round(mean(herb_fuel_live_wd + herb_fuel_dead_wd, na.rm = TRUE) * 0.000446090, 4),
            "loading_herb_total_max"= round(max(herb_fuel_live_wd + herb_fuel_dead_wd, na.rm = TRUE) * 0.000446090, 4),
            "loading_dwd_10hr_min" = round(min(dwd_fuel_10h, na.rm = TRUE) * 0.000446090, 4),
            "loading_dwd_10hr_mean"= round(mean(dwd_fuel_10h, na.rm = TRUE) * 0.000446090, 4),
            "loading_dwd_10hr_max"= round(max(dwd_fuel_10h, na.rm = TRUE) * 0.000446090, 4),
            "loading_dwd_100hr_min" = round(min(dwd_fuel_100h, na.rm = TRUE) * 0.000446090, 4),
            "loading_dwd_100hr_mean"= round(mean(dwd_fuel_100h, na.rm = TRUE) * 0.000446090, 4),
            "loading_dwd_100hr_max"= round(max(dwd_fuel_100h, na.rm = TRUE) * 0.000446090, 4),
            "loading_dwd_1000hrS_min" = round(min(dwd_fuel_1000h_s, na.rm = TRUE) * 0.000446090, 4),
            "loading_dwd_1000hrS_mean"= round(mean(dwd_fuel_1000h_s, na.rm = TRUE) * 0.000446090, 4),
            "loading_dwd_1000hrS_max"= round(max(dwd_fuel_1000h_s, na.rm = TRUE) * 0.000446090, 4),
            "loading_dwd_1000hrR_min" = round(min(dwd_fuel_1000h_r, na.rm = TRUE) * 0.000446090, 4),
            "loading_dwd_1000hrR_mean"= round(mean(dwd_fuel_1000h_r, na.rm = TRUE) * 0.000446090, 4),
            "loading_dwd_1000hrR_max"= round(max(dwd_fuel_1000h_r, na.rm = TRUE) * 0.000446090, 4),
            "loading_ltrDuff_ilitter_min" = round(min(ispace_litter_load_wd, na.rm = TRUE) * 0.000446090, 4),
            "loading_ltrDuff_ilitter_mean"= round(mean(ispace_litter_load_wd, na.rm = TRUE) * 0.000446090, 4),
            "loading_ltrDuff_ilitter_max"= round(max(ispace_litter_load_wd, na.rm = TRUE) * 0.000446090, 4),
            "loading_ltrDuff_ltrDuff_min" = round(min(tree_ltr_duff_ld, na.rm = TRUE) * 0.000446090, 4),
            "loading_ltrDuff_ltrDuff_mean"= round(mean(tree_ltr_duff_ld, na.rm = TRUE) * 0.000446090, 4),
            "loading_ltrDuff_ltrDuff_max"= round(max(tree_ltr_duff_ld, na.rm = TRUE) * 0.000446090, 4),
            "bulkDensity_shrub_ARTRW8_min" = round(min(shrub_bd_ARTRW8, na.rm = TRUE) * 0.0624280, 4),
            "bulkDensity_shrub_ARTRW8_mean"= round(mean(shrub_bd_ARTRW8, na.rm = TRUE) * 0.0624280, 4),
            "bulkDensity_shrub_ARTRW8_max"= round(max(shrub_bd_ARTRW8, na.rm = TRUE) * 0.0624280, 4),
            "bulkDensity_shrub_CHVI8_min" = round(min(shrub_bd_CHVI8, na.rm = TRUE) * 0.0624280, 4),
            "bulkDensity_shrub_CHVI8_mean"= round(mean(shrub_bd_CHVI8, na.rm = TRUE) * 0.0624280, 4),
            "bulkDensity_shrub_CHVI8_max"= round(max(shrub_bd_CHVI8, na.rm = TRUE) * 0.0624280, 4),
            "bulkDensity_shrub_PUST_min" = round(min(shrub_bd_PUST, na.rm = TRUE) * 0.0624280, 4),
            "bulkDensity_shrub_PUST_mean"= round(mean(shrub_bd_PUST, na.rm = TRUE) * 0.0624280, 4),
            "bulkDensity_shrub_PUST_max"= round(max(shrub_bd_PUST, na.rm = TRUE) * 0.0624280, 4))

  
###############ROUNDING

#######check tree_ltr_duff_ld for NA + NA = 0 
####################CONVERSION FACTORS


d_co_1 <- d[which(d$sp_phase == 1 & d$treatment == 'CO'),]
d_co_2 <- d[which(d$sp_phase == 2 & d$treatment == 'CO'),]
d_co_3 <- d[which(d$sp_phase == 3 & d$treatment == 'CO'),]

dco2_long <- gather(data = d_co_2, key = name, value = value, - c(sp_phase, treatment))
dco2_long <- dco2_long[, -c(1,2)]
dco2_sep <- separate(data = dco2_long, col = name, into = c("Variable", "Category", "Component", "stat"), sep = "\\_")
dco2_sprd <- spread(data = dco2_sep, key = stat, value = value)

#reorder by column name
d_order <- dco2_sprd[c('Variable', 'Category', 'Component', 'min', 'mean', 'max')]

#order table
var <- c('cover', 'density', 'height', 'loading', 'bulkDensity')
cat <- c('tree', 'shrub', 'herb', 'dwd', 'ltrDuff', 'bareground')

d_order1 <- d_order %>% arrange(match(Variable, var))
d_order2 <- d_order1 %>% arrange(match(Category, cat))

###clean and filter function
clean_subset <- function(data, region, phase, treatment1){
  data <- filter(data, year != 'NA' | sp_phase != 'NA')   #remove rows where year = NA or sp_phase = NA
  data <- distinct(data)                                  #remove duplicate rows
  
  #remove rows where year != 10 years since treatment
  #but first create relational dataframe with site code (scode) and year of implementation
  scode <- c('GR', 'ON', 'SC')                         #vector of sites
  y_imp <- c(7, 6, 7)                                  #vector of year of implementation for corresponding sites
  
  relation <- data.frame(scode, y_imp)                 #create relational dataframe
  
  data <- left_join(data, relation, by = 'scode')      #join data and relational dataframes by scode
  data$yst <- data$year - data$y_imp                   #create year since treatment column (yst)
  data <- filter(data, yst == 10)                      #subset data where yst = 10
  
  data <- filter(data,                                 #subset data based on variables passed to clean_subset function
                 rcode == region, 
                 sp_phase == phase, 
                 treatment1 == as.factor(treatment))   
}
data <- read.csv('C:\\Users\\User\\Documents\\GitHub\\thesis\\fuels_guide\\all\\data_20180416145329.csv', header = TRUE)
data <- clean_subset(data, region = 'JP', phase = '2', treatment1 = 'CO')
  
####need to be columns--data$tree_cvr_JUOS
cover = list(JUOS = data$tree_cvr_JUOS, PIED = data$tree_cvr_PIED)
density = list(ARTRW8 = data$shrub_dns_ARTRW8, CHVI8 = data$shrub_dns_CHVI8)
height = list(grass = data$grass_ht_avg, forb = data$forb_ht_avg)
loading = list(dwd10h = data$dwd_fuel_10h, dwd100h = data$dwd_fuel_100h)
bulkDens = list(ARTRW8 = data$shrub_bd_ARTRW8, CHVI8 = data$shrub_bd_CHVI8)
var_list <- list(cover = cover, density = density, height = height, loading = loading, bulkDens = bulkDens)

fuels_please <- function(list){
  stat_cover <- function(x){c(min(x, na.rm = TRUE), 
                              mean(x, na.rm = TRUE), 
                              max(x, na.rm = TRUE))}
  stat_density <-  function(x){c(min(x, na.rm = TRUE) * 0.404686, 
                                 mean(x, na.rm = TRUE) * 0.404686, 
                                 max(x, na.rm = TRUE) * 0.404686)
    }
  stat_height <- function(x){c(min(x, na.rm = TRUE) * 0.393701, 
                               mean(x, na.rm = TRUE) * 0.393701, 
                               max(x, na.rm = TRUE) * 0.393701)
    }
  stat_loading <- function(x){c(min(x, na.rm = TRUE) * 0.000446090, 
                                mean(x, na.rm = TRUE) * 0.000446090, 
                                max(x, na.rm = TRUE) * 0.000446090)
    }
  stat_bulkDens <- function(x){c(min(x, na.rm = TRUE) * 0.0624280, 
                                 mean(x, na.rm = TRUE) * 0.0624280, 
                                 max(x, na.rm = TRUE) * 0.0624280)
  }
  table_cover <- sapply(var_list[['cover']], stat_cover)
  table_density <- sapply(var_list[['density']], stat_density)
  table_height <- sapply(var_list[['height']], stat_height)
  table_loading <- sapply(var_list[['loading']], stat_loading)
  table_bulkDens <- sapply(var_list[['cover']], stat_bulkDens)
  table_list <- list(cover = table_cover, 
                     density = table_density, 
                     height = table_height, 
                     loading = table_loading, 
                     bulkDens = table_bulkDens)
  return(table_list)
  #fuels <- lapply(var_list, sapply, stats)
  #return(fuels)
}
fuels_please(list = var_list)
fuels_please(data = data, list = var_list, region = 'JP', phase = 2, treatment1 = 'CO')
