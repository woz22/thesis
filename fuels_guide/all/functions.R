library(dplyr)
library(tidyr)

data <- read.csv('C:\\Users\\User\\Documents\\GitHub\\thesis\\fuels_guide\\all\\data_20180416145329.csv', header = TRUE)

#data cleaning and subsetting function
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
data <- clean_subset(data, region = 'JP', phase = '2', treatment1 = 'CO')

###create lists of data 
#need to be columns--data$tree_cvr_JUOS
cover = list(cover_tree_JUOS = data$tree_cvr_JUOS, 
             cover_tree_PIED = data$tree_cvr_PIED)
density = list(density_shrub_ARTRW8 = data$shrub_dns_ARTRW8, 
               density_shrub_CHVI8 = data$shrub_dns_CHVI8)
height = list(height_herb_grass = data$grass_ht_avg, 
              height_herb_forb = data$forb_ht_avg)
loading = list(loading_dwd_dwd10h = data$dwd_fuel_10h, 
               loading_dwd_dwd100h = data$dwd_fuel_100h)
bulkDens = list(bulkDens_shrub_ARTRW8 = data$shrub_bd_ARTRW8, 
                bulkDens_shrub_CHVI8 = data$shrub_bd_CHVI8)
var_list <- list(cover = cover, density = density, 
                 height = height, loading = loading, 
                 bulkDens = bulkDens)

fuels_please <- function(list){
  stat_cover <- function(x){c(min(x, na.rm = TRUE), 
                              mean(x, na.rm = TRUE), 
                              max(x, na.rm = TRUE))
    }
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
  table_cover <- as.data.frame(sapply(var_list[['cover']], stat_cover))
  table_density <- as.data.frame(sapply(var_list[['density']], stat_density))
  table_height <- as.data.frame(sapply(var_list[['height']], stat_height))
  table_loading <- as.data.frame(sapply(var_list[['loading']], stat_loading))
  table_bulkDens <- as.data.frame(sapply(var_list[['bulkDens']], stat_bulkDens))
  fuel <- cbind(table_cover, table_density, table_height, table_loading, table_bulkDens)
  fuel$stat <- c('min', 'mean', 'max')
  #table_list <- list(cover = table_cover, 
  #                   density = table_density, 
  #                   height = table_height, 
  #                   loading = table_loading, 
   #                  bulkDens = table_bulkDens)
  return(fuel)
  #fuels <- lapply(var_list, sapply, stats)
  #return(fuels)
}
fuel_wide <- fuels_please(list = var_list)

fuel_long <- fuel_wide %>%
  gather(key = component, value = value, -stat) %>%
  spread(key = stat, value = value) %>%
  separate(col = component, into = c("Variable", "Category", "Component"), sep = "\\_")


fuel_order <- fuel_long[c('Variable', 'Category', 'Component', 'min', 'mean', 'max')]

fuels_please(data = data, list = var_list, region = 'JP', phase = 2, treatment1 = 'CO')

