# Getting world polygons
polygons <- ne_countries(returnclass = "sf")

# Check for the presence of polygon data file and extract data from web if file is unavailable (Source: https://github.com/wmgeolab/geoBoundaries)
if(file.exists('data/india.rds')) {
   polygons <- readRDS('data/india.rds')
} else {
  iso3 <- "IND"
  adm_level <- "ADM0"
  india <- st_read(fromJSON(content(GET(paste0("https://www.geoboundaries.org/api/current/gbOpen/", iso3, "/", adm_level, "/")), "text"))$simplifiedGeometryGeoJSON)
  saveRDS(india,'data/india.rds')
}

# Adding India polygons to world data
polygons <- bind_rows(polygons %>% filter(name != "India"), india %>% mutate(name = "India"))

# Getting COVID-19 data (Source: https://covid19datahub.io/)
data <- covid19(polygons$name,verbose = FALSE) # Global COVID-19 data by country

# Converting cumulative values to absolute (Source: https://covid19datahub.io/articles/docs.html)
cumulative_to_individual <- function(x)
{
  if(length(x) > 1)
  {
    abs(x[2:length(x)] - x[1:(length(x) - 1)])
  } else {
    cat("Vector length not sufficient!\n")
  }
}
manipulated_data <- rbind(data[1, c(3:9, 11:21)], apply(data[, c(3:9, 11:21)], 2, cumulative_to_individual))
manipulated_data <- cbind(data[, c(1:2)], manipulated_data, data[, c(10, 22:ncol(data))])
data <- cbind(1:nrow(data),data)
colnames(data)[1] <- "index"
grouped_data <- data %>% group_split(id)
replace_values <- lapply(grouped_data,function(x){head(x,1)})
replace_values <- do.call(rbind,replace_values)
replace_values <- replace_values[order(replace_values$index),]
manipulated_data[replace_values$index,] <- data[replace_values$index,-1]
manipulated_data <- manipulated_data %>% select(colnames(manipulated_data)[c(1:5,32)])
colnames(manipulated_data) <- str_to_title(gsub("_", " ", colnames(manipulated_data)))
  
# Removing objects that are not required
rm(data,grouped_data,replace_values,cumulative_to_individual)