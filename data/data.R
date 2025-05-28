# Check for the presence of polygon data file and extract data from web if file is unavailable
if(file.exists('data/simplified_polygons.rds')) {
  polygons <- readRDS('data/simplified_polygons.rds')
} else {
  # API to obtain up-to-date boundaries (Source: https://github.com/wmgeolab/geoBoundaries)
  url <- "https://api.github.com/repos/wmgeolab/geoBoundaries/contents/sourceData/gbOpen"
  
  # Obtaining country codes
  country_codes <- unique(substr(sapply(fromJSON(content(GET(url), "text"))$name, as.character), 1, 3))
  
  # Obtaining polygons for world countries
  polygons <- data.frame()
  adm_level <- "ADM0"
  for(i in 1:length(country_codes)) {
    iso3 <- country_codes[i]
    try(polygons <- rbind(polygons,st_read(fromJSON(content(GET(paste0("https://www.geoboundaries.org/api/current/gbOpen/", iso3, "/", adm_level, "/")), "text"))$simplifiedGeometryGeoJSON)))
    Sys.sleep(2) # 2 seconds delay between requests
  }
  
  # Saving data
  saveRDS(polygons,'data/polygons.rds')
  
  # Simplifying polygon data
  sf_use_s2(FALSE)
  polygons_valid <- st_make_valid(polygons)
  polygons_projected <- st_transform(polygons_valid, crs = 3857)
  simplified_projected <- st_simplify(polygons_projected, dTolerance = 1000, preserveTopology = TRUE)
  simplified_sf <- st_transform(simplified_projected, crs = st_crs(polygons))
  
  # Saving data
  saveRDS(simplified_sf,'data/simplified_polygons.rds')
}

# Getting COVID-19 data
data <- covid19(polygons$shapeName,verbose = FALSE) # Global COVID-19 data by country

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