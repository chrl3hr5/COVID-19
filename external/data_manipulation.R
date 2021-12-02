# COVID-19 data
`Cumulative to Individual` <- function(x)
{
  if(length(x) > 1)
  {
    abs(x[2:length(x)] - x[1:(length(x) - 1)])
  } else {
    cat("Vector length not sufficient!\n")
  }
}
`Manipulated Data` <- rbind(Data[1, c(3:9, 11:21)], apply(Data[, c(3:9, 11:21)], 2, `Cumulative to Individual`))
`Manipulated Data` <- cbind(Data[, c(1:2)], `Manipulated Data`, Data[, c(10, 22:ncol(Data))])
Data <- cbind(1:nrow(Data),Data)
colnames(Data)[1] <- "index"
grouped_data <- Data %>% group_split(Id)
replace_values <- lapply(grouped_data,function(x){head(x,1)})
replace_values <- do.call(rbind,replace_values)
replace_values <- replace_values[order(replace_values$index),]
`Manipulated Data`[replace_values$index,] <- Data[replace_values$index,-1]
rm(grouped_data,replace_values,`Cumulative to Individual`)

# Polygon data
Positions <- as.data.frame(cbind(rep(unique(`Manipulated Data`$Id), times = length(wrld_simpl@data$ISO3)), rep(as.character.factor(wrld_simpl@data$ISO3), each = length(unique(`Manipulated Data`$Id)))))
colnames(Positions) <- c("Id","ISO3")
Unique <- length(unique(Positions$Id))
Locations <- NULL
for (i in 1:(nrow(Positions) / Unique))
{
  Locations[i] <- if (length(which(unname(apply(Positions[1:Unique, ], 1, function(t) {
    length(unique(t)) == 1
  })))) == 1) {
    which(unname(apply(Positions[1:Unique, ], 1, function(t) {
      length(unique(t)) == 1
    })))
  } else {
    NA
  }
  Positions <- tail(Positions, -Unique)
}