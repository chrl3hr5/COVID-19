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
for (i in 1:length(unique(`Manipulated Data`$Id)))
{
  `Manipulated Data`[head(which(`Manipulated Data`$Id == unique(`Manipulated Data`$Id)[i]), 1), ] <- Data[head(which(`Manipulated Data`$Id == unique(`Manipulated Data`$Id)[i]), 1), ]
}

# Polygon data
Positions <- as.data.frame(cbind(rep(unique(`Manipulated Data`$Id), times = length(wrld_simpl@data$ISO3)), rep(as.character.factor(wrld_simpl@data$ISO3), each = length(unique(`Manipulated Data`$Id)))))
Unique <- length(unique(Positions$V1))
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