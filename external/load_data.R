# Loading data
data(wrld_simpl) # World polygons
Data <- covid19(verbose = FALSE) # Worldwide COVID-19 data by country
colnames(Data) <- gsub("_", " ", str_to_title(colnames(Data))) # Reformatting column names