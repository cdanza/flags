library("tidyverse")

# Create dataframe from downloaded dataset, and adding column names from the documentation
df <- read_csv("rawdata/flag.csv",
      col_names = c("name", "landmass","zone", "area", "population", "language", "religion", "bars", "stripes", "colours", "red", "green", "blue", "gold", "white", "black", "orange", "mainhue", "circles", "crosses", "saltires", "quarters", "sunstars", "crescent", "triangle", "icon", "animate", "text", "topleft", "botright"))

print(df)

# Creating clean file in new subfolder
write_csv(df, "data/clean_flag.csv")