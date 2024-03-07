install.packages("ggmap")

library(sp)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(spdep)
library(here)
library(dplyr)
library(ggplot2)
library(ggmap)
library(dplyr)
library(spgwr)
library(readr)
library(skimr)


#Reading in my data, first my UK LSOA shapefile and then my LSOA lookup file
UK_LSOAS <- st_read(("LSOA_2021_EW_BGC.shp"))
LSOA_LOOKUP <- read.csv('LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales_(Version_2).csv')

#Checking to see that my shapefile is set to British National Grid (epsg:27700)
summary(UK_LSOAS)

#Here I am filtering my data to all rows with 'Nottingham' in the column 'LAD22NM'
#This will enable me to subset for just Nottingham data
NOTTS_LOOKUP <- LSOA_LOOKUP[grep('Nottingham', LSOA_LOOKUP$LAD22NM), ]

#Now I am merging my data based on the common shared colunmn 'LSOA21CD'
merged_LSOA <- merge(UK_LSOAS, NOTTS_LOOKUP, by.x = "LSOA21CD", by.y = "LSOA21CD")

#Checking if my data will plot now that it has been cut down
#Getting an initial map of the research area
tm_shape(merged_LSOA) + tm_polygons()



merged_LSOA <- st_read(here::here("Documents/full_merged.shp"))


#_____________________________________________________________________
#HEALTHCARE DATA

health_centers <- read.csv("ncc_Health_Centres.csv")

class(health_centers$POSTCODE)

geo_data <- ggmap::geocode(health_centers$POSTCODE)

register_google(key = "AIzaSyDhfholCLREgptr9AO4o12PmnA4Y27yAjI")

geo_data <- geocode(health_centers$POSTCODE)

merged_healthcenters <- cbind(health_centers, geo_data)

# Save the merged data to a new file
write.csv(merged_healthcenters, here::here("health_data", "merged_healthcenters.csv"), row.names = FALSE)

health_centers <- read.csv("healthcentres_notts.csv")

#making this file into a spatial features object in order to plot it on the map
health_centers_sf <- st_as_sf(health_centers, coords = c("lon", "lat"), crs = 4326)

#tranforming the CRS into BNG as I had previously set it as WGS84
health_centers_bng <- st_transform(health_centers_sf, 27700)

#Visualising the health centers on the nottingham map
tm_shape(merged_LSOA) + 
  tm_polygons() +
  tm_shape(health_centers_sf) +
  tm_dots(col = "red", size = 0.5)

#________________________________________________________________________
#Reading in the Indices of Multiple Deprivation csv--------
#
IOD <- read.csv("All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv")

#Filter down to only rows with nottingham in them
IOD_NOTTS <- IOD[grep('Nottingham', IOD$Local.Authority.District.name..2019.), ]

#renaming a column in IOD_NOTTS file to 'LSOA21CD', so that i can join my two datatsets easier
IOD_NOTTS <- IOD_NOTTS %>%
  rename(LSOA21CD = LSOA.code..2011.)

#merging the IOD data file with the LSOA file to create a new sf df
merged_IOD_LSOA <- merged_LSOA %>%
  left_join(IOD_NOTTS, by = "LSOA21CD")

#there are some duplicates so I am removing these
duplicated_rows <- merged_IOD_LSOA[duplicated(merged_IOD_LSOA$LSOA21CD), ]
merged_IOD_LSOA <- merged_IOD_LSOA[!duplicated(merged_IOD_LSOA$LSOA21CD), ]

#saving the file to my files just in case
write.csv(merged_IOD_LSOA, "merged_IOD_LSOA.csv", row.names = FALSE)

merged_IOD_LSOA <- read.csv("merged_IOD_LSOA.csv")

#visualising the data to check that the file has kept its geometry and will plot
tm_shape(merged_LSOA) + tm_polygons()

#-------------------------------------------------------------------------
#GREENSPACE AREA ANALYSIS

#reading in greenspace csv
greenspace <- read.csv("ncc_OpenSpaces.csv")

#finding out the names of the columns in greenspace file  so i can correctly copy them
column_names <- colnames(greenspace)
print(column_names)

#filtering my greenspaces to areas above 0.25 hectares only in the AREA_HA column
greenspace_filtered <- greenspace %>%
  filter(SITE_AREA_HA >= 0.25)

#creating a greenspace shapefile from the Easting and Northing coordinates
#setting CRS to BNG as i load in the file
#note: this file is called greenspace_sf2 as I initially loaded the shapefile and then removed it later
greenspace_sf <- st_as_sf(greenspace_filtered, coords = c("EASTING", "NORTHING"), crs = st_crs(27700))

#checking to see how these greenspaces plot
tm_shape(merged_LSOA) + 
  tm_polygons() +
  tm_shape(greenspace_sf) +
  tm_dots(col = "darkgreen", size = 0.05) +
  tm_layout(title = "Grrenspace Points")

#I can see here that there are some points outside of the region's boundary so I am cookie-cuttering these away
clipped_points <- st_intersection(greenspace_sf, merged_LSOA)

# I am now performing a spatial join between my datasets
joined_data <- st_join(merged_LSOA, greenspace_sf, by = "LSOA21CD")

#plotting to ensure the points have been removed, it all looks good!
tm_shape(clipped_points) +
  tm_dots(col = "darkgreen", size = 0.05) +
  tm_shape(merged_LSOA) +
  tm_borders() +
  tm_layout(title = "Points of Greenspace in Nottingham")

#counting the number of points (greenspaces) present in each LSOA
points_count_per_LSOA <- joined_data %>%
  group_by(LSOA21CD) %>%
  summarise(points_count = n())

full_merged <- st_join(points_count_per_LSOA, merged_IOD_LSOA, join = st_equals)


#HEALTHCARE CENTRES----------------------------------------------------
#remove duplicates 
merged_LSOA <- merged_LSOA %>% distinct(LSOA21CD, .keep_all = TRUE)

LSOA_centroids <- merged_LSOA %>%
  st_centroid() %>%
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  st_coordinates()
LSOA_centroids <- as.data.frame(LSOA_centroids)
LSOA_centroids$id <- row_number(LSOA_centroids)  
# Plot the centroids
plot(LSOA_centroids, axes = TRUE)


LSOA_centroids_sf <- st_as_sf(LSOA_centroids, coords = c("X", "Y"), crs = 4326)
health_centers_sf

#find nearest health center and distance (np = index, np_dist = meters distance)
LSOA_centroids_sf %>% 
  group_by(id) %>%
  mutate(np = st_nearest_feature(geometry, health_centers_sf),
        dist_np = as.numeric( st_distance(geometry, health_centers_sf[np,])))


variables <- read_csv('variables.csv',show_col_types = FALSE)
head(variables, 5)
skim(variables)
library(car)
library(corrplot)
library(caTools)
library(quantmod)
library(MASS)

plot(variables$distances, variables$Health.Deprivation.and.Disability.Score, main="Scatterplot Example",
     xlab="distances", ylab="health index ", pch=19)

plot(variables$points_count, variables$Health.Deprivation.and.Disability.Score, main="Scatterplot Example",
     xlab="points count", ylab="health index ", pch=19)

variablesx <- variables[,3:5]                                       # independent variables 

var <- cor(variablesx)                                         # independent variables correlation matrix 

var_inv <- ginv(var)                                       # independent variables inverse correlation matrix 

colnames(var_inv) <- colnames(variablesx)                      # rename the row names and column names
rownames(var_inv) <- colnames(variablesx)

corrplot(var_inv,method='number',is.corr = F)
scale(variables)
#Building a regression model and putting output into 'modelMLR'
modelMLR <- lm(Health.Deprivation.and.Disability.Score ~ distances + points_count, data = variables)

# Include the 'scipen=7' argument in the summary() function remove those annoying scientific notation!
options(scipen = 7)
# summary() calls report the output stored in object 'modelMLR'
summary(modelMLR)

#Extracting the residuals and putting them into a new object
final_datafile$RESIDUALS <- modelMLR$residuals


main_map <- tm_shape(merged_LSOA) +
  tm_polygons() +
  tm_borders() +
  
  # tm_shape(clipped_points) +
  # tm_dots(col = "darkgreen", size = 0.05) +
  
  tm_shape(health_centers_sf) +
  tm_dots(col = "maroon", size = 0.6) +
  
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "top")) +
  
  tm_add_legend('symbol', 
                col = c('darkgreen',"maroon"),
                border.col = "grey40",
                labels = c('Green Space Count','Health Centres'),
                title = 'Legend')

main_map
#LINEAR MODEL-----------------------------------------------------------
model_data <- full_merged %>%
  st_drop_geometry() %>%
  select(Health.Deprivation.and.Disability.Score, Income.Score..rate., points_count)

# Log data
write.table(model_data, "logged_model_data.txt", sep = "\t", quote = FALSE, row.names = FALSE)

# Remove rows with missing values
model_data <- na.omit(model_data)

# Check if there are still any missing values
if (any(is.na(model_data))) {
  stop("Missing values detected. Please handle missing values before fitting the model.")
}

# Fit a linear regression model
lm_model <- lm(Health.Deprivation.and.Disability.Score ~ Income.Score..rate. + points_count, data = model_data)

# Print model summary
summary(lm_model)

#SPATAL LAG-------------------------------------------------------------

model_data <- full_merged %>%
  st_drop_geometry() %>%
  select(Health.Deprivation.and.Disability.Score, Income.Score..rate., points_count)

# Logging data
write.table(model_data, "logged_model_data.txt", sep = "\t", quote = FALSE, row.names = FALSE)

# Removing rows with missing values
model_data <- na.omit(model_data)

# Checking if there are still any missing values
if (any(is.na(model_data))) {
  stop("Missing values detected. Please handle missing values before fitting the model.")
}

# Loading spdep package to make sure it is properly installed
install.packages('spdep')
library(spdep)

#creating weights matrix
w <- poly2nb(full_merged, queen = TRUE)

# Add row names to spatial weights matrix
row.names(w) <- full_merged$ID

# Add row names to spatial weights matrix
lag_model <- spdep::lagsarlmtree(Health.Deprivation.and.Disability.Score ~ Income.Score..rate. + points_count,
                                 data = model_data,
                                 weights = w)

# Print model summary
summary(lag_model)

#maps at end:

#map of greenspace points
  tm_shape(clipped_points) +
  tm_dots(col = "darkgreen", size = 0.05) +
  tm_shape(merged_LSOA) +
  tm_borders() +
  tm_layout(
    title = "Points of Greenspace in Nottingham",
    title.position = c("center", "top"),
    title.size = 0.75,
    inner.margins = c(0.1, 0.1, 0.1, 0.1)  # Add margins to the layout
  )



