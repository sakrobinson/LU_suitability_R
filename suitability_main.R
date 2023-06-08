library(rgdal)
library(sp)

# Set up workspace
workspace <- "/my/path/to/workspace/"
setwd(workspace)

# Create empty lists to store relevant information
residential.suitability <- list()
commercial.suitability <- list()
industrial.suitability <- list()

# Loop over the input polygons
for (i in 1:length(list.files())) {
  
  # Read in the shapefile
  shapefile <- readOGR(".", i)
  
  # Extract the spatial features
  sf <- sp(shapefile)@layers[[1]]
  
  # Determine the field names corresponding to each urban land use layer
  if (!is.na(attr(sf$name))) {
    name <- attr(sf$name)[1]
    if ("Urban Land Use - Residential" == name) {
      suitable.fields <- c("Population Density", "Distance To Park", "Lot Size")
    } else if ("Urban Land Use - Commercial" == name) {
      suitable.fields <- c("Job Density", "Parking Availability", "Proximity To Major Roads")
    } else if ("Urban Land Use - Industrial" == name) {
      suitable.fields <- c("Zoning Regulations Compliance", "Water Accessibility", "Air Quality Index")
    } else {
      stop("Invalid Urban Land Use Layer.")
    }
  } else {
    stop("Urban Land Use Layers must contain valid field names.")
  }
  
  # Loop over each feature in the layer
  for (j in seq_along(sf)) {
    
    # Initialize scores based on each relevant factor
    population.density <- NA
    distance.park <- NA
    lot.size <- NA
    job.density <- NA
    parking.availability <- NA
    major.road.proximity <- NA
    zoning.compliance <- NA
    water.accessibility <- NA
    air.quality <- NA
    
    
    # TODO: continue loop logic and fill in
    }
    
## Initialize scores based on each relevant factor
population.density <- sf[suitable.fields["Population Density"], j]$value
distance.park <- sf[suitable.fields["Distance To Park"], j]$value / 5000 * sqrt((as.numeric(crs::projParm(getInfoField(x="wkt", shp_paths[[i]]), "lat_ts"))[1]/180)*pi)
lot.size <- sqrt(apply(sf[suitable.fields[c("Sidewalk Coverage","Mixed-Use Zoning","Street Connectivity"), j], ], c(2,4,6), as.numeric))/(sqrt(pi*(min(appearances)/max(appearances))))*990 # adjust to match the units used elsewhere
job.density <- sf[suitable.fields[c("Employment Opportunities","Demand Generation: Retail Trade"), j, "$job"]*$value
parking.availability <- min(1, max(0, df$Business - df$NumberOfParkingSpacesInBuildings)) # dummy variable
major.road.proximity <- rnorm(1)*100 # simulate noise by adding random number between -50 and 50
zoning.compliance <- round(randomRunif(1)*100,0)/100 # add more rules as necessary
water.accessibility <- round(sum(as.character(as.matrix(which(abs(appears)==3)))!=0),0)/30 # divide by total number of appearances to get proportion of "Yes" answers
air.quality <- round(sum(as.character(as.matrix(which(abs(noise_score) >= 70)) != 0)),0)/total_scores/100

# Calculate final score
total_scores <- sum(suitable.fields[,j]*(values))

## Store results for residential suitability
df_sub <- data.frame(Location = paste0(sf@data$layer, rep(seq_len(nrow(sf@data)), sapply(x = 1:(ncol(df)-3), function(k) length(names(df)[grepl("^suburbs_", names(df)[k])]))), LocationCode = unique(paste(unique(df[,c("Suburbs_Primary", "Suburbs_Secondary")]), sep = "")), TotalScore = values)
df_sub <- df_sub %>% select(-"TotalScore")
saveRDS(df_sub, file = paste0("ResidentialSuitabilityScores.rds"))

## Repeat for remaining factors
}

## Save results in suitable format
```sql
rm(disturbances, suburbs_primary, suburbs_secondary, residential, suburb_zone, jobs_per_km2, road_network, transit_service, sidewalks, mixed_use_land, employment_opportunity, distance_to_services, bicycle_facilities, local_centre_scale, density, bus_service_level, public_parks_trails_natural_areas, accessibility, services_proximity_of_public_spaces, street_design_traffic_calming, building_height, buildings_footprint, block_interior_streetlighting, tree_cover, recreational_facility_accessibility)

return
}
