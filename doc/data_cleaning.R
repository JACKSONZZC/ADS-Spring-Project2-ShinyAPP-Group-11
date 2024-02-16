# Install library for data loading
install.packages("tidyverse")
install.packages("shiny")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("shinydashboard")
install.packages("tidyr")
install.packages("leaflet")
install.packages("plotly")
install.packages("sf")
install.packages("readr")
install.packages("shinyWidgets")
library(tidyverse)
library(shiny)
library(leaflet)
library(lubridate)


# Choose working directory and load csv file
disaster_data <- read_csv("FemaWebDisasterDeclarations.csv")
owner_data <- read_csv("HousingAssistanceOwners.csv")
renter_data <- read_csv("HousingAssistanceRenters.csv")

# Data Cleaning

# Standardize the zipcode in renter_data
renter_data$zipCode <- as.character(renter_data$zipCode)
renter_data$zipCode <- sprintf("%05s", renter_data$zipCode) #Standardize zipCode to ensure it has 5 digits

# For renter_data: Keep rows with zip codes that have exactly 5 digits
valid_renter_data <- renter_data %>% 
  filter(grepl("^\\d{5}$", zipCode))

# Standardize the zipcode in owner_data
owner_data$zipCode <- as.character(owner_data$zipCode)
owner_data$zipCode <- sprintf("%05s", owner_data$zipCode) 

# For owner_data: Keep rows with zip codes that have exactly 5 digits
valid_owner_data <- owner_data %>% 
  filter(grepl("^\\d{5}$", zipCode))

# Checking duplicates 
# renter_data duplicates
unique_renter_data <- valid_renter_data[!duplicated(valid_renter_data),]

# owner_data duplicates
unique_owner_data <- valid_owner_data[!duplicated(valid_owner_data),]

# disaster_data duplicates
unique_disaster_data <- disaster_data[!duplicated(disaster_data),]

# Column renaming 
# Replace "County" with empty string in renter_data
renter_data_cleaned <- unique_renter_data %>%
  mutate(county = gsub("\\s*\\(County\\)", "", county, ignore.case = TRUE))

# Change renter_data columns' names for widget design.
renter_data_renamed <- renter_data_cleaned %>%
  rename(
    `Disaster Number` = disasterNumber,
    `State` = state,
    `City` = city,
    `County` = county,
    `Zip Code` = zipCode,
    `Valid Registrations` = validRegistrations,
    `Total Inspected` = totalInspected,
    `Inspected with No Damage` = totalInspectedWithNoDamage,
    `Total with Moderate Damage` = totalWithModerateDamage,
    `Total with Major Damage` = totalWithMajorDamage,
    `Total with Substantial Damage` = totalWithSubstantialDamage,
    `Approved for FEMA Assistance` = approvedForFemaAssistance,
    `Total Approved IHP Amount` = totalApprovedIhpAmount,
    `Repair/Replace Amount` = repairReplaceAmount,
    `Rental Amount` = rentalAmount,
    `Other Needs Amount` = otherNeedsAmount,
    `Approved between $1 and $10000` = approvedBetween1And10000,
    `Approved between $10001 and $25000` = approvedBetween10001And25000,
    `Approved between $25001 and Max` = approvedBetween25001AndMax
  ) 

# Replace "County" with empty string in owner_data
owner_data_cleaned <- unique_owner_data %>%
  mutate(county = gsub("\\s*\\(County\\)", "", county, ignore.case = TRUE))

# Change owner_data columns' names for widget design.
owner_data_renamed <- owner_data_cleaned %>%
  rename(
    `Disaster Number` = disasterNumber,
    `State` = state,
    `City` = city,
    `County` = county,
    `Zip Code` = zipCode,
    `Valid Registrations` = validRegistrations,
    `Average FEMA Inspected Damage` = averageFemaInspectedDamage,
    `Total Inspected` = totalInspected,
    `Total Damage` = totalDamage,
    `No FEMA Inspected Damage` = noFemaInspectedDamage,
    `FEMA Inspected Damage between $1 and $10000` = femaInspectedDamageBetween1And10000,
    `FEMA Inspected Damage between $10001 and $20000` = femaInspectedDamageBetween10001And20000,
    `FEMA Inspected Damage between $20001 and $30000` = femaInspectedDamageBetween20001And30000,
    `FEMA Inspected Damage > $30000` = femaInspectedDamageGreaterThan30000,
    `Approved for FEMA Assistance` = approvedForFemaAssistance,
    `Total Approved IHP Amount` = totalApprovedIhpAmount,
    `Repair/Replace Amount` = repairReplaceAmount,
    `Rental Amount` = rentalAmount,
    `Other Needs Amount` = otherNeedsAmount,
    `Approved between $1 and $10000` = approvedBetween1And10000,
    `Approved between $10001 and $25000` = approvedBetween10001And25000,
    `Approved between $25001 and Max` = approvedBetween25001AndMax
  )

# Cleaning disaster_data
# Standardizing 'incidentBeginDate'
disaster_data$incidentBeginDate <- as.Date(disaster_data$incidentBeginDate)
disaster_data$incidentBeginDate <- format(disaster_data$incidentBeginDate, "%m-%d-%Y")

# Standardizing 'incidentEndDate'
disaster_data$incidentEndDate <- as.Date(disaster_data$incidentEndDate)
disaster_data$incidentEndDate <- format(disaster_data$incidentEndDate, "%m-%d-%Y")

# Standardizing 'declarationDate'
disaster_data$declarationDate <- as.Date(disaster_data$declarationDate)
disaster_data$declarationDate <- format(disaster_data$declarationDate, "%m-%d-%Y")

# Standardizing 'closeoutDate'
disaster_data$closeoutDate <- as.Date(disaster_data$closeoutDate)
disaster_data$closeoutDate <- format(disaster_data$closeoutDate, "%m-%d-%Y")

# Changing columns' names for disaster_data for widget design
disaster_data_renamed <- disaster_data %>%
  rename(
    `Disaster Number` = disasterNumber,
    `Declaration Date` = declarationDate,
    `Disaster Name` = disasterName,
    `Incident Begin Date` = incidentBeginDate,
    `Incident End Date` = incidentEndDate,
    `Declaration Type` = declarationType,
    `State Code` = stateCode,
    `State Name` = stateName,
    `Incident Type` = incidentType,
    `Entry Date` = entryDate,
    `Update Date` = updateDate,
    `Closeout Date` = closeoutDate,
    `Region` = region
  )

# Remove Irrelevant columns of disaster_data
cleaned_disaster_data <- disaster_data_renamed %>%
  select(
    `Disaster Number`,
    `Declaration Date`,
    `Disaster Name`,
    `Incident Begin Date`,
    `Incident End Date`,
    `Declaration Type`,
    `State Code`,
    `State Name`,
    `Incident Type`,
    `Closeout Date`,
    `Region`
  )

# Merge the renter and owner dataset
merged_data <- inner_join(owner_data_renamed, renter_data_renamed, by = c("Disaster Number", "Zip Code", "County", "City", "State"), relationship = "many-to-many")

# Clean merged_data
# Rename the merged column names
# Rename columns ending with .x to have .owner and .y to have .renter
merged_data_renamed <- merged_data %>%
  rename_with(~ gsub("\\.x$", " of Owner", .x), .cols = ends_with(".x")) %>%
  rename_with(~ gsub("\\.y$", " of Renter", .x), .cols = ends_with(".y"))

# Removed some irrelevant columns
cleaned_merged_data <- merged_data_renamed %>%
  select(
    -`id of Owner`,
    -`id of Renter`
  )

# Export cleaned datasets
# Export disaster_renamed to a CSV file
write.csv(cleaned_disaster_data, "disaster_meta_data.csv", row.names = FALSE)

# Export owner_renamed to a CSV file
write.csv(owner_data_renamed, "owner_renamed.csv", row.names = FALSE)

# Export renter_renamed to a CSV file
write.csv(renter_data_renamed, "renter_renamed.csv", row.names = FALSE)

# Export the merged dataset for renter and owner
write.csv(cleaned_merged_data, "merged_data.csv", row.names = FALSE)