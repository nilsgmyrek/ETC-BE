# Clean environment
rm(list = ls())
gc()

# Load necessary libraries
library(readxl)
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

# Read the Natura 2000 species code list
natura_species_list <- read_excel("/Users/nr72kini/Desktop/ETC_BE/1.1.1.3 - Species Support/Natura2000 species code list v2021 revised 202201-2.xlsx", 
                                  sheet = 'Species_codelist_2021')

# Create subset for 'Addition' data
natura_added <- natura_species_list %>%
  filter(`Update status` == 'Addition')
# Create subset for 'Remains' data - no need to process 
natura_remains <- natura_species_list %>%
  filter(`Update status` == 'Remains')
# Create subset for 'Deletion' data 
natura_deleted <- natura_species_list %>%
  filter(`Update status` == 'Deletion')

# Function to search for species in Catalogue of Life (COL) API with type filtering
search_COL_species <- function(query, rank = "species", content = "SCIENTIFIC_NAME", search_type = "prefix", fuzzy = T) {
  base_url <- "https://api.checklistbank.org/dataset/COL2024/nameusage/search"
  
  # Build query parameters dynamically based on provided inputs
  query_params <- list(q = query)
  
  if (!is.null(rank)) {
    query_params$rank <- rank  # Correct assignment of rank
  }

  if (!is.null(content)) {
    query_params$content <- content  # Correct assignment of content
  }

  if (!is.null(search_type)) {
    query_params$type <- search_type  # Correct assignment of search_type
  }

  if (!is.null(fuzzy)) {
    query_params$fuzzy <- fuzzy  # Correct assignment of fuzzy
  }
  
  # Make GET request to the API
  response <- GET(base_url, query = query_params)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse JSON content
    content_json <- content(response, as = "text", encoding = "UTF-8")
    content_parsed <- fromJSON(content_json, flatten = TRUE)
    
    # Extract useful information from the response
    results <- content_parsed$result
    
    # Return results
    return(results)
  } else {
    # If the request failed, return the status code and message
    stop(paste("Failed to retrieve data. Status code:", status_code(response)))
  }
}


# Example usage of the search function
# Search for species "Vulpes vulpes" with the default search parameters
species_results_exact <- search_COL_species(query = 'Egretta albas') # 'Egretta alba' officially 
species_results_exact$usage.accepted.label
rm(species_results_exact) 

# Create subset with 'Deletion' Data which have Information on Alternative Scientific Name
deleted_withName <- natura_deleted %>%
  filter(!is.na(`Alternative scientific name`)) %>%
  select(`Species code`, `Species name`, `Update status`, `Alternative scientific name`)

# Create subset with 'Deletion' Data which have Information on Alternative Scientific Name
deleted_withoutName <- natura_deleted %>%
  filter(is.na(`Alternative scientific name`)) %>%
  select(`Species code`, `Species name`, `Update status`, `Alternative scientific name`)

## WITHOUT NAMES ####
# Initialize an empty list to store results for each species
# CoL_deleted_withoutName <- list()
# 
# # Loop API-Query through Natura2000 Species-List
# for (i in 1:nrow(deleted_withoutName)) {
#   species_name <- deleted_withoutName$`Species name`[i]
#   
#   # Search for each species in the Catalogue of Life
#   species_results <- search_COL_species(query = species_name)
#   
#   # Store the results in the list, with species name as the key
#   CoL_deleted_withoutName[[species_name]] <- species_results
#   
#   # Print progress
#   print(paste("Processed species:", species_name))
# }
# 
# # Convert the list of results into a data frame  
# CoL_deleted_withoutName_df <- bind_rows(CoL_deleted_withoutName, .id = "Species") 
# 
# # Perform the join by linking Species name and provided ScientificName
# CoL_deleted_withoutName <- CoL_deleted_withoutName_df %>%
#   left_join(deleted_withoutName, by = c("Species" = "Species name"), relationship = "many-to-many")%>%
#    select(Species, usage.status, usage.name.scientificName, usage.accepted.status,
#           usage.accepted.label, usage.accepted.name.scientificName, usage.accepted.name.authorship, classification, usage.accepted.name.genus, usage.accepted.name.specificEpithet,
#           usage.accepted.name.rank)
# 
# # Clean container
# rm(CoL_deleted_withoutName_df, species_results, i, species_name)
# 
# 
# # Create subset - synonym
# CoL_deleted_withoutName_syn <- CoL_deleted_withoutName %>%
#   filter(usage.status == 'synonym')
# # Create subset - accepted  
# CoL_deleted_withoutName_acc <- CoL_deleted_withoutName %>%
#   filter(usage.status == 'accepted')
# # Create subset - misapplied
# CoL_deleted_withoutName_mis <- CoL_deleted_withoutName %>%
#   filter(usage.status == 'misapplied')
# # Create subset - ambiguous synonym
# CoL_deleted_withoutName_as <- CoL_deleted_withoutName %>%
#   filter(usage.status == 'ambiguous synonym')
# # Create subset - provisionally accepted
# CoL_deleted_withoutName_pa <- CoL_deleted_withoutName %>%
#   filter(usage.status == 'provisionally accepted')
# # Create subset - NA
# CoL_deleted_withoutName_na <- CoL_deleted_withoutName %>%
#   filter(is.na(usage.status)) # = 0
# 

## WITH NAMES ####
# Initialize a list to store species results
# Initialize lists to store species results
CoL_deleted_withName <- list()
CoL_deleted_withName_alt <- list()

# Loop through each row of the Natura2000 species list
for (i in 1:nrow(deleted_withName)) {
  # Extract primary species name and store the result
  primary_name <- deleted_withName$`Species name`[i]
  
  # Run API query for the primary name
  primary_results <- search_COL_species(query = primary_name)
  CoL_deleted_withName[[primary_name]] <- primary_results
  
  # Print progress for the primary name
  print(paste("Processed primary species:", primary_name))
  
  # Check if there are alternative scientific names and split if needed
  alternative_names <- strsplit(deleted_withName$`Alternative scientific name`[i], ",\\s*")[[1]]
  
  for (name in alternative_names) {
    # Run API query for each alternative name
    alt_results <- search_COL_species(query = name)
    
    # Store the results in the list, using each alternative name as the key
    CoL_deleted_withName_alt[[name]] <- alt_results
    
    # Print progress for the alternative names
    print(paste("Processed alternative species:", name))
  }
}

# Define the function to convert list of results to a data frame with source labels
convert_results_to_df <- function(results_list, source_label) {
  # Bind rows and add source and original name columns
  results_df <- bind_rows(results_list, .id = "QueryName") %>%
    mutate(Source = source_label)  # Add label for whether it was a primary or alternative search
  return(results_df)
}


# Convert the results to data frames with source labels
CoL_primary_df <- convert_results_to_df(CoL_primary_results, "Primary")
CoL_alternative_df <- convert_results_to_df(CoL_alternative_results, "Alternative")

# Combine the primary and alternative results into a single data frame
CoL_all_results <- bind_rows(CoL_primary_df, CoL_alternative_df)

# Join the results back with original deleted species information
final_results <- deleted_withName %>%
  rename(QueryName = `Species name`) %>%
  left_join(CoL_all_results, by = "QueryName")


# Convert the list of results into a data frame 
CoL_deleted_withName <- bind_rows(CoL_deleted_withName, .id = "Species")

# Clean environment 
rm(primary_results,alt_results, alternative_names, name, i, primary_name)


# Perform the join by linking Species name and provided ScientificName
CoL_deleted_withName_df <- deleted_withName %>%
  left_join(CoL_deleted_withName, by = c("Species name" = "Species"), relationship = "many-to-many") %>%
  mutate(is_same_name = `Species name` == usage.name.scientificName) %>%
  select(`Species code`, `Species name`, `Update status`, `Alternative scientific name`, usage.status, usage.name.scientificName, usage.accepted.status, 
         usage.accepted.label, usage.accepted.name.scientificName, usage.accepted.name.authorship, classification, usage.accepted.name.genus, usage.accepted.name.specificEpithet,
         usage.accepted.name.rank, is_same_name)


# Create Subset of Species to process
CoL_deleted_withName_acc <- CoL_deleted_withName_df %>%
  filter(usage.status == 'accepted') 

CoL_deleted_withName_syn <- CoL_deleted_withName_df %>%
  filter(usage.status == 'synonym') 

CoL_deleted_withName_syn_n <- CoL_deleted_withName_df %>%
  filter(usage.status == 'synonym') %>%
  filter(`Alternative scientific name` == usage.accepted.name.scientificName) 

CoL_deleted_withName_syn_y <- CoL_deleted_withName_df %>%
  filter(usage.status == 'synonym') %>%
  filter(`Alternative scientific name` != usage.accepted.name.scientificName) 

CoL_deleted_withName_as <- CoL_deleted_withName_df %>%
  filter(usage.status == 'ambiguous synonym') 

CoL_deleted_withName_na <- CoL_deleted_withName_df %>%
  filter(is.na(usage.status))


# CoL_deleted_withName_na
# Initialize a list to store species results
CoL_deleted_withName_na_list <- list()

# Loop through each row of the Natura2000 species list
for (i in 1:nrow(CoL_deleted_withName_na)) {
  # Extract alternative scientific names, and split them if they contain commas
  alternative_names <- strsplit(CoL_deleted_withName_na$`Alternative scientific name`[i], ",\\s*")[[1]]
  
  for (name in alternative_names) {
    # Run API query for each alternative name
    primary_results <- search_COL_species(query = name)
    
    # Store the results in the list, using each alternative name as the key
    CoL_deleted_withName_na_list[[name]] <- primary_results
    
    # Print progress
    print(paste("Processed species:", name))
  }
}

# Convert the list of results into a data frame 
CoL_deleted_withName_na_df <- bind_rows(CoL_deleted_withName_na_list, .id = "Species")

# Clean environment 
rm(primary_results, i, primary_name, name)

# Perform the join by linking Species name and provided ScientificName
CoL_deleted_withName_df <- deleted_withName %>%
  left_join(CoL_deleted_withName, by = c("Species name" = "Species"), relationship = "many-to-many") %>%
  mutate(is_same_name = `Species name` == usage.name.scientificName) %>%
  select(`Species code`, `Species name`, `Update status`, `Alternative scientific name`, usage.status, usage.name.scientificName, usage.accepted.status, 
         usage.accepted.label, usage.accepted.name.scientificName, usage.accepted.name.authorship, classification, usage.accepted.name.genus, usage.accepted.name.specificEpithet,
         usage.accepted.name.rank, is_same_name)




View(deleted)

# Create Subset of Species for Rule 1.
unique_rows <- deleted %>%
  filter(is_same_name == T)  %>%
  group_by(`Species code`, `Species name`) %>%
  filter(n() == 1) %>%
  ungroup()

# Function to extract genus and species epithet (excluding subspecies)
extract_genus_species <- function(species_name) {
  # Use a regular expression to capture only the genus and species epithet
  genus_species <- str_extract(species_name, "^[A-Za-z]+\\s[A-Za-z]+")
  
  return(genus_species)
}

# Create an empty data frame with predefined column names and types
output_Rule1_ <- data.frame(
  old_code = character(nrow(unique_rows)),                # Species code from the old data
  old_scientificName = character(nrow(unique_rows)),      # Old scientific name
  new_code = character(nrow(unique_rows)),                # New species code (if available)
  new_scientificName = character(nrow(unique_rows)),      # New scientific name
  canonicalName = character(nrow(unique_rows)),           # Canonical name (genus + species epithet)
  authorship = character(nrow(unique_rows)),              # Authorship Information
  status = character(nrow(unique_rows)),                  # Status of the species (accepted, synonym, etc.)
  acceptedScientificName = character(nrow(unique_rows)),  # New accepted scientific name with authorship
  ruleApplied = character(nrow(unique_rows)),             # Rule applied (e.g., 'Simple Name and Code Update')
  classification = character(nrow(unique_rows)),          # Classification information (taxonomy)
  stringsAsFactors = FALSE                            # Ensures character columns remain as characters, not factors
)

# Test loop
i <- 1

# Loop throught each row of subset Data
for (i in 1:nrow(unique_rows)) {
  
  # Fill in the old scientific-Name and Species Code
  output_Rule1$old_code[i] <- unique_rows$`Species code`[i]
  output_Rule1$old_scientificName[i] <- unique_rows$`Species name`[i]
  
  # Search and Fill in the new scientific-Name with and without Author
  output_Rule1$acceptedScientificName[i] <- unique_rows$usage.accepted.label[i]  
  output_Rule1$new_scientificName[i] <- unique_rows$usage.accepted.name.scientificName[i]
  
  # Set Canonical Name
  output_Rule1$canonicalName[i] <- extract_genus_species(unique_rows$usage.accepted.name.scientificName[i])
  
  # Search for new Species Code using either canonicalName or new_scientificName
  species_code <- natura_species_list %>%
    filter((`Species name` == output_Rule1$canonicalName[i] | 
              `Species name` == output_Rule1$new_scientificName[i]) & 
             `Update status` != 'deleted') %>%
    select(`Species code`) %>%
    pull()  # Pull the result out as a vector
  
  # If species_code is empty, set it to "species not listed"
  output_Rule1$new_code[i] <- ifelse(length(species_code) > 0, species_code, "not listed")
  
  # Add authorship and Status
  output_Rule1$authorship[i] <- unique_rows$usage.accepted.name.authorship[i]
  output_Rule1$status[i] <- unique_rows$usage.status[i]
  
  # Apply the rule name as 'Simple Name and Code Update'
  output_Rule1$ruleApplied[i] <- '1. Simple Name and Code Update'
  
  # Copy the classification info
  output_Rule1$classification[i] <- unique_rows$classification[i]
  
  }
