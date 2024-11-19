# Clean environment
rm(list = ls())

# Load Necessary Libraries
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

# Import and Preprocess the Natura 2000 Species List
natura_species_list <- read_excel(
  "/Users/nr72kini/Desktop/ETC_BE/Github/ETC-BE/1.1.1.3 Species Support/Data/Natura2000 species code list v2021 revised 202201-2.xlsx", 
  sheet = "Species_codelist_2021"
)

# # Create scientificName Column for Catalogue of Life
scientificName <- natura_species_list$`Species name`

# Filter for species with status "Deletion" and retain necessary columns
natura_species_deleted <- natura_species_list %>%
  filter(`Update status` == "Deletion") %>%
  select(`Species code`, `Species name`, `Update status`, `Alternative scientific name`) %>%
  rename(
    old_code = `Species code`,
    old_scientificName = `Species name`,
    alt_scientificName = `Alternative scientific name`,
    UpdateStatus = `Update status`
  )

# Export the Prepared List for Catalogue of Life Matching
write.csv(
  scientificName, 
  "/Users/nr72kini/Desktop/ETC_BE/Github/ETC-BE/1.1.1.3 Species Support/Data/CoL.csv", 
  row.names = FALSE
)

# Import Name Matching Results from Catalogue of Life (COL)
CoL_results <- read.csv(
  "/Users/nr72kini/Desktop/ETC_BE/Github/ETC-BE/1.1.1.3 Species Support/Data/Namematch_result_COL24.10.csv"
)

# Perform the join operation
merged_data <- natura_species_deleted %>%
  left_join(CoL_results, by = c("old_scientificName" = "providedScientificName"), relationship = "many-to-many") %>%
  select(-c(matchRemark, taxonId, acceptedTaxonId, parentTaxonId))

# Display the cleaned data
print(merged_data, n = 10)

# Categorize Species Based on Taxonomic Resolution
resolved_species <- merged_data %>%
  mutate(
    category = case_when(
      status == "synonym" ~ "Synonym",
      status == "accepted" ~ "Accepted",
      status == "ambiguous synonym" ~ "Ambiguous",
      status == "misapplied" ~ "Misapplied",
      TRUE ~ "Unresolved"
    )
  )

#### Functions ####

# Function to divide scientific name and author, including special characters in author names
extract_species_info <- function(species_info) {
  # First, handle species name (genus + species + optional subspecies)
  # Stops before encountering the author (Linnaeus or author with special characters)
  species_pattern <- "^[A-Za-z]+\\s[A-Za-z]+(\\s[A-Za-z]+)?(?=\\s\\(|\\s[A-Za-zÀ-ÿ]+,|\\s[A-Za-zÀ-ÿ]+$)"
  species_name <- str_extract(species_info, species_pattern)
  
  # Handle author, with or without parentheses, including special characters in author names
  author_pattern <- "\\(.*\\)$|[A-Za-zÀ-ÿ]+,\\s\\d+|[A-Za-zÀ-ÿ]+$"
  author_info <- str_extract(species_info, author_pattern)
  
  # Clean up any extra spaces
  species_name <- trimws(species_name)
  author_info <- trimws(author_info)
  
  list(species_name = species_name, author_info = author_info)
}

# Function to extract genus and species epithet (excluding subspecies)
extract_genus_species <- function(species_name) {
  # Use a regular expression to capture only the genus and species epithet
  genus_species <- str_extract(species_name, "^[A-Za-z]+\\s[A-Za-z]+")
  
  return(genus_species)
}

# Create an empty data frame with predefined columns
output <- data.frame(
  old_code = character(nrow(resolved_species)),                # Species code from the old data
  old_scientificName = character(nrow(resolved_species)),      # Old scientific name
  old_UpdateStatus = character(nrow(resolved_species)),        # Old Update Status
  ScientificNameAuthorCoL = character(nrow(resolved_species)), # New accepted scientific name with authorship
  TaxonomicStatus = character(nrow(resolved_species)),         # Status of the species (accepted, synonym, etc.)
  new_canonicalName = character(nrow(resolved_species)),       # Canonical name (genus + species epithet)
  new_authorship = character(nrow(resolved_species)),          # Authorship Information
  new_code = character(nrow(resolved_species)),                # New species code (if available)
  new_scientificName = character(nrow(resolved_species)),      # New scientific name
  new_UpdateStatus = character(nrow(resolved_species)),        # New Update Status
  ruleApplied = character(nrow(resolved_species)),             # Rule applied
  new_classification = character(nrow(resolved_species)),      # Classification information
  stringsAsFactors = FALSE
)

# Loop through each row of the resolved_species data frame
for (i in 1:nrow(resolved_species)) {
  
  # Assign old values directly
  output$old_code[i] <- resolved_species$old_code[i]
  output$old_scientificName[i] <- resolved_species$old_scientificName[i]
  output$old_UpdateStatus[i] <- resolved_species$UpdateStatus[i]
  output$ScientificNameAuthorCoL[i] <- resolved_species$acceptedScientificName[i]
  
  # Extract the new scientific name
  output$new_scientificName[i] <- extract_species_info(resolved_species$acceptedScientificName[i])[1]
  
  # Extract canonical name from new scientific name
  output$new_canonicalName[i] <- extract_genus_species(output$new_scientificName[i])
  
  if (is.na(output$new_scientificName[i]) || output$new_scientificName[i] == "NANA") {
    output$new_scientificName[i] <- "not found in Natura2000 List"
  }
  
  # Extract the matched species once for both new code and update status
  matched_species <- natura_species_list %>%
    filter(`Update status` != 'Deletion') %>%
    filter(`Species name` == output$new_canonicalName[i])
  
  # Check if any species matched
  if (nrow(matched_species) > 0) {
    # Assign the first match for both the new code and new UpdateStatus
    output$new_code[i] <- matched_species %>% pull(`Species code`) %>% first()
    output$new_UpdateStatus[i] <- matched_species %>% pull(`Update status`) %>% first()
    
  } else {
    # If no match, assign "not found"
    output$new_code[i] <- "not found in Natura2000 List"
    output$new_UpdateStatus[i] <- "not available"
    
    # Print a single message for missing match
    print(paste("No match found for:", resolved_species$old_scientificName[i]))
  }
  
  # Assign additional attributes directly
  output$new_authorship[i] <- resolved_species$authorship[i]
  output$TaxonomicStatus[i] <- resolved_species$category[i]
  output$ruleApplied[i] <- paste(output$TaxonomicStatus[i], output$new_UpdateStatus[i], sep = " - ")
  output$new_classification[i] <- resolved_species$classification[i]
  
  # Set values for matched species to not found in case of no match
  if (output$new_code[i] == 'not found in Natura2000 List') {
    output$new_canonicalName[i] <-  'not available'
    output$new_authorship[i] <-  'not available'
    output$new_classification[i] <-  'not available'
    output$new_scientificName[i] <-  'not available'
    output$new_UpdateStatus[i] <-  'not available'
  }
}

# Save Output permanently 
write.csv(output,"/Users/nr72kini/Desktop/ETC_BE/Github/ETC-BE/1.1.1.3 Species Support/Results/Tracked_Natura2000_Species_List.csv")

# Analyse
# Summary of Categories
category_summary <- resolved_species %>%
  group_by(category) %>%
  summarize(count = n())

print(category_summary)

# Summary of Output
table(output$ruleApplied)
