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

# Filter for species with status "Deletion" and retain necessary columns
natura_species_deleted <- natura_species_list %>%
  filter(`Update status` == "Deletion") %>%
  filter(!is.na(`Alternative scientific name`)) 

# List alternative Species Names
natura_species_deleted$`Alternative scientific name`

# Split names and reshape data
natura_species_split <- natura_species_deleted %>%
  mutate(Alternative.scientific.name = strsplit(`Alternative scientific name`, ",\\s*|\\s+and\\s+")) %>%
  unnest(Alternative.scientific.name)

# View the result
natura_species_split$Alternative.scientific.name

str(natura_species_split)

# Split the 'Alternative.scientific.name' column and duplicate rows where necessary
natura_species_split_expanded <- natura_species_split %>%
  separate_rows(Alternative.scientific.name, sep = ",\\s*") %>%  # Split by comma and create separate rows
  mutate(
    Alternative.scientific.name = str_trim(Alternative.scientific.name, side = "both"),  # Remove extra spaces
    Alternative.scientific.name = str_remove(Alternative.scientific.name, "\\?$")  # Remove trailing question marks
  )

str(natura_species_split_expanded)

# Create an empty data frame with predefined columns
output <- data.frame(
  old_code = character(nrow(natura_species_split_expanded)),                     # Species code from the old data
  old_scientificName = character(nrow(natura_species_split_expanded)),           # Old scientific name
  old_UpdateStatus = character(nrow(natura_species_split_expanded)),             # Old Update Status
  Alternative.scientific.name = character(nrow(natura_species_split_expanded)),  # Alternative scientific name
  new_code = character(nrow(natura_species_split_expanded)),                     # New species code (if available)
  new_UpdateStatus = character(nrow(natura_species_split_expanded)),             # New Update Status
  stringsAsFactors = FALSE
)


#i <- 1

# Loop through each row of the natura_species_split_expanded data frame
for (i in 1:nrow(natura_species_split_expanded)) {
  
  # Assign old values directly
  output$old_code[i] <- natura_species_split_expanded$'Species code'[i]
  output$old_scientificName[i] <- natura_species_split_expanded$'Species name'[i]
  output$old_UpdateStatus[i] <- natura_species_split_expanded$'Update status'[i]
  output$Alternative.scientific.name[i] <- natura_species_split_expanded$Alternative.scientific.name[i]
  
  # Extract the matched species once for both new code and update status
  matched_species <- natura_species_list %>%
    filter(`Update status` != 'Deletion') %>%
    filter(`Species name` == output$Alternative.scientific.name[i])
  
  # Check if any species matched
  if (nrow(matched_species) > 0) {
    # Assign the first match for both the new code and new UpdateStatus
    output$new_code[i] <- matched_species %>% pull(`Species code`) %>% first()
    output$new_UpdateStatus[i] <- matched_species %>% pull(`Update status`) %>% first()
    
  } else {
    # If no match, assign "not found"
    output$new_code[i] <- "not found in Natura2000 List"
    output$new_UpdateStatus[i] <- "not available"
    }
  
  # Set values for matched species to not found in case of no match
  if (output$new_code[i] == 'not found in Natura2000 List') {
    # output$new_canonicalName[i] <-  'not available'
    # output$new_authorship[i] <-  'not available'
    # output$new_classification[i] <-  'not available'
    # output$new_scientificName[i] <-  'not available'
    output$new_UpdateStatus[i] <-  'not available'
  }
}

str(output)




# Save Output permanently 
write.xlsx(output,"/Users/nr72kini/Desktop/ETC_BE/Github/ETC-BE/1.1.1.3 Species Support/Results/Alternative_Natura2000_Species_List.csv")
