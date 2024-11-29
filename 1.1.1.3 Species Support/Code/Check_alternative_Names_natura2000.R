# Clean environment
rm(list = ls())

# Load Necessary Libraries
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(xlsx)

# Import the Natura 2000 Species List
natura_species_list <- read_excel(
  "/Users/nr72kini/Desktop/ETC_BE/Github/ETC-BE/1.1.1.3 Species Support/Data/Natura2000 species code list v2021 revised 202201-2.xlsx", 
  sheet = "Species_codelist_2021"
)

# Add the missing species to the list
missing_species <- data.frame(
  `Species code` = 6965,
  `Species name` = "Cottus gobio all others",
  `Update status` = "Deletion",
  `Alternative scientific name` = NA,
  stringsAsFactors = FALSE
)

natura_species_list <- bind_rows(natura_species_list, missing_species)

# Filter for species with status "Deletion" and retain necessary columns
natura_species_deleted <- natura_species_list %>%
  filter(`Update status` == "Deletion") 

# Split the alternative names and reshape data
natura_species_split <- natura_species_deleted %>%
  select(
    `Species code`,
    `Species name`,
    `Update status`,
    `Alternative scientific name`
  ) %>%
  mutate(Alternative.scientific.name = strsplit(`Alternative scientific name`, ",\\s*|\\s+and\\s+")) %>%
  unnest(Alternative.scientific.name) %>%
  mutate(Alternative.scientific.name = str_trim(Alternative.scientific.name))  # Remove extra spaces 


# Handle species without alternative names
natura_species_split <- natura_species_split %>%
  mutate(Alternative.scientific.name = ifelse(is.na(Alternative.scientific.name), NA, Alternative.scientific.name))

# Prepare the output data frame for deleted species and their alternatives
output <- natura_species_split %>%
  rename(
    `Deleted code` = `Species code`,
    `Deleted name` = `Species name`
  ) %>%
  mutate(
    `Species code` = NA,
    `Species name` = NA
  )

# Loop through rows to fill new columns based on alternative names
expanded_output <- data.frame()  # Create an empty data frame for expanded results
for (i in 1:nrow(output)) {
  # If there are alternative names, replicate the row for each alternative name
  num_alternatives <- length(output$Alternative.scientific.name[[i]])
  for (j in 1:num_alternatives) {
    new_row <- output[i,]  # Copy the original row
    new_row$`Species code` <- NA  # Reset species code for this row
    new_row$`Species name` <- NA  # Reset species name for this row
    
    if (!is.na(new_row$Alternative.scientific.name[j])) {
      matched_species <- natura_species_list %>%
        filter(`Update status` != "Deletion") %>%
        filter(`Species name` == new_row$Alternative.scientific.name[j])
      
      if (nrow(matched_species) > 0) {
        new_row$`Species code` <- matched_species$`Species code`[1]
        new_row$`Species name` <- matched_species$`Species name`[1]
      } else {
        new_row$`Species code` <- "not found in Natura2000 List"
        new_row$`Species name` <- "not available"
      }
    }
    
    # Add the row to the expanded output
    expanded_output <- bind_rows(expanded_output, new_row)
  }
}

# Celan expanded_output
expanded_output <- expanded_output %>%
  select( -c("Update status", "Alternative scientific name"))

# Clean 'natura_species_deleted'
natura_species_deleted <- natura_species_deleted %>%
  select(-Species.code, -Species.name, -Update.status, -Alternative.scientific.name, - "Species code")

# Clean `natura_species_list` and add new columns at the specified positions
natura_species_list <- natura_species_list %>%
  filter(`Update status` != "Deletion") %>% # Correct use of backticks for column name
  select(-Species.code, -Species.name, -Update.status) %>% # Exclude specified columns
  mutate(`Deleted code` = NA, `Deleted name` = NA) %>%  # Add new columns
  relocate(`Deleted code`, `Deleted name`, Alternative.scientific.name, .before = everything()) # Move them to the first positions

  
  
# Bind the non-deleted species (natura_species_list) to the expanded output
# Perform a left join to combine the two datasets
final_output <- left_join(
  expanded_output,
  natura_species_deleted,
  relationship = "many-to-many",
  by = c("Deleted name" = "Species name")
)

colnames(natura_species_list) == colnames(final_output)

final_output <- bind_rows(
  natura_species_list,
  final_output,
  by = c("Deleted name" = "Species name")
)
                          
# Save the result to an Excel file
write.xlsx(final_output, "/Users/nr72kini/Desktop/ETC_BE/Github/ETC-BE/1.1.1.3 Species Support/Results/Alternative_Natura2000_Species_List.xlsx")

# Verify structure
str(final_output)
