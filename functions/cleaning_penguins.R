# Function to clean the column names (remove columns and 
  # make lower case and snake case)
clean_column_names <- function(penguins_data) {
  penguins_data %>%
    select(-starts_with("Delta")) %>%
    select(-Comments) %>%
    select(-starts_with("X")) %>%
    clean_names()
}

# Function to subset the data based on the list of column names
subset_columns <- function(penguins_data, column_names) {
  penguins_data %>%
    select(all_of(column_names))
}

# Function to shorten the species names
shorten_species <- function(penguins_data) {
  penguins_data %>%
    mutate(species = case_when(
      species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
      species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
      species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
    ))
}

# Function to remove any empty columns or rows
remove_empty_columns_rows <- function(penguins_data) {
  penguins_data %>%
    remove_empty(c("rows", "cols"))
}

# Function to remove rows that contain NA values
remove_NA <- function(penguins_data) {
  penguins_data %>%
    na.omit()
}

# Function to subset the penguins data set based on species
filter_by_species <- function(penguins_data, selected_species) {
  penguins_data %>%
    filter(species == selected_species)
}

# A function to filter the penguins data set based on island
filter_by_island <- function(penguins_data, island) {
  penguins_data %>%
    filter(species == island)
}



# --- --- ---

