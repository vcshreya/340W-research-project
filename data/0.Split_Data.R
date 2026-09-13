library(readxl)
library(dplyr)
library(writexl)

# Load the dataset
file_path <- "data/0.Final.Data.xlsx"

acute_long <- read_excel(file_path, sheet = "Acute Long")
resting_long <- read_excel(file_path, sheet = "Resting Long")

# Check number of unique participants
length(unique(acute_long$id))

# Create participant-level table
participants <- acute_long %>%
  distinct(id, condition.rir)

# View participants
participants

# Check number in each resistance-training group
participants %>%
  count(condition.rir)

# Split data into training, test, and validation sets

# Set seed so that the split is reproducible
set.seed(340)

# Randomize the participants within each RIR condition
split_ids <- participants %>%
  group_by(condition.rir) %>%
  mutate(random_number = runif(n())) %>%
  arrange(random_number, .by_group = TRUE) %>%
  mutate(
    split = case_when(
      row_number() == 1 ~ "Validation",
      row_number() == 2 ~ "Test",
      TRUE ~ "Training"
    )
  ) %>%
  ungroup() %>%
  select(-random_number)

# View the participant assignments
split_ids

###CHECK
split_ids %>%
  count(split)

# Check that each RIR condition appears in each dataset
split_ids %>%
  count(condition.rir, split)

# Get participant IDs for each split
train_ids <- split_ids %>%
  filter(split == "Training") %>%
  pull(id)

test_ids <- split_ids %>%
  filter(split == "Test") %>%
  pull(id)

validation_ids <- split_ids %>%
  filter(split == "Validation") %>%
  pull(id)

# Split the acute long data
train_acute <- acute_long %>%
  filter(id %in% train_ids)

test_acute <- acute_long %>%
  filter(id %in% test_ids)

validation_acute <- acute_long %>%
  filter(id %in% validation_ids)

# Split resting long data
train_resting <- resting_long %>%
  filter(id %in% train_ids)

test_resting <- resting_long %>%
  filter(id %in% test_ids)

validation_resting <- resting_long %>%
  filter(id %in% validation_ids)

## saving the datasets 
dir.create("data/splits", showWarnings = FALSE)

# Save the training dataset
write_xlsx(
  list(
    "Acute Long" = train_acute,
    "Resting Long" = train_resting
  ),
  "data/splits/training_data.xlsx"
)

# Save test dataset
write_xlsx(
  list(
    "Acute Long" = test_acute,
    "Resting Long" = test_resting
  ),
  "data/splits/test_data.xlsx"
)

# Save validation dataset
write_xlsx(
  list(
    "Acute Long" = validation_acute,
    "Resting Long" = validation_resting
  ),
  "data/splits/validation_data.xlsx"
)
