library(tidyverse)
library(janitor)
library(lubridate)
library(scales)

input_folder <- "unicef-ssa-child-mortality"
output_folder <- "cleaned_data"

if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

csv_files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)

for (file in csv_files) {
  data <- read_csv(file) %>%
    clean_names() %>%
    drop_na()
  
  output_file <- file.path(output_folder, basename(file))
  write_csv(data, output_file)
}
