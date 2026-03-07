# set working directory
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
source("./helper/preprocessing.R")

# Preprocess the data
processed_data <- basic_data_preprocessing(
  workerids_csv_path = "../../data/korean_stops_perception_3_poa_all_ages-workerids.csv",
  trials_csv_path = "../../data/korean_stops_perception_3_poa_all_ages-trials.csv",
  subject_information_csv_path = "../../data/korean_stops_perception_3_poa_all_ages-subject_information.csv"
)

sprintf("The number of `asp` rows: %d", nrow(processed_data[processed_data$response == "asp", ]))
sprintf("The number of `fortis` rows: %d", nrow(processed_data[processed_data$response == "fortis", ]))
sprintf("The number of `lenis` rows: %d", nrow(processed_data[processed_data$response == "lenis", ]))
sprintf("The number of rows: %d", nrow(processed_data))

# Create output directory if it doesn't exist
if (!dir.exists("./output")) {
  dir.create("./output", recursive = TRUE)
}

# Save preprocessed data to CSV
write.csv(processed_data, "./output/perception_preprocessed_data.csv", row.names = FALSE)
