source("source/masks_source.R")
source("source/main.R")

# Load test image
folder_sim <- list.files(path = shared_sim_data_dir, full.names = T)

sub_folder_sim <- list.files(path = folder_sim[3], full.names = T)

files_sim <- list.files(path = sub_folder_sim[1], full.names = T)

print(files_sim[1])
test_data_all <- stack(paste(files_sim[1]))
test_data <- test_data_all[[10]]
rm(test_data_all)
