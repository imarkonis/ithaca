# Load required dataset info
source('source/main.R')
source('source/data_registry.R')

# Summarize available evap datasets
summarize_datasets(var = "evap")

# Filter datasets
selected <- filter_datasets()
print(selected)

selected <- filter_datasets(
  dname = c("CRU", "GPCP"),
  var = "precip",
  tstep = "monthly",
  area = "land"
)
print(selected)


