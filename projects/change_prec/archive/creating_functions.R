source("source/main.R")

dummy <- setDT(read.csv('~/shared/data/paleo/processed/ljn_hydro_proxy/Ber14.txt', sep = ""))
dummy[, diff(year)]

set.seed(123)
num_rows <- 5
my_data <- data.table(x = 1:5, y = runif(num_rows))

# Function to calculate pairwise differences
calculate_pairwise_diff <- function(data) {
  num_rows <- nrow(data)
  differece <- matrix(NA, nrow = num_rows, ncol = num_rows)
  diff_labels <- matrix(NA, nrow = num_rows, ncol = num_rows)
  for (i in 1:num_rows) {
    for (j in 1:num_rows) {
      differece[i, j] <- as.numeric(data[j] - data[i])
      diff_labels[i, j]  <- as.numeric(data[i])
    }
  }
  return(cbind(time = diff_labels[upper.tri(diff_labels)], difference = differece[upper.tri(differece)]))
}


# Calculate pairwise ratios of differences
calculate_pairwise_ratio <- function(data) {
  num_rows <- nrow(data)
  ratios <- matrix(NA, nrow = num_rows, ncol = num_rows)
  ratios_labels <- matrix(NA, nrow = num_rows, ncol = num_rows)
  for (i in 1:num_rows) {
    for (j in 1:num_rows) {
      ratios[i, j] <- as.numeric((data[j] - data[i]) / data[i])
      ratios_labels[i, j]  <- as.numeric(data[i])
    }
  }
  return(cbind(value = ratios_labels[upper.tri(ratios_labels)], ratio = ratios[upper.tri(ratios)]))
}

# Calculate all pairwise ratios of differences
all_ratios_x <- calculate_pairwise_diff(dummy[, .(value = year)])
all_ratios_y <- calculate_pairwise_ratio(dummy[, .(value = value)])

all_diffs <- data.table(all_ratios_x, all_ratios_y)

all_diffs[, time_step := cut(difference, c(0, 5, 10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 2000))]

ggplot(dummy, aes(year, value)) +
  geom_line()

ggplot(all_diffs, aes(difference, value)) +
  geom_point()

ggplot(all_diffs[ratio > 0], aes(value, fill = time_step)) +
  geom_boxplot() + 
  coord_flip() +
  theme_bw()

ggplot(all_diffs[value < 0], aes(value, fill = time_step)) +
  geom_boxplot() + 
  coord_flip() +
  theme_bw()

ratios_2 <- matrix(NA, nrow = num_rows, ncol = num_rows)

