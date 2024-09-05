library(dplyr)

# Read data
pole <- read.csv("/Users/pp16/Downloads/archive/results.csv")
drivers <- read.csv("/Users/pp16/Downloads/archive/drivers.csv")

# Merge data
positions <- merge(pole, drivers, by = "driverId") %>%
  select(driverId, forename, surname, raceId, grid, position)

# Create a variable indicating whether the driver started from pole position
positions <- positions %>%
  mutate(started_from_pole = ifelse(grid == 1, "Yes", "No"))

# Sort the dataframe by the 'position' column, with '1' values grouped together
positions <- positions %>%
  arrange(position != 1, position)

# If you want to keep the original order of the dataframe intact for other columns, you can add a secondary sort by row number
positions <- positions %>%
  arrange(position != 1, position, row_number())

positions

# Create a contingency table
contingency_table <- table(positions$started_from_pole, positions$position)
contingency_table

# Define the positions in ascending order
sorted_positions <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33)

# Reorder the columns of the contingency table
contingency_table_sorted <- contingency_table[, match(sorted_positions, colnames(contingency_table))]

# Print the sorted contingency table
contingency_table_sorted

# Perform Chi-square test
chi_sq_test <- chisq.test(contingency_table)
chi_sq_test

# Perform Fisher's exact test with simulation
fisher_test <- fisher.test(contingency_table, simulate.p.value = TRUE)
fisher_test

# Plot observed frequencies or proportions for each category
barplot(contingency_table_sorted,
        beside = TRUE,
        legend.text = TRUE,  # Add legend
        col = c("blue", "red"),
        xlab = "Final Position",  # Label x-axis
        ylab = "Count",            # Label y-axis
        main = "Effect of Starting from Pole Position on Final Positions")  # Add title
legend("topright", legend = c("No (Not start from pole (1st) position)", "Yes (Started from pole position)"), fill = c("blue", "red"))  # Legend labels

# Plot observed frequencies or proportions for each category
barplot(contingency_table_sorted,
        beside = TRUE,
        col = c("blue", "red"),
        xlab = "Final Position",  # Label x-axis
        ylab = "Count",            # Label y-axis
        main = "Effect of Starting from Pole Position on Final Positions")  # Add title

# Adjust the size of the legend
legend("topright",
       legend = c("Did not start from pole (1st) position", "Started from pole (1st) position"),
       fill = c("blue", "red"),
       cex = 0.8)  # Smaller size, adjust as needed

# Filter out missing values
positions <- positions[complete.cases(positions$position), ]

# Convert "position" column to numeric
positions$position <- as.numeric(positions$position)

# Histogram for drivers who started from the pole position
hist(positions$position[positions$started_from_pole == "Yes"],
     main = "Histogram of Final Positions for drivers who started from Pole position",
     xlab = "Final Position",
     ylab = "Frequency",
     col = "grey",
     breaks = 0:33,  # Specify breaks from 0 to 33
     xlim = c(0, 34),  # Set the x-axis limits
    ylim = c(0,500))

# Add custom labels to the x-axis
axis(1, at = 0:33, labels = 0:33)

# Histogram for drivers who didn't start from the pole position
hist(positions$position[positions$started_from_pole == "No"],
     main = "Histogram of Final Positions (Did not Start from Pole)",
     xlab = "Final Position",
     ylab = "Frequency",
     col = "grey",
     breaks = 0:33,  # Specify breaks from 0 to 33
     xlim = c(0, 34),  # Set the x-axis limits
     ylim = c(0,1100))

# Add custom labels to the x-axis
axis(1, at = 0:33, labels = 0:33)
