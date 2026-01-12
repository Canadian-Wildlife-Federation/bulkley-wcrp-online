
library(jsonlite)
library(forcats)
library(dplyr)
library(ggplot2)



plot_cumulative_avg_gain <- function(table_path, top_n = 20) {
  library(dplyr)
  library(ggplot2)
  
  tracktable <- read.csv(table_path)
  
  # Convert empty strings to NA and keep only ranked barriers
  tracktable$rank_total_upstr_hab[tracktable$rank_total_upstr_hab == ""] <- NA
  tracktable <- tracktable[!is.na(tracktable$rank_total_upstr_hab), ]
  
  # Sort by avg_gain_per_barrier (descending)
  tracktable <- tracktable %>%
    arrange(desc(avg_gain_per_barrier)) %>%
    mutate(cum_avg_gain = cumsum(avg_gain_per_barrier))
  
  # Take top N barriers
  tracktable_top <- tracktable %>%
    slice_max(order_by = avg_gain_per_barrier, n = top_n)
  
  # Build and return ggplot
  ggplot(
    tracktable_top,
    aes(x = seq_along(cum_avg_gain), y = cum_avg_gain)
  ) +
    geom_line() +
    geom_point() +
    labs(
      x = "Index",
      y = "Cumulative Average Gain"
    )
}