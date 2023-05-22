library(dplyr)
library(stringr)

YBreaks <- function(data, scaling_factor) {
  min_value <- floor(min(data$value))
  max_value <- ceiling(max(data$value) + max(data$value) * scaling_factor)
  unique(c(if_else(min_value < 0, min_value, 0), 0,
           round(max_value / 2, digits = 1), max_value))
}

YRange <- function(y_breaks) {
  c(min(y_breaks), max(y_breaks))
}

GetUniformDispersionX <- function(data, point_gap) {
  data %>% mutate(id = paste0(group, "_", value),
                  group.num = as.integer(group)) -> data
  
  df <- data.frame()
  for (g in unique(data$id)) {
    g_df <- filter(data, id == g)
    if (nrow(g_df) %% 2 == 0) {
      max_offset <- floor(nrow(g_df) / 2) * point_gap - point_gap / 2
      g_df$x <- seq(unique(g_df$group.num) - max_offset,
                    unique(g_df$group.num) + max_offset,
                    point_gap)
    } else {
      max_offset <- floor(nrow(g_df) / 2) * point_gap
      g_df$x <- seq(unique(g_df$group.num) - max_offset,
                    unique(g_df$group.num) + max_offset,
                    point_gap)
    }
    df <- bind_rows(df, g_df)
  }
  df
}
