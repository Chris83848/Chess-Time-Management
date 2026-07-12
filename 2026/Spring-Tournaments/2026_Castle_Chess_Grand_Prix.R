library(ggplot2)
library(dplyr)

# -------------------------------
# 1. IDEAL LINE SETUP
# -------------------------------

slope <- -85 / 40
intercept <- 90

ideal_moves <- 0:60
ideal_time_raw <- intercept + slope * ideal_moves
ideal_time <- ifelse(ideal_time_raw > 0, ideal_time_raw, 0)

# -------------------------------
# 2. FUNCTION TO ADD A GAME
# -------------------------------

add_game <- function(times, name) {
  data.frame(
    move = 0:(length(times)-1),
    time = times,
    type = name
  )
}

# -------------------------------
# 3. ADD YOUR GAMES HERE
# -------------------------------

# Game 1 (example)
game1_times <- c(
  60, 60, 60, 60, 60,  59, 59, 59, 58, 53, 53, 48, 48, 43, 42, 39, 28, 14, 14, 9, 9, 
  5, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
)

game1_df <- add_game(game1_times, "Game 1")

# Game 2 (example)
game2_times <- c(
  90, 90, 90, 90, 90, 90, 89, 89, 88, 87, 85, 83, 83, 79, 74, 72, 70, 61, 56, 53
)

game2_df <- add_game(game2_times, "Game 2")

game3_times <- c(
  90, 90, 90, 90, 90, 90, 89, 89, 88, 83, 81, 73, 72, 67, 46, 45, 45, 34, 32, 32, 
  25, 25, 25, 23, 13, 12, 8, 8, 7, 3, 2, 1, 1, 1, 1, 1, 1, 1
)

game3_df <- add_game(game3_times, "Game 3")

game4_times <- c(
  90, 90, 89, 89, 85, 85, 82, 78, 72, 71, 69, 55, 53, 50, 46, 44, 35, 33, 20, 17, 
  17, 16, 15, 11, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
)

game4_df <- add_game(game4_times, "Game 4")

game5_times <- c(
  90, 90, 90, 90, 90, 90, 89, 88, 87, 86, 85, 81, 67, 63, 62, 58, 55
)

game5_df <- add_game(game5_times, "Game 5")

# -------------------------------
# 4. COMBINE ALL DATA
# -------------------------------

plot_df <- bind_rows(
  game1_df,
  game2_df,
  game3_df,
  game4_df,
  game5_df
)

# -------------------------------
# 5. FILTER WHICH LINES TO SHOW
# -------------------------------

plot_df_filtered <- plot_df %>%
  filter(type %in% c("Ideal", "Game 1", "Game 2", "Game 3", "Game 4", "Game 5"))

# To hide a game, just remove it:
# filter(type %in% c("Ideal", "Game 1"))

# -------------------------------
# 6. CUSTOM COLORS
# -------------------------------

custom_colors <- c(
  "Ideal" = "black",
  "Game 1" = "green",
  "Game 2" = "lightgreen",
  "Game 3" = "blue",
  "Game 4" = "red",
  "Game 5" = "lightblue"
)

# -------------------------------
# 7. PLOT
# -------------------------------

ggplot() +
  
  # Ideal line (background)
  geom_line(data = ideal_df,
            aes(x = move, y = time),
            color = "black",
            size = 1,
            alpha = 0.25) +
  
  # Game lines (foreground)
  geom_line(data = plot_df_filtered %>% filter(type != "Ideal"),
            aes(x = move, y = time, color = type),
            size = 1.3) +
  
  scale_color_manual(values = custom_colors) +
  
  # Change x-axis
  scale_x_continuous(
    breaks = seq(0, max(plot_df$move), by = 5),
    limits = c(0, 50)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 90, by = 10),
    limits = c(0, 90)
  ) +
  
  labs(
    title = "2026 Castle Chess Grand Prix Tournament Time Management Breakdown",
    x = "Move Number",
    y = "Minutes Left",
    color = "Game"
  ) +
  
  theme_minimal(base_size = 14)