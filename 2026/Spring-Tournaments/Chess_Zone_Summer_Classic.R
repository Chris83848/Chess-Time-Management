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
ideal_df <- data.frame(move = ideal_moves, time = ideal_time)

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
  90, 90, 90, 90, 90, 90, 89, 88, 85, 83, 83, 79, 77, 75, 58, 57, 52, 36, 33, 30, 
  22, 19, 18, 9, 4, 3, 3, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0
)

game1_df <- add_game(game1_times, "Game 1")

# Game 2 (example)
game2_times <- c(
  90, 90, 90, 90, 90, 90, 88, 87, 86, 82, 76, 73, 72, 69, 62, 46, 36, 35, 30, 28, 
  25, 21, 15, 13, 12, 12, 11, 10, 4, 4, 4, 2, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 
)

game2_df <- add_game(game2_times, "Game 2")

game3_times <- c(
  90, 90, 90, 89, 89, 84, 83, 81, 81, 71, 66, 64, 50, 37, 24, 15, 14, 10, 10, 9, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0
)

game3_df <- add_game(game3_times, "Game 3")

game4_times <- c(
  90, 90, 90, 90, 90, 90, 89, 88, 88, 87, 86, 76, 70, 70, 67, 59, 53, 45, 41, 34, 29, 28, 27, 
  25, 19, 3, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
)

game4_df <- add_game(game4_times, "Game 4")

game5_times <- c(
  90, 90, 90, 90, 90, 89, 83, 80, 72, 69, 67, 64, 62, 57, 49, 48, 40, 36, 23, 17, 16, 14, 13, 
  8, 8, 7, 3, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 
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
  "Game 1" = "darkblue",
  "Game 2" = "blue",
  "Game 3" = "green",
  "Game 4" = "red",
  "Game 5" = "darkgreen"
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
    limits = c(0, 70)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 90, by = 10),
    limits = c(0, 90)
  ) +
  
  labs(
    title = "Chess Zone Summer Classic Tournament Time Management Breakdown",
    x = "Move Number",
    y = "Minutes Left",
    color = "Game"
  ) +
  
  theme_minimal(base_size = 14)