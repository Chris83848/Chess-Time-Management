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
  90, 90, 90, 90, 89, 89, 86, 85, 74, 70, 43, 40, 
  36, 29, 17, 15, 12, 5, 5, 5, 4, 4, 2, 1, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
)

game1_df <- add_game(game1_times, "Game 1")

# Game 2 (example)
game2_times <- c(
  90, 90, 90, 90, 89, 89, 88, 84, 79, 69, 61, 50, 50, 
  47, 45, 36, 30, 23, 23, 16, 3, 3, 2, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0
)

game2_df <- add_game(game2_times, "Game 2")

game3_times <- c(
  90, 90, 90, 89, 89, 87, 86, 84, 84, 82, 82, 80, 72, 
  72, 69, 66, 61, 60, 37, 36, 35, 34, 28, 19, 18, 9, 
  7, 7, 6, 5, 4, 4, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
)

game3_df <- add_game(game3_times, "Game 3")

game4_times <- c(
  90, 90, 90, 90, 90, 90, 89, 89, 88, 88, 86, 81, 71, 
  66, 60, 50, 39, 30, 20, 20, 13, 11, 9, 3, 3, 1, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
)

game4_df <- add_game(game4_times, "Game 4")

game5_times <- c(
  90, 90, 90, 90, 90, 89, 89, 88, 88, 82, 79, 78, 75, 75, 
  75, 71, 57, 57, 57, 57, 56, 56, 49, 46, 44, 41, 37, 35, 
  33, 32, 32, 32, 27, 26, 25, 23, 22, 22, 22, 21, 21, 21, 
  20, 20, 19, 19, 18, 18, 16, 16, 16, 13, 12, 11, 11, 11, 
  11, 11, 10, 10, 10, 10, 10, 10, 9, 9, 8, 8, 8, 8, 8, 8, 
  8, 8, 8, 8, 8, 8, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4
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
  filter(type %in% c("Ideal", "Game 3"))

# To hide a game, just remove it:
# filter(type %in% c("Ideal", "Game 1"))

# -------------------------------
# 6. CUSTOM COLORS
# -------------------------------

custom_colors <- c(
  "Ideal" = "black",
  "Game 1" = "blue",
  "Game 2" = "red",
  "Game 3" = "blue",
  "Game 4" = "blue",
  "Game 5" = "blue"
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
  
  scale_x_continuous(
    breaks = seq(0, max(plot_df$move), by = 5),
    limits = c(0, 60)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 90, by = 10),
    limits = c(0, 90)
  ) +
  
  labs(
    title = "2026 Phillip O'Neal Taylor GA State Championship Game 3 Time Management Breakdown",
    x = "Move Number",
    y = "Minutes Left",
    color = "Game"
  ) +
  
  theme_minimal(base_size = 14)