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

game6_times <- c(
  90, 90, 90, 90, 90, 90, 89, 88, 85, 83, 83, 79, 77, 75, 58, 57, 52, 36, 33, 30, 
  22, 19, 18, 9, 4, 3, 3, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0
)

game6_df <- add_game(game6_times, "Game 6")

game7_times <- c(
  90, 90, 90, 90, 90, 90, 88, 87, 86, 82, 76, 73, 72, 69, 62, 46, 36, 35, 30, 28, 
  25, 21, 15, 13, 12, 12, 11, 10, 4, 4, 4, 2, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 
)

game7_df <- add_game(game7_times, "Game 7")

game8_times <- c(
  90, 90, 90, 89, 89, 84, 83, 81, 81, 71, 66, 64, 50, 37, 24, 15, 14, 10, 10, 9, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0
)

game8_df <- add_game(game8_times, "Game 8")

game9_times <- c(
  90, 90, 90, 90, 90, 90, 89, 88, 88, 87, 86, 76, 70, 70, 67, 59, 53, 45, 41, 34, 29, 28, 27, 
  25, 19, 3, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
)

game9_df <- add_game(game9_times, "Game 9")

game10_times <- c(
  90, 90, 90, 90, 90, 89, 83, 80, 72, 69, 67, 64, 62, 57, 49, 48, 40, 36, 23, 17, 16, 14, 13, 
  8, 8, 7, 3, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 
)

game10_df <- add_game(game10_times, "Game 10")

game11_times <- c(
  60, 60, 60, 60, 60,  59, 59, 59, 58, 53, 53, 48, 48, 43, 42, 39, 28, 14, 14, 9, 9, 
  5, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
)

game11_df <- add_game(game11_times, "Game 11")

game12_times <- c(
  90, 90, 90, 90, 90, 90, 89, 89, 88, 87, 85, 83, 83, 79, 74, 72, 70, 61, 56, 53
)

game12_df <- add_game(game12_times, "Game 12")

game13_times <- c(
  90, 90, 90, 90, 90, 90, 89, 89, 88, 83, 81, 73, 72, 67, 46, 45, 45, 34, 32, 32, 
  25, 25, 25, 23, 13, 12, 8, 8, 7, 3, 2, 1, 1, 1, 1, 1, 1, 1
)

game13_df <- add_game(game13_times, "Game 13")

game14_times <- c(
  90, 90, 89, 89, 85, 85, 82, 78, 72, 71, 69, 55, 53, 50, 46, 44, 35, 33, 20, 17, 
  17, 16, 15, 11, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
)

game14_df <- add_game(game14_times, "Game 14")

game15_times <- c(
  90, 90, 90, 90, 90, 90, 89, 88, 87, 86, 85, 81, 67, 63, 62, 58, 55
)

game15_df <- add_game(game15_times, "Game 15")

# -------------------------------
# 4. COMBINE ALL DATA
# -------------------------------

plot_df <- bind_rows(
  game1_df,
  game2_df,
  game3_df,
  game4_df,
  game5_df,
  game6_df,
  game7_df,
  game8_df,
  game9_df,
  game10_df,
  game11_df,
  game12_df,
  game13_df,
  game14_df,
  game15_df
)

# -------------------------------
# 5. MAP GAMES TO OUTCOMES
# -------------------------------

outcome_map <- c(
  "Game 1"  = "Draw",
  "Game 2"  = "Loss",
  "Game 3"  = "Draw",
  "Game 4"  = "Draw",
  "Game 5"  = "Draw",
  "Game 6"  = "Draw",
  "Game 7"  = "Draw",
  "Game 8"  = "Win",
  "Game 9"  = "Loss",
  "Game 10" = "Win",
  "Game 11" = "Win",
  "Game 12" = "Win",
  "Game 13" = "Draw",
  "Game 14" = "Loss",
  "Game 15" = "Draw"
)

plot_df <- plot_df %>%
  mutate(outcome = outcome_map[type])

# -------------------------------
# 6. FILTER WHICH LINES TO SHOW
# -------------------------------

plot_df_filtered <- plot_df %>%
  filter(type %in% c("Game 1", "Game 2", "Game 3", "Game 4", "Game 5", "Game 6", "Game 7",
                     "Game 8", "Game 9", "Game 10", "Game 11", "Game 12", "Game 13", "Game 14", "Game 15"))

# -------------------------------
# 7. CUSTOM COLORS (by outcome, not by game)
# -------------------------------

outcome_colors <- c(
  "Win"  = "green",
  "Draw" = "blue",
  "Loss" = "red"
)

# -------------------------------
# 8. PLOT
# -------------------------------

ggplot() +
  
  # Ideal line (background)
  geom_line(data = ideal_df,
            aes(x = move, y = time),
            color = "black",
            size = 1,
            alpha = 0.25) +
  
  # Game lines (foreground), grouped by game but colored by outcome
  geom_line(data = plot_df_filtered,
            aes(x = move, y = time, color = outcome, group = type),
            size = 1.3) +
  
  scale_color_manual(values = outcome_colors) +
  
  scale_x_continuous(
    breaks = seq(0, max(plot_df$move), by = 5),
    limits = c(0, 90)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 90, by = 10),
    limits = c(0, 90)
  ) +
  
  labs(
    title = "Spring 2026 Tournaments Time Management Breakdown",
    x = "Move Number",
    y = "Minutes Left",
    color = "Result"
  ) +
  
  theme_minimal(base_size = 14)