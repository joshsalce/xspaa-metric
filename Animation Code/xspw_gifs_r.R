# %% [code] {"_execution_state":"idle","execution":{"iopub.status.busy":"2025-02-18T22:05:41.440078Z","iopub.execute_input":"2025-02-18T22:05:41.441845Z","iopub.status.idle":"2025-02-18T22:05:41.655269Z","shell.execute_reply":"2025-02-18T22:05:41.653209Z"}}
library(dplyr)
library(knitr)
library(reshape2)
library(readr)
library(ggplot2)
library(gganimate)
library(gridExtra)

options(repr.plot.width = 20, repr.plot.height = 10)
options(warn = -1)

list.files(path = "../input")

event_predictions <- read_csv("../input/xspaa-data/event_predictions.csv")
points_data <- read_csv("../input/xspaa-data/points_data.csv") %>%
    mutate(returner = ifelse(rallyid == 196, "Nadal", returner))

# %% [code] {"execution":{"iopub.status.busy":"2025-02-18T21:48:40.463242Z","iopub.execute_input":"2025-02-18T21:48:40.464846Z","iopub.status.idle":"2025-02-18T21:48:40.488714Z","shell.execute_reply":"2025-02-18T21:48:40.486543Z"}}
# Invert axis, create hitter and receiver columns
event_predictions_join <- event_predictions %>% 
    rename(server_x = server_y,
           server_y = server_x,
           receiver_x = receiver_y,
           receiver_y = receiver_x) %>%
    inner_join(
        points_data %>% select(rallyid, server, returner),
        "rallyid"
    ) %>%
    mutate(
        hitter = ifelse(strokeid %% 2 == 0, returner, server),
        receiver = ifelse(strokeid %% 2 == 0, server, returner),
    )

# %% [code] {"execution":{"iopub.status.busy":"2025-02-18T21:48:40.491742Z","iopub.execute_input":"2025-02-18T21:48:40.493314Z","iopub.status.idle":"2025-02-18T21:48:40.511746Z","shell.execute_reply":"2025-02-18T21:48:40.509839Z"}}
# Encode "ball position" using hitter location, conditional on strokeid
ball_pos <- event_predictions_join %>%
    mutate(
        ball_x = ifelse(strokeid %% 2 == 1, server_x, receiver_x),
        ball_y = ifelse(strokeid %% 2 == 1, server_y, receiver_y)
    ) %>%
    select(rallyid, strokeid, hitter, receiver, ball_x, ball_y)

# %% [code] {"execution":{"iopub.status.busy":"2025-02-18T21:48:40.514792Z","iopub.execute_input":"2025-02-18T21:48:40.516403Z","iopub.status.idle":"2025-02-18T21:48:40.914243Z","shell.execute_reply":"2025-02-18T21:48:40.912277Z"}}
# Build court diagram
out_bounds <- data.frame(
    x = c(0, 0, 23.77, 23.77, 0),
    y = c(0, 10.97, 10.97, 0, 0)
)

t_lines <- data.frame(
    x = c(5.585, 5.585, 5.585, 18.385, 18.385, 18.385),
    y = c(1.37, 9.6, 5.485, 5.485, 1.37, 9.6)
)

court_b <- ggplot() +
    geom_path(data = out_bounds, aes(x = x, y = y), colour = "white", linewidth = 1.5, lineend = "square") +
    geom_path(data = t_lines, aes(x = x, y = y), colour = "white", linewidth = 1.5, lineend = "square") +
    geom_path(aes(x = c(23.77, 0), y = c(1.37, 1.37)), colour = "white", linewidth = 1.5, lineend = "square") + # lower singles lines
    geom_path(aes(x = c(23.77, 0), y = c(9.6, 9.6)), colour = "white", linewidth = 1.5, lineend = "square") + # upper singles lines
    geom_path(aes(x = c(11.985, 11.985), y = c(0, 10.97)), lty = 1, colour = "white", linewidth = 1.5, lineend = "square") + # net line
    ylim(c(-3, 13)) + xlim(c(-6, 30)) + 
    theme_void() +
    theme(
        plot.background = element_rect("#5080B0")
    )
court_b

# %% [code] {"execution":{"iopub.status.busy":"2025-02-18T21:48:40.917009Z","iopub.execute_input":"2025-02-18T21:48:40.918541Z","iopub.status.idle":"2025-02-18T21:48:41.543356Z","shell.execute_reply":"2025-02-18T21:48:41.539859Z"}}
# Pull example rally, plot assumed ball position at each stroke (w/, w/o lines)
rally_df <- ball_pos %>% 
    filter(rallyid == 169) 

rally_points <- court_b + 
    geom_point(data = rally_df, aes(x = ball_x, y = ball_y), colour = "green")

rally_lines <- court_b + 
    geom_point(data = rally_df, aes(x = ball_x, y = ball_y), colour = "green") +
    geom_path(data = rally_df, aes(x = ball_x, y = ball_y), colour = "green")
 
grid.arrange(rally_points, rally_lines, nrow = 2)

# %% [code] {"execution":{"iopub.status.busy":"2025-02-18T21:48:41.548870Z","iopub.execute_input":"2025-02-18T21:48:41.551771Z","iopub.status.idle":"2025-02-18T21:48:41.648252Z","shell.execute_reply":"2025-02-18T21:48:41.645924Z"}}
# First, create new variables for the positions
ball_player_pos <- ball_pos %>% 
    mutate(
        nadal_x = ifelse(hitter == "Nadal", ball_x, NA),
        nadal_y = ifelse(hitter == "Nadal", ball_y, NA),
        djo_x = ifelse(hitter == "Djokovic", ball_x, NA),
        djo_y = ifelse(hitter == "Djokovic", ball_y, NA)
        )

# Fill the new variables with starting positions for the return player
event_predictions_receiver_locs <- event_predictions_join[c("rallyid", "strokeid", "receiver_x", "receiver_y")]

# Join data with receiver locations, create Nadal and Djokovic location columns
ball_player_pos_data <- ball_player_pos %>% 
    left_join(event_predictions_receiver_locs, by = c("rallyid", "strokeid")) %>% 
    mutate(
        nadal_x = ifelse((strokeid == 1 & receiver == "Nadal"), receiver_x, nadal_x),
        nadal_y = ifelse(strokeid == 1 & receiver == "Nadal", receiver_y, nadal_y),
        djo_x = ifelse(strokeid == 1 & receiver == "Djokovic", receiver_x, djo_x),
        djo_y = ifelse(strokeid == 1 & receiver == "Djokovic", receiver_y, djo_y)
    ) %>% 
    select(-receiver_x, -receiver_y)

# Fill the player positions with the end positions of a rally
ball_player_pos_data <- ball_player_pos_data %>% 
    group_by(rallyid) %>% 
    mutate(
        nadal_x = ifelse(strokeid == max(strokeid) & strokeid != 1 & receiver == "Nadal",
                         nadal_x[strokeid == max(strokeid) - 1], 
                         nadal_x),
        nadal_y = ifelse(strokeid == max(strokeid) & strokeid != 1 & receiver == "Nadal", 
                         nadal_y[strokeid == max(strokeid) - 1], 
                         nadal_y),
        djo_x = ifelse(strokeid == max(strokeid) & strokeid != 1 & receiver == "Djokovic", 
                       djo_x[strokeid == max(strokeid) - 1], 
                       djo_x),
        djo_y = ifelse(strokeid == max(strokeid) & strokeid != 1 & receiver == "Djokovic", 
                       djo_y[strokeid == max(strokeid) - 1],
                       djo_y),
    )

# Interpolate locations between each hit
ball_player_pos_data <- ball_player_pos_data %>% 
    group_by(rallyid) %>% 
    mutate(
        nadal_x = ifelse(is.na(nadal_x), (lag(nadal_x) + lead(nadal_x))/2, nadal_x),
        nadal_y = ifelse(is.na(nadal_y), (lag(nadal_y) + lead(nadal_y))/2, nadal_y),
        djo_x = ifelse(is.na(djo_x), (lag(djo_x) + lead(djo_x))/2, djo_x),
        djo_y = ifelse(is.na(djo_y), (lag(djo_y) + lead(djo_y))/2, djo_y)
    )

# Gather data for points, event probabilities
points_data_join <- points_data[, c("rallyid", "score")]

event_pred_probas <- event_predictions_join[, c("rallyid", "strokeid", "prob")]

# Join location, point, event probability data
ball_player_pos_data <- ball_player_pos_data %>% 
    left_join(points_data_join, by = "rallyid") %>% 
    left_join(event_pred_probas, by = c("rallyid", "strokeid")) %>%
    filter(!is.na("score")) %>%
    mutate(prob = round(100*prob, 1))

# %% [code] {"execution":{"iopub.status.busy":"2025-02-18T22:16:33.477826Z","iopub.execute_input":"2025-02-18T22:16:33.479960Z","iopub.status.idle":"2025-02-18T22:17:30.906970Z","shell.execute_reply":"2025-02-18T22:17:30.904932Z"}}
rallies_to_animate <- ball_player_pos_data %>% 
    select(rallyid) %>% 
    distinct() %>% 
    pull()

# For each rallyid, create gifs with point animations and changing probabilities
for(rally in rallies_to_animate) {
    print(paste0("Rally: ", rally))

    # Filter data, point-specific info
    rally_data <- ball_player_pos_data %>% 
        filter(rallyid == rally)
    
    score <- rally_data[1, "score"]
    serve <- rally_data[1, "hitter"]

    # Calculate number of strokes
    num_strokes <- rally_data %>%
        summarise(strokes = max(strokeid)) %>%
        pull()
    num_frames <- num_strokes * 20
    
    point_gif <- court_b +
        ylim(c(-1, 14)) +
        geom_text(aes(x = 11.985, y = 13, label = paste0("Score: ", score)), color = "white") +
        geom_text(aes(x = 11.985, y = 14, label = paste0("Serve: ", serve)), color = "white") +
        # Plot assumed ball locations
        geom_point(data = rally_data, aes(x = ball_x, y = ball_y), colour = "yellow", size = 3) +
        geom_path(data = rally_data, aes(x = ball_x, y = ball_y), colour = NA) +     
        # Plot Nadal locations
        geom_point(data = rally_data, aes(x = nadal_x, y = nadal_y), size = 8, col = "orange") + 
        geom_text(data = rally_data, aes(x = nadal_x, y = nadal_y, label = "N")) +
        # Plot Djokovic locations
        geom_point(data = rally_data, aes(x = djo_x, y = djo_y), size = 8, col = "white") +
        geom_text(data = rally_data, aes(x = djo_x, y = djo_y, label = "D")) +
        labs(subtitle = "Probability, Server Wins: {closest_state}%") +
        theme(
            plot.subtitle = element_text(hjust = 0.5, vjust = -2, colour = "white")
        ) +
        transition_reveal(strokeid) #+
        #transition_states(states = prob, transition_length = 1, state_length = 1, wrap = FALSE)
    
    animate(point_gif, nframes = num_frames, width = 800, renderer = gifski_renderer(paste0("animation_", rally, ".gif")))
}