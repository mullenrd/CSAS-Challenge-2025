library(arrow)
library(dplyr)
library(tidyverse)
library(httr)
library(jsonlite)
library(ggplot2)
library(rvest)
library(RSelenium)
#Intitial Dataset
#swing<-read_feather("/Users/reesemullen/Desktop/Statistical Practice/ASA-2025-Challenge/OneDrive_1_10-12-2024/statcast_pitch_swing_data_20240402_20240630.arrow")

#Updated Dataset
swing<-file.choose()
swing<-read.csv(swing)

swing$game_date<-as.Date(swing$game_date)
summary(swing$game_date)

#Remove Deprecated Stats
swing1<-Filter(function(x)!all(is.na(x)), swing)

# Add binary variable for pitches in the strike zone
swing1 <- swing1 %>%
  mutate(
    in_strike_zone = if_else(
      plate_z >= sz_bot & plate_z <= sz_top & 
        plate_x >= -0.71 & plate_x <= 0.71,
      1,  # In strike zone
      0,  # Out of strike zone
      missing = 0  # Handle missing values
    )
  )

swing1 <- swing1 %>%
  mutate(
    pitch_type_basic = case_when(
      pitch_type %in% c("FF", "FC", "SI") ~ "Fastball",
      pitch_type %in% c("CU", "SL", "SV", "KC", "ST") ~ "Breaking Ball",
      pitch_type %in% c("CH", "EP", "FO", "FS", "SC", "KN", "", "FA", "CS") ~ "Offspeed",
      TRUE ~ "Unknown"  # Catch unexpected pitch types
    )
  )
#Average Swing speed
average_swing_speed <- swing1 %>%
  group_by(batter) %>%
  summarise(avg_swing_speed = mean(bat_speed, na.rm = TRUE))

unique_player_names <- swing1 %>%
  dplyr::select(batter, player_name) %>%
  distinct()

# Join on batter
average_swing_speed <- average_swing_speed %>%
  left_join(unique_player_names, by = "batter")



#Extracting Age, Height, and Weight Data

# Unique Players
player_ids <- unique(average_swing_speed$batter)

player_info_list <- list()



# Loop through each player ID
for (id in player_ids) {
  # API URL
  url <- paste0("https://statsapi.mlb.com/api/v1/people?personIds=", id, "&hydrate=stats(group=[hitting],type=[byDateRange],season=2024)")
  
  # GET request
  response <- GET(url)
  
  # Check if the response is successful
  if (status_code(response) == 200) {
    player_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    if (!is.null(player_data$people) && nrow(player_data$people) > 0) {
      # Extract the player data
      player <- player_data$people[1,]
      
      # Extract general information
      batter <- id
      height <- if (!is.null(player$height)) player$height else NA
      weight <- if (!is.null(player$weight)) player$weight else NA
      age <- if (!is.null(player$currentAge)) player$currentAge else NA
      primary_position <- if (!is.null(player$primaryPosition$abbreviation)) player$primaryPosition$abbreviation else NA
      bat_side <- if (!is.null(player$batSide$code)) player$batSide$code else NA
      
      # Extract strike zone information
      strike_zone_top <- if (!is.null(player$strikeZoneTop)) player$strikeZoneTop else NA
      strike_zone_bottom <- if (!is.null(player$strikeZoneBottom)) player$strikeZoneBottom else NA
      
      # Initialize hitting stats
      hit_by_pitch <- 0
      caught_stealing <- 0
      stolen_bases <- 0
      steal_attempts <- 0
      
      # Extract hitting stats if available
      if (!is.null(player$stats) && length(player$stats) > 0) {
        # Assuming the first split contains the relevant data
        if (!is.null(player$stats[[1]]$splits) && length(player$stats[[1]]$splits) > 0) {
          hitting_stats <- player$stats[[1]]$splits[[1]]$stat
          
          # Extract required stats
          hit_by_pitch <- if (!is.null(hitting_stats$hitByPitch)) hitting_stats$hitByPitch else 0
          caught_stealing <- if (!is.null(hitting_stats$caughtStealing)) hitting_stats$caughtStealing else 0
          stolen_bases <- if (!is.null(hitting_stats$stolenBases)) hitting_stats$stolenBases else 0
          steal_attempts <- caught_stealing + stolen_bases  # Calculate steal attempts
        }
      }
      
      # Create a temporary data frame for the player
      player_info_temp <- data.frame(
        batter = batter,
        height = height,
        weight = weight,
        age = age,
        primary_position = primary_position,
        bat_side = bat_side,
        strike_zone_top = strike_zone_top,
        strike_zone_bottom = strike_zone_bottom,
        hit_by_pitch = hit_by_pitch,
        caught_stealing = caught_stealing,
        stolen_bases = stolen_bases,
        steal_attempts = steal_attempts,
        stringsAsFactors = FALSE
      )
      
      # Append to the list
      player_info_list[[length(player_info_list) + 1]] <- player_info_temp
    } else {
      # Debugging: No player data
      print(paste("No player data for batter ID:", id))
    }
  } else {
    # Debugging: Failed request
    print(paste("Failed to retrieve data for batter ID:", id, "with status code:", status_code(response)))
  }
  
  # Delay to avoid hitting API limits
  Sys.sleep(0.1)
}


# Combine the list into a data frame
player_info <- bind_rows(player_info_list)

# Check the result
if (nrow(player_info) == 0) {
  print("Warning: No data was collected in player_info.")
} else {
  print("Successfully collected player stats and info.")
}

# Join the player info with swing speed 
average_swing_speed <- average_swing_speed %>%
  left_join(player_info, by = "batter")

average_swing_speed <- average_swing_speed %>%
  distinct(batter, .keep_all = TRUE)

average_swing_speed$weight <- as.numeric(average_swing_speed$weight)
average_swing_speed$average_swing_speed <- as.numeric(average_swing_speed$avg_swing_speed)
summary(average_swing_speed$weight)


# Height in inches
convert_height_to_inches <- function(height_string) {
  
  height_parts <- str_match(height_string, "(\\d+)' (\\d+)")
  
  # Numeric Values
  feet <- as.numeric(height_parts[, 2])
  inches <- as.numeric(height_parts[, 3])
  
  # Convert height to total inches
  height_inches <- (feet * 12) + inches
  return(height_inches)
}

# Apply the height conversion function to the height column
average_swing_speed <- average_swing_speed %>%
  mutate(height_inches = sapply(height, convert_height_to_inches))

# Mean of age, height, and weight
height_mean <- mean(average_swing_speed$height_inches, na.rm = TRUE)
weight_mean <- mean(average_swing_speed$weight, na.rm = TRUE)
age_mean<- mean(average_swing_speed$age, na.rm = TRUE)

# Center age, height, and weight
average_swing_speed <- average_swing_speed %>%
  mutate(
    height_centered = height_inches - height_mean,
    weight_centered = weight - weight_mean,
    age_centered = age - age_mean
  )

summary(average_swing_speed$age)





# Individual PLayer stats
player_swing_stats <- swing1 %>%
  group_by(batter) %>%
  summarise(
    total_pitches = n(),  # Total number of pitches seen by each player
    total_swings = sum(!is.na(bat_speed))  # Count swings (where avg_swing_speed is not NA)
  )

# Join player stats 
average_swing_speed <- average_swing_speed %>%
  left_join(player_swing_stats, by = "batter")

ggplot(data = swing1, aes(x = bat_speed, color = factor(batter))) +
  geom_density() +
  labs(
    title = "Distribution of Swing Speeds by Batter",
    x = "Swing Speed (mph)",
    y = "Density"
  ) +
  guides(color = "none") +  # Remove the color legend
  theme_minimal()




#Find Player's Team
swing1 <- swing1 %>%
  mutate(
    bat_team = ifelse(inning_topbot == "top", away_team, home_team)
  )
# Join the centered columns from average_swing_speed to swing1
swing1 <- swing1 %>%
  left_join(average_swing_speed %>% 
              dplyr::select(batter, height_centered, weight_centered, age_centered), by = "batter")


# Subset the dataset
swing1_subset <- swing1 %>%
  dplyr::select(
    pitch_type, release_speed, release_pos_x, release_pos_z, player_name,
    zone, stand, p_throws, pfx_x, pfx_z, outs_when_up,
    release_spin_rate, release_extension, plate_x, plate_z, 
    pitch_number, bat_speed, swing_length, bat_score_diff, balls, strikes,
    age_bat, n_thruorder_pitcher, n_priorpa_thisgame_player_at_bat, 
    batter_days_since_prev_game, bat_team, height_centered, weight_centered, game_date, batter
  )

# Only pitches with swings
swing1_clean <- swing1_subset %>%
  filter(!is.na(bat_speed))


swing1_swing<-swing1 %>%
  filter(!is.na(bat_speed))

ggplot(data = swing1_subset, aes(x = bat_speed, color = factor(bat_team))) +
  geom_density() +
  labs(title = "Distribution of Swing Speeds by Batter",x = "Swing Speed (mph)",y = "Density") +
  guides(color = "none") +  
  theme_minimal()

#Team Level stats
swing_summary <- swing1 %>%
  filter(!is.na(bat_speed)) %>%
  group_by(bat_team) %>%
  summarise(
    mean_swing_speed = mean(bat_speed),
    sd_swing_speed = sd(bat_speed),
    count = n()
  )

summary(average_swing_speed)

# List of events and their corresponding baserunning distances
event_distances <- c(
  "single" = 90,
  "grounded_into_double_play" = 90,
  "double_play" = 90,
  "force_out" = 90,
  "sac_bunt" = 90,
  "triple_play" = 90,
  "fielder's_choice" = 90,
  "field_error" = 90,
  "field_out" = 90,
  "fielder's_choice_out" = 90,
  "sac_fly" = 90,
  "sac_fly_double_play" = 90,
  "double" = 180,
  "triple" = 270
)

# Add a baserunning distance column to swing1
swing1 <- swing1 %>%
  dplyr::mutate(
    baserunning_distance = dplyr::case_when(
      events %in% names(event_distances) ~ as.numeric(event_distances[events]),
      TRUE ~ 0
    )
  )
# Reformat player_name to match Name_Last_First (Last, First)
average_swing_speed <- average_swing_speed %>%
  mutate(Name_Last_First = str_replace(player_name, "(\\w+)\\s(\\w+)", "\\2, \\1"))

swing1<- swing1 %>%
  mutate(Name_Last_First = player_name)

# Summarize baserunning distance for each player
player_baserunning_distance <- swing1 %>%
  dplyr::group_by(Name_Last_First) %>%
  dplyr::summarise(total_baserunning_distance = sum(baserunning_distance, na.rm = TRUE))

# Join the total baserunning distance with the average_swing_speed dataset
average_swing_speed <- average_swing_speed %>%
  dplyr::left_join(player_baserunning_distance, by = "Name_Last_First")

# View the updated dataset
head(average_swing_speed)


###Fielding Stats
# Initialize an empty list to store fielding stats for each player
fielding_stats_list <- list()

# Loop through each player ID
for (id in player_ids) {
  # API URL for fielding stats
  url <- paste0("https://statsapi.mlb.com/api/v1/people?personIds=", id, 
                "&hydrate=stats(group=[fielding],type=[byDateRange],season=2024)")
  
  # GET request
  response <- GET(url)
  
  # Check if the response is successful
  if (status_code(response) == 200) {
    player_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    if (!is.null(player_data$people) && length(player_data$people) > 0) {
      # Extract the player's fielding stats
      player <- player_data$people[1,]
      fielding_splits <- player$stats[[1]]$splits
      
      # Extract relevant fields for all positions played
      if (length(fielding_splits) > 0) {
        fielding_stats <- lapply(fielding_splits, function(split) {
          data.frame(
            batter = id,
            fullName = player$fullName,
            innings = as.numeric(split$stat$innings),
            range_factor = as.numeric(split$stat$rangeFactorPer9Inn),
            stringsAsFactors = FALSE
          )
        })
        
        # Combine all positions for this player into a single data frame
        player_fielding_stats <- bind_rows(fielding_stats) %>%
          filter(!is.na(innings), !is.na(range_factor))  # Remove invalid rows
        
        # Summarize to the player level
        player_summary <- player_fielding_stats %>%
          summarise(
            total_innings = sum(innings, na.rm = TRUE),
            weighted_range_factor = sum(range_factor * innings, na.rm = TRUE) / sum(innings, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            batter = id,
            fullName = player$fullName
          )
        
        # Append to the list
        fielding_stats_list[[length(fielding_stats_list) + 1]] <- player_summary
      }
    } else {
      # Debugging: No player data
      print(paste("No player data for batter ID:", id))
    }
  } else {
    # Debugging: Failed request
    print(paste("Failed to retrieve data for batter ID:", id, "with status code:", status_code(response)))
  }
  
  # Delay to avoid hitting API limits
  Sys.sleep(0.1)
}

# Combine all players' fielding stats into a single data frame
fielding_stats_df <- bind_rows(fielding_stats_list)

# View the resulting data frame
print(fielding_stats_df)

# Join fielding_stats_df to average_swing_speed on `batter`
average_swing_speed <- average_swing_speed %>%
  left_join(
    fielding_stats_df %>% dplyr::select(-fullName),  # Exclude fullName column from fielding_stats_df
    by = "batter"
  )

# View the resulting dataset
print(average_swing_speed)

average_swing_speed <- average_swing_speed %>%
  mutate(rf_innings = weighted_range_factor*total_innings)

average_swing_speed <- average_swing_speed %>%
  distinct(batter, .keep_all = TRUE)

