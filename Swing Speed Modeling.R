library(gtsummary)
library("rdd")


#Remove DH's, P's, and Catchers
average_swing_speed_clean <- average_swing_speed %>%
  filter(primary_position != 'P' & primary_position != 'TWP' & primary_position != 'DH' )

average_swing_speed_clean <- average_swing_speed_clean %>%
  dplyr::mutate(
    position_basic = dplyr::case_when(
      primary_position %in% c("LF", "CF", "RF", "OF") ~ "Outfield",  # Outfield positions
      primary_position %in% c("1B", "2B", "3B", "SS") ~ "Infield",  # Infield positions
      primary_position == "C" ~ "Catcher",  # Catcher position
      TRUE ~ "Other"  # Any other position (e.g., DH, P)
    )
  )

#Calculate average swing length by batter in swing1_swing
average_swing_length <- swing1_swing %>%
  dplyr::group_by(batter) %>%
  dplyr::summarise(avg_swing_length = mean(swing_length, na.rm = TRUE)) %>%
  ungroup()

# Join the calculated average swing length with average_swing_speed_clean
average_swing_speed_clean <- average_swing_speed_clean %>%
  left_join(average_swing_length, by = "batter")

# Replace NA with 0 for relevant columns
average_swing_speed_clean <- average_swing_speed_clean %>%
  mutate(across(
    where(is.numeric),  # Select only numeric columns
    ~ replace_na(., 0)  # Replace NA with 0
  ))


# Run ANOVA
anova_results <- aov(avg_swing_speed ~ primary_position, data = average_swing_speed_clean)

# View summary
summary(anova_results)

# Tukey's Honest Significant Difference (HSD) test
post_hoc <- TukeyHSD(anova_results)

# View post-hoc results
print(post_hoc)


# Run T Test
t.test.results2 <- pairwise.t.test(x = average_swing_speed_clean$avg_swing_speed, 
                                  g = average_swing_speed_clean$position_basic, 
                                  p.adjust.method = "holm")

# View summary
print(t.test.results2)




#Modeling
il_model1<-lm(I(Total_Days_Out^2)~height_inches:weight+age_centered+total_swings:I(avg_swing_speed^2)+
                primary_position:rf_innings+total_baserunning_distance+hit_by_pitch, 
              data = average_swing_speed_clean)
summary(il_model1)

il_model2<-lm(Total_Days_Out~height_inches:weight+age_centered+log(total_swings):log(avg_swing_speed)+
                position_basic:rf_innings+total_baserunning_distance+hit_by_pitch, 
              data = no_catch)
summary(il_model2)

# Fit a Poisson regression model
il_model1_poisson <- glm(
  Total_Days_Out ~ 
    as.numeric(height_inches):weight + 
    age_centered + 
    log(total_swings):avg_swing_speed + 
    primary_position:rf_innings + 
    total_innings + 
    total_baserunning_distance,
  family = poisson(link = "log"),  # Specify Poisson family with log link
  data = average_swing_speed_clean
)

# Summarize the Poisson regression model
summary(il_model1_poisson)

# Calculate dispersion parameter for Poisson model
dispersion_parameter <- il_model1_poisson$deviance / il_model1_poisson$df.residual

# Print the dispersion parameter
print(paste("Dispersion Parameter:", round(dispersion_parameter, 4)))

# Check for overdispersion
if (dispersion_parameter > 1.5) {
  print("Overdispersion detected. Consider using a quasi-Poisson or negative binomial model.")
} else if (dispersion_parameter < 0.75) {
  print("Underdispersion detected. Check the model assumptions.")
} else {
  print("Dispersion parameter is within the acceptable range.")
}

# Fit a quasi-Poisson regression model
il_model1_quasipoisson <- glm(
  Total_Days_Out ~ 
    as.numeric(height_inches):weight + 
    age_centered + 
    log(total_swings):log(avg_swing_speed) + 
    primary_position :rf_innings + 
    total_innings + 
    total_baserunning_distance,
  family = quasipoisson(link = "log"),  # Quasi-Poisson family for overdispersion
  data = average_swing_speed_clean
)

# Summarize the quasi-Poisson regression model
summary(il_model1_quasipoisson)

# Load the MASS package for negative binomial regression
library(MASS)

# Refit the model with a negative binomial family
il_model1_negbin <- glm.nb(
  Total_Days_Out ~ 
    as.numeric(height_inches):weight + 
    age_centered + 
    total_swings:I(avg_swing_speed^2) + 
    primary_position : rf_innings +
    total_baserunning_distance+avg_swing_length,
  data = average_swing_speed_clean
)

# Summary of the negative binomial model
summary(il_model1_negbin)

# Compare AIC for Poisson and Negative Binomial models
AIC(il_model1_poisson, il_model1_negbin)

# Install the pscl package if not already installed
if (!requireNamespace("pscl", quietly = TRUE)) {
  install.packages("pscl")
}

library(pscl)

il_model1_zinb <- zeroinfl(
  Total_Days_Out ~ 
    as.numeric(height_inches):weight + 
    total_swings:avg_swing_speed + 
    position_basic:rf_innings +avg_swing_length |  # Negative binomial component
    age_centered + 
    rf_innings+total_baserunning_distance,  # Zero-inflation component
  data = average_swing_speed_clean,
  dist = "negbin"
)


# Summary of the ZINB model
summary(il_model1_zinb)

AIC(il_model1_zinb)

library(car)
vif_model <- lm(Total_Days_Out ~ as.numeric(height_inches):weight + total_swings:avg_swing_speed +
                  position_basic:rf_innings, data = average_swing_speed_clean)
vif(vif_model)


average_swing_speed_clean <- average_swing_speed_clean %>%
  mutate(
    height_inches_scaled = scale(as.numeric(height_inches)),
    weight_scaled = scale(weight),
    total_swings_scaled = scale(total_swings),
    avg_swing_speed_scaled = scale(avg_swing_speed),
    rf_innings_scaled = scale(rf_innings),
    total_baserunning_distance_scaled = scale(total_baserunning_distance),
    swing_length_scaled = scale(avg_swing_length)
  )

no_catch<- average_swing_speed_clean %>%
  filter(primary_position != 'C' )
ggplot(no_catch, aes(x = Total_Days_Out)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Total Days Out",
    x = "Total Days Out",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  )

il_model1_zinb <- zeroinfl(
  Total_Days_Out ~ 
    height_inches_scaled:weight_scaled + 
    total_swings_scaled:avg_swing_speed_scaled + 
    position_basic:rf_innings_scaled +swing_length_scaled| 
    age_centered + rf_innings_scaled + total_baserunning_distance_scaled,
  data = no_catch,
  dist = "negbin"
)
summary(il_model1_zinb)
stargazer(il_model1_zinb)
AIC(il_model1_zinb)
plot(il_model1_zinb)
# Paired t-test
t_test_results1 <- t.test(
  pre_post_injury_results$pre_injury_swing_speed,
  pre_post_injury_results$post_injury_swing_speed,
  paired = TRUE,
  na.rm = TRUE
)

print(t_test_results1)

#Swing Speed Variance by Zone and Pitch Type?
unique(swing$pitch_type)

# Two-way ANOVA
swing_speed_aov <- aov(bat_speed ~ pitch_type_basic *as.factor(zone), data = swing1_swing)

# Summary of the ANOVA
summary(swing_speed_aov)
# Plot residuals
plot(swing_speed_aov)

# Tukey's HSD Test for pairwise comparisons
TukeyHSD(swing_speed_aov)
 


model3<-lm(bat_speed~age_centered+height_centered:weight_centered+ 
             pitch_type_basic:as.factor(zone)+release_speed:pitch_type_basic+ 
             release_spin_rate:pitch_type_basic, data = swing1_swing)
summary(model3)

#Predict Injuries

#Stat Compilation




# Join batter from swing1 to position_player_table
position_player_table <- position_player_table %>%
  left_join(
    swing1 %>% dplyr::select(batter, Name_Last_First) %>% distinct(),  # Select and deduplicate batter and Name_Last_First
    by = "Name_Last_First"
  )

# View the updated position_player_table
print(position_player_table)


swing1_subset2 <- swing1 %>%
  filter(!is.na(events) & events != "" & events != "strikeout" & events != "walk")

batter_names <- unique(position_player_table$batter)

swing1_filtered <-swing1_subset2 %>%
  filter(fielder_2 %in% batter_names | 
           fielder_3 %in% batter_names | 
           fielder_4 %in% batter_names | 
           fielder_5 %in% batter_names | 
           fielder_6 %in% batter_names | 
           fielder_7 %in% batter_names | 
           fielder_8 %in% batter_names | 
           fielder_9 %in% batter_names
  )

# Step 2: Initialize an empty results data frame
appearance_counts <- data.frame()

# Step 3: Loop through each row in `position_player_table` (one row per injury)
for (i in 1:nrow(position_player_table)) {
  player <- position_player_table$batter[i]
  injury_date <- position_player_table$`Injury / Surgery Date`[i]
  
  # Filter swings for the current player
  player_swing_data <- swing1_filtered %>%
    filter(
      batter == player | fielder_2 == player | fielder_3 == player | 
        fielder_4 == player | fielder_5 == player | fielder_6 == player | 
        fielder_7 == player | fielder_8 == player | fielder_9 == player
    )
  
  # Pre-2-weeks subset
  pre_2_weeks_count <- player_swing_data %>%
    filter(game_date <= (injury_date - 14)) %>%
    nrow()
  
  # 2-weeks-to-Injury subset
  two_weeks_to_injury_count <- player_swing_data %>%
    filter(game_date > (injury_date - 14) & game_date <= injury_date) %>%
    nrow()
  
  # Add counts to the results data frame
  appearance_counts <- rbind(
    appearance_counts,
    data.frame(
      batter = player,
      injury_date = injury_date,  # Include Injury Date for verification
      pre_2_weeks_count = pre_2_weeks_count,
      two_weeks_to_injury_count = two_weeks_to_injury_count
    )
  )
}

# Step 4: Merge counts back with position_player_table
position_player_table_updated <- position_player_table%>%
  left_join(appearance_counts, by = c("batter", "Injury / Surgery Date" = "injury_date"))



position_player_table_updated <- position_player_table_updated %>%
  left_join(
    average_swing_speed_clean %>% 
      dplyr::select(batter, age, weight, height_inches, primary_position), 
    by = "batter"
  )



# Ensure proper date formats
position_player_table <- position_player_table %>%
  mutate(`Injury / Surgery Date` = as.Date(`Injury / Surgery Date`))

swing1 <- swing1 %>%
  mutate(game_date = as.Date(game_date))

# Define the descriptions of interest
valid_descriptions <- c(
  "hit_into_play", "foul", "called_strike", "foul_tip",
  "ball", "swinging_strike", "blocked_ball", "swinging_strike_blocked",
  "hit_by_pitch", "foul_bunt", "pitchout", "missed_bunt", "bunt_foul_tip"
)

# Expand injury table with time periods
injury_with_periods <- position_player_table %>%
  rowwise() %>%
  mutate(
    `2_weeks_before_start` = `Injury / Surgery Date` - 14,
    `2_weeks_until_injury` = `Injury / Surgery Date` - 14
  ) %>%
  ungroup()

# Summarize pitch and swing metrics for long-term and short-term periods
# Summarize pitch and swing metrics for long-term and short-term periods
swing_summary <- swing1 %>%
  filter(description %in% valid_descriptions) %>% # Filter valid swing descriptions
  dplyr::inner_join(
    injury_with_periods,
    by = "batter",
    relationship = "many-to-many"
  ) %>%
  # Assign each swing to a time period
  dplyr::mutate(
    time_period = dplyr::case_when(
      game_date < `Injury / Surgery Date` - 14 ~ "long_term", # Entire season before 2 weeks
      game_date >= `Injury / Surgery Date` - 14 & game_date < `Injury / Surgery Date` ~ "short_term", # 2 weeks before
      TRUE ~ NA_character_ # Exclude other data
    )
  ) %>%
  dplyr::filter(!is.na(time_period)) %>% # Remove irrelevant data
  dplyr::group_by(batter, `Injury / Surgery Date`, time_period) %>%
  dplyr::summarise(
    # Total pitches and swings
    total_pitches = dplyr::n(),
    total_swings = sum(description %in% c(
      "hit_into_play", "foul", "foul_tip", "swinging_strike", "swinging_strike_blocked"
    )),
    swing_percentage = total_swings / total_pitches,
    
    # In-zone metrics (only for swings in the strike zone)
    total_inzone_swings = sum(
      in_strike_zone == 1 & description %in% c(
        "hit_into_play", "foul", "foul_tip", "swinging_strike", "swinging_strike_blocked"
      ),
      na.rm = TRUE
    ),
    total_inzone_contact = sum(
      in_strike_zone == 1 & description %in% c("hit_into_play", "foul", "foul_tip"),
      na.rm = TRUE
    ),
    inzone_contact_rate = ifelse(
      total_inzone_swings > 0,
      total_inzone_contact / total_inzone_swings,
      NA
    ),
    .groups = "drop"
  ) %>%
  # Pivot the results to wide format
  tidyr::pivot_wider(
    names_from = time_period,
    values_from = c(
      total_pitches, total_swings, swing_percentage,
      total_inzone_swings, total_inzone_contact, inzone_contact_rate
    ),
    names_prefix = ""
  )

position_player_table_updated <- position_player_table_updated %>%
  left_join(
    swing_summary, 
    by = c("batter", "Injury / Surgery Date")
  )


# View the result
head(swing_summary)

library(purrr)

pitch_type_injury_summary <- position_player_table %>%
  dplyr::select(batter, `Injury / Surgery Date`) %>%
  distinct() %>%
  rowwise() %>%
  mutate(
    # Long-term: Entire season up to 2 weeks before injury
    total_pitches_long_term = swing1 %>%
      filter(
        batter == !!batter,
        game_date < as.Date(`Injury / Surgery Date`) - 14,
        description %in% valid_descriptions
      ) %>%
      nrow(),
    
    total_offspeed_long_term = swing1 %>%
      filter(
        batter == !!batter,
        game_date < as.Date(`Injury / Surgery Date`) - 14,
        description %in% valid_descriptions,
        pitch_type_basic == "Offspeed"
      ) %>%
      nrow(),
    
    total_breaking_long_term = swing1 %>%
      filter(
        batter == !!batter,
        game_date < as.Date(`Injury / Surgery Date`) - 14,
        description %in% valid_descriptions,
        pitch_type_basic == "Breaking Ball"
      ) %>%
      nrow(),
    
    offspeed_percentage_long_term = ifelse(
      total_pitches_long_term > 0,
      total_offspeed_long_term / total_pitches_long_term * 100,
      NA
    ),
    breaking_percentage_long_term = ifelse(
      total_pitches_long_term > 0,
      total_breaking_long_term / total_pitches_long_term * 100,
      NA
    ),
    
    # Short-term: 2 weeks leading up to the injury
    total_pitches_short_term = swing1 %>%
      filter(
        batter == !!batter,
        game_date < as.Date(`Injury / Surgery Date`),
        game_date >= as.Date(`Injury / Surgery Date`) - 14,
        description %in% valid_descriptions
      ) %>%
      nrow(),
    
    total_offspeed_short_term = swing1 %>%
      filter(
        batter == !!batter,
        game_date < as.Date(`Injury / Surgery Date`),
        game_date >= as.Date(`Injury / Surgery Date`) - 14,
        description %in% valid_descriptions,
        pitch_type_basic == "Offspeed"
      ) %>%
      nrow(),
    
    total_breaking_short_term = swing1 %>%
      filter(
        batter == !!batter,
        game_date < as.Date(`Injury / Surgery Date`),
        game_date >= as.Date(`Injury / Surgery Date`) - 14,
        description %in% valid_descriptions,
        pitch_type_basic == "Breaking Ball"
      ) %>%
      nrow(),
    
    offspeed_percentage_short_term = ifelse(
      total_pitches_short_term > 0,
      total_offspeed_short_term / total_pitches_short_term * 100,
      NA
    ),
    breaking_percentage_short_term = ifelse(
      total_pitches_short_term > 0,
      total_breaking_short_term / total_pitches_short_term * 100,
      NA
    )
  ) %>%
  ungroup() %>%
  dplyr::select(
    batter, `Injury / Surgery Date`,
    offspeed_percentage_long_term, breaking_percentage_long_term,
    offspeed_percentage_short_term, breaking_percentage_short_term
  )

position_player_table_updated <- position_player_table_updated %>%
  dplyr::left_join(
    pitch_type_injury_summary,
    by = c("batter", "Injury / Surgery Date")
  )

# View the result
head(pitch_type_injury_summary)

# Calculate baserunning distance and average swing speed for both time periods
baserunning_swing_summary <- swing1 %>%
  dplyr::inner_join(
    position_player_table %>%
      dplyr::select(batter, `Injury / Surgery Date`) %>%
      dplyr::distinct(),
    by = "batter"
  ) %>%
  dplyr::mutate(
    # Define time periods
    time_period = dplyr::case_when(
      game_date < `Injury / Surgery Date` - 14 ~ "long_term",  # Season until 2 weeks before injury
      game_date >= `Injury / Surgery Date` - 14 & game_date < `Injury / Surgery Date` ~ "short_term", # 2 weeks until injury
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(time_period)) %>%  # Keep only relevant periods
  dplyr::group_by(batter, `Injury / Surgery Date`, time_period) %>%
  dplyr::summarise(
    total_baserunning_distance = sum(baserunning_distance, na.rm = TRUE),  # Sum baserunning distance
    avg_swing_speed = mean(bat_speed, na.rm = TRUE),  # Average swing speed
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = time_period,
    values_from = c(total_baserunning_distance, avg_swing_speed),
    names_prefix = ""
  )

# Join the results back to position_player_table
position_player_table_updated <- position_player_table_updated %>%
  dplyr::left_join(
    baserunning_swing_summary,
    by = c("batter", "Injury / Surgery Date")
  )

# Perform the join
position_player_table_updated <- position_player_table_updated %>%
  dplyr::left_join(
    average_swing_speed_clean %>%
      dplyr::select(batter, weighted_range_factor),  # Select relevant columns
    by = "batter"
  )

# Check the resulting table
dplyr::glimpse(position_player_table_updated)


#Total Calculations 
position_player_table_updated$baserun_dist <- position_player_table_updated$total_baserunning_distance_long_term+
  position_player_table_updated$total_baserunning_distance_short_term
position_player_table_updated$plays<-position_player_table_updated$pre_2_weeks_count+
  position_player_table_updated$two_weeks_to_injury_count
position_player_table_updated$range_plays<- position_player_table_updated$plays*
  position_player_table_updated$weighted_range_factor

#Deviation Calculation
position_player_table_updated$swing_deviation <- 
  position_player_table_updated$avg_swing_speed_long_term-position_player_table_updated$avg_swing_speed_short_term
position_player_table_updated$break_dev <- 
  position_player_table_updated$breaking_percentage_long_term-position_player_table_updated$breaking_percentage_short_term
position_player_table_updated$off_dev <- 
  position_player_table_updated$offspeed_percentage_long_term-position_player_table_updated$offspeed_percentage_short_term
position_player_table_updated$zone_dev <- 
  position_player_table_updated$inzone_contact_rate_long_term-position_player_table_updated$inzone_contact_rate_short_term


model4<-lm(swing_deviation~ age+height_inches:weight+ break_dev +off_dev + 
             zone_dev+ total_swings_long_term+ total_swings_short_term+ total_baserunning_distance_long_term+
             primary_position:plays+baserun_dist+as.factor(IL_Type_Days)+Category_refined+Injury_Type, 
           data = position_player_table_updated)
summary(model4)

position_player_table_updated <- position_player_table_updated %>%
  mutate(
    swing_dev_scaled = scale(swing_deviation),
    age_scaled = scale(age),
    height_inches_scaled = scale(as.numeric(height_inches)),
    weight_scaled = scale(weight),
    total_swings_short_scaled = scale(total_swings_short_term),
    total_swings_long_scaled = scale(total_swings_long_term),
    break_dev_scaled = scale(break_dev),
    off_dev_scaled = scale(off_dev),
    zone_dev_scaled = scale(zone_dev),
    pre_2_weeks_count_scaled = scale(pre_2_weeks_count),
    two_weeks_scaled = scale(two_weeks_to_injury_count),
    baserun_scaled = scale(baserun_dist),
    range_plays_scaled = scale(range_plays)
  )
position_player_table_updated <- position_player_table_updated %>%
  dplyr::distinct(batter, `Injury / Surgery Date`, .keep_all = TRUE)

built_up<- position_player_table_updated%>%
  filter(Injury_Type == "Built-Up")
model5<-lm(swing_dev_scaled~ age_scaled+ height_inches_scaled:weight_scaled+ break_dev_scaled +off_dev_scaled + 
             zone_dev_scaled+ total_swings_long_scaled+ total_swings_short_scaled+
             primary_position+pre_2_weeks_count_scaled+ two_weeks_scaled+Injury_Type:Category_refined, 
           data = position_player_table_updated)
summary(model5)

built_up <- built_up %>%
  dplyr::mutate(
    position_basic = dplyr::case_when(
      primary_position %in% c("LF", "CF", "RF", "OF") ~ "Outfield",  # Outfield positions
      primary_position %in% c("1B", "2B", "3B", "SS") ~ "Infield",  # Infield positions
      primary_position == "C" ~ "Catcher",  # Catcher position
      TRUE ~ "Other"  # Any other position (e.g., DH, P)
    )
  )
model6<-lm(swing_dev_scaled~ age_scaled+ height_inches_scaled:weight_scaled+ break_dev_scaled +off_dev_scaled + 
             zone_dev_scaled+ total_swings_long_scaled+ total_swings_short_scaled+
             position_basic:range_plays_scaled+baserun_scaled+Category_refined, 
           data = built_up)
summary(model6)

built_no_catch <- built_up %>%
  filter(primary_position != "C")

model7<-lm(swing_dev_scaled~ age_scaled+ height_inches_scaled:weight_scaled+ break_dev_scaled +off_dev_scaled + 
             zone_dev_scaled+ total_swings_long_scaled+ total_swings_short_scaled+
             position_basic+baserun_scaled+Category_refined, 
           data = built_no_catch)
summary(model7)

plot(model7)

#Data Visualization
library(zoo)
# Calculate rolling averages
swing1_swing <- swing1_swing %>%
  dplyr::arrange(batter, game_date) %>% # Ensure data is ordered by batter and date
  dplyr::group_by(batter) %>%
  dplyr::mutate(
    # Rolling season average swing speed
    rolling_season_avg_swing_speed = dplyr::cummean(bat_speed),
    
    # 50-swing rolling average swing speed using zoo::rollapply
    rolling_50_swing_avg_swing_speed = zoo::rollapply(
      bat_speed, 
      width = 50, 
      FUN = mean, 
      fill = NA, 
      align = "right"
    )
  ) %>%
  dplyr::ungroup()

swing1_swing$speed_diff <- swing1_swing$rolling_season_avg_swing_speed - swing1_swing$rolling_50_swing_avg_swing_speed
library(ggplot2)

swing1_swing <- swing1_swing %>%
  dplyr::arrange(game_date, inning)  # Sort by date and inning

# Add a swing_number column
swing1_swing <- swing1_swing %>%
  dplyr::group_by(batter) %>% # Group by batter
  dplyr::mutate(swing_number = row_number()) %>% # Create a cumulative count of swings
  dplyr::ungroup()

# Create the graph
ggplot(swing1_swing, aes(x = swing_number, y = speed_diff, color = factor(batter))) +
  geom_line(alpha =0.5) + # Line plot
  labs(
    title = "Difference Between Rolling Averages of Swing Speed by Batter",
    x = "Game Date",
    y = "Swing Speed Difference (Season Avg - 50-Swing Avg)",
    color = "Batter"
  ) +
  theme_minimal() +
  theme(legend.position = "none") # Optional: Adjust legend position

swing1_swing <- swing1_swing %>%
  dplyr::mutate(
    in_position_table = ifelse(batter %in% position_player_table$batter, "In Table", "Not In Table")
  )

# Create a plot with unique colors for "In Table" batters and dulled gray for others
ggplot(swing1_swing, aes(x = swing_number, y = speed_diff, group = batter)) +
  geom_line(aes(color = interaction(batter, in_position_table), alpha = in_position_table)) +
  scale_color_manual(
    values = c(
      setNames(
        rainbow(length(unique(swing1_swing$batter[swing1_swing$in_position_table == "In Table"]))), # Unique colors for "In Table"
        unique(swing1_swing$batter[swing1_swing$in_position_table == "In Table"])
      ),
      "Not In Table" = "gray"
    )
  ) +
  scale_alpha_manual(values = c("In Table" = 1, "Not In Table" = 0.3)) +
  labs(
    title = "Swing Speed Difference Highlighting Players in Position Player Table",
    x = "Swing Number",
    y = "Swing Speed Difference (Season Avg - 50-Swing Avg)",
    color = "Player",
    alpha = "Player Status"
  ) +
  theme_minimal()


# Calculate first and second half swing speed averages
swing_half_averages <- swing1_swing %>%
  mutate(
    game_date = as.Date(game_date),  # Ensure the date column is in Date format
    half = ifelse(game_date < as.Date("2024-06-26"), "First_Half", "Second_Half")  # Define the half
  ) %>%
  dplyr::group_by(batter, half) %>%
  dplyr::summarise(
    avg_swing_speed = mean(bat_speed, na.rm = TRUE),  # Calculate the average swing speed
    total_swings = n(),  # Optional: Add total swings for each half
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = half, 
    values_from = c(avg_swing_speed, total_swings),
    names_prefix = ""
  )


swing1_swing <- swing1_swing %>%
  dplyr::left_join(
    swing_half_averages,
    by = "batter"
  )


# Filter batters with at least 50 swings before and after June 26, 2024
batters_with_50_swings <- swing1_swing %>%
  mutate(game_date = as.Date(game_date)) %>%  # Ensure the date column is in Date format
  dplyr::group_by(batter) %>%
  dplyr::summarise(
    swings_before = sum(game_date < as.Date("2024-06-26")),
    swings_after = sum(game_date >= as.Date("2024-06-26")),
    .groups = "drop"  # Ungroup after summarizing
  ) %>%
  dplyr::filter(swings_before >= 50 & swings_after >= 50) %>%
  dplyr::pull(batter)  # Extract the list of batters who meet the criteria

# Subset the data for the selected batters
swing1_swing_filtered <- swing1_swing %>%
  filter(batter %in% batters_with_50_swings)

# Check the resulting dataset
swing1_swing_filtered

swing1_swing <- swing1_swing %>%
  arrange(batter, game_date, swing_number) %>%  # Ensure proper ordering
  group_by(batter) %>%
  mutate(
    # Rolling season average
    rolling_season_avg_swing_length = cummean(swing_length),
    
    # Rolling 50-swing average
    rolling_50_swing_avg_swing_length = rollmean(swing_length, 50, fill = NA, align = "right"),
    
    # Difference between the two rolling averages
    swing_length_diff = rolling_season_avg_swing_length - rolling_50_swing_avg_swing_length
  ) %>%
  ungroup()

# Preview the updated dataset
head(swing1_swing)
# Filter the dataset for Rafael Devers
devers_data <- swing1_swing %>%
  dplyr::filter(player_name == "Devers, Rafael")

# Create the plot
ggplot(devers_data, aes(x = swing_number)) +
  geom_line(aes(y = rolling_season_avg_swing_speed, color = "Season Rolling Avg")) +
  geom_line(aes(y = rolling_50_swing_avg_swing_speed, color = "50-Swing Moving Avg")) +
  geom_vline(xintercept = 705, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(
    values = c("Season Rolling Avg" = "blue", "50-Swing Moving Avg" = "red"),
    name = "Metric"
  ) +
  labs(
    title = "Rafael Devers Rolling Average Swing Speed",
    x = "Swing Number",
    y = "Swing Speed (mph)",
    color = "Legend"
  ) +
  theme_minimal()

ggplot(devers_data, aes(x = swing_number)) +
  geom_line(aes(y = rolling_season_avg_swing_length, color = "Season Rolling Avg")) +
  geom_line(aes(y = rolling_50_swing_avg_swing_length, color = "50-Swing Moving Avg")) +
  geom_vline(xintercept = 705, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(
    values = c("Season Rolling Avg" = "blue", "50-Swing Moving Avg" = "red"),
    name = "Metric"
  ) +
  labs(
    title = "Rafael Devers Rolling Average Swing Length",
    x = "Swing Number",
    y = "Swing Length (ft.)",
    color = "Legend"
  ) +
  theme_minimal()

# Calculate the swing speed differences and categorize by in_position_table
swing1_swing_filtered <- swing1_swing_filtered %>%
  dplyr::mutate(
    swing_speed_diff = avg_swing_speed_First_Half - avg_swing_speed_Second_Half
  )

# Perform a t-test for the swing speed differences
t_test_results3 <- t.test(
  swing_speed_diff ~ in_position_table, 
  data = swing1_swing_filtered,
  var.equal = TRUE # Assuming equal variances; change to FALSE if variances differ
)

# Display the t-test results
print(t_test_results3)
# Update labels in swing1_swing_filtered
swing1_swing_filtered <- swing1_swing_filtered %>%
  mutate(
    player_status = ifelse(in_position_table == "In Table", "Injured", "Non-Injured")
  )

# Create the violin and boxplot with updated labels
ggplot(swing1_swing_filtered, aes(x = player_status, y = swing_speed_diff, fill = player_status)) +
  geom_violin(trim = TRUE, alpha = 0.6) +
  geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA, alpha = 0.8) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, color = "black", fill = "yellow") +
  stat_summary(fun = mean, geom = "hline", aes(yintercept = ..y.., group = player_status),
               linetype = "dashed", color = "darkgray", size = 0.8) +
  labs(
    title = "Swing Speed Differences by Player Status",
    x = "Player Status",
    y = "Swing Speed Difference (First Half - Second Half)",
    fill = "Player Status"
  ) +
  scale_fill_manual(values = c("Injured" = "red", "Non-Injured" = "blue")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  )


rd_1<-RDestimate(bat_speed~ swing_number | pitch_type_basic+release_speed+in_strike_zone, 
                 data =devers_data, cutpoint = 705)

summary(rd_1)


# Visualize swing speed around the cutpoint
library(ggplot2)

ggplot(devers_data, aes(x = swing_number, y = bat_speed)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 705, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", data = subset(devers_data, swing_number < 705), aes(color = "Before Cutpoint")) +
  geom_smooth(method = "lm", data = subset(devers_data, swing_number >= 705), aes(color = "After Cutpoint")) +
  labs(
    title = "Swing Speed Around Swing Number 705",
    x = "Swing Number",
    y = "Swing Speed (mph)",
    color = "Regression Line"
  ) +
  theme_minimal()

#Calculate percentages of swings > 75 mph before and after swing number 705
swings_before_705 <- devers_data %>%
  filter(swing_number < 705 & bat_speed > 75) %>%
  nrow()
total_before_705 <- devers_data %>%
  filter(swing_number < 705) %>%
  nrow()
percentage_before_705 <- (swings_before_705 / total_before_705) * 100

swings_after_705 <- devers_data %>%
  filter(swing_number >= 705 & bat_speed > 75) %>%
  nrow()
total_after_705 <- devers_data %>%
  filter(swing_number >= 705) %>%
  nrow()
percentage_after_705 <- (swings_after_705 / total_after_705) * 100

# Display percentages
list(
  percentage_before_705 = percentage_before_705,
  percentage_after_705 = percentage_after_705
)
