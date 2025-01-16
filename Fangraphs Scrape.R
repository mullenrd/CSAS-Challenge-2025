# Install chromote if not installed
if (!requireNamespace("chromote", quietly = TRUE)) {
  install.packages("chromote")
}

library(chromote)
library(rvest)

# Start a Chromote session
b <- ChromoteSession$new()

# Navigate to the page
url <- "https://www.fangraphs.com/roster-resource/injury-report?groupby=injury&timeframe=all&season=2024"
b$Page$navigate(url)

# Wait for the page to fully load
Sys.sleep(5)  # Adjust sleep time as needed

# Get the rendered HTML
rendered_html <- b$DOM$getDocument()$root$nodeId
html_content <- b$DOM$getOuterHTML(nodeId = rendered_html)$outerHTML

# Parse the HTML with rvest
page <- read_html(html_content)
# Extract all tables from the rendered HTML
tables <- page %>% html_elements("table")

# Check the number of tables
length(tables)

# Print each table to find the one you need
for (i in seq_along(tables)) {
  cat("Table", i, ":\n")
  print(tables[[i]] %>% html_table())
}


# Assuming you have all tables in a list called `tables`
# Extract tables 15 to 46
selected_tables <- tables[15:46]

# Combine all selected tables into a single data frame
combined_table <- bind_rows(lapply(selected_tables, html_table))

combined_table <- combined_table[!duplicated(combined_table), ]

# View the combined table
print(combined_table)

#Position Player Injuries
position_player_table<-combined_table%>%
  filter(Pos != "SP"& Pos != "RP")
unique(position_player_table$`Injury / Surgery`)

categories <- list(
  "Arm Injuries" = c("elbow", "wrist", "hand","pinky", "finger", "thumb", "biceps", "shoulder", "Tommy John", "forearm", "rotator", "hamate"),
  "Leg Injuries" = c("ankle", "calf", "foot", "knee", "shin", "toe", "leg", "achilles", "hamstring", "plantar", "quad", "heel", "femoral"),
  "Back Injuries" = c("back", "spine", "lumbar", "lat", "neck"),
  "Core Injuries" = c("hip", "groin", "abdominal", "hernia", "core", "rib", "oblique"),
  "General Medical" = c("flu", "infection", "illness", "medical", "procedure"),
  "Other" = c("concussion", "cartilage", "fracture", "stress", "contusion")
)

# Function to assign a category based on keywords
assign_category <- function(injury) {
  for (category in names(categories)) {
    if (any(sapply(categories[[category]], function(keyword) grepl(keyword, injury, ignore.case = TRUE)))) {
      return(category)
    }
  }
  return("Uncategorized")  # Default if no keywords match
}

# Apply the function to the dataframe
position_player_table <- position_player_table %>%
  mutate(Category = sapply(`Injury / Surgery`, assign_category))

# View the updated dataframe
print(position_player_table)

categories_refined <- list(
  "Arm Injuries" = c("elbow", "biceps", "shoulder", "Tommy John", "forearm", "rotator"),
  "Wrist/Hand Injuries" = c("wrist", "hand", "pinky", "finger", "thumb", "hamate"),
  "Upper Leg Injuries" = c("knee", "leg", "hamstring", "quad", "femoral"),
  "Lower Leg Injuries" = c("calf", "shin", "achilles"),
  "Foot/Ankle Injuries" = c("ankle", "foot", "toe", "heel", "plantar"),
  "Back/Neck Injuries" = c("back", "spine", "lumbar", "lat", "neck"),
  "Core Injuries" = c("hip", "groin", "abdominal", "hernia", "core", "rib", "oblique"),
  "General Medical" = c("flu", "infection", "illness", "medical", "procedure"),
  "Other" = c("concussion", "cartilage", "fracture", "stress", "contusion")
)

# Function to assign a category based on keywords
assign_category <- function(injury) {
  for (category in names(categories_refined)) {
    if (any(sapply(categories_refined[[category]], function(keyword) grepl(keyword, injury, ignore.case = TRUE)))) {
      return(category)
    }
  }
  return("Uncategorized")  # Default if no keywords match
}

# Apply the function to the dataframe
position_player_table <- position_player_table %>%
  mutate(Category_refined = sapply(`Injury / Surgery`, assign_category))

# View the updated dataframe
print(position_player_table)

# Convert relevant columns to proper date format
position_player_table <- position_player_table %>%
  mutate(
    `Injury / Surgery Date` = as.Date(`Injury / Surgery Date`, format = "%m/%d/%y"),
    `Return Date` = as.Date(`Return Date`, format = "%m/%d/%y"),
    `IL Retro Date` = as.Date(`IL Retro Date`, format = "%m/%d/%y"),
    `Eligible to Return` = as.Date(`Eligible to Return`, format = "%m/%d/%y")
  )

# Create new columns for Days_Out and IL Type Category
position_player_table <- position_player_table %>%
  mutate(
    # Calculate Days_Out
    Days_Out = if_else(
      `Return Date` == as.Date("2024-09-30"),
      NA_real_,
      as.numeric(`Return Date` - `Injury / Surgery Date`)
    ),
    # Calculate IL Type Category
    IL_Type_Days = as.numeric(`Eligible to Return` - `IL Retro Date`)
  )

# View the updated dataframe
print(position_player_table)

il_days_summary <- position_player_table %>%
  dplyr::group_by(Category) %>%
  summarise(
    # Summary statistics for Days_Out
    Avg_Days_Out = mean(Days_Out, na.rm = TRUE),          # Average Days Out
    Median_Days_Out = median(Days_Out, na.rm = TRUE),     # Median Days Out
    Max_Days_Out = max(Days_Out, na.rm = TRUE),           # Maximum Days Out
    Min_Days_Out = min(Days_Out, na.rm = TRUE),           # Minimum Days Out
    
    # Count of rows
    Count = n()  # Count of injuries per category
  ) %>%
  arrange(desc(Avg_Days_Out))  # Sort by Avg_IL_Type_Days or any other column

# View the summary
print(il_days_summary)

ggplot(il_days_summary, aes(x = reorder(Category, Median_Days_Out))) +
  #geom_bar(aes(y = Avg_IL_Type_Days, fill = "IL Type Days"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Median_Days_Out, fill = "Days Out"), stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Average IL Type Days and Days Out by Category",
    x = "Category",
    y = "Average Days",
    fill = "Metric"
  )

position_player_table <- position_player_table %>%
  mutate(
    Injury_Type = case_when(
      # Reaction injuries
      `Injury / Surgery` %in% c(
        "Fractured ankle", "Fractured forearm", "Fractured hand", 
        "Fractured pisiform bone (right wrist)", "Fractured finger (right ring)", 
        "Fractured finger (left index)", "Fractured rib", "Fractured shoulder", 
        "Fractured shin", "Fractured toe", "Fractured hamate", 
        "Torn Achilles' tendon", "Torn rib cartilage", "Torn knee meniscus", 
        "Sprained ankle", "Sprained knee", "Sprained shoulder (AC joint)", 
        "Sprained thumb", "Concussion", "Rib contusion", "Thumb contusion",
        "Knee surgery", "Knee surgery (torn ACL)", "Knee surgery (torn meniscus)", 
        "Sprained wrist", "Fractured pinky (left)", "Fractured thumb (left)",
        "Fractured thumb (right)", "Fractured wrist (right)", 
        "Fractured wrist", "Fractured heel", "Fractured finger (left middle)",
        "Fractured hand (right pinkie)", "Fractured hand (right)", "Medical procedure", 
        "Non-displaced rib fractures", "Viral illness", "Influenza", 
        "Stomach flu", "Viral infection", "Fractured leg (fibula)", 
        "Fractured forearm (non-displaced)", "Fractured rib (non-displaced)",
        "Sprained elbow (non-throwing)", "Fielding collision", "Fractured finger (right middle)",
        "Knee contusion", "Sprained lower back", "Sprained shoulder", "Calf infection", 
        "Costochondral cartilage injury", "Foot infection", "Forearm surgery (fracture)",
        "Fractured foot (non-displaced)", "Kidney infection", "Knee bone bruise", 
        "Knee surgery (lateral meniscectomy)", "Knee surgery (meniscectomy)", "Knee surgery (meniscus)",
        "Non-displaced wrist fracture", "Shoulder surgery", "Shoulder surgery (fractured glenoid)",
        "Thumb surgery (torn ligament)", "Wrist bone bruise (left)"
      ) ~ "Reaction",
      
      # Built-Up injuries
      `Injury / Surgery` %in% c(
        "Tommy John surgery", "Shoulder surgery (labrum repair)", 
        "Shoulder surgery (rotator cuff)", "Strained rotator cuff", 
        "Herniated disc in back", "Plantar fasciitis", "Hip impingement", 
        "Wrist tendinitis", "Knee inflammation", "Wrist inflammation", 
        "Achilles tendinitis", "Biceps tendinitis", "Stress fracture (lumbar spine)", 
        "Hip surgery (labrum repair)", "Hip surgery (surfacing procedure)", 
        "Arthroscopic knee surgery", "Core muscle surgery", "Sports hernia surgery", 
        "Back inflammation (lumbar spine)", "Shoulder impingement", "Strained calf", 
        "Strained hamstring", "Strained quad", "Strained groin", "Strained shoulder", 
        "Strained lower back", "Strained oblique", "Back discomfort", 
        "Shoulder discomfort", "Thumb discomfort", "Hip discomfort", 
        "Lower back tightness", "Back inflammation", "Back spasms", 
        "Lower back spasms", "Lower back inflammation", "Strained back", 
        "Strained lat", "Knee discomfort", "Knee tendinitis", "Neck inflammation",
        "Arthroscopic wrist surgery", "Rib cage inflammation", "Hand inflammation (left)",
        "Strained hip flexor", "Strained hand", "Strained forearm", 
        "Shoulder surgery (torn labrum)", "Wrist surgery", "Elbow/Flexor tendon surgery",
        "Elbow surgery (internal brace)", "Hip flexor inflammation", 
        "Hip inflammation", "Spine disc injury", "Core muscle strain", 
        "Hip surgery (labrum repair)", "Torn thumb ligament", "Thumb surgery (ligament repair)",
        "Wrist surgery (ulnar styloid fracture)", "Bacterial infection", 
        "Femoral stress reaction", "Tenex procedure (right knee)", "Strained hip", "Plantar fascitis", 
        "Ankle procedure (bone spur removal)", "Arthroscopic hip surgery", "Back surgery",
        "Foot discomfort (partially torn plantar fascia)", "Forearm tightness", "Shoulder inflammation"
      ) ~ "Built-Up",
      
      # Unknown category
      TRUE ~ "Unknown"
    )
  )


# Count injuries by type
injury_summary <- position_player_table %>%
  group_by(Injury_Type) %>%
  summarise(Count = n())

# View the summary
print(injury_summary)

position_player_table <- position_player_table %>%
  separate(Name, into = c("First", "Last"), sep = " ", extra = "merge", fill = "right") %>%
  mutate(
    Name_Last_First = paste(Last, First, sep = ", ")  # Combine Last and First into "Last, First"
  ) %>%
  dplyr::select(-First, -Last)  # Remove temporary columns if not needed




injury_counts <- position_player_table %>%
  group_by(Name_Last_First) %>%
  summarise(times_injured = n())

average_swing_speed <- average_swing_speed %>%
  left_join(injury_counts, by = "Name_Last_First") %>%
  mutate(times_injured = ifelse(is.na(times_injured), 0, times_injured))


# Get the first three injury dates for each player
injury_dates <- position_player_table %>%
  group_by(Name_Last_First) %>%
  summarise(
    Injury_Date_1 = ifelse(length(`Injury / Surgery Date`) >= 1, `Injury / Surgery Date`[1], NA),
    Injury_Date_2 = ifelse(length(`Injury / Surgery Date`) >= 2, `Injury / Surgery Date`[2], NA),
    Injury_Date_3 = ifelse(length(`Injury / Surgery Date`) >= 3, `Injury / Surgery Date`[3], NA)
  )

# Convert injury date columns to Date format
injury_dates <- injury_dates %>%
  mutate(
    Injury_Date_1 = as.Date(Injury_Date_1, format = "%Y-%m-%d"),
    Injury_Date_2 = as.Date(Injury_Date_2, format = "%Y-%m-%d"),
    Injury_Date_3 = as.Date(Injury_Date_3, format = "%Y-%m-%d")
  )

# Join the injury dates with the average_swing_speed dataset
average_swing_speed <- average_swing_speed %>%
  left_join(injury_dates, by = "Name_Last_First")



calculate_pre_post_injury_speeds <- function(swing_data, injury_data) {
  # Ensure date columns are in the correct format
  swing_data <- swing_data %>%
    dplyr::mutate(game_date = as.Date(game_date))  # Convert game_date to Date
  
  injury_data <- injury_data %>%
    dplyr::mutate(`Injury / Surgery Date` = as.Date(`Injury / Surgery Date`))  # Convert injury date to Date
  
  # Join swings with injuries based on player name
  joined_data <- swing_data %>%
    dplyr::inner_join(
      injury_data %>% dplyr::select(Name_Last_First, `Injury / Surgery Date`, Category_refined),
      by = "Name_Last_First",
      relationship = "many-to-many"  # Explicitly handle many-to-many relationships
    )
  
  # Summarize pre- and post-injury swings for each injury
  summarized <- joined_data %>%
    dplyr::group_by(Name_Last_First, `Injury / Surgery Date`, Category_refined) %>%
    dplyr::summarise(
      pre_injury_swing_speed = mean(bat_speed[game_date < `Injury / Surgery Date`], na.rm = TRUE),
      post_injury_swing_speed = mean(bat_speed[game_date > `Injury / Surgery Date`], na.rm = TRUE),
      pre_injury_swing_count = sum(game_date < `Injury / Surgery Date`, na.rm = TRUE),
      post_injury_swing_count = sum(game_date > `Injury / Surgery Date`, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(summarized)
}


pre_post_injury_results <- calculate_pre_post_injury_speeds(swing1, position_player_table)

# View Results
head(pre_post_injury_results)

pre_post_injury_results <- pre_post_injury_results %>%
  dplyr::mutate(speed_deviation = pre_injury_swing_speed - post_injury_swing_speed)

# Calculate average speed deviation by Category
speed_deviation_by_category <- pre_post_injury_results %>%
  dplyr::group_by(Category_refined) %>%
  dplyr::summarise(avg_speed_deviation = mean(speed_deviation, na.rm = TRUE)) %>%
  dplyr::arrange(desc(avg_speed_deviation))

# Create the bar chart
ggplot(speed_deviation_by_category, aes(x = reorder(Category_refined, avg_speed_deviation), y = avg_speed_deviation)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip coordinates for horizontal bars
  labs(
    title = "Average Swing Speed Deviation by Injury Category",
    x = "Injury Category",
    y = "Average Speed Deviation (Pre - Post)"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))  # Adjust font size for readability

# Create the violin plot
ggplot(pre_post_injury_results, aes(x = Category_refined, y = speed_deviation)) +
  geom_violin(fill = "steelblue", color = "black") +
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkred") +  # Adds individual data points
  labs(
    title = "Swing Speed Deviation by Injury Category",
    x = "Injury Category",
    y = "Speed Deviation (Pre - Post)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )

# Summarize total Days_Out for each player in position_player_table
total_days_out <- position_player_table %>%
  dplyr::group_by(Name_Last_First) %>%
  dplyr::summarise(
    Total_Days_Out = sum(Days_Out, na.rm = TRUE),  # Sum all Days_Out for each player
    .groups = "drop"  # Drop grouping after summarization
  )

# Join the Total_Days_Out column to the average_swing_speed dataset
average_swing_speed <- average_swing_speed %>%
  dplyr::left_join(total_days_out, by = "Name_Last_First")

# Step 1: Add Speed Deviation and Reshape
pre_post_injury_wide <- pre_post_injury_results %>%
  group_by(Name_Last_First) %>%
  mutate(
    Injury_Number = row_number(),  # Assign a sequential number to each injury
    speed_deviation = pre_injury_swing_speed - post_injury_swing_speed  # Calculate speed deviation
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Injury_Number,
    values_from = c(
      pre_injury_swing_speed, post_injury_swing_speed, speed_deviation,
      pre_injury_swing_count, post_injury_swing_count, Category_refined
    ),
    names_prefix = "Injury_"
  ) %>%
  dplyr::select(-contains("Injury / Surgery Date"))  # Exclude date column

# Step 2: Join with `average_swing_speed`
average_swing_speed<- average_swing_speed %>%
  left_join(pre_post_injury_wide, by = "Name_Last_First")




# Aggregate player_info by batter
player_info_summary <- player_info %>%
  group_by(batter) %>%
  summarise(steal_attempts = sum(steal_attempts, na.rm = TRUE), .groups = "drop")

# Join aggregated data
average_swing_speed <- average_swing_speed %>%
  mutate(
    total_baserunning_distance = total_baserunning_distance + (steal_attempts * 90)
  )


# Replace NA values in `total_baserunning_distance` with 0 (if any)
average_swing_speed$total_baserunning_distance[is.na(average_swing_speed$total_baserunning_distance)] <- 0

# View updated dataset
print(average_swing_speed)


calculate_inzone_contact <- function(swing_data, injury_data, injury_cols) {
  # Ensure the swing_data has the proper date format
  swing_data <- swing_data %>%
    mutate(game_date = as.Date(game_date))
  
  # Ensure the injury_data has injury dates properly formatted
  injury_data <- injury_data %>%
    mutate(across(all_of(injury_cols), as.Date))
  
  # Initialize results
  results <- list()
  
  # Loop over each injury column
  for (i in seq_along(injury_cols)) {
    injury_col <- injury_cols[i]
    
    # Join swing data with injury data for this injury
    joined_data <- swing_data %>%
      inner_join(
        injury_data %>% dplyr::select(Name_Last_First, !!rlang::sym(injury_col)),
        by = "Name_Last_First",
        relationship = "many-to-many"  # Specify the relationship
      )
    
    # Pre-injury metrics
    pre_injury <- joined_data %>%
      filter(game_date < !!rlang::sym(injury_col) & in_strike_zone == 1) %>%
      group_by(Name_Last_First) %>%
      summarise(
        pre_inzone_contact = sum(description %in% c("hit_into_play", "foul", "foul_tip")) /
          sum(description %in% c("hit_into_play", "foul", "foul_tip", "swinging_strike")),
        .groups = "drop"
      )
    
    # Post-injury metrics
    post_injury <- joined_data %>%
      filter(game_date >= !!rlang::sym(injury_col) & in_strike_zone == 1) %>%
      group_by(Name_Last_First) %>%
      summarise(
        post_inzone_contact = sum(description %in% c("hit_into_play", "foul", "foul_tip")) /
          sum(description %in% c("hit_into_play", "foul", "foul_tip", "swinging_strike")),
        .groups = "drop"
      )
    
    # Combine pre- and post-injury results
    combined <- pre_injury %>%
      full_join(post_injury, by = "Name_Last_First") %>%
      rename_with(~ paste0(.x, "_", i), -Name_Last_First)
    
    # Store results
    results[[i]] <- combined
  }
  
  # Combine all results into a single dataset
  injury_results <- purrr::reduce(results, full_join, by = "Name_Last_First")
  
  # Calculate baseline metrics for the whole season
  baseline_results <- swing_data %>%
    filter(in_strike_zone == 1) %>%
    group_by(Name_Last_First) %>%
    summarise(
      baseline_inzone_contact = sum(description %in% c("hit_into_play", "foul", "foul_tip")) /
        sum(description %in% c("hit_into_play", "foul", "foul_tip", "swinging_strike")),
      .groups = "drop"
    )
  
  # Combine injury results with baseline results
  final_results <- injury_results %>%
    full_join(baseline_results, by = "Name_Last_First")
  
  return(final_results)
}


 


# Define the list of injury columns in `average_swing_speed`
injury_cols <- c("Injury_Date_1", "Injury_Date_2", "Injury_Date_3")

# Calculate in-zone contact metrics
inzone_contact_results <- calculate_inzone_contact(swing1, average_swing_speed, injury_cols)

# Join with `average_swing_speed`
average_swing_speed <- average_swing_speed %>%
  left_join(inzone_contact_results, by = "Name_Last_First")

# View the updated dataset
print(average_swing_speed)

