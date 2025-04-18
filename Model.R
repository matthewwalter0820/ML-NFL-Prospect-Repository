library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(nflreadr)
library(readr)
library(tidyr)
library(stringr)
library(scales)
library(purrr)
library(lubridate)
library(xgboost)

# Load your combined dataset (already merged)
all_data <- read.csv("all_data.csv")  # Adjust the path if necessary

df <- load_players()

all_data <- all_data %>%
  left_join(df %>% select(smart_id, birth_date), by = "smart_id")

all_data <- all_data %>%
  # Ensure birth_date is a Date
  mutate(birth_date = as.Date(birth_date)) %>%
  
  # Extract birth year and month-day
  mutate(
    birth_year = year(birth_date),
    birth_month_day = format(birth_date, "%m-%d")
  ) %>%
  
  # Compute nfl_age using the Year column (use backticks to reference it)
  mutate(
    nfl_age = if_else(
      birth_month_day <= "09-04", 
      as.numeric(`Year`) - birth_year, 
      as.numeric(`Year`) - birth_year - 1
    )
  )

all_data <- all_data %>%
  distinct(player_id, Year, Category, .keep_all = TRUE)


defense <- all_data %>%
  filter(!is.na(grades_defense))


defense <- defense %>%
  filter(snap_counts_defense >= 100)

defense <- defense %>%
  filter(!is.na(grades_defense), !is.na(snap_counts_defense)) %>%
  group_by(player_id) %>%
  mutate(grades_weighted = weighted.mean(grades_defense, snap_counts_defense, na.rm = TRUE)) %>%
  ungroup()


merged_df <- read.csv("C:/Users/jackd/OneDrive/Desktop/UC BANA/Capstone/College/Defense/Overall/Merged_Defense_Data.csv")


cvg <- read.csv("College/Defense/Coverage Scheme/Coverage Scheme All Years.csv")
passrush <- read.csv("College/Defense/Pass Rush/Pass Rush All Years.csv")
rundef <- read.csv("College/Defense/Run Defense/Run Defense All Years.csv")
slot <- read.csv("College/Defense/Slot/Slot All Years.csv")

slot <- slot %>%
  select(-c(player_game_count, receptions, targets, yards, qb_rating_against, 
            coverage_snaps_per_reception, team_name))


rundef <- rundef %>%
  select(-c(player_game_count, grades_coverage_defense, grades_defense_penalty, grades_defense,
            grades_pass_rush_defense, grades_run_defense, grades_tackle, missed_tackles, snap_counts_run,
            stops, declined_penalties, penalties, team_name))


passrush <- passrush %>%
  select(-c(player_game_count, grades_pass_rush_defense, pass_rush_wins, prp, snap_counts_pass_play,
            true_pass_set_prp, declined_penalties, true_pass_set_pass_rush_wins, 
            true_pass_set_grades_pass_rush_defense, true_pass_set_snap_counts_pass_rush,
            total_pressures, true_pass_set_total_pressures, snap_counts_pass_rush,
            true_pass_set_snap_counts_pass_play, penalties, team_name
  ))


cvg <- cvg %>%
  select(-c(player_game_count, base_snap_counts_coverage, declined_penalties, 
            man_forced_incompletes, man_grades_coverage_defense, man_missed_tackles, 
            man_assists, man_grades_coverage_defense, man_longest, man_missed_tackles,
            man_receptions, man_coverage_percent, man_snap_counts_pass_play, man_targets,
            man_yards, zone_assists, zone_coverage_percent, zone_forced_incompletes,
            zone_grades_coverage_defense, zone_missed_tackles, zone_longest, zone_receptions,
            zone_snap_counts_pass_play, zone_targets, zone_yards, penalties, team_name
  ))

merged_df <- merged_df %>%
  rename("year" = "Year")

merged_df <- merged_df %>%
  select(player_id, player, franchise_id, year, position, snap_counts_box, snap_counts_corner,
         snap_counts_dl_a_gap, snap_counts_dl_b_gap, snap_counts_dl_outside_t,
         snap_counts_dl_over_t, snap_counts_fs, snap_counts_offball, snap_counts_slot)


combined_defense <- cvg %>%
  full_join(passrush, by = c("player_id", "player", "franchise_id", "year", "position")) %>%
  full_join(rundef, by = c("player_id", "player", "franchise_id", "year", "position")) %>%
  full_join(slot, by = c("player_id", "player", "franchise_id", "year", "position")) %>%
  full_join(merged_df, by = c("player_id", "player", "franchise_id", "year", "position"))


rm(cvg, rundef, passrush, merged_df, slot)

player_info <- all_data %>%
  select(player_id, birth_date) %>%
  distinct()



combined_defense <- combined_defense %>%
  left_join(player_info, by = "player_id")



combined_defense <- combined_defense %>%
  # Ensure birth_date is a Date
  mutate(birth_date = as.Date(birth_date)) %>%
  
  # Extract birth year and month-day
  mutate(
    birth_year = year(birth_date),
    birth_month_day = format(birth_date, "%m-%d")
  ) %>%
  
  # Compute nfl_age using the Year column (use backticks to reference it)
  mutate(
    college_age = if_else(
      birth_month_day <= "09-04", 
      as.numeric(`year`) - birth_year, 
      as.numeric(`year`) - birth_year - 1
    )
  )


combined_defense <- combined_defense %>% 
  filter(player_id %in% defense$player_id)

defense <- defense %>% 
  filter(player_id %in% combined_defense$player_id)


combine <- load_combine(2017:2024)

# Clean names in combine data
combine <- combine %>%
  rename(player = player_name) %>%  # Rename for consistency
  mutate(
    player = tolower(player),                                   # Lowercase names
    player = str_replace_all(player, "[\\.']", ""),             # Remove periods and apostrophes
    player = str_replace_all(player, "\\s+jr|sr\\.?$", ""),     # Remove "Jr." or "Sr."
    player = str_replace_all(player, "\\s+(ii|iii|iv|v|vi|vii|viii|ix|x)$", ""),  # Remove Roman numerals
    player = str_squish(player)                                 # Trim and collapse whitespace
  )

combine <- combine %>%
  filter(!(player == "byron young" & school != "Tennessee"))



rosters <- load_rosters(2017:2024) %>%
  filter(!is.na(smart_id)) %>%
  distinct(smart_id, .keep_all = TRUE)



rosters <- rosters %>%
  rename(player = full_name)

# Step 2: Standardize player names
rosters <- rosters %>%
  mutate(
    player = tolower(player),                                      # Lowercase
    player = str_replace_all(player, "[\\.']", ""),                # Remove periods/apostrophes
    player = str_replace_all(player, "\\s+jr\\.?$", ""),           # Remove "Jr."
    player = str_replace_all(player, "\\s+sr\\.?$", ""),           # Remove "Sr."
    player = str_replace_all(player, "\\s+(ii|iii|iv|v|vi|vii|viii|ix|x)$", ""), # Remove Roman numerals
    player = str_squish(player)                                    # Remove extra whitespace
  )

library(dplyr)

# Step 1: Filter combine for rows that have non-missing pfr_id
combine_with_pfr <- combine %>%
  filter(!is.na(pfr_id)) %>%
  left_join(
    rosters %>% select(pfr_id, smart_id),
    by = "pfr_id"
  )

# Step 2: Keep the rest of combine (with NA pfr_id) as is
combine_missing_pfr <- combine %>%
  filter(is.na(pfr_id))

# Step 3: Bind both sets together
combine_updated <- bind_rows(combine_with_pfr, combine_missing_pfr)


# Step 1: Filter combine entries without smart_id
combine_missing_id <- combine_updated %>%
  filter(is.na(smart_id)) %>%
  select(-smart_id)  # remove placeholder columns for clean re-join

# Step 5: First fallback: Join by draft_number and rookie_year
fallback_by_draft <- combine_missing_id %>%
  filter(!is.na(draft_ovr)) %>%
  left_join(
    rosters %>% select(smart_id, draft_number, rookie_year),
    by = c("draft_ovr" = "draft_number", "season" = "rookie_year")
  )

# Step 6: Remove successful matches from fallback_by_draft before next step
still_missing <- combine_missing_id %>%
  filter(!(player %in% fallback_by_draft$player[fallback_by_draft$smart_id %>% is.na() == FALSE]))

# Step 7: Final fallback: Join by player and rookie_year
fallback_by_name <- still_missing %>%
  left_join(
    rosters %>% select(smart_id, player, rookie_year),
    by = c("player", "season" = "rookie_year")
  )

# Step 8: Combine everything into a final combine dataset
combine_final <- bind_rows(
  combine_with_pfr %>% filter(!is.na(smart_id)),  # already matched by pfr_id
  fallback_by_draft %>% filter(!is.na(smart_id)),
  fallback_by_name %>% filter(!is.na(smart_id))
)



duplicate_players <- combine_final %>%
  filter(!is.na(smart_id)) %>%
  count(smart_id) %>%
  filter(n > 1) %>%
  pull(smart_id)


# Step 2: Filter the full dataframe for those players
combine_duplicates <- combine_final %>%
  filter(smart_id %in% duplicate_players)


combine_cleaned <- combine_final %>%
  filter(!(smart_id %in% duplicate_players))


combine_cleaned <- combine_cleaned %>%
  separate(ht, into = c("feet", "inches"), sep = "-", convert = TRUE) %>%
  mutate(height = feet * 12 + inches) %>%
  select(-feet, -inches)

combine_cleaned <- combine_cleaned %>%
  mutate(
    broad_jump = as.character(broad_jump),
    broad_feet = ifelse(nchar(broad_jump) == 3,
                        as.numeric(substr(broad_jump, 1, 2)),
                        as.numeric(substr(broad_jump, 1, 1))),
    broad_inches = ifelse(nchar(broad_jump) == 3,
                          as.numeric(substr(broad_jump, 3, 3)),
                          as.numeric(substr(broad_jump, 2, 2))),
    broad_jump_inches = broad_feet * 12 + broad_inches
  ) %>%
  select(-broad_jump, -broad_feet, -broad_inches)  # Optional: drop intermediates

combine_cleaned <- combine_cleaned %>%
  select(height, wt, forty, bench, vertical, cone, shuttle, broad_jump_inches, smart_id)

defense <- defense %>%
  left_join(combine_cleaned, by = "smart_id")

defense <- defense %>%
  select(player_id, grades_defense, grades_weighted, Year, nfl_age, smart_id, 
         nfl_age, position, forty, bench, vertical, cone, shuttle, broad_jump_inches, snap_counts_defense,
         college_conference)

#cols_median <- c("forty", "bench", "vertical", "shuttle", "cone", "broad_jump_inches")
#defense <- defense %>%
 # group_by(position) %>%
  #mutate(across(all_of(cols_median), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
 # ungroup()


defense <- defense %>%
  left_join(
    rosters %>%
      select(smart_id, height, weight),
    by = "smart_id")

defense <- defense %>%
  select(-c(position, smart_id))

combined_defense <- combined_defense %>%
  select(-c(birth_date, birth_year, birth_month_day, franchise_id))


library(randomForest)

# Parameters: adjust these based on your calibration
k <- 500       # threshold snap count, e.g. average snaps for a robust sample
gamma <- 1     # scaling factor; adjust for a more/less aggressive weight


# Add the baseline_grade column and compute grades_weighted all at once
defense <- defense %>%
  group_by(player_id) %>%
  mutate(baseline_grade = mean(grades_defense, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(grades_weighted = baseline_grade +
           (grades_defense - baseline_grade) * ((snap_counts_defense / k)^gamma))


test <- combined_defense %>%
  full_join(defense, by = "player_id") %>%
  select(-c(player, snap_counts_defense, grades_weighted, year, Year, man_snap_counts_coverage_percent, zone_snap_counts_coverage_percent,
            pass_rush_percent, true_pass_set_pass_rush_percent, baseline_grade))


test <- test %>%
  filter(!is.na(nfl_age) & !is.na(college_age))

# Create a vector of snap count columns excluding "snap_counts_offball"
snap_cols <- names(test)[grepl("snap_counts", names(test)) & !grepl("snap_counts_offball", names(test))]

# Summarize snap counts by player using only the selected columns
snap_summary_by_player <- test %>%
  group_by(player_id) %>%
  summarise(
    across(all_of(snap_cols), ~ sum(.x, na.rm = TRUE), .names = "total_{.col}"),
    total_snaps = sum(c_across(all_of(snap_cols)), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Now, for each snap count variable, compute its proportion of the overall total
  mutate(
    across(starts_with("total_snap_counts"), ~ .x / total_snaps, .names = "prop_{.col}")
  )


snap_summary_by_player <- snap_summary_by_player %>%
  left_join(combined_defense %>% select(player_id, position), by = "player_id")

snap_summary_by_player <- snap_summary_by_player %>%
  filter(position %in% c("LB", "S", "CB"))


# Pivot the proportion columns into long format
majority_snap <- snap_summary_by_player %>%
  pivot_longer(
    cols = starts_with("prop_total_"),
    names_to = "snap_type",
    values_to = "prop_value"
  ) %>%
  group_by(player_id) %>%
  summarise(majority_snap = snap_type[which.max(prop_value)]) %>%
  ungroup()

# Now join this back to your original summary data
snap_summary_by_player <- snap_summary_by_player %>%
  left_join(majority_snap, by = "player_id")

group_counts <- snap_summary_by_player %>%
  group_by(majority_snap) %>%
  count(name = "n")

sec_lb <- test %>%
  filter(position %in% c("LB", "S", "CB"))

sec_lb_with_majority <- sec_lb %>%
  left_join(majority_snap, by = "player_id") %>%
  filter(majority_snap %in% c("prop_total_snap_counts_box", 
                              "prop_total_snap_counts_slot",
                              "prop_total_snap_counts_dl_outside_t",
                              "prop_total_snap_counts_fs", 
                              "prop_total_snap_counts_corner"))


# Assuming your data frame after the join is called sec_lb_with_majority
cb_df   <- sec_lb_with_majority %>% filter(majority_snap == "prop_total_snap_counts_corner")
slot_df <- sec_lb_with_majority %>% filter(majority_snap %in% c("prop_total_snap_counts_slot",
                                                                "prop_total_snap_counts_dl_outside_t"))
box_df  <- sec_lb_with_majority %>% filter(majority_snap == "prop_total_snap_counts_box")
fs_df   <- sec_lb_with_majority %>% filter(majority_snap == "prop_total_snap_counts_fs")

cb_df <- cb_df %>%
  select(-c(position, majority_snap))
slot_df <- slot_df %>%
  select(-c(position, majority_snap))
fs_df <- fs_df %>%
  select(-c(position, majority_snap))
box_df <- box_df %>%
  select(-c(position, majority_snap))

# Defensive Tackles -------------------------------------------------------


di <- test %>%
  filter(position == "DI") %>%
  select(-c(position, snap_counts_box, snap_counts_corner, snap_counts_fs, snap_counts_offball,
         snap_counts_slot))


franchise_summary <- di %>% 
  group_by(college_conference) %>% 
  summarise(
    count_entries = n(),
    count_unique_players = n_distinct(player_id))

di <- di %>%
  select(-player_id)
threshold <- 0.4

set.seed(42)


# Define the combine measurement columns
combine_cols <- c("forty", "bench", "vertical", "cone", "shuttle", "broad_jump_inches")

# Separate the data into combine columns and all other columns
non_combine <- di %>% select(-all_of(combine_cols))
combine_data <- di %>% select(all_of(combine_cols))

# For the combine columns, keep only those columns where the proportion of NAs is below the threshold
non_combine_filtered <- non_combine %>% select_if(~ mean(is.na(.)) <= threshold)

# Bind the non-combine columns (kept entirely) with the filtered combine columns
di_filtered <- bind_cols(combine_data, non_combine_filtered)


# Split the data into training and test sets
train_idx <- sample(seq_len(nrow(di_filtered)), size = 0.8 * nrow(di_filtered))
train_data <- di_filtered[train_idx, ]
test_data  <- di_filtered[-train_idx, ]

# Ensure global option is set so that NA rows are not dropped
options(na.action = "na.pass")

### Create design matrix and response vector for the training set
X_train <- model.matrix(grades_defense ~ ., data = train_data, na.action = na.pass)[, -1]  # drop intercept column
y_train <- train_data$grades_defense

cat("Training data: ", nrow(train_data), "rows\n")
cat("Rows in X_train:", nrow(X_train), "\n")
cat("Length of y_train:", length(y_train), "\n")

# (Optional) Check for rows with all predictors missing in train_data
predictor_cols <- setdiff(names(train_data), "grades_defense")
rows_all_missing <- which(apply(train_data[, predictor_cols], 1, function(x) all(is.na(x))))
if(length(rows_all_missing) > 0){
  cat("Rows with all predictors missing in train_data:", rows_all_missing, "\n")
}

if(nrow(X_train) != length(y_train)){
  stop("Mismatch in number of rows between X_train and y_train. Check your training data.")
}

# Create the DMatrix for training
dtrain <- xgb.DMatrix(data = X_train, label = y_train, missing = NA)


### Create design matrix and response vector for the test set
X_test <- model.matrix(grades_defense ~ ., data = test_data, na.action = na.pass)[, -1]
y_test <- test_data$grades_defense

cat("Test data: ", nrow(test_data), "rows\n")
cat("Rows in X_test:", nrow(X_test), "\n")
cat("Length of y_test:", length(y_test), "\n")

# (Optional) Check for rows with all predictors missing in test_data
predictor_cols_test <- setdiff(names(test_data), "grades_defense")
rows_all_missing_test <- which(apply(test_data[, predictor_cols_test], 1, function(x) all(is.na(x))))
if(length(rows_all_missing_test) > 0){
  cat("Rows with all predictors missing in test_data:", rows_all_missing_test, "\n")
}

if(nrow(X_test) != length(y_test)){
  stop("Mismatch in number of rows between X_test and y_test. Check your test data.")
}

# Create the DMatrix for testing
dtest <- xgb.DMatrix(data = X_test, label = y_test, missing = NA)

### Define xgboost parameters (example settings)
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 6
)

### Train model, using the test set for early stopping
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  watchlist = list(train = dtrain, test = dtest),
  nrounds = 1000,
  early_stopping_rounds = 50,
  verbose = 1
)

### Predict on the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

### Compute performance metrics on the test set
mse_test <- mean((y_test - xgb_pred)^2)
r2_test <- 1 - sum((y_test - xgb_pred)^2) / sum((y_test - mean(y_test))^2)
cat("Test MSE:", mse_test, "\n")
cat("Test R²:", r2_test, "\n")
cat("Test % Var Explained:", r2_test * 100, "\n")

# 7. (Optional) Plot predicted vs. actual for the test set
plot(y_test, xgb_pred,
     xlab = "Actual Grades (Test)",
     ylab = "Predicted Grades (Test)",
     main = "DI XGBoost: Predicted vs Actual (Test)",
     pch = 16, col = "blue")

abline(a = 0, b = 1, col = "red", lwd = 2)


# Assuming you have:
# X_test: a matrix (or data frame) of predictors for the test set
# y_test: the actual response values (grades_defense) for the test set
# xgb_model: your trained XGBoost model

# 1. Generate predictions for the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

# 2. Calculate the residuals (actual - predicted)
xgb_resid <- y_test - xgb_pred

# 3. Compute the Mean Absolute Error (MAE)
mae <- mean(abs(xgb_resid))
cat("XGBoost MAE:", mae, "\n")

# 4. Create a residual plot
plot(xgb_pred, xgb_resid,
     main = "Cornerbacks XGBoost Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals",
     pch = 16,         # Solid circle markers
     col = "blue")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at 0 for reference



# 1) Compute the importance matrix
importance_matrix <- xgb.importance(model = xgb_model)

# 2) Print it in tabular form
print(importance_matrix)

# 3) Plot a bar chart of feature importance
xgb.plot.importance(importance_matrix,
                    main = "XGBoost Feature Importance",
                    top_n = 30)


imp <- xgb.importance(model = xgb_model)

# Suppose the dummy columns for franchise_id are named like "franchise_id_123"
# We can group them under the same base name "franchise_id" by removing the numeric part.
# Adjust the pattern if your naming scheme is different.
# Create a new column "base_var" that groups all features starting with "franchise_id" together.
imp_grouped <- imp %>%
  mutate(base_var = if_else(str_detect(Feature, "^college_conference"),
                            "college_conference",
                            Feature)) %>%
  group_by(base_var) %>%
  summarise(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Gain))

top30 <- imp_grouped %>% slice_head(n = 30)

# Optionally, create a bar plot of the top 30 features
ggplot(top30, aes(x = reorder(base_var, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "DI Top 30 Aggregated Feature Importance", 
       x = "Feature", 
       y = "Total Gain") +
  theme_minimal()





# Edges -------------------------------------------------------------------


ed <- test %>%
  filter(position == "ED") %>%
  select(-position)

franchise_summary <- ed %>% 
  group_by(college_conference) %>% 
  summarise(
    count_entries = n(),
    count_unique_players = n_distinct(player_id))

ed <- ed %>%
  group_by(college_conference) %>%
  filter(n() > 20) %>%
  ungroup()

ed <- ed %>%
  select(-player_id)


# Define the combine measurement columns
combine_cols <- c("forty", "bench", "vertical", "cone", "shuttle", "broad_jump_inches")

# Separate the data into combine columns and all other columns
non_combine <- ed %>% select(-all_of(combine_cols))
combine_data <- ed %>% select(all_of(combine_cols))

# For the combine columns, keep only those columns where the proportion of NAs is below the threshold
non_combine_filtered <- non_combine %>% select_if(~ mean(is.na(.)) <= threshold)

# Bind the non-combine columns (kept entirely) with the filtered combine columns
ed_filtered <- bind_cols(combine_data, non_combine_filtered)

# Split the data into training and test sets
train_idx <- sample(seq_len(nrow(ed_filtered)), size = 0.8 * nrow(ed_filtered))
train_data <- ed_filtered[train_idx, ]
test_data  <- ed_filtered[-train_idx, ]

# Ensure global option is set so that NA rows are not dropped
options(na.action = "na.pass")

### Create design matrix and response vector for the training set
X_train <- model.matrix(grades_defense ~ ., data = train_data, na.action = na.pass)[, -1]  # drop intercept column
y_train <- train_data$grades_defense

cat("Training data: ", nrow(train_data), "rows\n")
cat("Rows in X_train:", nrow(X_train), "\n")
cat("Length of y_train:", length(y_train), "\n")

# (Optional) Check for rows with all predictors missing in train_data
predictor_cols <- setdiff(names(train_data), "grades_defense")
rows_all_missing <- which(apply(train_data[, predictor_cols], 1, function(x) all(is.na(x))))
if(length(rows_all_missing) > 0){
  cat("Rows with all predictors missing in train_data:", rows_all_missing, "\n")
}

if(nrow(X_train) != length(y_train)){
  stop("Mismatch in number of rows between X_train and y_train. Check your training data.")
}

# Create the DMatrix for training
dtrain <- xgb.DMatrix(data = X_train, label = y_train, missing = NA)


### Create design matrix and response vector for the test set
X_test <- model.matrix(grades_defense ~ ., data = test_data, na.action = na.pass)[, -1]
y_test <- test_data$grades_defense

cat("Test data: ", nrow(test_data), "rows\n")
cat("Rows in X_test:", nrow(X_test), "\n")
cat("Length of y_test:", length(y_test), "\n")

# (Optional) Check for rows with all predictors missing in test_data
predictor_cols_test <- setdiff(names(test_data), "grades_defense")
rows_all_missing_test <- which(apply(test_data[, predictor_cols_test], 1, function(x) all(is.na(x))))
if(length(rows_all_missing_test) > 0){
  cat("Rows with all predictors missing in test_data:", rows_all_missing_test, "\n")
}

if(nrow(X_test) != length(y_test)){
  stop("Mismatch in number of rows between X_test and y_test. Check your test data.")
}

# Create the DMatrix for testing
dtest <- xgb.DMatrix(data = X_test, label = y_test, missing = NA)

### Define xgboost parameters (example settings)
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 7
)

### Train model, using the test set for early stopping
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  watchlist = list(train = dtrain, test = dtest),
  nrounds = 1000,
  early_stopping_rounds = 50,
  verbose = 1
)

### Predict on the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

### Compute performance metrics on the test set
mse_test <- mean((y_test - xgb_pred)^2)
r2_test <- 1 - sum((y_test - xgb_pred)^2) / sum((y_test - mean(y_test))^2)
cat("Test MSE:", mse_test, "\n")
cat("Test R²:", r2_test, "\n")
cat("Test % Var Explained:", r2_test * 100, "\n")

# 7. (Optional) Plot predicted vs. actual for the test set
plot(y_test, xgb_pred,
     xlab = "Actual Grades (Test)",
     ylab = "Predicted Grades (Test)",
     main = "ED XGBoost: Predicted vs Actual (Test)",
     pch = 16, col = "blue")

abline(a = 0, b = 1, col = "red", lwd = 2)


# Assuming you have:
# X_test: a matrix (or data frame) of predictors for the test set
# y_test: the actual response values (grades_defense) for the test set
# xgb_model: your trained XGBoost model

# 1. Generate predictions for the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

# 2. Calculate the residuals (actual - predicted)
xgb_resid <- y_test - xgb_pred

# 3. Compute the Mean Absolute Error (MAE)
mae <- mean(abs(xgb_resid))
cat("XGBoost MAE:", mae, "\n")

# 4. Create a residual plot
plot(xgb_pred, xgb_resid,
     main = "ED XGBoost Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals",
     pch = 16,         # Solid circle markers
     col = "blue")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at 0 for reference



# 1) Compute the importance matrix
importance_matrix <- xgb.importance(model = xgb_model)

# 2) Print it in tabular form
print(importance_matrix)

# 3) Plot a bar chart of feature importance
xgb.plot.importance(importance_matrix,
                    main = "XGBoost Feature Importance",
                    top_n = 30)


imp <- xgb.importance(model = xgb_model)

# Suppose the dummy columns for franchise_id are named like "franchise_id_123"
# We can group them under the same base name "franchise_id" by removing the numeric part.
# Adjust the pattern if your naming scheme is different.
# Create a new column "base_var" that groups all features starting with "franchise_id" together.
imp_grouped <- imp %>%
  mutate(base_var = if_else(str_detect(Feature, "^college_conference"),
                            "college_conference",
                            Feature)) %>%
  group_by(base_var) %>%
  summarise(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Gain))

top30 <- imp_grouped %>% slice_head(n = 30)

# Optionally, create a bar plot of the top 30 features
ggplot(top30, aes(x = reorder(base_var, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "ED Top 30 Aggregated Feature Importance", 
       x = "Feature", 
       y = "Total Gain") +
  theme_minimal()




# Test Segmented Secondary and LBs ----------------------------------------



# CB ----------------------------------------------------------------------


franchise_summary <- cb_df %>% 
  group_by(college_conference) %>% 
  summarise(
    count_entries = n(),
    count_unique_players = n_distinct(player_id))

cb_df <- cb_df %>%
  group_by(college_conference) %>%
  filter(n() >= 5) %>%
  ungroup()


cb_df <- cb_df %>%
  select(-player_id)
# Split the data into training and test sets
train_idx <- sample(seq_len(nrow(cb_df)), size = 0.8 * nrow(cb_df))
train_data <- cb_df[train_idx, ]
test_data  <- cb_df[-train_idx, ]

# Ensure global option is set so that NA rows are not dropped
options(na.action = "na.pass")

### Create design matrix and response vector for the training set
X_train <- model.matrix(grades_defense ~ ., data = train_data, na.action = na.pass)[, -1]  # drop intercept column
y_train <- train_data$grades_defense

cat("Training data: ", nrow(train_data), "rows\n")
cat("Rows in X_train:", nrow(X_train), "\n")
cat("Length of y_train:", length(y_train), "\n")

# (Optional) Check for rows with all predictors missing in train_data
predictor_cols <- setdiff(names(train_data), "grades_defense")
rows_all_missing <- which(apply(train_data[, predictor_cols], 1, function(x) all(is.na(x))))
if(length(rows_all_missing) > 0){
  cat("Rows with all predictors missing in train_data:", rows_all_missing, "\n")
}

if(nrow(X_train) != length(y_train)){
  stop("Mismatch in number of rows between X_train and y_train. Check your training data.")
}

# Create the DMatrix for training
dtrain <- xgb.DMatrix(data = X_train, label = y_train, missing = NA)


### Create design matrix and response vector for the test set
X_test <- model.matrix(grades_defense ~ ., data = test_data, na.action = na.pass)[, -1]
y_test <- test_data$grades_defense

cat("Test data: ", nrow(test_data), "rows\n")
cat("Rows in X_test:", nrow(X_test), "\n")
cat("Length of y_test:", length(y_test), "\n")

# (Optional) Check for rows with all predictors missing in test_data
predictor_cols_test <- setdiff(names(test_data), "grades_defense")
rows_all_missing_test <- which(apply(test_data[, predictor_cols_test], 1, function(x) all(is.na(x))))
if(length(rows_all_missing_test) > 0){
  cat("Rows with all predictors missing in test_data:", rows_all_missing_test, "\n")
}

if(nrow(X_test) != length(y_test)){
  stop("Mismatch in number of rows between X_test and y_test. Check your test data.")
}

# Create the DMatrix for testing
dtest <- xgb.DMatrix(data = X_test, label = y_test, missing = NA)


### Define xgboost parameters (example settings)
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.08,
  max_depth = 6
)

### Train model, using the test set for early stopping
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  watchlist = list(train = dtrain, test = dtest),
  nrounds = 1000,
  early_stopping_rounds = 50,
  verbose = 1
)

### Predict on the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

### Compute performance metrics on the test set
mse_test <- mean((y_test - xgb_pred)^2)
r2_test <- 1 - sum((y_test - xgb_pred)^2) / sum((y_test - mean(y_test))^2)
cat("Test MSE:", mse_test, "\n")
cat("Test R²:", r2_test, "\n")
cat("Test % Var Explained:", r2_test * 100, "\n")

# 7. (Optional) Plot predicted vs. actual for the test set
plot(y_test, xgb_pred,
     xlab = "Actual Grades (Test)",
     ylab = "Predicted Grades (Test)",
     main = "Cornerbacks XGBoost: Predicted vs Actual (Test)",
     pch = 16, col = "blue")

abline(a = 0, b = 1, col = "red", lwd = 2)


# Assuming you have:
# X_test: a matrix (or data frame) of predictors for the test set
# y_test: the actual response values (grades_defense) for the test set
# xgb_model: your trained XGBoost model

# 1. Generate predictions for the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

# 2. Calculate the residuals (actual - predicted)
xgb_resid <- y_test - xgb_pred

# 3. Compute the Mean Absolute Error (MAE)
mae <- mean(abs(xgb_resid))
cat("XGBoost MAE:", mae, "\n")

# 4. Create a residual plot
plot(xgb_pred, xgb_resid,
     main = "Cornerbacks XGBoost Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals",
     pch = 16,         # Solid circle markers
     col = "blue")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at 0 for reference



# 1) Compute the importance matrix
importance_matrix <- xgb.importance(model = xgb_model)

# 2) Print it in tabular form
print(importance_matrix)

# 3) Plot a bar chart of feature importance
xgb.plot.importance(importance_matrix,
                    main = "XGBoost Feature Importance",
                    top_n = 30)


imp <- xgb.importance(model = xgb_model)

# Suppose the dummy columns for franchise_id are named like "franchise_id_123"
# We can group them under the same base name "franchise_id" by removing the numeric part.
# Adjust the pattern if your naming scheme is different.
# Create a new column "base_var" that groups all features starting with "franchise_id" together.
imp_grouped <- imp %>%
  mutate(base_var = if_else(str_detect(Feature, "^college_conference"),
                            "college_conference",
                            Feature)) %>%
  group_by(base_var) %>%
  summarise(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Gain))

top30 <- imp_grouped %>% slice_head(n = 30)

# Optionally, create a bar plot of the top 30 features
ggplot(top30, aes(x = reorder(base_var, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Cornerbacks Top 30 Aggregated Feature Importance", 
       x = "Feature", 
       y = "Total Gain") +
  theme_minimal()







# Box ---------------------------------------------------------------------


# 1. Split into train and test sets
set.seed(42)



franchise_summary <- box_df %>% 
  group_by(college_conference) %>% 
  summarise(
    count_entries = n(),
    count_unique_players = n_distinct(player_id))

box_df <- box_df %>%
  group_by(college_conference) %>%
  filter(n() > 15) %>%
  ungroup()


box_df <- box_df %>%
  select(-player_id)


# Split the data into training and test sets
train_idx <- sample(seq_len(nrow(box_df)), size = 0.8 * nrow(box_df))
train_data <- box_df[train_idx, ]
test_data  <- box_df[-train_idx, ]

# Ensure global option is set so that NA rows are not dropped
options(na.action = "na.pass")

### Create design matrix and response vector for the training set
X_train <- model.matrix(grades_defense ~ ., data = train_data, na.action = na.pass)[, -1]  # drop intercept column
y_train <- train_data$grades_defense

cat("Training data: ", nrow(train_data), "rows\n")
cat("Rows in X_train:", nrow(X_train), "\n")
cat("Length of y_train:", length(y_train), "\n")

# (Optional) Check for rows with all predictors missing in train_data
predictor_cols <- setdiff(names(train_data), "grades_defense")
rows_all_missing <- which(apply(train_data[, predictor_cols], 1, function(x) all(is.na(x))))
if(length(rows_all_missing) > 0){
  cat("Rows with all predictors missing in train_data:", rows_all_missing, "\n")
}

if(nrow(X_train) != length(y_train)){
  stop("Mismatch in number of rows between X_train and y_train. Check your training data.")
}

# Create the DMatrix for training
dtrain <- xgb.DMatrix(data = X_train, label = y_train, missing = NA)


### Create design matrix and response vector for the test set
X_test <- model.matrix(grades_defense ~ ., data = test_data, na.action = na.pass)[, -1]
y_test <- test_data$grades_defense

cat("Test data: ", nrow(test_data), "rows\n")
cat("Rows in X_test:", nrow(X_test), "\n")
cat("Length of y_test:", length(y_test), "\n")

# (Optional) Check for rows with all predictors missing in test_data
predictor_cols_test <- setdiff(names(test_data), "grades_defense")
rows_all_missing_test <- which(apply(test_data[, predictor_cols_test], 1, function(x) all(is.na(x))))
if(length(rows_all_missing_test) > 0){
  cat("Rows with all predictors missing in test_data:", rows_all_missing_test, "\n")
}

if(nrow(X_test) != length(y_test)){
  stop("Mismatch in number of rows between X_test and y_test. Check your test data.")
}

# Create the DMatrix for testing
dtest <- xgb.DMatrix(data = X_test, label = y_test, missing = NA)

### Define xgboost parameters (example settings)
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.05,
  max_depth = 7
)

### Train model, using the test set for early stopping
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  watchlist = list(train = dtrain, test = dtest),
  nrounds = 1000,
  early_stopping_rounds = 50,
  verbose = 1
)

### Predict on the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

### Compute performance metrics on the test set
mse_test <- mean((y_test - xgb_pred)^2)
r2_test <- 1 - sum((y_test - xgb_pred)^2) / sum((y_test - mean(y_test))^2)
cat("Test MSE:", mse_test, "\n")
cat("Test R²:", r2_test, "\n")
cat("Test % Var Explained:", r2_test * 100, "\n")

# 7. (Optional) Plot predicted vs. actual for the test set
plot(y_test, xgb_pred,
     xlab = "Box Defenders Actual Grades (Test)",
     ylab = "Predicted Grades (Test)",
     main = "XGBoost: Predicted vs Actual (Test)",
     pch = 16, col = "blue")

abline(a = 0, b = 1, col = "red", lwd = 2)


# Assuming you have:
# X_test: a matrix (or data frame) of predictors for the test set
# y_test: the actual response values (grades_defense) for the test set
# xgb_model: your trained XGBoost model

# 1. Generate predictions for the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

# 2. Calculate the residuals (actual - predicted)
xgb_resid <- y_test - xgb_pred

# 3. Compute the Mean Absolute Error (MAE)
mae <- mean(abs(xgb_resid))
cat("XGBoost MAE:", mae, "\n")

# 4. Create a residual plot
plot(xgb_pred, xgb_resid,
     main = "Box Defenders XGBoost Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals",
     pch = 16,         # Solid circle markers
     col = "blue")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at 0 for reference



# 1) Compute the importance matrix
importance_matrix <- xgb.importance(model = xgb_model)

# 2) Print it in tabular form
print(importance_matrix)

# 3) Plot a bar chart of feature importance
xgb.plot.importance(importance_matrix,
                    main = "XGBoost Feature Importance",
                    top_n = 30)


imp <- xgb.importance(model = xgb_model)

# Suppose the dummy columns for franchise_id are named like "franchise_id_123"
# We can group them under the same base name "franchise_id" by removing the numeric part.
# Adjust the pattern if your naming scheme is different.
# Create a new column "base_var" that groups all features starting with "franchise_id" together.
imp_grouped <- imp %>%
  mutate(base_var = if_else(str_detect(Feature, "^college_conference"),
                            "college_conference",
                            Feature)) %>%
  group_by(base_var) %>%
  summarise(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Gain))

top30 <- imp_grouped %>% slice_head(n = 30)

# Optionally, create a bar plot of the top 30 features
ggplot(top30, aes(x = reorder(base_var, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Box Defenders Top 30 Aggregated Feature Importance", 
       x = "Feature", 
       y = "Total Gain") +
  theme_minimal()





# Slot --------------------------------------------------------------------


# 1. Split into train and test sets
set.seed(42)


franchise_summary <- slot_df %>% 
  group_by(college_conference) %>% 
  summarise(
    count_entries = n(),
    count_unique_players = n_distinct(player_id))

slot_df <- slot_df %>%
  group_by(college_conference) %>%
  filter(n() >= 20) %>%
  ungroup()


slot_df <- slot_df %>%
  select(-player_id)
# Split the data into training and test sets
train_idx <- sample(seq_len(nrow(slot_df)), size = 0.8 * nrow(slot_df))
train_data <- slot_df[train_idx, ]
test_data  <- slot_df[-train_idx, ]

# Ensure global option is set so that NA rows are not dropped
options(na.action = "na.pass")

### Create design matrix and response vector for the training set
X_train <- model.matrix(grades_defense ~ ., data = train_data, na.action = na.pass)[, -1]  # drop intercept column
y_train <- train_data$grades_defense

cat("Training data: ", nrow(train_data), "rows\n")
cat("Rows in X_train:", nrow(X_train), "\n")
cat("Length of y_train:", length(y_train), "\n")

# (Optional) Check for rows with all predictors missing in train_data
predictor_cols <- setdiff(names(train_data), "grades_defense")
rows_all_missing <- which(apply(train_data[, predictor_cols], 1, function(x) all(is.na(x))))
if(length(rows_all_missing) > 0){
  cat("Rows with all predictors missing in train_data:", rows_all_missing, "\n")
}

if(nrow(X_train) != length(y_train)){
  stop("Mismatch in number of rows between X_train and y_train. Check your training data.")
}

# Create the DMatrix for training
dtrain <- xgb.DMatrix(data = X_train, label = y_train, missing = NA)


### Create design matrix and response vector for the test set
X_test <- model.matrix(grades_defense ~ ., data = test_data, na.action = na.pass)[, -1]
y_test <- test_data$grades_defense

cat("Test data: ", nrow(test_data), "rows\n")
cat("Rows in X_test:", nrow(X_test), "\n")
cat("Length of y_test:", length(y_test), "\n")

# (Optional) Check for rows with all predictors missing in test_data
predictor_cols_test <- setdiff(names(test_data), "grades_defense")
rows_all_missing_test <- which(apply(test_data[, predictor_cols_test], 1, function(x) all(is.na(x))))
if(length(rows_all_missing_test) > 0){
  cat("Rows with all predictors missing in test_data:", rows_all_missing_test, "\n")
}

if(nrow(X_test) != length(y_test)){
  stop("Mismatch in number of rows between X_test and y_test. Check your test data.")
}

# Create the DMatrix for testing
dtest <- xgb.DMatrix(data = X_test, label = y_test, missing = NA)

# 3. Define xgboost parameters (example settings)
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 10
)

# 4. Train model, possibly with early stopping using the test set
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  watchlist = list(train = dtrain, test = dtest),
  nrounds = 1000,
  early_stopping_rounds = 10,
  verbose = 1
)

# 5. Predict on the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

# 6. Compute performance metrics on the test set
mse_test <- mean((y_test - xgb_pred)^2)
r2_test <- 1 - sum((y_test - xgb_pred)^2) / sum((y_test - mean(y_test))^2)
cat("Test MSE:", mse_test, "\n")
cat("Test R²:", r2_test, "\n")
cat("Test % Var Explained:", r2_test * 100, "\n")

# 7. (Optional) Plot predicted vs. actual for the test set
plot(y_test, xgb_pred,
     xlab = "Actual Grades (Test)",
     ylab = "Predicted Grades (Test)",
     main = "Slot Defenders XGBoost: Predicted vs Actual (Test)",
     pch = 16, col = "blue")

abline(a = 0, b = 1, col = "red", lwd = 2)


# Assuming you have:
# X_test: a matrix (or data frame) of predictors for the test set
# y_test: the actual response values (grades_defense) for the test set
# xgb_model: your trained XGBoost model

# 1. Generate predictions for the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

# 2. Calculate the residuals (actual - predicted)
xgb_resid <- y_test - xgb_pred

# 3. Compute the Mean Absolute Error (MAE)
mae <- mean(abs(xgb_resid))
cat("XGBoost MAE:", mae, "\n")

# 4. Create a residual plot
plot(xgb_pred, xgb_resid,
     main = "Slot Defenders XGBoost Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals",
     pch = 16,         # Solid circle markers
     col = "blue")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at 0 for reference


# 1) Compute the importance matrix
importance_matrix <- xgb.importance(model = xgb_model)

# 2) Print it in tabular form
print(importance_matrix)

# 3) Plot a bar chart of feature importance
xgb.plot.importance(importance_matrix,
                    main = "XGBoost Feature Importance")

imp <- xgb.importance(model = xgb_model)

# Suppose the dummy columns for franchise_id are named like "franchise_id_123"
# We can group them under the same base name "franchise_id" by removing the numeric part.
# Adjust the pattern if your naming scheme is different.
# Create a new column "base_var" that groups all features starting with "franchise_id" together.
imp_grouped <- imp %>%
  mutate(base_var = if_else(str_detect(Feature, "^college_conference"),
                            "college_conference",
                            Feature)) %>%
  group_by(base_var) %>%
  summarise(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Gain))

top30 <- imp_grouped %>% slice_head(n = 30)

# Optionally, create a bar plot of the top 30 features
ggplot(top30, aes(x = reorder(base_var, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Slot Defenders Top 30 Aggregated Feature Importance", 
       x = "Feature", 
       y = "Total Gain") +
  theme_minimal()


# FS ----------------------------------------------------------------------

# 1. Split into train and test sets
set.seed(42)


franchise_summary <- fs_df %>% 
  group_by(college_conference) %>% 
  summarise(
    count_entries = n(),
    count_unique_players = n_distinct(player_id))

fs_df <- fs_df %>%
  group_by(college_conference) %>%
  filter(n() > 5) %>%
  ungroup()


fs_df <- fs_df %>%
  select(-player_id)
# Split the data into training and test sets
train_idx <- sample(seq_len(nrow(fs_df)), size = 0.8 * nrow(fs_df))
train_data <- fs_df[train_idx, ]
test_data  <- fs_df[-train_idx, ]

# Ensure global option is set so that NA rows are not dropped
options(na.action = "na.pass")

### Create design matrix and response vector for the training set
X_train <- model.matrix(grades_defense ~ ., data = train_data, na.action = na.pass)[, -1]  # drop intercept column
y_train <- train_data$grades_defense

cat("Training data: ", nrow(train_data), "rows\n")
cat("Rows in X_train:", nrow(X_train), "\n")
cat("Length of y_train:", length(y_train), "\n")

# (Optional) Check for rows with all predictors missing in train_data
predictor_cols <- setdiff(names(train_data), "grades_defense")
rows_all_missing <- which(apply(train_data[, predictor_cols], 1, function(x) all(is.na(x))))
if(length(rows_all_missing) > 0){
  cat("Rows with all predictors missing in train_data:", rows_all_missing, "\n")
}

if(nrow(X_train) != length(y_train)){
  stop("Mismatch in number of rows between X_train and y_train. Check your training data.")
}

# Create the DMatrix for training
dtrain <- xgb.DMatrix(data = X_train, label = y_train, missing = NA)


### Create design matrix and response vector for the test set
X_test <- model.matrix(grades_defense ~ ., data = test_data, na.action = na.pass)[, -1]
y_test <- test_data$grades_defense

cat("Test data: ", nrow(test_data), "rows\n")
cat("Rows in X_test:", nrow(X_test), "\n")
cat("Length of y_test:", length(y_test), "\n")

# (Optional) Check for rows with all predictors missing in test_data
predictor_cols_test <- setdiff(names(test_data), "grades_defense")
rows_all_missing_test <- which(apply(test_data[, predictor_cols_test], 1, function(x) all(is.na(x))))
if(length(rows_all_missing_test) > 0){
  cat("Rows with all predictors missing in test_data:", rows_all_missing_test, "\n")
}

if(nrow(X_test) != length(y_test)){
  stop("Mismatch in number of rows between X_test and y_test. Check your test data.")
}

# Create the DMatrix for testing
dtest <- xgb.DMatrix(data = X_test, label = y_test, missing = NA)

### Define xgboost parameters (example settings)
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 6
)

### Train model, using the test set for early stopping
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  watchlist = list(train = dtrain, test = dtest),
  nrounds = 1000,
  early_stopping_rounds = 50,
  verbose = 1
)

### Predict on the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

### Compute performance metrics on the test set
mse_test <- mean((y_test - xgb_pred)^2)
r2_test <- 1 - sum((y_test - xgb_pred)^2) / sum((y_test - mean(y_test))^2)
cat("Test MSE:", mse_test, "\n")
cat("Test R²:", r2_test, "\n")
cat("Test % Var Explained:", r2_test * 100, "\n")

# 7. (Optional) Plot predicted vs. actual for the test set
plot(y_test, xgb_pred,
     xlab = "Actual Grades (Test)",
     ylab = "Predicted Grades (Test)",
     main = "Free Safety XGBoost: Predicted vs Actual (Test)",
     pch = 16, col = "blue")

abline(a = 0, b = 1, col = "red", lwd = 2)


# Assuming you have:
# X_test: a matrix (or data frame) of predictors for the test set
# y_test: the actual response values (grades_defense) for the test set
# xgb_model: your trained XGBoost model

# 1. Generate predictions for the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

# 2. Calculate the residuals (actual - predicted)
xgb_resid <- y_test - xgb_pred

# 3. Compute the Mean Absolute Error (MAE)
mae <- mean(abs(xgb_resid))
cat("XGBoost MAE:", mae, "\n")

# 4. Create a residual plot
plot(xgb_pred, xgb_resid,
     main = "Free Safety XGBoost Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals",
     pch = 16,         # Solid circle markers
     col = "blue")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at 0 for reference



# 1) Compute the importance matrix
importance_matrix <- xgb.importance(model = xgb_model)

# 2) Print it in tabular form
print(importance_matrix)

# 3) Plot a bar chart of feature importance
xgb.plot.importance(importance_matrix,
                    main = "XGBoost Feature Importance",
                    top_n = 30)



imp <- xgb.importance(model = xgb_model)

# Suppose the dummy columns for franchise_id are named like "franchise_id_123"
# We can group them under the same base name "franchise_id" by removing the numeric part.
# Adjust the pattern if your naming scheme is different.
# Create a new column "base_var" that groups all features starting with "franchise_id" together.
imp_grouped <- imp %>%
  mutate(base_var = if_else(str_detect(Feature, "^college_conference"),
                            "college_conference",
                            Feature)) %>%
  group_by(base_var) %>%
  summarise(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Gain))

top30 <- imp_grouped %>% slice_head(n = 30)

# Optionally, create a bar plot of the top 30 features
ggplot(top30, aes(x = reorder(base_var, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Free Safety Top 30 Aggregated Feature Importance", 
       x = "Feature", 
       y = "Total Gain") +
  theme_minimal()






# Predict Defense ---------------------------------------------------------


merged_2024 <- read.csv("C:/Users/jackd/OneDrive/Desktop/UC BANA/Capstone/College/Defense/2024/Defense Ovr 2024.csv")


cvg_2024 <- read.csv("C:/Users/jackd/OneDrive/Desktop/UC BANA/Capstone/College/Defense/2024/Coverage Scheme 2024.csv")
passrush_2024 <- read.csv("C:/Users/jackd/OneDrive/Desktop/UC BANA/Capstone/College/Defense/2024/Pass Rush 2024.csv")
rundef_2024 <- read.csv("C:/Users/jackd/OneDrive/Desktop/UC BANA/Capstone/College/Defense/2024/Run Defense 2024.csv")
slot_2024 <- read.csv("C:/Users/jackd/OneDrive/Desktop/UC BANA/Capstone/College/Defense/2024/Slot 2024.csv")

slot_2024 <- slot_2024 %>%
  select(-c(player_game_count, receptions, targets, yards, qb_rating_against, 
            coverage_snaps_per_reception, team_name))


rundef_2024 <- rundef_2024 %>%
  select(-c(player_game_count, grades_coverage_defense, grades_defense, grades_defense_penalty,
            grades_pass_rush_defense, grades_run_defense, grades_tackle, missed_tackles, snap_counts_run,
            stops, declined_penalties, penalties, team_name))


passrush_2024 <- passrush_2024 %>%
  select(-c(player_game_count, grades_pass_rush_defense, pass_rush_wins, prp, snap_counts_pass_play,
            true_pass_set_prp, declined_penalties, true_pass_set_pass_rush_wins, 
            true_pass_set_grades_pass_rush_defense, true_pass_set_snap_counts_pass_rush,
            total_pressures, true_pass_set_total_pressures, snap_counts_pass_rush,
            true_pass_set_snap_counts_pass_play, penalties, team_name
  ))


cvg_2024 <- cvg_2024 %>%
  select(-c(player_game_count, base_snap_counts_coverage, declined_penalties, 
            man_forced_incompletes, man_grades_coverage_defense, man_missed_tackles, 
            man_assists, man_grades_coverage_defense, man_longest, man_missed_tackles,
            man_receptions, man_coverage_percent, man_snap_counts_pass_play, man_targets,
            man_yards, zone_assists, zone_coverage_percent, zone_forced_incompletes,
            zone_grades_coverage_defense, zone_missed_tackles, zone_longest, zone_receptions,
            zone_snap_counts_pass_play, zone_targets, zone_yards, penalties, team_name
  ))



merged_2024 <- merged_2024 %>%
  select(player_id, player, franchise_id, position, snap_counts_box, snap_counts_corner,
         snap_counts_dl_a_gap, snap_counts_dl_b_gap, snap_counts_dl_outside_t,
         snap_counts_dl_over_t, snap_counts_fs, snap_counts_offball, snap_counts_slot)


combined_2024 <- cvg_2024 %>%
  full_join(passrush_2024, by = c("player_id", "player", "franchise_id", "position")) %>%
  full_join(rundef_2024, by = c("player_id", "player", "franchise_id", "position")) %>%
  full_join(slot_2024, by = c("player_id", "player", "franchise_id", "position")) %>%
  full_join(merged_2024, by = c("player_id", "player", "franchise_id", "position"))

combined_2024 <- combined_2024 %>%
  mutate(year = 2024)


rosters <- cfbd_rosters(year = 2023)

teams <- cfbd_teams(year = 2023)

rosters_with_conf <- rosters %>%
  left_join(teams %>% select(school, conference), by = c("school" = "school"))








# Offense -----------------------------------------------------------------



offense <- all_data %>%
  filter(!is.na(grades_offense))


offense <- offense %>%
  filter(snap_counts_offense >= 100 | dropbacks >= 100) %>%
  select(-c(dropbacks, snap_counts_offense))


offense <- offense %>%
  left_join(combine_cleaned, by = "smart_id")

offense <- offense %>%
  select(player_id, grades_offense, Year, nfl_age, smart_id, nfl_age, position, forty, bench, 
         vertical, cone, shuttle, broad_jump_inches, college_conference)


# Define the folder path
folder_path <- "C:/Users/jackd/OneDrive/Desktop/UC BANA/Capstone/College/Offense/merged"

# List all Excel files in the folder (change the pattern if needed)
files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read each file into a list of data frames
df_list <- lapply(files, function(file) {
  df <- read.csv(file)
  return(df)
})

names(df_list) <- sapply(files, function(x) {
  # Remove the extension
  name <- tools::file_path_sans_ext(basename(x))
  # Remove the '_merged' portion
  name <- gsub("_merged", "", name)
  # Replace spaces with underscores
  name <- gsub(" ", ".", name)
  name
})
list2env(df_list, envir = .GlobalEnv)


Blocking <- Pass.Blocking %>%
  left_join(Run.Blocking, by = c("player", "player_id", "position", "year", "franchise_id", "team_name", "penalties"))

OLine <- Blocking %>%
  filter(position %in% c("G", "T", "C"))

OLine <- OLine %>%
  select(-c(player, position, player_game_count.x, player_game_count.y, declined_penalties.x,
            declined_penalties.y, franchise_id, grades_pass_block, team_name, grades_pass_block,
            pbe, true_pass_set_pbe, pass_block_percent, true_pass_set_non_spike_pass_block_percentage,
            non_spike_pass_block_percentage, true_pass_set_grades_pass_block, run_block_percent,
            zone_run_block_percent, gap_run_block_percent, gap_snap_counts_run_block_percent,
            zone_snap_counts_run_block_percent, zone_snap_counts_run_play, gap_snap_counts_run_play,
            pressures_allowed, snap_counts_pass_play, snap_counts_run_play, true_pass_set_pass_block_percent,
            true_pass_set_pressures_allowed, true_pass_set_snap_counts_pass_play))

OLine <- OLine %>%
  filter(player_id %in% offense$player_id)









# OLine -------------------------------------------------------------------
library(xgboost)

OLine <- OLine %>%
  left_join(offense, by = "player_id")

OLine <- OLine %>% 
  select(-c(year, Year, position))


# 1. Split into train and test sets
set.seed(42)


franchise_summary <- OLine %>% 
  group_by(college_conference) %>% 
  summarise(
    count_entries = n(),
    count_unique_players = n_distinct(player_id))

OLine$college_conference <- as.factor(OLine$college_conference)

OLine <- OLine %>%
  select(-c(player_id, smart_id))
# Split the data into training and test sets
train_idx <- sample(seq_len(nrow(OLine)), size = 0.8 * nrow(OLine))
train_data <- OLine[train_idx, ]
test_data  <- OLine[-train_idx, ]

# Ensure global option is set so that NA rows are not dropped
options(na.action = "na.pass")

### Create design matrix and response vector for the training set
X_train <- model.matrix(grades_offense ~ ., data = train_data, na.action = na.pass)[, -1]  # drop intercept column
y_train <- train_data$grades_offense

cat("Training data: ", nrow(train_data), "rows\n")
cat("Rows in X_train:", nrow(X_train), "\n")
cat("Length of y_train:", length(y_train), "\n")

# (Optional) Check for rows with all predictors missing in train_data
predictor_cols <- setdiff(names(train_data), "grades_offense")
rows_all_missing <- which(apply(train_data[, predictor_cols], 1, function(x) all(is.na(x))))
if(length(rows_all_missing) > 0){
  cat("Rows with all predictors missing in train_data:", rows_all_missing, "\n")
}

if(nrow(X_train) != length(y_train)){
  stop("Mismatch in number of rows between X_train and y_train. Check your training data.")
}

# Create the DMatrix for training
dtrain <- xgb.DMatrix(data = X_train, label = y_train, missing = NA)


### Create design matrix and response vector for the test set
X_test <- model.matrix(grades_offense ~ ., data = test_data, na.action = na.pass)[, -1]
y_test <- test_data$grades_offense

cat("Test data: ", nrow(test_data), "rows\n")
cat("Rows in X_test:", nrow(X_test), "\n")
cat("Length of y_test:", length(y_test), "\n")

# (Optional) Check for rows with all predictors missing in test_data
predictor_cols_test <- setdiff(names(test_data), "grades_offense")
rows_all_missing_test <- which(apply(test_data[, predictor_cols_test], 1, function(x) all(is.na(x))))
if(length(rows_all_missing_test) > 0){
  cat("Rows with all predictors missing in test_data:", rows_all_missing_test, "\n")
}

if(nrow(X_test) != length(y_test)){
  stop("Mismatch in number of rows between X_test and y_test. Check your test data.")
}

# Create the DMatrix for testing
dtest <- xgb.DMatrix(data = X_test, label = y_test, missing = NA)

### Define xgboost parameters (example settings)
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.08,
  max_depth = 10
)

### Train model, using the test set for early stopping
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  watchlist = list(train = dtrain, test = dtest),
  nrounds = 1000,
  early_stopping_rounds = 50,
  verbose = 1
)

### Predict on the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

### Compute performance metrics on the test set
mse_test <- mean((y_test - xgb_pred)^2)
r2_test <- 1 - sum((y_test - xgb_pred)^2) / sum((y_test - mean(y_test))^2)
cat("Test MSE:", mse_test, "\n")
cat("Test R²:", r2_test, "\n")
cat("Test % Var Explained:", r2_test * 100, "\n")

# 7. (Optional) Plot predicted vs. actual for the test set
plot(y_test, xgb_pred,
     xlab = "Actual Grades (Test)",
     ylab = "Predicted Grades (Test)",
     main = "OL XGBoost: Predicted vs Actual (Test)",
     pch = 16, col = "blue")

abline(a = 0, b = 1, col = "red", lwd = 2)


# Assuming you have:
# X_test: a matrix (or data frame) of predictors for the test set
# y_test: the actual response values (grades_defense) for the test set
# xgb_model: your trained XGBoost model

# 1. Generate predictions for the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

# 2. Calculate the residuals (actual - predicted)
xgb_resid <- y_test - xgb_pred

# 3. Compute the Mean Absolute Error (MAE)
mae <- mean(abs(xgb_resid))
cat("XGBoost MAE:", mae, "\n")

# 4. Create a residual plot
plot(xgb_pred, xgb_resid,
     main = "OL XGBoost Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals",
     pch = 16,         # Solid circle markers
     col = "blue")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at 0 for reference



# 1) Compute the importance matrix
importance_matrix <- xgb.importance(model = xgb_model)

# 2) Print it in tabular form
print(importance_matrix)

# 3) Plot a bar chart of feature importance
xgb.plot.importance(importance_matrix,
                    main = "XGBoost Feature Importance",
                    top_n = 30)



imp <- xgb.importance(model = xgb_model)

# Suppose the dummy columns for franchise_id are named like "franchise_id_123"
# We can group them under the same base name "franchise_id" by removing the numeric part.
# Adjust the pattern if your naming scheme is different.
# Create a new column "base_var" that groups all features starting with "franchise_id" together.
imp_grouped <- imp %>%
  mutate(base_var = if_else(str_detect(Feature, "^college_conference"),
                            "college_conference",
                            Feature)) %>%
  group_by(base_var) %>%
  summarise(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Gain))

top30 <- imp_grouped %>% slice_head(n = 30)

# Optionally, create a bar plot of the top 30 features
ggplot(top30, aes(x = reorder(base_var, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "OL Top 30 Aggregated Feature Importance", 
       x = "Feature", 
       y = "Total Gain") +
  theme_minimal()





# TE ----------------------------------------------------------------------


Receiving <- Receiving.Concept %>%
  left_join(Receiving.Depth, by = c("player", "player_id", "position", "year", "franchise_id", "team_name", "penalties",
                                    "base_targets", "player_game_count", "declined_penalties")) %>%
  left_join(Receiving.Scheme, by = c("player", "player_id", "position", "year", "franchise_id", "team_name", "penalties",
                                     "base_targets", "player_game_count", "declined_penalties"))


TE <- Blocking %>%
  filter(position == "TE") %>%
  left_join(Receiving, by = c("player", "player_id", "position", "year", "franchise_id", "team_name", "penalties"))

TE <- TE %>%
  select(-c(player, position, player_game_count.x, player_game_count.y, declined_penalties.x,
            declined_penalties.y, franchise_id, grades_pass_block, team_name, pbe, true_pass_set_pbe, 
            true_pass_set_grades_pass_block, pressures_allowed, snap_counts_pass_play, snap_counts_run_play, 
            true_pass_set_pressures_allowed, true_pass_set_snap_counts_pass_play, snap_counts_run_play,
            player_game_count, declined_penalties
            ))

# Define the substrings to remove
cols_to_remove <- c("grades", "longest", "first_downs", "fumbles", "interceptions")

# Identify the columns that do NOT contain any of these substrings
cols_keep <- names(TE)[ !grepl(paste(cols_to_remove, collapse="|"), names(TE)) ]

# Create a new data frame with only the columns to keep
TE <- TE[, cols_keep]

TE <- TE %>%
  filter(player_id %in% offense$player_id)



library(xgboost)

TE <- TE %>%
  left_join(offense, by = "player_id")

TE <- TE %>% 
  select(-c(year, Year, position))


# 1. Split into train and test sets
set.seed(42)


franchise_summary <- TE %>% 
  group_by(college_conference) %>% 
  summarise(
    count_entries = n(),
    count_unique_players = n_distinct(player_id))

TE$college_conference <- as.factor(TE$college_conference)

TE <- TE %>%
  select(-c(player_id, smart_id))
# Split the data into training and test sets
train_idx <- sample(seq_len(nrow(TE)), size = 0.8 * nrow(TE))
train_data <- TE[train_idx, ]
test_data  <- TE[-train_idx, ]

# Ensure global option is set so that NA rows are not dropped
options(na.action = "na.pass")

### Create design matrix and response vector for the training set
X_train <- model.matrix(grades_offense ~ ., data = train_data, na.action = na.pass)[, -1]  # drop intercept column
y_train <- train_data$grades_offense

cat("Training data: ", nrow(train_data), "rows\n")
cat("Rows in X_train:", nrow(X_train), "\n")
cat("Length of y_train:", length(y_train), "\n")

# (Optional) Check for rows with all predictors missing in train_data
predictor_cols <- setdiff(names(train_data), "grades_offense")
rows_all_missing <- which(apply(train_data[, predictor_cols], 1, function(x) all(is.na(x))))
if(length(rows_all_missing) > 0){
  cat("Rows with all predictors missing in train_data:", rows_all_missing, "\n")
}

if(nrow(X_train) != length(y_train)){
  stop("Mismatch in number of rows between X_train and y_train. Check your training data.")
}

# Create the DMatrix for training
dtrain <- xgb.DMatrix(data = X_train, label = y_train, missing = NA)


### Create design matrix and response vector for the test set
X_test <- model.matrix(grades_offense ~ ., data = test_data, na.action = na.pass)[, -1]
y_test <- test_data$grades_offense

cat("Test data: ", nrow(test_data), "rows\n")
cat("Rows in X_test:", nrow(X_test), "\n")
cat("Length of y_test:", length(y_test), "\n")

# (Optional) Check for rows with all predictors missing in test_data
predictor_cols_test <- setdiff(names(test_data), "grades_offense")
rows_all_missing_test <- which(apply(test_data[, predictor_cols_test], 1, function(x) all(is.na(x))))
if(length(rows_all_missing_test) > 0){
  cat("Rows with all predictors missing in test_data:", rows_all_missing_test, "\n")
}

if(nrow(X_test) != length(y_test)){
  stop("Mismatch in number of rows between X_test and y_test. Check your test data.")
}

# Create the DMatrix for testing
dtest <- xgb.DMatrix(data = X_test, label = y_test, missing = NA)

### Define xgboost parameters (example settings)
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.08,
  max_depth = 10
)



### Train model, using the test set for early stopping
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  watchlist = list(train = dtrain, test = dtest),
  nrounds = 1000,
  early_stopping_rounds = 50,
  verbose = 1
)

### Predict on the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

### Compute performance metrics on the test set
mse_test <- mean((y_test - xgb_pred)^2)
r2_test <- 1 - sum((y_test - xgb_pred)^2) / sum((y_test - mean(y_test))^2)
cat("Test MSE:", mse_test, "\n")
cat("Test R²:", r2_test, "\n")
cat("Test % Var Explained:", r2_test * 100, "\n")

# 7. (Optional) Plot predicted vs. actual for the test set
plot(y_test, xgb_pred,
     xlab = "Actual Grades (Test)",
     ylab = "Predicted Grades (Test)",
     main = "TE XGBoost: Predicted vs Actual (Test)",
     pch = 16, col = "blue")

abline(a = 0, b = 1, col = "red", lwd = 2)


# Assuming you have:
# X_test: a matrix (or data frame) of predictors for the test set
# y_test: the actual response values (grades_defense) for the test set
# xgb_model: your trained XGBoost model

# 1. Generate predictions for the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

# 2. Calculate the residuals (actual - predicted)
xgb_resid <- y_test - xgb_pred

# 3. Compute the Mean Absolute Error (MAE)
mae <- mean(abs(xgb_resid))
cat("XGBoost MAE:", mae, "\n")

# 4. Create a residual plot
plot(xgb_pred, xgb_resid,
     main = "TE XGBoost Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals",
     pch = 16,         # Solid circle markers
     col = "blue")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at 0 for reference



# 1) Compute the importance matrix
importance_matrix <- xgb.importance(model = xgb_model)

# 2) Print it in tabular form
print(importance_matrix)

# 3) Plot a bar chart of feature importance
xgb.plot.importance(importance_matrix,
                    main = "XGBoost Feature Importance",
                    top_n = 30)



imp <- xgb.importance(model = xgb_model)

# Suppose the dummy columns for franchise_id are named like "franchise_id_123"
# We can group them under the same base name "franchise_id" by removing the numeric part.
# Adjust the pattern if your naming scheme is different.
# Create a new column "base_var" that groups all features starting with "franchise_id" together.
imp_grouped <- imp %>%
  mutate(base_var = if_else(str_detect(Feature, "^college_conference"),
                            "college_conference",
                            Feature)) %>%
  group_by(base_var) %>%
  summarise(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Gain))

top30 <- imp_grouped %>% slice_head(n = 30)

# Optionally, create a bar plot of the top 30 features
ggplot(top30, aes(x = reorder(base_var, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "TE Top 30 Aggregated Feature Importance", 
       x = "Feature", 
       y = "Total Gain") +
  theme_minimal()






# WR ----------------------------------------------------------------------

WR <- Receiving %>%
  filter(position == "WR")

WR <- WR %>%
  select(-c(player, position, player_game_count, declined_penalties, franchise_id, team_name, declined_penalties
  ))

# Define the substrings to remove
cols_to_remove <- c("grades", "longest", "first_downs", "fumbles", "interceptions")

# Identify the columns that do NOT contain any of these substrings
cols_keep <- names(WR)[ !grepl(paste(cols_to_remove, collapse="|"), names(WR)) ]

# Create a new data frame with only the columns to keep
WR <- WR[, cols_keep]

WR <- WR %>%
  filter(player_id %in% offense$player_id)



library(xgboost)

WR <- WR %>%
  left_join(offense, by = "player_id")

WR <- WR %>% 
  select(-c(year, Year, position))


# 1. Split into train and test sets
set.seed(42)


franchise_summary <- WR %>% 
  group_by(college_conference) %>% 
  summarise(
    count_entries = n(),
    count_unique_players = n_distinct(player_id))

WR <- WR %>%
  group_by(college_conference) %>%
  filter(n() >= 5) %>%
  ungroup()


WR$college_conference <- as.factor(WR$college_conference)

WR <- WR %>%
  select(-c(player_id, smart_id))
# Split the data into training and test sets
train_idx <- sample(seq_len(nrow(WR)), size = 0.8 * nrow(WR))
train_data <- WR[train_idx, ]
test_data  <- WR[-train_idx, ]

# Ensure global option is set so that NA rows are not dropped
options(na.action = "na.pass")

### Create design matrix and response vector for the training set
X_train <- model.matrix(grades_offense ~ ., data = train_data, na.action = na.pass)[, -1]  # drop intercept column
y_train <- train_data$grades_offense

cat("Training data: ", nrow(train_data), "rows\n")
cat("Rows in X_train:", nrow(X_train), "\n")
cat("Length of y_train:", length(y_train), "\n")

# (Optional) Check for rows with all predictors missing in train_data
predictor_cols <- setdiff(names(train_data), "grades_offense")
rows_all_missing <- which(apply(train_data[, predictor_cols], 1, function(x) all(is.na(x))))
if(length(rows_all_missing) > 0){
  cat("Rows with all predictors missing in train_data:", rows_all_missing, "\n")
}

if(nrow(X_train) != length(y_train)){
  stop("Mismatch in number of rows between X_train and y_train. Check your training data.")
}

# Create the DMatrix for training
dtrain <- xgb.DMatrix(data = X_train, label = y_train, missing = NA)


### Create design matrix and response vector for the test set
X_test <- model.matrix(grades_offense ~ ., data = test_data, na.action = na.pass)[, -1]
y_test <- test_data$grades_offense

cat("Test data: ", nrow(test_data), "rows\n")
cat("Rows in X_test:", nrow(X_test), "\n")
cat("Length of y_test:", length(y_test), "\n")

# (Optional) Check for rows with all predictors missing in test_data
predictor_cols_test <- setdiff(names(test_data), "grades_offense")
rows_all_missing_test <- which(apply(test_data[, predictor_cols_test], 1, function(x) all(is.na(x))))
if(length(rows_all_missing_test) > 0){
  cat("Rows with all predictors missing in test_data:", rows_all_missing_test, "\n")
}

if(nrow(X_test) != length(y_test)){
  stop("Mismatch in number of rows between X_test and y_test. Check your test data.")
}

# Create the DMatrix for testing
dtest <- xgb.DMatrix(data = X_test, label = y_test, missing = NA)

### Define xgboost parameters (example settings)
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.05,
  max_depth = 5
)



### Train model, using the test set for early stopping
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  watchlist = list(train = dtrain, test = dtest),
  nrounds = 1000,
  early_stopping_rounds = 50,
  verbose = 1
)

### Predict on the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

### Compute performance metrics on the test set
mse_test <- mean((y_test - xgb_pred)^2)
r2_test <- 1 - sum((y_test - xgb_pred)^2) / sum((y_test - mean(y_test))^2)
cat("Test MSE:", mse_test, "\n")
cat("Test R²:", r2_test, "\n")
cat("Test % Var Explained:", r2_test * 100, "\n")

# 7. (Optional) Plot predicted vs. actual for the test set
plot(y_test, xgb_pred,
     xlab = "Actual Grades (Test)",
     ylab = "Predicted Grades (Test)",
     main = "WR XGBoost: Predicted vs Actual (Test)",
     pch = 16, col = "blue")

abline(a = 0, b = 1, col = "red", lwd = 2)


# Assuming you have:
# X_test: a matrix (or data frame) of predictors for the test set
# y_test: the actual response values (grades_defense) for the test set
# xgb_model: your trained XGBoost model

# 1. Generate predictions for the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

# 2. Calculate the residuals (actual - predicted)
xgb_resid <- y_test - xgb_pred

# 3. Compute the Mean Absolute Error (MAE)
mae <- mean(abs(xgb_resid))
cat("XGBoost MAE:", mae, "\n")

# 4. Create a residual plot
plot(xgb_pred, xgb_resid,
     main = "WR XGBoost Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals",
     pch = 16,         # Solid circle markers
     col = "blue")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at 0 for reference



# 1) Compute the importance matrix
importance_matrix <- xgb.importance(model = xgb_model)

# 2) Print it in tabular form
print(importance_matrix)

# 3) Plot a bar chart of feature importance
xgb.plot.importance(importance_matrix,
                    main = "XGBoost Feature Importance",
                    top_n = 30)



imp <- xgb.importance(model = xgb_model)

# Suppose the dummy columns for franchise_id are named like "franchise_id_123"
# We can group them under the same base name "franchise_id" by removing the numeric part.
# Adjust the pattern if your naming scheme is different.
# Create a new column "base_var" that groups all features starting with "franchise_id" together.
imp_grouped <- imp %>%
  mutate(base_var = if_else(str_detect(Feature, "^college_conference"),
                            "college_conference",
                            Feature)) %>%
  group_by(base_var) %>%
  summarise(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Gain))

top30 <- imp_grouped %>% slice_head(n = 30)

# Optionally, create a bar plot of the top 30 features
ggplot(top30, aes(x = reorder(base_var, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "WR Top 30 Aggregated Feature Importance", 
       x = "Feature", 
       y = "Total Gain") +
  theme_minimal()



wrtest <- Receiving.Scheme %>%
  filter(position == "WR") 

wrtest <- wrtest %>%
  select(-c(player, position, player_game_count, declined_penalties, franchise_id, team_name, declined_penalties
  ))



wrtest <- wrtest %>%
  filter(player_id %in% offense$player_id)



library(xgboost)

wrtest <- wrtest %>%
  left_join(offense, by = "player_id")

wrtest <- wrtest %>% 
  select(-c(year, Year, position))


# 1. Split into train and test sets
set.seed(42)


franchise_summary <- wrtest %>% 
  group_by(college_conference) %>% 
  summarise(
    count_entries = n(),
    count_unique_players = n_distinct(player_id))

wrtest <- wrtest%>%
  group_by(college_conference) %>%
  filter(n() >= 5) %>%
  ungroup()


wrtest$college_conference <- as.factor(wrtest$college_conference)

wrtest <- wrtest %>%
  select(-c(player_id, smart_id))
# Split the data into training and test sets
train_idx <- sample(seq_len(nrow(wrtest)), size = 0.8 * nrow(wrtest))
train_data <- wrtest[train_idx, ]
test_data  <- wrtest[-train_idx, ]

# Ensure global option is set so that NA rows are not dropped
options(na.action = "na.pass")

### Create design matrix and response vector for the training set
X_train <- model.matrix(grades_offense ~ ., data = train_data, na.action = na.pass)[, -1]  # drop intercept column
y_train <- train_data$grades_offense

cat("Training data: ", nrow(train_data), "rows\n")
cat("Rows in X_train:", nrow(X_train), "\n")
cat("Length of y_train:", length(y_train), "\n")

# (Optional) Check for rows with all predictors missing in train_data
predictor_cols <- setdiff(names(train_data), "grades_offense")
rows_all_missing <- which(apply(train_data[, predictor_cols], 1, function(x) all(is.na(x))))
if(length(rows_all_missing) > 0){
  cat("Rows with all predictors missing in train_data:", rows_all_missing, "\n")
}

if(nrow(X_train) != length(y_train)){
  stop("Mismatch in number of rows between X_train and y_train. Check your training data.")
}

# Create the DMatrix for training
dtrain <- xgb.DMatrix(data = X_train, label = y_train, missing = NA)


### Create design matrix and response vector for the test set
X_test <- model.matrix(grades_offense ~ ., data = test_data, na.action = na.pass)[, -1]
y_test <- test_data$grades_offense

cat("Test data: ", nrow(test_data), "rows\n")
cat("Rows in X_test:", nrow(X_test), "\n")
cat("Length of y_test:", length(y_test), "\n")

# (Optional) Check for rows with all predictors missing in test_data
predictor_cols_test <- setdiff(names(test_data), "grades_offense")
rows_all_missing_test <- which(apply(test_data[, predictor_cols_test], 1, function(x) all(is.na(x))))
if(length(rows_all_missing_test) > 0){
  cat("Rows with all predictors missing in test_data:", rows_all_missing_test, "\n")
}

if(nrow(X_test) != length(y_test)){
  stop("Mismatch in number of rows between X_test and y_test. Check your test data.")
}

# Create the DMatrix for testing
dtest <- xgb.DMatrix(data = X_test, label = y_test, missing = NA)

### Define xgboost parameters (example settings)
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 6
)



### Train model, using the test set for early stopping
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  watchlist = list(train = dtrain, test = dtest),
  nrounds = 1000,
  early_stopping_rounds = 50,
  verbose = 1
)

### Predict on the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

### Compute performance metrics on the test set
mse_test <- mean((y_test - xgb_pred)^2)
r2_test <- 1 - sum((y_test - xgb_pred)^2) / sum((y_test - mean(y_test))^2)
cat("Test MSE:", mse_test, "\n")
cat("Test R²:", r2_test, "\n")
cat("Test % Var Explained:", r2_test * 100, "\n")

# 7. (Optional) Plot predicted vs. actual for the test set
plot(y_test, xgb_pred,
     xlab = "Actual Grades (Test)",
     ylab = "Predicted Grades (Test)",
     main = "WR XGBoost: Predicted vs Actual (Test)",
     pch = 16, col = "blue")

abline(a = 0, b = 1, col = "red", lwd = 2)


# Assuming you have:
# X_test: a matrix (or data frame) of predictors for the test set
# y_test: the actual response values (grades_defense) for the test set
# xgb_model: your trained XGBoost model

# 1. Generate predictions for the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

# 2. Calculate the residuals (actual - predicted)
xgb_resid <- y_test - xgb_pred

# 3. Compute the Mean Absolute Error (MAE)
mae <- mean(abs(xgb_resid))
cat("XGBoost MAE:", mae, "\n")

# 4. Create a residual plot
plot(xgb_pred, xgb_resid,
     main = "WR XGBoost Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals",
     pch = 16,         # Solid circle markers
     col = "blue")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at 0 for reference



# 1) Compute the importance matrix
importance_matrix <- xgb.importance(model = xgb_model)

# 2) Print it in tabular form
print(importance_matrix)

# 3) Plot a bar chart of feature importance
xgb.plot.importance(importance_matrix,
                    main = "XGBoost Feature Importance",
                    top_n = 30)



imp <- xgb.importance(model = xgb_model)

# Suppose the dummy columns for franchise_id are named like "franchise_id_123"
# We can group them under the same base name "franchise_id" by removing the numeric part.
# Adjust the pattern if your naming scheme is different.
# Create a new column "base_var" that groups all features starting with "franchise_id" together.
imp_grouped <- imp %>%
  mutate(base_var = if_else(str_detect(Feature, "^college_conference"),
                            "college_conference",
                            Feature)) %>%
  group_by(base_var) %>%
  summarise(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Gain))

top30 <- imp_grouped %>% slice_head(n = 30)

# Optionally, create a bar plot of the top 30 features
ggplot(top30, aes(x = reorder(base_var, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "WR Top 30 Aggregated Feature Importance", 
       x = "Feature", 
       y = "Total Gain") +
  theme_minimal()



# HB ----------------------------------------------------------------------

HB <- Rushing %>%
  filter(position == "HB") %>%
  left_join(Pass.Blocking, by = c("player", "player_id", "position", "year", "franchise_id", "team_name", "penalties",
                                  "player_game_count", "declined_penalties")) %>%
  left_join(Receiving.Scheme, by = c("player", "player_id", "position", "year", "franchise_id", "team_name", "penalties",
                                     "player_game_count", "declined_penalties"))

HB <- HB %>%
  select(-c(player, position, player_game_count, declined_penalties, franchise_id, team_name, declined_penalties,
            grades_offense
  ))



HB <- HB %>%
  filter(player_id %in% offense$player_id)



library(xgboost)

HB <- HB %>%
  left_join(offense, by = "player_id")

HB <- HB %>% 
  select(-c(year, Year, position))


# 1. Split into train and test sets
set.seed(42)


franchise_summary <- HB %>% 
  group_by(college_conference) %>% 
  summarise(
    count_entries = n(),
    count_unique_players = n_distinct(player_id))

HB$college_conference <- as.factor(HB$college_conference)

HB <- HB %>%
  select(-c(player_id, smart_id, grades_hands_fumble, grades_offense_penalty, grades_pass, grades_pass_block.x, grades_pass_route,
            grades_run, grades_run_block, grades_pass_block.y, man_grades_hands_drop, scramble_yards, scrambles,
            man_grades_pass_route, zone_grades_hands_drop, zone_grades_pass_route, true_pass_set_grades_pass_block,
            elusive_rating, longest, pbe, pressures_allowed, true_pass_set_pressures_allowed,
            true_pass_set_pbe
            ))
# Split the data into training and test sets
train_idx <- sample(seq_len(nrow(HB)), size = 0.8 * nrow(HB))
train_data <- HB[train_idx, ]
test_data  <- HB[-train_idx, ]

# Ensure global option is set so that NA rows are not dropped
options(na.action = "na.pass")

### Create design matrix and response vector for the training set
X_train <- model.matrix(grades_offense ~ ., data = train_data, na.action = na.pass)[, -1]  # drop intercept column
y_train <- train_data$grades_offense

cat("Training data: ", nrow(train_data), "rows\n")
cat("Rows in X_train:", nrow(X_train), "\n")
cat("Length of y_train:", length(y_train), "\n")

# (Optional) Check for rows with all predictors missing in train_data
predictor_cols <- setdiff(names(train_data), "grades_offense")
rows_all_missing <- which(apply(train_data[, predictor_cols], 1, function(x) all(is.na(x))))
if(length(rows_all_missing) > 0){
  cat("Rows with all predictors missing in train_data:", rows_all_missing, "\n")
}

if(nrow(X_train) != length(y_train)){
  stop("Mismatch in number of rows between X_train and y_train. Check your training data.")
}

# Create the DMatrix for training
dtrain <- xgb.DMatrix(data = X_train, label = y_train, missing = NA)


### Create design matrix and response vector for the test set
X_test <- model.matrix(grades_offense ~ ., data = test_data, na.action = na.pass)[, -1]
y_test <- test_data$grades_offense

cat("Test data: ", nrow(test_data), "rows\n")
cat("Rows in X_test:", nrow(X_test), "\n")
cat("Length of y_test:", length(y_test), "\n")

# (Optional) Check for rows with all predictors missing in test_data
predictor_cols_test <- setdiff(names(test_data), "grades_offense")
rows_all_missing_test <- which(apply(test_data[, predictor_cols_test], 1, function(x) all(is.na(x))))
if(length(rows_all_missing_test) > 0){
  cat("Rows with all predictors missing in test_data:", rows_all_missing_test, "\n")
}

if(nrow(X_test) != length(y_test)){
  stop("Mismatch in number of rows between X_test and y_test. Check your test data.")
}

# Create the DMatrix for testing
dtest <- xgb.DMatrix(data = X_test, label = y_test, missing = NA)

### Define xgboost parameters (example settings)
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.02,
  max_depth = 8
)



### Train model, using the test set for early stopping
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  watchlist = list(train = dtrain, test = dtest),
  nrounds = 1000,
  early_stopping_rounds = 50,
  verbose = 1
)

### Predict on the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

### Compute performance metrics on the test set
mse_test <- mean((y_test - xgb_pred)^2)
r2_test <- 1 - sum((y_test - xgb_pred)^2) / sum((y_test - mean(y_test))^2)
cat("Test MSE:", mse_test, "\n")
cat("Test R²:", r2_test, "\n")
cat("Test % Var Explained:", r2_test * 100, "\n")

# 7. (Optional) Plot predicted vs. actual for the test set
plot(y_test, xgb_pred,
     xlab = "Actual Grades (Test)",
     ylab = "Predicted Grades (Test)",
     main = "HB XGBoost: Predicted vs Actual (Test)",
     pch = 16, col = "blue")

abline(a = 0, b = 1, col = "red", lwd = 2)


# Assuming you have:
# X_test: a matrix (or data frame) of predictors for the test set
# y_test: the actual response values (grades_defense) for the test set
# xgb_model: your trained XGBoost model

# 1. Generate predictions for the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

# 2. Calculate the residuals (actual - predicted)
xgb_resid <- y_test - xgb_pred

# 3. Compute the Mean Absolute Error (MAE)
mae <- mean(abs(xgb_resid))
cat("XGBoost MAE:", mae, "\n")

# 4. Create a residual plot
plot(xgb_pred, xgb_resid,
     main = "HB XGBoost Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals",
     pch = 16,         # Solid circle markers
     col = "blue")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at 0 for reference



# 1) Compute the importance matrix
importance_matrix <- xgb.importance(model = xgb_model)

# 2) Print it in tabular form
print(importance_matrix)

# 3) Plot a bar chart of feature importance
xgb.plot.importance(importance_matrix,
                    main = "XGBoost Feature Importance",
                    top_n = 30)



imp <- xgb.importance(model = xgb_model)

# Suppose the dummy columns for franchise_id are named like "franchise_id_123"
# We can group them under the same base name "franchise_id" by removing the numeric part.
# Adjust the pattern if your naming scheme is different.
# Create a new column "base_var" that groups all features starting with "franchise_id" together.
imp_grouped <- imp %>%
  mutate(base_var = if_else(str_detect(Feature, "^college_conference"),
                            "college_conference",
                            Feature)) %>%
  group_by(base_var) %>%
  summarise(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Gain))

top30 <- imp_grouped %>% slice_head(n = 30)

# Optionally, create a bar plot of the top 30 features
ggplot(top30, aes(x = reorder(base_var, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "HB Top 30 Aggregated Feature Importance", 
       x = "Feature", 
       y = "Total Gain") +
  theme_minimal()



# QB ----------------------------------------------------------------------

QB <- Rushing %>%
  filter(position == "QB") %>%
  left_join(Passing.Concept, by = c("player", "player_id", "position", "year", "franchise_id", "team_name", "penalties",
                                    "player_game_count", "declined_penalties")) %>%
  left_join(Passing.Pressure, by = c("player", "player_id", "position", "year", "franchise_id", "team_name", "penalties",
                                     "player_game_count", "declined_penalties"))



QB <- QB %>%
  select(-c(player, position, player_game_count, declined_penalties, franchise_id, team_name, declined_penalties
  ))

# Define the substrings to remove
cols_to_remove <- c("grades", "longest", "first_downs", "fumbles")

# Identify the columns that do NOT contain any of these substrings
cols_keep <- names(QB)[ !grepl(paste(cols_to_remove, collapse="|"), names(QB)) ]

# Create a new data frame with only the columns to keep
QB <- QB[, cols_keep]

QB <- QB %>%
  filter(player_id %in% offense$player_id)



library(xgboost)

QB <- QB %>%
  left_join(offense, by = "player_id")

QB <- QB %>% 
  select(-c(year, Year, position))


# 1. Split into train and test sets
set.seed(42)


franchise_summary <- QB %>% 
  group_by(college_conference) %>% 
  summarise(
    count_entries = n(),
    count_unique_players = n_distinct(player_id))

QB <- QB %>%
  group_by(college_conference) %>%
  filter(n() >= 10) %>%
  ungroup()

QB$college_conference <- as.factor(QB$college_conference)

QB <- QB %>%
  select(-c(player_id, smart_id))
# Split the data into training and test sets
train_idx <- sample(seq_len(nrow(QB)), size = 0.8 * nrow(QB))
train_data <- QB[train_idx, ]
test_data  <- QB[-train_idx, ]

# Ensure global option is set so that NA rows are not dropped
options(na.action = "na.pass")

### Create design matrix and response vector for the training set
X_train <- model.matrix(grades_offense ~ ., data = train_data, na.action = na.pass)[, -1]  # drop intercept column
y_train <- train_data$grades_offense

cat("Training data: ", nrow(train_data), "rows\n")
cat("Rows in X_train:", nrow(X_train), "\n")
cat("Length of y_train:", length(y_train), "\n")

# (Optional) Check for rows with all predictors missing in train_data
predictor_cols <- setdiff(names(train_data), "grades_offense")
rows_all_missing <- which(apply(train_data[, predictor_cols], 1, function(x) all(is.na(x))))
if(length(rows_all_missing) > 0){
  cat("Rows with all predictors missing in train_data:", rows_all_missing, "\n")
}

if(nrow(X_train) != length(y_train)){
  stop("Mismatch in number of rows between X_train and y_train. Check your training data.")
}

# Create the DMatrix for training
dtrain <- xgb.DMatrix(data = X_train, label = y_train, missing = NA)


### Create design matrix and response vector for the test set
X_test <- model.matrix(grades_offense ~ ., data = test_data, na.action = na.pass)[, -1]
y_test <- test_data$grades_offense

cat("Test data: ", nrow(test_data), "rows\n")
cat("Rows in X_test:", nrow(X_test), "\n")
cat("Length of y_test:", length(y_test), "\n")

# (Optional) Check for rows with all predictors missing in test_data
predictor_cols_test <- setdiff(names(test_data), "grades_offense")
rows_all_missing_test <- which(apply(test_data[, predictor_cols_test], 1, function(x) all(is.na(x))))
if(length(rows_all_missing_test) > 0){
  cat("Rows with all predictors missing in test_data:", rows_all_missing_test, "\n")
}

if(nrow(X_test) != length(y_test)){
  stop("Mismatch in number of rows between X_test and y_test. Check your test data.")
}

# Create the DMatrix for testing
dtest <- xgb.DMatrix(data = X_test, label = y_test, missing = NA)

### Define xgboost parameters (example settings)
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.06,
  max_depth = 10
)



### Train model, using the test set for early stopping
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  watchlist = list(train = dtrain, test = dtest),
  nrounds = 1000,
  early_stopping_rounds = 50,
  verbose = 1
)

### Predict on the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

### Compute performance metrics on the test set
mse_test <- mean((y_test - xgb_pred)^2)
r2_test <- 1 - sum((y_test - xgb_pred)^2) / sum((y_test - mean(y_test))^2)
cat("Test MSE:", mse_test, "\n")
cat("Test R²:", r2_test, "\n")
cat("Test % Var Explained:", r2_test * 100, "\n")

# 7. (Optional) Plot predicted vs. actual for the test set
plot(y_test, xgb_pred,
     xlab = "Actual Grades (Test)",
     ylab = "Predicted Grades (Test)",
     main = "QB XGBoost: Predicted vs Actual (Test)",
     pch = 16, col = "blue")

abline(a = 0, b = 1, col = "red", lwd = 2)


# Assuming you have:
# X_test: a matrix (or data frame) of predictors for the test set
# y_test: the actual response values (grades_defense) for the test set
# xgb_model: your trained XGBoost model

# 1. Generate predictions for the test set
xgb_pred <- predict(xgb_model, newdata = X_test)

# 2. Calculate the residuals (actual - predicted)
xgb_resid <- y_test - xgb_pred

# 3. Compute the Mean Absolute Error (MAE)
mae <- mean(abs(xgb_resid))
cat("XGBoost MAE:", mae, "\n")

# 4. Create a residual plot
plot(xgb_pred, xgb_resid,
     main = "QB XGBoost Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals",
     pch = 16,         # Solid circle markers
     col = "blue")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at 0 for reference



# 1) Compute the importance matrix
importance_matrix <- xgb.importance(model = xgb_model)

# 2) Print it in tabular form
print(importance_matrix)

# 3) Plot a bar chart of feature importance
xgb.plot.importance(importance_matrix,
                    main = "XGBoost Feature Importance",
                    top_n = 30)



imp <- xgb.importance(model = xgb_model)

# Suppose the dummy columns for franchise_id are named like "franchise_id_123"
# We can group them under the same base name "franchise_id" by removing the numeric part.
# Adjust the pattern if your naming scheme is different.
# Create a new column "base_var" that groups all features starting with "franchise_id" together.
imp_grouped <- imp %>%
  mutate(base_var = if_else(str_detect(Feature, "^college_conference"),
                            "college_conference",
                            Feature)) %>%
  group_by(base_var) %>%
  summarise(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Gain))

top30 <- imp_grouped %>% slice_head(n = 30)

# Optionally, create a bar plot of the top 30 features
ggplot(top30, aes(x = reorder(base_var, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "QB Top 30 Aggregated Feature Importance", 
       x = "Feature", 
       y = "Total Gain") +
  theme_minimal()





# Project Prospects -------------------------------------------------------
library(readxl)

prospects <- read_xlsx("C:/Users/jackd/OneDrive/Desktop/UC BANA/Capstone/Prospect_Ages.xlsx")

combine_2025 <- load_combine(2025)
combine_2025 <- combine_2025 %>%
  mutate(player_name = recode(player_name,
                              "Cleveland Harris"      = "Tre Harris",
                              "Jacobee Bryant"        = "Cobee Bryant",
                              "Ra'Mello Dotson"       = "Mello Dotson",
                              "Joshua Lane"           = "Jaylin Lane",
                              "Woody Marks"           = "Jo'Quavious Marks",
                              "Robert Webb"           = "Clay Webb",
                              "Decarrion McWilliams"  = "Mac McWilliams"
  ))


missingprospects <- combine_2025 %>%
  filter(!player_name %in% prospects$player) %>%
  filter(!pos %in% c("K", "P", "LS"))

missingcombine <- prospects %>%
  filter(!player %in% combine_2025$player_name)
  

prospects <- prospects %>%
  left_join(combine_2025, c("player" = "player_name"))

prospects <- prospects %>%
  select(-c(draft_year, draft_ovr, draft_team, draft_round, pfr_id, cfb_id, school, pos, season))

prospects <- prospects %>%
  filter(!is.na(draft_age))


prospects_long <- prospects %>%
  pivot_longer(
    cols = NFL_1_Age:NFL_5_Age,
    names_to = "season",
    names_pattern = "NFL_(\\d+)_Age",
    values_to = "nfl_age"
  ) %>%
  mutate(Year = as.integer(season) + 2024) %>%  # 1 becomes 2025, 2 becomes 2026, etc.
  select(-c(season, player)) 

# You can either read them one by one, specifying exact file paths:
coverage_scheme_2024 <- read.csv("C:/Users/jackd/OneDrive/Desktop/UC BANA/Capstone/College/Defense/2024/Coverage Scheme 2024.csv")
defense_ovr_2024     <- read.csv("C:/Users/jackd/OneDrive/Desktop/UC BANA/Capstone/College/Defense/2024/Defense Ovr 2024.csv")
pass_rush_2024       <- read.csv("C:/Users/jackd/OneDrive/Desktop/UC BANA/Capstone/College/Defense/2024/Pass Rush 2024.csv")
run_defense_2024     <- read.csv("C:/Users/jackd/OneDrive/Desktop/UC BANA/Capstone/College/Defense/2024/Run Defense 2024.csv")
slot_2024            <- read.csv("C:/Users/jackd/OneDrive/Desktop/UC BANA/Capstone/College/Defense/2024/Slot 2024.csv")




slot_2024 <- slot_2024 %>%
  select(-c(player_game_count, receptions, targets, yards, qb_rating_against, 
            coverage_snaps_per_reception, team_name))


run_defense_2024 <- run_defense_2024 %>%
  select(-c(player_game_count, grades_coverage_defense, grades_defense_penalty, grades_defense,
            grades_pass_rush_defense, grades_run_defense, grades_tackle, missed_tackles, snap_counts_run,
            stops, declined_penalties, penalties, team_name))


pass_rush_2024 <- pass_rush_2024 %>%
  select(-c(player_game_count, grades_pass_rush_defense, pass_rush_wins, prp, snap_counts_pass_play,
            true_pass_set_prp, declined_penalties, true_pass_set_pass_rush_wins, 
            true_pass_set_grades_pass_rush_defense, true_pass_set_snap_counts_pass_rush,
            total_pressures, true_pass_set_total_pressures, snap_counts_pass_rush,
            true_pass_set_snap_counts_pass_play, penalties, team_name
  ))


coverage_scheme_2024 <- coverage_scheme_2024 %>%
  select(-c(player_game_count, base_snap_counts_coverage, declined_penalties, 
            man_forced_incompletes, man_grades_coverage_defense, man_missed_tackles, 
            man_assists, man_grades_coverage_defense, man_longest, man_missed_tackles,
            man_receptions, man_coverage_percent, man_snap_counts_pass_play, man_targets,
            man_yards, zone_assists, zone_coverage_percent, zone_forced_incompletes,
            zone_grades_coverage_defense, zone_missed_tackles, zone_longest, zone_receptions,
            zone_snap_counts_pass_play, zone_targets, zone_yards, penalties, team_name
  ))


defense_ovr_2024 <- defense_ovr_2024 %>%
  select(player_id, player, franchise_id, position, snap_counts_box, snap_counts_corner,
         snap_counts_dl_a_gap, snap_counts_dl_b_gap, snap_counts_dl_outside_t,
         snap_counts_dl_over_t, snap_counts_fs, snap_counts_offball, snap_counts_slot)


combined_defense_2024 <- coverage_scheme_2024 %>%
  full_join(pass_rush_2024, by = c("player_id", "player", "franchise_id", "position")) %>%
  full_join(run_defense_2024, by = c("player_id", "player", "franchise_id", "position")) %>%
  full_join(slot_2024, by = c("player_id", "player", "franchise_id", "position")) %>%
  full_join(defense_ovr_2024, by = c("player_id", "player", "franchise_id", "position"))


rm(slot_2024, run_defense_2024, pass_rush_2024, defense_ovr_2024, coverage_scheme_2024)

combined_defense_2024$year <- 2024

prospects_pff <- rbind(combined_defense, combined_defense_2024)


prospects_pff <- prospects_pff %>%
  filter(player_id %in% prospects$player_id)


prospects_pff <- prospects_pff %>%
  filter(!position %in% c("WR", "HB", "TE"))

prospect_predictions_df <- prospects_pff %>%
  left_join(prospects_long, by = "player_id")

prospect_predictions_df <- prospect_predictions_df %>%
  mutate(college_age = nfl_age - (Year - year))

prospect_predictions_df <- prospect_predictions_df %>%
  # Convert broad jump (applied to all prospects)
  mutate(
    broad_jump = as.character(broad_jump),
    broad_feet = ifelse(nchar(broad_jump) == 3,
                        as.numeric(substr(broad_jump, 1, 2)),
                        as.numeric(substr(broad_jump, 1, 1))),
    broad_inches = ifelse(nchar(broad_jump) == 3,
                          as.numeric(substr(broad_jump, 3, 3)),
                          as.numeric(substr(broad_jump, 2, 2))),
    broad_jump_inches = broad_feet * 12 + broad_inches
  ) %>%
  # Drop intermediate columns
  select(-broad_jump, -broad_feet, -broad_inches)


### PART 4: Aggregate Snap Counts and Compute Career-Aggregated Snap Metrics ###
# Identify snap count columns (exclude "snap_counts_offball")
snap_cols <- names(prospects_pff)[
  grepl("snap_counts", names(prospects_pff)) & 
    !grepl("snap_counts_offball", names(prospects_pff))
]

snap_summary_by_player <- prospects_pff %>%
  group_by(player_id) %>%
  summarise(
    across(all_of(snap_cols), ~ sum(.x, na.rm = TRUE), .names = "total_{.col}"),
    total_snaps = sum(c_across(all_of(snap_cols)), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(starts_with("total_snap_counts"), ~ .x / total_snaps, .names = "prop_{.col}")
  )

### PART 5: Group by Player to Extract DI/ED (by Direct Position Designation) ###
# Here we check each player's rows in prospect_predictions_df.
# If any row has position == "DI" or "ED", flag that player.
player_designation <- prospect_predictions_df %>%
  group_by(player_id) %>%
  summarise(
    has_DI = any(position == "DI"),
    has_ED = any(position == "ED"),
    .groups = "drop"
  )
# Join these flags back
prospect_predictions_df <- prospect_predictions_df %>%
  left_join(player_designation, by = "player_id")

# Immediately extract players that have a direct DI or ED designation.
DI_df_existing <- prospect_predictions_df %>% filter(has_DI)
ED_df_existing <- prospect_predictions_df %>% filter(has_ED)

### PART 6: For Players Without DI/ED, Use Snap Count Evaluation ###
# Identify players with neither DI nor ED
unassigned_players <- player_designation %>%
  filter(!has_DI & !has_ED) %>%
  pull(player_id)

# For these players, use four key snap count proportions: corner, box, slot, and fs.
unassigned_snap <- snap_summary_by_player %>%
  filter(player_id %in% unassigned_players) %>%
  pivot_longer(
    cols = c(prop_total_snap_counts_corner,
             prop_total_snap_counts_box,
             prop_total_snap_counts_slot,
             prop_total_snap_counts_fs),
    names_to = "snap_type",
    values_to = "prop_value"
  ) %>%
  group_by(player_id) %>%
  summarise(
    majority_snap = snap_type[which.max(prop_value)],
    .groups = "drop"
  ) %>%
  mutate(
    final_designation = case_when(
      majority_snap == "prop_total_snap_counts_box" ~ "box",
      majority_snap == "prop_total_snap_counts_corner" ~ "corner",  # Changed from "lb" to "corner"
      majority_snap == "prop_total_snap_counts_slot" ~ "slot",
      majority_snap == "prop_total_snap_counts_fs" ~ "fs",
      TRUE ~ NA_character_
    )
  )
# Join the fallback designation into the main predictions data
prospect_predictions_df <- prospect_predictions_df %>%
  left_join(unassigned_snap %>% select(player_id, final_designation), by = "player_id")

# For unassigned players, create subsets based on final_designation:
box_df  <- prospect_predictions_df %>% filter(final_designation == "box")
corner_df <- prospect_predictions_df %>% filter(final_designation == "corner")  # Now using "corner"
slot_df <- prospect_predictions_df %>% filter(final_designation == "slot")
fs_df   <- prospect_predictions_df %>% filter(final_designation == "fs")

### FINAL OUTPUT ###
# You now have:
# - DI_df_existing: players with at least one entry where position == "DI"
# - ED_df_existing: players with at least one entry where position == "ED"
# And for players unassigned by direct designation, you have fallback groups:
#       box_df, corner_df, slot_df, fs_df

# (Optional) Display counts:
cat("Direct DI players:", n_distinct(DI_df_existing$player_id), "\n")
cat("Direct ED players:", n_distinct(ED_df_existing$player_id), "\n")
cat("Fallback box players:", n_distinct(box_df$player_id), "\n")
cat("Fallback corner players:", n_distinct(corner_df$player_id), "\n")
cat("Fallback slot players:", n_distinct(slot_df$player_id), "\n")
cat("Fallback fs players:", n_distinct(fs_df$player_id), "\n")



# DI Prospects ------------------------------------------------------------

missing_in_prospects <- setdiff(colnames(di_filtered), colnames(DI_df_existing))
extra_in_prospects   <- setdiff(colnames(DI_df_existing), colnames(di_filtered))
print(missing_in_prospects)
print(extra_in_prospects)


# Ensure factor variables (like college_conference) have consistent levels.
DI_df_existing$college_conference <- factor(
  DI_df_existing$college_conference, 
  levels = levels(train_data$college_conference)
)

# --- STEP 1: Save Identifier Columns ---
# Include the college_age column along with others for a richer context.
ids <- DI_df_existing %>% select(player_id, player, nfl_age, college_age)

predictor_vars <- colnames(X_train)

# --- STEP 2: Subset New Data to Predictor Columns ---
# Use predictor_vars (from training) to select only the needed columns.
proj_data <- DI_df_existing %>% select(any_of(predictor_vars))

# --- STEP 3: Create the Design Matrix ---
# Create dummy variables using model.matrix (dropping the intercept).
X_proj <- model.matrix(~ ., data = proj_data, na.action = na.pass)[, -1]

# Check for any missing predictors relative to the training set.
missing_cols <- setdiff(predictor_vars, colnames(X_proj))
if(length(missing_cols) > 0) {
  for (col in missing_cols) {
    # Add the missing predictor as a numeric vector filled with zeros.
    X_proj <- cbind(X_proj, rep(0, nrow(X_proj)))
    colnames(X_proj)[ncol(X_proj)] <- col
  }
}

# Reorder columns to exactly match the training predictor order.
X_proj <- as.matrix(X_proj[, predictor_vars])

# --- STEP 4: Build DMatrix and Generate Predictions ---
dproj <- xgb.DMatrix(data = X_proj, missing = NA)
predictions <- predict(xgb_model, dproj)

# --- STEP 5: Merge Predictions with Identifiers ---
# Combine predictions with the identifying info including college_age.
DI_df_existing <- ids %>% mutate(projection = predictions)

# Optionally, if DI_df_existing has multiple rows per player/nfl_age, you might want to aggregate.
# Here, we group by player, player_id, nfl_age, and college_age.
aggregated_projections <- DI_df_existing %>%
  group_by(player_id, player, nfl_age, college_age) %>%
  summarise(avg_projection = mean(projection, na.rm = TRUE),
            .groups = "drop")



# Plot each player's projection over time
p <- ggplot(aggregated_projections, aes(x = nfl_age, y = avg_projection,
                                      color = player, group = player)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Player Projections Over Time",
       x = "NFL Age",
       y = "Average Projection") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

print(p)



# ED Prospects ------------------------------------------------------------

# Assume train_data was your training dataset
# Explicitly set factor levels in ED_df_existing to match training data
ED_df_existing$college_conference <- factor(
  ED_df_existing$college_conference,
  levels = levels(train_data$college_conference)
)
library(dplyr)
library(xgboost)


# --- Prepare identifier columns ---
# Save player_id, player, and nfl_age from your new data.
ids <- ED_df_existing %>% select(player_id, player, nfl_age)

predictor_vars <- colnames(X_train)

# --- Subset new data for prediction ---
# Use the predictor variables stored from training in predictor_vars.
# (Assume predictor_vars includes all column names in the training design matrix.)
proj_data <- ED_df_existing %>% select(any_of(predictor_vars))


# Ensure factors (like college_conference) are set correctly, if needed:
ED_df_existing$college_conference <- factor(
  ED_df_existing$college_conference,
  levels = levels(train_data$college_conference)
)

# --- Create design matrix ---
X_proj <- model.matrix(~ ., data = proj_data, na.action = na.pass)[, -1]

# Identify any missing columns compared to predictor_vars and add them.
missing_cols <- setdiff(predictor_vars, colnames(X_proj))
if (length(missing_cols) > 0) {
  for (col in missing_cols) {
    X_proj <- cbind(X_proj, rep(0, nrow(X_proj)))
    colnames(X_proj)[ncol(X_proj)] <- col
  }
}

# Reorder columns to match training predictors and ensure the object is a numeric matrix.
X_proj <- as.matrix(X_proj[, predictor_vars])

# --- Build DMatrix and generate predictions ---
dproj <- xgb.DMatrix(data = X_proj, missing = NA)
predictions <- predict(xgb_model, dproj)

# --- Merge predictions with identifiers including the 'player' column ---
ED_df_existing <- ids %>% mutate(projection = predictions)

# If you have multiple rows per player, aggregate if needed:
averaged_projections <- ED_df_existing %>%
  group_by(player_id, player, nfl_age) %>%
  summarise(avg_projection = mean(projection, na.rm = TRUE),
            .groups = "drop")

print(averaged_projections)


# Plot each player's projection over time
p <- ggplot(averaged_projections, aes(x = nfl_age, y = avg_projection,
                                      color = player, group = player)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Player Projections Over Time",
       x = "NFL Age",
       y = "Average Projection") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

print(p)



# CB Prospects ------------------------------------------------------------


