# 🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳                                                                                                             

#
#           .o.       oooooo     oooo ooooo ooooooooooooo ooooo     ooo  .oooooo..o      8      ooooo        ooooooooo.   
#          .888.       `888.     .8'  `888' 8'   888   `8 `888'     `8' d8P'    `Y8      8      `888'        `888   `Y88. 
#         .8"888.       `888.   .8'    888       888       888       8  Y88bo.           8       888          888   .d88' 
#        .8' `888.       `888. .8'     888       888       888       8   `"Y8888o.       8       888          888ooo88P'  
#       .88ooo8888.       `888.8'      888       888       888       8       `"Y88b      8       888          888`88b.    
#      .8'     `888.       `888'       888       888       `88.    .8'  oo     .d8P      8       888       o  888  `88b.  
#     o88o     o8888o       `8'       o888o     o888o        `YbodP'    8""88888P'       8      o888ooooood8 o888o  o888o 
#                                                                                                                    
#                                                                                                                    
# 🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳🔳                                                                                                          


##### 0. ***START*** Date configuration & library loading logic ---------------------------------


library(dplyr)
library(tidyr)
library(caret)
library(glmnet)
library(e1071)
library(ggplot2)
library(ROCR)
library(broom)
library(parallel)
library(broom)


##### 0. ***END*** Date configuration & library loading logic---------------------------------

# 1.2 Load BaseStats_Team
team_output_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/Datahub (Temp)/1. hoopR/1. BaseStats_Team/BaseStats_Team.csv"
BaseStats_Team <- read.csv(team_output_path, stringsAsFactors = FALSE)

# 1.3 Reference the past data for BaseStats_Team
past_data <- BaseStats_Team

# Updated AVITUS (Logistic Regression Model) with Training & Testing Data
# ------------------------------------------------------------

# 1.1 Prepare Data
# Updated filter columns based on expanded data frame structure
predictor_columns <- c(
  "team_winner", "T_TS_PCT","T_AST_PCT", "T_REB_PCT", "T_STL_PCT", "T_TOV_PCT", "T_FG_PCT_DIFF", "T_OREB_PCT", "T_FTR2nd"

)

# 1.2 Reference the past data for BaseStats_Team
past_data <- BaseStats_Team

# 1.3 Select only the necessary columns
valid_columns <- intersect(c("game_id_alt", "team", "opp", "game_id", predictor_columns), names(past_data))
past_data <- past_data[, valid_columns, drop = FALSE]

# Ensure team_winner is a factor (0 or 1)
past_data$team_winner <- as.factor(past_data$team_winner)

# Handle missing values
past_data <- na.omit(past_data)

# Ensure all predictor columns are numeric
numeric_cols <- setdiff(names(past_data), c("game_id_alt", "team", "opp", "game_id", "team_winner"))
past_data[numeric_cols] <- lapply(past_data[numeric_cols], as.numeric)

# 1.4 Split Data into Training & Testing Sets
set.seed(123)  # Ensure reproducibility
train_indices <- sample(1:nrow(past_data), size = 0.8 * nrow(past_data))  # 80% train, 20% test
training_data <- past_data[train_indices, ]
testing_data <- past_data[-train_indices, ]

# 1.5 Build Logistic Regression Model
logreg_model <- glm(
  team_winner ~ .,
  data = training_data[, -which(names(training_data) %in% c("game_id_alt", "team", "opp", "game_id"))],
  family = binomial(link = "logit")
)

# 1.6 Evaluate Model on Testing Data
predictions <- predict(logreg_model, newdata = testing_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)  # Convert probabilities to 0 or 1
confusion_matrix <- table(Predicted = predicted_classes, Actual = testing_data$team_winner)
print("Confusion Matrix:")
print(confusion_matrix)

# 1.7 Load Current Schedule for Predictions
if (exists(paste0("nba_schedule_", formatted_date))) {
  data_schedule <- get(paste0("nba_schedule_", formatted_date))
} else if (exists(paste0("nba_schedule", formatted_date))) {
  data_schedule <- get(paste0("nba_schedule", formatted_date))
} else {
  stop("Current schedule data not found. Please ensure it is loaded correctly.")
}

data_schedule$win_prob <- NA

data_schedule <- data_schedule[, c(
  "team", "opp", "game_id", "win_prob",
  setdiff(names(data_schedule), c("team", "opp", "game_id", "win_prob"))
)]

# 1.8 Make Predictions for the Current Schedule
for (game in unique(data_schedule$game_id)) {
  game_rows <- which(data_schedule$game_id == game)
  if (length(game_rows) != 2) next  # Ensure exactly two teams per game
  
  current_team <- data_schedule$team[game_rows[1]]
  current_opp <- data_schedule$team[game_rows[2]]
  
  team_stats <- past_data[past_data$team == current_team, ]
  opp_stats <- past_data[past_data$team == current_opp, ]
  
  if (nrow(team_stats) == 0 || nrow(opp_stats) == 0) {
    print(paste("No data for:", current_team, "or", current_opp))
    next
  }
  
  team_prediction <- mean(predict(logreg_model, newdata = team_stats, type = "response"))
  opp_prediction <- mean(predict(logreg_model, newdata = opp_stats, type = "response"))
  
  total_prob <- team_prediction + opp_prediction
  team_win_prob <- team_prediction / total_prob
  opp_win_prob <- opp_prediction / total_prob
  
  data_schedule$win_prob[game_rows[1]] <- team_win_prob
  data_schedule$win_prob[game_rows[2]] <- opp_win_prob
}

# 1.9 Save Predictions
output_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/Datahub (Temp)/0. Predicition Data/team_predictions"
output_file <- paste0(output_path, "/team_predictions_", formatted_date, ".csv")
write.csv(data_schedule, output_file, row.names = FALSE)


# 1.9 Save Predictions
output_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/Datahub (Temp)/0. Predicition Data/team_predictions"
output_file <- paste0(output_path, "/team_predictions.csv")
write.csv(data_schedule, output_file, row.names = FALSE)

# 1.10 Clean Up Workspace
rm(past_data, team_prediction, data_schedule, testing_data, training_data, opp_prediction, current_team, current_opp, total_prob, predictor_columns, opp_stats, team_stats)

summary(logreg_model)


##### 2. ***END*** Date configuration & library loading logic-------------------------------------




library(dplyr)
library(caret)
library(glmnet)
library(e1071)
library(ggplot2)
library(pROC)
library(broom)
library(parallel)

# Use existing dataframe (BaseStats_Player) instead of loading from CSV
nba_data <- BaseStats_Player

# Dynamically load the correct nba_schedule for the current date
if (exists(paste0("nba_schedule", formatted_date))) {
  nba_schedule <- get(paste0("nba_schedule", formatted_date))
} else {
  stop("nba_schedule for the given date not found. Ensure the correct data is loaded.")
}

# Merge schedule with player stats by matching team and opponent, ensuring unique rows
nba_data <- nba_data %>%
  inner_join(nba_schedule, by = c("team" = "team")) %>%
  distinct(game_id.x, player_name, .keep_all = TRUE)  # Keep only unique player-game rows

# Ensure the `reason` column exists before using mutate
if (!"reason" %in% colnames(nba_data)) {
  nba_data$reason <- NA_character_  # Add it as an empty column if missing
}

# Function to prepare data for a specific stat threshold
prepare_data <- function(stat_col, threshold) {
  df <- nba_data %>% 
    mutate(target = ifelse(.data[[stat_col]] >= threshold, 1, 0)) %>%
    filter(!is.na(target))  # Remove rows with missing values
  
  # Ensure target has both 0 and 1 levels, otherwise skip training
  if (length(unique(df$target)) < 2) {
    return(NULL)
  }
  return(df)
}

# Define the thresholds for each category
thresholds <- list(
  PTS = c(10, 15, 20, 25, 30, 35, 40, 45, 50),
  REB = c(2, 4, 6, 8, 10),
  AST = c(2, 4, 6, 8, 10)
)

# Train logistic regression models for each threshold
models <- list()
for (category in names(thresholds)) {
  for (threshold in thresholds[[category]]) {
    cat("Training model for", category, ">=", threshold, "\n")
    
    # Prepare data
    df <- prepare_data(category, threshold)
    if (is.null(df)) {
      cat("Skipping model for", category, ">=", threshold, "due to insufficient data\n")
      next
    }
    
    # Convert categorical variables to factors only if they have 2+ unique values
    df <- df %>% mutate_if(is.character, as.factor)
    df <- df %>% mutate_if(is.factor, ~ifelse(nlevels(.) > 1, ., as.factor(NA_character_)))
    
    # Remove predictors with only one unique value
    df <- df %>% select_if(~ length(unique(.)) > 1)
    
    # Train-test split
    set.seed(123)
    trainIndex <- createDataPartition(df$target, p = 0.8, list = FALSE)
    trainData <- df[trainIndex, ]
    testData <- df[-trainIndex, ]
    
    # Ensure target column has both 0 and 1 levels in training data
    if (length(unique(trainData$target)) < 2) {
      cat("Skipping model for", category, ">=", threshold, "due to insufficient variation in training data\n")
      next
    }
    
    # Train logistic regression model with regularization
    model <- glmnet(as.matrix(trainData %>% select(-target)), trainData$target, family = "binomial", alpha = 0.5)
    models[[paste(category, threshold, sep = "_")]] <- model
    
    # Evaluate model (optional)
    predictions <- predict(model, as.matrix(testData %>% select(-target)), type = "response")
    auc <- pROC::roc(testData$target, predictions)$auc
    cat("AUC for", category, ">=", threshold, "is", auc, "\n")
  }
}

# Function to predict probabilities for scheduled games
predict_probabilities <- function(player_data) {
  probs <- data.frame(player_name = player_data$player_name)
  for (model_name in names(models)) {
    model <- models[[model_name]]
    probs[[model_name]] <- predict(model, as.matrix(player_data %>% select(-player_name)), type = "response")
  }
  return(probs)
}

# Predict for the current schedule
scheduled_players <- nba_data %>%
  select(player_name, team, opp) %>%
  distinct()

predictions <- predict_probabilities(scheduled_players)

# Save Predictions
output_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/Datahub (Temp)/0. Prediction Data/player_predictions.csv"
write.csv(predictions, output_path, row.names = FALSE)

