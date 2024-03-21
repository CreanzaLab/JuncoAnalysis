## Load packages
require("pacman")
p_load(tidymodels,vip, rpart.plot, dplyr, themis, gridExtra, cowplot)
tidymodels_prefer() # Resolve conflicts between tidymodels packages

## Load log-transformed song analysis data
junco_dat_all <- read.csv("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_JuncoSongData_Subspecies_and_ID.csv")

## Define subspecies pair to go through classifier; uncomment to select pair:
#filterout <- c("migratory","caniceps","dorsalis","mearnsi","oreganus","unknown") #compares hyemalis and aikeni
#filterout <- c("migratory","oreganus","dorsalis","mearnsi","aikeni","unknown") #compares hyemalis and caniceps
#filterout <- c("migratory","caniceps","oreganus","mearnsi","aikeni","unknown") #compares hyemalis and dorsalis
#filterout <- c("migratory","caniceps","dorsalis","oreganus","aikeni","unknown") #compares hyemalis and mearnsi
#filterout <- c("migratory","caniceps","dorsalis","mearnsi","aikeni","unknown") #compares hyemalis and oreganus
#filterout <- c("migratory","hyemalis","dorsalis","mearnsi","oreganus","unknown") #compares caniceps and aikeni
#filterout <- c("migratory","hyemalis","oreganus","mearnsi","aikeni","unknown") #compares caniceps and dorsalis
#filterout <- c("migratory","hyemalis","dorsalis","oreganus","aikeni","unknown") #compares caniceps and mearnsi
#filterout <- c("migratory","hyemalis","dorsalis","mearnsi","aikeni","unknown") #compares caniceps and oreganus
#filterout <- c("migratory","caniceps","hyemalis","mearnsi","oreganus","unknown") #compares dorsalis and aikeni
#filterout <- c("migratory","caniceps","hyemalis","oreganus","aikeni","unknown") #compares dorsalis and mearnsi
#filterout <- c("migratory","caniceps","hyemalis","mearnsi","aikeni","unknown") #compares dorsalis and oreganus
#filterout <- c("migratory","caniceps","dorsalis","hyemalis","oreganus","unknown") #compares mearnsi and aikeni
#filterout <- c("migratory","caniceps","dorsalis","hyemalis","aikeni","unknown") #compares mearnsi and oreganus
#filterout <- c("migratory","caniceps","dorsalis","mearnsi","hyemalis","unknown") #compares oreganus and aikeni
#filterout <- c("migratory","unknown") #compares all 6 subspecies

## Create object with only data from the subspecies pair being tested
junco_dat <- na.omit(junco_dat_all[!junco_dat_all$subspecies %in% filterout,])

## Table with number of songs available for each subspecies
table(junco_dat$subspecies) 

## Set seed
set.seed(123)

## Split the data, training on 75% of the data and testing on 25% of the data
deju_split <- initial_split(junco_dat, strata = subspecies)
deju_train <- training(deju_split)
deju_test <- testing(deju_split)

## Downsample the training set to the size of the smaller class
deju_recipe <- recipe(subspecies ~ ., data = deju_train) %>%
  update_role(ID_Num, new_role = "ID") %>% 
  step_downsample(subspecies, under_ratio = 1)

## Return the results of the recipe created in the previous step
prepped <- prep(deju_recipe)
juiced <- juice(prepped) 

## Table with the downsampled training set sizes
table(juiced$subspecies)

## Random forest model using 100 decision trees
tune_spec <- rand_forest(
  mtry = tune(),
  trees = 100,
  min_n = tune()
) %>% 
  set_mode("classification") %>% 
  set_engine("ranger")

## Convenience container for carrying around bits of the model
tune_wf <- workflow() %>%
  add_model(tune_spec) %>% 
  add_recipe(deju_recipe) 

## Set seed and V-fold cross validation
set.seed(234)
deju_folds <- vfold_cv(deju_train)

doParallel::registerDoParallel()

## Set seed and set performance metrics for a pre-defined set of tuning
set.seed(345)
tune_res <- tune_grid(
  tune_wf,
  resamples = deju_folds,
  grid = 10
)

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")


## Fine tune grid to capture the data range
rf_grid <- grid_regular(
  mtry(range = c(2, 10)), # For each pair, manually set range that captures best mtry points. # Number of variables sampled at each split
  min_n(range = c(5,40)), # For each pair, manually set range that captures best min_n points
  levels = 5 # Randomly chooses points on grid and does combinations for the min_n and mtry with highest accuracy
)
rf_grid # Rows in rf_grid is levels^2

## Set seed and tune rf_grid
set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = deju_folds,
  grid = rf_grid
)

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, linewidth = 1.5) +
  geom_point() +
  labs(y = "AUC")

## Select line with the highest AUC
best_auc <- select_best(regular_res, "roc_auc")

## Specify the final model, with the original recipe and best hyperparameters 
final_rf <- finalize_model( 
  tune_spec,
  best_auc
)
final_rf
deju_prep <- prep(deju_recipe)

## VIP plot -- which song features have the most predictive power?
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(subspecies ~ .,
      data = juice(deju_prep) %>% select(-ID_Num)
  ) %>%
  vip(geom = "point")

final_wf <- workflow() %>%
  add_recipe(deju_recipe) %>%
  add_model(final_rf)

final_fit <- final_wf %>%
  last_fit(deju_split)

final_fit %>%
  collect_metrics()

## Prediction accuracy plot
plotdata <- final_fit %>%
  collect_predictions() %>%
  mutate(correct = case_when(
    subspecies == .pred_class ~ "Correct",
    TRUE ~ "Incorrect"
  )) %>%
  bind_cols(deju_test)

final_fit %>%
  collect_predictions() %>%
  mutate(correct = case_when(
    subspecies == .pred_class ~ "Correct",
    TRUE ~ "Incorrect"
  )) %>%
  bind_cols(deju_test) %>%
  ggplot(aes(avg_sylls_lower_freq.Hz., avg_sylls_upper_freq.Hz., color = correct)) + # For each subspecies pair, replace the x and y plotted to be the top 2 variables of importance
  geom_point(size = 0.5, alpha = 0.85) +
  labs(color = NULL) +
  scale_color_manual(values = c("gray80", "darkred"))

final_tree <- extract_workflow(final_fit)
final_tree

subspp_names <- as.character(unique(plotdata$subspecies...20)) # may need to change the subspecies... to match the column name in `plotdata`, as it can change
subspp_accu <- rep(0, length(subspp_names))
subspp_prop <- rep(0, length(subspp_names))
for (i in 1:length(subspp_names)){
  cur_name <- subspp_names[i]
  pct_correct <- nrow(plotdata[plotdata$subspecies...20==cur_name&plotdata$correct == "Correct",])/nrow(plotdata[plotdata$subspecies...20==cur_name,])
  pct_prop <- nrow(plotdata[plotdata$subspecies...20==cur_name,])/nrow(plotdata)
  subspp_accu[i] <- pct_correct
  subspp_prop[i] <- pct_prop
}

## Display classifier results
results_big2 <- as_tibble(cbind(subspp_names,subspp_prop,subspp_accu))

colnames(results_big2) <- c("Subsp Name","Proportion of Total","Accuracy")
results_big2$`Proportion of Total` <- round(as.numeric(results_big2$`Proportion of Total`),4)
results_big2$`Accuracy` <- round(as.numeric(results_big2$`Accuracy`),4)
results_big2

## Training set size vs. classifier accuracy for the 15 subspecies pairs
classifier_trainingsize_accuracy <- read.csv("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_MLclassifier_trainingsizes_and_accuracies.csv")

ggplot(classifier_trainingsize_accuracy, aes(Training.set.size.after.downsampling, Average.classifier.accuracy)) + 
  geom_point(shape=19, size=8) +
  labs(x = "Training set size after downsampling", 
       y = "Average classifier accuracy") +
  geom_smooth(method = "lm", se = F) +
  theme_bw()

## For hybrid analysis, boxplot of ML accuracies for the 3 different groups
require(tidyverse)
boxplot_colors <- c("Non-adjacent" = "#bebada", "Hybridizing" = "#8dd3c7", "Adjacent non-hybridizing" = "#ffffb3")
ggplot(classifier_trainingsize_accuracy, aes(x = Group, y = Average.classifier.accuracy, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = boxplot_colors) +
  labs(x = "Hybrid Status",
       y = "Average classifier accuracy",
       fill = "Hybrid Status") +
  theme_bw()
