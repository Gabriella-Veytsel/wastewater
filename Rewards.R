library(tidyverse)

subsample1_logfile <- read_delim("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample1 - done/dta/combined.log.txt") 
subsample2_logfile <- read_delim("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample2 - done/dta/combined.log.txt")
subsample3_logfile <- read_delim("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample3 - done/dta/combined.log.txt")
subsample4_logfile <- read_delim("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample4 - done/dta/combined.log.txt")
subsample5_logfile <- read_delim("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample5 - done/dta/combined.log.txt")

reward_cols <- names(subsample1_logfile)[grepl("^c_", names(subsample1_logfile)) & !grepl("all|count", names(subsample1_logfile))]

# Select reward and treelength columns
subsample1_reward_columns <- subsample1_logfile %>% select(all_of(reward_cols), treeLength)
subsample2_reward_columns <- subsample2_logfile %>% select(all_of(reward_cols), treeLength)
subsample3_reward_columns <- subsample3_logfile %>% select(all_of(reward_cols), treeLength)
subsample4_reward_columns <- subsample4_logfile %>% select(all_of(reward_cols), treeLength)
subsample5_reward_columns <- subsample5_logfile %>% select(all_of(reward_cols), treeLength)

# Compute the mean rewards for each subsample
mean_reward_subsample1 <- colMeans(subsample1_reward_columns, na.rm = TRUE)
mean_reward_subsample2 <- colMeans(subsample2_reward_columns, na.rm = TRUE)
mean_reward_subsample3 <- colMeans(subsample3_reward_columns, na.rm = TRUE)
mean_reward_subsample4 <- colMeans(subsample4_reward_columns, na.rm = TRUE)
mean_reward_subsample5 <- colMeans(subsample5_reward_columns, na.rm = TRUE)

# Remove "c_" and "_reward[1]" from the reward names
cleaned_names <- gsub("c_", "", names(mean_reward_subsample1))
cleaned_names <- gsub("_reward\\[1\\]", "", cleaned_names)

# Get treeLength values for each subsample
treeLength_subsample1 <- mean(subsample1_reward_columns$treeLength, na.rm = TRUE)
treeLength_subsample2 <- mean(subsample2_reward_columns$treeLength, na.rm = TRUE)
treeLength_subsample3 <- mean(subsample3_reward_columns$treeLength, na.rm = TRUE)
treeLength_subsample4 <- mean(subsample4_reward_columns$treeLength, na.rm = TRUE)
treeLength_subsample5 <- mean(subsample5_reward_columns$treeLength, na.rm = TRUE)

# Combine the results into a data frame
mean_rewards_df <- tibble(
  Reward = cleaned_names,
  Subsample1 = mean_reward_subsample1,
  Subsample2 = mean_reward_subsample2,
  Subsample3 = mean_reward_subsample3,
  Subsample4 = mean_reward_subsample4,
  Subsample5 = mean_reward_subsample5,
)

mean_rewards_df <- mean_rewards_df[1:3,]

# Calculate the Proportion and remove the TreeLength columns
mean_rewards_df_long <- mean_rewards_df %>%
  pivot_longer(cols = starts_with("Subsample"),
               names_to = "Subsample",
               values_to = "Mean_Reward") 

mean_rewards_df_long <- mean_rewards_df_long %>%
  mutate(Proportion = case_when(
    Subsample == "Subsample1" ~ Mean_Reward / treeLength_subsample1 * 100,
    Subsample == "Subsample2" ~ Mean_Reward / treeLength_subsample2 * 100,
    Subsample == "Subsample3" ~ Mean_Reward / treeLength_subsample3 * 100,
    Subsample == "Subsample4" ~ Mean_Reward / treeLength_subsample4 * 100,
    Subsample == "Subsample5" ~ Mean_Reward / treeLength_subsample5 * 100
  ))

colorset <- c(Clarke = "hotpink2", Georgia = "skyblue2", OOS = "gray")
ggplot(mean_rewards_df_long, aes(x = Subsample, y = Proportion, fill = Reward)) +
  geom_bar(stat = "identity", position = "fill") +  # Normalize to 100%
  scale_y_continuous(labels = scales::percent) +    # Format y-axis as percentages
  scale_fill_manual(values = colorset) +
  labs(
    x = "",
    y = "Proportion of the Total Time (%)",
    fill = "Location",
    title = "Mean SARS-CoV-2 Waiting Times (Rewards)"
  ) +
  theme_minimal() +  # Use a clean minimal theme
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = ),  # Center and style title
    legend.position = "top"  # Move legend to the top
  )
ggsave("C:/Users/gev25289/Desktop/wastewater/Analysis3/rewards.pdf", height = 4, width = 6)
