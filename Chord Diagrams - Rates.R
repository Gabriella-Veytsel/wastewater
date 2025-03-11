# Load necessary libraries
library(circlize)
library(gridGraphics)
library(grid)
library(gridExtra)
library(dplyr)
library(readr)
library(tidyr)
library(dplyr)
library(ggthemes)
library(tidyverse)

# Color settings for grid.col
grid.col = c(Clarke = "hotpink2", Georgia = "skyblue2", OOS = "gray")

# Load and process data
subsample1_spread <- read_tsv("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample1 - done/dta/spread.txt") %>% mutate(FromTo = paste(FROM, TO, sep = "."))
subsample2_spread <- read_tsv("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample2 - done/dta/spread.txt") %>% mutate(FromTo = paste(FROM, TO, sep = "."))
subsample3_spread <- read_tsv("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample3 - done/dta/spread.txt") %>% mutate(FromTo = paste(FROM, TO, sep = "."))
subsample4_spread <- read_tsv("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample4 - done/dta/spread.txt") %>% mutate(FromTo = paste(FROM, TO, sep = "."))
subsample5_spread <- read_tsv("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample5 - done/dta/spread.txt") %>% mutate(FromTo = paste(FROM, TO, sep = "."))

subsample1_logfile <- read_delim("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample1 - done/dta/combined.location.rates.log") 
subsample2_logfile <- read_delim("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample2 - done/dta/combined.location.rates.log")
subsample3_logfile <- read_delim("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample3 - done/dta/combined.location.rates.log")
subsample4_logfile <- read_delim("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample4 - done/dta/combined.location.rates.log")
subsample5_logfile <- read_delim("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample5 - done/dta/combined.location.rates.log")

# Function to generate a chord diagram and add a title
get_chord_diagram <- function(logfile, subsample_spread, title) {
  actualRates <- logfile %>% select(starts_with("Location.actualRates"))
  locationRates <- logfile %>% select(starts_with("Location.rates"))
  colnames(locationRates) <- gsub("Location.rates.", "" , colnames(locationRates))
  colnames(actualRates) <- colnames(locationRates)
  colnames(actualRates) <- paste("Location.actualRates" ,colnames(actualRates),sep=".") 
  meanCol <- colMeans(actualRates)
  actualRates <- actualRates %>% rbind(meanCol) # The last row is the column mean of actual rates
  actualRatesMean <- actualRates[27004,]
  actualRatesMean <- actualRatesMean %>%
    pivot_longer(
      cols = starts_with("Location.actualRates."),
      names_to = "FromTo",
      names_prefix = "Location.actualRates.",
      values_to = "Rate"
    ) 
  spread <- subsample_spread %>% left_join(actualRatesMean, by = "FromTo")
  spread <- spread %>% filter(`POSTERIOR PROBABILITY` > 0.5 & BAYES_FACTOR > 3)
  spread$Rate <- round(spread$Rate, digits = 3)
  
  spread_chord_df <- spread %>% select(FROM, TO, Rate) %>% as.data.frame()
  
  chord_matrix <- spread_chord_df %>%
    pivot_wider(names_from = TO, values_from = Rate, values_fill = 0) %>%
    column_to_rownames("FROM") %>%
    as.matrix()
  
  # Create the chord diagram
  chord_plot_function <- function() {
    chordDiagram(
      chord_matrix, 
      grid.col = grid.col,
      annotationTrack = c("name", "grid", "axis"),
      transparency = 0.5
    )
  }
  
  gridGraphics::grid.echo(chord_plot_function)
  chord_grob <- grid.grab()
  
  # Add the title to the chord grob
  titled_grob <- gTree(children = gList(
    chord_grob, 
    textGrob(title, x = 0.5, y = 0.95, gp = gpar(fontsize = 16, fontface = "bold"))
  ))
  
  return(titled_grob)
}

# Generate and title all chord diagrams
sub1 <- get_chord_diagram(subsample1_logfile, subsample1_spread, "")
sub2 <- get_chord_diagram(subsample2_logfile, subsample2_spread, "Replicate 2")
sub3 <- get_chord_diagram(subsample3_logfile, subsample3_spread, "Replicate 3")
sub4 <- get_chord_diagram(subsample4_logfile, subsample4_spread, "Replicate 4")
sub5 <- get_chord_diagram(subsample5_logfile, subsample5_spread, "Replicate 5")

#Jumps
sub1jumps <- readRDS("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample1 - done/dta/sub1jumps.rds")
sub1jumpstime <- readRDS("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample1 - done/dta/sub1jumpstime.rds")

plotJumps <- function(data, color) {
  p <- ggplot(data, aes(x = To, y = From, fill = ave)) +
    scale_fill_gradient2(low = 'wheat', mid = 'papayawhip', high = color, 
                         midpoint = mean(data$ave, na.rm = TRUE), limits = c(0, max(data$ave, na.rm = TRUE))) +
    guides(fill = guide_colourbar(title = "Average Markov\nJump Counts")) 
  return(p)
}

sub1jumpplot <- plotJumps(sub1jumps, "skyblue") +
  geom_tile(color="white", size=0.1) +
  coord_equal() +
  labs(x="Sink", y="Source", title="") + #Markov Jump Counts
  theme_tufte(base_family = "Helvetica") +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 0.5), 
        panel.border = element_blank(),
        strip.text = element_text(hjust = 0, size = 10, face = "bold", color = "black"), 
        plot.title = element_text(hjust = 0),
        panel.margin.x = unit(0.5, "cm"),
        panel.margin.y = unit(0.5, "cm"),
        legend.title = element_text(size = 10),
        legend.title.align = 1,
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.key.size = unit(0.2, "cm"),
        legend.key.width = unit(1, "cm")) 

#Rewards
###############################################################################################
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
sub1_rewards <- mean_rewards_df_long %>% filter(Subsample == "Subsample1")
sub1_rewards_plot <- ggplot(sub1_rewards, aes(x = "", y = Proportion, fill = Reward)) +
  geom_bar(stat = "identity", position = "fill") +  # Normalize to 100%
  scale_y_continuous(labels = scales::percent) +    # Format y-axis as percentages
  scale_fill_manual(values = colorset) +
  labs(
    x = "Waiting Time (Rewards)",  # x-axis title remains
    y = "Proportion of Total Time (%)",
    fill = "Location",
    title = " " #Mean SARS-CoV-2 Waiting Times (Rewards)
  ) +
  theme_minimal() +  # Use a clean minimal theme
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank(), # Remove x-axis ticks
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Center and style title
    legend.position = "top"  # Move legend to the top
  )
ggsave("C:/Users/gev25289/Desktop/wastewater/Analysis3/rewards.pdf", height = 4, width = 6)

# Arrange all plots in a grid
# When ggsave removes the plot titles, use this instead
pdf("C:/Users/gev25289/Desktop/wastewater/Analysis3/combined_chord_plots.pdf", width = 10, height = 12)  # Set PDF output file and size
gridExtra::grid.arrange(sub1, sub2, sub3, sub4, sub5, ncol = 2)
dev.off()  # Close the PDF device to save the file

pdf("C:/Users/gev25289/Desktop/wastewater/Analysis3/sub1_chord_plots.pdf", width = 10, height = 12)  # Set PDF output file and size
gridExtra::grid.arrange(sub1)
dev.off()  

pdf("C:/Users/gev25289/Desktop/wastewater/Analysis3/sub1_plots.pdf", width = 13, height = 5)  # Set PDF output file and size
gridExtra::grid.arrange(sub1, sub1_rewards_plot, sub1jumpplot, ncol=3)
dev.off() 
