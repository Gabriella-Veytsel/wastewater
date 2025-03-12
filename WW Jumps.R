library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggthemes)

subsample1 <- "/scratch/gev25289/workdir/wastewater/analysis3/beast_subsample/subsample1/dta/jumptimes.txt"
subsample2 <- "/scratch/gev25289/workdir/wastewater/analysis3/beast_subsample/subsample2/dta/jumptimes.txt"
subsample3 <- "/scratch/gev25289/workdir/wastewater/analysis3/beast_subsample/subsample3/dta/jumptimes.txt"
subsample4 <- "/scratch/gev25289/workdir/wastewater/analysis3/beast_subsample/subsample4/dta/jumptimes.txt"
subsample5 <- "/scratch/gev25289/workdir/wastewater/analysis3/beast_subsample/subsample5/dta/jumptimes.txt"

totalJumps <- function(input_dir, mrsd, subsample) {
  # Attempt to read the file and handle errors
  jumps <- tryCatch(
    read.table(input_dir, header = TRUE),
    error = function(e) {
      message(sprintf("Error reading file for %s: %s", subsample, e))
      return(NULL)  # Return NULL if an error occurs
    }
  )

  # If reading the file failed, return NULL
  if (is.null(jumps)) return(NULL)

  jumps <- read.table(input_dir, header = TRUE)

  state <- n_distinct(jumps$state) #Number of total state counts
  jumps$from_to = paste(jumps$from,jumps$to, sep=".")
  jumps <- jumps %>% mutate(time = mrsd - as.numeric(time))
  jumps$year <- format(date_decimal(jumps$time), "%Y-%m")

  #From, To
  count_total <- jumps %>% group_by(from_to) %>% count()
  count_total <- count_total %>% separate_wider_delim(from_to, ".", names = c("From", "To"))
  count_total <- count_total %>% mutate(ave=n/state) %>% mutate(Subsample = subsample)
  return(count_total)
}

sub1jumps <- totalJumps(subsample1, 2022.676712328767, "Subsample 1")
sub2jumps <- totalJumps(subsample2, 2022.676712328767, "Subsample 2")
sub3jumps <- totalJumps(subsample3, 2022.676712328767, "Subsample 3")
sub4jumps <- totalJumps(subsample4, 2022.676712328767, "Subsample 4")
sub5jumps <- totalJumps(subsample5, 2022.676712328767, "Subsample 5")

saveRDS(sub1jumps, "/scratch/gev25289/workdir/wastewater/analysis3/beast_subsample/subsample1/dta/sub1jumps.rds")

subjumps <- bind_rows(sub1jumps, sub2jumps, sub3jumps, sub4jumps, sub5jumps)

plotJumps <- function(data, color) {
  p <- ggplot(data, aes(x = To, y = From, fill = ave)) +
    scale_fill_gradient2(low = 'wheat', mid = 'papayawhip', high = color, 
                         midpoint = mean(data$ave, na.rm = TRUE), limits = c(0, max(data$ave, na.rm = TRUE))) +
    guides(fill = guide_colourbar(title = "Average Markov\nJump Counts")) 
  return(p)
}

plotJumps(subjumps, "skyblue") +
  facet_wrap(~ Subsample, nrow = 4) +
  geom_tile(color="white", size=0.1) +
  coord_equal() +
  labs(x="Sink", y="Source", title="Markov Jump Counts") +
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
ggsave("/scratch/gev25289/workdir/wastewater/jumps.pdf", width = 4, height=6)

###############################################################################################################

JumpsTime <- function(input_dir, mrsd, subsample) {
  # Attempt to read the file and handle errors
  jumps <- tryCatch(
    read.table(input_dir, header = TRUE),
    error = function(e) {
      message(sprintf("Error reading file for %s: %s", subsample, e))
      return(NULL)  # Return NULL if an error occurs
    }
  )
  
  # If reading the file failed, return NULL
  if (is.null(jumps)) return(NULL)
  
  jumps <- read.table(input_dir, header = TRUE)
  
  state <- n_distinct(jumps$state) #Number of total state counts
  jumps$from_to = paste(jumps$from,jumps$to, sep=".")
  jumps <- jumps %>% mutate(time = mrsd - as.numeric(time))
  jumps$year <- format(date_decimal(jumps$time), "%Y-%m")
  
  #From, To
  count <- jumps %>% group_by(from_to, year) %>% count()
  count2 <- cbind(count, read.table(text = as.character(count$from_to), sep = ".")) #Forgot to do "From" and "To", so it's V1, V2
  count2 <- count2 %>% mutate(ave=n/state) %>% mutate(Subsample = subsample)
  return(count2)
}

sub1jumpstime <- JumpsTime(subsample1, 2022.676712328767, "Subsample 1")
sub2jumpstime <- JumpsTime(subsample2, 2022.676712328767, "Subsample 2")
sub3jumpstime <- JumpsTime(subsample3, 2022.676712328767, "Subsample 3")
sub4jumpstime <- JumpsTime(subsample4, 2022.676712328767, "Subsample 4")
sub5jumpstime <- JumpsTime(subsample5, 2022.676712328767, "Subsample 5")

saveRDS(sub1jumpstime, "/scratch/gev25289/workdir/wastewater/analysis3/beast_subsample/subsample1/dta/sub1jumpstime.rds")

plotJumpsTime <- function(data) {
  p <- ggplot(data, aes(x = year, y = V1, fill = ave)) +
    scale_fill_gradient2(low = 'darkseagreen1', mid = 'papayawhip', high = 'seagreen3', 
                         midpoint = mean(data$ave, na.rm = TRUE), limits = c(0, max(data$ave, na.rm = TRUE)),
                         labels = scales::label_number(accuracy = 1)) +
    guides(fill = guide_colourbar(title = "Average Markov\nJump Counts")) +
    labs(x="Month", y="Source", title = "Markov Jump Counts over Time") 
  return(p)
}

# Define a common theme
common_theme <- theme_tufte(base_family = "Helvetica") +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 5), 
    axis.ticks = element_blank(),
    axis.text = element_text(size = 4),
    panel.border = element_blank(),
    plot.title = element_text(size = 5, face = "bold", hjust = 0.5, vjust = -8),
    strip.text = element_text(size = 5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right", # Move legend to the right
    legend.key.size = unit(0.2, "cm"),  # Reduce the legend key size
    legend.text = element_text(size = 4),  # Reduce the text size of the legend
    legend.title = element_text(face = "bold", size = 4),  # Adjust legend title size
    panel.spacing = unit(0.1, "cm"),
    strip.background = element_rect(fill = "white", color = "black"),
    plot.margin = margin(0, 0, 0, 0, "cm")) 

# Function to generate each plot
create_plot <- function(data, title) {
  plotJumpsTime(data) +
    facet_wrap(~V2) +
    geom_tile(color="white", size=0.1, width = 1.7, height = 1.7) +  # Uniform size for tiles
    coord_equal() +
    ggtitle(title) +
    common_theme
}

# Create plots
sub1time <- create_plot(sub1jumpstime, "Subsample 1") 
sub2time <- create_plot(sub2jumpstime, "Subsample 2") 
sub3time <- create_plot(sub3jumpstime, "Subsample 3") 
sub4time <- create_plot(sub4jumpstime, "Subsample 4") 
sub5time <- create_plot(sub5jumpstime, "Subsample 5") 

# Adjust axis text for sub5time as needed
sub5time <- sub5time + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
  axis.title.x = element_text(size = 5))

# Combine plots in a grid
plot_grid(sub1time, sub2time, sub3time, sub4time, sub5time, 
          ncol = 1, # Arrange plots in a single column
          rel_widths = c(1), 
          rel_heights = c(1),
          align = 'v')
library(patchwork)
combined_plot <- sub1time + sub2time + sub3time + sub4time + sub5time + 
  plot_layout(ncol = 1) 
ggsave("/scratch/gev25289/workdir/wastewater/jump_time.pdf", width = 7, height=4.5)
