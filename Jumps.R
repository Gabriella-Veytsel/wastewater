library(ggthemes)
library(ggplot2)
sub1jumps <- readRDS("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample1 - done/dta/sub1jumps.rds")
sub1jumpstime <- readRDS("C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample1 - done/dta/sub1jumpstime.rds")

plotJumps <- function(data, color) {
  p <- ggplot(data, aes(x = To, y = From, fill = ave)) +
    scale_fill_gradient2(low = 'wheat', mid = 'papayawhip', high = color, 
                         midpoint = mean(data$ave, na.rm = TRUE), limits = c(0, max(data$ave, na.rm = TRUE))) +
    guides(fill = guide_colourbar(title = "")) #Average Markov\nJump Counts"
  return(p)
}

sub1jumpplot <- plotJumps(sub1jumps, "skyblue") +
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

# Time
################################################################################################
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
sub1timeplot <- create_plot(sub1jumpstime, "Subsample 1") 
