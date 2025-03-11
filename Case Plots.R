library(tidyverse)
library(phylotools)
library(lubridate)
library(ggtree)

options(scipen = 999)

colorset <- c(Clarke = "hotpink2", 'Clarke - WW' = "red", Georgia = "skyblue2", OOS = "gray")
fasta <- read.fasta("C:/Users/gev25289/Desktop/wastewater/Analysis3/fasta/subsample1_output_mismatch4_trim.fasta")

ww_fasta <- read.fasta("C:/Users/gev25289/Desktop/wastewater/Analysis2/WW - done/output_mismatch4_trim.fasta")
ww_fasta$seq.name <- str_replace(ww_fasta$seq.name, "^(.*?)(?=\\1/)", "")
ww_fasta$variant <- str_extract(ww_fasta$seq.name, "(?<=_)[^/]+")
ww_fasta$date <- str_extract(ww_fasta$seq.name, "[^/]+$")

clinical_fasta <- read.fasta("C:/Users/gev25289/Desktop/wastewater/Analysis2/Clinical - done/gisaid_clarke_632.fasta")
clinical_fasta$variant <- str_extract(clinical_fasta$seq.name, "[^|]+$")
clinical_fasta$date <- str_extract(clinical_fasta$seq.name, "(?<=\\|)[^|]+")

#Analysis 3
fasta$location <- str_extract(fasta$seq.name, "(?<=\\|)[^|]+")
# fasta <- fasta %>%
#   mutate(Sample = ifelse(grepl("^W", seq.name), "Wastewater", "Clinical"))
fasta <- fasta %>%
  mutate(location = ifelse(grepl("^W", seq.name), "Clarke - WW", location))
fasta$date <- str_extract(fasta$seq.name,str_extract(fasta$seq.name, "[^|]+$"))
fasta$date <- as.Date(fasta$date)

ggplot(fasta, aes(x=date, fill=location)) +
  geom_histogram(color = "black", position="stack", alpha = 0.9, binwidth=15) +
  scale_fill_manual(values = colorset)

ggplot(fasta, aes(x=date, fill=Sample)) +
  geom_histogram(color = "black", position="stack") +
  scale_fill_manual(values = c("blue", "orange"))

ggplot(fasta, aes(x = date, fill = location)) +
  geom_histogram(color = "black", position = "stack") +
  scale_fill_manual(values = colorset) +
  facet_wrap(~ Sample, ncol = 1)

#case data
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)

# Read and filter data
outbreak <- read_csv("C:/Users/gev25289/Desktop/xps/georgia/raw data/ga_covid_data/epicurve_rpt_date.csv") %>% 
  filter(measure == "county_stats") %>%
  select(county, report_date, total_cases) %>%
  mutate(report_date = as.Date(report_date, "%Y-%m-%d")) %>%
  filter(county == "Clarke") %>%
  filter(report_date >= as.Date("2021-04-01"), report_date <= as.Date("2022-12-31"))

# Aggregate by epidemiological week (starting Sunday)
outbreak_ag <- outbreak %>%
  mutate(week = floor_date(report_date, "week", week_start = 7)) %>%
  group_by(week) %>%
  summarize(cases = sum(total_cases), .groups = "drop")

# Define the first set of highlight periods (Alpha, Delta, Omicron)
highlight_periods <- data.frame(
  xmin = as.Date(c("2021-03-07", "2021-06-27", "2021-12-12")),
  xmax = as.Date(c("2021-06-19", "2021-12-11", "2022-12-31")),
  fill = c("lightblue", "pink", "yellow"),
  label = c("Alpha", "Delta", "Omicron")
)

# Define the second set of highlight periods (custom periods)
highlight_periods_2 <- data.frame(
  xmin = as.Date(c("2019-12-05", "2020-03-09", "2020-04-29", "2020-12-10", "2021-05-04", "2021-12-8", "2022-03-07", "2022-05-04")),
  xmax = as.Date(c("2020-01-7", "2020-03-15", "2020-08-20", "2021-01-13", "2021-08-18", "2022-01-10", "2022-03-13", "2022-08-17")),
  fill = c("cadetblue1", "plum3", "sandybrown", "cadetblue1", "sandybrown", "cadetblue1", "plum3", "sandybrown"),
  label = c("Winter", "Spring", "Summer", 
            "Winter", "Summer", 
            "Winter", "Spring", "Summer")
)

# Calculate the midpoint using numeric representation of dates
highlight_periods$midpoint <- as.numeric(highlight_periods$xmin + (highlight_periods$xmax - highlight_periods$xmin) / 2)
highlight_periods_2$midpoint <- as.numeric(highlight_periods_2$xmin + (highlight_periods_2$xmax - highlight_periods_2$xmin) / 2)

#Georgia
#################################################################################################
# Create the plot
ggplot(outbreak_ag, aes(y = cases, x = week)) + 
  # Highlight the first set of periods (Alpha, Delta, Omicron)
  geom_rect(data = highlight_periods, aes(xmin = xmin, xmax = xmax, 
                                          ymin = -Inf, ymax = Inf, fill = fill), 
            alpha = 0.3, inherit.aes = FALSE, color = "black", size = 0.5, linetype = "solid") + 
  
  # Bar plot
  geom_bar(stat = "identity", fill = "darkgray", color = "black", width = 6) +  
  scale_fill_identity() +  
  
  # Add bold and larger labels for Alpha, Delta, Omicron
  geom_text(data = highlight_periods, 
            aes(x = as.Date(midpoint, origin = "1970-01-01"),  # Convert midpoint back to Date
                y = max(outbreak_ag$cases, na.rm = TRUE) * 1.1,  # Place text above the bars
                label = label), 
            inherit.aes = FALSE, size = 5, fontface = "bold", color = "black", hjust = 0.5, vjust = 0) + 
  
  theme_minimal(base_size = 14) +  
  labs(x = "Week", y = "Total Cases", title = "Weekly COVID-19 Cases in Georgia") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
    breaks = seq(0, max(outbreak_ag$cases, na.rm = TRUE), by = 20000), #for GA, 20,000
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_date(date_breaks = "3 weeks", date_labels = "%Y-%m-%d",
               limits = c(as.Date("2019-12-15"), as.Date("2023-01-10")),  # Use Date scale for x-axis limits
               expand = expansion(mult = c(0, 0)))    # Use Date scale for x-axis
  
ggsave("C:/Users/gev25289/Desktop/wastewater/epicurve-covid19-cases.pdf", height = 8, width = 14)

# Create the plot
# Define the first set of highlight periods (Alpha, Delta, Omicron)
highlight_periods <- data.frame(
  xmin = as.Date(c("2021-03-07", "2021-06-27", "2021-12-12")),
  xmax = as.Date(c("2021-06-19", "2021-12-11", "2022-12-31")),
  fill = c("lightblue", "pink", "yellow"),
  label = c("Alpha", "Delta", "Omicron")
)

# Define the second set of highlight periods (custom periods)
highlight_periods_2 <- data.frame(
  xmin = as.Date(c("2019-12-05", "2020-03-09", "2020-04-29", "2020-12-10", "2021-05-04", "2021-12-8", "2022-03-07", "2022-05-04")),
  xmax = as.Date(c("2020-01-7", "2020-03-15", "2020-08-20", "2021-01-13", "2021-08-18", "2022-01-10", "2022-03-13", "2022-08-17")),
  fill = c("cadetblue1", "plum3", "sandybrown", "cadetblue1", "sandybrown", "cadetblue1", "plum3", "sandybrown"),
  label = c("Winter", "Spring", "Summer", 
            "Winter", "Summer", 
            "Winter", "Spring", "Summer")
)

# Calculate the midpoint using numeric representation of dates
highlight_periods$midpoint <- as.numeric(highlight_periods$xmin + (highlight_periods$xmax - highlight_periods$xmin) / 2)
highlight_periods_2$midpoint <- as.numeric(highlight_periods_2$xmin + (highlight_periods_2$xmax - highlight_periods_2$xmin) / 2)

# Create the plot
ggplot(outbreak_ag, aes(y = cases, x = week)) + 
  # Highlight the first set of periods (Alpha, Delta, Omicron) with solid lines
  geom_rect(data = highlight_periods, aes(xmin = xmin, xmax = xmax, 
                                          ymin = -Inf, ymax = Inf, fill = fill), 
            alpha = 0.3, inherit.aes = FALSE, color = "black", size = 0.5, linetype = "solid") + 
  
  # Highlight the second set of periods with dashed lines
  geom_rect(data = highlight_periods_2, aes(xmin = xmin, xmax = xmax, 
                                            ymin = -Inf, ymax = 1900, fill = fill), 
            alpha = 0.7, inherit.aes = FALSE, color = "black", size = 0.5, linetype = "dashed") +  
  # Bar plot
  geom_bar(stat = "identity", fill = "darkgray", color = "black", width = 6) +  
  scale_fill_identity() +  
  
  # Add bold and larger labels for Alpha, Delta, Omicron
  geom_text(data = highlight_periods, 
            aes(x = as.Date(midpoint, origin = "1970-01-01"),  # Convert midpoint back to Date
                y = max(outbreak_ag$cases, na.rm = TRUE) * 1.1,  # Place text above the bars
                label = label), 
            inherit.aes = FALSE, size = 5, fontface = "bold", color = "black", hjust = 0.5, vjust = 0) + 
  
  # Add labels for the second set of periods (adjust placement slightly)
  geom_text(data = highlight_periods_2, 
            aes(x = as.Date(midpoint, origin = "1970-01-01"),  # Convert midpoint back to Date
                y = max(outbreak_ag$cases, na.rm = TRUE) * 1.1,  # Place text above the bars
                label = label), 
            inherit.aes = FALSE, size = 4, color = "black", hjust = 0.5, vjust = 2.2) + 
  
  theme_minimal(base_size = 14) +  
  labs(x = "Week", y = "Total Cases", title = "Weekly COVID-19 Cases in Clarke County, GA") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
    breaks = seq(0, max(outbreak_ag$cases, na.rm = TRUE), by = 500),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_date(date_breaks = "3 weeks", date_labels = "%Y-%m-%d",
               limits = c(as.Date("2021-03-01"), as.Date("2023-01-10")),  # Use Date scale for x-axis limits
               expand = expansion(mult = c(0, 0)))    # Use Date scale for x-axis

ggsave("C:/Users/gev25289/Desktop/wastewater/epicurve-covid19-cases-clarke.pdf", height = 8, width = 14)
