library(tidyverse)
library(ggplot2)
library(ggridges)
library(cowplot)
library(lubridate)
library(HDInterval)

#WW
ww <- read.table("C:/Users/gev25289/Desktop/wastewater/Analysis2/WW - done/skyride 0.001/analysis2.ww.combined_resample10k.log", header=TRUE) %>% 
  select(starts_with("age")) %>% mutate(Sample = "Wastewater") %>%
  select(-"age.Omicron.")

ww_ba <- read_tsv("C:/Users/gev25289/Desktop/wastewater/Analysis2/WW - done/skyride 0.001/omicron_tmrca.tsv")
ww_ba <- ww_ba %>%
  mutate(youngesttip = 2022.676712328767) %>%
  mutate(age.BA5 = youngesttip - `tMRCA(BA5)`) %>%
  mutate(age.BA2 = youngesttip - `tMRCA(BA2)`) %>%
  mutate(age.BA1 = youngesttip - `tMRCA(BA1)`) %>%
  select(state, age.BA5, age.BA2, age.BA1)

ww <- bind_cols(ww, ww_ba)

#Clinical
clinical <- read.table("C:/Users/gev25289/Desktop/wastewater/Analysis2/Clinical - done/skyride/analysis2.clinical.combined_resample10k.log", header=TRUE) %>% 
  select(starts_with("age")) %>% mutate(Sample = "Clinical") %>%
  select(-"age.Omicron.")
clinical_ba <- read_tsv("C:/Users/gev25289/Desktop/wastewater/Analysis2/Clinical - done/skyride/omicron_tmrca.tsv")

clinical_ba <- clinical_ba %>%
  mutate(youngesttip = 2022.6712328767123) %>%
  mutate(age.BA5 = youngesttip - `tMRCA(BA.5)`) %>%
  mutate(age.BA2 = youngesttip - `tMRCA(BA.2)`) %>%
  mutate(age.BA1 = youngesttip - `tMRCA(BA.1)`) %>%
  select(state, age.BA5, age.BA2, age.BA1)

clinical <- bind_cols(clinical, clinical_ba)

#Combined
combined <- read.table("C:/Users/gev25289/Desktop/wastewater/Analysis1/analysis1.combined_resample10k.log", header=TRUE) %>% 
  select(starts_with("age")) %>% mutate(Sample = "Combined") %>%
  select(-"age.Omicron.")

combined_ba <- read_tsv("C:/Users/gev25289/Desktop/wastewater/Analysis1/omicron_tmrca.tsv")
combined_ba <- combined_ba %>%
  mutate(youngesttip = 2022.676712328767) %>%
  mutate(age.BA5 = youngesttip - `tMRCA(BA5)`) %>%
  mutate(age.BA2 = youngesttip - `tMRCA(BA2)`) %>%
  mutate(age.BA1 = youngesttip - `tMRCA(BA1)`) %>%
  select(state, age.BA5, age.BA2, age.BA1)
combined <- bind_cols(combined, combined_ba)

data <- bind_rows(ww, clinical, combined)

# Reshape the data to long format for ggplot
data_long <- data %>%
  pivot_longer(cols = starts_with("age."),
               names_to = "Variant",
               values_to = "Age") %>%
  mutate(Variant = case_when(
    Variant == "age.Alpha." ~ "Alpha",
    Variant == "age.Delta." ~ "Delta",
    Variant == "age.BA1" ~ "Omicron:BA.1",
    Variant == "age.BA2" ~ "Omicron:BA.2",
    Variant == "age.BA5" ~ "Omicron:BA.5",
    Variant == "age.root." ~ "root"
  ))

data_long_alpha <- data_long %>% filter(Variant == "Alpha")
data_long_delta <- data_long %>% filter(Variant == "Delta")
data_long_ba1 <- data_long %>% filter(Variant == "Omicron:BA.1")
data_long_ba2 <- data_long %>% filter(Variant == "Omicron:BA.2")
data_long_ba5 <- data_long %>% filter(Variant == "Omicron:BA.5")

data_long_alpha$Age <- as.Date(date_decimal(data_long_alpha$Age))
data_long_delta$Age <- as.Date(date_decimal(data_long_delta$Age))
data_long_ba1$Age <- as.Date(date_decimal(data_long_ba1$Age))
data_long_ba2$Age <- as.Date(date_decimal(data_long_ba2$Age))
data_long_ba5$Age <- as.Date(date_decimal(data_long_ba5$Age))

class(data_long_alpha$Age)

plot_alpha <- ggplot(data_long_alpha, aes(x = Age, y = Sample)) +
  geom_density_ridges(fill = "salmon", alpha = 0.75) + 
  theme_minimal() + 
  ggtitle("Alpha") +
  labs(x = NULL, y = " ") +
  scale_x_date(limits = as.Date(c("2019-03-09","2022-04-26")), date_labels = "%Y-%m-%d") +
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(), #removes x-axis labels
    plot.margin = margin(10, 10, 10, 10)
  )

plot_delta <- ggplot(data_long_delta, aes(x = Age, y = Sample)) +
  geom_density_ridges(fill = "plum1", alpha = 0.75) + 
  theme_minimal() + 
  ggtitle("Delta") +
  labs(x = NULL, y = " ") +
  scale_x_date(limits = as.Date(c("2019-03-09", "2022-04-26")), date_labels = "%Y-%m-%d") +
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(), #removes x-axis labels
    plot.margin = margin(10, 10, 10, 10)
  )

plot_ba1 <- ggplot(data_long_ba1, aes(x = Age, y = Sample)) +
  geom_density_ridges(fill = "skyblue", alpha = 0.75) + 
  theme_minimal() + 
  labs(x = NULL, y = "Sample Source") +
  ggtitle("Omicron: BA.1") +
  scale_x_date(limits = as.Date(c("2019-03-09", "2022-04-26")), date_labels = "%Y-%m-%d") +
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold", color = "black"),
    axis.title = element_text(size = 12, face = "bold", color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(), #removes x-axis labels
    plot.margin = margin(10, 10, 10, 10)
  )

plot_ba2 <- ggplot(data_long_ba2, aes(x = Age, y = Sample)) +
  geom_density_ridges(fill = "deepskyblue", alpha = 0.75) + 
  theme_minimal() + 
  ggtitle("Omicron: BA.2") +
  labs(x = NULL, y = " ") +
  scale_x_date(limits = as.Date(c("2019-03-09", "2022-04-26")), date_labels = "%Y-%m-%d") +
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold", color = "black"),
    axis.title = element_text(size = 12, face = "bold", color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(), #removes x-axis labels
    plot.margin = margin(10, 10, 10, 10)
  )

plot_ba5 <- ggplot(data_long_ba5, aes(x = Age, y = Sample)) +
  geom_density_ridges(fill = "royalblue", alpha = 0.75) + 
  theme_minimal() + 
  labs(x = NULL, y = " ") +
  ggtitle("Omicron: BA.5") +
  labs(x = "tMRCA (95% HPD)", y = " ") +
  scale_x_date(limits = as.Date(c("2019-03-09", "2022-04-26")), date_labels = "%Y-%m-%d") +
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold", color = "black"),
    axis.title = element_text(size = 12, face = "bold", color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )
variant_tmrca_plot <- plot_grid(plot_alpha, plot_delta, plot_ba1, plot_ba2, plot_ba5, ncol=1)
ggsave("C:/Users/gev25289/Desktop/wastewater/figures/tmrca_plots.pdf", height = 10, width = 12)

#Omicron sublineage tMRCAs
###########################################################################################################
calculate_hpd <- function(mcmc_data) {
  # Select only numeric columns for HPD calculation
  numeric_data <- mcmc_data[sapply(mcmc_data, is.numeric)]
  
  # Apply the calculation to each numeric column
  results <- sapply(numeric_data, function(col) {
    hpd_vals <- hdi(col)  # Calculate HPD interval
    
    # Convert decimal years to dates using date_decimal
    mean_date <- date_decimal(mean(col))
    hpd_lower_date <- date_decimal(hpd_vals[1])
    hpd_upper_date <- date_decimal(hpd_vals[2])
    
    # Return mean and HPD converted to dates (as character format for easy reading)
    c(mean = format(mean_date, "%Y-%m-%d"), 
      hpd_lower = format(hpd_lower_date, "%Y-%m-%d"),
      hpd_upper = format(hpd_upper_date, "%Y-%m-%d"))
  })
  
  # Transpose the result and convert to a dataframe
  results <- t(results)
  colnames(results) <- c("mean", "hpd_lower", "hpd_upper")
  
  return(as.data.frame(results))  # Return results
}

calculate_hpd(ww_ba)
calculate_hpd(clinical_ba)
calculate_hpd(combined_ba)

#mean(ww_ba$age.BA5)
#date_decimal(2021.404)
