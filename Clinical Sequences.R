library(tidyverse)
library(ggtree)
library(ggplot2)
library(readxl)
library(phylotools)

source("C:/Users/gev25289/Desktop/xps/georgia/code/functions.R")

# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("ggtree")

#UGA Clinical: 557 samples
#################################################################################################################################
uga_clinical_samples <- read_excel("D:/wastewater/Clinical_sample_metadata.xlsx") #includes metadata without associated samples
uga_clinical_samples <- uga_clinical_samples %>%
  mutate(seq.name = paste(Sample, lineage, date, `S#`, sep = "|"))

uga_clinical_samples_fasta <- read.fasta("D:/wastewater/analysis2/clinical/all_clincal_seqs_N50_aligned_formatted.fasta")
uga_clinical_samples_join <- uga_clinical_samples_fasta %>% inner_join(uga_clinical_samples)
uga_clinical_samples_join <- uga_clinical_samples_join %>%
  mutate(zip = as.double(zip))

uga_clinical_samples_join_who <- who_name(uga_clinical_samples_join)
uga_clinical_samples_join_who$WHO_name <- ifelse(is.na(uga_clinical_samples_join_who$WHO_name), "Other", uga_clinical_samples_join_who$WHO_name) #No WHO name
uga_clinical_samples_join_who$WHO_name <- as.factor(uga_clinical_samples_join_who$WHO_name)

range(uga_clinical_samples_join$date) #dates range from 2021-03-10 to 2022-02-25
ggplot(uga_clinical_samples_join, aes(x=date)) + geom_histogram()
ggsave("D:/wastewater/figures/UGA Sequences over Time.pdf")

ggplot(uga_clinical_samples_join_who, aes(x=WHO_name)) + geom_histogram(stat="count")
ggsave("D:/wastewater/figures/UGA Variants.pdf", width=10, height=7)

#GISAID
#################################################################################################################################
metadata_district10 <- read_tsv("C:/Users/gev25289/Desktop/xps/georgia/metadata_district10.tsv") %>% distinct(strain, .keep_all = TRUE) #checked two duplicate names, sequence were the same too
range(metadata_district10$date) #2,532 sequences, dates range from 2020-12-26 to 2022-10-19

metadata_district10 %>% group_by(county) %>% summarise(n=n())  #678 samples in Clarke County
metadata_clarke <- metadata_district10 %>% filter(county == "Clarke County")

#If I match the dates of Clarke County wastewater samples
#Before I matched to UGA samples, which end in Feb 2022...
metadata_clarke <- metadata_clarke %>%
  filter(date <= as.Date("2022-09-05")) %>%
  filter(date >= as.Date("2021-04-14")) 

#678 samples in Clarke County -> 632 samples in Clarke County during same time period
metadata_clarke$WHO_name <- as.factor(metadata_clarke$WHO_name)
#metadata_clarke %>% group_by(major_city) %>% summarise(n=n()) #586 samples in Athens, 48 in Winterville
metadata_clarke %>% group_by(coverage) %>% summarise(n=n()) 

#I think these samples are more recent than my download to figure out coverage
#Instead, paste the strain names into gisaid and filter
#Coverage -> 249 high coverage
names <- metadata_clarke %>% pull(strain) %>% as.data.frame()
write.table(names, "C:/Users/gev25289/Desktop/wiser/Analysis 2/clinical/IDs.tsv", quote = FALSE, col.names =FALSE,row.names = FALSE)
#349/632 = 55.22%

ggplot(metadata_clarke, aes(x=date)) + geom_histogram() 
ggsave("D:/wastewater/figures/GISAID Sequences over Time.pdf")

ggplot(metadata_clarke, aes(x=WHO_name)) + geom_histogram(stat="count")
ggsave("D:/wastewater/figures/GISAID Variants.pdf", width = 10, height=7)

#Compare
metadata_clarke$dataset = "GISAID"
uga_clinical_samples_join_who$dataset = "UGA"
combined_data <- bind_rows(metadata_clarke, uga_clinical_samples_join_who)

ggplot(combined_data, aes(x = date, fill = dataset)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  labs(title = "Overlayed Histograms", x = "date", y = "Frequency") +
  theme_minimal()
ggsave("D:/wastewater/figures/UGA vs. GISAID Sequences over Time.pdf")

ggplot(combined_data, aes(x = WHO_name, fill = dataset)) + 
  geom_histogram(stat="count", bins = 30) +
  labs(title = "Stacked Bar Chart", x = "Variant", y = "Frequency") +
  theme_minimal()
ggsave("D:/wastewater/figures/UGA vs. GISAID Variants.pdf", height=7, width=12)

#Get the FASTA for GISAID Clarke County same time period
write_tsv(metadata_clarke, "C:/Users/gev25289/Desktop/xps/georgia/metadata_clarke_634.tsv")

complete <- read.fasta("C:/Users/gev25289/Desktop/xps/georgia old/gisaid/complete delta genomes/gisaid_sequences.combined.fasta") %>% distinct(seq.name, .keep_all = TRUE)
clarke <- metadata_clarke %>% left_join(complete, by = c("strain" = "seq.name"))
clarke$seq.name <- paste(clarke$strain, clarke$date, clarke$pangolin_lineage, clarke$WHO_name, sep = "|")
dat2fasta(clarke, "C:/Users/gev25289/Desktop/wiser/Analysis 2/clinical/gisaid_clarke_632.fasta")