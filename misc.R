library(tidyverse)
library(ggtree)
library(phylotools)
library(treeio)

ww <- phylotools::read.fasta("C:/Users/gev25289/Desktop/wastewater/Analysis2/WW - done/output_mismatch4_trim.fasta") %>%
  mutate(date = sub("([^/]*/){2}", "", seq.name)) %>%
  mutate(date = as.Date(date))
clinical <- phylotools::read.fasta("C:/Users/gev25289/Desktop/wastewater/Analysis2/Clinical - done/output_632_trim.fasta") %>%
  mutate(date = as.Date(sub(".*?\\|(.*?)\\|.*", "\\1", seq.name), format = "%Y-%m-%d"))

combined <- phylotools::read.fasta("C:/Users/gev25289/Desktop/wastewater/Analysis1/632.output_mismatch4_trim.fasta")

ww_ba1 <- ww %>%
  filter(grepl("BA.1", seq.name)) 

ww_ba2 <- ww %>%
  filter(grepl("BA.2", seq.name))

ww_ba4 <- ww %>%
  filter(grepl("BA.4", seq.name))

ww_ba5 <- ww %>%
  filter(grepl("BA.5", seq.name))

##################################################################################################################
clinical_ba1 <- clinical %>%
  filter(grepl("BA.1", seq.name)) 

clinical_ba2 <- clinical %>%
  filter(grepl("BA.2", seq.name))

clinical_ba4 <- clinical %>%
  filter(grepl("BA.4", seq.name))

clinical_ba5 <- clinical %>%
  filter(grepl("BA.5", seq.name))

clinical_ba5 <- clinical %>%
  filter(grepl("BA.5", seq.name))