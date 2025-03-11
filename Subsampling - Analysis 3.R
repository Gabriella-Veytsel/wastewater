#Analysis 3

#Wastewater - Clarke (n=80)
#################################################################################################################################
ww <- read.fasta("C:/Users/gev25289/Desktop/xps/ww full length/ww_full_removeNs.fasta")
dates <- str_extract(ww$seq.name, "(?<=\\|)[^|]+(?=\\|)")
formatted_dates <- format(mdy(dates), "%Y-%m-%d")
ww$seq.name <- str_replace(ww$seq.name, "(?<=\\|)[^|]+(?=\\|)", formatted_dates)
ww$seq.name <- str_replace(ww$seq.name, "^(.*)\\|([^|]+)\\|(.*)$", "\\1|\\3|\\2")
ww$seq.name <- str_replace(ww$seq.name, "^(.*?)\\1(\\|.*)", "\\1\\2") #remove repeated strings
ww$seq.name <- gsub("_", "|", ww$seq.name)
ww$seq.name <- str_replace(ww$seq.name, "^(.*?\\|)", "\\1Clarke|")

#Clinical - Clarke (n=632)
#################################################################################################################################
gisaid <- read.fasta("C:/Users/gev25289/Desktop/wiser/Analysis 2/clinical/gisaid_clarke_632.fasta")
gisaid <- gisaid %>%
  mutate(ID = sub("\\|.*", "", seq.name)) %>%
  mutate(date = sub("^[^|]*\\|([^|]*)\\|.*", "\\1", seq.name)) %>%
  mutate(pangolin_lineage = sub("^[^|]*\\|[^|]*\\|([^|]*).*", "\\1", seq.name)) %>%
  mutate(WHO_name = sub("^(.*?\\|.*?\\|.*?\\|)(.*)", "\\2", seq.name)) %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date)) %>%
  mutate(loc = "Clarke") %>%
  mutate(seq.name = paste(ID, loc, pangolin_lineage, WHO_name, date, sep = "|"))

#Subset Clinical - Clarke Sequences (n=261)
#################################################################################################################################
set.seed(123)
gisaid_subset1 <- gisaid %>%
  group_by(year, month) %>% 
  slice_sample(n=18)
set.seed(888)
gisaid_subset2 <- gisaid %>%
  group_by(year, month) %>% 
  slice_sample(n=18)
set.seed(900)
gisaid_subset3 <- gisaid %>%
  group_by(year, month) %>% 
  slice_sample(n=18)
set.seed(4)
gisaid_subset4 <- gisaid %>%
  group_by(year, month) %>% 
  slice_sample(n=18)
set.seed(2)
gisaid_subset5 <- gisaid %>%
  group_by(year, month) %>% 
  slice_sample(n=18)

#Generate contextual dataset for Clarke County sequences 
#################################################################################################################################
#In GISAID, filter to same time period as wastewater and clinical Clarke County seqs: 2021-04-14 to 2022-09-05
contextual_metadata <- read_tsv("C:/Users/gev25289/Desktop/wastewater/Contextual datasets/metadata_combined.tsv") %>% 
  filter(strain != "strain") %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  distinct(strain, .keep_all = TRUE) #GISAID has LOTS of duplicates 

contextual_metadata <- who_name(contextual_metadata)

#Georgia
NS3 <- read_csv("C:/Users/gev25289/Desktop/xps/georgia/raw data/NS3.csv") %>% dplyr::select(c(`GISAID Accession`, `GISAID Name`, Zip)) %>% distinct()
ELR <- read_csv("C:/Users/gev25289/Desktop/xps/georgia/raw data/ELR.csv") %>% dplyr::select(c(`GISAID Accession`, `GISAID Name`, Zip)) %>% distinct()
#All I need is zip code from this dataset

#Clean zip code field
#table(NS3$zip) #don't want to guess the ones with intervals
NS3 <- NS3 %>% mutate(Zip = as.integer(Zip)) %>% #but, can fix the ones with decimals for crosswalk 
  mutate(Zip = as.character(Zip)) %>% distinct() #change back for crosswalk join
ELR <- ELR %>% mutate(Zip = as.integer(Zip)) %>%  
  mutate(Zip = as.character(Zip)) 

NS3_name <- NS3 %>% filter(!is.na(`GISAID Name`)) %>% select(-`GISAID Accession`)
NS3_accession <- NS3 %>% filter(!is.na(`GISAID Accession`)) %>% select(-`GISAID Name`)

contextual_metadata_georgia <- contextual_metadata %>%
  filter(division == "Georgia") %>%
  filter(!strain %in% gisaid$ID) 

#Add zip code column to larger GISAID download
contextual_metadata_georgia <- left_join(contextual_metadata_georgia, NS3_name, by = c("strain"="GISAID Name")) 
contextual_metadata_georgia <- left_join(contextual_metadata_georgia, NS3_accession, by = c("gisaid_epi_isl"= "GISAID Accession")) %>% distinct() 
contextual_metadata_georgia %>% group_by_all() %>% filter(n()>1) %>% ungroup() #used distinct() above since one was duplicate in NS3 
contextual_metadata_georgia$zip <- ifelse(is.na(contextual_metadata_georgia$Zip.x), contextual_metadata_georgia$Zip.y, contextual_metadata_georgia$Zip.x)
contextual_metadata_georgia <- left_join(contextual_metadata_georgia, ELR, by = c("gisaid_epi_isl"= "GISAID Accession"))
contextual_metadata_georgia$zip <- ifelse(is.na(contextual_metadata_georgia$zip), contextual_metadata_georgia$Zip, contextual_metadata_georgia$zip)

#Package zipcodeR for zip code croswalk 
zip_code_db <- zipcodeR::zip_code_db %>%
  dplyr::select(c(zipcode, major_city, county, state, lat, lng, 
                  population, population_density, land_area_in_sqmi, median_household_income))

contextual_metadata_georgia <- left_join(contextual_metadata_georgia, zip_code_db, by = c("zip" = "zipcode")) 

#Convert county to public health district
contextual_metadata_georgia <- public_health_district_county(contextual_metadata_georgia)
contextual_metadata_georgia$public_health_district[contextual_metadata_georgia$location == "Atlanta"] <- "Fulton"
contextual_metadata_georgia$public_health_district[contextual_metadata_georgia$location == "Bibb County"] <- "North Central"
contextual_metadata_georgia$public_health_district[contextual_metadata_georgia$location == "DULUTH"] <- "GNR"
contextual_metadata_georgia$public_health_district[contextual_metadata_georgia$location == "Forsyth County"] <- "North"
contextual_metadata_georgia %>% filter(!is.na(public_health_district)) %>% select(public_health_district) %>% summarise(n=n()) #438/552 have ph district
contextual_metadata_georgia %>% group_by(public_health_district) %>% select(public_health_district) %>% summarise(n=n())
contextual_metadata_georgia <- contextual_metadata_georgia %>% mutate(loc = "Georgia")

#Subset Georgia Contextual Sequences (n=182)
#################################################################################################################################
set.seed(123)
contextual_subset_ga1 <- contextual_metadata_georgia %>% 
  group_by(public_health_district, year, month) %>% 
  slice_sample(n=1)
set.seed(888)
contextual_subset_ga2 <- contextual_metadata_georgia %>% 
  group_by(public_health_district, year, month) %>% 
  slice_sample(n=1)
set.seed(900)
contextual_subset_ga3 <- contextual_metadata_georgia %>% 
  group_by(public_health_district, year, month) %>% 
  slice_sample(n=1)
set.seed(4)
contextual_subset_ga4 <- contextual_metadata_georgia %>% 
  group_by(public_health_district, year, month) %>% 
  slice_sample(n=1)
set.seed(2)
contextual_subset_ga5 <- contextual_metadata_georgia %>% 
  group_by(public_health_district, year, month) %>% 
  slice_sample(n=1)

#Subset Worldwide Contextual Sequences (n=108)
#################################################################################################################################
contextual_metadata_worldwide <- contextual_metadata %>%
  filter(division != "Georgia" | is.na(division)) %>% #Filter drops missing values otherwise
  mutate(loc = "OOS")
set.seed(123)
contextual_subset1 <- contextual_metadata_worldwide %>% 
  group_by(region, year, month) %>% 
  slice_sample(n=1)
set.seed(888)
contextual_subset2 <- contextual_metadata_worldwide %>% 
  group_by(region, year, month) %>% 
  slice_sample(n=1)
set.seed(900)
contextual_subset3 <- contextual_metadata_worldwide %>% 
  group_by(region, year, month) %>% 
  slice_sample(n=1)
set.seed(4)
contextual_subset4 <- contextual_metadata_worldwide %>% 
  group_by(region, year, month) %>% 
  slice_sample(n=1)
set.seed(2)
contextual_subset5 <- contextual_metadata_worldwide %>% 
  group_by(region, year, month) %>% 
  slice_sample(n=1)

contextual_fasta <- read.fasta("C:/Users/gev25289/Desktop/wastewater/Contextual datasets/sequences_combined.fasta") %>% distinct(seq.name, .keep_all = TRUE)

contextual_subset_all1 <- bind_rows(contextual_subset_ga1, contextual_subset1) %>% select(strain, date, public_health_district, WHO_name, pangolin_lineage, region, division, loc) %>% left_join(contextual_fasta, by = c("strain" = "seq.name")) %>% distinct()
contextual_subset_all2 <- bind_rows(contextual_subset_ga2, contextual_subset2) %>% select(strain, date, public_health_district, WHO_name, pangolin_lineage, region, division, loc) %>% left_join(contextual_fasta, by = c("strain" = "seq.name")) %>% distinct()
contextual_subset_all3 <- bind_rows(contextual_subset_ga3, contextual_subset3) %>% select(strain, date, public_health_district, WHO_name, pangolin_lineage, region, division, loc) %>% left_join(contextual_fasta, by = c("strain" = "seq.name")) %>% distinct()
contextual_subset_all4 <- bind_rows(contextual_subset_ga4, contextual_subset4) %>% select(strain, date, public_health_district, WHO_name, pangolin_lineage, region, division, loc) %>% left_join(contextual_fasta, by = c("strain" = "seq.name")) %>% distinct()
contextual_subset_all5 <- bind_rows(contextual_subset_ga5, contextual_subset5) %>% select(strain, date, public_health_district, WHO_name, pangolin_lineage, region, division, loc) %>% left_join(contextual_fasta, by = c("strain" = "seq.name")) %>% distinct()

#Rename
contextual_subset_all1$seq.name <- paste(contextual_subset_all1$strain, contextual_subset_all1$loc, contextual_subset_all1$pangolin_lineage, contextual_subset_all1$WHO_name, contextual_subset_all1$date, sep = "|")
contextual_subset_all2$seq.name <- paste(contextual_subset_all2$strain, contextual_subset_all2$loc, contextual_subset_all2$pangolin_lineage, contextual_subset_all2$WHO_name, contextual_subset_all2$date, sep = "|")
contextual_subset_all3$seq.name <- paste(contextual_subset_all3$strain, contextual_subset_all3$loc, contextual_subset_all3$pangolin_lineage, contextual_subset_all3$WHO_name, contextual_subset_all3$date, sep = "|")
contextual_subset_all4$seq.name <- paste(contextual_subset_all4$strain, contextual_subset_all4$loc, contextual_subset_all4$pangolin_lineage, contextual_subset_all4$WHO_name, contextual_subset_all4$date, sep = "|")
contextual_subset_all5$seq.name <- paste(contextual_subset_all5$strain, contextual_subset_all5$loc, contextual_subset_all5$pangolin_lineage, contextual_subset_all5$WHO_name, contextual_subset_all5$date, sep = "|")

#Combine
gisaid_contextual1 <- bind_rows(contextual_subset_all1, gisaid_subset1)
gisaid_contextual2 <- bind_rows(contextual_subset_all2, gisaid_subset2)
gisaid_contextual3 <- bind_rows(contextual_subset_all3, gisaid_subset3)
gisaid_contextual4 <- bind_rows(contextual_subset_all4, gisaid_subset4)
gisaid_contextual5 <- bind_rows(contextual_subset_all5, gisaid_subset5)

#Export
dat2fasta(gisaid_contextual1, "C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample1.fasta")
dat2fasta(gisaid_contextual2, "C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample2.fasta")
dat2fasta(gisaid_contextual3, "C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample3.fasta")
dat2fasta(gisaid_contextual4, "C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample4.fasta")
dat2fasta(gisaid_contextual5, "C:/Users/gev25289/Desktop/wastewater/Analysis3/subsample5.fasta")

#PDA - I didn't end up using this as a subsampling strategy in my analyses
contextual_georgia <- contextual_metadata_georgia %>% left_join(contextual_fasta, by = c("strain" = "seq.name"))
contextual_world <- contextual_metadata_worldwide %>% left_join(contextual_fasta, by = c("strain" = "seq.name"))
contextual_georgia$seq.name <- paste(contextual_georgia$strain, contextual_georgia$loc, contextual_georgia$pangolin_lineage, contextual_georgia$WHO_name, contextual_georgia$date, sep = "|")
contextual_world$seq.name <- paste(contextual_world$strain, contextual_world$loc, contextual_world$pangolin_lineage, contextual_world$WHO_name, contextual_world$date, sep = "|")

dat2fasta(contextual_georgia, "C:/Users/gev25289/Desktop/wastewater/Analysis3/contextual_georgia.fasta")
dat2fasta(contextual_world, "C:/Users/gev25289/Desktop/wastewater/Analysis3/contextual_worldwide.fasta")

contextual_worldwide_aligned <- read.fasta("C:/Users/gev25289/Desktop/wastewater/Analysis3/contextual_worldwide_aligned.fasta")
contextual_worldwide_aligned$seq.text <- substr(contextual_worldwide_aligned$seq.text, 107, nchar(contextual_worldwide_aligned$seq.text))
contextual_worldwide_aligned$seq.text <- substr(contextual_worldwide_aligned$seq.text, 1, 29700)
dat2fasta(contextual_worldwide_aligned, "C:/Users/gev25289/Desktop/wastewater/Analysis3/contextual_worldwide_aligned_trim.fasta")
