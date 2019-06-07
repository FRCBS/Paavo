# This script aims to carry out the same preprocessing as the previously existing scripts r_descsForSanguinSpring.R and r_donorStats.R.
# It uses the f_runFunction.R script.
# This script aims to fix the following issues:
#   Keep all donation events for FinnDonor participants regardless of donation type. 
# It is so that measurement events (Rows in the blood measurement file from HUS) may result from full blood or non full blood donations. 
# This needs to be taken into account when counting the number of donation events for prediction purposes.

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)


# load file
load("/home/ilpo/Paavo/src/ilposdata.rdata")

# Load all visits data
file = "../data/r01ps.pro.data.rdata"
load(file)
data_temp <- output %>% 
  rename(sex  =gender,
         blood_volume = dunno3)
rm(output) 


# The goal of the first part of the script is to preprocess the donation event file progesa.rdata
# which has all donation events in the last 15 years. 
# The code checks if the intermediary file has been previously created 
# in /data/preprocessed_data.RData . Loads it if it hasen't,
# creates it from the raw data otherwise.

setwd("~/home/ilpo/Paavo/src")
print(getwd())
data2 <- paste0("data/","preprocessed_data.RData")

if (!file.exists(data2)) {
  file = paste0("data/raw_data.RData")
  cat("Loading",file,"\n")
  load(file)
  data <- ilposdata
  rm(ilposdata)
  decorateData <- function(data) {
    
    #Take all donations events (reagrdless of type)
    data <- data %>% 
    
    mutate(day_date = as_date(dateonly)) %>%
    
   
      #Take all donations events (reagrdless of type)
      data <- data %>% 
        # filter(donat_phleb == 'K') %>% 
        mutate(locSim = case_when(
          site == "H0091" | site == "H0092" ~ "Kivihaka",
          site == "H0093"  ~ "Sanomatalo",
          site == "H0096"  ~ "Espoo",
          TRUE ~ "Other"),
          locSim2 = ifelse(locSim != "Other","StudySite","Other")) %>% 
    
    
  #Keep only donors who have donated full blood or tried to donate full blood after 2015
  #Here we compute the number of donations per year, excluding deferred donations.
  
  data_temp <- 
    data_temp %>% 
    filter(donat_phleb == 'K' & status == "-" | 
             donat_phleb == 'K' & status != "-" & blood_volume > 100 |
             donat_phleb == 'H' & blood_volume > 100 |
             donat_phleb == "*") %>% 
    filter(Year >= 2015) %>% 
    distinct(donor) %>% 
    inner_join(data_temp, by = "donor")
  
# Let's compute lifetime nb of donations for each donor and Year of interest (2015-2019).

ref_year <- 2015
 lifetime_donation_data 


get_count<-function(ref_year, data_temp){
     result <- data_temp %>% 
        filter(donat_phleb == 'K' & status == "-" | 
                donat_phleb == 'K' & status != "-" & blood_volume > 100 |
               donat_phleb == 'H' & blood_volume > 100) %>% 
        filter(Year < ref_year) %>% 
        group_by(donor) %>% 
        summarise(all_donations_count = n()) %>% 
        mutate(Year = ref_year) 
     
     return(result)
}

 data_temp %>% 
   filter(Year >= 2015) %>% 
   distinct(Year) %>% 
   map_dfr(~get_count(ref_year = .x, data_temp = data_temp))

for (ref_year in c(2015:2019)){
  lifetime_donation_data_temp <- get_count(ref_year = ref_year, data_temp = data_temp)
  
  if(!exists("lifetime_donation_data")){
    lifetime_donation_data <- lifetime_donation_data_temp
  }else{
    lifetime_donation_data <- 
      lifetime_donation_data %>% 
      bind_rows(lifetime_donation_data_temp)
  }
  
  rm(lifetime_donation_data_temp)
}
 
 
 # Groub it all by gender. 
 

# map_dfr(c(2012:2018), get_count(ref_year = .x, data_temp = data_temp))


#  emerge the paavo file to donor register
 
 
 
 #table 1 - Desciptive statistics 
 
 #Paavo
 
 #Donor register 
 
 
 
 #Whats interesting in Paavo? Toimeentulotuki, median income, education, share of unemployed. 
 

 
 #chi-squared tables
 
 
 #standard estimation tables
 
 
 # plotting everything
 
 # Grouping zip-codes. Maybe a factorial analysis/principal component analysis?
 
 #figure out the right estimation model
 
#Find a way to test autocorrelation 

  # Visualise it with map 
  