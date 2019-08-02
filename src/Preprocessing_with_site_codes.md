Preprocessing with site codes
================
Ilpo Arminen
7/31/2019

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.0          ✔ purrr   0.3.2     
    ## ✔ tibble  2.1.3          ✔ dplyr   0.8.2     
    ## ✔ tidyr   0.8.3.9000     ✔ stringr 1.4.0     
    ## ✔ readr   1.3.1          ✔ forcats 0.4.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

# This is done by comparing missing values in zip codes with the list of Statistical centers Paavo data.

\#I took all the zip codes from blood doning data which had more than 50
missing values and did not have match \# in paavodata.

``` r
ilposdata %>% mutate(zip = recode(zip,
#old value = new value
"106001" = "10600",
"201001" = "20100",
"216001"= "21600",
"257001" = "25710",
"27001" = "27100",
"41301"= "41310" ,
"61001"="61100",
"651001"= "61100",
"669001"="66900",
"685001" = "68500",
"686001"= "68600",
"686201"= "68620",
"651001"= "06500",
"651001" = "65100",
"652001" = "65200",
"669001"= "66900",
"686001" = "68600",
"688001" = "68600",
"688001" = "68600"))
```

    ## # A tibble: 1,537,680 x 13
    ##    donor Site  dateonly   status donat_phleb    Hb gender aborh zip     age
    ##    <fct> <fct> <date>     <fct>  <fct>       <dbl> <fct>  <fct> <fct> <int>
    ##  1 DR00… L3149 2018-10-08 -      K             141 Women  A Rh… 90570    19
    ##  2 DR00… L3149 2018-10-08 -      K             156 Men    O Rh… 90530    21
    ##  3 DR00… L3149 2018-10-08 -      K             156 Men    A Rh… 90560    22
    ##  4 DR00… L0564 2019-01-17 -      K             163 Men    A Rh… 90560    22
    ##  5 DR00… L3149 2018-10-08 R      K             127 Women  A Rh… 90570    20
    ##  6 DR00… L3149 2019-01-14 E      *             138 Women  A Rh… 90570    20
    ##  7 DR00… L3149 2018-10-08 E      *             144 Women  ""    90550    19
    ##  8 DR00… L3149 2018-10-08 -      K             140 Women  O Rh… 90530    21
    ##  9 DR00… L3149 2018-10-08 -      K             128 Women  B Rh… 90500    19
    ## 10 DR00… L3149 2018-10-08 -      K             153 Women  AB R… 90530    20
    ## # … with 1,537,670 more rows, and 3 more variables: age.group <fct>,
    ## #   Hb_deferral <fct>, FirstEvent <lgl>

# Removing string value “Na” which shows in plots.

``` r
ilposdata$zip <- gsub('.*NA.*',NA,ilposdata$zip)
ilposdata <- ilposdata %>% 
filter(!is.na(zip))
```

\#\#Numbers of donors per zip

``` r
prepocessing <- ilposdata %>%
mutate(Year = year(dateonly)) %>% # Getting the variable Year from dates
filter(donat_phleb == "K") %>%    # filtering full blood donations only
filter(Year == 2017 | Year == 2018) %>%  # Selecting wanted years
count(donor, Year,zip) %>%   #  Selecting wanted variables
count(Year, zip) %>%        # Needed for counting donors per  zip codes and yearly count
rename(nb_donors_per_zip = n)  # renaming the n to numbers of donors per postal code
```

## number of donations per zip

``` r
test <-ilposdata %>%  #new subset
mutate(Year = year(dateonly)) %>%  # Getting the variable Year from dates
filter(donat_phleb == "K") %>%       # filtering full blood donations only
filter(Year == 2017| Year == 2018) %>% # Selecting wanted years 
count(Year,zip) %>%   # Counting the donors straight to year and postal code. Not counting the donors means getting donations per postal code
rename(nb_donations_per_zip=n) 
```

## joining the data with commong variables

``` r
preprosessing <-left_join(prepocessing,test,
by = c("zip", "Year"))
```

# Nb of first time donors

``` r
firstevent <- ilposdata %>%
mutate(Year = year(dateonly)) %>%
filter(donat_phleb == "K") %>%
filter(Year == 2017| Year== 2018) %>%
select(zip,FirstEvent,Year) %>%
filter(FirstEvent == TRUE) %>%  #For selecting only donors who had their first blood donation in the selected years
group_by(zip,Year) %>%          # counting first time donors by postal code and selected years
summarise(nb_first_time_donors= n()) %>% #renaming
ungroup()  # ungrouping for safety reasons
```

## Number of repeated donors

``` r
preprocessed <- left_join(preprosessing ,firstevent,
by = c("zip", "Year")) %>%
  mutate( nb_repeat_donors= nb_donors_per_zip - nb_first_time_donors) 
#mutate(stupid_dummy=ifelse(Year == 2018, 1,0)) 

# Joining all the data which has been made this far and counting the number of repeat donors (more than 1 donation) by postal code. Repeat donors equals number of donors per zip code minus number of first time donors per postal code.


preprocessed$nb_first_time_donors[is.na(preprocessed$nb_first_time_donors)] <- 0
preprocessed$nb_repeat_donors[is.na(preprocessed$nb_repeat_donors)] <- 0   # Deleting missing values
```

## Sitedata

``` r
sitedata <- ilposdata %>% 
mutate(Year = year(dateonly)) %>%
filter(donat_phleb == "K") %>%
filter(Year == 2017| Year== 2018) %>% 
count(Year,Site)  
#mutate(stupid_dummy=ifelse(Year == 2018, 1,0)) 
```

# I am having hard time to get the variable “site” moved to created dataset (processed\_site\_paavo). I have tryed every possible join with preprocessed and creating new data frames, but im only managed to create dataframes with duplicate or too many values.

``` r
example <- left_join(sitedata,preprocessed, by=c("Year")) 


#This creates dataframe with 1433351 obs, which is terribly wrong since sitedata is 484 obs and preprocessed is 5923. 
# "The most commonly used join is the left join: you use this whenever you look up additional data from another table, because it preserves the original observations even when there isn’t a match. The left join should be your default join: use it unless you have a strong reason to prefer one of the others".
```

# Lets try to do this with creating nonsense dummy variable to both datasets and then joining by that dummy variable.

``` r
#mergedata <- right_join(preprocessed, sitedata, by= c("stupid_dummy")) # still duplicating values.  removing this. 
```

\#joining the data with Paavodata

``` r
#processed_site_paavo <- paavodata %>%
#rename(zip = pono, Year= vuosi) %>%  # Data needs to be translated to english from finnish
#filter(Year == 2019) %>%   # Paavodata has been collected in the year 2017 so there is no need to get more years than one for our data. 
#mutate(eligible_population = he_18_19 + he_20_24 + he_25_29 + he_30_34 + he_40_44 +
#he_45_49 + he_50_54 + he_55_59 + he_60_64 + he_65_69) %>%   # Eligible population means age groups that can donate blood
#dplyr::select(-Year)                                        # For losing two year variables
#processed_site_paavo <-right_join(processed_site_paavo, big_df, by = c("zip")) %>%          #joining with common factors
#dplyr::select(zip, Year, eligible_population,hr_mtu, hr_ktu,ko_al_kork, ko_yl_kork, nb_donors_per_zip,nb_donations_per_zip, nb_first_time_donors, 
#nb_repeat_donors, pt_tyott, pt_tyoll, ko_ika18y, hr_tuy, euref_x, euref_y, nimi Site) %>%          # Selecting wanted variables
#rename (unemployed = pt_tyott,
#employed = pt_tyoll,
#medianincome= hr_mtu,
#averageincome= hr_ktu,
#population18= ko_ika18y,
#bachelor_degree= ko_al_kork,
#masters_degree= ko_yl_kork,
#averageincome= hr_ktu,
#medianincome= hr_mtu,
#name= nimi) %>%          # mutating finnish variables to English.
#mutate( prop_donors= nb_donors_per_zip/eligible_population,  #Proportion of donors is number of donors per postal code minus eligible population
#nb_donation_per_act_donor= nb_donations_per_zip/nb_donors_per_zip, #  Number of donations per postal code divided by number of donors per postal code
#prop_new_donors= nb_first_time_donors/eligible_population,   # Number of first time donors divided by eligible population per postal code
#prop_repeat_donors= nb_repeat_donors/eligible_population,   #umber of repeat donors divided by eligible population per postal code
#higher_education =bachelor_degree+masters_degree, # Combining university education together. 
#proportion_inhabitants_with_higher_education= higher_education/eligible_population) # Higher education divided by eligible population per postal code


#Drop postal codes with no data
#processed_site_paavo <- processed_site_paavo %>% filter(!is.na(eligible_population))


#Are there NA's left
#processed_site_paavo[apply(processed_site_paavo,1,function(x){any(is.na(x))}),]

#summary(processed_site_paavo)
```

## Make a variable which divides sites into mobile and fixed sites

``` r
#colnames(processed_site_paavo)[grep("site",colnames(processed_site_paavo))] <- "Site"
#Get the sites that are on the data
#processed_site_paavo$Site <- recode(processed_site_paavo$Site,
               #       H0091="Kivihaka",
                #      H0092='Kivihaka',
                 #     H0093='Sanomatalo',
                  #    H0096='Espoo',
                   #   K0297='Kuopio',
                  #    L0564='Oulu',
                  #    T0179='Jyväskylä',
                  #    T0398='Lahti',
                  #    T0743='Seinäjoki',
                  #    T0837='Tampere',
                  #    U0853='Turku',
                  #    .default = levels(processed_site_paavo$Site)
                   #   )


#Add the mobile units in
#processed_site_paavo$Site <- as.character(donation$Site)
#processed_site_paavo$Site[grep("^H1\\d{3}$",donation$Site,perl = TRUE)] <- "Kivihaka mobile"
#processed_site_paavo$Site[grep("^K1\\d{3}$",donation$Site,perl = TRUE)] <- "Kuopio mobile"
#processed_site_paavo$Site[grep("^L1\\d{3}$",donation$Site,perl = TRUE)] <- "Oulu mobile"
#processed_site_paavo$Site[grep("^S1\\d{3}$",donation$Site,perl = TRUE)] <- "Seinäjoki mobile"
#processed_site_paavo$Site[grep("^T1\\d{3}$",donation$Site,perl = TRUE)] <- "Tampere mobile"
#processed_site_paavo$Site[grep("^U1\\d{3}$",donation$Site,perl = TRUE)] <- "Turku mobile"
#if (length(table(nchar(as.character(processed_site_paavo$Site)) == 0)) > 1 ) {
#processed_site_paavo$Site[nchar(as.character(processed_site_paavo$Site)) == 0 ] <-  "unknown"}



#processed_site_paavo$mobile <- recode(processed_site_paavo$Site,
 #                         Kivihaka="fixed",
  #                        Sanomatalo="fixed",
   #                       Espoo="fixed",
    #                      Kuopio="fixed",
     #                     Oulu="fixed",
      #                    Jyväskylä="fixed",
       #                   Lahti="fixed",
        #                  Seinäjoki="fixed",
         #                 Tampere="fixed",
        #                  Turku="fixed",
        #                  Muu="fixed",
         #                 Tuntematon="fixed",
          #                "Kivihaka mobile"="mobile",
           #               "Kuopio mobile"="mobile",
            #              "Oulu mobile"="mobile",
            #              "Seinäjoki mobile"="mobile",
            #              "Tampere mobile"="mobile",
            #              "Turku mobile"="mobile",                            .default = levels(processed_site_paavo$Site))
```

\#Find the zipcodes for each mobile site and fixed
site

## Get the number of donation opportunities for each mobile site in a years of interest

\#\#Compute distances between each zipcode and each mobile/fixed site.
Then choose for each zipcode the closest fixed and the closest mobile.

for each zipcode compute proportion of donations made in fixed site.

``` r
#save(preprosessed_paavo,
#file = paste0("preprocessed.",Sys.Date(),".RData"))
```
