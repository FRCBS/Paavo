Preprocessing fixed sites
================
Ilpo Arminen
7/31/2019

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.0          ✔ purrr   0.3.2     
    ## ✔ tibble  2.1.3          ✔ dplyr   0.8.2     
    ## ✔ tidyr   0.8.3.9000     ✔ stringr 1.4.0     
    ## ✔ readr   1.3.1          ✔ forcats 0.4.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
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

\#\#Numbers of donors that donated to a fixed site per zip

``` r
prepocessing <- ilposdata %>%
mutate(Year = year(dateonly)) %>% # Getting the variable Year from dates
filter(donat_phleb == "K") %>%    # filtering full blood donations only
filter(Year == 2017 | Year == 2018) %>%  # Selecting wanted years
 filter(Site %in% c("H0091", "H0092", "H0093","H0096","T0398")) %>%  # Selecting only Kivihaka,Espoo,Sanomatalo and Lahti fixed sites
count(donor, Year,zip) %>%   #  Selecting wanted variables
count(Year, zip) %>%        # Needed for counting donors per  zip codes and yearly count
rename(nb_fixed_donors_per_zip = n)  # renaming the n to numbers of donors per postal code
```

## number of donations that donated to a fixed site per

``` r
test <-ilposdata %>%  #new subset
mutate(Year = year(dateonly)) %>%  # Getting the variable Year from dates
filter(donat_phleb == "K") %>%       # filtering full blood donations only
filter(Year == 2017| Year == 2018) %>%
filter(Site %in% c("H0091", "H0092", "H0093","H0096","T0398")) %>% # Selecting wanted years 
count(Year,zip) %>%   # Counting the donors straight to year and postal code. Not counting the donors means getting donations per postal code
rename(nb_fixed_donations_per_zip=n) 
```

## joining the data with commong variables

``` r
preprosessing <-left_join(prepocessing,test,
by = c("zip", "Year"))
```

# Nb of first time donors that donated to a fixed site per

``` r
firstevent <- ilposdata %>%
mutate(Year = year(dateonly)) %>%
filter(donat_phleb == "K") %>%
filter(Year == 2017| Year== 2018) %>%
 filter(Site %in% c("H0091", "H0092", "H0093","H0096","T0398")) %>% 
dplyr::select(zip,FirstEvent,Year) %>%
filter(FirstEvent == TRUE) %>%  #For selecting only donors who had their first blood donation in the selected years
group_by(zip,Year) %>%          # counting first time donors by postal code and selected years
summarise(nb_fixed_first_time_donors= n()) %>% #renaming
ungroup()  # ungrouping for safety reasons
```

## Number of repeated donors that donated to a fixed site per

``` r
fix_preprocessed <- left_join(preprosessing ,firstevent,
by = c("zip", "Year")) %>%
  mutate( nb_fixed_repeat_donors= nb_fixed_donors_per_zip - nb_fixed_first_time_donors) 


# Joining all the data which has been made this far and counting the number of repeat donors (more than 1 donation) by postal code. Repeat donors equals number of donors per zip code minus number of first time donors per postal code.


fix_preprocessed$nb_fixed_first_time_donors[is.na(fix_preprocessed$nb_fixed_first_time_donors)] <- 0
fix_preprocessed$nb_fixed_repeat_donors[is.na(fix_preprocessed$nb_fixed_repeat_donors)] <- 0   # Deleting missing values
```

\#joining the data with Paavodata

``` r
processed_site_paavo <- paavodata %>%
rename(zip = pono, Year= vuosi) %>%  # Data needs to be translated to english from finnish
filter(Year == 2019) %>%   # Paavodata has been collected in the year 2017 so there is no need to get more years than one for our data. 
mutate(eligible_population = he_18_19 + he_20_24 + he_25_29 + he_30_34 + he_40_44 +
he_45_49 + he_50_54 + he_55_59 + he_60_64 + he_65_69) %>%   # Eligible population means age groups that can donate blood
dplyr::select(-Year)                                        # For losing two year variables
processed_site_paavo <-right_join(processed_site_paavo, fix_preprocessed, by = c("zip")) %>%          #joining with common factors
dplyr::select(zip, Year, eligible_population,hr_mtu, hr_ktu,ko_al_kork, ko_yl_kork, nb_fixed_donors_per_zip,nb_fixed_donations_per_zip, nb_fixed_first_time_donors, 
nb_fixed_repeat_donors, pt_tyott, pt_tyoll, ko_ika18y, hr_tuy,nimi) %>%          # Selecting wanted variables
rename (unemployed = pt_tyott,
employed = pt_tyoll,
medianincome= hr_mtu,
averageincome= hr_ktu,
population18= ko_ika18y,
bachelor_degree= ko_al_kork,
masters_degree= ko_yl_kork,
averageincome= hr_ktu,
medianincome= hr_mtu,
name= nimi) %>%          # mutating finnish variables to English.
mutate( prop_donors= nb_fixed_donors_per_zip/eligible_population,  #Proportion of donors is number of donors per postal code minus eligible population
nb_fixed_donation_per_act_donor= nb_fixed_donations_per_zip/nb_fixed_donors_per_zip, #  Number of donations per postal code divided by number of donors per postal code
prop_new_donors= nb_fixed_first_time_donors/eligible_population,   # Number of first time donors divided by eligible population per postal code
prop_repeat_donors= nb_fixed_repeat_donors/eligible_population,   #umber of repeat donors divided by eligible population per postal code
higher_education =bachelor_degree+masters_degree, # Combining university education together. 
proportion_inhabitants_with_higher_education= higher_education/eligible_population) # Higher education divided by eligible population per postal code


#Drop postal codes with no data
processed_site_paavo <- processed_site_paavo %>% filter(!is.na(eligible_population))


#Are there NA's left
processed_site_paavo[apply(processed_site_paavo,1,function(x){any(is.na(x))}),]
```

    ##  [1] zip                                         
    ##  [2] Year                                        
    ##  [3] eligible_population                         
    ##  [4] medianincome                                
    ##  [5] averageincome                               
    ##  [6] bachelor_degree                             
    ##  [7] masters_degree                              
    ##  [8] nb_fixed_donors_per_zip                     
    ##  [9] nb_fixed_donations_per_zip                  
    ## [10] nb_fixed_first_time_donors                  
    ## [11] nb_fixed_repeat_donors                      
    ## [12] unemployed                                  
    ## [13] employed                                    
    ## [14] population18                                
    ## [15] hr_tuy                                      
    ## [16] name                                        
    ## [17] prop_donors                                 
    ## [18] nb_fixed_donation_per_act_donor             
    ## [19] prop_new_donors                             
    ## [20] prop_repeat_donors                          
    ## [21] higher_education                            
    ## [22] proportion_inhabitants_with_higher_education
    ## <0 rows> (or 0-length row.names)

``` r
summary(processed_site_paavo)
```

    ##      zip                 Year      eligible_population  medianincome  
    ##  Length:2014        Min.   :2017   Min.   :   26.0     Min.   :10469  
    ##  Class :character   1st Qu.:2017   1st Qu.:  560.5     1st Qu.:18994  
    ##  Mode  :character   Median :2017   Median : 1602.5     Median :21194  
    ##                     Mean   :2017   Mean   : 2428.1     Mean   :21565  
    ##                     3rd Qu.:2018   3rd Qu.: 3526.2     3rd Qu.:23723  
    ##                     Max.   :2018   Max.   :18536.0     Max.   :35612  
    ##  averageincome    bachelor_degree   masters_degree  
    ##  Min.   : 12124   Min.   :   0.00   Min.   :   0.0  
    ##  1st Qu.: 21410   1st Qu.:  73.25   1st Qu.:  48.0  
    ##  Median : 23302   Median : 234.50   Median : 167.5  
    ##  Mean   : 24390   Mean   : 407.93   Mean   : 401.3  
    ##  3rd Qu.: 25912   3rd Qu.: 580.00   3rd Qu.: 501.0  
    ##  Max.   :100488   Max.   :4036.00   Max.   :5704.0  
    ##  nb_fixed_donors_per_zip nb_fixed_donations_per_zip
    ##  Min.   :  1.00          Min.   :  1.00            
    ##  1st Qu.:  1.00          1st Qu.:  2.00            
    ##  Median :  3.00          Median :  5.00            
    ##  Mean   : 27.66          Mean   : 46.71            
    ##  3rd Qu.: 17.00          3rd Qu.: 28.00            
    ##  Max.   :552.00          Max.   :929.00            
    ##  nb_fixed_first_time_donors nb_fixed_repeat_donors   unemployed     
    ##  Min.   : 0.000             Min.   :  0.00         Min.   :   1.00  
    ##  1st Qu.: 0.000             1st Qu.:  0.00         1st Qu.:  50.25  
    ##  Median : 0.000             Median :  0.00         Median : 159.50  
    ##  Mean   : 3.218             Mean   : 22.88         Mean   : 259.64  
    ##  3rd Qu.: 2.000             3rd Qu.: 13.00         3rd Qu.: 363.00  
    ##  Max.   :73.000             Max.   :479.00         Max.   :2654.00  
    ##     employed        population18         hr_tuy            name          
    ##  Min.   :   13.0   Min.   :   43.0   Min.   :   41.0   Length:2014       
    ##  1st Qu.:  403.5   1st Qu.:  758.2   1st Qu.:  756.2   Class :character  
    ##  Median : 1151.5   Median : 2164.5   Median : 2178.5   Mode  :character  
    ##  Mean   : 1703.5   Mean   : 3263.5   Mean   : 3242.1                     
    ##  3rd Qu.: 2414.0   3rd Qu.: 4709.0   3rd Qu.: 4626.0                     
    ##  Max.   :13006.0   Max.   :24408.0   Max.   :24161.0                     
    ##   prop_donors        nb_fixed_donation_per_act_donor prop_new_donors   
    ##  Min.   :0.0001414   Min.   :1.000                   Min.   :0.000000  
    ##  1st Qu.:0.0011435   1st Qu.:1.000                   1st Qu.:0.000000  
    ##  Median :0.0035714   Median :1.537                   Median :0.000000  
    ##  Mean   :0.0107559   Mean   :1.579                   Mean   :0.001060  
    ##  3rd Qu.:0.0165123   3rd Qu.:1.889                   3rd Qu.:0.001267  
    ##  Max.   :0.1176471   Max.   :5.000                   Max.   :0.029412  
    ##  prop_repeat_donors higher_education
    ##  Min.   :0.000000   Min.   :   0.0  
    ##  1st Qu.:0.000000   1st Qu.: 124.0  
    ##  Median :0.000000   Median : 416.0  
    ##  Mean   :0.006738   Mean   : 809.2  
    ##  3rd Qu.:0.007523   3rd Qu.:1083.0  
    ##  Max.   :0.088235   Max.   :8603.0  
    ##  proportion_inhabitants_with_higher_education
    ##  Min.   :0.0000                              
    ##  1st Qu.:0.1813                              
    ##  Median :0.2540                              
    ##  Mean   :0.2821                              
    ##  3rd Qu.:0.3492                              
    ##  Max.   :0.7696

``` r
save(processed_site_paavo,
file = paste0("name.",Sys.Date(),".RData"))
```
