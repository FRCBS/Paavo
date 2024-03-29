Preprocessing
================

``` r
setwd(dir ="/home/ilpo/Paavo/src")
```

``` r
load("../data/paavodata.rdata")
load("../data/ilposdata.rdata")
```

# Numbers of donors per zip and number of donations per zip

``` r
ilposdata$zip <- gsub('.*NA.*',NA,ilposdata$zip)

prepocessing <- ilposdata %>% 
  mutate(Year = year(dateonly)) %>% 
#  filter(donat_phleb == "K") %>% 
    count(donor, Year,zip) %>% 
    count(Year, zip) %>% 
    filter(Year == 2017 | Year == 2018) %>%
    rename(nb_donors_per_zip = n) 
```

# 

``` r
test <-ilposdata %>% 
  mutate(Year = year(dateonly)) %>%  
#  filter(donat_phleb == "K") %>% 
filter(Year == 2017| Year== 2018) %>% 
count(Year,zip) %>% 
rename(nb_donations_per_zip=n) 
```

``` r
  preprosessing <-left_join(prepocessing,test,
  by = c("zip", "Year"))
```

\#nb\_first\_time\_donors

``` r
firstevent <- ilposdata %>% 
 mutate(Year = year(dateonly)) %>%  
  filter(donat_phleb == "K") %>% 
filter(Year == 2017| Year== 2018) %>% 
select(zip,FirstEvent,Year) %>% 
filter(FirstEvent == TRUE) %>% 
group_by(zip,Year) %>% 
summarise(nb_first_time_donors= n()) %>% 
ungroup()
```

\#nb\_repeat\_donors

``` r
repeatedevent <- ilposdata %>%
 mutate(Year = year(dateonly)) %>%
 filter(Year == 2017| Year== 2018) %>% 
 filter(donat_phleb == "K") %>% 
 group_by(donor, Year) %>% #
filter(dateonly == min(dateonly)) %>% 
 filter(FirstEvent == FALSE) %>%  
 ungroup() %>%
count(zip, Year) %>%  
  rename(nb_repeat_donors= n)
```

``` r
events <- left_join(firstevent,repeatedevent,
by = c("zip","Year"))
```

``` r
preprocessed <- left_join(preprosessing ,events,
  by = c("zip", "Year"))
```

# \#joining the data with Paavodata

``` r
preprosessed_paavo <- paavodata %>% 
  rename(zip = pono, Year= vuosi) %>%   
  filter(Year == 2019) %>% 
  mutate(eligible_population = he_18_19+ he_20_24+ he_25_29+ he_30_34+ he_40_44+
                                  he_45_49+ he_50_54+ he_55_59+ he_60_64+ he_65_69,) %>% 
  dplyr::select(-Year) %>% 
  full_join(preprocessed,by = c("zip")) %>% 
  dplyr::select(zip, Year, eligible_population, nb_donors_per_zip,nb_donations_per_zip, nb_first_time_donors, nb_repeat_donors, hr_mtu, hr_ktu,nimi, ko_al_kork, ko_yl_kork,
pt_tyott, pt_tyoll,  ko_ika18y, hr_tuy) %>% 
    rename (unemployed = pt_tyott,
           employed = pt_tyoll,
           medianincome= hr_mtu,
           averageincome= hr_ktu,
           population18= ko_ika18y,
           bachelor_degree= ko_al_kork,
           masters_degree= ko_yl_kork,
          averageincome= hr_ktu,
          medianincome= hr_mtu) %>% 
mutate( prop_donors= nb_donors_per_zip/eligible_population,
        nb_donation_per_act_donor= nb_donations_per_zip/nb_donors_per_zip,
        prop_new_donors= nb_first_time_donors/eligible_population,
        prop_repeat_donors= nb_repeat_donors/eligible_population,
         higher_education =bachelor_degree+masters_degree, 
  proportion_inhabitants_with_higher_education= higher_education/eligible_population)
```

``` r
save(preprosessed_paavo, file = "preprocessed.RData") 
```
