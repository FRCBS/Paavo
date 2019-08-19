
-----

title: “distance\_function” author: “Ilpo Arminen” date: “8/6/2019”
output: github\_document
—

``` r
 df <- readr::read_tsv("coordinates.txt",col_names= FALSE) # Getting the coordinates from geonames.org, because Geosphere package do not work with eurefin coordinates. 
```

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_character(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_character(),
    ##   X5 = col_character(),
    ##   X6 = col_character(),
    ##   X7 = col_character(),
    ##   X8 = col_character(),
    ##   X9 = col_character(),
    ##   X10 = col_double(),
    ##   X11 = col_double(),
    ##   X12 = col_double()
    ## )

``` r
#load("../data/ilposdata.rdata")
load("/home/ilpo/Paavo/data/site_codes.rdata")
load("/home/ilpo/Paavo/data/processed_site_paavo.RData") # This data is filtered by site 
```

\#loadin the
    file

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.0          ✔ purrr   0.3.2     
    ## ✔ tibble  2.1.3          ✔ dplyr   0.8.2     
    ## ✔ tidyr   0.8.3.9000     ✔ stringr 1.4.0     
    ## ✔ readr   1.3.1          ✔ forcats 0.4.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(geosphere)
library(table1)
```

    ## 
    ## Attaching package: 'table1'

    ## The following objects are masked from 'package:base':
    ## 
    ##     units, units<-

``` r
 df <- readr::read_tsv("coordinates.txt",col_names= FALSE)
```

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_character(),
    ##   X2 = col_character(),
    ##   X3 = col_character(),
    ##   X4 = col_character(),
    ##   X5 = col_character(),
    ##   X6 = col_character(),
    ##   X7 = col_character(),
    ##   X8 = col_character(),
    ##   X9 = col_character(),
    ##   X10 = col_double(),
    ##   X11 = col_double(),
    ##   X12 = col_double()
    ## )

``` r
# This is downloaded from geonames.com. It has all the zip codes of Finland with longitude and latitude. 

  Uusimaa_coordinates <- df %>% 
dplyr::select(-X1,-X5, -X7,-X9,-X12,-X6,-X8) %>%  # unnecessary variables
## filter(X4 == "Uusimaa") %>%           #Filtered to uusimaa region
rename(zip=X2, city= X3, region=X4, longitude= X10, latitude= X11) #renaming the variables 
```

## Filtering the site\_codes\_data

``` {,recho=false}
site_zip <- site_codes_data %>% 
select(c(1,2,5)) %>% #selecting only Site, site name and site zip. 
rename(Site=code, name=site_name, site_zip=zip)   # renamed to match variable names in processed paavo data 


```

``` r
new_data <- left_join(processed_site_paavo, Uusimaa_coordinates, by= c("zip")) # Now preprocessed file has longitude and latitude instead of euref. 
```

``` 
              #Kamppi                 Kiminkylä
```

distHaversine(p1= c(60.1714 , 24.9316), p2= c(60.7121 , 26.2904)) \#
There are quite a few different calculation to distance which all
differs mostly on how round they assume that Earth is. Couple of meters
between Haversine and Vincenty ellipsoid doesnt really matter in this
project. \`\`\` case of a more general formula in spherical
trigonometry, the law of haversines, that relates the sides and angles
of spherical triangles.\# Haversine Formula (from R.W. Sinnott, “Virtues
of the Haversine”, Sky and Telescope, vol. 68, no. 2, 1984, p. 159):

    dlon = lon2 - lon1
    dlat = lat2 - lat1
    a = sin^2(dlat/2) + cos(lat1) * cos(lat2) * sin^2(dlon/2)
    c = 2 * arcsin(min(1,sqrt(a)))
    d = R * c

``` r
###The haversine formula determines the great-circle distance between two points on a sphere given their longitudes and latitudes. VincentyEllipsoid differs on on Haversine on taking more exact picture on how round or ellipsoid the earth sphere is. 
                  #  Kamppi              Punavuori
distHaversine(p1=c(60.1714, 24.9316), p2= c( 60.1632, 24.9391))
```

    ## [1] 1175.666

``` r
distVincentyEllipsoid(p1=c(60.1714, 24.9316), p2= c(60.1632, 24.9391))
```

    ## [1] 1173.101

``` r
# Result in distance between Kamppi and punavuori differs about 2 meters, so its not gonna make a lot of bias on which to choose. 
```

## Calculating the distance between each fixed site and every zip, so that dataset is going to have 4 new variables which tells the distance to each fixed site.

``` r
final_data <- new_data %>% mutate(distKivihaka = distHaversine(cbind(60.2106, 24.9035), cbind(longitude,latitude))) %>% 
mutate(distEspoo = distHaversine(cbind(60.1525, 24.746), cbind(longitude,latitude))) %>% 
  mutate(distSanomatalo = distHaversine(cbind(60.1714, 24.9316), cbind(longitude,latitude))) %>% 
mutate(distLahti = distHaversine(cbind(60.9742, 25.6586), cbind(longitude,latitude)))
```

``` r
label(final_data$medianincome)  <- "Median income of household per postal code"
label(final_data$averageincome) <- "Average income of household per postal code"
label(final_data$proportion_inhabitants_with_higher_education)  <- "Share of inhabitants with higher education per postal code"
label(final_data$prop_donors) <- "Proportion of donors per postal code "
label(final_data$nb_fixed_donations_per_zip) <- "Proportion of donations per postal code"
label(final_data$nb_fixed_donors_per_zip) <- "Number of donors per zip"
label(final_data$proportion_inhabitants_with_higher_education) <- "Proportion_inhabitants_with_higher_education"
label(final_data$nb_fixed_repeat_donors) <- "Number of donors who donated more than once per zip"
label(final_data$nb_fixed_first_time_donors) <- "Number of first time donors per zip"
label(final_data$nb_fixed_donation_per_act_donor) <- "Number of donations divided by numbers of donor per zip"
label(final_data$eligible_population) <- "Number of residents per zip who is suitable for donating blood"
units(final_data$medianincome) <- "Euro"
units(final_data$averageincome) <- "Euro" 

 table1(~prop_donors + eligible_population + nb_fixed_donors_per_zip + nb_fixed_donations_per_zip + nb_fixed_donors_per_zip + medianincome + averageincome + proportion_inhabitants_with_higher_education + proportion_inhabitants_with_higher_education + prop_repeat_donors + prop_new_donors + prop_donors + nb_fixed_donation_per_act_donor,data = final_data, overall = "Total", topclass="Rtable1-grid Rtable1-center")
```

\[1\] "\<table class="Rtable1-grid
Rtable1-center"\>

<thead>

<tr>

<th class="rowlabel firstrow lastrow">

</th>

<th class="firstrow lastrow">

<span class="stratlabel">Total<br><span class="stratn">(n=2014)</span></span>

</th>

</tr>

</thead>

<tbody>

<tr>

<td class="rowlabel firstrow">

<span class="varlabel">Proportion of donors per postal code </span>

</td>

<td class="firstrow">

</td>

</tr>

<tr>

<td class="rowlabel">

Mean (SD)

</td>

<td>

0.0108 (0.0140)

</td>

</tr>

<tr>

<td class="rowlabel lastrow">

Median \[Min, Max\]

</td>

<td class="lastrow">

0.00357 \[0.000141, 0.118\]

</td>

</tr>

<tr>

<td class="rowlabel firstrow">

<span class="varlabel">Number of residents per zip who is suitable for
donating blood</span>

</td>

<td class="firstrow">

</td>

</tr>

<tr>

<td class="rowlabel">

Mean (SD)

</td>

<td>

2430 (2520)

</td>

</tr>

<tr>

<td class="rowlabel lastrow">

Median \[Min, Max\]

</td>

<td class="lastrow">

1600 \[26.0, 18500\]

</td>

</tr>

<tr>

<td class="rowlabel firstrow">

<span class="varlabel">Number of donors per zip</span>

</td>

<td class="firstrow">

</td>

</tr>

<tr>

<td class="rowlabel">

Mean (SD)

</td>

<td>

27.7 (60.8)

</td>

</tr>

<tr>

<td class="rowlabel lastrow">

Median \[Min, Max\]

</td>

<td class="lastrow">

3.00 \[1.00, 552\]

</td>

</tr>

<tr>

<td class="rowlabel firstrow">

<span class="varlabel">Proportion of donations per postal code</span>

</td>

<td class="firstrow">

</td>

</tr>

<tr>

<td class="rowlabel">

Mean (SD)

</td>

<td>

46.7 (103)

</td>

</tr>

<tr>

<td class="rowlabel lastrow">

Median \[Min, Max\]

</td>

<td class="lastrow">

5.00 \[1.00, 929\]

</td>

</tr>

<tr>

<td class="rowlabel firstrow">

<span class="varlabel">Median income of household per postal
code<span class="varunits"> (Euro)</span></span>

</td>

<td class="firstrow">

</td>

</tr>

<tr>

<td class="rowlabel">

Mean (SD)

</td>

<td>

21600 (3340)

</td>

</tr>

<tr>

<td class="rowlabel lastrow">

Median \[Min, Max\]

</td>

<td class="lastrow">

21200 \[10500, 35600\]

</td>

</tr>

<tr>

<td class="rowlabel firstrow">

<span class="varlabel">Average income of household per postal
code<span class="varunits"> (Euro)</span></span>

</td>

<td class="firstrow">

</td>

</tr>

<tr>

<td class="rowlabel">

Mean (SD)

</td>

<td>

24400 (5390)

</td>

</tr>

<tr>

<td class="rowlabel lastrow">

Median \[Min, Max\]

</td>

<td class="lastrow">

23300 \[12100,
1e+05\]

</td>

</tr>

<tr>

<td class="rowlabel firstrow">

<span class="varlabel">Proportion\_inhabitants\_with\_higher\_education</span>

</td>

<td class="firstrow">

</td>

</tr>

<tr>

<td class="rowlabel">

Mean (SD)

</td>

<td>

0.282 (0.136)

</td>

</tr>

<tr>

<td class="rowlabel lastrow">

Median \[Min, Max\]

</td>

<td class="lastrow">

0.254 \[0.00, 0.770\]

</td>

</tr>

<tr>

<td class="rowlabel firstrow">

<span class="varlabel">prop\_repeat\_donors</span>

</td>

<td class="firstrow">

</td>

</tr>

<tr>

<td class="rowlabel">

Mean (SD)

</td>

<td>

0.00674 (0.0124)

</td>

</tr>

<tr>

<td class="rowlabel lastrow">

Median \[Min, Max\]

</td>

<td class="lastrow">

0.00 \[0.00, 0.0882\]

</td>

</tr>

<tr>

<td class="rowlabel firstrow">

<span class="varlabel">prop\_new\_donors</span>

</td>

<td class="firstrow">

</td>

</tr>

<tr>

<td class="rowlabel">

Mean (SD)

</td>

<td>

0.00106 (0.00209)

</td>

</tr>

<tr>

<td class="rowlabel lastrow">

Median \[Min, Max\]

</td>

<td class="lastrow">

0.00 \[0.00, 0.0294\]

</td>

</tr>

<tr>

<td class="rowlabel firstrow">

<span class="varlabel">Number of donations divided by numbers of donor
per zip</span>

</td>

<td class="firstrow">

</td>

</tr>

<tr>

<td class="rowlabel">

Mean (SD)

</td>

<td>

1.58 (0.607)

</td>

</tr>

<tr>

<td class="rowlabel lastrow">

Median \[Min, Max\]

</td>

<td class="lastrow">

1.54 \[1.00,
5.00\]

</td>

</tr>

</tbody>

</table>

"

## Calculating the min distance to the closest donation site. Filtering the proportion of donors bigger than 99 for new dataset.

``` r
 final_data$minDist <- apply(X = final_data[,c("distSanomatalo", "distKivihaka", "distEspoo", "distLahti")],FUN = min,MARGIN = 1,na.rm=TRUE)
```

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf
    
    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf
    
    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf
    
    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf
    
    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf
    
    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning
    ## Inf

``` r
mod_data <- final_data %>% 
filter(nb_fixed_donors_per_zip > 99)
```

``` r
save(mod_data,
file = paste0("name.",Sys.Date(),".RData"))
```
