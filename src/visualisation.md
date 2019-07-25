Visualisation
================

``` r
load("/home/ilpo/Paavo/data/preprocessed.2019-07-24.RData")
```

``` r
knitr::opts_chunk$set(echo = TRUE)
```

``` r
setwd(dir="/home/ilpo/Paavo/src")
getwd()
```

    ## [1] "/home/ilpo/Paavo/src"

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like
    this:

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.0          ✔ purrr   0.3.2     
    ## ✔ tibble  2.1.3          ✔ dplyr   0.8.2     
    ## ✔ tidyr   0.8.3.9000     ✔ stringr 1.4.0     
    ## ✔ readr   1.3.1          ✔ forcats 0.4.0

    ## ── Conflicts ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
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

``` r
library(viridis)
```

    ## Loading required package: viridisLite

# Relationship between proportion of donors and median income.

``` r
df <- as.tbl(preprosessed_paavo) %>% filter(!is.na(prop_donors))
p <- ggplot(data=df)
p <- p +  geom_point(mapping=aes(y = prop_donors, x = medianincome, colour=eligible_population))
p <- p +   scale_x_log10()+
  geom_smooth(aes(y = prop_donors, x = medianincome)) +
 scale_color_viridis(discrete=FALSE,direction = -1,trans="log")+
labs(x = "Median income",
        y = "Proportion of donors",
        title = "Proportion of donors per median income of postal codes per year")+
facet_grid(Year ~.)
p
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 6 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](visualisation_files/figure-gfm/unnamed-chunk-4-1.png)<!-- --> \# New
donors and repeated donors per median income

``` r
df <- as.tbl(preprosessed_paavo) %>% filter(!is.na(prop_new_donors))
p <- ggplot(data=df)
p <- p +  geom_point(mapping=aes(y = prop_new_donors, x = medianincome, colour=eligible_population))+
  scale_color_viridis(discrete=FALSE,direction = -1,trans="log")+
  facet_grid(Year~.)
p
```

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](visualisation_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
df <- as.tbl(preprosessed_paavo) %>% filter(!is.na(prop_donors))
p <- ggplot(data=df)
p <- p +  geom_point(mapping=aes(y = prop_repeat_donors, x = medianincome, colour=eligible_population))+
  scale_color_viridis(discrete=FALSE,direction = -1,trans="log")+
geom_smooth(aes(y = prop_repeat_donors, x = medianincome)) + 
facet_grid (Year~.)
p
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 6 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](visualisation_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Average income

``` r
df <- as.tbl(preprosessed_paavo) %>% filter(!is.na(prop_donors))
p <- ggplot(data=df)
p <- p +  geom_point(mapping=aes(y = prop_donors, x = averageincome, colour=eligible_population))
p <- p +   scale_x_log10() + geom_smooth(aes(y = prop_donors, x = medianincome)) +
 scale_color_viridis(discrete=FALSE,direction = -1,trans="log")+
labs(x = "Average income",
        y = "Proportion of donors",
        title = "Proportion of donors per average income of postal codes per year")+
facet_grid(Year ~.)
p
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 6 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](visualisation_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
df <- as.tbl(preprosessed_paavo) %>% filter(!is.na(prop_new_donors))
p <- ggplot(data=df)
p <- p +  geom_point(mapping=aes(y = prop_new_donors, x = averageincome, colour=eligible_population))
p <- p +   scale_x_log10() + geom_smooth(aes(y = prop_new_donors, x = averageincome)) +
 scale_color_viridis(discrete=FALSE,direction = -1,trans="log")+
labs(x = "Average income",
        y = "Proportion of first time donors",
        title = "Proportion of donors per average income of postal codes per year")+
facet_grid(Year ~.)
p
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 6 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](visualisation_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
df <- as.tbl(preprosessed_paavo) %>% filter(!is.na(prop_repeat_donors))
p <- ggplot(data=df)
p <- p +  geom_point(mapping=aes(y = prop_repeat_donors, x = averageincome, colour=eligible_population))
p <- p +   scale_x_log10() + geom_smooth(aes(y = prop_repeat_donors, x = averageincome)) +
 scale_color_viridis(discrete=FALSE,direction = -1,trans="log")+
labs(x = "Average income",
        y = "Proportion of non repeat donors",
        title = "Proportion of repeat  donors per average income of postal codes per year")+
facet_grid(Year ~.)
p
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 6 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](visualisation_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Relationship between higher education and proportion of donors

``` r
p <- ggplot(data=df)
p <- p +  geom_point(mapping=aes(y = prop_donors, x = proportion_inhabitants_with_higher_education, colour=eligible_population))
p <- p +   scale_x_log10() + geom_smooth(aes(y = prop_donors, x = proportion_inhabitants_with_higher_education)) +
scale_color_viridis(discrete=FALSE,direction = -1,trans="log") + 
  facet_grid(Year~.)
p
```

    ## Warning: Transformation introduced infinite values in continuous x-axis
    
    ## Warning: Transformation introduced infinite values in continuous x-axis

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 15 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](visualisation_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> \#
First time donors

``` r
p <- ggplot(data=df)
p <- p +  geom_point(mapping=aes(y = prop_new_donors, x = proportion_inhabitants_with_higher_education, colour=eligible_population))
p <- p +   scale_x_log10() + geom_smooth(aes(y = prop_new_donors, x = proportion_inhabitants_with_higher_education)) +
scale_color_viridis(discrete=FALSE,direction = -1,trans="log") +
  facet_grid(Year~.)
p
```

    ## Warning: Transformation introduced infinite values in continuous x-axis
    
    ## Warning: Transformation introduced infinite values in continuous x-axis

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 15 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](visualisation_files/figure-gfm/unnamed-chunk-11-1.png)<!-- --> \#
repeated donors

``` r
p <- ggplot(data=df)
p <- p +  geom_point(mapping=aes(y = prop_repeat_donors, x = proportion_inhabitants_with_higher_education, colour=eligible_population))
p <- p +   scale_x_log10() + 
geom_smooth(aes(y = prop_repeat_donors, x = proportion_inhabitants_with_higher_education)) +
scale_color_viridis(discrete=FALSE,direction = -1,trans="log") +
  facet_grid(Year~.)
p
```

    ## Warning: Transformation introduced infinite values in continuous x-axis
    
    ## Warning: Transformation introduced infinite values in continuous x-axis

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 15 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](visualisation_files/figure-gfm/unnamed-chunk-12-1.png)<!-- --> \#
Helsinki-Uusimaa

``` r
huuma <-preprosessed_paavo %>% 
filter(zip < 1) 

ggplot(data=huuma, mapping = aes(x= medianincome, y= prop_donors, color= eligible_population))+
geom_point(mapping = aes(x= medianincome, y= prop_donors)) +
geom_smooth () + 
scale_x_log10() +
scale_color_viridis(discrete=FALSE,direction = -1,trans="log") + 
facet_grid(Year~.)
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](visualisation_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
huuma <-preprosessed_paavo %>% 
filter(zip < 1) 

ggplot(data=huuma, mapping = aes(x= proportion_inhabitants_with_higher_education, y= prop_donors, color= eligible_population))+
geom_point(mapping = aes(x= proportion_inhabitants_with_higher_education, y= prop_donors)) +
geom_smooth () + 
scale_x_log10() +
scale_color_viridis(discrete=FALSE,direction = -1,trans="log") +
facet_grid(Year~.)
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](visualisation_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
ggplot(data=huuma, mapping = aes(x= medianincome, y= prop_donors, color= eligible_population))+
geom_point(mapping = aes(x= medianincome, y= prop_donors)) +
geom_smooth () + 
scale_x_log10() +
scale_color_viridis(discrete=FALSE,direction = -1,trans="log") + 
facet_grid(Year~.)
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](visualisation_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

\#new
donors

``` r
ggplot(data=huuma, mapping = aes(x= medianincome, y= prop_new_donors, color= eligible_population))+
geom_point(mapping = aes(x= medianincome, y= prop_new_donors)) +
geom_smooth () + 
scale_x_log10() +
scale_color_viridis(discrete=FALSE,direction = -1,trans="log") + 
facet_grid(Year~.)
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](visualisation_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

\#repeat
donors

``` r
ggplot(data=huuma, mapping = aes(x= medianincome, y= prop_repeat_donors, color= eligible_population))+
geom_point(mapping = aes(x= medianincome, y= prop_repeat_donors)) +
geom_smooth () + 
scale_x_log10() +
scale_color_viridis(discrete=FALSE,direction = -1,trans="log") + 
facet_grid(Year~.)
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](visualisation_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
ggplot(data=huuma, mapping = aes(x= medianincome, y= prop_donors, group= zip, color=eligible_population))+
geom_point(mapping = aes(x= medianincome, y= prop_donors)) +
#geom_smooth () + 
scale_x_log10() +
scale_color_viridis(discrete=FALSE,direction = -1,trans="log") + 
geom_boxplot() 
```

![](visualisation_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
#coord_flip()
```

# Not very informative part

``` r
 ggplot(data = huuma,
            mapping = aes(x = medianincome, y = prop_donors))+
geom_point(alpha= 0.25) +
scale_x_log10() + 
  geom_text(data = huuma,check_overlap = TRUE, size=3,
mapping = aes(label = nimi)) + 
facet_grid(Year ~.)
```

![](visualisation_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
 ggplot(data = huuma,
            mapping = aes(x = proportion_inhabitants_with_higher_education, y = prop_donors))+
geom_point(alpha= 0.25) +
scale_x_log10() + 
  geom_text(data = huuma,check_overlap = TRUE,
mapping = aes(label = nimi)) +
facet_grid(Year ~.)
```

![](visualisation_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

# colored by repeated donors

``` r
ggplot(data=preprosessed_paavo, mapping= aes(x= medianincome, y= prop_donors,  color=prop_repeat_donors))+
geom_point ()+ 
scale_x_log10() +
geom_smooth()+
scale_color_viridis(discrete=FALSE,direction = -1,trans="log") +
facet_wrap (Year~.)
```

    ## Warning: Transformation introduced infinite values in discrete y-axis
    
    ## Warning: Transformation introduced infinite values in discrete y-axis

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 6 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](visualisation_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
ggplot(data=preprosessed_paavo, mapping= aes(x= medianincome, y= prop_donors,  color=prop_new_donors))+
geom_point ()+ 
scale_x_log10() +
geom_smooth()+
scale_color_viridis(discrete=FALSE,direction = -1,trans="log") +
facet_wrap (Year~.)
```

    ## Warning: Transformation introduced infinite values in discrete y-axis
    
    ## Warning: Transformation introduced infinite values in discrete y-axis

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 6 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](visualisation_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
ggplot(data=preprosessed_paavo, mapping= aes(x= prop_new_donors))+
geom_histogram ()+
scale_x_log10()
```

    ## Warning: Transformation introduced infinite values in continuous x-axis

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2047 rows containing non-finite values (stat_bin).

![](visualisation_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
facet_grid(Year~.)
```

    ## <ggproto object: Class FacetGrid, Facet, gg>
    ##     compute_layout: function
    ##     draw_back: function
    ##     draw_front: function
    ##     draw_labels: function
    ##     draw_panels: function
    ##     finish_data: function
    ##     init_scales: function
    ##     map_data: function
    ##     params: list
    ##     setup_data: function
    ##     setup_params: function
    ##     shrink: TRUE
    ##     train_scales: function
    ##     vars: function
    ##     super:  <ggproto object: Class FacetGrid, Facet, gg>

``` r
ggplot(data=preprosessed_paavo, mapping= aes(x= prop_repeat_donors))+
geom_histogram ()+
scale_x_log10()+
  facet_grid(Year~.)
```

    ## Warning: Transformation introduced infinite values in continuous x-axis

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2101 rows containing non-finite values (stat_bin).

![](visualisation_files/figure-gfm/unnamed-chunk-23-2.png)<!-- -->

``` r
ggplot(data=preprosessed_paavo, mapping= aes(x= prop_donors))+
geom_histogram() +
scale_x_log10()+
facet_grid(Year~.)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](visualisation_files/figure-gfm/unnamed-chunk-23-3.png)<!-- -->
