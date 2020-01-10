BIOL4408 Marine Ecology: Lobster density 2.
================
TimLanglois
09/01/2020

## 2\. Check the data and make corrections.
Load extra librarys

``` r
library(tidyr) #to tidy data
library(dplyr) #to transform data
library(readr) #to write data
library(here) #to make robust links to files

library(forcats) #to transform catagorical data
library(ggplot2) #to plot data
```

Set a study name

``` r
study<-"lobster.density"
```

Use here() to make a shortcut to the “Data” directory.

``` r
data.dir <- here("Analysis_Lobster_density","Data")
```

Read in the data and glimpse the data. glimpse() shows the variable
names, formats, head of the data.

``` r
setwd(data.dir)#this is out shortcut using here()
dir()
```

    ## [1] "dat.csv"                    "lobster.density.csv"       
    ## [3] "lobster.density.gsheet.csv" "status.barplot.png"        
    ## [5] "status.year.sanctuary.png"

``` r
gsheet.dat<-read_csv("lobster.density.gsheet.csv")%>%
  glimpse()
```

    ## Observations: 2,825
    ## Variables: 38
    ## $ year              <dbl> 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 201…
    ## $ date              <dttm> 2014-01-26 16:00:00, 2014-01-26 16:00:00, 2014-01-…
    ## $ sanctuary         <chr> "Armstrong Bay", "Armstrong Bay", "Armstrong Bay", …
    ## $ status            <chr> "No-take", "No-take", "No-take", "No-take", "No-tak…
    ## $ site              <chr> "Little Armstrong", "Little Armstrong", "Little Arm…
    ## $ replicate         <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, …
    ## $ sampling.location <chr> "none", "none", "none", "none", "none", "none", "no…
    ## $ complexity        <dbl> 0, 2, 4, 1, 4, 2, 2, 2, 2, 1, 3, 1, 1, 2, 1, 3, 1, …
    ## $ algal.cover       <dbl> 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, …
    ## $ unsized           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ legal.unsized     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ sublegal.unsized  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x25               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x30               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x35               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, …
    ## $ x40               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x45               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 0, 0, 0, …
    ## $ x50               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x55               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 1, 0, …
    ## $ x60               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x65               <dbl> 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, …
    ## $ x70               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x75               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x80               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x85               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, …
    ## $ x90               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x95               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x100              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x105              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x110              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x115              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x120              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x125              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x130              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x135              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x140              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x145              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x150              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …

This is wide format data.

Check for unique levels of status.

``` r
unique(gsheet.dat$status)
```

    ## [1] "No-take" "Fished"  "No-Take"

there is a typo we need to correct in No-Take

Check for unique levels of
    sanctuary.

``` r
unique(gsheet.dat$sanctuary)
```

    ## [1] "Armstrong Bay" "Parker Point"  "Green Island"  "Parker_Pt"    
    ## [5] "Armstrong"     "Green_Island"

We need to make these consistent.

Check for unique levels of
    site.

``` r
unique(gsheet.dat$site)
```

    ##  [1] "Little Armstrong"     "City of York"         "Ricey Beach"         
    ##  [4] "Parker Point"         "Armstrong Point"      "Salmon Bay"          
    ##  [7] "Green Island"         "Mary Cove"            "Geordie Bay"         
    ## [10] "West Salmon"          "Strickland Bay"       "Parakeet Bay"        
    ## [13] "Stark Bay"            "Rocky Bay"            "Longreach"           
    ## [16] "Fairbridge"           "Little Salmon"        "eastsalmon"          
    ## [19] "East_Salmon"          "Geordie_Bay"          "East_Strickland"     
    ## [22] "Green_Island"         "Ricey_Bay"            "Little_Salmon"       
    ## [25] "City of York Bay"     "Little_Armstrong_Bay" "Poc_Reef"            
    ## [28] "Mary_Cove"            "West_Salmon_Bay"      "Parakeet_Bay"

“eastsalmon” should be “Salmon Bay”

Check for unique levels of year.

``` r
unique(gsheet.dat$year)
```

    ## [1] 2014 2015 2016 2017 2018 2019

We think the data from 2017 has errors in it. We should remove it.

Check measures of habitat complexity. As this is a semi-continous
variable we will use summary() to see the range.

``` r
unique(gsheet.dat$complexity)
```

    ## [1] 0 2 4 1 3

``` r
summary(gsheet.dat$complexity)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   2.000   2.000   2.353   3.000   4.000

The maximum value is supposed to be 4. Looks ok.

Check measures of habitat cover.

``` r
unique(gsheet.dat$algal.cover)
```

    ## [1] 3 2 1 0 4 7

``` r
summary(gsheet.dat$algal.cover)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   1.000   2.000   2.008   3.000   7.000

``` r
unique(filter(gsheet.dat,year=="2019")$algal.cover)
```

    ## [1] 2 1 3 4 0

The maximum value is supposed to be 4. This does not look consistent.
Suggest you only use the most recent years of data.

Check what sites are missing between years.

``` r
table(gsheet.dat$site,gsheet.dat$year)
```

    ##                       
    ##                        2014 2015 2016 2017 2018 2019
    ##   Armstrong Point        28    0    0    0    0    0
    ##   City of York           30   30   30   40   28    0
    ##   City of York Bay        0    0    0    0    0   30
    ##   East_Salmon             0    0    0    0    0   30
    ##   East_Strickland         0    0    0    0    0   30
    ##   eastsalmon              0    0    0    0   30    0
    ##   Fairbridge              0   29   30   40   30   30
    ##   Geordie Bay            30   40   30   40   30    0
    ##   Geordie_Bay             0    0    0    0    0   30
    ##   Green Island           30   20   30   40   30    0
    ##   Green_Island            0    0    0    0    0   30
    ##   Little Armstrong       58   30   30   40   31    0
    ##   Little Salmon           0   30   30   40   39    0
    ##   Little_Armstrong_Bay    0    0    0    0    0   30
    ##   Little_Salmon           0    0    0    0    0   31
    ##   Longreach               0   10    0    0    0    0
    ##   Mary Cove              29   30   30   40   30    0
    ##   Mary_Cove               0    0    0    0    0   30
    ##   Parakeet Bay            0   30   60   80   40    0
    ##   Parakeet_Bay            0    0    0    0    0   30
    ##   Parker Point          105   60   60   80   70    0
    ##   Poc_Reef                0    0    0    0    0   30
    ##   Ricey Beach            30   30   30   40   40    0
    ##   Ricey_Bay               0    0    0    0    0   30
    ##   Rocky Bay               0   33    0    0    0    0
    ##   Salmon Bay             29   30   30   40    0    0
    ##   Stark Bay               0   20    0    0    0    0
    ##   Strickland Bay         25   40   30   40   40    0
    ##   West Salmon            27   23   30   40   40    0
    ##   West_Salmon_Bay         0    0    0    0    0   30

“Armstrong Point”,“Longreach”,“Rocky Bay”,“Stark Bay” - are only sampled
once and we should remove them.

Check how we have used site names and that they are unique between
levels of status?

``` r
table(gsheet.dat$site,gsheet.dat$status)
```

    ##                       
    ##                        Fished No-take No-Take
    ##   Armstrong Point           0      28       0
    ##   City of York            158       0       0
    ##   City of York Bay         30       0       0
    ##   East_Salmon               0      30       0
    ##   East_Strickland          30       0       0
    ##   eastsalmon                0      30       0
    ##   Fairbridge              159       0       0
    ##   Geordie Bay             170       0       0
    ##   Geordie_Bay              30       0       0
    ##   Green Island              0     150       0
    ##   Green_Island              0      30       0
    ##   Little Armstrong          0     189       0
    ##   Little Salmon             0     139       0
    ##   Little_Armstrong_Bay      0       0      30
    ##   Little_Salmon             0      31       0
    ##   Longreach                10       0       0
    ##   Mary Cove                 0     159       0
    ##   Mary_Cove                 0       0      30
    ##   Parakeet Bay            140      70       0
    ##   Parakeet_Bay             30       0       0
    ##   Parker Point            170     205       0
    ##   Poc_Reef                 30       0       0
    ##   Ricey Beach             170       0       0
    ##   Ricey_Bay                30       0       0
    ##   Rocky Bay                33       0       0
    ##   Salmon Bay                0     129       0
    ##   Stark Bay                20       0       0
    ##   Strickland Bay          175       0       0
    ##   West Salmon             160       0       0
    ##   West_Salmon_Bay          30       0       0

## Corrections to the data

We have some corrections to make. It is always good to have our source
data as ‘correct’ as possible. However, in this case we will make these
corrections in our R import script so that we can keep a record of the
changes/corrections we are making to the data.

We can use this list of corrections to go back and check and correct the
raw data if we want.

Make corrections and re-format.

``` r
correct.dat<-gsheet.dat %>%
  dplyr::mutate(status = fct_recode(status,
                                    "No-take" = "No-Take"))%>%
  dplyr::mutate(sanctuary = fct_recode(sanctuary,
                                    "Parker Point" = "Parker_Pt",
                                    "Green Island" = "Green_Island",
                                    "Armstrong Bay" = "Armstrong"))%>%
  
#recode the site names
  
    dplyr::mutate(site = fct_recode(site,
                           "Salmon Bay" = "eastsalmon",
                           "City of York" = "City of York Bay",
                           "Ricey Beach" = "Ricey_Bay",
                           "Salmon Bay" = "East_Salmon",
                           "Little Salmon" = "Little_Salmon",
                           "Parker Point" = "Poc_Reef",
                           "Green Island" = "Green_Island",
                           "Parakeet Bay" = "Parakeet_Bay",
                           "Salmon Bay" = "West_Salmon_Bay",
                           "Little Armstrong" = "Little_Armstrong_Bay",
                           "Geordie Bay" = "Geordie_Bay",
                           "Mary Cove" = "Mary_Cove",
                           "Strickland Bay" = "East_Strickland"))%>%
    
  #filter out sites only done once
    filter(!site%in%c(
    "Armstrong Point",
    "Longreach",
    "Rocky Bay",
    "Stark Bay"))%>%
  
  #filter out suspicous year  
    filter(!year==2017)%>%
  
  #remove the levels for the filtered facotrs
    droplevels()%>%
  
  # Make a new unique Site name - to account for Parker Point being used for inside and outside the NTZ.
  dplyr::mutate(site.new=paste(site,status,sep="."))%>%
  
    glimpse()
```

    ## Observations: 2,134
    ## Variables: 39
    ## $ year              <dbl> 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 201…
    ## $ date              <dttm> 2014-01-26 16:00:00, 2014-01-26 16:00:00, 2014-01-…
    ## $ sanctuary         <fct> Armstrong Bay, Armstrong Bay, Armstrong Bay, Armstr…
    ## $ status            <fct> No-take, No-take, No-take, No-take, No-take, No-tak…
    ## $ site              <fct> Little Armstrong, Little Armstrong, Little Armstron…
    ## $ replicate         <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, …
    ## $ sampling.location <chr> "none", "none", "none", "none", "none", "none", "no…
    ## $ complexity        <dbl> 0, 2, 4, 1, 4, 2, 2, 2, 2, 1, 3, 1, 1, 2, 1, 3, 1, …
    ## $ algal.cover       <dbl> 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, …
    ## $ unsized           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ legal.unsized     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ sublegal.unsized  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x25               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x30               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x35               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, …
    ## $ x40               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x45               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 0, 0, 0, …
    ## $ x50               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x55               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 1, 0, …
    ## $ x60               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x65               <dbl> 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, …
    ## $ x70               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x75               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x80               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x85               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, …
    ## $ x90               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x95               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x100              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x105              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x110              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x115              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x120              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x125              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x130              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x135              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x140              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x145              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x150              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ site.new          <chr> "Little Armstrong.No-take", "Little Armstrong.No-ta…

Now we can check our corrections

``` r
glimpse(correct.dat)
```

    ## Observations: 2,134
    ## Variables: 39
    ## $ year              <dbl> 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 201…
    ## $ date              <dttm> 2014-01-26 16:00:00, 2014-01-26 16:00:00, 2014-01-…
    ## $ sanctuary         <fct> Armstrong Bay, Armstrong Bay, Armstrong Bay, Armstr…
    ## $ status            <fct> No-take, No-take, No-take, No-take, No-take, No-tak…
    ## $ site              <fct> Little Armstrong, Little Armstrong, Little Armstron…
    ## $ replicate         <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, …
    ## $ sampling.location <chr> "none", "none", "none", "none", "none", "none", "no…
    ## $ complexity        <dbl> 0, 2, 4, 1, 4, 2, 2, 2, 2, 1, 3, 1, 1, 2, 1, 3, 1, …
    ## $ algal.cover       <dbl> 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, …
    ## $ unsized           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ legal.unsized     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ sublegal.unsized  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x25               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x30               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x35               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, …
    ## $ x40               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x45               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 0, 0, 0, …
    ## $ x50               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x55               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 1, 0, …
    ## $ x60               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x65               <dbl> 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, …
    ## $ x70               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x75               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x80               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x85               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, …
    ## $ x90               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x95               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x100              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x105              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x110              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x115              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x120              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x125              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x130              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x135              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x140              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x145              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ x150              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ site.new          <chr> "Little Armstrong.No-take", "Little Armstrong.No-ta…

``` r
unique(correct.dat$sanctuary) 
```

    ## [1] Armstrong Bay Parker Point  Green Island 
    ## Levels: Armstrong Bay Green Island Parker Point

``` r
unique(correct.dat$site) 
```

    ##  [1] Little Armstrong City of York     Ricey Beach      Parker Point    
    ##  [5] Salmon Bay       Green Island     Mary Cove        Geordie Bay     
    ##  [9] West Salmon      Strickland Bay   Parakeet Bay     Fairbridge      
    ## [13] Little Salmon   
    ## 13 Levels: City of York Salmon Bay Strickland Bay Fairbridge ... West Salmon

``` r
unique(correct.dat$status)
```

    ## [1] No-take Fished 
    ## Levels: Fished No-take

``` r
table(correct.dat$site.new,correct.dat$status)
```

    ##                           
    ##                            Fished No-take
    ##   City of York.Fished         148       0
    ##   Fairbridge.Fished           119       0
    ##   Geordie Bay.Fished          160       0
    ##   Green Island.No-take          0     140
    ##   Little Armstrong.No-take      0     179
    ##   Little Salmon.No-take         0     130
    ##   Mary Cove.No-take             0     149
    ##   Parakeet Bay.Fished         130       0
    ##   Parakeet Bay.No-take          0      30
    ##   Parker Point.Fished         160       0
    ##   Parker Point.No-take          0     165
    ##   Ricey Beach.Fished          160       0
    ##   Salmon Bay.Fished            30       0
    ##   Salmon Bay.No-take            0     149
    ##   Strickland Bay.Fished       165       0
    ##   West Salmon.Fished          120       0

Now make new varialbes for sum of legal and sub.legal. Make the data
long format.

``` r
dat<-correct.dat%>%
  
  replace(is.na(.), 0)%>%
  
  mutate_at(vars(starts_with("x")), funs(ifelse(is.na(.),0,.)))%>%
  mutate_at(vars(starts_with("unsi")), funs(ifelse(is.na(.),0,.)))%>%
  mutate_at(vars(starts_with("legal")), funs(ifelse(is.na(.),0,.)))%>%
  mutate_at(vars(starts_with("sub")), funs(ifelse(is.na(.),0,.)))%>%
  
  # make the legal sum
  dplyr::mutate(legal=(legal.unsized+x80+x85+x90+x95+x100+x105+x110+x115+x120+x125+x130+x135+x140+x145+x150))%>%
  
  # make the sub.legal sum
  dplyr::mutate(sub.legal=(sublegal.unsized+x25+x30+x35+x40+x45+x50+x55+x60+x65+x70+x75))%>%
  
  # make an all sum
  dplyr::mutate(all=(legal+sub.legal+unsized))%>%
  
  # make a unique sample number
  dplyr::mutate(sample.no=1:nrow(.))%>%
  
  # select the variables of interest
  select(c(sample.no,year,date,sanctuary,status,site.new,complexity,algal.cover,legal,sub.legal,all))%>%
  
  # # # make the data long
  gather(key="size.class",value="count",legal, sub.legal,all)%>%
  glimpse()
```

    ## Warning: funs() is soft deprecated as of dplyr 0.8.0
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once per session.

    ## Observations: 6,402
    ## Variables: 10
    ## $ sample.no   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17…
    ## $ year        <dbl> 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 201…
    ## $ date        <dttm> 2014-01-26 16:00:00, 2014-01-26 16:00:00, 2014-01-26 16:…
    ## $ sanctuary   <fct> Armstrong Bay, Armstrong Bay, Armstrong Bay, Armstrong Ba…
    ## $ status      <fct> No-take, No-take, No-take, No-take, No-take, No-take, No-…
    ## $ site.new    <chr> "Little Armstrong.No-take", "Little Armstrong.No-take", "…
    ## $ complexity  <dbl> 0, 2, 4, 1, 4, 2, 2, 2, 2, 1, 3, 1, 1, 2, 1, 3, 1, 2, 2, …
    ## $ algal.cover <dbl> 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 2, 2, …
    ## $ size.class  <chr> "legal", "legal", "legal", "legal", "legal", "legal", "le…
    ## $ count       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, …

Write the data

``` r
setwd(data.dir) #set the directory
dir() #look in the directory
```

    ## [1] "dat.csv"                    "lobster.density.csv"       
    ## [3] "lobster.density.gsheet.csv" "status.barplot.png"        
    ## [5] "status.year.sanctuary.png"

``` r
# Write dat using study name
write_csv(dat,paste(study,"csv",sep = "."))
```

[Go to Lobster denisty 3. Basic plots to check the
data](https://github.com/TimLanglois/BIOL4408/blob/c5aa4f8881ad41e1352c8618091e4de530171b0f/Analysis_Lobster_density/3_lobster-density_basic-plots-to-check-data.md)
