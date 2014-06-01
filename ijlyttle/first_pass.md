# First pass through the data

This will be a stream-of-concoiusness walk through the data, a first pass to see what I can see.

## Libraries

At some point, we should make sure we agree on which versions of which packages we propose to use. As a starting point, I can show what I am using:


```r
library(stringr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggvis)
```

Here's my versions:


```r
sessionInfo()
```

```
## R version 3.0.3 (2014-03-06)
## Platform: x86_64-apple-darwin10.8.0 (64-bit)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] ggvis_0.2.0.99  lubridate_1.3.3 dplyr_0.2.0.99  ggplot2_0.9.3.1
## [5] stringr_0.6.2   knitr_1.5.33   
## 
## loaded via a namespace (and not attached):
##  [1] assertthat_0.1     bitops_1.0-6       caTools_1.16      
##  [4] colorspace_1.2-4   dichromat_2.0-0    digest_0.6.4      
##  [7] evaluate_0.5.5     formatR_0.10       grid_3.0.3        
## [10] gtable_0.1.2       httpuv_1.2.3       labeling_0.2      
## [13] magrittr_1.0.0     MASS_7.3-29        memoise_0.1       
## [16] munsell_0.4.2      parallel_3.0.3     plyr_1.8.1        
## [19] proto_0.3-10       RColorBrewer_1.0-5 Rcpp_0.11.1       
## [22] reshape2_1.4       RJSONIO_1.0-3      scales_0.2.3      
## [25] shiny_0.9.1.9007   tools_3.0.3        xtable_1.7-1
```

I suspect that for publication, `ggplot2` will be more capable than `ggvis`.

# Load the data


```r
sets <- c("item", "parent", "school", "scoredItem", "student")

# function to build the file names
fn_build <- function(file_name){
 
  template <- c("2012.rda", "2012dict.rda")
 
  file_name %>% 
    vapply(str_join, template, template) %>% 
    file.path("..", "data", .)
}

# load the data
sets %>% fn_build %>% lapply(load, .GlobalEnv)
```

```
## [[1]]
## [1] "item2012"
## 
## [[2]]
## [1] "item2012dict"
## 
## [[3]]
## [1] "parent2012"
## 
## [[4]]
## [1] "parent2012dict"
## 
## [[5]]
## [1] "school2012"
## 
## [[6]]
## [1] "school2012dict"
## 
## [[7]]
## [1] "scoredItem2012"
## 
## [[8]]
## [1] "scoredItem2012dict"
## 
## [[9]]
## [1] "student2012"
## 
## [[10]]
## [1] "student2012dict"
```

```r
# clean
rm(fn_build, sets)
```

The dictionaries are stored as named vectors - I would prefer data frames so that I can use the RStudio GUI to examine them.


```r
# function to convert to data-frames
fn_make_df <- function(named_vector){
  data.frame(
    variable = attr(named_vector, "names"),
    description = named_vector,
    row.names = NULL
  )
}

# there's a clever way to do this, but beyond me for naw
dict_item2012 <- fn_make_df(item2012dict) 
dict_parent2012 <- fn_make_df(parent2012dict) 
dict_school2012 <- fn_make_df(school2012dict) 
dict_scoredItem2012 <- fn_make_df(scoredItem2012dict) 
dict_student2012 <- fn_make_df(student2012dict) 

# clean
rm(fn_make_df)
rm(item2012dict, parent2012dict, school2012dict, scoredItem2012dict, student2012dict)
```

Looking at the number of variables in each of the data frames, I wish I knew how to use ggobi...


