Exploring maps
========================================================



```r
library(stringr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggvis)
library(maps)
library(ggmap)
library(rworldmap)
library(grid)
library(scales)
```



```r
setwd("..")
sets <- c("item", "parent", "school", "scoredItem", "student")

# function to build the file names
fn_build <- function(file_name) {
    
    template <- c("2012.rda", "2012dict.rda")
    
    file_name %>% vapply(str_join, template, template) %>% file.path(".", "data", 
        .)
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

# clean rm(fn_build, sets)
```



```r
# function to convert to data-frames
fn_make_df <- function(named_vector) {
    data.frame(variable = attr(named_vector, "names"), description = named_vector, 
        row.names = NULL)
}

# there's a clever way to do this, but beyond me for naw
dict_item2012 <- fn_make_df(item2012dict)
dict_parent2012 <- fn_make_df(parent2012dict)
dict_school2012 <- fn_make_df(school2012dict)
dict_scoredItem2012 <- fn_make_df(scoredItem2012dict)
dict_student2012 <- fn_make_df(student2012dict)

# clean rm(fn_make_df) rm(item2012dict, parent2012dict, school2012dict,
# scoredItem2012dict, student2012dict)
```



```r
extractPolygons <- function(shapes) {
    
    dframe <- ldply(1:length(shapes@polygons), function(i) {
        ob <- shapes@polygons[[i]]@Polygons
        dframe <- ldply(1:length(ob), function(j) {
            x <- ob[[j]]
            co <- x@coords
            data.frame(co, order = 1:nrow(co), group = j)
        })
        dframe$region <- i
        dframe$name <- shapes@polygons[[i]]@ID
        dframe
    })
    # construct a group variable from both group and polygon:
    dframe$group <- interaction(dframe$region, dframe$group)
    
    dframe
}

# To get a blank background on map
new_theme_empty <- theme_bw()
new_theme_empty$line <- element_blank()
new_theme_empty$rect <- element_blank()
new_theme_empty$strip.text <- element_blank()
new_theme_empty$axis.text <- element_blank()
new_theme_empty$plot.title <- element_blank()
new_theme_empty$axis.title <- element_blank()
new_theme_empty$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, 
    class = "unit")
```



```r
sort(table(student2012$CNT))
```

```
## 
##            Liechtenstein        Connecticut (USA)      Massachusetts (USA) 
##                      293                     1697                     1723 
## Perm(Russian Federation)            Florida (USA)                  Iceland 
##                     1761                     1896                     3508 
##              New Zealand                   Latvia                  Tunisia 
##                     4291                     4306                     4407 
##              Netherlands               Costa Rica                   Poland 
##                     4460                     4602                     4607 
##                   France                Lithuania          Hong Kong-China 
##                     4613                     4618                     4670 
##          Slovak Republic                   Serbia                   Norway 
##                     4678                     4684                     4686 
##                   Sweden                  Albania               Montenegro 
##                     4736                     4743                     4744 
##                  Austria                  Estonia                  Hungary 
##                     4755                     4779                     4810 
##                   Turkey                  Vietnam United States of America 
##                     4848                     4959                     4978 
##                  Germany                  Croatia                  Ireland 
##                     5001                     5008                     5016 
##                    Korea                   Israel                  Romania 
##                     5033                     5055                     5074 
##                   Greece           China-Shanghai                 Malaysia 
##                     5125                     5177                     5197 
##       Russian Federation               Luxembourg                 Bulgaria 
##                     5231                     5258                     5282 
##                  Uruguay           Czech Republic              Macao-China 
##                     5315                     5327                     5335 
##                Singapore                Indonesia                 Portugal 
##                     5546                     5622                     5722 
##               Kazakhstan                Argentina                 Slovenia 
##                     5808                     5908                     5911 
##                     Peru           Chinese Taipei                    Japan 
##                     6035                     6046                     6351 
##                 Thailand                    Chile                   Jordan 
##                     6606                     6856                     7038 
##                  Denmark                  Belgium                  Finland 
##                     7481                     8597                     8829 
##                 Colombia                    Qatar              Switzerland 
##                     9073                    10966                    11229 
##     United Arab Emirates           United Kingdom                Australia 
##                    11500                    12659                    14481 
##                   Brazil                   Canada                    Spain 
##                    19204                    21544                    25313 
##                    Italy                   Mexico 
##                    31073                    33806
```

```r
schools.sampled <- summarise(group_by(student2012[, c(1, 6)], CNT), numschools = length(unique(SCHOOLID)))
schools.sampled[order(schools.sampled$numschools, decreasing = T), ]
```

```
## Source: local data frame [68 x 2]
## 
##                         CNT numschools
## 40                   Mexico       1471
## 30                    Italy       1194
## 17                    Spain        902
## 9                    Canada        885
## 8                    Brazil        839
## 4                 Australia        775
## 21           United Kingdom        507
## 2      United Arab Emirates        458
## 10              Switzerland        411
## 12                 Colombia        352
## 16                  Denmark        341
## 60                 Slovenia        338
## 19                  Finland        311
## 14           Czech Republic        297
## 6                   Belgium        287
## 46                     Peru        240
## 63                 Thailand        239
## 31                   Jordan        233
## 59          Slovak Republic        231
## 15                  Germany        230
## 56       Russian Federation        227
## 3                 Argentina        226
## 20                   France        226
## 11                    Chile        221
## 33               Kazakhstan        218
## 36                Lithuania        216
## 38                   Latvia        211
## 26                Indonesia        209
## 61                   Sweden        209
## 18                  Estonia        206
## 1                   Albania        204
## 25                  Hungary        204
## 44                   Norway        197
## 48                 Portugal        195
## 13               Costa Rica        193
## 5                   Austria        191
## 32                    Japan        191
## 7                  Bulgaria        188
## 22                   Greece        188
## 47                   Poland        184
## 27                  Ireland        183
## 66                  Uruguay        180
## 43              Netherlands        179
## 55                  Romania        178
## 45              New Zealand        177
## 29                   Israel        172
## 57                Singapore        172
## 65                   Turkey        170
## 42                 Malaysia        164
## 24                  Croatia        163
## 62           Chinese Taipei        163
## 67 United States of America        162
## 68                  Vietnam        162
## 49                    Qatar        157
## 34                    Korea        156
## 50           China-Shanghai        155
## 58                   Serbia        153
## 64                  Tunisia        153
## 23          Hong Kong-China        148
## 28                  Iceland        134
## 51 Perm(Russian Federation)         63
## 52            Florida (USA)         54
## 41               Montenegro         51
## 53        Connecticut (USA)         50
## 54      Massachusetts (USA)         49
## 39              Macao-China         45
## 37               Luxembourg         42
## 35            Liechtenstein         12
```



```r
# Extract map polygons for modern world
world <- getMap(resolution = "low")
library(plyr)
```

```
## -------------------------------------------------------------------------
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
## -------------------------------------------------------------------------
## 
## Attaching package: 'plyr'
## 
## The following object is masked from 'package:lubridate':
## 
##     here
## 
## The following objects are masked from 'package:dplyr':
## 
##     arrange, desc, failwith, id, mutate, summarise, summarize
```

```r
world.polys <- extractPolygons(world)
detach("package:plyr")
# Subset data
student2012.sub <- student2012[, c(1:7, seq(501, 550, 5))]
colnames(student2012.sub)[1] <- "name"
student2012.sub$name <- as.character(student2012.sub$name)
# Check mismatches of names unique(anti_join(student2012.sub,
# world.polys)[1])
student2012.sub$name[student2012.sub$name == "Serbia"] <- "Republic of Serbia"
student2012.sub$name[student2012.sub$name == "Korea"] <- "South Korea"
student2012.sub$name[student2012.sub$name == "Chinese Taipei"] <- "Taiwan"
student2012.sub$name[student2012.sub$name == "Slovak Republic"] <- "Slovakia"
student2012.sub$name[student2012.sub$name == "Russian Federation"] <- "Russia"
student2012.sub$name[student2012.sub$name == "Perm(Russian Federation)"] <- "Russia"
student2012.sub$name[student2012.sub$name == "Hong Kong-China"] <- "Hong Kong S.A.R."
student2012.sub$name[student2012.sub$name == "China-Shanghai"] <- "China"
student2012.sub$name[student2012.sub$name == "China-Macau"] <- "China"
student2012.sub$name[student2012.sub$name == "Connecticut (USA)"] <- "United States of America"
student2012.sub$name[student2012.sub$name == "Florida (USA)"] <- "United States of America"
student2012.sub$name[student2012.sub$name == "Massachusetts (USA)"] <- "United States of America"

# Only need one of the plausible values, checked they are effectively
# identical
student2012.sub$PV1MATH <- as.numeric(student2012.sub$PV1MATH)
student2012.sub$PV1MACC <- as.numeric(student2012.sub$PV1MACC)
student2012.sub$PV1MACQ <- as.numeric(student2012.sub$PV1MACQ)
student2012.sub$PV1MACS <- as.numeric(student2012.sub$PV1MACS)
student2012.sub$PV1MACU <- as.numeric(student2012.sub$PV1MACU)
student2012.sub$PV1MAPE <- as.numeric(student2012.sub$PV1MAPE)
student2012.sub$PV1MAPF <- as.numeric(student2012.sub$PV1MAPF)
student2012.sub$PV1MAPI <- as.numeric(student2012.sub$PV1MAPI)
student2012.sub$PV1READ <- as.numeric(student2012.sub$PV1READ)
student2012.sub$PV1SCIE <- as.numeric(student2012.sub$PV1SCIE)
student2012.sub.summary <- summarise(group_by(student2012.sub[, c(1, 8:17)], 
    name), math = mean(PV1MATH, na.rm = T), mCC = mean(PV1MACC, na.rm = T), 
    mCQ = mean(PV1MACQ, na.rm = T), mCS = mean(PV1MACS, na.rm = T), mCU = mean(PV1MACU, 
        na.rm = T), mPE = mean(PV1MAPE, na.rm = T), mPF = mean(PV1MAPF, na.rm = T), 
    mPI = mean(PV1MAPI, na.rm = T), read = mean(PV1READ, na.rm = T), science = mean(PV1SCIE, 
        na.rm = T), mathr = diff(range(PV1MATH, na.rm = T)), mCCr = diff(range(PV1MACC, 
        na.rm = T)), mCQr = diff(range(PV1MACQ, na.rm = T)), mCSr = diff(range(PV1MACS, 
        na.rm = T)), mCUr = diff(range(PV1MACU, na.rm = T)), mPEr = diff(range(PV1MAPE, 
        na.rm = T)), mPFr = diff(range(PV1MAPF, na.rm = T)), mPIr = diff(range(PV1MAPI, 
        na.rm = T)), readr = diff(range(PV1READ, na.rm = T)), sciencer = diff(range(PV1SCIE, 
        na.rm = T)))
```

```
## Warning: no non-missing arguments to min; returning Inf
## Warning: no non-missing arguments to max; returning -Inf
## Warning: no non-missing arguments to min; returning Inf
## Warning: no non-missing arguments to max; returning -Inf
## Warning: no non-missing arguments to min; returning Inf
## Warning: no non-missing arguments to max; returning -Inf
## Warning: no non-missing arguments to min; returning Inf
## Warning: no non-missing arguments to max; returning -Inf
## Warning: no non-missing arguments to min; returning Inf
## Warning: no non-missing arguments to max; returning -Inf
## Warning: no non-missing arguments to min; returning Inf
## Warning: no non-missing arguments to max; returning -Inf
## Warning: no non-missing arguments to min; returning Inf
## Warning: no non-missing arguments to max; returning -Inf
```

```r
colnames(student2012.sub.math)[3:9] <- c("Change", "Quantity", "Spatial", "Data", 
    "Employ", "Formulate", "Interpret")
```

```
## Error: object 'student2012.sub.math' not found
```

```r

# Left join to only get countries that are measured
student2012.sub.map <- left_join(student2012.sub.summary, world.polys)
```

```
## Joining by: "name"
```

```r
# qplot(X1, X2, order=order, group=group, data=student2012.sub.map,
# geom='polygon', fill=math) + coord_map() + new_theme_empty

# Really need all boundaries, doesn't quite work to have boundaries with one
# data: coord_map is the problem
ggplot(data = world.polys) + geom_path(aes(x = X1, y = X2, order = order, group = group), 
    colour = I("grey70")) + geom_polygon(data = student2012.sub.map, aes(x = X1, 
    y = X2, order = order, group = group, fill = math)) + new_theme_empty + 
    theme(legend.position = "none")
```

![plot of chunk maps](figure/maps1.png) 

```r

# Now try to do an insert Note China is represented just Shanghai and Macau
# Individual states in the USA, Florida, Mass, Conn are included as US
# Russian Federation has two groups intributinf
student2012.sub.summary$name <- factor(student2012.sub.summary$name, levels = student2012.sub.summary$name[order(student2012.sub.summary$math)])
p1 <- ggplot(data = world.polys) + geom_path(aes(x = X1, y = X2, order = order, 
    group = group), colour = I("grey70")) + geom_polygon(data = student2012.sub.map, 
    aes(x = X1, y = X2, order = order, group = group, fill = math)) + new_theme_empty + 
    theme(legend.position = "none")
p2 <- qplot(name, math, data = student2012.sub.summary, colour = math, ylab = "Math Score", 
    xlab = "") + coord_flip() + theme(legend.position = "none")
p2 = ggplotGrob(p2)
p1 + annotation_custom(grob = p2, xmin = -40, xmax = 80, ymin = -110, ymax = 10)
```

![plot of chunk maps](figure/maps2.png) 

```r

student2012.sub.summary$name <- factor(student2012.sub.summary$name, levels = student2012.sub.summary$name[order(student2012.sub.summary$mathr)])
p1 <- ggplot(data = world.polys) + geom_path(aes(x = X1, y = X2, order = order, 
    group = group), colour = I("grey70")) + geom_polygon(data = student2012.sub.map, 
    aes(x = X1, y = X2, order = order, group = group, fill = mathr)) + new_theme_empty + 
    theme(legend.position = "none")
p2 <- qplot(name, mathr, data = student2012.sub.summary, colour = mathr, ylab = "Range Math", 
    xlab = "") + coord_flip() + theme(legend.position = "none")
p2 = ggplotGrob(p2)
p1 + annotation_custom(grob = p2, xmin = -40, xmax = 80, ymin = -110, ymax = 10)
```

![plot of chunk maps](figure/maps3.png) 

```r

# Reading
student2012.sub.summary$name <- factor(student2012.sub.summary$name, levels = student2012.sub.summary$name[order(student2012.sub.summary$read)])
p1 <- ggplot(data = world.polys) + geom_path(aes(x = X1, y = X2, order = order, 
    group = group), colour = I("grey70")) + geom_polygon(data = student2012.sub.map, 
    aes(x = X1, y = X2, order = order, group = group, fill = read)) + new_theme_empty + 
    theme(legend.position = "none")
p2 <- qplot(name, read, data = student2012.sub.summary, colour = read, ylab = "Reading Score", 
    xlab = "") + coord_flip() + theme(legend.position = "none")
p2 = ggplotGrob(p2)
p1 + annotation_custom(grob = p2, xmin = -40, xmax = 80, ymin = -110, ymax = 10)
```

![plot of chunk maps](figure/maps4.png) 

```r

student2012.sub.summary$name <- factor(student2012.sub.summary$name, levels = student2012.sub.summary$name[order(student2012.sub.summary$readr)])
p1 <- ggplot(data = world.polys) + geom_path(aes(x = X1, y = X2, order = order, 
    group = group), colour = I("grey70")) + geom_polygon(data = student2012.sub.map, 
    aes(x = X1, y = X2, order = order, group = group, fill = readr)) + new_theme_empty + 
    theme(legend.position = "none")
p2 <- qplot(name, readr, data = student2012.sub.summary, colour = readr, ylab = "Range Reading", 
    xlab = "") + coord_flip() + theme(legend.position = "none")
p2 = ggplotGrob(p2)
p1 + annotation_custom(grob = p2, xmin = -40, xmax = 80, ymin = -110, ymax = 10)
```

![plot of chunk maps](figure/maps5.png) 

```r

# Science
student2012.sub.summary$name <- factor(student2012.sub.summary$name, levels = student2012.sub.summary$name[order(student2012.sub.summary$science)])
p1 <- ggplot(data = world.polys) + geom_path(aes(x = X1, y = X2, order = order, 
    group = group), colour = I("grey70")) + geom_polygon(data = student2012.sub.map, 
    aes(x = X1, y = X2, order = order, group = group, fill = science)) + new_theme_empty + 
    theme(legend.position = "none")
p2 <- qplot(name, science, data = student2012.sub.summary, colour = science, 
    ylab = "Science Score", xlab = "") + coord_flip() + theme(legend.position = "none")
p2 = ggplotGrob(p2)
p1 + annotation_custom(grob = p2, xmin = -40, xmax = 80, ymin = -110, ymax = 10)
```

![plot of chunk maps](figure/maps6.png) 

```r

student2012.sub.summary$name <- factor(student2012.sub.summary$name, levels = student2012.sub.summary$name[order(student2012.sub.summary$sciencer)])
p1 <- ggplot(data = world.polys) + geom_path(aes(x = X1, y = X2, order = order, 
    group = group), colour = I("grey70")) + geom_polygon(data = student2012.sub.map, 
    aes(x = X1, y = X2, order = order, group = group, fill = sciencer)) + new_theme_empty + 
    theme(legend.position = "none")
p2 <- qplot(name, sciencer, data = student2012.sub.summary, colour = sciencer, 
    ylab = "Range Science", xlab = "") + coord_flip() + theme(legend.position = "none")
p2 = ggplotGrob(p2)
p1 + annotation_custom(grob = p2, xmin = -40, xmax = 80, ymin = -110, ymax = 10)
```

![plot of chunk maps](figure/maps7.png) 




```r
library(YaleToolkit)
```

```
## Loading required package: lattice
## Loading required package: vcd
## Loading required package: MASS
## 
## Attaching package: 'MASS'
## 
## The following object is masked from 'package:dplyr':
## 
##     select
## 
## Loading required package: colorspace
## Loading required package: barcode
## Loading required package: gpairs
```

```r
gpairs(student2012.sub.math[, -1])
```

```
## Error: object 'student2012.sub.math' not found
```

```r
# Should do a PCA, to look at what countries do better on what types of math
# skills
student2012.sub.math.nomiss <- subset(student2012.sub.math, !is.na(student2012.sub.math$Change) & 
    !is.na(student2012.sub.math$Employ))
```

```
## Error: object 'student2012.sub.math' not found
```

```r
rownames(student2012.sub.math.nomiss) <- student2012.sub.math.nomiss[, 1]
```

```
## Error: object 'student2012.sub.math.nomiss' not found
```

```r
student2012.sub.math.pca <- prcomp(student2012.sub.math.nomiss[, -c(1, 2)], 
    scale = T, retx = T)
```

```
## Error: object 'student2012.sub.math.nomiss' not found
```

```r
student2012.sub.math.pca
```

```
## Error: object 'student2012.sub.math.pca' not found
```

```r
qplot(PC2, PC3, data = data.frame(student2012.sub.math.pca$x)) + theme(aspect.ratio = 1)
```

```
## Error: object 'student2012.sub.math.pca' not found
```

```r
# High values on PC2 correspond to high data and interpretation, low
# corresponds to high spatial Ireland, UK, Greece, US states, Netherland,
# Norway, NZ, Finland, France, Croatia, Australia come up as high on data
# and interp but less on spatial AND Chine, Albania, Taiwan, Kazakhstan,
# Korea, Japan, Russia come up as being high on spatial, less on data and
# interp
rownames(student2012.sub.math.pca$x)[order(student2012.sub.math.pca$x[, 2], 
    decreasing = T)]
```

```
## Error: object 'student2012.sub.math.pca' not found
```

```r
# High on PC3 correspond to high quantification, employ, low on data Israel,
# Croatia, Estonia, Czech are high; Taiwan, Japan, Indonesia,
# Massachussetts, Jordan are low
rownames(student2012.sub.math.pca$x)[order(student2012.sub.math.pca$x[, 3], 
    decreasing = T)]
```

```
## Error: object 'student2012.sub.math.pca' not found
```

