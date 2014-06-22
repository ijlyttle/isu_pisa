Exploring possessions, and other demographics
========================================================


```r
library(stringr)
library(reshape2)
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
student2012.sub <- student2012[, c(1:7, 44:57, 61:66, seq(501, 550, 5))]
colnames(student2012.sub)[1] <- "name"
for (i in 8:21) {
    student2012.sub[, i] <- as.character(student2012.sub[, i])
    student2012.sub[is.na(student2012.sub[, i]), i] <- ""
    student2012.sub[student2012.sub[, i] == "Yes", i] <- "1"
    student2012.sub[student2012.sub[, i] == "No", i] <- "0"
    student2012.sub[, i] <- as.numeric(student2012.sub[, i])
}
for (i in 22:26) {
    student2012.sub[, i] <- as.character(student2012.sub[, i])
    student2012.sub[is.na(student2012.sub[, i]), i] <- ""
    student2012.sub[student2012.sub[, i] == "None", i] <- "0"
    student2012.sub[student2012.sub[, i] == "One", i] <- "1"
    student2012.sub[student2012.sub[, i] == "Two", i] <- "2"
    student2012.sub[student2012.sub[, i] == "Three or more", i] <- "3"
    student2012.sub[, i] <- as.numeric(student2012.sub[, i])
}
student2012.sub[, 27] <- as.character(student2012.sub[, 27])
student2012.sub[is.na(student2012.sub[, 27]), 27] <- ""
student2012.sub[student2012.sub[, 27] == "0-10 books ", 27] <- "0"
student2012.sub[student2012.sub[, 27] == "11-25 books ", 27] <- "1"
student2012.sub[student2012.sub[, 27] == "26-100 books ", 27] <- "2"
student2012.sub[student2012.sub[, 27] == "101-200 books ", 27] <- "3"
student2012.sub[student2012.sub[, 27] == "201-500 books ", 27] <- "4"
student2012.sub[student2012.sub[, 27] == "More than 500 books", 27] <- "5"
student2012.sub[, 27] <- as.numeric(student2012.sub[, 27])
student2012.sub$numposs <- apply(student2012.sub[, 8:21], 1, sum, na.rm = T)
student2012.sub$numedposs <- apply(student2012.sub[, 22:27], 1, sum, na.rm = T)
```



```r
qplot(numposs, PV1MATH, data = student2012.sub, alpha = I(0.1), geom = c("point", 
    "smooth"))
```

![plot of chunk plotstuff](figure/plotstuff1.png) 

```r
qplot(numedposs, PV1MATH, data = student2012.sub, alpha = I(0.1), geom = c("point", 
    "smooth"))
```

![plot of chunk plotstuff](figure/plotstuff2.png) 

```r
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
student2012.sub.summary <- summarise(group_by(student2012.sub[, c(1, 8:39)], 
    name), math = mean(PV1MATH, na.rm = T), read = mean(PV1READ, na.rm = T), 
    science = mean(PV1SCIE, na.rm = T), poss = mean(numposs, na.rm = T), edposs = mean(numedposs, 
        na.rm = T))
qplot(poss, math, data = student2012.sub.summary) + geom_smooth(method = "lm", 
    se = F)
```

![plot of chunk plotstuff](figure/plotstuff3.png) 

```r
qplot(poss, read, data = student2012.sub.summary) + geom_smooth(method = "lm", 
    se = F)
```

![plot of chunk plotstuff](figure/plotstuff4.png) 

```r
qplot(poss, science, data = student2012.sub.summary) + geom_smooth(method = "lm", 
    se = F)
```

![plot of chunk plotstuff](figure/plotstuff5.png) 

```r
head(student2012.sub.summary[order(student2012.sub.summary$math, decreasing = T), 
    ], 20)
```

```
## Source: local data frame [20 x 6]
## 
##                   name  math  read science   poss edposs
## 50      China-Shanghai 610.5 567.4   578.5 10.487 10.218
## 57           Singapore 568.4 537.4   546.7  9.883 11.501
## 23     Hong Kong-China 561.3 544.9   555.4  9.321  9.487
## 62      Chinese Taipei 557.6 521.9   522.1  9.840 11.271
## 34               Korea 554.2 536.1   537.9  9.969 11.447
## 39         Macao-China 538.3 509.2   520.8  9.069 10.286
## 35       Liechtenstein 537.9 518.4   525.5 11.495 13.939
## 32               Japan 536.1 537.8   547.1  9.155 11.645
## 18             Estonia 521.8 517.8   543.2 11.249 12.104
## 10         Switzerland 520.7 497.2   503.1 10.695 12.520
## 47              Poland 520.6 521.1   528.6 11.221 11.856
## 6              Belgium 519.9 512.3   510.5 10.640 12.610
## 14      Czech Republic 519.8 513.0   527.6 11.125 12.488
## 43         Netherlands 518.1 506.3   517.2 11.163 12.761
## 15             Germany 513.9 507.5   523.7  9.656 11.325
## 68             Vietnam 510.6 508.3   528.2  7.375  6.466
## 9               Canada 509.3 511.2   514.7 10.496 13.691
## 5              Austria 507.7 490.9   507.2 11.204 13.016
## 19             Finland 507.5 510.9   528.0 10.705 12.874
## 54 Massachusetts (USA) 506.4 520.2   519.8 10.644 13.558
```

```r
# Vietnam is the outlier, few poss, high scores
lm(math ~ poss, data = student2012.sub.summary)
```

```
## 
## Call:
## lm(formula = math ~ poss, data = student2012.sub.summary)
## 
## Coefficients:
## (Intercept)         poss  
##       219.2         25.6
```

```r
# Each increase in one possession tends to increase math score by 25 points
# (poss=7, 12)
qplot(edposs, math, data = student2012.sub.summary) + geom_smooth(method = "lm", 
    se = F)
```

![plot of chunk plotstuff](figure/plotstuff6.png) 

```r
qplot(edposs, read, data = student2012.sub.summary) + geom_smooth(method = "lm", 
    se = F)
```

![plot of chunk plotstuff](figure/plotstuff7.png) 

```r
qplot(edposs, science, data = student2012.sub.summary) + geom_smooth(method = "lm", 
    se = F)
```

![plot of chunk plotstuff](figure/plotstuff8.png) 

```r
lm(math ~ edposs, data = student2012.sub.summary)
```

```
## 
## Call:
## lm(formula = math ~ edposs, data = student2012.sub.summary)
## 
## Coefficients:
## (Intercept)       edposs  
##       328.6         13.1
```

```r
# Each increase in one possession tends to increase math score by 13 points
# (edposs=5, 14)
qplot(edposs, poss, data = student2012.sub.summary)
```

![plot of chunk plotstuff](figure/plotstuff9.png) 




```r
student2012.sub.summary <- summarise(group_by(student2012.sub[, c(1, 8, 28, 
    38)], name), math05 = mean(PV1MATH[numposs < 6], na.rm = T), math06 = mean(PV1MATH[numposs == 
    6], na.rm = T), math07 = mean(PV1MATH[numposs == 7], na.rm = T), math08 = mean(PV1MATH[numposs == 
    8], na.rm = T), math09 = mean(PV1MATH[numposs == 9], na.rm = T), math10 = mean(PV1MATH[numposs == 
    10], na.rm = T), math11 = mean(PV1MATH[numposs == 11], na.rm = T), math12 = mean(PV1MATH[numposs == 
    12], na.rm = T), math13 = mean(PV1MATH[numposs == 13], na.rm = T), math14 = mean(PV1MATH[numposs == 
    14], na.rm = T))
student2012.sub.summary.m <- melt(student2012.sub.summary)
student2012.sub.summary.m$variable <- substr(student2012.sub.summary.m$variable, 
    5, 6)
student2012.sub.summary.m$variable <- as.numeric(student2012.sub.summary.m$variable)
qplot(variable, value, data = student2012.sub.summary.m, group = name, geom = c("point", 
    "smooth"), se = F) + facet_wrap(~name, ncol = 8)
```

![plot of chunk plotstuff2](figure/plotstuff2.png) 



```r
summary(student2012$ST06Q01)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##       4       6       6       6       7      16   27496
```

```r
student2012$ST06Q01 <- as.numeric(student2012$ST06Q01)
student2012.age.iqr <- summarise(group_by(student2012[, c(1, 14)], CNT), q0 = min(ST06Q01, 
    na.rm = T), q25 = quantile(ST06Q01, 0.25, na.rm = T), q50 = median(ST06Q01, 
    na.rm = T), q75 = quantile(ST06Q01, 0.75, na.rm = T), q100 = max(ST06Q01, 
    na.rm = T), count = length(ST06Q01))
student2012.age.iqr$CNT <- factor(student2012.age.iqr$CNT, levels = student2012.age.iqr$CNT[order(student2012.age.iqr$q50)])
ggplot(data = student2012.age.iqr) + ylab("Age") + xlab("") + geom_point(aes(x = CNT, 
    y = q50, size = count)) + geom_segment(aes(x = CNT, xend = CNT, y = q0, 
    yend = q25)) + geom_segment(aes(x = CNT, xend = CNT, y = q75, yend = q100)) + 
    coord_flip() + theme(legend.position = "none")
```

![plot of chunk age](figure/age.png) 



```r
student2012.sub <- student2012[, c(1, 14, 501)]
colnames(student2012.sub)[1] <- "name"
student2012.sub$PV1MATH <- as.numeric(student2012.sub$PV1MATH)
student2012.sub$ST06Q01 <- as.numeric(student2012.sub$ST06Q01)
student2012.sub.summary <- summarise(group_by(student2012.sub, name), math = mean(PV1MATH, 
    na.rm = T), age = mean(ST06Q01, na.rm = T))
qplot(age, math, data = student2012.sub.summary)
```

![plot of chunk agemath](figure/agemath1.png) 

```r
# Age at start school does not seem to make a difference on country level
# qplot(ST06Q01, PV1MATH, data=student2012, alpha=I(0.1), xlab='Age',
# ylab='Math')
student2012.sub.summary <- summarise(group_by(student2012.sub, name), math4 = mean(PV1MATH[ST06Q01 == 
    4], na.rm = T), math5 = mean(PV1MATH[ST06Q01 == 5], na.rm = T), math6 = mean(PV1MATH[ST06Q01 == 
    6], na.rm = T), math7 = mean(PV1MATH[ST06Q01 == 7], na.rm = T), math8 = mean(PV1MATH[ST06Q01 > 
    7], na.rm = T))
student2012.sub.summary.m <- melt(student2012.sub.summary)
student2012.sub.summary.m$variable <- substr(student2012.sub.summary.m$variable, 
    5, 5)
student2012.sub.summary.m$variable <- as.numeric(student2012.sub.summary.m$variable)
qplot(variable, value, data = student2012.sub.summary.m, group = name, geom = c("point", 
    "smooth"), se = F) + facet_wrap(~name, ncol = 8)
```

![plot of chunk agemath](figure/agemath2.png) 

```r
# Looking by country is important: generally starting school early leads to
# better scores
```

