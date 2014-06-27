Grouping schools by poverty
========================================================








```r
school2012.sub = school2012 %>% select(CNT, SCHOOLID, SC22Q01, SC22Q02, SC22Q03, 
    SC22Q04, SC22Q05, SC22Q06, SC22Q07, SC22Q08, SC22Q09, SC22Q10, SC22Q11, 
    SC22Q12, SC22Q13)
student2012.sub = student2012 %>% select(CNT, SCHOOLID, math, read, sci, ST08Q01, 
    ST09Q01)
school2012.sub = fix_country(school2012.sub)
student2012.sub = fix_country(student2012.sub)


truancy = student2012.sub %>% group_by(CNT, SCHOOLID) %>% left_join(school2012.sub, 
    by = c("CNT", "SCHOOLID"))
```



```r
# school perception of truancy vs math scores percentage of schools
# reporting high truancy issues
df = school2012.sub %>% group_by(CNT) %>% summarise(truancy = sum(SC22Q01 == 
    "A lot", na.rm = T)/length(CNT)) %>% arrange(desc(truancy)) %>% left_join(student2012.sub %>% 
    group_by(CNT) %>% summarise(m_math = mean(math)))
```

```
## Joining by: "CNT"
```

```r
# russia and china are outliers
qplot(x = truancy, y = m_math, data = df) + stat_smooth(method = "lm", se = FALSE)
```

![plot of chunk truancy](figure/truancy1.png) 

```r

# student self reported truancy within the last two weeeks percentage of
# high truancy students
df2 = student2012.sub %>% group_by(CNT) %>% summarise(truancy_student = sum(ST08Q01 == 
    "Five or more times  " | ST08Q01 == "Three or four times  " | ST08Q01 == 
    "One or two times  ", na.rm = T)/length(CNT)) %>% arrange(desc(truancy_student)) %>% 
    left_join(student2012.sub %>% group_by(CNT) %>% summarise(m_math = mean(math)))
```

```
## Joining by: "CNT"
```

```r

qplot(truancy_student, m_math, data = df2)
```

![plot of chunk truancy](figure/truancy2.png) 



```r
# percentage of schools reporting drug problems
drugs = school2012.sub %>% group_by(CNT) %>% summarise(drug = sum(SC22Q07 == 
    "A lot" | SC22Q07 == "To some extent", na.rm = T)/length(CNT)) %>% arrange(desc(drug)) %>% 
    left_join(student2012.sub %>% group_by(CNT) %>% summarise(m_math = mean(math)))
```

```
## Joining by: "CNT"
```

```r


school2012.sub %>% group_by(CNT) %>% summarise(drug = sum(SC22Q07 == "A lot", 
    na.rm = T)/length(CNT)) %>% arrange(desc(drug))
```

```
## Source: local data frame [64 x 2]
## 
##                         CNT     drug
## 1                Kazakhstan 0.321101
## 2                     China 0.258065
## 3                    Russia 0.151724
## 4                    Jordan 0.111588
## 5                  Colombia 0.102273
## 6               Macao-China 0.088889
## 7                  Bulgaria 0.079787
## 8      United Arab Emirates 0.069869
## 9                    Taiwan 0.055215
## 10                   Greece 0.053191
## 11                   Turkey 0.047059
## 12               Costa Rica 0.036269
## 13                    Chile 0.031674
## 14                  Austria 0.031414
## 15                   Brazil 0.030989
## 16                 Malaysia 0.030488
## 17                  Tunisia 0.026144
## 18                    Qatar 0.025478
## 19                  Croatia 0.024540
## 20                  Albania 0.024510
## 21                   Israel 0.023256
## 22                     Peru 0.020833
## 23                   Mexico 0.019714
## 24                  Uruguay 0.016667
## 25                   Canada 0.015819
## 26                 Portugal 0.015385
## 27                Lithuania 0.009259
## 28                    Spain 0.008869
## 29                Argentina 0.008850
## 30                   France 0.008850
## 31              Switzerland 0.007299
## 32         Hong Kong S.A.R. 0.006757
## 33           Czech Republic 0.006734
## 34                  Finland 0.006431
## 35              South Korea 0.006410
## 36                  Denmark 0.005865
## 37                   Poland 0.005435
## 38                    Japan 0.005236
## 39                  Hungary 0.004902
## 40                Indonesia 0.004785
## 41                   Sweden 0.004785
## 42                   Latvia 0.004739
## 43                Australia 0.003871
## 44                    Italy 0.001675
## 45                  Belgium 0.000000
## 46                  Estonia 0.000000
## 47                  Germany 0.000000
## 48                  Iceland 0.000000
## 49                  Ireland 0.000000
## 50            Liechtenstein 0.000000
## 51               Luxembourg 0.000000
## 52               Montenegro 0.000000
## 53              Netherlands 0.000000
## 54              New Zealand 0.000000
## 55                   Norway 0.000000
## 56       Republic of Serbia 0.000000
## 57                  Romania 0.000000
## 58                Singapore 0.000000
## 59                 Slovakia 0.000000
## 60                 Slovenia 0.000000
## 61                 Thailand 0.000000
## 62           United Kingdom 0.000000
## 63 United States of America 0.000000
## 64                  Vietnam 0.000000
```


Competition between schools

```r
comp = school2012 %>% select(CNT, SCHOOLID, SC04Q01) %>% group_by(CNT) %>% summarise(other = sum(SC04Q01 == 
    "Two or More", na.rm = T)/length(CNT)) %>% arrange(desc(other)) %>% left_join(student2012.sub %>% 
    group_by(CNT) %>% summarise(m_math = mean(math)))
```

```
## Joining by: "CNT"
```

```r

qplot(m_math, other, data = comp)
```

```
## Warning: Removed 9 rows containing missing values (geom_point).
```

![plot of chunk competition](figure/competition.png) 



Math teacher shortages

```r
school2012.sub = school2012 %>% select(CNT, SCHOOLID, SC11Q02, SC11Q03, SC14Q02, 
    SC14Q07, SC14Q08, SC14Q09, SC14Q10, SC14Q11, SC14Q12, SC14Q13)
student2012.sub = student2012 %>% select(CNT, SCHOOLID, math, read, sci)
school2012.sub = fix_country(school2012.sub)
student2012.sub = fix_country(student2012.sub)

scores_by_school = student2012.sub %>% group_by(CNT, SCHOOLID) %>% summarise(mmath = mean(math), 
    mread = mean(read), msci = mean(sci))


df = scores_by_school %>% left_join(school2012.sub)
```

```
## Joining by: c("CNT", "SCHOOLID")
```

```r
qplot(SC14Q02, mmath2, data = df %>% group_by(CNT, SC14Q02) %>% summarise(mmath2 = mean(mmath)), 
    facets = ~CNT)
```

![plot of chunk shortages](figure/shortages1.png) 

```r
qplot(SC14Q10, mmath2, data = df %>% group_by(CNT, SC14Q10) %>% summarise(mmath2 = mean(mmath)), 
    facets = ~CNT)
```

![plot of chunk shortages](figure/shortages2.png) 



looking at just the US

```r
library(ggplot2)
qplot(SC14Q02, mmath, data = df %>% group_by(CNT, SC14Q02) %>% filter(CNT == 
    "United States of America"), geom = "boxplot")
```

![plot of chunk shortages1](figure/shortages1.png) 




```r
library(ggplot2)
df2 = df %>% select(CNT, SCHOOLID, mmath, mread, msci, SC14Q02, SC14Q07, SC14Q08, 
    SC14Q09, SC14Q10, SC14Q11, SC14Q12, SC14Q13)
for (i in 6:13) {
    df2[, i] = as.numeric(df2[, i])
}

df2$sum = apply(df2[, 6:13], 1, sum, na.rm = T)
qplot(x = sum, y = m2, data = df2 %>% group_by(CNT, sum) %>% summarise(m2 = mean(mmath)), 
    facets = ~CNT)
```

![plot of chunk shortages2](figure/shortages21.png) 

```r

qplot(x = pSC14Q07, mmath2, data = df2 %>% group_by(CNT) %>% summarise(pSC14Q07 = sum(SC14Q07 == 
    4, na.rm = T)/length(CNT), mmath2 = mean(mmath)) %>% arrange(pSC14Q07))
```

![plot of chunk shortages2](figure/shortages22.png) 






