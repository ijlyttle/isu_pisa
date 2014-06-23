setwd("/Users/lukefostvedt/Documents/PISA 2012/RDA files/")
library(stringr)
library(ggplot2)
library(reshape)
library(dplyr)
library(lubridate)
library(ggvis)
library(doBy)
library(maps)
library(ggmap)
library(mice)
library(boot)
sessionInfo()
sets <- c("item", "parent", "school", "scoredItem", "student")

# function to build the file names
fn_build <- function(file_name) {

    template <- c("2012.rda", "2012dict.rda")

    file_name %>% vapply(str_join, template, template) %>% file.path(".", "", 
        .)
}

# load the data
sets %>% fn_build %>% lapply(load, .GlobalEnv)


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
load("student2012pvmeans.Rdata")
student2012 <- cbind(student2012,a)

head(student2012)


ind <- which(student2012$OCOD1 == "Housewife")
ind2 <- setdiff(1:dim(student2012)[1],ind)
data <- student2012[,c("CNT","OCOD1","pvM","pvR","pvS")]
data$Housewife <- NA
data$Housewife[ind] <- "Yes"
data$Housewife[ind2] <- "No"
data$Housewife <- factor(data$Housewife)

p <- ggplot(data=data, aes(Housewife, pvM))+ geom_boxplot()+coordflip()

qplot(data=data, reorder(factor(CNT),pvM,median),pvM, fill=Housewife, geom="boxplot")+ ggplot(data=data, aes(reorder(factor(CNT),pvR,median),pvM))+ geom_boxplot()+coordflip()


student2012.sub <- student2012[,c("CNT","OCOD1","pvM","pvR","pvS")]
student2012.yes <- summarise(group_by(student2012.sub[ind,], CNT), score10 = quantile(pvM,.1), score50 = quantile(pvM,.5),score90 = quantile(pvM,.9))
student2012.no <- summarise(group_by(student2012.sub[ind2,], CNT), score10 = quantile(pvM,.1), score50 = quantile(pvM,.5),score90 = quantile(pvM,.9))

student2012.no$CNT <- reorder(factor(student2012.no$CNT),student2012.no$score50,median)
student2012.yes$CNT <- reorder(factor(student2012.yes$CNT),student2012.yes$score50,median)

b1 <-  melt(student2012.no)
b <- melt(student2012.yes)
qplot(CNT, value, color=variable, data = b) coord_flip()

data <- student2012[,c("CNT","OCOD1","pvM","pvR","pvS")]
data$Housewife <- NA
data$Housewife[ind] <- "Yes"
data$Housewife[ind2] <- "No"
data$Housewife <- factor(data$Housewife)

data$CNT <- reorder(factor(data$CNT),data$pvM,median)

b2 <- summaryBy(data=data, pvM~Housewife+CNT, FUN=mean, rm.na=T )
qplot(CNT, pvM.mean, col=Housewife, data = b2) +coord_flip()




summary()






