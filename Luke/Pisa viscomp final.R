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
library(rworldmap)
sessionInfo()
################## IMPORTANT #######################
# ************************************************
# My file path has the rda files in the main folder.
# They are not in a "data" folder.
# ************************************************
####################################################

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

student2012$ST15Q01 <- addNA(student2012$ST15Q01)
student2012$ST19Q01 <- addNA(student2012$ST19Q01)

student2012$name <- as.character(student2012$CNT)
# unique(anti_join(student2012.sub, world.polys)[1])
student2012$name[student2012$name=="United Arab Emirates"] <- "UAE"
student2012$name[student2012$name=="United Kingdom"] <- "UK"
student2012$name[student2012$name=="Serbia"] <- "Serbia"
student2012$name[student2012$name=="Korea"] <- "South Korea"
student2012$name[student2012$name=="Chinese Taipei"] <- "Taiwan"
student2012$name[student2012$name=="Slovak Republic"] <- "Slovakia"
student2012$name[student2012$name=="Russian Federation"] <- "Russia"
student2012$name[student2012$name=="Perm(Russian Federation)"] <- "Russia"
student2012$name[student2012$name=="Hong Kong-China"] <- "Hong Kong"
student2012$name[student2012$name=="China-Shanghai"] <- "China"
student2012$name[student2012$name=="Macau"] <- "China"
student2012$name[student2012$name=="Connecticut (USA)"] <- "USA"
student2012$name[student2012$name=="Florida (USA)"] <- "USA"
student2012$name[student2012$name=="Massachusetts (USA)"] <- "USA"
student2012$name[student2012$name=="United States of America"] <- "USA"
unique(student2012$name)
student2012$name <- factor(student2012$name)

student2012$UID <- paste(student2012$STRATUM,student2012$SCHOOLID,sep="")
school2012$UID <- paste(school2012$STRATUM,school2012$SCHOOLID,sep="")
ind <- match(student2012$UID,school2012$UID)
studschool <- cbind(student2012,school2012[ind,-c(1:6)])


# Mother's Occupation ST15Q01, Father's ST19Q01
aa1 <- a1 <- summaryBy(data=student2012, pvM + ESCS ~ ST15Q01 +ST19Q01 + CNT,FUN=mean)
#a2 <- melt(a1)
levels(a1$ST15Q01) <- levels(a1$ST19Q01)<- c("Full-time","Part-time","Unemployed","Other","NA")
#a1$ST15Q01 <- factor(factor(a1$ST15Q01),levels=rev(levels(a1$ST15Q01)))
#a1$ST19Q01 <- factor(factor(a1$ST19Q01),levels=rev(levels(a1$ST19Q01)))

ind <- which(a1$ST15Q01=="Full-time")
a1$CNT <- factor(a1$CNT,levels(a1$CNT)[order(a1$pvM.mean[ind])])
#a1$CNT[ind] <- reorder(factor(a1$CNT[ind]),a1$pvM.mean[ind],mean)
p <- qplot(CNT, pvM.mean, col=ST15Q01, data = a1) +coord_flip()+ facet_grid(~ST19Q01);p
#ggsave(p,file="Parentmathachievement.pdf",height=10,width=18)

a1 <- cbind(student2012[,c("name","ST15Q01")],Parent="Mother")
a2 <- cbind(student2012[,c("name","ST19Q01")],Parent="Father")
levels(a1$ST15Q01) <-  c("Full-time","Part-time","Unemployed","Other","NA")
levels(a2$ST19Q01) <- c("Full-time","Part-time","Unemployed","Other","NA")

names(a1) <- names(a2) <- c("Country","Occupation","Parent")
a3 <- rbind(a1,a2)
p <- ggplot(a3, aes(Occupation, fill=Parent)) + geom_bar(position="dodge")
#ggsave(p,file="ParentJobBar.pdf",width=6,height=4)


a6 <- summaryBy(data=student2012, pvM + ESCS ~ ST15Q01+ name,FUN=mean,rm.na=T)
a7 <- summaryBy(data=student2012, pvM + ESCS ~ ST19Q01+ name,FUN=mean,rm.na=T)
colnames(a6) <- colnames(a7) <- c("Job.Status","Country","Math","ESCS")
levels(a6$Job.Status) <- levels(a7$Job.Status)  <- c("Full-Time","Part-Time","Unemployed","Other","NA")

a8 <- rbind(cbind(a6,Parent="Mother"),cbind(a7,Parent="Father"))
#a2 <- melt(a1)
ind <- which(a8$Job.Status=="Other"& a8$Parent=="Mother")
a8$Country <- factor(a8$Country,levels(a8$Country)[order(a8$Math[ind])])
p <- qplot(Country, Math, col=Job.Status, data = a8) +coord_flip() + facet_wrap(~Parent);p
#ggsave(p,file="Parentmathachievement.pdf",height=9,width=11)







# Out of School Study time
b1 <- summaryBy(data=student2012, pvM + pvR + pvS+ ST57Q01+ ST57Q02 +ST57Q03+ ST57Q04+ ST57Q05+ ST57Q06 ~CNT,FUN=mean, na.rm=T)
names(b1) <- c("CNT","pvM","pvR","pvS","ST57Q01", "ST57Q02","ST57Q03", "ST57Q04", "ST57Q05", "ST57Q06" )
# Out of school study time (total hours HW)
b1$CNT <- reorder(factor(b1$CNT),b1$pvS, mean)
b2 <- melt(b1[,c("CNT","pvM","pvR","pvS")],id="CNT")
b2 <- cbind(b2, b1[match(b2$CNT,b1$CNT), 5:10])
qplot(CNT, value,color=variable,size=ST57Q01, data = b2)+ coord_flip()
head(b2)
qplot(ST57Q01, value,color=variable,size=ST57Q02, data = b2)+ coord_flip()+facet_wrap(~CNT)

student.sub <- studschool[,c("name","pvM","pvR","pvS","ST57Q01","OECD","SC03Q01")]
study <- melt(student.sub,id=c("name","ST57Q01","OECD","SC03Q01"))
names(study) <- c("Country","HomeworkHours","OECD","AREA","Subject","Score")
levels(study$Subject) <- c("Math","Reading","Science")
p <- ggplot(data = study, aes(HomeworkHours,Score,colour=Subject) )+xlim(0,20)+facet_wrap(~Country)+stat_smooth(se=F)+ xlab("Out-of-School Study Hours")
#ggsave(p,file="studytime.pdf",height=13,width=13)
p <- ggplot(data=student.sub, aes(ST57Q01, ..density..)) + geom_histogram(binwidth=5) + xlab("Out-of-School Study Hours") +xlim(0,30)
ggsave(p,file="HistHWschool.pdf",width=8,height=6)

student.sub <- studschool[,c("name","pvM","ST57Q01","SC03Q01")]
study <- student.sub
names(study) <- c("Country","Math","HomeworkHours","Schoolplace")

p <- ggplot(data = study, aes(HomeworkHours,Math,colour=Schoolplace) )+xlim(0,20)+facet_wrap(~Country)+stat_smooth(se=F)
#ggsave(p,file="studytime.pdf",height=13,width=13)
p <- ggplot(data=student.sub, aes(ST57Q01, ..density..)) + geom_histogram(binwidth=5) + xlab("Out-of-School Study Hours") + facet_grid(SC03Q01~.) +xlim(0,30)
ggsave(p,file="HistHWschool.pdf",width=9,height=8)




# Time spent learning these materials
c1 <- summaryBy(data=student2012, pvM + pvR + pvS+ ST57Q01+ LMINS+MMINS+SMINS ~CNT,FUN=mean, na.rm=T)


f1 <- summaryBy(data=student2012, pvM + pvR + pvS+ ST57Q01+ LMINS+MMINS+SMINS ~CNT,FUN=function(x) length(x), keep.names=TRUE)

f2 <- summaryBy(data=student2012, pvM + pvR + pvS+ ST57Q01+ LMINS+MMINS+SMINS ~CNT,FUN=function(x) sum(is.na(x)), keep.names=TRUE)


names(c1) <- c("CNT","pvM","pvR","pvS","ST57Q01", "LMINS","MMINS","SMINS")
# Out of school study time (total hours HW)
c1$CNT <- reorder(factor(c1$CNT),b1$pvM, mean)
c2 <- melt(c1[,c("CNT","pvM","pvR","pvS")],id="CNT")
c2 <- cbind(c2, c1[match(c2$CNT,c1$CNT), 5:8])
qplot(CNT, value,color=variable,size=ST57Q01, data = c2)+ coord_flip()
head(c2)


student.learn <- student2012[,c("CNT","pvM","pvR","pvS","ST57Q01", "LMINS","MMINS","SMINS")]
learn <- melt(student.learn,id=c("CNT","ST57Q01", "LMINS","MMINS","SMINS"))

r <- ggplot(data = learn, aes(x=LMINS,y=value,colour=variable) )+facet_wrap(~CNT)+stat_smooth(se=F)
#ggsave(r,file="learntime.pdf")




head(student2012)
## making a variable that identifies which students live in a home where 
# the mother's occupation is  housewife
ind <- which(student2012$OCOD1 == "Housewife")
ind2 <- setdiff(1:dim(student2012)[1],ind)
data <- student2012[,c("CNT","OCOD1","pvM","pvR","pvS")]
data$Housewife <- NA
data$Housewife[ind] <- "Yes"
data$Housewife[ind2] <- "No"
data$Housewife <- factor(data$Housewife)

# simple boxplot comparing overall scores in math for housewife (yes/no)
p <- ggplot(data=data, aes(Housewife, pvM))+ geom_boxplot()+coord_flip()

# ploting a boxplot for scores of Housewife (yes/no) for each country. They are
# ordered by the overall median. 
qplot(data=data, reorder(factor(CNT),pvM,median),pvM, fill=Housewife, geom="boxplot")+coord_flip()

# I would like to overlay the boxplots but am not sure how. 


# plotting some various percentiles individually for Housewife (yes/no)
student2012.sub <- student2012[,c("CNT","OCOD1","pvM","pvR","pvS")]
student2012.yes <- summarise(group_by(student2012.sub[ind,], CNT), score10 = quantile(pvM,.1), score50 = quantile(pvM,.5),score90 = quantile(pvM,.9))
student2012.no <- summarise(group_by(student2012.sub[ind2,], CNT), score10 = quantile(pvM,.1), score50 = quantile(pvM,.5),score90 = quantile(pvM,.9))
student2012.no$CNT <- reorder(factor(student2012.no$CNT),student2012.no$score50,median)
student2012.yes$CNT <- reorder(factor(student2012.yes$CNT),student2012.yes$score50,median)

# This function is in library(reshape)
# Not a housewife
b1 <-  melt(student2012.no)
qplot(CNT, value, color=variable, data = b1)+ coord_flip()
# Housewife
b <- melt(student2012.yes)
qplot(CNT, value, color=variable, data = b)+ coord_flip()


# Plotting housewife v. no housewife ordered by overall score 
data <- student2012[,c("CNT","OCOD1","pvM","pvR","pvS")]
data$Housewife <- NA
data$Housewife[ind] <- "Yes"
data$Housewife[ind2] <- "No"
data$Housewife <- factor(data$Housewife)
data$CNT <- reorder(factor(data$CNT),data$pvM,median)
qplot(CNT, pvM.mean, col=Housewife, data = b2) +coord_flip()
b2 <- summaryBy(data=data, pvM~Housewife+CNT, FUN=mean, rm.na=T )
# making a column to show the percentage of housewifes
# will order the factor levels by the percentage of housewives per country
table(data$OCOD1,data$CNT) -> aa
d1 <- aa[which(rownames(aa)=="Housewife"),] / table(data$CNT)
d1[which(d1==0)] <- NA
data$pcHW <- as.numeric(d1)[match(data$CNT,names(d1))]
data$CNT <- reorder(factor(data$CNT),data$pvM,median)
b2 <- summaryBy(data=data, pvM+pvR+pvS~Housewife+CNT, FUN=mean, rm.na=T )
b2$pct.HouseWife <- as.numeric(d1)[match(b2$CNT,names(d1))]
qplot(CNT, pvM.mean, col=Housewife,size=pct.HouseWife, data = b2) +coord_flip()


data <- student2012[,c("CNT","OCOD1","pvM","pvR","pvS")]
data$Housewife <- NA
data$Housewife[which(data$OCOD1=="Housewife")] <- "Yes"
data$Housewife[which(data$OCOD1!="Housewife")] <- "No"
data$Housewife <- factor(data$Housewife)
data1 <- data
data1$CNT <- reorder(factor(data1$CNT),data$pvS,median)
b2 <- summaryBy(data=data1, pvM+pvR+pvS~Housewife+CNT, FUN=mean, rm.na=T )
b2$pct.HouseWife <- as.numeric(d1)[match(b2$CNT,names(d1))]
qplot(CNT, pvS.mean, col=Housewife,size=pct.HouseWife, data = b2) +coord_flip()



# would like to make a map that shows the percentage of housewife's in each country
table(data$OCOD1,data$CNT) -> aa
d1 <- aa[which(rownames(aa)=="Housewife"),] / table(data$CNT)
d1[which(d1==0)] <- NA




student2012.sub <- student2012[,c(1,12)]
student2012.gender <- summarise(group_by(student2012.sub, CNT), PropBoys=length(ST04Q01[ST04Q01=="Male"])/length(ST04Q01))
student2012.gender$CNT <- factor(student2012.gender$CNT, levels=levels(student2012.gender$CNT)[order(student2012.gender$PropBoys)])
qplot(CNT, PropBoys, data=student2012.gender) + coord_flip()
map.CNT <- map()$names
world <- map_data("world")
student2012.gender$CNT <- as.character(student2012.gender$CNT)
student2012.gender$CNT[student2012.gender$CNT=="United States of America"] <- "USA"
student2012.gender.map <- merge(student2012.gender, world, by.x="CNT", by.y="region")
qplot(long, lat, data=student2012.gender.map, group=group, order=order, geom="polygon", fill=PropBoys) + coord_map()

# Use dplyr fo rcheck for mismatches
library(dplyr)
library(rworldmap)
world <- getMap(resolution = "low")
world$NAME
colnames(student2012.gender)[1] <- "NAME"
sort(unique(world$region))
anti_join(student2012.gender, world@data)
student2012.gender$NAME[student2012.gender$NAME=="United States of America"] <- "United States"
student2012.gender$NAME[student2012.gender$NAME=="USA"] <- "United States"
student2012.gender$NAME[student2012.gender$NAME=="Czech Republic"] <- "Czech Rep."
student2012.gender$NAME[student2012.gender$NAME=="Chinese Taipei"] <- "Taiwan"
student2012.gender$NAME[student2012.gender$NAME=="Slovak Republic"] <- "Slovakia"
student2012.gender$NAME[student2012.gender$NAME=="Russian Federation"] <- "Russia"
student2012.gender$NAME[student2012.gender$NAME=="Korea"] <- "S. Korea"
student2012.gender$NAME[student2012.gender$NAME=="Jordan"] <- "Jordan"
student2012.gender$NAME[student2012.gender$NAME=="Hong Kong-China"] <- "Hong Kong"
student2012.gender$NAME[student2012.gender$NAME=="China-Shanghai"] <- "China"
student2012.gender.map <- inner_join(student2012.gender, world@data)
new_theme_empty <- theme_bw()
new_theme_empty$line <- element_blank()
new_theme_empty$rect <- element_blank()
new_theme_empty$strip.text <- element_blank()
new_theme_empty$axis.text <- element_blank()
new_theme_empty$plot.title <- element_blank()
new_theme_empty$axis.title <- element_blank()
new_theme_empty$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")

ggplot(world) + aes(long, lat, group=group) + 
  geom_polygon(fill="grey80") +
  geom_path(color="white") +
  coord_equal() + new_theme_empty
student2012.gender.map <- joinData2Map(student2012.gender, world, "NAME", "NAME")
ggplot(student2012.gender.map) + aes(long, lat, group=group, fill=PropBoys) + 
  geom_polygon() +
  #geom_path(color="white") +
  coord_equal() + new_theme_empty

extractPolygons <- function(shapes) {

  dframe <- ldply(1:length(shapes@polygons), function(i) {
    ob <- shapes@polygons[[i]]@Polygons
    dframe <- ldply(1:length(ob), function(j) {
      x <- ob[[j]]
      co <- x@coords
      data.frame(co, order=1:nrow(co), group=j)
    })
    dframe$region <- i
    dframe$name <- shapes@polygons[[i]]@ID
    dframe
  })
  # construct a group variable from both group and polygon:
  dframe$group <- interaction(dframe$region, dframe$group)
  
  dframe
}

world.polys <- extractPolygons(world)
colnames(student2012.sub)[1] <- "name"
student2012.sub[,1] <- as.character(student2012.sub[,1])
unique(anti_join(student2012.sub, world.polys)[1])
student2012.sub$name[student2012.sub$name=="United States"] <- "United States of America"
student2012.sub$name[student2012.sub$name=="Czech Republic"] <- "Czech Rep."
student2012.sub$name[student2012.sub$name=="Serbia"] <- "Republic of Serbia"
student2012.sub$name[student2012.sub$name=="Korea"] <- "S. Korea"
student2012.sub$name[student2012.sub$name=="Jordan"] <- "Jordan"
student2012.sub$name[student2012.sub$name=="Hong Kong-China"] <- "Hong Kong S.A.R."
student2012.sub$name[student2012.sub$name=="Chinese Taipei"] <- "Taiwan"
student2012.sub$name[student2012.sub$name=="Slovak Republic"] <- "Slovakia"
student2012.sub$name[student2012.sub$name=="S. Korea"] <- "Korea"
student2012.sub$name[student2012.sub$name=="Russian Federation"] <- "Russia"
student2012.sub.map <- join(student2012.sub, world.polys)
unique(student2012.sub$name)
qplot(X1, X2, order=order, group=group, data=student2012.sub.map, geom="polygon", fill=PropBoys) + new_theme_empty