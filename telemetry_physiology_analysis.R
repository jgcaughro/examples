
#analysis for upcoming FNEMO presentations Jan-Feb 2024

  #load data

library('RODBC')
library('tmap')
library(ggplot2)


#establish connection to Access database 
db<-file.path("//Ad/dfsroot/data/coastal/Projects/RECOVER/FNEMO/Data/FNEMO.accdb") #connect database.

channel<-odbcConnectAccess2007(db) #internal RODBC function
dataSetName<-sqlFetch(channel,"TableName") #read particular table from Access database file.

con <- odbcConnectAccess2007('FNEMO')

#load tables into R from Access 
blood <- sqlFetch(con, "Blood")
tag <- sqlFetch(con, "Tag_Site")

#close the ODBC connection
odbcCloseAll()


#load data from JG local 
blood <- read.csv("./Blood_Data.csv", header = T)
tag <- read.csv('./Site_Tag_Data.csv', header = T)
total_detect <- read.csv('clean_dets.csv', header = T)
total_detectSLR <- read.csv('clean_dets_SLR_only.csv', header = T)
inletdist <- read.csv('inlet distances 23FEB22.csv', header = T)





  #demographics
Snook_tag <- subset(tag, Species == "Cun")
Sheep_tag <- subset(tag, Species == "Apr")

hist(as.numeric(Snook_tag$TL_mm), breaks = 40)
hist(as.numeric(Sheep_tag$TL_mm))

#create stacked bar plot with TL on x and color is hab type

 

#looking for individuals that fall within M & F 50% maturity size class per Matheson 1998
  #M - 330-348
  #F - 500-522

sizeM <- subset(Snook_tag,  SL_mm< 400)
sizeF <- subset(Snook_tag, 550<SL_mm)


#how many pings at what receivers 
  #dataframe with each individual, number of detetections, number of receivers, list of receivers

tag_names <- unique(total_detect$tagname)
fishid <- unique(total_detect$catalognumber)


fish <- NULL
temp_output <- NULL
detect_summary <- NULL

for (i in fishid) {
#subset each fish
  fish <- subset(total_detect, catalognumber == i)
#calc total detection number
  tot_detect <- length(fish$tagname)
#calc number receivers
  tot_receiv <- length(unique(fish$station))
#list of stations
  list_receiv <- toString(unique(fish$station))
#store information
  fishid <- i
  tagname <- unique(fish$tagname)
  spec <- unique(fish$Common_Name)
  temp_output <- data.frame(spec, tagname, fishid, tot_detect, tot_receiv, list_receiv)
  colnames(temp_output) <- c("spec", "tagname", "fishid", "total_detections", "total_receivers", "receiver_list")
  detect_summary <- rbind(detect_summary, temp_output)
}


write.csv(detect_summary, "./reports/detect_summary122023.csv", row.names = F)


#few quick stats on detections and receivers 
  #total detection average
aggregate(as.numeric(total_detections) ~ spec, detect_summary, mean)
 
 #number of stations average
aggregate(as.numeric(total_receivers) ~ spec, detect_summary, mean)



  #PCV
#look at any impact of water temp ~ PCV value. Just look for some signals in data
#do some research on what PCV can tell us. 

#subset Snook and Sheep blood
Snook_blood <- subset(blood, Species == "Cun")
Sheep_blood <- subset(blood, Species == "Apr")

## Snook PCV
  #by length
plot(Snook_blood$PVC_av ~ Snook_tag$TL_mm)
lines(lowess(Snook_tag$TL_mm, Snook_blood$PCV_av), col = 'red')

  #by season
boxplot(Snook_blood$PVC_av ~ Snook_tag$Season)

  #by water temp
plot(Snook_blood$PVC_av ~ Snook_tag$Temp)
lines(lowess(Snook_tag$Temp, Snook_blood$PCV_av), col = 'red')

  #by salinity
plot(Snook_blood$PVC_av ~ Snook_tag$Sal)
lines(lowess(Snook_tag$Sal, Snook_blood$PCV_av), col = 'red')



boxplot(blood$PVC_av ~ tag$Species)

## Sheep PCV
#by length
plot(Sheep_blood$PVC_av ~ Sheep_tag$TL_mm)
lines(lowess(Sheep_tag$TL_mm, Sheep_blood$PCV_av), col = 'red')

#by season
boxplot(Sheep_blood$PVC_av ~ Sheep_tag$Season)

#by water temp
plot(Sheep_blood$PVC_av ~ Sheep_tag$Temp)
lines(lowess(Sheep_tag$Temp, Sheep_blood$PCV_av), col = 'red')

#by salinity
plot(na.rm(Sheep_blood$PVC_av) ~ na.rm(Sheep_tag$Sal))
lines(lowess(Sheep_tag$Sal, Sheep_blood$PCV_av), col = 'red', na.rm = T)


boxplot(blood$PVC_av ~ blood$Date)


library(ggplot2)


# add mean to ggplot2 boxplot

#pulling month
library("lubridate")
Month <- month(as.POSIXlt(blood$Date, format = "%m/%d/%Y"))

#by month
ggplot(blood, aes(x = as.factor(Month), y = PVC_av)) + 
  geom_boxplot(aes(fill = Species)) 

#### FIGURE FOR PCV VS SEASON FOR BOTH SPECIES - need to clean up make prettier
#by season 
blood$Season <- factor(blood$Season, levels = c("Wet22", "Dry23", "Wet23", "Dry24"))

ppi <- 600
png("./plots/PCVboxplot_seasons.png", width=9*ppi, height=6*ppi, res=ppi)

ggplot(blood, aes(x = as.factor(Season), y = as.numeric(PVC_av))) + 
  geom_boxplot(aes(fill = Species)) +
  scale_fill_manual(name = "Species", labels = c("Sheepshead", "Common Snook"),
                    values = c("blue", "orange"))+
  labs(x = "Season", y = "PCV (%)") +
  theme(
    legend.position = "top",
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14,face="bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold")
  )
    

dev.off()


 # Significant By Species 
with(blood, t.test(PVC_av ~ Species))

  #Significant By Season
summary(with(Sheep_blood, aov(PVC_av ~ as.factor(Season))))

summary(with(Snook_blood, aov(PVC_av ~ as.factor(Season))))



#### Plot of Dry Seasons splitting Dry 2024 to pre and post Lake release 

#subset Dry season data
blood_dry <- subset(blood, Season == c("Dry23", "Dry24"))
blood_dry$Date <- as.Date(blood_dry$Date, "%m/%d/%Y")

#fish tagged Dry 24 pre 2/20/24 sampling == pre release. Fish tagged 2/20 & after == during release
dates <- as.Date(c("2024-02-20", "2024-02-27", "2024-03-12"))
blood_dry$Release <- ifelse(blood_dry$Season == "Dry23", "Dry23", 
                            ifelse(blood_dry$Season == "Dry24" & blood_dry$Date %in% dates, 
                                   "Dry24R", "Dry24"))


#create box plot similar to above 
blood_dry$Season <- factor(blood_dry$Season, levels = c("Dry23", "Dry24", "Dry24R"))

ppi <- 600
png("./plots/PCVboxplot_dryrelease.png", width=9*ppi, height=6*ppi, res=ppi)

ggplot(blood_dry, aes(x = as.factor(Release), y = as.numeric(PVC_av))) + 
  geom_boxplot(aes(fill = Species)) +
  scale_fill_manual(name = "Species", labels = c("Sheepshead", "Common Snook"),
                    values = c("blue", "orange"))+
  labs(x = "Season", y = "PCV (%)") +
  ylim (15, 60) +
  stat_summary(aes(color = factor(Species)), fun.data = stat_box_data, geom = "text", fun.y = median, 
               position = position_dodge(width = 0.9), fontface = "bold", show.legend = F) +
  scale_color_manual(values = c("blue", "orange")) +
  theme(
    legend.position = "top",
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14,face="bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold")
  )


dev.off()



#### boxplot with all Wet, all Dry (not release), Dry (Release)
blood$Date <- as.Date(blood$Date, "%m/%d/%Y")
blood$Release <- ifelse(blood$Season == "Wet22", "Wet",
                        ifelse(blood$Season == "Wet23", "Wet", 
                               ifelse(blood$Season == "Dry24" & blood$Date %in% dates, "Dry24R", "Dry")))


blood$Release <- factor(blood$Release, levels = c("Wet", "Dry", "Dry24R"))


stat_box_data <- function(y) {
  return( 
    data.frame(
      y = 70,  # may need to modify this depending on your data
      label = paste('N =', length(y), '\n',
                    'median =', round(median(y), 1), '\n')
    )
  )
}




ppi <- 600
png("./plots/PCVboxplot_release2.png", width=9*ppi, height=6*ppi, res=ppi)

ggplot(blood, aes(x = as.factor(Release), y = as.numeric(PVC_av), fill = Species)) + 
  geom_boxplot() +
  scale_fill_manual(name = "Species", labels = c("Sheepshead", "Common Snook"),
                    values = c("blue", "orange"))+
  labs(x = "Season", y = "PCV (%)") +
  stat_summary(aes(color = factor(Species)), fun.data = stat_box_data, geom = "text", fun.y = median, 
              position = position_dodge(width = 0.75), fontface = "bold", show.legend = F) +
  scale_color_manual(values = c("blue", "orange")) +
  theme(
    legend.position = "top",
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14,face="bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold")
  )


dev.off()





library('dplyr')

  #getting average temps to add to plot
AV_environ <- tag %>%
  group_by(Date, Rel_Site_Num) %>%
  summarize(
    temp = unique(Temp),
    DO = unique(DO_mgL),
    Sal = unique(Sal),
    season = unique(Season)
  )

AV_tempDOSal <- data.frame(Season = c("Wet", "Dry", "Dry24R"), 
                      av_temp = c(mean(as.numeric(subset(AV_environ$temp, AV_environ$temp != "NDR" & AV_environ$Release == "Wet"))),
                                  mean(as.numeric(subset(AV_environ$temp, AV_environ$temp != "NDR" & AV_environ$Release == "Dry"))),
                                  mean(as.numeric(subset(AV_environ$temp, AV_environ$temp != "NDR" & AV_environ$Release == "Dry24R")))),
                      sd_temp = c(sd(as.numeric(subset(AV_environ$temp, AV_environ$temp != "NDR" & AV_environ$Release == "Wet"))),
                                  sd(as.numeric(subset(AV_environ$temp, AV_environ$temp != "NDR" & AV_environ$Release == "Dry"))),
                                  sd(as.numeric(subset(AV_environ$temp, AV_environ$temp != "NDR" & AV_environ$Release == "Dry24R")))),
                      av_do = c(mean(as.numeric(subset(AV_environ$DO, AV_environ$DO != "NDR" & AV_environ$Release == "Wet"))),
                                mean(as.numeric(subset(AV_environ$DO, AV_environ$DO != "NDR" & AV_environ$Release == "Dry"))),
                                mean(as.numeric(subset(AV_environ$DO, AV_environ$DO != "NDR" & AV_environ$Release == "Dry24R")))),
                      sd_do = c(sd(as.numeric(subset(AV_environ$DO, AV_environ$DO != "NDR" & AV_environ$Release == "Wet"))),
                                sd(as.numeric(subset(AV_environ$DO, AV_environ$DO != "NDR" & AV_environ$Release == "Dry"))),
                                sd(as.numeric(subset(AV_environ$DO, AV_environ$DO != "NDR" & AV_environ$Release == "Dry24R")))),
                      av_sal = c(mean(as.numeric(subset(AV_environ$Sal, AV_environ$Sal != "NDR" & AV_environ$Release == "Wet"))),
                                mean(as.numeric(subset(AV_environ$Sal, AV_environ$Sal != "NDR" & AV_environ$Release == "Dry"))),
                                mean(as.numeric(subset(AV_environ$Sal, AV_environ$Sal != "NDR" & AV_environ$Release == "Dry24R")))),
                      sd_sal = c(sd(as.numeric(subset(AV_environ$Sal, AV_environ$Sal != "NDR" & AV_environ$Release == "Wet"))),
                                 sd(as.numeric(subset(AV_environ$DO, AV_environ$Sal != "NDR" & AV_environ$Release == "Dry"))),
                                 sd(as.numeric(subset(AV_environ$DO, AV_environ$Sal != "NDR" & AV_environ$Release == "Dry24R")))))


  #relevel factor to make it in time order on graph
AV_tempDOSal$Season <- factor(AV_tempDOSal$Season, levels = c("Wet", "Dry", "Dry24R"))


#temp & DO ggplots

#temp 
png("./plots/Avtempbar.png", width=9*ppi, height=6*ppi, res=ppi)

ggplot(AV_tempDOSal, aes(x = Season)) +
  geom_bar(aes(y = av_temp), stat = "identity", width = 0.8, fill = "lightblue") +
  coord_cartesian(ylim = c(0, 40)) +
  geom_errorbar(aes(ymin = av_temp-sd_temp, ymax = av_temp+sd_temp), width = 0.2) +
  labs(x = "Season", y = "Average Temperature (\u00B0C)") +
  theme(
    legend.position = "top",
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14,face="bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold")
  )

dev.off()

#DO
png("./plots/AvDObar.png", width=9*ppi, height=6*ppi, res=ppi)

ggplot(AV_tempDOSal, aes(x = Season)) +
  geom_bar(aes(y = av_do), stat = "identity", width = 0.8, fill = "darkgreen") +
  coord_cartesian(ylim = c(0,10)) +
  geom_errorbar(aes(ymin = av_do-sd_do, ymax = av_do+sd_do), width = 0.2) +
  labs(x = "Season", y = "Average DO (mg/L)") +
  theme(
    legend.position = "top",
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14,face="bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold")
  )

dev.off()


#Salinity
png("./plots/AvSalbar.png", width=9*ppi, height=6*ppi, res=ppi)

ggplot(AV_tempDOSal, aes(x = Season)) +
  geom_bar(aes(y = av_sal), stat = "identity", width = 0.8, fill = "darkgreen") +
  coord_cartesian(ylim = c(0,30)) +
  geom_errorbar(aes(ymin = av_sal-sd_sal, ymax = av_sal+sd_sal), width = 0.2) +
  labs(x = "Season", y = "Average Salinity") +
  theme(
    legend.position = "top",
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14,face="bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold")
  )

dev.off()





#### Making temp, DO, and salinity plots separating Dry post release ## just adjusted code above to this
AV_environ$Date <- as.Date(AV_environ$Date, "%m/%d/%Y")

dates <- as.Date(c("2024-02-20", "2024-02-27", "2024-03-12"))

AV_environ$Release <- ifelse(AV_environ$season == "Wet22", "Wet",
                        ifelse(AV_environ$season == "Wet23", "Wet", 
                               ifelse(AV_environ$season == "Dry24" & AV_environ$Date %in% dates, "Dry24R", "Dry")))




#### ABACUS PLOTS LOOKING FOR MIGRATION FOR SPAWNING #### 
library(lattice)
library(viridis)

ppi <- 600

png("abacus plot all GG.png", width=9*ppi, height=6*ppi, res=ppi)

dev.off()



#subset specific fish to look at 
fishlistSN <- c("SN26", "SN29", "SN33", "SN32", "SN15", "SN18")
SNsubset <- total_detectSLR[total_detectSLR$Animal_ID %in% fishlistSN, ]
SNsubset$datecollected <- as.POSIXct(SNsubset$datecollected, tz = Sys.timezone())

#plot of just a few Snook 
dotplot(reorder(SNsubset$Animal_ID, SNsubset$datecollected) ~ SNsubset$datecollected, SNsubset, 
        labels = row.names(SNsubset$Animal_ID), cex = 0.7,
        groups = factor(SNsubset$region, levels = c("Crossroads", "GP", "inlet", "IRL", "ME", "NF", "SF"),
                        labels = c("Crossroads", "GP", "inlet", "IRL", "ME", "NF", "SF")), 
        col = c('lightblue', "orange2", "red2", "purple2", "green2"), 
        xlab = "Date", ylab = "Snook ID", scales = list(format = '%b %Y', tick.number = 20, rot = 45), pch = 20,
        key = list(space = "top", columns = 5,
                   text = list(c("Crossroads", "GP", "inlet", "IRL", "ME", "NF", "SF"),
                                 col = c('lightblue', "orange2", "red2", "purple2", "green2"))))


#plot of all Snook 
justSNdetect <- subset(total_detectSLR, total_detectSLR$commonname == "common snook")
justSNdetect$datecollected <- as.POSIXct(justSNdetect$datecollected, tz = Sys.timezone())


justSNdetect$region <- as.factor(justSNdetect$region)
levels(justSNdetect$region) <- c("inlet", "Crossroads", "GP", "ME", "NF", "SF", "IRL")


#plot of all Snook
  #saving
ppi <- 600
png("allSnook.png", width=9*ppi, height=6*ppi, res=ppi)
  #plot
dotplot(reorder(justSNdetect$Animal_ID, justSNdetect$datecollected) ~ 
          justSNdetect$datecollected, justSNdetect, 
        labels = row.names(justSNdetect$Animal_ID), cex = 0.8,
        groups = factor(justSNdetect$region, 
                        labels = c("Crossroads", "GP", "inlet", "IRL", "ME", "NF", "SF")), 
        col = c('#EE3377', "#EE7733", "#CC3311", "#BBBBBB", "#0077BB", "#33BBEE", "#009988"), 
        xlab = "Date", ylab = "Snook ID", 
        scales = list(format = '%b %Y', tick.number = 20, rot = 45, y = list(cex = 0.5)), 
        pch = 20,
        key = list(space = "left", rows = 7,
                   text = list(c('Inlet','Crossroads','GP','ME','NF','SF','IRL'),
                               col = c("#CC3311", "#EE3377","#EE7733", "#0077BB",
                                       "#33BBEE", "#009988", "#BBBBBB"))))
dev.off()



#CC3311   #EE3377   #EE7733 #0077BB #33BBEE #009988  #BBBBBB
# inlet, crossroads,   GP,   ME,      NF,     SF,      IRL



#plot of all Sheepshead
justSHdetect <- subset(total_detectSLR, total_detectSLR$commonname == "sheepshead")
justSHdetect$datecollected <- as.POSIXct(justSHdetect$datecollected, tz = Sys.timezone())

#plot of all Sheepshead
  #saving
ppi <- 600
png("allSheepshead.png", width=9*ppi, height=6*ppi, res=ppi)
  #plot
dotplot(reorder(justSHdetect$Animal_ID, justSHdetect$datecollected) ~ 
          justSHdetect$datecollected, justSHdetect, 
        labels = row.names(justSHdetect$Animal_ID), cex = 0.8,
        groups = factor(justSHdetect$region, 
                        labels = c("Crossroads", "GP", "inlet", "ME", "NF", "SF")), 
        col = c('#EE3377', "#EE7733", "#CC3311", "#0077BB", "#33BBEE", "#009988"), 
        xlab = "Date", ylab = "Sheepshead ID", 
        scales = list(format = '%b %Y', tick.number = 20, rot = 45,y = list(cex = 0.5)), 
        pch = 20,
        key = list(space = "left", rows = 6,
                   text = list(c('Inlet','Crossroads','GP','ME','NF','SF'),
                               col = c("#CC3311", "#EE3377","#EE7733", "#0077BB",
                                       "#33BBEE", "#009988"))))

dev.off()

#select Snook and Sheepshead
fishlist <- c( "SN24", "SN26", "SN34", "SN18", "SN29", "SN33", "SN37",
              "SH03", "SH27", "SH30", "SH23", "SH32", "SH31", "SH13")
fishsubset <- total_detectSLR[total_detectSLR$Animal_ID %in% fishlist, ]
fishsubset$datecollected <- as.POSIXct(fishsubset$datecollected, tz = Sys.timezone())
fishsubset$Animal_ID <- factor(fishsubset$Animal_ID, levels = c( "SN24", "SN26", 
                                        "SN34", "SN18", "SN29", "SN33", "SN37",
                                        "SH03", "SH27", "SH30", "SH23", "SH32", 
                                        "SH31", "SH13"))

#plot
  #saving
ppi <- 600
png("SeclectFish.png", width=9*ppi, height=6*ppi, res=ppi)
  #plot
dotplot(fishsubset$Animal_ID ~ fishsubset$datecollected, fishsubset, 
        labels = row.names(fishsubset$Animal_ID), cex = 0.8,
        groups = factor(fishsubset$region, 
                        labels = c("Crossroads", "GP", "inlet", "IRL", "ME", "NF", "SF")), 
        col = c('#EE3377', "#EE7733", "#CC3311", "#BBBBBB", "#0077BB", "#33BBEE", "#009988"), 
        xlab = "Date", ylab = "Fish ID", 
        scales = list(format = '%b %Y', tick.number = 20, rot = 45), 
        pch = 20,
        key = list(space = "left", rows = 7,
                   text = list(c('Inlet','Crossroads','GP','ME','NF','SF','IRL'),
                               col = c("#CC3311", "#EE3377","#EE7733", "#0077BB",
                                       "#33BBEE", "#009988", "#BBBBBB"))))
dev.off()




#Snook that moved out of system for Hurricane
HurrSNlist <- c("SN02", "SN08", "SN16", "SN21", "SN31", "SN28", "SN14", "SN07", "SN18")
HurrSNsubset <- total_detectSLR[total_detectSLR$Animal_ID %in% HurrSNlist, ]
HurrSNsubset$datecollected <- as.POSIXct(HurrSNsubset$datecollected, tz = Sys.timezone())
HurrSNsubset$Animal_ID <- factor(HurrSNsubset$Animal_ID, levels = c( "SN02", "SN08",
                                                                     "SN16", "SN21", 
                                                                     "SN31", "SN28", 
                                                                     "SN14", "SN07", 
                                                                     "SN18"))
#plot
#saving
ppi <- 600
png("HurricaneSNFish.png", width=9*ppi, height=6*ppi, res=ppi)
#plot
dotplot(HurrSNsubset$Animal_ID ~ HurrSNsubset$datecollected, HurrSNsubset, 
        labels = row.names(HurrSNsubset$Animal_ID), cex = 0.8,
        groups = factor(HurrSNsubset$region, 
                        labels = c("GP", "inlet", "IRL", "ME", "NF", "SF")), 
        col = c("#EE7733", "#CC3311", "#BBBBBB", "#0077BB", "#33BBEE", "#009988"), 
        xlab = "Date", ylab = "Snook ID", 
        scales = list(format = '%b %Y', tick.number = 20, rot = 45), 
        pch = 20,
        key = list(space = "left", rows = 6,
                   text = list(c('Inlet','GP','ME','NF','SF','IRL'),
                               col = c("#CC3311","#EE7733", "#0077BB",
                                       "#33BBEE", "#009988", "#BBBBBB"))))
dev.off()





#### ANIMATION OF DIFFERENT INDIVIDUALS THROUGH TIME ####
library(sf)
library(raster)
library(ggmap)
library(gganimate)
library(RColorBrewer)
library(tmap)

#removing crazy longitude point
total_detect <- subset(total_detect, longitude < -77)

SN33detect <- subset(total_detect, catalognumber == "FNEMO-SN33-2023-02-28")
SN26detect <- subset(total_detect, Animal_ID == "SN26")

fl_map <- st_read("./floridashapefile", layer = "Fl_bound_poly")

test <- st_transform(fl_map, "+proj=longlat +lat_0=27.2 +lon_0=-80.2")

  plot(fl_map)


 
mypal <- brewer.pal(length(unique(total_detect$catalognumber)), "Reds")


coordinates(HurrSNsubset) <- ~longitude + latitude

HurricaneSNanimation <- 
  tm_shape(fl_map) +
  tm_borders() +
  tm_shape(HurrSNsubset) +
  tm_dots(size = 0.2) +
  tm_facets(along = "datecollected", free.coords = F) +
  tm_layout(legend.position = c('left', 'bottom'))

tmap_animation(HurricaneSNanimation,
               filename = "HurricaneSNanimation.gif", fps = 250, width = 2400,
               height = 1200)



#ggplot
p = ggplot()+
  geom_sf(data = test, aes(fill = ATTRIBUTE)) +
  coord_sf(xlim = range(SN33detect$longitude, na.rm = TRUE), 
           ylim = range(SN33detect$latitude, na.rm = TRUE), 
           expand = T) 
  
  # lines and points
  geom_path(data = SN33detect, 
            aes(x=longitude,y=latitude,group=catalognumber,color= "blue"), 
            alpha = 0.3) +
 
  geom_point(data = SN33detect, 
             aes(x=longitude,y=latitude,group=catalognumber,fill= "red"),
             alpha = 0.7, shape=23, size = 2) 
  
  # formatting
 # scale_fill_brewer()+
  #scale_color_brewer()+
  #scale_size_continuous(range = c(0.1,10))+
  #labs(x=NULL, y=NULL, 
      #fill = 'Speed (m/s)', 
    #   color = 'Speed (m/s)')+
 # theme_dark()
 #theme(panel.grid = element_blank())

plot(p)


anim <- p +
  transition_reveal(along = julianday) +
  ease_aes('linear') +
  ggtitle("Date: {frame_along}")

animate(anim, nframes = 100, fps =10)




  
  ggplot() +
    geom_sf(data = fl_map) +
    #coord_sf(xlim =, 
           #  ylim = c(25, 28), 
            # expand = T) +
    theme_classic()
  
  
