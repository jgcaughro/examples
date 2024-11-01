
#NESEA sea grass convert datasheet structure

library(reshape2)
library(dplyr)
library(tibble)


#### Phase 1 - Reorganize Canopy Ht Columns ####
#read data
seagrass_dat <- read.csv('./IRL_TierII_Wet_2020_WQ_SAV.csv', header = T)


  ## Pulling info from each Canopy Height Column
#Canopy Height 1
Can1dat <- with(seagrass_dat, as.data.frame(cbind(Segment, Site, Rep, Can.ht.sp1, Cm1)))
  #expand to each species column
Can1 <- dcast(Can1dat, Segment + Site + Rep ~ Can.ht.sp1)
  #order sites for later merge
Can1 <- Can1[order(Can1$Segment, Can1$Site, Can1$Rep), ]
 # created 2 Hd columns Can1. Merging here.
Can1$Hd <- coalesce(Can1$Hd, Can1$`Hd `)
Can1$Hw <- coalesce(Can1$Hw, Can1$`Hw `)
Can1$Hj <- coalesce(Can1$Hj, Can1$`Hj `)


#Canopy Height 2
Can2dat <- with(seagrass_dat, as.data.frame(cbind(Segment, Site, Rep, Can.ht.sp2, Cm2)))
  #expand to each species column
Can2 <- dcast(Can2dat, Segment + Site + Rep ~ Can.ht.sp2)
  #order sites for later merge
Can2 <- Can2[order(Can2$Segment, Can2$Site, Can2$Rep), ]


#Canopy Height 3
Can3dat <- with(seagrass_dat, as.data.frame(cbind(Segment, Site, Rep, Can.ht.sp3, Cm3)))
  #expand to each species column
Can3 <- dcast(Can3dat, Segment + Site + Rep ~ Can.ht.sp3)
  #order sites for later merge
Can3 <- Can3[order(Can3$Segment, Can3$Site, Can3$Rep), ]


#Canopy Height 4
Can4dat <- with(seagrass_dat, as.data.frame(cbind(Segment, Site, Rep, Can.ht.sp4, Cm4)))
  #expand to each species column
Can4 <- dcast(Can4dat, Segment + Site + Rep ~ Can.ht.sp4)
  #order sites for later merge
Can4 <- Can4[order(Can4$Segment, Can4$Site, Can4$Rep), ]


#Canopy Height 5
Can5dat <- with(seagrass_dat, as.data.frame(cbind(Segment, Site, Rep, Can.ht.sp5, Cm5)))
  #expand to each species column
Can5 <- dcast(Can5dat, Segment + Site + Rep ~ Can.ht.sp5)
  #order sites for later merge
Can5 <- Can5[order(Can5$Segment, Can5$Site, Can5$Rep), ]



##Coalesce for each species 
  #place to save 
seagrass_merged <- as.data.frame(cbind(seagrass_dat$Segment, seagrass_dat$Site,
                                       seagrass_dat$Rep))
  #change column names
colnames(seagrass_merged) <- c("Segment", "Site", "Rep")

  #order sites for merge
seagrass_merged <- seagrass_merged[order(seagrass_merged$Segment, seagrass_merged$Site,
                              seagrass_merged$Rep), ]


  #Hw
seagrass_merged$Hw_Can_Ht <- coalesce(Can1$Hw, Can2$Hw, Can3$Hw, Can4$Hw, Can5$Hw)

  #Hd
seagrass_merged$Hd_Can_Ht <- coalesce(Can1$Hd, Can2$Hd, Can3$Hd, Can4$Hd, Can5$Hd)

  #He
seagrass_merged$He_Can_Ht <- coalesce(Can1$He, Can2$He, Can3$He, Can4$He, Can5$He)

  #Hj
seagrass_merged$Hj_Can_Ht <- coalesce(Can1$Hj, Can2$Hj, Can3$Hj, Can4$Hj, Can5$Hj)


  #Tt
seagrass_merged$Tt_Can_Ht <- coalesce(Can1$Tt, Can2$Tt, Can3$Tt, Can4$Tt, Can5$Tt)

  #Sf
seagrass_merged$Sf_Can_Ht <- coalesce(Can1$Sf, Can2$Sf, Can3$Sf, Can4$Sf, Can5$Sf)

  #NDR
seagrass_merged$NDR_ht <- Can1$NDR


#convert NAs to blanks
seagrass_merged[is.na(seagrass_merged)] <- " "




## Export
write.csv(seagrass_merged, "./seagrass_merged_IRLWet2020TierII.csv", row.names = F)




#### Phase 2 - Reorganize SAV Data to Merge to Complete Table ####

#read data 
  #full data
seagrass_dat <- read.csv('./IRL_TierII_Wet_2020_WQ_SAV.csv', header = T)
  #reorg canopy hts
can_hts <- read.csv("./seagrass_merged_IRLWet2020TierII.csv", header = T)
can_hts[is.na(can_hts)] <- " "




#save as output
output <- seagrass_dat

#order rows same as can hts 
output <- output[order(output$Segment, output$Site, output$Rep), ]

#set order of columns 
output <- output[, c(1:11,15,14,16:19,12,13,20:34)] 
#output <- output[, c(1:10,13:30,12,31,11,32:35)] 

## Edits to each individual col to prep for final merge

#Date

#Year

#Month

#Season
  #fill rows with category
    #if column exists
#output$Season <- rep("Dry", length(output$Season))
    #if column doesn't exist
output <- output %>%
  add_column(Season = rep("Wet", length(output$Date)), .after = "Month")


#Location
  #fill rows with category
    #if column exists
#output$Location <- rep("IRL", length(output$Location))
    #if column doesn't exist
output <- output %>%
  add_column(Location = rep("IRL", length(output$Date)), .after = "Season")


#Segment

#Site

#Quad
  #change col name from Rep to Quad
names(output)[names(output) == 'Rep'] <- 'Quad'

#TOTAL_BBCA
  #change col name from TMP.DR to TOTAL_BBCA
names(output)[names(output) == "TMP.DR"] <- "TOTAL_BBCA"

#DR_BBCA
names(output)[names(output) == "DR"] <- "DR_BBCA"

#DR_Spp
output <- output %>%
  add_column(DR_Spp = "", .after = "DR_BBCA")

#BMA - will be moved to comments & col deleted if contains info. Check if it has any data before removing
  #remove
#output <- subset(output, select=-c(BMA))
  #send column to the end
output <- output %>%
  relocate(BMA, .after = last_col())



#TMP - col deleted
output <- subset(output, select=-c(TMP))

#TSG_BBCA
names(output)[names(output) == "TSG"] <- "TSG_BBCA"

#Tt_BBCA
names(output)[names(output) == "Tt"] <- "Tt_BBCA"


#Tt_CanHt
output <- output %>%
  add_column(Tt_CanHt = can_hts$Tt_Can_Ht, .after = "Tt_BBCA")

#Sf_BBCA
names(output)[names(output) == "Sf"] <- "Sf_BBCA"


#Sf_CanHt 
output <- output %>%
  add_column(Sf_CanHt = can_hts$Sf_Can_Ht, .after = "Sf_BBCA")

#Hw_BBCA
names(output)[names(output) == "Hw"] <- "Hw_BBCA"


#Hw_CanHt 
output <- output %>%
  add_column(Hw_CanHt = can_hts$Hw_Can_Ht, .after = "Hw_BBCA")

#Hj_BBCA
names(output)[names(output) == "Hj"] <- "Hj_BBCA"


#Hj_CanHt
output <- output %>%
  add_column(Hj_CanHt = can_hts$Hj_Can_Ht, .after = "Hj_BBCA")

#He_BBCA
names(output)[names(output) == "He"] <- "He_BBCA"


#He_CanHt 
output <- output %>%
  add_column(He_CanHt = can_hts$He_Can_Ht, .after = "He_BBCA")
  #if no He in can ht
#output <- output %>%
 # add_column(He_CanHt = "", .after = "He_BBCA")

#Hd_BBCA
names(output)[names(output) == "Hd"] <- "Hd_BBCA"


#Hd_CanHt 
output <- output %>%
  add_column(Hd_CanHt = can_hts$Hd_Can_Ht, .after = "Hd_BBCA")

#Va_BBCA
names(output)[names(output) == "Va"] <- "Va_BBCA"
  #converting NAs to zeros
#output$Va_BBCA[is.na(output$Va_BBCA)] <- 0


#Va_CanHt
output <- output %>%
  add_column(Va_CanHt = "", .after = "Va_BBCA")

#Rm_BBCA
names(output)[names(output) == "Rm"] <- "Rm_BBCA"
  #converting NAs to zeros
#output$Rm_BBCA[is.na(output$Rm_BBCA)] <- 0


#Rm_CanHt
output <- output %>%
  add_column(Rm_CanHt = "", .after = "Rm_BBCA")

#Caul_BBCA
names(output)[names(output) == "Cau"] <- "Caul_BBCA"


#Caul_Spp
names(output)[names(output) == "Cau.sp"] <- "Caul_Spp"


#TGO_BBCA
names(output)[names(output) == "TGO"] <- "TGO_BBCA"


#TGO_Spp
output <- output %>%
  add_column(TGO_Spp = "", .after = "TGO_BBCA")

#remove old can ht columns 
output <- subset(output, select=-c(Can.ht.sp1, Cm1, Can.ht.sp2, Cm2, Can.ht.sp3, Cm3,
                                   Can.ht.sp4, Cm4, Can.ht.sp5, Cm5))

#Epiphyte
output <- output %>%
  add_column(Epiphyte = "", .after = "TGO_Spp")

#Lyngbya 
output <- output %>%
  add_column(Lyngbya = "", .after = "Epiphyte")

#Sediment
output <- output %>%
  add_column(Sediment = "", .after = "Lyngbya")

#Comments

#Staff
#names(output)[names(output) == "STAFF"] <- "Staff"


#getting rid of any extra columns added
#output <- subset(output, select= -c(X))


#Save output 
write.csv(output, "./FinalFiles/IRL_Wet2020_SAV.csv", row.names = F)












#### IN PROGRESS: Phase 3 - Reorganize WQ Data From Wide to Long form where needed

library(tidyr)

#build out using function pivot_longer() ex. below 
  #useful post
  #https://stackoverflow.com/questions/71359531/pivot-longer-into-several-pairs-of-columns

#load data
wq <- read.csv("./WQ/Dry_2018_IRL_WQ.csv", header = T)

long <- wide %>% 
  pivot_longer(
    cols = `1950`:`1954`, 
    names_to = "year",
    values_to = "value"
  )


long <- WQ %>%
  pivot_longer(
    cols = c(starts_with("Surf"), starts_with("Bot")), 
    names_to = c('.value', 'depth'),
    names_pattern = '(.*?)_(.*)')

  



