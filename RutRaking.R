#Rut Rake

#### INSTRUCTIONS ####
#Step 1 - save newest versions of nest & relocation data to file locations in 
  #load data section
#Step 2 - change the date in Enter Friday Date section
#Step 3- Click source button on top right of this pane 
#Step 4 - PDFs will be located in the PDF folder, both separated by Day & HCP Zone 
  #and merged for one PDF per HCP zone with all the days of the week included. 

# *** Special Note *** 
  #Once per year the origin date for the date calculation in the table must be updated. 
    #Search for origin in 'subset output for each HCP zone & create tables' subsection
    #and update. Origin should be December 31st of previous year.
    #Ex. in 2024 origin = "2023-12-31" 

#### Load Libraries & Data ####

#library 
library(lubridate)
library(dplyr)
library(tidyr)
library(gt)
library(qpdf)


#load data 
nest <- read.csv("./data/Data.csv", header = T, comment.char = "/")
relocate <- read.csv("./data/Relocation_Info.csv", header = T)



#### Enter Friday date ####
friday <- as.Date("10/11/2024", tryFormats = "%m/%d/%Y")
friday <- yday(friday)



#### Prepare Data ####

#Subset for needed columns 
nest <- nest %>%
  select(Local.Nest.ID, Survey.Date, Report.Type, Address, Nest.Inventoried, 
         Initial.Treatment, Local.Zones, Report.Latitude, Species, City)

#select for nests 
nest <- subset(nest, Report.Type == "Nest, Above HT" | Report.Type == "Nest, Below HT" | 
                 Report.Type == "Missed Nest" | Report.Type == "False Crawl That Hatched")

#separate treatment type letter from description
nest <- separate(nest, Initial.Treatment, into = c("Initial.Treatment", "Treatment_Descrip"), sep = 1 )

#Convert Crawl Date to Julian Day
nest$Survey.Date <- as.Date(nest$Survey.Date, tryFormats = "%m/%d/%Y")
nest$survey.date.J <- yday(nest$Survey.Date)

#Adding column with date when nest reaches threshold
nest$date_incj <- with(nest, ifelse(Species == "Leatherback (Dc)", 
                                    (as.numeric(survey.date.J) + 59),
                                    (as.numeric(survey.date.J) + 45)))
nest$date_inc <- format(as.Date(nest$date_inc, origin = "2023-12-31"), "%m/%d")

#Filter Nests for Nest Inventoried == Y or N
active_nests <- subset(nest, Nest.Inventoried == '')




#### For loop that filters & compiles nest list for each day in each HCP zone ####

    #Address from relocate address using nest ID when initial treatment == E, F, G, H. 
    #And will take address from nest using nest ID when initial treatment == A, B, C, D. 

  #selecting for each day of the week
for (y in 0:6) {

  #update active nest list, if else for Leatherback to populate at 59 days. All other spp at 45 days.
  active_nest_day <- subset(active_nests,ifelse(Species == "Leatherback (Dc)" ,
                                                ((friday +y) >= (survey.date.J + 59)),
                                                ((friday +y) >= (survey.date.J + 45))))
                                     
  #update nestID list
  nestID <- unique(active_nest_day$Local.Nest.ID)
  
  #null output object
  output <- NULL

    for (i in nestID) {
        #selecting one nest at a time
      single_nest <- subset(active_nest_day, active_nest_day$Local.Nest.ID == i)
            
            #determining data selection for relocated nests
        if (with(single_nest, Initial.Treatment == "E"| Initial.Treatment == "F" | 
            Initial.Treatment == "G"| Initial.Treatment == "H")) {
   
            address <- relocate[relocate$Local.Nest.ID == i, ]$Relocation.Address
            lat <- relocate[relocate$Local.Nest.ID == i, ]$Relocation.Latitutde
 
        }
            #determining data selection for all other nests
        else{
            address <- single_nest$Address
            lat <- single_nest$Report.Latitude
        }
    #preparing for storage
    nestID <- i 
    address <- address
    HCPzone <- single_nest$Local.Zones
    lat <- lat
    city <- single_nest$City
    date_i <- single_nest$date_inc
    
    #storage
    temp <- data.frame(cbind(nestID, address, HCPzone, lat, city, date_i))
    output <- rbind(output, temp)
  }

  
  
### subset output for each HCP zone & create tables ####
  
#HCP Zone 2
  #prep
    #subset for zone
  rut_HCPzone2 <- subset(output, HCPzone == 2)
    #adding additional write in columns
  rut_HCPzone2$NestInspected <- rep("O", each = length(rut_HCPzone2$nestID))
  rut_HCPzone2$ReasonNotInspected <- rep(" ", each = length(rut_HCPzone2$nestID))
  rut_HCPzone2$NoRuts  <- rep("O", each = length(rut_HCPzone2$nestID))
  rut_HCPzone2$HandRaked  <- rep("O", each = length(rut_HCPzone2$nestID))
  rut_HCPzone2$BarRaked  <- rep("O", each = length(rut_HCPzone2$nestID))
  rut_HCPzone2$Comments <- rep(" ", each = length(rut_HCPzone2$nestID)) 
    #ordering nests from N to S using Latitude
  rut_HCPzone2 <- rut_HCPzone2[order(rut_HCPzone2$lat, decreasing = T),]
    #removing columns for HCP zone & Latitude from table
  rut_HCPzone2 <- rut_HCPzone2[, -c(3,4)]
  
  #save
    #creating table
  gt2 <- gt(rut_HCPzone2)
    #editing format of table
  gt2 <- gt2 |>
      #adding title and subtitle
    tab_header(
      title = "Volusia County Sea Turtle Monitoring HCP Zone 2",
      subtitle = paste0("Date: ", paste(format(as.Date(friday + y,
                                                       origin = "2023-12-31"), 
                                               "%m/%d/%Y")), 
                        " Name_________________________________ Start Time__________________ 
                        End Time__________________  
                        Beach Condition___________________________________")
    )|>
      #centering values in each cell
    cols_align(align = "center") |>
      #adding horizontal lines between each column
    tab_style(style = cell_borders(sides = c("left", "right"), color = "black",
                                   style = "solid"), locations = cells_body()) |>
      #making rows narrower
     tab_options(data_row.padding = px(1)) |>
      
      #changing column names to better names
    cols_label(
      nestID = "Nest ID",
      address = "Address",
      city = "City",
      date_i = "Flag Date",
      NestInspected = "Nest Inspected",
      ReasonNotInspected = "Reason Not Inspected",
      NoRuts = "No Ruts",
      HandRaked = "Hand Raked",
      BarRaked = "Bar Raked",
      Comments = "Comments"
    ) |>
      
      #making comment column wider
    cols_width(
      Comments ~ px(200)
    )
    
    #saving PDF with Day and HCP Zone in file name
 gt2 %>%
    gtsave(filename = paste0('./pdfs/Day', paste(y), "_HCPzone2.pdf" ))
  

 
 

#HCP Zone 3
 #prep
 #subset for zone
 rut_HCPzone3 <- subset(output, HCPzone == 3)
 #adding additional write in columns
 rut_HCPzone3$NestInspected <- rep("O", each = length(rut_HCPzone3$nestID))
 rut_HCPzone3$ReasonNotInspected <- rep(" ", each = length(rut_HCPzone3$nestID))
 rut_HCPzone3$NoRuts  <- rep("O", each = length(rut_HCPzone3$nestID))
 rut_HCPzone3$HandRaked  <- rep("O", each = length(rut_HCPzone3$nestID))
 rut_HCPzone3$BarRaked  <- rep("O", each = length(rut_HCPzone3$nestID))
 rut_HCPzone3$Comments <- rep(" ", each = length(rut_HCPzone3$nestID)) 
 #ordering nests from N to S using Latitude
 rut_HCPzone3 <- rut_HCPzone3[order(rut_HCPzone3$lat, decreasing = T),]
 #removing columns for HCP zone & Latitude from table
 rut_HCPzone3 <- rut_HCPzone3[, -c(3,4)]
 
 #save
 #creating table
 gt3 <- gt(rut_HCPzone3)
 #editing format of table
 gt3 <- gt3 |>
   #adding title and subtitle
   tab_header(
     title = "Volusia County Sea Turtle Monitoring HCP Zone 3",
     subtitle = paste0("Date: ", paste(format(as.Date(friday + y,
                                                      origin = "2023-12-31"), 
                                              "%m/%d/%Y")), 
                       " Name_________________________________ Start Time__________________ 
                        End Time__________________  
                        Beach Condition___________________________________")
   )|>
   #centering values in each cell
   cols_align(align = "center") |>
   #adding horizontal lines between each column
   tab_style(style = cell_borders(sides = c("left", "right"), color = "black",
                                  style = "solid"), locations = cells_body()) |>
   #making rows narrower
   tab_options(data_row.padding = px(1)) |>
   
   #changing column names to better names
   cols_label(
     nestID = "Nest ID",
     address = "Address",
     city = "City",
     date_i = "Flag Date",
     NestInspected = "Nest Inspected",
     ReasonNotInspected = "Reason Not Inspected",
     NoRuts = "No Ruts",
     HandRaked = "Hand Raked",
     BarRaked = "Bar Raked",
     Comments = "Comments"
   ) |>
   
   #making comment column wider
   cols_width(
     Comments ~ px(200)
   )
 
 #saving PDF with Day and HCP Zone in file name
 gt3 %>%
   gtsave(filename = paste0('./pdfs/Day', paste(y), "_HCPzone3.pdf" )) 
 
 
 
 
 
#HCP Zone 4
 #prep
 #subset for zone
 rut_HCPzone4 <- subset(output, HCPzone == 4)
 #adding additional write in columns
 rut_HCPzone4$NestInspected <- rep("O", each = length(rut_HCPzone4$nestID))
 rut_HCPzone4$ReasonNotInspected <- rep(" ", each = length(rut_HCPzone4$nestID))
 rut_HCPzone4$NoRuts  <- rep("O", each = length(rut_HCPzone4$nestID))
 rut_HCPzone4$HandRaked  <- rep("O", each = length(rut_HCPzone4$nestID))
 rut_HCPzone4$BarRaked  <- rep("O", each = length(rut_HCPzone4$nestID))
 rut_HCPzone4$Comments <- rep(" ", each = length(rut_HCPzone4$nestID)) 
 #ordering nests from N to S using Latitude
 rut_HCPzone4 <- rut_HCPzone4[order(rut_HCPzone4$lat, decreasing = T),]
 #removing columns for HCP zone & Latitude from table
 rut_HCPzone4 <- rut_HCPzone4[, -c(3,4)]
 
 #save
 #creating table
 gt4 <- gt(rut_HCPzone4)
 #editing format of table
 gt4 <- gt4 |>
   #adding title and subtitle
   tab_header(
     title = "Volusia County Sea Turtle Monitoring HCP Zone 4",
     subtitle = paste0("Date: ", paste(format(as.Date(friday + y,
                                                      origin = "2023-12-31"), 
                                              "%m/%d/%Y")),  
                       " Name_________________________________ Start Time__________________ 
                        End Time__________________  
                        Beach Condition___________________________________")
   )|>
   #centering values in each cell
   cols_align(align = "center") |>
   #adding horizontal lines between each column
   tab_style(style = cell_borders(sides = c("left", "right"), color = "black",
                                  style = "solid"), locations = cells_body()) |>
   #making rows narrower
   tab_options(data_row.padding = px(1)) |>
   
   #changing column names to better names
   cols_label(
     nestID = "Nest ID",
     address = "Address",
     city = "City",
     date_i = "Flag Date",
     NestInspected = "Nest Inspected",
     ReasonNotInspected = "Reason Not Inspected",
     NoRuts = "No Ruts",
     HandRaked = "Hand Raked",
     BarRaked = "Bar Raked",
     Comments = "Comments"
   ) |>
   
   #making comment column wider
   cols_width(
     Comments ~ px(200)
   )
 
 #saving PDF with Day and HCP Zone in file name
 gt4 %>%
   gtsave(filename = paste0('./pdfs/Day', paste(y), "_HCPzone4.pdf" ))
 
 
 
 
 
#HCP Zone 5
 #prep
 #subset for zone
 rut_HCPzone5 <- subset(output, HCPzone == 5)
 #adding additional write in columns
 rut_HCPzone5$NestInspected <- rep("O", each = length(rut_HCPzone5$nestID))
 rut_HCPzone5$ReasonNotInspected <- rep(" ", each = length(rut_HCPzone5$nestID))
 rut_HCPzone5$NoRuts  <- rep("O", each = length(rut_HCPzone5$nestID))
 rut_HCPzone5$HandRaked  <- rep("O", each = length(rut_HCPzone5$nestID))
 rut_HCPzone5$BarRaked  <- rep("O", each = length(rut_HCPzone5$nestID))
 rut_HCPzone5$Comments <- rep(" ", each = length(rut_HCPzone5$nestID)) 
 #ordering nests from N to S using Latitude
 rut_HCPzone5 <- rut_HCPzone5[order(rut_HCPzone5$lat, decreasing = T),]
 #removing columns for HCP zone & Latitude from table
 rut_HCPzone5 <- rut_HCPzone5[, -c(3,4)]
 
 #save
 #creating table
 gt5 <- gt(rut_HCPzone5)
 #editing format of table
 gt5 <- gt5 |>
   #adding title and subtitle
   tab_header(
     title = "Volusia County Sea Turtle Monitoring HCP Zone 5",
     subtitle = paste0("Date: ", paste(format(as.Date(friday + y,
                                                      origin = "2023-12-31"), 
                                              "%m/%d/%Y")),  
                       " Name_________________________________ Start Time__________________ 
                        End Time__________________  
                        Beach Condition___________________________________")
   )|>
   #centering values in each cell
   cols_align(align = "center") |>
   #adding horizontal lines between each column
   tab_style(style = cell_borders(sides = c("left", "right"), color = "black",
                                  style = "solid"), locations = cells_body()) |>
   #making rows narrower
   tab_options(data_row.padding = px(1)) |>
   
   #changing column names to better names
   cols_label(
     nestID = "Nest ID",
     address = "Address",
     city = "City",
     date_i = "Flag Date",
     NestInspected = "Nest Inspected",
     ReasonNotInspected = "Reason Not Inspected",
     NoRuts = "No Ruts",
     HandRaked = "Hand Raked",
     BarRaked = "Bar Raked",
     Comments = "Comments"
   ) |>
   
   #making comment column wider
   cols_width(
     Comments ~ px(200)
   )
 
 #saving PDF with Day and HCP Zone in file name
 gt5 %>%
   gtsave(filename = paste0('./pdfs/Day', paste(y), "_HCPzone5.pdf" ))
 
 
 
 
 
#HCP Zone 6
 #prep
 #subset for zone
 rut_HCPzone6 <- subset(output, HCPzone == 6)
 #adding additional write in columns
 rut_HCPzone6$NestInspected <- rep("O", each = length(rut_HCPzone6$nestID))
 rut_HCPzone6$ReasonNotInspected <- rep(" ", each = length(rut_HCPzone6$nestID))
 rut_HCPzone6$NoRuts  <- rep("O", each = length(rut_HCPzone6$nestID))
 rut_HCPzone6$HandRaked  <- rep("O", each = length(rut_HCPzone6$nestID))
 rut_HCPzone6$BarRaked  <- rep("O", each = length(rut_HCPzone6$nestID))
 rut_HCPzone6$Comments <- rep(" ", each = length(rut_HCPzone6$nestID)) 
 #ordering nests from N to S using Latitude
 rut_HCPzone6 <- rut_HCPzone6[order(rut_HCPzone6$lat, decreasing = T),]
 #removing columns for HCP zone & Latitude from table
 rut_HCPzone6 <- rut_HCPzone6[, -c(3,4)]
 
 #save
 #creating table
 gt6 <- gt(rut_HCPzone6)
 #editing format of table
 gt6 <- gt6 |>
   #adding title and subtitle
   tab_header(
     title = "Volusia County Sea Turtle Monitoring HCP Zone 6",
     subtitle = paste0("Date: ", paste(format(as.Date(friday + y,
                                                      origin = "2023-12-31"), 
                                              "%m/%d/%Y")),  
                       " Name_________________________________ Start Time__________________ 
                        End Time__________________  
                        Beach Condition___________________________________")
   )|>
   #centering values in each cell
   cols_align(align = "center") |>
   #adding horizontal lines between each column
   tab_style(style = cell_borders(sides = c("left", "right"), color = "black",
                                  style = "solid"), locations = cells_body()) |>
   #making rows narrower
   tab_options(data_row.padding = px(1)) |>
   
   #changing column names to better names
   cols_label(
     nestID = "Nest ID",
     address = "Address",
     city = "City",
     date_i = "Flag Date",
     NestInspected = "Nest Inspected",
     ReasonNotInspected = "Reason Not Inspected",
     NoRuts = "No Ruts",
     HandRaked = "Hand Raked",
     BarRaked = "Bar Raked",
     Comments = "Comments"
   ) |>
   
   #making comment column wider
   cols_width(
     Comments ~ px(200)
   )
 
 #saving PDF with Day and HCP Zone in file name
 gt6 %>%
   gtsave(filename = paste0('./pdfs/Day', paste(y), "_HCPzone6.pdf" ))
 
 
 
 
 
#HCP Zone 7
 #prep
 #subset for zone
 rut_HCPzone7 <- subset(output, HCPzone == 7)
 #adding additional write in columns
 rut_HCPzone7$NestInspected <- rep("O", each = length(rut_HCPzone7$nestID))
 rut_HCPzone7$ReasonNotInspected <- rep(" ", each = length(rut_HCPzone7$nestID))
 rut_HCPzone7$NoRuts  <- rep("O", each = length(rut_HCPzone7$nestID))
 rut_HCPzone7$HandRaked  <- rep("O", each = length(rut_HCPzone7$nestID))
 rut_HCPzone7$BarRaked  <- rep("O", each = length(rut_HCPzone7$nestID))
 rut_HCPzone7$Comments <- rep(" ", each = length(rut_HCPzone7$nestID)) 
 #ordering nests from N to S using Latitude
 rut_HCPzone7 <- rut_HCPzone7[order(rut_HCPzone7$lat, decreasing = T),]
 #removing columns for HCP zone & Latitude from table
 rut_HCPzone7 <- rut_HCPzone7[, -c(3,4)]
 
 #save
 #creating table
 gt7 <- gt(rut_HCPzone7)
 #editing format of table
 gt7 <- gt7 |>
   #adding title and subtitle
   tab_header(
     title = "Volusia County Sea Turtle Monitoring HCP Zone 7",
     subtitle = paste0("Date: ", paste(format(as.Date(friday + y,
                                                      origin = "2023-12-31"), 
                                              "%m/%d/%Y")),  
                       " Name_________________________________ Start Time__________________ 
                        End Time__________________  
                        Beach Condition___________________________________")
   )|>
   #centering values in each cell
   cols_align(align = "center") |>
   #adding horizontal lines between each column
   tab_style(style = cell_borders(sides = c("left", "right"), color = "black",
                                  style = "solid"), locations = cells_body()) |>
   #making rows narrower
   tab_options(data_row.padding = px(1)) |>
   
   #changing column names to better names
   cols_label(
     nestID = "Nest ID",
     address = "Address",
     city = "City",
     date_i = "Flag Date",
     NestInspected = "Nest Inspected",
     ReasonNotInspected = "Reason Not Inspected",
     NoRuts = "No Ruts",
     HandRaked = "Hand Raked",
     BarRaked = "Bar Raked",
     Comments = "Comments"
   ) |>
   
   #making comment column wider
   cols_width(
     Comments ~ px(200)
   )
 
 #saving PDF with Day and HCP Zone in file name
 gt7 %>%
   gtsave(filename = paste0('./pdfs/Day', paste(y), "_HCPzone7.pdf" ))
 
 
 
 
 
#HCP Zone 8
 #prep
 #subset for zone
 rut_HCPzone8 <- subset(output, HCPzone == 8)
 #adding additional write in columns
 rut_HCPzone8$NestInspected <- rep("O", each = length(rut_HCPzone8$nestID))
 rut_HCPzone8$ReasonNotInspected <- rep(" ", each = length(rut_HCPzone8$nestID))
 rut_HCPzone8$NoRuts  <- rep("O", each = length(rut_HCPzone8$nestID))
 rut_HCPzone8$HandRaked  <- rep("O", each = length(rut_HCPzone8$nestID))
 rut_HCPzone8$BarRaked  <- rep("O", each = length(rut_HCPzone8$nestID))
 rut_HCPzone8$Comments <- rep(" ", each = length(rut_HCPzone8$nestID)) 
 #ordering nests from N to S using Latitude
 rut_HCPzone8 <- rut_HCPzone8[order(rut_HCPzone8$lat, decreasing = T),]
 #removing columns for HCP zone & Latitude from table
 rut_HCPzone8 <- rut_HCPzone8[, -c(3,4)]
 
 #save
 #creating table
 gt8 <- gt(rut_HCPzone8)
 #editing format of table
 gt8 <- gt8 |>
   #adding title and subtitle
   tab_header(
     title = "Volusia County Sea Turtle Monitoring HCP Zone 8",
     subtitle = paste0("Date: ", paste(format(as.Date(friday + y,
                                                      origin = "2023-12-31"), 
                                              "%m/%d/%Y")),  
                       " Name_________________________________ Start Time__________________ 
                        End Time__________________  
                        Beach Condition___________________________________")
   )|>
   #centering values in each cell
   cols_align(align = "center") |>
   #adding horizontal lines between each column
   tab_style(style = cell_borders(sides = c("left", "right"), color = "black",
                                  style = "solid"), locations = cells_body()) |>
   #making rows narrower
   tab_options(data_row.padding = px(1)) |>
   
   #changing column names to better names
   cols_label(
     nestID = "Nest ID",
     address = "Address",
     city = "City",
     date_i = "Flag Date",
     NestInspected = "Nest Inspected",
     ReasonNotInspected = "Reason Not Inspected",
     NoRuts = "No Ruts",
     HandRaked = "Hand Raked",
     BarRaked = "Bar Raked",
     Comments = "Comments"
   ) |>
   
   #making comment column wider
   cols_width(
     Comments ~ px(200)
   )
 
 #saving PDF with Day and HCP Zone in file name
 gt8 %>%
   gtsave(filename = paste0('./pdfs/Day', paste(y), "_HCPzone8.pdf" ))
 
 
 
 
 
#HCP Zone 9
 #prep
 #subset for zone
 rut_HCPzone9 <- subset(output, HCPzone == 9)
  #subset for nests north of point to be raked
 rut_HCPzone9 <- subset(rut_HCPzone9, lat >= 28.981398)
 #adding additional write in columns
 rut_HCPzone9$NestInspected <- rep("O", each = length(rut_HCPzone9$nestID))
 rut_HCPzone9$ReasonNotInspected <- rep(" ", each = length(rut_HCPzone9$nestID))
 rut_HCPzone9$NoRuts  <- rep("O", each = length(rut_HCPzone9$nestID))
 rut_HCPzone9$HandRaked  <- rep("O", each = length(rut_HCPzone9$nestID))
 rut_HCPzone9$BarRaked  <- rep("O", each = length(rut_HCPzone9$nestID))
 rut_HCPzone9$Comments <- rep(" ", each = length(rut_HCPzone9$nestID)) 
 #ordering nests from N to S using Latitude
 rut_HCPzone9 <- rut_HCPzone9[order(rut_HCPzone9$lat, decreasing = T),]
 #removing columns for HCP zone & Latitude from table
 rut_HCPzone9 <- rut_HCPzone9[, -c(3,4)]
 
 #save
 #creating table
 gt9 <- gt(rut_HCPzone9)
 #editing format of table
 gt9 <- gt9 |>
   #adding title and subtitle
   tab_header(
     title = "Volusia County Sea Turtle Monitoring HCP Zone 9",
     subtitle = paste0("Date: ", paste(format(as.Date(friday + y,
                                                      origin = "2023-12-31"), 
                                              "%m/%d/%Y")),  
                       " Name_________________________________ Start Time__________________ 
                        End Time__________________  
                        Beach Condition___________________________________")
   )|>
   #centering values in each cell
   cols_align(align = "center") |>
   #adding horizontal lines between each column
   tab_style(style = cell_borders(sides = c("left", "right"), color = "black",
                                  style = "solid"), locations = cells_body()) |>
   #making rows narrower
   tab_options(data_row.padding = px(1)) |>
   
   #changing column names to better names
   cols_label(
     nestID = "Nest ID",
     address = "Address",
     city = "City",
     date_i = "Flag Date",
     NestInspected = "Nest Inspected",
     ReasonNotInspected = "Reason Not Inspected",
     NoRuts = "No Ruts",
     HandRaked = "Hand Raked",
     BarRaked = "Bar Raked",
     Comments = "Comments"
   ) |>
   
   #making comment column wider
   cols_width(
     Comments ~ px(200)
   )
 
 #saving PDF with Day and HCP Zone in file name
 gt9 %>%
   gtsave(filename = paste0('./pdfs/Day', paste(y), "_HCPzone9.pdf" ))
 
 
  }





####Combining One PDF for each HCP Zone for the week ####
  #HCP Zone 2
pdf_combine(input = c("./pdfs/Day0_HCPzone2.pdf", "./pdfs/Day1_HCPzone2.pdf", 
                      "./pdfs/Day2_HCPzone2.pdf", "./pdfs/Day3_HCPzone2.pdf",
                      "./pdfs/Day4_HCPzone2.pdf", "./pdfs/Day5_HCPzone2.pdf",
                      "./pdfs/Day6_HCPzone2.pdf"), output = "./pdfs/HCPzone2.pdf")

#HCP Zone 3
pdf_combine(input = c("./pdfs/Day0_HCPzone3.pdf", "./pdfs/Day1_HCPzone3.pdf", 
                      "./pdfs/Day2_HCPzone3.pdf", "./pdfs/Day3_HCPzone3.pdf",
                      "./pdfs/Day4_HCPzone3.pdf", "./pdfs/Day5_HCPzone3.pdf",
                      "./pdfs/Day6_HCPzone3.pdf"), output = "./pdfs/HCPzone3.pdf")

#HCP Zone 4
pdf_combine(input = c("./pdfs/Day0_HCPzone4.pdf", "./pdfs/Day1_HCPzone4.pdf", 
                      "./pdfs/Day2_HCPzone4.pdf", "./pdfs/Day3_HCPzone4.pdf",
                      "./pdfs/Day4_HCPzone4.pdf", "./pdfs/Day5_HCPzone4.pdf",
                      "./pdfs/Day6_HCPzone4.pdf"), output = "./pdfs/HCPzone4.pdf")

#HCP Zone 5
pdf_combine(input = c("./pdfs/Day0_HCPzone5.pdf", "./pdfs/Day1_HCPzone5.pdf", 
                      "./pdfs/Day2_HCPzone5.pdf", "./pdfs/Day3_HCPzone5.pdf",
                      "./pdfs/Day4_HCPzone5.pdf", "./pdfs/Day5_HCPzone5.pdf",
                      "./pdfs/Day6_HCPzone5.pdf"), output = "./pdfs/HCPzone5.pdf")

#HCP Zone 6
pdf_combine(input = c("./pdfs/Day0_HCPzone6.pdf", "./pdfs/Day1_HCPzone6.pdf", 
                      "./pdfs/Day2_HCPzone6.pdf", "./pdfs/Day3_HCPzone6.pdf",
                      "./pdfs/Day4_HCPzone6.pdf", "./pdfs/Day5_HCPzone6.pdf",
                      "./pdfs/Day6_HCPzone6.pdf"), output = "./pdfs/HCPzone6.pdf")

#HCP Zone 7
pdf_combine(input = c("./pdfs/Day0_HCPzone7.pdf", "./pdfs/Day1_HCPzone7.pdf", 
                      "./pdfs/Day2_HCPzone7.pdf", "./pdfs/Day3_HCPzone7.pdf",
                      "./pdfs/Day4_HCPzone7.pdf", "./pdfs/Day5_HCPzone7.pdf",
                      "./pdfs/Day6_HCPzone7.pdf"), output = "./pdfs/HCPzone7.pdf")

#HCP Zone 8
pdf_combine(input = c("./pdfs/Day0_HCPzone8.pdf", "./pdfs/Day1_HCPzone8.pdf", 
                      "./pdfs/Day2_HCPzone8.pdf", "./pdfs/Day3_HCPzone8.pdf",
                      "./pdfs/Day4_HCPzone8.pdf", "./pdfs/Day5_HCPzone8.pdf",
                      "./pdfs/Day6_HCPzone8.pdf"), output = "./pdfs/HCPzone8.pdf")

#HCP Zone 9
pdf_combine(input = c("./pdfs/Day0_HCPzone9.pdf", "./pdfs/Day1_HCPzone9.pdf", 
                      "./pdfs/Day2_HCPzone9.pdf", "./pdfs/Day3_HCPzone9.pdf",
                      "./pdfs/Day4_HCPzone9.pdf", "./pdfs/Day5_HCPzone9.pdf",
                      "./pdfs/Day6_HCPzone9.pdf"), output = "./pdfs/HCPzone9.pdf")




