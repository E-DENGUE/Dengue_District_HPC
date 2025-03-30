##This file includes the process to get the final data swt the we use it for monthly analysis
## The data cleaning process including obtain the data from different resources, here these data has been arranged to match our new boundaries
## To see how we obtained the new boundaries please see shapefile_new_from_2004.rmd
##The data process in  this file includes dengue cases, population, weather variable form ERA5, preventive factors and sociology economic factors.



################################################################################################Dengue cases 
library(ggplot2)
library(dplyr)
library(readxl) 
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(spdep)
library(sp)
library(MASS)
library(surveillance)
library(tidyr)
library(splines)
library(tidyverse)
library(tsibble)
library(sf)
library(scales)
library(RColorBrewer)
library(viridis)
library(readxl)
Dengue<- read_excel("C:/Users/uqwareed/OneDrive - The University of Queensland/Two months_predictions/HPC_District_monthly_update_lag/Raw_data/20231130_ED_MDR Dengue district data_2000-2023_original modification_updated 20May24.xlsx", sheet=1)
Dengue_filtered_data <- Dengue[Dengue$year >= 2004, ]
dim(Dengue_filtered_data)

unique(Dengue_filtered_data$district)

sum(is.na(Dengue_filtered_data$Dengue))
sum(is.na(Dengue_filtered_data$SevereDHF))


#Fill the missing values using two ways
##Use the average fro the same month in previous years.

Dengue_filtered_data <- Dengue_filtered_data %>%
  arrange(district, province, year, month)

# Define a function to fill missing values with the average from the same month in preceding years
fill_missing_with_average <- function(data, column) {
  data %>%
    group_by(district, province, month) %>%
    mutate({{column}} := ifelse(is.na({{column}}), mean({{column}}[year %in% (max(year) - 1):(min(year) + 1)], na.rm = TRUE), {{column}})) %>%
    ungroup()
}

# Apply the function to the columns "Dengue" and "SevereDHF"
Dengue_filtered_data <- Dengue_filtered_data %>%
  fill_missing_with_average(., Dengue) %>%
  fill_missing_with_average(., SevereDHF)

Dengue_filtered_data$Dengue<- ceiling(Dengue_filtered_data$Dengue)
Dengue_filtered_data$SevereDHF<- ceiling(Dengue_filtered_data$SevereDHF)
# Count the number of remaining missing values
missing_dengue <- sum(is.na(Dengue_filtered_data$Dengue))
missing_severe_dhf <- sum(is.na(Dengue_filtered_data$SevereDHF))

# Print the result
cat("Missing values in Dengue:", missing_dengue, "\n")
cat("Missing values in SevereDHF:", missing_severe_dhf, "\n")

#####Match the boundaries 

Dengue_filtered_data$district<- toupper(Dengue_filtered_data$district)
# Assuming your data frame is named filtered_data
Dengue_filtered_data_summarized <- Dengue_filtered_data %>%  group_by(province, district, year, month)%>%
  mutate(
    district = ifelse(
      (district %in% c("VINH LOI", "HOA BINH")) & (province == "BAC LIEU"),
      "VINH LOI",
      ifelse(
        (district %in% c("MO CAY","MO CAY NAM", "MO CAY BAC")) & (province == "BEN TRE"),
        "MO CAY",
        ifelse(
          (district %in% c("CO DO", "THOI LAI")) & (province == "CAN THO"),
          "CO DO",
          ifelse(
            (district %in% c("HONG NGU DISTRICT", "HONG NGU CITY" )) & (province ==  "DONG THAP" ),
            "HONG NGU",
            ifelse(
              (district %in% c( "NGA BAY CITY" , "PHUNG HIEP" )) & (province ==  "HAU GIANG" ),
              "NGA BAY",
              ifelse(
                (district %in% c( "LONG MY"  , "LONG MY TOWN" )) & (province ==  "HAU GIANG" ) ,
                "LONG MY",
                ifelse(
                  (district %in% c(  "AN MINH",  "U MINH THUONG"  , "AN BIEN" , "VINH THUAN"  )) & (province == "KIEN GIANG" ),
                  "AN MINH",
                  ifelse(
                    (district %in% c(  "KIEN LUONG",   "GIANG THANH"  )) & (province == "KIEN GIANG" ),
                    "KIEN LUONG",
                    ifelse(
                      (district %in% c(  "KIEN TUONG",   "MOC HOA"  )) & (province == "LONG AN" ),
                      "MOC HOA",
                      ifelse(
                        (district %in% c(  "LONG PHU" ,   "MY XUYEN","TRAN DE"   )) & (province == "SOC TRANG" ),
                        "LONG PHU",
                        ifelse(
                          (district %in% c(  "CHAU THANH" ,   "MY TU" )) & (province == "SOC TRANG" ),
                          "MY TU",
                          ifelse(
                            (district %in% c(  "CAI LAY" ,   "CAI LAY CITY" ,"CAI LAY DISTRICT"  )) & (province == "TIEN GIANG" ),
                            "CAI LAY",
                            ifelse(
                              (district %in% c(  "GO CONG TAY" ,   "GO CONG DONG", "TAN PHU DONG"   )) & (province == "TIEN GIANG" ),
                              "GO CONG DONG",
                              ifelse(
                                (district %in% c(  "DUYEN HAI DISTRICT" ,   "DUYEN HAI", "DUYEN HAI TOWN" ,"TRA CU"  )) & (province == "TRA VINH"   ),
                                "TRA CU",
                                ifelse(
                                  (district %in% c(    "BINH MINH", "BINH TAN"   )) & (province == "VINH LONG" ),
                                  "BINH MINH",
                                  district
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ) )%>%
  
  summarise(Dengue = sum(Dengue,na.rm = TRUE), SevereDHF = sum(SevereDHF,na.rm = TRUE)) %>%
  ungroup()

Dengue_filtered_data_summarized <- Dengue_filtered_data_summarized %>%
  mutate(
    district = ifelse(district ==  "RACH GIA CI TY"  , "RACH GIA CITY" , district)
  )



MDR_NEW <- st_read(dsn = "C:/Users/uqwareed/OneDrive - The University of Queensland/Re District-level data_DUNG/MDR_NEW_Boundaries.shp")

MDR_NEW$VARNAME  <- toupper(MDR_NEW$VARNAME)
Dengue_filtered_data_summarized$district<- toupper(Dengue_filtered_data_summarized$district)
unique(MDR_NEW$VARNAME)

Dengue_filtered_data_summarized$province<- toupper(Dengue_filtered_data_summarized$province)
MDR_NEW$NAME_En<- toupper( MDR_NEW$NAME_En)

matching_district <- MDR_NEW$VARNAME %in% Dengue_filtered_data_summarized$district &
  MDR_NEW$NAME_En %in% Dengue_filtered_data_summarized$province

matching_district 
unique(Dengue_filtered_data_summarized$district%in% MDR_NEW$VARNAME )
# Identify values in filtered_data_summarized$district that are not in MDR_NEW$VARNAME
not_in_MDR_NEW <- setdiff(Dengue_filtered_data_summarized$district, MDR_NEW$VARNAME)

setdiff( MDR_NEW$VARNAME,Dengue_filtered_data_summarized$district)
setdiff(Dengue_filtered_data_summarized$district, MDR_NEW$VARNAME)
# Print or inspect the values
print(not_in_MDR_NEW)

# Display the matching values
matched_names <- unique(MDR_NEW$VARNAME[matching_district])

Non_matching<- unique(MDR_NEW$VARNAME[!matching_district])

Non_matching
# Display the non-matching values
# Assuming matching_district is a logical vector
indices_not_matching <- which(!matching_district)


# Display the indices or positions where matching_district is FALSE
q<- (unique(paste(MDR_NEW$VARNAME, MDR_NEW$NAME_En, sep = "_")))
#subset(MDR_NEW,MDR_NEW$VARNAME=="HONG NGU" )
#subset(MDR_2,MDR_2$VARNAME_2=="HONG NGU" )
non_unique_combinations <- MDR_NEW[duplicated(paste(MDR_NEW$VARNAME, MDR_NEW$NAME_En, sep = "_")) | duplicated(paste(MDR_NEW$VARNAME, MDR_NEW$NAME_En, sep = "_"), fromLast = TRUE), ]

MDR_Dengue_map_pre <- left_join( Dengue_filtered_data_summarized,MDR_NEW, by = c( "district"="VARNAME" ,"province"="NAME_En"))

dim(MDR_Dengue_map_pre)
names(MDR_Dengue_map_pre)
selected_columns <- c(
  "province",  "district",  "year" ,     "month"   ,  "Dengue",    "SevereDHF"
  ,  "NAME_1",       "NAME_2"   ,"TYPE_2"    ,"ENGTYPE" ,     "ID")

# Create the new data frame
MDR_Dengue_map_pre <- MDR_Dengue_map_pre %>%
  dplyr::select(all_of(selected_columns))

MDR_Dengue_map_Final<- st_drop_geometry(MDR_Dengue_map_pre)

BAC_LIEU_N <- MDR_Dengue_map_Final[MDR_Dengue_map_Final$district=='BAC LIEU',]

ggplot(BAC_LIEU_N, aes(x = month, y = Dengue, group = year, color = factor(year))) +
  geom_line() +
  labs(title = "Monthly Dengue Cases in Bac Lieu",
       x = "Month",
       y = "Number of Dengue Cases",
       color = "Year") +
  theme_minimal()


BAC_LIEU_N$date <- as.Date(paste(BAC_LIEU_N$year, BAC_LIEU_N$month, "1", sep = "-"))

# Create a time series plot
ggplot(BAC_LIEU_N, aes(x = date, y = Dengue)) +
  geom_line() +
  labs(title = "Time Series of Dengue Cases in Bac Lieu",
       x = "Date",
       y = "Number of Dengue Cases") +
  theme_minimal()

################################################Population Data

#The NIHE team has provided population data obtained for the years 2009 to 2022. The provided code clean the data provided by them and the We 
#are seeking estimates for the population data spanning from 2000 to 2022.

