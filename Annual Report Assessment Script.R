remove(list=ls()) #removes all objects from project

library(RODBC)
library(readxl)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(gghighlight)
library(RColorBrewer)
library(rvest)
library(viridis)
library(Hmisc)
library(ggpmisc)
library(ggrepel)


# Import Project Data -----------------------------------------------------

Cattail_ERDP_WQ_DATA <- read_csv("Data/Cattail_Data.csv")
ERDP_Soil_Data <- read_csv("Data/ERDP_Soil_Data.csv")
L8FEBOG_DBHYDRO_SAMPLE_WITH_QC_VIEW <- read_csv("Data/L8FEB_Data.csv")
PFLUX_SOIL_ERDP_DATA <- read_csv("Data/PFLUX_Data.csv")
PFLUX_ERDP_WQ_DATA <- read_csv("Data/PFLUX_WQ_Data.csv")
ERDP_WQ_DATA <- read_csv("Data/ERDP_WQ_DATA.csv")
PDYNAMICS_ERDP_WQ_DATA <- read_csv("Data/PDYNAMICS_ERDP_WQ_DATA.csv")
PDYNAMICS_ERDP_SOIL_DATA <- read_csv("Data/PDYNAMICS_ERDP_SOIL_DATA.csv")

write.csv(L8FEBOG_DBHYDRO_SAMPLE_WITH_QC_VIEW,file="./Data/L8FEB_Data.csv")
write.csv(PFLUX_SOIL_ERDP_DATA,file="./Data/PFLUX_Data.csv")
write.csv(PFLUX_ERDP_WQ_DATA,file="./Data/PFLUX_WQ_Data.csv")
write.csv(Cattail_ERDP_WQ_DATA,file="./Data/Cattail_Data.csv")
write.csv(SOIL_ERDP_DATA,file="./Data/ERDP_Soil_Data.csv")



# Screen data for report time period --------------------------------------

Latest_Collection_date <-"2021-05-01 00:00:00"         #latest date of collected data
Earliest_Collection_date <- "2020-05-01 00:00:00"      #Earliest date of collected data 

#Projects with soils data by year in ERDP 
ERDP_Soils_data_by_year <-ERDP_Soil_Data %>%
mutate(DATE=ymd_hms(SDATE)) %>%
mutate(year=year(SDATE)) %>%
group_by(year) %>%  
distinct(SUBSTUDY) 

#projects with WQ data by year in ERDP
ERDP_WQ_data_by_year <-ERDP_WQ_DATA %>%
mutate(DATE=ymd(SDATE)) %>%
mutate(year=year(SDATE)) %>%
group_by(year) %>%  
distinct(SUBSTUDY) 

#WY21 WQ data in ERDP
ERDP_WQ_WY21_Data <- ERDP_WQ_DATA %>%
filter(ymd(SDATE) <"2021-05-01",ymd(SDATE)>="2020-05-01")

#WY21 WQ data in DBHYDRO
DBHYDRO_WQ_WY21_Data <- L8FEBOG_DBHYDRO_SAMPLE_WITH_QC_VIEW %>%
filter(ymd_hms(DATE_COLLECTED) <"2021-05-01 00:00:00",ymd_hms(DATE_COLLECTED)>="2019-05-01 00:00:00")

#WY21 Soils data in ERDP
ERDP_Soils_WY21_Data <- ERDP_Soil_Data %>%
filter(ymd_hms(SDATE) <"2021-05-01 00:00:00",ymd_hms(SDATE)>="2020-05-01 00:00:00")

# DQOs Grabs------------------------------------------------------------
#Completeness
write_csv(bind_rows(DBHYDRO_Completeness(PARAMETER_Names(DBHYDRO_Time_Filter(L8FEBOG_DBHYDRO_SAMPLE_WITH_QC_VIEW))),
                    ERDP_Completeness(ERDP_Time_Filter(PDYNAMICS_ERDP_WQ_DATA))),path="./DQO Files/Completeness.csv")                        #Completeness

write_csv(bind_rows(DBHYDRO_Completeness_Parameters(PARAMETER_Names(DBHYDRO_Time_Filter(L8FEBOG_DBHYDRO_SAMPLE_WITH_QC_VIEW))),
                    ERDP_Completeness_Parameters(ERDP_Time_Filter(PDYNAMICS_ERDP_WQ_DATA))),path="./DQO Files/Completeness_Parameters.csv")  #Completeness with Parameters

#No replicate samples collected? 
#Precision
write_csv(na.omit(bind_rows(ERDP_Precision(ERDP_Time_Filter(PDYNAMICS_ERDP_WQ_DATA)))),path="./DQO Files/Precision.csv")   
#Precision
write_csv(bind_rows(DBHYDRO_Precision_Parameters(PARAMETER_Names(DBHYDRO_Time_Filter(L8FEBOG_DBHYDRO_SAMPLE_WITH_QC_VIEW))),
                    ERDP_Precision_Parameters(ERDP_Time_Filter(PDYNAMICS_ERDP_WQ_DATA)),
                    ERDP_Precision_Parameters(REST_ERDP_WQ_DATA),
                    ERDP_Precision_Parameters(PTS_ERDP_WQ_DATA),
                    ERDP_Precision_Parameters(PFLUX_ERDP_WQ_DATA)),path="./DQO Files/Precision_Parameters.csv")   #Precision with parameters

#Accuracy
write_csv(bind_rows(DBHYDRO_Accuracy(PARAMETER_Names(DBHYDRO_Time_Filter(L8FEBOG_DBHYDRO_SAMPLE_WITH_QC_VIEW))),
                    ERDP_Accuracy(ERDP_Time_Filter(PDYNAMICS_ERDP_WQ_DATA))),path="./DQO Files/Accuracy.csv")   #Precision

write_csv(bind_rows(DBHYDRO_Accuracy_Parameters(PARAMETER_Names(DBHYDRO_Time_Filter(L8FEBOG_DBHYDRO_SAMPLE_WITH_QC_VIEW))),
                    ERDP_Accuracy_Parameters(ERDP_Time_Filter(PDYNAMICS_ERDP_WQ_DATA))),path="./DQO Files/Accuracy_Parameters.csv")   #Precision


# DQOs autosampler --------------------------------------------------------
#Completeness
write_csv(bind_rows(DBHYDRO_Completeness_Autosampler(PARAMETER_Names(DBHYDRO_Time_Filter(L8FEBOG_DBHYDRO_SAMPLE_WITH_QC_VIEW)))),path="./DQO Files/Completeness_Autosamplers.csv")                        #Completeness

write_csv(bind_rows(DBHYDRO_Completeness_Parameters_Autosampler(PARAMETER_Names(DBHYDRO_Time_Filter(L8FEBOG_DBHYDRO_SAMPLE_WITH_QC_VIEW)))),path="./DQO Files/Completeness_Autosamplers_Parameters.csv")  #Completeness with Parameters

#Accuracy
write_csv(bind_rows(DBHYDRO_Autosampler_Accuracy(DBHYDRO_Time_Filter(L8FEBOG_DBHYDRO_SAMPLE_WITH_QC_VIEW))),path="./DQO Files/Accuracy_Autosamplers.csv")

write_csv(bind_rows(DBHYDRO_Accuracy_Autosampler_Parameters(DBHYDRO_Time_Filter(L8FEBOG_DBHYDRO_SAMPLE_WITH_QC_VIEW))),path="./DQO Files/Accuracy_Autosamplers_Parameters.csv")


# DQOs Soils  -------------------------------------------------------------
#Completeness
write_csv(ERDP_Soil_Completeness(ERDP_Time_Filter(PDYNAMICS_ERDP_SOIL_DATA)),path="./DQO Files/Completeness_Soils.csv")             #Completeness
write_csv(ERDP_Soil_Completeness_Parameters(ERDP_Time_Filter(PDYNAMICS_ERDP_SOIL_DATA)),path="./DQO Files/Completeness_Soils_Parameters.csv")  #Completeness with Parameters
#Precision
write_csv(ERDP_Precision_Soil(ERDP_Time_Filter(PDYNAMICS_ERDP_SOIL_DATA)),path="./DQO Files/Precision_Soils.csv")             #Precision
write_csv(ERDP_Precision_Soil_Parameters(ERDP_Time_Filter(PDYNAMICS_ERDP_SOIL_DATA)),path="./DQO Files/Precision_Soils_Parameters.csv")  #Precision with Parameters

# DBHYDRO Grab Functions -------------------------------------------------------

PARAMETER_Names <- function(df) #convert to standard analyte names. DBHYDRO TEST_NAME to ERDP PARAMETER 
{
  df1 <- df %>%
    mutate(TEST_NAME=case_when(TEST_NAME=="TEMP"~"Temp",
                               TEST_NAME=="DISSOLVED OXYGEN"~"DO",
                               TEST_NAME=="SP CONDUCTIVITY, FIELD"~"SpCond",
                               TEST_NAME=="PH, FIELD"~"PH",
                               TEST_NAME=="TOTAL SUSPENDED SOLIDS"~"TSS",
                               TEST_NAME=="NITRATE+NITRITE-N"~"NOX",
                               TEST_NAME=="AMMONIA-N"~"NH4",
                               TEST_NAME=="PHOSPHATE, ORTHO AS P"~"OPO4",
                               TEST_NAME=="PHOSPHATE, TOTAL AS P"~"TPO4",
                               TEST_NAME=="PHOSPHATE, DISSOLVED AS P"~"TDPO4",
                               TEST_NAME=="SODIUM"~"NA",
                               TEST_NAME=="POTASSIUM"~"K",
                               TEST_NAME=="CALCIUM"~"CA",
                               TEST_NAME=="MAGNESIUM"~"MG",
                               TEST_NAME=="CHLORIDE"~"CL",
                               TEST_NAME=="SULFATE"~"SO4",
                               TEST_NAME=="HARDNESS AS CACO3"~"HARDNESS",
                               TEST_NAME=="ALKALINITY, TOT, CACO3"~"ALKA",
                               TEST_NAME=="TOTAL NITROGEN"~"TN",
                               TEST_NAME=="DEPTH, TOTAL"~"Depth",
                               TEST_NAME=="NO BOTTLE SAMPLE"~"No Bottle",
                               TEST_NAME=="CARBON, DISSOLVED ORGANIC"~"DOC",
                               TEST_NAME=="TURBIDITY"~"Turbidity",
                               TEST_NAME=="VOLATILE SUSPENDED SOLIDS"~"VSS",
                               TEST_NAME=="CHLOROPHYLL-A(LC)"~"CHLOROPHYLL A",
                               TEST_NAME=="PHEOPHYTIN-A(LC)"~"PHEOPHYTIN A"))
  return(df1)                             
}


DBHYDRO_Completeness  <- function(df)
{
  df1 <- df  %>%
    filter(SOURCE != "FLD") %>%                                        #Remove Field Paramters
    filter(COLLECT_METHOD=="G" | COLLECT_METHOD=="GP") %>%              # Grab Samples only
    mutate(Date=as.Date(DATE_COLLECTED)) %>%  
    mutate(`Collection Agency`="") %>%
    rename(SUBSTUDY="PROJECT_CODE") %>%
    group_by(SUBSTUDY,`Collection Agency`) %>%
    summarise(Samples=sum(if_else(SAMPLE_TYPE_NEW=="SAMP",1,0)),FCEB=sum(if_else(SAMPLE_TYPE_NEW=="FCEB",1,0)),FD=sum(if_else(SAMPLE_TYPE_NEW=="FD",1,0)),FB=sum(if_else(SAMPLE_TYPE_NEW=="FB",1,0)),RS=sum(if_else(SAMPLE_TYPE_NEW=="RS",1,0)),EB=sum(if_else(SAMPLE_TYPE_NEW=="EB",1,0)),`Total Blanks`=FCEB+FB+EB, `Blank %`=percent(`Total Blanks`/Samples))
  return(df1)  
}

DBHYDRO_Completeness_Parameters  <- function(df)
{
  df1 <- df  %>%
    filter(SOURCE != "FLD") %>%                                        #Remove Field Paramters
    filter(COLLECT_METHOD=="G" | COLLECT_METHOD=="GP") %>%                                    # Grab Samples only
    mutate(Date=as.Date(DATE_COLLECTED)) %>% 
    mutate(`Collection Agency`="") %>%                                             #Collecting Agency unknown. How can this be discerned from DBHYDRO DB?
    rename(SUBSTUDY="PROJECT_CODE",PARAMETER="TEST_NAME") %>%  
    group_by(SUBSTUDY,`Collection Agency`,PARAMETER)  %>%  
    summarise(Samples=sum(if_else(SAMPLE_TYPE_NEW=="SAMP",1,0)),FCEB=sum(if_else(SAMPLE_TYPE_NEW=="FCEB",1,0)),FD=sum(if_else(SAMPLE_TYPE_NEW=="FD",1,0)),FB=sum(if_else(SAMPLE_TYPE_NEW=="FB",1,0)),RS=sum(if_else(SAMPLE_TYPE_NEW=="RS",1,0)),EB=sum(if_else(SAMPLE_TYPE_NEW=="EB",1,0)),`Total Blanks`=FCEB+FB+EB, `Blank %`=percent(`Total Blanks`/Samples))
  return(df1)  
}

#Precision
DBHYDRO_Precision_Parameters <- function(df)
{  
  df1 <- df %>%
    filter(COLLECT_METHOD=="G") %>%
    filter(SOURCE != "FLD") %>%
    mutate(DATE=as.Date(DATE_COLLECTED)) %>%
    filter(SAMPLE_TYPE_NEW=="SAMP")  %>%
    left_join(Find_RS(df), by=c("DATE","STATION_ID","TEST_NAME"))  %>%
    filter(!is.na(Rep1)) %>%
    mutate(`Collection Agency`="") %>%   #Collecting Agency unknown. How can this be discerned from DBHYDRO DB?
    select(PROJECT_CODE,`Collection Agency`,TEST_NAME,VALUE,Rep1,Rep2) %>%
    gather("Replicate type","VALUE",Rep1,Rep2,VALUE) %>%
    rename(SUBSTUDY="PROJECT_CODE",PARAMETER="TEST_NAME") %>%  
    group_by(SUBSTUDY,`Collection Agency`,PARAMETER)  %>%  
    summarise(`Replicate Samples`=n(),RSD=percent(sd(VALUE)/mean(VALUE)))
  return(df1) 
}

DBHYDRO_Precision <- function(df)
{
  df1 <- DBHYDRO_Precision_Parameters(df) %>%  
    mutate(RSD=as.numeric(gsub("%", "",RSD)))  %>%
    group_by(SUBSTUDY,`Collection Agency`)  %>%  
    summarise(`Number of Parameters`=n(),`Average Parameter RSD`=percent(mean(RSD,na.rm = TRUE)/100))
  return(df1)
}

#find Blanks
Find_FCEB <-function(df)
{  
  df1 <-df  %>%
    mutate(DATE=as.Date(DATE_COLLECTED)) %>%
    filter(SAMPLE_TYPE_NEW=="FCEB") %>%
    select(DATE,STATION_ID,TEST_NAME,VALUE) %>%
    rename(FCEB=VALUE) 
  return(df1)
}

Find_RS <-function(df)   #works for 2 reps, not 1
{  
  df1 <-df  %>%
    mutate(DATE=as.Date(DATE_COLLECTED)) %>%
    filter(SAMPLE_TYPE_NEW=="RS") %>%
    select(SAMPLE_ID,DATE,STATION_ID,TEST_NAME,VALUE) %>%
    spread(SAMPLE_ID,VALUE)  %>%
    rename(Rep1=4,Rep2=5)
  return(df1)
}

Find_EB <-function(df)   #
{  
  df1 <-df  %>%
    mutate(DATE=as.Date(DATE_COLLECTED)) %>%
    filter(SAMPLE_TYPE_NEW=="EB") %>%
    select(SAMPLE_ID,DATE,STATION_ID,TEST_NAME,VALUE) %>%
    rename(EB=VALUE)
  return(df1)
}

Find_FB <-function(df)   #
{  
  df1 <-df  %>%
    mutate(DATE=as.Date(DATE_COLLECTED)) %>%
    filter(SAMPLE_TYPE_NEW=="FB") %>%
    select(SAMPLE_ID,DATE,STATION_ID,TEST_NAME,VALUE) %>%
    rename(FB=VALUE)
  return(df1)
}

#Accuracy
DBHYDRO_Accuracy_Parameters <-function(df)
{  
  df1 <-df  %>%
    filter(SOURCE != "FLD") %>%           #remove field parameters
    mutate(DATE=as.Date(DATE_COLLECTED)) %>%
    left_join(Find_FCEB(df), by=c("DATE","TEST_NAME"),keep=FALSE) %>%    #join FCEBs
    left_join(Find_EB(df), by=c("DATE","TEST_NAME"),keep=FALSE) %>%          #Join EBs
    mutate(`Greater than MDL`=ifelse(SAMPLE_TYPE_NEW=="SAMP",if_else(`VALUE` > MDL ,1,0),0)) %>%
    mutate(`Blank within 10x sample`=ifelse(SAMPLE_TYPE_NEW=="SAMP",if_else(`VALUE` <= abs(FCEB)*10 | `VALUE` <= abs(EB)*10 ,1,0),0)) %>%
    mutate(`Blank Hits`=if_else( `Blank within 10x sample`+`Greater than MDL`==2 ,1,0)) %>%
    mutate(`Collection Agency`="") %>%                                             #Collecting Agency unknown. How can this be discerned from DBHYDRO DB?
    rename(SUBSTUDY="PROJECT_CODE",PARAMETER="TEST_NAME") %>% 
    group_by(SUBSTUDY,`Collection Agency`,PARAMETER) %>%
    summarise(`Samples`=sum(if_else(SAMPLE_TYPE_NEW=="SAMP" ,1,0),na.rm=TRUE),             
              `Blanks`=sum(if_else(SAMPLE_TYPE_NEW=="FCEB" ,1,0),na.rm=TRUE),
              `Blank > MDL`=sum(`Greater than MDL`),
              `Value < Blank 10x`=sum(`Blank within 10x sample`,na.rm=TRUE),
              `Blank Hits`=sum(`Blank Hits`,na.rm=TRUE),
              `% Blank Hits`=percent(`Blank Hits`/`Samples`))  
  return(df1)
}  

DBHYDRO_Accuracy <-function(df)
{  
  df1 <-df  %>%
    DBHYDRO_Accuracy_Parameters() %>%
    group_by(SUBSTUDY,`Collection Agency`) %>%
    summarise(`Total Samples`=sum(Samples),
              `Total Blank Hits`=sum(`Blank Hits`,na.rm=TRUE),
              `Total Blank Hits (%)`=percent(`Total Blank Hits`/`Total Samples`))  
  return(df1)
}  


# ERDP Grab Functions ----------------------------------------------------------

Find_EB_ERDP <-function(df)
{  
  df1 <-df  %>%
    filter(FQC=="EB")   %>%   #EB Samples Only
    mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD",TRUE~"")) %>%
    mutate(DATE=as.Date(SDATE)) %>%
    filter(!is.na(PARAMETER)) %>%
    select(`Collection Agency`,DATAFILE,DATE,SITE,PARAMETER,SUBSITE,RESULT) %>%
    rename(EB="RESULT")
  return(df1)
}

Find_FD_ERDP <-function(df)
{  
  df1 <- df  %>%
    filter(FQC=="FD")   %>%   #FD Samples Only
    mutate(DATE=as.Date(SDATE)) %>%
    select(DATE,SITE,PARAMETER,SUBSITE,RESULT) %>%
    rename(FD="RESULT")
  return(df1)
}

Find_RS_ERDP <-function(df)
{  
  df1 <- df  %>%
    mutate(DATE=as.Date(SDATE)) %>%
    filter(FQC=="RS")   %>%   #Replicate Samples Only
    select(DATE,SITE,PARAMETER,SUBSITE,RESULT)  %>%
    rename(REP1="RESULT")
  return(df1)
}

ERDP_Completeness  <- function(df)
{
  
  number_agencies <-df %>%   #find number of collecting agencies
    mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE", str_detect(FLDCOMMENT,"WMD") ~"WMD",
                                         TRUE~"")) %>%
    distinct(`Collection Agency`)  
  
  if(nrow(number_agencies)<1)
  {
    df1 <- df  %>%
      filter(FLDMETHOD == "GRAB") %>%                                       #Remove Field Parameters
      mutate(DATE=as.Date(SDATE)) %>%
      filter(!is.na(PARAMETER)) %>%
      group_by(SUBSTUDY) %>%
      summarise(Samples=sum(if_else(is.na(FQC),1,0)),FCEB=sum(if_else(FQC=="FCEB",1,0),na.rm=TRUE),FD=sum(if_else(FQC=="FD",1,0),na.rm=TRUE),FB=sum(if_else(FQC=="FB",1,0),na.rm=TRUE),RS=sum(if_else(FQC=="RS",1,0),na.rm=TRUE),EB=sum(if_else(FQC=="EB",1,0),na.rm=TRUE),`Total Blanks`=FCEB+FB+EB, `Blank %`=percent(`Total Blanks`/Samples))
  }
  
  if(nrow(number_agencies)>=1)
  {
    df1 <- df  %>%
      filter(FLDMETHOD == "GRAB") %>%                                       #Remove Field Parameters
      mutate(DATE=as.Date(SDATE)) %>%
      mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD", TRUE~"")) %>%
      filter(!is.na(PARAMETER)) %>%
      group_by(SUBSTUDY,`Collection Agency`)  %>%
      summarise(Samples=sum(if_else(is.na(FQC),1,0)),FCEB=sum(if_else(FQC=="FCEB",1,0),na.rm=TRUE),FD=sum(if_else(FQC=="FD",1,0),na.rm=TRUE),FB=sum(if_else(FQC=="FB",1,0),na.rm=TRUE),RS=sum(if_else(FQC=="RS",1,0),na.rm=TRUE),EB=sum(if_else(FQC=="EB",1,0),na.rm=TRUE),`Total Blanks`=FCEB+FB+EB, `Blank %`=percent(`Total Blanks`/Samples))
    
  }
  
  return(df1)
}

ERDP_Completeness_Parameters  <- function(df)
{
  number_agencies <-df %>%   #find number of collecting agencies
    mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE", str_detect(FLDCOMMENT,"WMD") ~"WMD", TRUE~"")) %>%
    distinct(`Collection Agency`)  
  
  if(nrow(number_agencies)<1)
  {
    df1 <- df  %>%
      filter(FLDMETHOD == "GRAB") %>%                                       #Remove Field Parameters
      mutate(DATE=as.Date(SDATE)) %>%
      mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD", TRUE~"")) %>%
      filter(!is.na(PARAMETER)) %>%
      group_by(SUBSTUDY,`Collection Agency`,PARAMETER) %>%
      summarise(Samples=sum(if_else(is.na(FQC),1,0)),FCEB=sum(if_else(FQC=="FCEB",1,0),na.rm=TRUE),FD=sum(if_else(FQC=="FD",1,0),na.rm=TRUE),FB=sum(if_else(FQC=="FB",1,0),na.rm=TRUE),RS=sum(if_else(FQC=="RS",1,0),na.rm=TRUE),EB=sum(if_else(FQC=="EB",1,0),na.rm=TRUE),`Total Blanks`=FCEB+FB+EB, `Blank %`=percent(`Total Blanks`/Samples))
  }
  
  if(nrow(number_agencies)>=1)
  {
    df1 <- df  %>%
      filter(FLDMETHOD == "GRAB") %>%                                       #Remove Field Parameters
      mutate(DATE=as.Date(SDATE)) %>%
      mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD", TRUE~"")) %>%
      filter(!is.na(PARAMETER)) %>%
      group_by(SUBSTUDY,`Collection Agency`,PARAMETER) %>%
      summarise(Samples=sum(if_else(is.na(FQC),1,0)),FCEB=sum(if_else(FQC=="FCEB",1,0),na.rm=TRUE),FD=sum(if_else(FQC=="FD",1,0),na.rm=TRUE),FB=sum(if_else(FQC=="FB",1,0),na.rm=TRUE),RS=sum(if_else(FQC=="RS",1,0),na.rm=TRUE),EB=sum(if_else(FQC=="EB",1,0),na.rm=TRUE),`Total Blanks`=FCEB+FB+EB, `Blank %`=percent(`Total Blanks`/Samples))
  }
  return(df1)
}

ERDP_Precision_Parameters  <- function(df)
{
  if(nrow(Find_RS_ERDP(df))>=1)    #Only use this function when RS samples exist
  {
    df1 <- df%>%
      mutate(DATE=as.Date(SDATE)) %>%
      filter(is.na(FQC)) %>%  # remove reps as to not to join rep to self
      mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD",TRUE~"")) %>%
      left_join(Find_RS_ERDP(df) ,by=c("DATE","SITE","PARAMETER")) %>%
      filter(!is.na(REP1)) %>%
      gather("Replicate type","VALUE",REP1,RESULT) %>%
      select(SUBSTUDY,`Collection Agency`,DATE,SITE,PARAMETER,VALUE,`Replicate type`)  %>%
      filter(!is.na(PARAMETER)) %>%
      group_by(SUBSTUDY,`Collection Agency`,DATE,PARAMETER) %>%
      summarise(n=n(),RSD=sd(VALUE,na.rm=TRUE)/mean(VALUE,na.rm=TRUE)) %>%   #replicate samples collected on multiple days
      group_by(SUBSTUDY,`Collection Agency`,PARAMETER) %>%
      summarise(`Replicate Samples`=sum(n),`RSD`=percent(mean(RSD)))
    return(df1)
  }
  
  if(nrow(Find_FD_ERDP(df))>=1) 
  {
    df1 <- df %>%
      mutate(DATE=as.Date(SDATE)) %>%
      filter(is.na(FQC)) %>%  # remove reps as to not to join rep to self
      filter(!is.na(PARAMETER)) %>%
      mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD",TRUE~"")) %>%
      left_join(Find_FD_ERDP(df) ,by=c("DATE","SITE","PARAMETER","SUBSITE")) %>%
      filter(!is.na(FD)) %>%
      gather("Replicate type","VALUE",FD,RESULT) %>%
      select(SUBSTUDY,`Collection Agency`,DATE,SITE,SUBSITE,PARAMETER,VALUE,`Replicate type`)  %>%
      group_by(SUBSTUDY,`Collection Agency`,DATE,SITE,SUBSITE,PARAMETER) %>%
      filter(!is.na(PARAMETER)) %>%
      summarise(n=n(),RSD=sd(VALUE)/mean(VALUE)) %>%
      group_by(SUBSTUDY,`Collection Agency`,PARAMETER) %>%
      summarise(`Replicate Samples`=sum(n),`RSD`=percent(mean(RSD)))
    return(df1)
  }
  
  if(nrow(Find_FD_ERDP(df))<1 & nrow(Find_RS_ERDP(df))<1) 
  {
    df1 <- df%>%
      mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD",TRUE~"")) %>%
      group_by(SUBSTUDY,`Collection Agency`,PARAMETER) %>%
      summarise(`Replicate Samples`=0,RSD=NA)  
    return(df1)  
  }
}

ERDP_Precision  <- function(df)
{
  
  if(nrow(Find_FD_ERDP(df))<1 & nrow(Find_RS_ERDP(df))<1) 
  {   
    df1 <- df%>%
      mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD",TRUE~"")) %>%
      group_by(SUBSTUDY,`Collection Agency`) %>%
      summarise(`Number of Parameters`=0,`Average Parameter RSD`=NA)  
    return(df1)
  }  
  
  df1 <- ERDP_Precision_Parameters(df) %>%
    mutate(`RSD`=as.numeric(gsub("%", "",`RSD`))/100)  %>%
    group_by(SUBSTUDY,`Collection Agency`)  %>%
    summarise(`Number of Parameters`=n(),`Average Parameter RSD`=percent(mean(`RSD`,na.rm=TRUE)))
  return(df1)
}


ERDP_Accuracy_Parameters  <- function(df) 
{
  df1 <- df %>%
    mutate(DATE=as.Date(SDATE)) %>%
    mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD",TRUE~"")) %>%
    filter(FLDMETHOD == "GRAB" &  !is.na(PARAMETER)) %>%
    left_join(Find_EB_ERDP(df) ,by=c("Collection Agency","DATAFILE","DATE","PARAMETER")) %>%
    mutate(`Greater than MDL`=if_else(is.na(FQC) & EB > MDL ,1,0)) %>%
    mutate(`Blank within 10x sample`=if_else(is.na(FQC) & `RESULT` <= abs(EB)*10 ,1,0)) %>%
    mutate(`Blank Hits`=if_else( `Blank within 10x sample`+`Greater than MDL`==2 ,1,0)) %>%
    group_by(SUBSTUDY,`Collection Agency`,PARAMETER) %>%
    summarise(`Samples`=sum(if_else(is.na(FQC) ,1,0),na.rm=TRUE),
              `Blanks`=sum(if_else(FQC=="EB" ,1,0),na.rm=TRUE),
              `Blank > MDL`=sum(`Greater than MDL`,na.rm=TRUE),
              `Value < Blank 10x`=sum(`Blank within 10x sample`,na.rm=TRUE),
              `Blank Hits`=sum(`Blank Hits`,na.rm=TRUE),
              `% Blank Hits`=percent(`Blank Hits`/`Samples`))
  return(df1)
}

ERDP_Accuracy  <- function(df)
{
  df1 <- ERDP_Accuracy_Parameters(df) %>%
    mutate(`% Blank Hits`=as.numeric(gsub("%", "",`% Blank Hits`))/100)  %>%
    group_by(SUBSTUDY,`Collection Agency`)  %>%
    summarise(`Total Samples`=sum(Samples),`Total Blank Hits`=sum(`Blank Hits`),`Total Blank Hits (%)`=percent(mean(`% Blank Hits`,na.rm=TRUE)))
  return(df1)
}


# ERDP Autosamplers Functions ---------------------------------------------

ERDP_Completeness_Autosampler_Parameters  <- function(df)
{
  if (nrow(filter(df,FLDMETHOD == "ACT" | FLDMETHOD == "ADT" | str_detect(FLDMETHOD,"AUTO") ))<1)  
  { 
    df1 <- df  %>%
      mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD", TRUE~"")) %>%
      mutate(PARAMETER="") %>%
      group_by(SUBSTUDY,`Collection Agency`,PARAMETER) %>%
      summarise(Autosamples=0,FCEB=0,FD=0,FB=0,RS=0,EB=0,`Total Blanks`=0,`Blank %`=percent(0))
    return(df1)
  } 
  if (nrow(filter(df,FLDMETHOD == "ACT" | FLDMETHOD == "ADT" | str_detect(FLDMETHOD,"AUTO") ))>=1)  
  {  
    df1 <- df  %>%
      filter(FLDMETHOD == "ACT" | FLDMETHOD == "ADT" | str_detect(FLDMETHOD,"AUTO") ) %>%                                       #Remove Field Parameters
      mutate(DATE=as.Date(SDATE)) %>%
      mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD", TRUE~"")) %>%
      filter(!is.na(PARAMETER)) %>%
      group_by(SUBSTUDY,`Collection Agency`,PARAMETER) %>%
      summarise(Autosamples=sum(if_else(str_detect(FLDMETHOD,"AUTO") |FLDMETHOD == "ACT" | FLDMETHOD == "ADT",1,0),na.rm=TRUE),FCEB=sum(if_else(FQC=="FCEB",1,0),na.rm=TRUE),FD=sum(if_else(FQC=="FD",1,0),na.rm=TRUE),FB=sum(if_else(FQC=="FB",1,0),na.rm=TRUE),RS=sum(if_else(FQC=="RS",1,0),na.rm=TRUE),EB=sum(if_else(FQC=="EB",1,0),na.rm=TRUE),`Total Blanks`=FD+EB,`Blank %`=percent(`Total Blanks`/ifelse(`Autosamples`>0,`Autosamples`,0)))
    return(df1)
  }
}

ERDP_Completeness_Autosampler  <- function(df)
{
  if (nrow(filter(df,FLDMETHOD == "ACT" | FLDMETHOD == "ADT" | str_detect(FLDMETHOD,"AUTO") ))<1)  
  { 
    df1 <- df  %>%
      mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD", TRUE~"")) %>%
      group_by(SUBSTUDY,`Collection Agency`) %>%
      summarise(Autosamples=0,FCEB=0,FD=0,FB=0,RS=0,EB=0,`Total Blanks`=0,`Blank %`=percent(0))
    return(df1)
  }  
  if (nrow(filter(df,FLDMETHOD == "ACT" | FLDMETHOD == "ADT" | str_detect(FLDMETHOD,"AUTO") ))>=1)  
  {   
    df1 <- df  %>%
      filter(FLDMETHOD == "ACT" | FLDMETHOD == "ADT" | str_detect(FLDMETHOD,"AUTO") ) %>%                                       #Remove Field Parameters
      mutate(DATE=as.Date(SDATE)) %>%
      mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD", TRUE~"")) %>%
      filter(!is.na(PARAMETER)) %>%
      group_by(SUBSTUDY,`Collection Agency`) %>%
      summarise(Autosamples=sum(if_else(str_detect(FLDMETHOD,"AUTO") |FLDMETHOD == "ACT" | FLDMETHOD == "ADT",1,0),na.rm=TRUE),FCEB=sum(if_else(FQC=="FCEB",1,0),na.rm=TRUE),FD=sum(if_else(FQC=="FD",1,0),na.rm=TRUE),FB=sum(if_else(FQC=="FB",1,0),na.rm=TRUE),RS=sum(if_else(FQC=="RS",1,0),na.rm=TRUE),EB=sum(if_else(FQC=="EB",1,0),na.rm=TRUE),`Total Blanks`=FD+EB,`Blank %`=percent(`Total Blanks`/ifelse(`Autosamples`>0,`Autosamples`,0)))
    return(df1)
  }
}

ERDP_Accuracy_Autosampler_Parameters  <- function(df) 
{
  df1 <- df %>%
    mutate(DATE=as.Date(SDATE)) %>%
    mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD",TRUE~"")) %>%
    filter(FLDMETHOD == "ACT" | FLDMETHOD == "ADT" | str_detect(FLDMETHOD,"AUTO") ) %>%
    left_join(Find_EB_Autosampler_ERDP(df) ,by=c("Collection Agency","DATAFILE","DATE","PARAMETER")) %>%
    mutate(`Greater than MDL`=if_else(is.na(FQC) & EB > MDL ,1,0)) %>%
    mutate(`Blank within 10x sample`=if_else(is.na(FQC) & `RESULT` <= abs(EB)*10 ,1,0)) %>%
    mutate(`Blank Hits`=if_else( `Blank within 10x sample`+`Greater than MDL`==2 ,1,0)) %>%
    group_by(SUBSTUDY,`Collection Agency`,PARAMETER) %>%
    summarise(`Samples`=sum(if_else(is.na(FQC) ,1,0),na.rm=TRUE),
              `Blanks`=sum(if_else(FQC=="EB" ,1,0),na.rm=TRUE),
              `Blank > MDL`=sum(`Greater than MDL`,na.rm=TRUE),
              `Value < Blank 10x`=sum(`Blank within 10x sample`,na.rm=TRUE),
              `Blank Hits`=sum(`Blank Hits`,na.rm=TRUE),
              `% Blank Hits`=percent(`Blank Hits`/`Samples`))
  return(df1)
}

ERDP_Autosampler_Accuracy  <- function(df)
{
  df1 <- ERDP_Accuracy_Autosampler_Parameters(df) %>%
    mutate(`% Blank Hits`=as.numeric(gsub("%", "",`% Blank Hits`))/100)  %>%
    group_by(SUBSTUDY,`Collection Agency`)  %>%
    summarise(`Total Samples`=sum(Samples),`Total Blank Hits`=sum(`Blank Hits`),`Total Blank Hits (%)`=percent(mean(`% Blank Hits`,na.rm=TRUE)))
  return(df1)
}

Find_EB_Autosampler_ERDP <-function(df)
{  
  df1 <-df  %>%
    filter(FLDMETHOD == "ACT" | FLDMETHOD == "ADT" | str_detect(FLDMETHOD,"AUTO") ) %>%
    filter(FQC=="EB")   %>%   #EB Samples Only
    mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD",TRUE~"")) %>%
    mutate(DATE=as.Date(SDATE)) %>%
    filter(!is.na(PARAMETER)) %>%
    select(`Collection Agency`,DATAFILE,DATE,SITE,PARAMETER,SUBSITE,RESULT) %>%
    rename(EB="RESULT")
  return(df1)
}


# DBHYDRO Autosampler functions -------------------------------------------

#completeness 

DBHYDRO_Completeness_Parameters_Autosampler  <- function(df)
{
  df1 <- df  %>%
    mutate(P_num=str_sub(SAMPLE_ID,start=1,end=str_locate(SAMPLE_ID,"-")[,1]-1)) %>%    #create column of P# without dash numbers 
    rename(SUBSTUDY="PROJECT_CODE",`Collection Agency`="COLLECTION_AGENCY",PARAMETER="TEST_NAME") %>%
    group_by(SUBSTUDY,`Collection Agency`,P_num,PARAMETER) %>%
    summarise(`Autosamples`=sum(if_else(SAMPLE_TYPE_NEW=="SAMP"& str_detect(COLLECT_METHOD,"A"),1,0)),FCEB=sum(if_else(SAMPLE_TYPE_NEW=="FCEB",1,0)),FD=sum(if_else(SAMPLE_TYPE_NEW=="FD",1,0)),FB=sum(if_else(SAMPLE_TYPE_NEW=="FB",1,0)),RS=sum(if_else(SAMPLE_TYPE_NEW=="RS",1,0)),EB=sum(if_else(SAMPLE_TYPE_NEW=="EB",1,0)),`Total Blanks`=FCEB+FB+EB) %>%
    filter(`Autosamples`>=1) %>%
    group_by(SUBSTUDY,`Collection Agency`,PARAMETER) %>%  
    summarise(`Autosamples`=sum(`Autosamples`),FCEB=sum(FCEB),FD=sum(FD),FB=sum(FB),RS=sum(RS),EB=sum(EB),`Total Blanks`=FCEB+FB+EB,`Blank %`=percent(`Total Blanks`/`Autosamples`))
  return(df1)
}


DBHYDRO_Completeness_Autosampler  <- function(df)
{
  df1 <- df  %>%
    mutate(P_num=str_sub(SAMPLE_ID,start=1,end=str_locate(SAMPLE_ID,"-")[,1]-1)) %>%    #create column of P# without dash numbers 
    rename(SUBSTUDY="PROJECT_CODE",`Collection Agency`="COLLECTION_AGENCY") %>%
    group_by(SUBSTUDY,`Collection Agency`,P_num,TEST_NAME) %>%
    summarise(`Autosamples`=sum(if_else(SAMPLE_TYPE_NEW=="SAMP"& str_detect(COLLECT_METHOD,"A"),1,0)),FCEB=sum(if_else(SAMPLE_TYPE_NEW=="FCEB",1,0)),FD=sum(if_else(SAMPLE_TYPE_NEW=="FD",1,0)),FB=sum(if_else(SAMPLE_TYPE_NEW=="FB",1,0)),RS=sum(if_else(SAMPLE_TYPE_NEW=="RS",1,0)),EB=sum(if_else(SAMPLE_TYPE_NEW=="EB",1,0)),`Total Blanks`=FCEB+FB+EB) %>%
    filter(`Autosamples`>=1) %>%
    group_by(SUBSTUDY,`Collection Agency`) %>%  
    summarise(`Autosamples`=sum(`Autosamples`),FCEB=sum(FCEB),FD=sum(FD),FB=sum(FB),RS=sum(RS),EB=sum(EB),`Total Blanks`=FCEB+FB+EB,`Blank %`=percent(`Total Blanks`/`Autosamples`))
  return(df1)
}


#Accuracy
DBHYDRO_Completeness_Parameters_Autosampler(L8FEBOG_DBHYDRO_SAMPLE_WITH_QC_VIEW)
Find_Pnum_w_FCEB(L8FEBOG_DBHYDRO_SAMPLE_WITH_QC_VIEW)
Find_Autosampler_Blanks(L8FEBOG_DBHYDRO_SAMPLE_WITH_QC_VIEW)
df<- L8FEBOG_DBHYDRO_SAMPLE_WITH_QC_VIEW


DBHYDRO_Accuracy_Autosampler_Parameters <-function(df)
{  
  df1 <-df  %>%
    filter(SOURCE != "FLD") %>%                                                                                    #remove field data
    mutate(P_num=str_sub(SAMPLE_ID,start=1,end=str_locate(SAMPLE_ID,"-")[,1]-1)) %>%                               #create column of P# without dash numbers
    mutate(DATE=as.Date(DATE_COLLECTED)) %>%                                                                       #Change to  ERDP format
    rename(SUBSTUDY="PROJECT_CODE",`Collection Agency`="COLLECTION_AGENCY",PARAMETER="TEST_NAME") %>%
    left_join(Find_Autosampler_Blanks(df), by=c("P_num","PARAMETER")) %>%                                          #join Blanks
    filter(!is.na(BLANK)) %>%                                                                                      # keep only samples with associated blanks
    mutate(`Greater than MDL`=ifelse(SAMPLE_TYPE_NEW=="SAMP",if_else(`VALUE` > MDL ,1,0),0)) %>%
    mutate(`Blank within 10x sample`=ifelse(SAMPLE_TYPE_NEW=="SAMP",if_else(`VALUE` <= abs(BLANK)*10,1,0),0)) %>%
    mutate(`Blank Hits`=if_else( `Blank within 10x sample`+`Greater than MDL`==2 ,1,0)) %>%
    group_by(SUBSTUDY,`Collection Agency`,PARAMETER) %>%
    summarise(`Samples`=sum(if_else(SAMPLE_TYPE_NEW=="SAMP" ,1,0),na.rm=TRUE),             
              `Blanks`=sum(if_else(SAMPLE_TYPE_NEW=="FCEB" | SAMPLE_TYPE_NEW=="FB" ,1,0),na.rm=TRUE),
              `Blank > MDL`=sum(`Greater than MDL`),
              `Value < Blank 10x`=sum(`Blank within 10x sample`,na.rm=TRUE),
              `Blank Hits`=sum(`Blank Hits`,na.rm=TRUE),
              `% Blank Hits`=percent(`Blank Hits`/`Samples`))  
  return(df1)
}  

DBHYDRO_Autosampler_Accuracy <-function(df)
{  
  df1 <-df  %>%
    DBHYDRO_Accuracy_Autosampler_Parameters() %>%
    group_by(SUBSTUDY,`Collection Agency`) %>%
    summarise(`Total Samples`=sum(Samples),
              `Total Blank Hits`=sum(`Blank Hits`,na.rm=TRUE),
              `Total Blank Hits (%)`=percent(`Total Blank Hits`/`Total Samples`))  
  return(df1)
}  


Find_Pnum_w_FCEB <-function(df) #Find P#s with autosampler FCEB 
{
  df1 <- df  %>%
    mutate(P_num=str_sub(SAMPLE_ID,start=1,end=str_locate(SAMPLE_ID,"-")[,1]-1)) %>%    #create column of P# without dash numbers 
    rename(SUBSTUDY="PROJECT_CODE",`Collection Agency`="COLLECTION_AGENCY",PARAMETER="TEST_NAME") %>%
    group_by(SUBSTUDY,`Collection Agency`,P_num,PARAMETER) %>%
    summarise(`Autosamples`=sum(if_else(SAMPLE_TYPE_NEW=="SAMP"& str_detect(COLLECT_METHOD,"A"),1,0)),FCEB=sum(if_else(SAMPLE_TYPE_NEW=="FCEB",1,0)),FB=sum(if_else(SAMPLE_TYPE_NEW=="FB",1,0))) %>%
    filter(`Autosamples`>=1,FCEB>=1 | FB>=1) %>%
    distinct(P_num)
  return(df1)
}


Find_Autosampler_Blanks <-function(df) #find Blanks
{  
  df1 <-df  %>%
    mutate(P_num=str_sub(SAMPLE_ID,start=1,end=str_locate(SAMPLE_ID,"-")[,1]-1)) %>%    #create column of P# without dash numbers 
    rename(SUBSTUDY="PROJECT_CODE",`Collection Agency`="COLLECTION_AGENCY",PARAMETER="TEST_NAME") %>%
    inner_join(Find_Pnum_w_FCEB(df), by=c("P_num","SUBSTUDY","Collection Agency")) %>%
    filter(SAMPLE_TYPE_NEW=="FCEB" | (SAMPLE_TYPE_NEW=="FB")) %>%
    select(P_num,PARAMETER,VALUE) %>%
    rename(BLANK=VALUE) 
  return(df1)
}






# ERDP Soil Functions -----------------------------------------------------

ERDP_Soil_Completeness_Parameters  <- function(df)
{
  df1 <- df  %>%
    filter(FLDMETHOD == "CORE(10.1cm)") %>%                                       #Core 10cm method 
    mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD", TRUE~"")) %>%
    filter(!is.na(PARAMETER)) %>%
    #mutate(SUBTYPE=if_else(is.na(SUBTYPE),"SOIL",as.character(SUBTYPE))) %>%    #if subtype is missing then assumes it is soil
    group_by(SUBSTUDY,`Collection Agency`,PARAMETER,SUBTYPE) %>%
    summarise(Samples=sum(if_else(is.na(FQC),1,0)),FD=sum(if_else(FQC=="FD",1,0),na.rm=TRUE),RS=sum(if_else(FQC=="RS",1,0),na.rm=TRUE),`QC Samples`=FD+RS, `QC %`=percent(`QC Samples`/Samples))
  return(df1)
}

ERDP_Soil_Completeness  <- function(df)
{
  df1 <- df  %>%
    filter(FLDMETHOD == "CORE(10.1cm)") %>%                                       #Core 10cm method 
    mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD", TRUE~"")) %>%
    filter(!is.na(PARAMETER)) %>%
    #mutate(SUBTYPE=if_else(is.na(SUBTYPE),"SOIL",as.character(SUBTYPE))) %>%    #if subtype is missing then assumes it is soil
    group_by(SUBSTUDY,`Collection Agency`,SUBTYPE) %>%
    summarise(Samples=sum(if_else(is.na(FQC),1,0)),FD=sum(if_else(FQC=="FD",1,0),na.rm=TRUE),RS=sum(if_else(FQC=="RS",1,0),na.rm=TRUE),`QC Samples`=FD+RS, `QC %`=percent(`QC Samples`/Samples))
  return(df1)
}

ERDP_Precision_Soil_Parameters  <- function(df)
{
  if(nrow(Find_FD_Soils_ERDP(df))>=1) 
  {
    df1 <- df %>%
      mutate(DATE=as.Date(SDATE)) %>%
      filter(is.na(FQC)) %>%  # remove reps as to not to join rep to self
      filter(!is.na(PARAMETER)) %>%
      mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD",TRUE~"")) %>%
      left_join(Find_FD_Soils_ERDP(df) ,by=c("DATE","SITE","PARAMETER","SUBSITE","SUBTYPE","Collection Agency","HABITAT")) %>%
      filter(!is.na(FD)) %>%
      gather("Replicate type","VALUE",FD,RESULT) %>%
      select(SUBSTUDY,`Collection Agency`,DATE,SITE,SUBSITE,SUBTYPE,HABITAT,PARAMETER,VALUE,`Replicate type`)  %>%
      group_by(SUBSTUDY,`Collection Agency`,DATE,SITE,SUBSITE,SUBTYPE,HABITAT,PARAMETER) %>%
      filter(!is.na(PARAMETER)) %>%
      summarise(n=n(),RSD=sd(VALUE)/mean(VALUE)) %>%
      group_by(SUBSTUDY,`Collection Agency`,SUBTYPE,HABITAT,PARAMETER) %>%
      summarise(`Replicate or Duplicate Samples`=sum(n),`RSD`=percent(mean(RSD)))
    return(df1)
  }
}

ERDP_Precision_Soil  <- function(df)
{
  if(nrow(Find_FD_Soils_ERDP(df))>=1) 
  {
    df1 <- df %>%
      mutate(DATE=as.Date(SDATE)) %>%
      filter(is.na(FQC)) %>%  # remove reps as to not to join rep to self
      filter(!is.na(PARAMETER)) %>%
      mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD",TRUE~"")) %>%
      left_join(Find_FD_Soils_ERDP(df) ,by=c("DATE","SITE","PARAMETER","SUBSITE","SUBTYPE","Collection Agency","HABITAT")) %>%
      filter(!is.na(FD)) %>%
      gather("Replicate type","VALUE",FD,RESULT) %>%
      select(SUBSTUDY,`Collection Agency`,DATE,SITE,SUBSITE,SUBTYPE,HABITAT,PARAMETER,VALUE,`Replicate type`)  %>%
      group_by(SUBSTUDY,`Collection Agency`,DATE,SITE,SUBSITE,SUBTYPE,HABITAT,PARAMETER) %>%
      filter(!is.na(PARAMETER)) %>%
      summarise(n=n(),RSD=sd(VALUE)/mean(VALUE)) %>%
      group_by(SUBSTUDY,`Collection Agency`,SUBTYPE,HABITAT) %>%
      summarise(`Replicate or Duplicate Samples`=sum(n),`Average Parameter RSD`=percent(mean(RSD)))
    return(df1)
  }
}

Find_FD_Soils_ERDP  <- function(df) 
{
  df1 <- df %>%
    mutate(DATE=as.Date(SDATE)) %>%
    mutate(`Collection Agency`=case_when(str_detect(FLDCOMMENT,"DBE") ~"DBE",str_detect(FLDCOMMENT,"WMD") ~"WMD", TRUE~"")) %>%
    filter(FQC=="FD") %>%  # find FDs
    filter(!is.na(PARAMETER)) %>%
    select(DATE,SITE,PARAMETER,SUBSITE,SUBTYPE,`Collection Agency`,HABITAT,RESULT) %>%
    rename(FD="RESULT")
  return(df1)
}

ERDP_Time_Filter <-function(df)
{  
  df1 <-df  %>%
filter(ymd_hms(SDATE) <Latest_Collection_date,ymd_hms(SDATE)>=Earliest_Collection_date)
  return(df1)
}  

DBHYDRO_Time_Filter <-function(df)
{  
  df1 <-df  %>%
  filter(ymd_hms(DATE_COLLECTED) <Latest_Collection_date,ymd_hms(DATE_COLLECTED)>=Earliest_Collection_date)
  return(df1)
}  

