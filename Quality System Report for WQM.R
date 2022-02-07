#This script is created to analyze water quality bureau data for the quality systems annual report 


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

# Import data -------------------------------------------------------------

DBHYDRO_data_2021 <- read_excel("Data/DBHYDRO data 2021.xlsx")


# Screen data for report time period --------------------------------------

Latest_Collection_date <-"2022-01-01 00:00:00"         #latest date of collected data
Earliest_Collection_date <- "2021-01-01 00:00:00"      #Earliest date of collected data 

DBHYDRO_data_2021 <-DBHYDRO_data_2021 %>%
mutate(DATE_COLLECTED=ymd_hms(DATE_COLLECTED)) %>%  
filter(DATE_COLLECTED<Latest_Collection_date) %>%
filter(DATE_COLLECTED>Earliest_Collection_date)  


# Summary Stats for DBHYDRO Data ------------------------------------------

#Distinct collection agencies
DBHYDRO_data_2021 %>%
group_by(COLLECTION_AGENCY) %>%
summarise(`Number of Records`=n(),`Percent of Total`=percent(n()/nrow(DBHYDRO_data_2021),accuracy = 0.1))

#Distinct collection Methods
DBHYDRO_data_2021 %>%
group_by(COLLECT_METHOD) %>%
summarise(`Number of Records`=n(),`Percent of Total`=percent(n()/nrow(DBHYDRO_data_2021),accuracy = 0.1))

#Distinct Validation Levels
DBHYDRO_data_2021 %>%
group_by(VALIDATION_LEVEL) %>%
summarise(`Number of Records`=n(),`Percent of Total`=percent(n()/nrow(DBHYDRO_data_2021),accuracy = 0.1))

#Distinct QC Flags
DBHYDRO_data_2021 %>%
group_by(FLAG) %>%
summarise(`Number of Records`=n(),`Percent of Total`=percent(n()/nrow(DBHYDRO_data_2021),accuracy = 0.1))

#Distinct QC Flags by parameter and method
DBHYDRO_data_2021 %>%
group_by(TEST_NAME,COLLECT_METHOD,FLAG) %>%
summarise(`Number of Records`=n(),`Percent of Total`=percent(n()/nrow(DBHYDRO_data_2021),accuracy = 0.1)) 

#Distinct Remark Codes
DBHYDRO_data_2021 %>%
group_by(REMARK_CODE) %>%
summarise(`Number of Records`=n(),`Percent of Total`=percent(n()/nrow(DBHYDRO_data_2021),accuracy = 0.01))

#DQO completeness
Completeness_grabs<-Completeness(DBHYDRO_data_2021)
Completeness_grabs_parameters<-Completeness_Parameters(DBHYDRO_data_2021)

#DQO Precision by paramter and project code
Precision_Parameters <-DBHYDRO_data_2021 %>%
filter(COLLECT_METHOD=="G") %>%
filter(SOURCE != "FLD") %>%
mutate(DATE=as.Date(DATE_COLLECTED)) %>%
filter(SAMPLE_TYPE_NEW=="SAMP")  %>%
left_join(Find_RS(DBHYDRO_data_2021),by=c("DATE","STATION_ID","TEST_NAME"))  %>%
filter(!is.na(Rep1)) %>%
select(PROJECT_CODE,COLLECTION_AGENCY,TEST_NAME,VALUE,Rep1,Rep2) %>%
#gather("Replicate type","VALUE",Rep1,Rep2,VALUE) %>%
rowwise() %>%  
mutate(SD=sd(c_across(4:6)),Mean=mean(c_across(4:5)),RSD=abs(SD/Mean))  %>%
group_by(PROJECT_CODE,COLLECTION_AGENCY,TEST_NAME)  %>%  
summarise(`Replicate Samples`=n(),`Mean RSD`=percent(mean(RSD,na.rm=TRUE),accuracy = 0.1))

#Precision by parameter
Precision <- Precision_Parameters %>%  
mutate(RSD=ifelse(str_detect(`Mean RSD`,"%"),as.numeric(gsub("%", "",`Mean RSD`)),NA))  %>%
group_by(TEST_NAME)  %>%  
summarise(`Average Parameter RSD`=percent(mean(RSD,na.rm = TRUE)/100))

#Accuracy by parameter and project code
Accuracy_Parameters <-DBHYDRO_data_2021  %>%
filter(SOURCE != "FLD",TEST_NAME!="NO BOTTLE SAMPLE") %>%           #remove field parameters
filter(COLLECT_METHOD=="G") %>%  
mutate(DATE=as.Date(DATE_COLLECTED)) %>%
mutate(P_num=str_sub(SAMPLE_ID,start=1,end=str_locate(SAMPLE_ID,"-")[,1]-1)) %>%    #create column of P# without dash numbers 
select(DATE,P_num,PROJECT_CODE,COLLECTION_AGENCY,SAMPLE_TYPE_NEW,STATION_ID,MDL,REMARK_CODE,TEST_NAME,VALUE)  %>%
left_join(Find_FCEB(DBHYDRO_data_2021), by=c("DATE","TEST_NAME","P_num"),keep=FALSE) %>%       #join FCEBs
left_join(Find_EB(DBHYDRO_data_2021), by=c("DATE","TEST_NAME","P_num"),keep=FALSE) %>%         #Join EBs
left_join(Find_FB(DBHYDRO_data_2021), by=c("DATE","TEST_NAME","P_num"),keep=FALSE) %>%         #Join FBs
mutate(`FCEB`=ifelse(is.na(`FCEB`),0,FCEB)) %>%
mutate(`EB`=ifelse(is.na(`EB`),0,EB)) %>%  
mutate(`FB`=ifelse(is.na(`FB`),0,FB)) %>%    
mutate(`FCEB greater than MDL`=ifelse(SAMPLE_TYPE_NEW=="SAMP",if_else(`FCEB` >= MDL ,1,0),0)) %>%  
mutate(`EB greater than MDL`=ifelse(SAMPLE_TYPE_NEW=="SAMP",if_else(`EB` >= MDL ,1,0),0)) %>%  
mutate(`FB greater than MDL`=ifelse(SAMPLE_TYPE_NEW=="SAMP",if_else(`FB` >= MDL ,1,0),0)) %>%    
mutate(`G or I Qualified`=ifelse(str_detect(REMARK_CODE,c("G","I")),1,0))  %>%
mutate(`FCEB within 10x sample`=ifelse(SAMPLE_TYPE_NEW=="SAMP",ifelse(is.finite(FCEB),ifelse(VALUE<=FCEB*10 & VALUE >0,1,0),0),0)) %>% 
mutate(`EB within 10x sample`=ifelse(SAMPLE_TYPE_NEW=="SAMP",ifelse(is.finite(EB),ifelse(VALUE<=EB*10 & VALUE >0,1,0),0),0)) %>%    
mutate(`FB within 10x sample`=ifelse(SAMPLE_TYPE_NEW=="SAMP",ifelse(is.finite(FB),ifelse(VALUE<=FB*10 & VALUE >0,1,0),0),0)) %>%      
mutate(`Blank Hits`=if_else( `FCEB within 10x sample`+`EB within 10x sample`+`FB within 10x sample`+`EB greater than MDL`+`FCEB greater than MDL`+`FB greater than MDL`>=2 ,1,0)) %>%
group_by(PROJECT_CODE,COLLECTION_AGENCY,TEST_NAME) %>%
summarise(`Samples`=sum(if_else(SAMPLE_TYPE_NEW=="SAMP" ,1,0),na.rm=TRUE), 
          `FCEB`=sum(if_else(SAMPLE_TYPE_NEW=="FCEB" ,1,0),na.rm=TRUE),
          `EB`=sum(if_else(SAMPLE_TYPE_NEW=="EB" ,1,0),na.rm=TRUE),
          `FB`=sum(if_else(SAMPLE_TYPE_NEW=="FB" ,1,0),na.rm=TRUE),
          `Blanks`=FCEB+EB+FB,
          #`Value < FCEB 10x`=sum(`FCEB within 10x sample`,na.rm=TRUE),
          #`Value < EB 10x`=sum(`EB within 10x sample`,na.rm=TRUE),
          #`Value < FB 10x`=sum(`FB within 10x sample`,na.rm=TRUE),
          `Blank Hits`=sum(`Blank Hits`,na.rm=TRUE),
          `% Blank Hits`=percent(`Blank Hits`/`Samples`,accuracy=0.1))  
#accuracy
Accuracy <- Accuracy_Parameters %>%
group_by(TEST_NAME) %>%
summarise(`Total Samples`=sum(Samples),
            `Total Blank Hits`=sum(`Blank Hits`,na.rm=TRUE),
            `Total Blank Hits (%)`=percent(`Total Blank Hits`/`Total Samples`,accuracy=0.1))  %>% na.omit %>% arrange(`Total Blank Hits`/`Total Samples`)


# DBHYDRO Grab Functions -------------------------------------------------------

Completeness  <- function(df)
{
  df1 <- df  %>%
    filter(SOURCE != "FLD") %>%                                        #Remove Field Paramters
    filter(COLLECT_METHOD=="G" ) %>%              # Grab Samples only
    mutate(Date=as.Date(DATE_COLLECTED)) %>%  
    group_by(`COLLECTION_AGENCY`,PROJECT_CODE) %>%
    summarise(Samples=sum(if_else(SAMPLE_TYPE_NEW=="SAMP",1,0)),FCEB=sum(if_else(SAMPLE_TYPE_NEW=="FCEB",1,0)),FD=sum(if_else(SAMPLE_TYPE_NEW=="FD",1,0)),FB=sum(if_else(SAMPLE_TYPE_NEW=="FB",1,0)),RS=sum(if_else(SAMPLE_TYPE_NEW=="RS",1,0)),EB=sum(if_else(SAMPLE_TYPE_NEW=="EB",1,0)),`Total Blanks`=FCEB+FB+EB, `Blank %`=percent(`Total Blanks`/Samples))
  return(df1)  
}

Completeness_Parameters  <- function(df)
{
  df1 <- df  %>%
    filter(SOURCE != "FLD") %>%                                        #Remove Field Paramters
    filter(COLLECT_METHOD=="G") %>%                                    # Grab Samples only
    mutate(Date=as.Date(DATE_COLLECTED)) %>%                                          #Collecting Agency unknown. How can this be discerned from DBHYDRO DB?
    group_by(`COLLECTION_AGENCY`,PROJECT_CODE,TEST_NAME) %>%  
    summarise(Samples=sum(if_else(SAMPLE_TYPE_NEW=="SAMP",1,0)),FCEB=sum(if_else(SAMPLE_TYPE_NEW=="FCEB",1,0)),FD=sum(if_else(SAMPLE_TYPE_NEW=="FD",1,0)),FB=sum(if_else(SAMPLE_TYPE_NEW=="FB",1,0)),RS=sum(if_else(SAMPLE_TYPE_NEW=="RS",1,0)),EB=sum(if_else(SAMPLE_TYPE_NEW=="EB",1,0)),`Total Blanks`=FCEB+FB+EB, `Blank %`=percent(`Total Blanks`/Samples))
  return(df1)  
}

#find Blanks
Find_FCEB <-function(df)
{  
  df1 <-df  %>%
    mutate(DATE=as.Date(DATE_COLLECTED)) %>%
    filter(SAMPLE_TYPE_NEW=="FCEB") %>%
    mutate(P_num=str_sub(SAMPLE_ID,start=1,end=str_locate(SAMPLE_ID,"-")[,1]-1)) %>%    #create column of P# without dash numbers 
    select(P_num,DATE,STATION_ID,TEST_NAME,VALUE) %>%
    rename(FCEB=VALUE) 
  return(df1)
}

Find_RS <-function(df)   #works for 2 reps, not 1
{  
  df1 <-df  %>%
      mutate(DATE=as.Date(DATE_COLLECTED)) %>%
      filter(SAMPLE_TYPE_NEW=="RS") %>%
      mutate(P_num=str_sub(SAMPLE_ID,start=1,end=str_locate(SAMPLE_ID,"-")[,1]-1)) %>%    #create column of P# without dash numbers  
      select(P_num,DATE,STATION_ID,TEST_NAME,VALUE) %>%
      group_by(P_num,DATE,STATION_ID,TEST_NAME) %>%
      summarise(n=n(),Rep1=max(VALUE,na.rm=TRUE),Rep2=min(VALUE,na.rm=TRUE))  %>%
      filter(n==2)
  return(df1)
}

Find_EB <-function(df)   #
{  
  df1 <-df  %>%
    mutate(DATE=as.Date(DATE_COLLECTED)) %>%
    filter(SAMPLE_TYPE_NEW=="EB") %>%
    mutate(P_num=str_sub(SAMPLE_ID,start=1,end=str_locate(SAMPLE_ID,"-")[,1]-1)) %>%    #create column of P# without dash numbers  
    select(P_num,SAMPLE_ID,DATE,STATION_ID,TEST_NAME,VALUE) %>%
    rename(EB=VALUE)
  return(df1)
}

Find_FB <-function(df)   #
{  
  df1 <-df  %>%
    mutate(DATE=as.Date(DATE_COLLECTED)) %>%
    filter(SAMPLE_TYPE_NEW=="FB") %>%
    mutate(P_num=str_sub(SAMPLE_ID,start=1,end=str_locate(SAMPLE_ID,"-")[,1]-1)) %>%    #create column of P# without dash numbers  
    select(P_num,SAMPLE_ID,DATE,STATION_ID,TEST_NAME,VALUE) %>%
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
