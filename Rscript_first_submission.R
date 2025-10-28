############################################################
# Script name:   First Submission
# Author:        Edouard Mason, Samuel Mendoza Gramberg
# Date:          2025-10-23
# Description:   Programming a dashboard that visualizes 
#                Spains historical evolution of key 
#                macroeconomic indicators.
############################################################
#loading raw data in R
Exports<-read.csv("https://raw.githubusercontent.com/edouardmason/Programacion_project/refs/heads/main/Exports.csv")
Imports<-read.csv("https://raw.githubusercontent.com/edouardmason/Programacion_project/refs/heads/main/Imports.csv")
Industrial_Production<-read.csv("https://raw.githubusercontent.com/edouardmason/Programacion_project/refs/heads/main/Industrial_Production.csv")
International_Visitors<-read.csv("https://raw.githubusercontent.com/edouardmason/Programacion_project/refs/heads/main/Tourists_Inflow.csv",sep="")
Visitors_Expenditure<-read.csv("https://raw.githubusercontent.com/edouardmason/Programacion_project/refs/heads/main/Tourist_Expenditure.csv",sep="")
#
#start to clean data by erasing obsolete columns
Visitors_Expenditure <- Visitors_Expenditure[c("data", "Expenses")]
International_Visitors <- International_Visitors[c("Access", "route")]
#
#fix decimals 
International_Visitors$route <- gsub(",", "", International_Visitors$route)
International_Visitors$route <- as.numeric(International_Visitors$route)

#install and activate the Package "tidyverse"
install.packages("tidyverse")
library(tidyverse)
#
#We rename the columns for clarity
Industrial_Production<-rename(Industrial_Production,Value=ESPPROINDMISMEI)
Exports<-rename(Exports,Value=XTEXVA01ESQ664S)
Imports<-rename(Imports, Value=XTIMVA01ESQ667S)
International_Visitors<-rename(International_Visitors,Value=route)
International_Visitors<-rename(International_Visitors, observation_date=Access)
Visitors_Expenditure<-rename(Visitors_Expenditure, Value= Expenses)
Visitors_Expenditure<-rename(Visitors_Expenditure, observation_date=data)

#Adjust monthly data to quarterly by averaging
full_date <- as.Date(Industrial_Production$observation_date)   #We confirm text into a time object
#We pull out the year out of the full date variable
year_val <- format(full_date, "%Y")    
#We use the quarters() function to determine what quarter each date belongs to
quarter_val <- quarters(full_date)    
#We paste the year and quarter together for each date and create a new column named "quarter" in the Indsutrial Production dataframe.  
Industrial_Production$quarter <- paste(year_val, quarter_val, sep = "-")     
#We compute the average value of Industrial Production for each quarter and create a new dataframe named Quarterly_IP
Quarterly_IP <- aggregate(Value ~ quarter, data = Industrial_Production, FUN = mean)

#We rename the columns for clarity
Quarterly_IP <-rename(Quarterly_IP, IProduction=Value)
Quarterly_IP <-rename(Quarterly_IP, Date=quarter)

#We remove the original Industrial_Production dataframe we no longer need
rm(Industrial_Production)


