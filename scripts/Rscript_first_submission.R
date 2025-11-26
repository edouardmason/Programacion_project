############################################################
# Script name:   First Submission
# Author:        Edouard Mason, Samuel Mendoza Gramberg
# Date:          2025-10-23
# Description:   Programming a dashboard that visualizes 
#                Spain's historical evolution of key 
#                macroeconomic indicators.
############################################################
#loading raw data in R
Exports<-read.csv("https://raw.githubusercontent.com/edouardmason/Programacion_project/refs/heads/main/data/Exports.csv")
Imports<-read.csv("https://raw.githubusercontent.com/edouardmason/Programacion_project/refs/heads/main/data/Imports.csv")
Industrial_Production<-read.csv("https://raw.githubusercontent.com/edouardmason/Programacion_project/refs/heads/main/data/Industrial_Production.csv")
International_Visitors<-read.csv("https://raw.githubusercontent.com/edouardmason/Programacion_project/refs/heads/main/data/International_visitors.csv",sep=";")
Real_GDP <- read.csv("https://raw.githubusercontent.com/edouardmason/Programacion_project/refs/heads/main/data/rgdp_spain.csv")
CPI <- read.csv("https://raw.githubusercontent.com/edouardmason/Programacion_project/refs/heads/main/data/cpi.csv")
unemp_df <- read.csv("https://raw.githubusercontent.com/edouardmason/Programacion_project/refs/heads/main/data/unemprate.csv")


#We start to clean data by erasing obsolete columns
International_Visitors <- International_Visitors[c("Period", "Total")]
remove_cols_unemp <- c("Observation.value", "OBS_FLAG", "freq", "Confidentiality.status..flag.", "unit", "STRUCTURE", "Time.frequency", "s_adj", "indic", "STRUCTURE_ID", "Time", "CONF_STATUS", "geo", "Indicator", "Seasonal.adjustment","Observation.status..Flag..V2.structure", "Geopolitical.entity..reporting.", "STRUCTURE_NAME", "Unit.of.measure")
unemp_clean <- unemp_df[, !(names(unemp_df) %in% remove_cols_unemp)]
#We remove previous unemployment dataframe
rm(unemp_df)


#fix decimals 
International_Visitors$Total <- gsub(",", "", International_Visitors$Total)
International_Visitors$Total <- as.numeric(International_Visitors$Total)

#install and activate the Package "tidyverse"
install.packages("tidyverse")
library(tidyverse)

library(dplyr)
#
#We rename the columns for clarity
Industrial_Production<-rename(Industrial_Production,Value=ESPPROINDMISMEI)
Exports<-rename(Exports,Exports=XTEXVA01ESQ664S)
Imports<-rename(Imports, Imports=XTIMVA01ESQ664S)
#We adjust monthly data to quarterly by averaging

#Industrial Production

#We confirm text into a time object
full_date <- as.Date(Industrial_Production$observation_date)   
#We pull out the year of the full date variable
year_val <- format(full_date, "%Y")    
#We use the quarters() function to determine what quarter each date belongs to
quarter_val <- quarters(full_date)    
#We paste the year and quarter together for each date and create a new column named "quarter" in the Industrial Production dataframe.  
Industrial_Production$quarter <- paste(year_val, quarter_val, sep = "-")     
#We compute the average value of Industrial Production for each quarter and create a new dataframe named Quarterly_IP
Quarterly_IP <- aggregate(Value ~ quarter, data = Industrial_Production, FUN = mean)

#We rename the columns for clarity
Quarterly_IP <-rename(Quarterly_IP, IProduction=Value)
Quarterly_IP <-rename(Quarterly_IP, Date=quarter)

#We remove the original Industrial_Production dataframe we no longer need
rm(Industrial_Production)

#Unemployment rate

#We convert text into a time object. We add "-01" at the end of the TIME_PERIOD column to allow R to read the date properly
full_date <- as.Date(paste0(unemp_clean$TIME_PERIOD, "-01"))
#We pull out the year of the full date variable
year_val <- format(full_date, "%Y")
#We use the quarters() function to determine what quarter each date belongs to
quarter_val <- quarters(full_date)
#We paste the year and quarter together for each date and create a new column named "quarter" in the unemp_clean dataframe.  
unemp_clean$quarter <- paste(year_val, quarter_val, sep = "-")
#We compute the average value of Industrial Production for each quarter and create a new dataframe named Quarterly_Unemp
Quarterly_Unemp <- aggregate(OBS_VALUE ~ quarter, data = unemp_clean, FUN = mean)

#We rename the columns for clarity
Quarterly_Unemp <-rename(Quarterly_Unemp, Unemployment=OBS_VALUE)
Quarterly_Unemp <-rename(Quarterly_Unemp, Date=quarter)

#We remove the original Industrial_Production dataframe we no longer need
rm(unemp_clean)

library(ggplot2)
library(scales)

# We convert the observation_date column to date format and values to numeric
Exports$observation_date <- as.Date(Exports$observation_date)
Exports$Exports <- as.numeric(Exports$Exports)

#Line chart Exports 
ggplot(Exports, aes(x=observation_date, y=Exports/1e9))+
  geom_line(color="blue")+
  scale_y_continuous(n.breaks = 8)+
  geom_point(size=1.5)+
  labs(
    title = "Quarterly Exports of Spain from 2015 to 2025",
    x = "Year",
    y = "Exports (Billion €)"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5)
  )

# We convert the observation_date column to date format and values to numeric
Imports$observation_date <- as.Date(Imports$observation_date)
Imports$Imports <- as.numeric(Imports$Imports)

#Line chart Imports
ggplot(Imports, aes(x=observation_date, y=Imports/1e9))+
  geom_line(color="blue")+
  scale_y_continuous(n.breaks=8) +
  geom_point(size=1.5)+
  labs(
    title = "Quarterly Imports of Spain from 2015 to 2025",
    x = "Year",
    y = "Imports (billion €)"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5)
  )

# We convert the observation_date column to date format
Real_GDP$observation_date <- as.Date(Real_GDP$observation_date)

#Line chart Real GDP
ggplot(Real_GDP, aes(x=observation_date, y=RealGDP))+
  geom_line(color="blue")+
  scale_y_continuous(n.breaks=6)+
  geom_point(size=1.5)+
  labs(
    title = "Quarterly Real GDP of Spain from 2015 to 2025",
    x = "Year",
    y = "Real GDP (million €)"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5)
  )

# We convert the observation_date column to date format
CPI$observation_date <- as.Date(CPI$observation_date)

#Line chart CPI
ggplot(CPI, aes(x=observation_date, y=CPI))+
  geom_line(color="blue")+
  scale_y_continuous(n.breaks=6)+
  geom_point(size=1.5)+
  labs(
    title = "Quarterly Consumer Price Index of Spain from 2015 to 2025",
    x = "Year",
    y = "Harmonized Index of Consumer Prices (%)"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5)
  )

#Merging quarterly variables into a single dataframe
merged_df <- Reduce(function(x,y) merge(x,y, by = "observation_date"),
                    list(CPI, Exports, Imports, Real_GDP))
#Summary statistics
summary(merged_df)

library(reshape2)
quarterly_var_corr <- cor(merged_df[,2:5])
long_quarterly_var <- melt(quarterly_var_corr)

ggplot(long_quarterly_var, aes(x=Var1, y=Var2, fill=value))+
  geom_tile()+
  scale_fill_gradient(
    high="blue",
    low="white"
  )+
  labs(
    title="Correlation heatmap",
    x = "",
    y = ""
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5)
  )

#Histograms

#Imports
ggplot(Imports, aes(x = Imports/1e9)) +
  geom_histogram(
    bins = 15,
    fill = "skyblue",
    color = "black"
  ) +
  labs(
    title = "Distribution of Quarterly Imports of Spain",
    x = "Imports (Billion €)",
    y = "Frequency"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5)
  )

#Exports
ggplot(Exports, aes(x = Exports/1e9)) +
  geom_histogram(
    bins = 10,
    fill = "skyblue",
    color = "black"
  ) +
  labs(
    title = "Distribution of Quarterly Exports of Spain",
    x = "Exports (Billion €)",
    y = "Frequency"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5)
  )

#Real GDP
ggplot(Real_GDP, aes(x = RealGDP)) +
  geom_histogram(
    bins = 15,
    fill = "skyblue",
    color = "black"
  ) +
  labs(
    title = "Distribution of Quarterly Real GDP of Spain",
    x = "Real GDP (Billion €)",
    y = "Frequency"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5)
  )

#CPI
ggplot(CPI, aes(x = CPI)) +
  geom_histogram(
    bins = 15,
    fill = "skyblue",
    color = "black"
  ) +
  labs(
    title = "Distribution of Quarterly Consumer Price Index of Spain",
    x = "Consumer Price Index (%)",
    y = "Frequency"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5)
  )

#Box plots

#CPI
ggplot(merged_df, aes(y = CPI)) +
  geom_boxplot(fill="blue", color="black") +
  labs(
    title = "Box plot of Quarterly Consumer Price Index of Spain",
    y = "Consumer Price Index (%)"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5)
  )

#Imports
ggplot(merged_df, aes(y = Imports/1e9)) +
  geom_boxplot(fill="blue", color="black") +
  scale_y_continuous(n.breaks=6)+
  labs(
    title = "Box plot of Quarterly Imports of Spain",
    y = "Imports (Billion €)"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5)
  )

#Exports
ggplot(merged_df, aes(y = Exports/1e9)) +
  geom_boxplot(fill="blue", color="black") +
  scale_y_continuous(n.breaks=6)+
  labs(
    title = "Box plot of Quarterly Exports of Spain",
    y = "Exports (Billion €)"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5)
  )

#Real GDP
ggplot(merged_df, aes(y = RealGDP)) +
  geom_boxplot(fill="blue", color="black") +
  scale_y_continuous(n.breaks=6)+
  labs(
    title = "Box plot of Quarterly Real GDP of Spain",
    y = "Real GDP (Billion €)"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5)
  )
