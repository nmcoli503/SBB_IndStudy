#path: C:/Users/ninac/OneDrive - University of South Carolina/Documents/R Studio Documents/MSCI399/MSCI399_SBB_Bot_Boxplots.R

sbb_bot<-SBB_SedTrap_Bot_boxplots

library(ggplot2)
library(lubridate)
library(tidyverse)
library(carData)
library(gridBase)
library(gridExtra)
library(patchwork)
library (dplyr)

sbb_bot$Date_Open <-as.Date(sbb_bot$Date_Open)

#extracts all the months from the Date_Open and stores it in a new column called month
sbb_bot$month <- format(sbb_bot$Date_Open,"%m")

#extracts the year from the Date_Open column and stores it in a new column called year.
sbb_bot$year <- format(sbb_bot$Date_Open,"%Y")

#this line removes missing dates: if rows with missing dates aren't essential, you can remove them with filter (), 
#This code removes all rows from sbb_top where the Date_Open column has missing data
sbb_bot <- sbb_bot %>% filter(!is.na(Date_Open))

#I just wrote these next three lines to call the information so I could visualize it.
sbb_bot$Date_Open
sbb_bot$month
sbb_bot$year

#sbb_bot$year <- as.numeric(sbb_bot$year)
#sbb_bot$year_full <- ifelse(sbb_bot$year < 50, 
                          # paste0("20", sprintf("%02d", sbb_bot$year)), 
                          # paste0("19", sprintf("%02d", sbb_bot$year)))
#sbb_bot$year_full <- as.Date(sbb_bot$year_full, format = "%Y")

#################

massflux_monthlyavg_sbb_bot <- sbb_bot%>% 
  group_by(month) %>% 
  summarise (Avg_Total_Mass_Flux_sbb_bot = mean(na.omit(Total_Mass_Flux)))

terigflux_monthlyavg_sbb_bot <- sbb_bot%>% 
  group_by(month) %>% 
  summarise (Avg_Terrig_Flux_sbb_bot = mean(na.omit(Terrigenous_Flux)))

PONflux_monthlyavg_sbb_bot <- sbb_bot%>% 
  group_by(month) %>% 
  summarise (Avg_PON_Flux_sbb_bot = mean(na.omit(PON_Flux)))

POCflux_monthlyavg_sbb_bot <- sbb_bot%>% 
  group_by(month) %>% 
  summarise (Avg_POC_Flux_sbb_bot = mean(na.omit(POC_Flux)))

CaCO3flux_monthlyavg_sbb_bot <- sbb_bot%>% 
  group_by(month) %>% 
  summarise (Avg_CaCO3_Flux_sbb_bot = mean(na.omit(CaCO3_Flux)))

Opalflux_monthlyavg_sbb_bot <- sbb_bot%>% 
  group_by(month) %>% 
  summarise (Avg_Opal_Flux_sbb_bot = mean(na.omit(OPAL_Flux)))

##################

massflux_yearlyavg_sbb_bot <- sbb_bot%>% 
  group_by(year) %>% 
  summarise (Avg_Total_Mass_Flux_sbb_bot = mean(na.omit(Total_Mass_Flux)))

terigflux_yearlyavg_sbb_bot <- sbb_bot%>% 
  group_by(year) %>% 
  summarise (Avg_Terrig_Flux_sbb_bot = mean(na.omit(Terrigenous_Flux)))

PONflux_yearlyavg_sbb_bot <- sbb_bot%>% 
  group_by(year) %>% 
  summarise (Avg_PON_Flux_sbb_bot = mean(na.omit(PON_Flux)))

POCflux_yearlyavg_sbb_bot <- sbb_bot%>% 
  group_by(year) %>% 
  summarise (Avg_POC_Flux_sbb_bot = mean(na.omit(POC_Flux)))

CaCO3flux_yearlyavg_sbb_bot <- sbb_bot%>% 
  group_by(year) %>% 
  summarise (Avg_CaCO3_Flux_sbb_bot = mean(na.omit(CaCO3_Flux)))

Opalflux_yearlyavg_sbb_bot <- sbb_bot%>% 
  group_by(year) %>% 
  summarise (Avg_Opal_Flux_sbb_bot = mean(na.omit(OPAL_Flux)))

#################

plot_2a <- ggplot(sbb_bot, aes(x = month, y = Total_Mass_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "Total Mass Flux (g/m^2/day)") +
  ggtitle("SBB Bot: Monthly Average of Total Mass Flux")

plot_2b <- ggplot(sbb_bot, aes(x = month, y = Terrigenous_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "Terrigenous Flux (g/m^2/day)") +
  ggtitle("SBB Bot: Monthly Average of Terrigenous Flux")

plot_2c <- ggplot(sbb_bot, aes(x = month, y = PON_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "PON Flux (g/m^2/day)") +
  ggtitle("SBB Bot: Monthly Average of PON Flux")

plot_2d <- ggplot(sbb_bot, aes(x = month, y = POC_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "POC Flux (g/m^2/day)") +
  ggtitle("SBB Bot: Monthly Average of POC Flux")

plot_2e <- ggplot(sbb_bot, aes(x = month, y = CaCO3_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "CaCO3 Flux (g/m^2)/day") +
  ggtitle("SBB Bot: Monthly Average of CaCO3 Flux")

plot_2f <- ggplot(sbb_bot, aes(x = month, y = OPAL_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "Opal Flux (g/m^2/day)") +
  ggtitle("SBB Bot: Monthly Average of Opal Flux")

###################
plot_2g <- ggplot(sbb_bot, aes(x = year, y = Total_Mass_Flux))+
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "Total Mass Flux (g/m^2/day)") +
  ggtitle("SBB Bot: Yearly Average of Total Mass Flux") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

plot_2h <- ggplot(sbb_bot, aes(x = year, y = Terrigenous_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "Terrigenous Flux (g/m^2/day)") +
  ggtitle("SBB Bot: Yearly Average of Terrigenous Flux") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


plot_2i <- ggplot(sbb_bot, aes(x = year, y = PON_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "PON Flux (g/m^2/day)") +
  ggtitle("SBB Bot: Yearly Average of PON Flux") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

plot_2j <- ggplot(sbb_bot, aes(x = year, y = POC_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "POC Flux (g/m^2/day)") +
  ggtitle("SBB Bot: Yearly Average of POC Flux") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

plot_2k <- ggplot(sbb_bot, aes(x = year, y = CaCO3_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "CaCO3 Flux (g/m^2/day)") +
  ggtitle("SBB Bot: Yearly Average of CaCO3 Flux") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

plot_2l <- ggplot(sbb_bot, aes(x = year, y = OPAL_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "Opal Flux (g/m^2/day)") +
  ggtitle("SBB Bot: Yearly Average of Opal Flux") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

grid.arrange(plot_2a,plot_2b,plot_2c,plot_2d, plot_2e,plot_2f, nrow=3,ncol=3)
plotall_2a <- plot_2a+plot_2b+plot_2c+plot_2d+plot_2e+plot_2f
plotall_2a

grid.arrange(plot_2a,plot_2b,plot_2c, nrow=1,ncol=3)
plotall_2ab <- plot_2a+plot_2b+plot_2c
plotall_2ab

grid.arrange(plot_2d, plot_2e,plot_2f, nrow=1,ncol=3)
plotall_2ac <- plot_2d+plot_2e+plot_2f
plotall_2ac

grid.arrange(plot_2g,plot_2h,plot_2i,plot_2j, plot_2k,plot_2l, nrow=3,ncol=3)
plotall_2b <- plot_2g+plot_2h+plot_2i+plot_2j+plot_2k+plot_2l
plotall_2b
