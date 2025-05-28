## This code is pretty outdated ##

### SBB Ind Study Project ###
## Fall 2024 - Summer 2025 ##

#Santa Barbara Basin !TOP TRAP!

# this code here downloads and installs various packages from CRAN. Typically you only need to install once and then load the library in each script.

#download.packages("ggplot")

#install.packages("ggplot2") 
#install.packages("lubridate") 
#install.packages("tidyverse")
#install.packages("carData")
#install.packages("gridBase")
#install.packages("gridExtra")
#install.packages("patchwork")


# Here is how we grab our raw data, (THIS NEEDS TO BE UPDATED, 5/27/25)

#assigns data in excel sheet to a variable just to be succinct

sbb_top<-SBB_SedTrap_Master_updated_11_4_boxplots

library(ggplot2)
library(lubridate)
library(tidyverse)   # tidyverse (allows the gaps in data)
library(carData)     # cardata (helps clean data)
library(gridBase)    # gridbase
library(gridExtra)   # gridextra
library(patchwork)   # patchwork
library(dplyr)   #allows r to process data more easily

# these last three work together. Can plot multiple  graphs on one page

#str(sbb_top)


sbb_top$Date_Open <-as.Date(sbb_top$Date_Open)

#sbb_top <- sbb_top %>%
  #mutate(
   # month = month(Date_Open),
    #year = year(Date_Open))

#extracts all the months from the Date_Open and stores it in a new column called month
sbb_top$month <- format(sbb_top$Date_Open,"%m")

#extracts the year from the Date_Open column and stores it in a new column called year.
sbb_top$year <- format(sbb_top$Date_Open,"%y")

#this line removes missing dates: if rows with missing dates arent essential, you can remove them with filter (), 
#This code removes all rows from sbb_top where the Date_Open column has missing values, ensuring that the dataset contains only rows with valid dates.
sbb_top <- sbb_top %>% filter(!is.na(Date_Open))

#I just wrote these next three lines to call the information so I could visualize it.
sbb_top$Date_Open
sbb_top$month
sbb_top$year

#this code calculates the monthly average of a variable (total mass flux) and prepares it for plotting. 1) First we calculate the average total mass flux value for each month. 2) This information is then put into a new data frame called monthly average with two columns, month & total mass flux. 3) It is then grouped by the month column, and then the result of this information is piped into the line above it. 

#na.omit, gets rid of 0's in data

massflux_monthlyavg_sbb_top <- sbb_top%>% 
  group_by(month) %>% 
  summarise (Avg_Total_Mass_Flux_sbb_top = mean(na.omit(Total_Mass_Flux)))

terigflux_monthlyavg_sbb_top <- sbb_top%>% 
  group_by(month) %>% 
  summarise (Avg_Terrig_Flux_sbb_top = mean(na.omit(Terrigenous_Flux)))

PONflux_monthlyavg_sbb_top <- sbb_top%>% 
  group_by(month) %>% 
  summarise (Avg_PON_Flux_sbb_top = mean(na.omit(PON_Flux)))

POCflux_monthlyavg_sbb_top <- sbb_top%>% 
  group_by(month) %>% 
  summarise (Avg_POC_Flux_sbb_top = mean(na.omit(POC_Flux)))

CaCO3flux_monthlyavg_sbb_top <- sbb_top%>% 
  group_by(month) %>% 
  summarise (Avg_CaCO3_Flux_sbb_top = mean(na.omit(CaCO3_Flux)))

Opalflux_monthlyavg_sbb_top <- sbb_top%>% 
  group_by(month) %>% 
  summarise (Avg_Opal_Flux_sbb_top = mean(na.omit(OPAL_Flux)))

#################

massflux_yearlyavg_sbb_top <- sbb_top%>% 
  group_by(year) %>% 
  summarise (Avg_Total_Mass_Flux_sbb_top = mean(na.omit(Total_Mass_Flux)))

terigflux_yearlyavg_sbb_top <- sbb_top%>% 
  group_by(year) %>% 
  summarise (Avg_Terrig_Flux_sbb_top = mean(na.omit(Terrigenous_Flux)))

PONflux_yearlyavg_sbb_top <- sbb_top%>% 
  group_by(year) %>% 
  summarise (Avg_PON_Flux_sbb_top = mean(na.omit(PON_Flux)))

POCflux_yearlyavg_sbb_top <- sbb_top%>% 
  group_by(year) %>% 
  summarise (Avg_POC_Flux_sbb_top = mean(na.omit(POC_Flux)))

CaCO3flux_yearlyavg_sbb_top <- sbb_top%>% 
  group_by(year) %>% 
  summarise (Avg_CaCO3_Flux_sbb_top = mean(na.omit(CaCO3_Flux)))

Opalflux_yearlyavg_sbb_top <- sbb_top%>% 
  group_by(year) %>% 
  summarise (Avg_Opal_Flux_sbb_top = mean(na.omit(OPAL_Flux)))

###################
plot_a <- ggplot(sbb_top, aes(x = month, y = Total_Mass_Flux)) +
 geom_boxplot() +
 stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
 stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
   labs(x = "Month", y = "Total Mass Flux (g/m^2/day)") +
   ggtitle("SBB Top: Monthly Average of Total Mass Flux")

plot_b <- ggplot(sbb_top, aes(x = month, y = Terrigenous_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "Terrigenous Flux (g/m^2/day)") +
  ggtitle("SBB Top: Monthly Average of Terrigenous Flux")

plot_c <- ggplot(sbb_top, aes(x = month, y = PON_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "PON Flux (g/m^2/day)") +
  ggtitle("SBB Top: Monthly Average of PON Flux")

plot_d <- ggplot(sbb_top, aes(x = month, y = POC_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "POC Flux (g/m^2/day)") +
  ggtitle("SBB Top: Monthly Average of POC Flux")

plot_e <- ggplot(sbb_top, aes(x = month, y = CaCO3_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "CaCO3 Flux (g/m^2)/day") +
  ggtitle("SBB Top: Monthly Average of CaCO3 Flux")

plot_f <- ggplot(sbb_top, aes(x = month, y = OPAL_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "Opal Flux (g/m^2/day)") +
  ggtitle("SBB Top: Monthly Average of Opal Flux")

###################

plot_g <- ggplot(sbb_top, aes(x = year, y = Total_Mass_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "Total Mass Flux (g/m^2/day)") +
  ggtitle("SBB Top: Yearly Average of Total Mass Flux")

plot_h <- ggplot(sbb_top, aes(x = year, y = Terrigenous_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "Terrigenous Flux (g/m^2/day)") +
  ggtitle("SBB Top: Yearly Average of Terrigenous Flux")

plot_i <- ggplot(sbb_top, aes(x = year, y = PON_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "PON Flux (g/m^2/day)") +
  ggtitle("SBB Top: Yearly Average of PON Flux")

plot_j <- ggplot(sbb_top, aes(x = year, y = POC_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "POC Flux (g/m^2/day)") +
  ggtitle("SBB Top: Yearly Average of POC Flux")

plot_k <- ggplot(sbb_top, aes(x = year, y = CaCO3_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "CaCO3 Flux (g/m^2/day)") +
  ggtitle("SBB Top: Yearly Average of CaCO3 Flux")

plot_l <- ggplot(sbb_top, aes(x = year, y = OPAL_Flux)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "Opal Flux (g/m^2/day)") +
  ggtitle("SBB Top: Yearly Average of Opal Flux")

######
# Question 1: what is the red point? --> mean, thick line = median
# Question 2: the sd is different than the quartile lines??
# Question 3: how to get rid of N/A after 2024 and month 12
# Question 4: is removed rows okay? "Removed 53 rows containing non-finite outside the scale range (`stat_boxplot()`)."
# Question 5: putting them all together
# Question 6: why is there a value for NA

#class(plota)

grid.arrange(plot_a,plot_b,plot_c,plot_d, plot_e,plot_f, nrow=3,ncol=3)
plotall_a <- plot_a+plot_b+plot_c+plot_d+plot_e+plot_f
plotall_a

grid.arrange(plot_a,plot_b,plot_c, nrow=1,ncol=3)
plotall_ab <- plot_a+plot_b+plot_c
plotall_ab

grid.arrange(plot_d, plot_e,plot_f, nrow=1,ncol=3)
plotall_ac <- plot_d+plot_e+plot_f
plotall_ac

grid.arrange(plot_g,plot_h,plot_i,plot_j, plot_k,plot_l, nrow=3,ncol=3)
plotall_b <- plot_g+plot_h+plot_i+plot_j+plot_k+plot_l
plotall_b
