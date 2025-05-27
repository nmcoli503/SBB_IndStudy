### SBB Ind Study Project ###
## Fall 2024 - Summer 2025 ##

#Santa Barbara Basin !BOTTOM TRAP!

# this here takes the data from the excel (we converted it to a csv file) and reads it as a csv and creates our data frame. This is the data frame we will be using throughout the rest of this  r script. I titled it "sbb_bot_boxplots" because it's from the SBB project, is focusing on the bottom trap, and then df stands for data frame. 

sbb_bot_df = read.csv(("data/CSV_files/SBB_Bot_Boxplots_moles.csv"), header = TRUE, sep = ",")
# this makes it show up in the console.
sbb_bot_df  

######

# here i loaded all my libraries. previously these have been downloaded and installed which is what i only loaded them here. 

library(ggplot2)
library(lubridate)
library(tidyverse)
library(carData)
library(gridBase)
library(gridExtra)
library(patchwork)
library (dplyr)

######

# here i make the date open column into an as.date format. 
sbb_bot_df$Date_Open = as.Date(sbb_bot_df$Date_Open)

# here i extract all the ~months~ from the Date_Open column and store it in a new column called month.
sbb_bot_df$month = format(sbb_bot_df$Date_Open,"%m")

# here i extract all the ~years~ from the Date_Open column and store it in a new column called year.
# use a capital Y to convert to a 4 digit year rather than just a 2 digit year
sbb_bot_df$year = format(sbb_bot_df$Date_Open,"%Y")


######

# #this line removes missing dates: if rows with missing dates aren't essential, you can remove them with filter (), 
#This code removes all rows from sbb_top_df where the Date_Open column is NA (missing), ensuring that the dataset contains only rows with valid dates.
# is.na(Date_Open) is TRUE for rows where Date_Open is missing. ! means "not", so this keeps rows where Date_Open is NOT missing.
sbb_bot_df = sbb_bot_df %>% filter(!is.na(Date_Open))

# i just wrote these next three lines to call the information so I could visualize date open, month and year in the console.
sbb_bot_df$Date_Open
sbb_bot_df$month
sbb_bot_df$year

######

# this code calculates the average total mass flux for each month from the sbb_bot_df dataset (ignoring missing values) and then stores the result in a new data frame called total_massflux_monthlyavg_sbb_bot.

# %>% is a pipe operator form the dplyr package that passes the dataframe sbb_top_boxplots to the next part of the code. 

# group_by(month) groups the data in the sbb_top_boxplots dataframe and groups it based on month. 

# summarise creates a summary statistic for each group. In this case we calcualte a new column in this example called "Avg_Total_Flux_sbb_top to hold the average of the Total_Mass_Flux_g_m2_day values. 

# finally mean() computes the averages of the Total_Mass_Flux_g_m2_day values for each month and na.omit omits missing values so it doesn't affect the mean calculation. 

# the successive lines do the same thing for each constituient. 1) total mass flux, 2) terrig flux, 3) PON flux, 4) POC flux, 5) CaCO3 flux, 6) Opal flux, 7) TPP flux, 8) PIP flux, 9) POP flux. After running this section there should be 9 data frames each with two columns --> one column is labelled 1-12 for each month of the year, and the second column is the averaged value of each month throughout the entire time frame of the dataset (all 32 years).

totalmassflux_monthlyavg_sbb_bot = sbb_bot_df %>% 
  group_by(month) %>% 
  summarise (Avg_Total_Mass_Flux_g_m2_day_sbb_bot = mean(na.omit(Total_Mass_Flux_g_m2_day)))

terigflux_monthlyavg_sbb_bot = sbb_bot_df %>% 
  group_by(month) %>% 
  summarise (Avg_Terrig_Flux_g_m2_day_sbb_bot = mean(na.omit(Terrigenous_Flux_g_m2_day)))

PONflux_monthlyavg_sbb_bot = sbb_bot_df %>% 
  group_by(month) %>% 
  summarise (Avg_PON_Flux_mmoles_m2_day_sbb_bot = mean(na.omit(PON_Flux_mmoles_m2_day)))

POCflux_monthlyavg_sbb_bot = sbb_bot_df %>% 
  group_by(month) %>% 
  summarise (Avg_POC_Flux_mmoles_m2_day_sbb_bot = mean(na.omit(POC_Flux_mmoles_m2_day)))

CaCO3flux_monthlyavg_sbb_bot = sbb_bot_df %>% 
  group_by(month) %>% 
  summarise (Avg_CaCO3_Flux_mmoles_m2_day_sbb_bot = mean(na.omit(CaCO3_Flux_mmoles_m2_day)))

Opalflux_monthlyavg_sbb_bot = sbb_bot_df %>% 
  group_by(month) %>% 
  summarise (Avg_Opal_Flux_mmoles_m2_day_sbb_bot = mean(na.omit(OPAL_Flux_mmoles_m2_day)))

TPPflux_monthlyavg_sbb_top = sbb_bot_df %>% 
  group_by(month) %>% 
  summarise (Avg_TPP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(TPP_Flux_umolesP_m2_day)))

PIPflux_monthlyavg_sbb_top = sbb_bot_df %>% 
  group_by(month) %>% 
  summarise (Avg_PIP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(PIP_Flux_umolesP_m2_day)))

POPflux_monthlyavg_sbb_top = sbb_bot_df %>% 
  group_by(month) %>% 
  summarise (Avg_POP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(POP_Flux_umolesP_m2_day)))

######

# this is the same but groups by year to find yearly average rather than monthly. 

# this code calculates the average total mass flux for each year from the sbb_bot_df dataset (ignoring missing values) and then stores the result in a new data frame called totalmassflux_yearlyavg_sbb_bot.

# the successive lines do the same thing for each constituient. 1) total mass flux, 2) terig flux, 3) PON flux, 4) POC flux, 5) CaCO3 flux, 6) Opal flux, 7) TPP flux, 8) PIP flux, 9) POP flux. After running this section there should be 9 data frames each with two columns --> one column is labelled a year (1993-2024) for each year in the 32 year dataset, and the second column is the averaged value of each year (incorporating all the months from that entire year).

totalmassflux_yearlyavg_sbb_bot <- sbb_bot_df %>% 
  group_by(year) %>% 
  summarise (Avg_Total_Mass_Flux_g_m2_day_sbb_bot = mean(na.omit(Total_Mass_Flux_g_m2_day)))

terigflux_yearlyavg_sbb_bot <- sbb_bot_df %>% 
  group_by(year) %>% 
  summarise (Avg_Terrig_Flux_g_m2_day_sbb_bot = mean(na.omit(Terrigenous_Flux_g_m2_day)))

PONflux_yearlyavg_sbb_bot <- sbb_bot_df %>% 
  group_by(year) %>% 
  summarise (Avg_PON_Flux_mmoles_m2_day_sbb_bot = mean(na.omit(PON_Flux_mmoles_m2_day)))

POCflux_yearlyavg_sbb_bot <- sbb_bot_df %>% 
  group_by(year) %>% 
  summarise (Avg_POC_Flux_mmoles_m2_day_sbb_bot = mean(na.omit(POC_Flux_mmoles_m2_day)))

CaCO3flux_yearlyavg_sbb_bot <- sbb_bot_df %>% 
  group_by(year) %>% 
  summarise (Avg_CaCO3_Flux_mmoles_m2_day_sbb_bot = mean(na.omit(CaCO3_Flux_mmoles_m2_day)))

Opalflux_yearlyavg_sbb_bot <- sbb_bot_df %>% 
  group_by(year) %>% 
  summarise (Avg_Opal_Flux_mmoles_m2_day_sbb_bot = mean(na.omit(OPAL_Flux_mmoles_m2_day)))

TPPflux_yearlyyavg_sbb_top <- sbb_bot_df %>% 
  group_by(year) %>% 
  summarise (Avg_TPP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(TPP_Flux_umolesP_m2_day)))

PIPflux_yearlyavg_sbb_top <- sbb_bot_df %>% 
  group_by(year) %>% 
  summarise (Avg_PIP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(PIP_Flux_umolesP_m2_day)))

POPflux_yearlyavg_sbb_top <- sbb_bot_df %>% 
  group_by(year) %>% 
  summarise (Avg_POP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(POP_Flux_umolesP_m2_day)))


#######

# this code begins to plot it all. it creates a boxplot of Total_Mass_Flux_g_m2_day by month using the sbb_bot_df dataset. It also adds the monthly mean as red dots and error bars showing confidence intervals around those means. The plot is styled with labels and a formatted y-axis. a-i represents the first set of 9 constituents for the monthly graphs. The 2 just indicates this is for the bottom trap.

plot_2a = ggplot(sbb_bot_df, aes(x = month, y = Total_Mass_Flux_g_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(
    x = "Month",
    y = "Total Mass Flux (g/m^2/day)",
    title = "SBB Bot: Monthly Average of Total Mass Flux") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14))


plot_2b = ggplot(sbb_bot_df, aes(x = month, y = Terrigenous_Flux_g_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(
    x = "Month",
    y = "Terrigenous Flux (g/m^2/day)",
    title = "SBB Bot: Monthly Average of Terrigenous Flux") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14))


plot_2c = ggplot(sbb_bot_df, aes(x = month, y = PON_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(
    x = "Month",
    y = "PON Flux (mmoles/m^2/day)",
    title = "SBB Bot: Monthly Average of PON Flux") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14))


plot_2d = ggplot(sbb_bot_df, aes(x = month, y = POC_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(
    x = "Month",
    y = "POC Flux (mmoles/m^2/day)",
    title = "SBB Bot: Monthly Average of POC Flux") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12))


plot_2e = ggplot(sbb_bot_df, aes(x = month, y = CaCO3_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(
    x = "Month",
    y = "CaCO3 Flux (mmoles/m^2)/day",
    title = "SBB Bot: Monthly Average of CaCO3 Flux") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12))


plot_2f = ggplot(sbb_bot_df, aes(x = month, y = OPAL_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(
    x = "Month",
    y = "Opal Flux (mmoles/m^2/day)",
    title = "SBB Bot: Monthly Average of Opal Flux") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12))


plot_2g = ggplot(sbb_bot_df, aes(x = month, y = TPP_Flux_umolesP_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(
    x = "Month",
    y = "TPP Flux (umoles/m^2/day)",
    title = "SBB Bot: Monthly Average of TPP Flux") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12))


plot_2h = ggplot(sbb_bot_df, aes(x = month, y = PIP_Flux_umolesP_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(
    x = "Month",
    y = "PIP Flux (umoles/m^2/day)",
    title = "SBB Bot: Monthly Average of PIP Flux") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12))

plot_2i = ggplot(sbb_bot_df, aes(x = month, y = POP_Flux_umolesP_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(
    x = "Month",
    y = "POP Flux (umoles/m^2/day)",
    title = "SBB Bot: Monthly Average of POP Flux") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12))

#######

plot_2j = ggplot(sbb_bot_df, aes(x = year, y = Total_Mass_Flux_g_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(
    x = "Year",
    y = "Total Mass Flux (g/m^2/day)",
    title = "SBB Bot: Yearly Average of Total Mass Flux") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


plot_2k = ggplot(sbb_bot_df, aes(x = year, y = Terrigenous_Flux_g_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(
    x = "Year",
    y = "Terrigenous Flux (g/m^2/day)",
    title = "SBB Bot: Yearly Average of Terrigenous Flux") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


plot_2l = ggplot(sbb_bot_df, aes(x = year, y = PON_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "PON Flux (mmoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Yearly Average of PON Flux") + 
  theme(
  plot.title = element_text(size = 18, face = "bold"),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


plot_2m = ggplot(sbb_bot_df, aes(x = year, y = POC_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "POC Flux (mmoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Yearly Average of POC Flux") +  
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


plot_2n = ggplot(sbb_bot_df, aes(x = year, y = CaCO3_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "CaCO3 Flux (mmoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Yearly Average of CaCO3 Flux") +  
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


plot_2o = ggplot(sbb_bot_df, aes(x = year, y = OPAL_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "Opal Flux (mmoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Yearly Average of Opal Flux") +  
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


plot_2p = ggplot(sbb_bot_df, aes(x = year, y = TPP_Flux_umolesP_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "TPP Flux (umoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Yearly Average of TPP Flux") +  
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


plot_2q = ggplot(sbb_bot_df, aes(x = year, y = PIP_Flux_umolesP_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "PIP Flux (umoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Yearly Average of PIP Flux") +  
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


plot_2r = ggplot(sbb_bot_df, aes(x = year, y = POP_Flux_umolesP_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "POP Flux (umoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Yearly Average of POP Flux") +  
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

##########

## MONTHLY PLOTS ##

# organizes plots together: total mass, terrig, PON, POC, CaCO3, Opal
sbb_bot_plot_2a_to_2f = grid.arrange(plot_2a,plot_2b,plot_2c,plot_2d,plot_2e,plot_2f, nrow = 2,ncol = 3)
sbb_bot_plot_2a_to_2f

# TPP, PIP, POP
sbb_bot_plot_2ghi= grid.arrange(plot_2g,plot_2h,plot_2i, nrow=1,ncol=3)
sbb_bot_plot_2ghi

# total mass, terrig flux
plot_2a_2b = grid.arrange(plot_2a,plot_2b, nrow=2,ncol=1)
plot_2a_2b

# PON, POC
plot_2c_2d = grid.arrange(plot_2c,plot_2d, nrow=2,ncol=1)
plot_2c_2d

# CaCO3, Opal
plot_2e_2f = grid.arrange(plot_2e,plot_2f, nrow=2,ncol=1)
plot_2e_2f

# TPP, PIP
plot_2g_2h = grid.arrange(plot_2g,plot_2h, nrow=2,ncol=1)

# POP
plot_2i = grid.arrange(plot_2i,nrow=1,ncol=1)

# this code saves them as a PDF.
ggsave("figures/SBB_Bot_Monthly_Total_Terrig.pdf", plot = plot_2a_2b, width = 10, height = 8)
ggsave("figures/SBB_Bot_Monthly_PON_POC.pdf", plot = plot_2c_2d, width = 10, height = 8)
ggsave("figures/SBB_Bot_Monthly_Carbonate_Opal.pdf", plot = plot_2e_2f, width = 10, height = 8)
ggsave("figures/SBB_Bot_Monthly_TPP_PIP.pdf", plot = plot_2g_2h, width = 10, height = 8)
ggsave("figures/SBB_Bot_Monthly_POP.pdf", plot = plot_2i, width = 10, height = 8)

# different way of plotting TPP, PIP and POP.
grid.arrange(plot_2g,plot_2h,plot_2i, nrow=1,ncol=3)
plotall_2ab <- plot_2g+plot_2h+plot_2i
plotall_2ab

# just plots 3 at a time. 
#grid.arrange(plot_2a,plot_2b,plot_2c, nrow=1,ncol=3)
#plotall_2ac <- plot_2a+plot_2b+plot_2c
#plotall_2ac

#grid.arrange(plot_2d, plot_2e,plot_2f, nrow=1,ncol=3)
#plotall_2ae <- plot_2d+plot_2e+plot_2f
#plotall_2ae

#######

## YEARLY PLOTS ##

# total mass, terrig, PON, POC, CaCo3, Opal. 
sbb_bot_plot_2j_to_2o = grid.arrange(plot_2j,plot_2k,plot_2l,plot_2m, plot_2n,plot_2o, nrow=2,ncol=3)
sbb_bot_plot_2j_to_2o

# TPP, PIP, POP.
sbb_bot_plot_2pqr= grid.arrange(plot_2p,plot_2q,plot_2r, nrow=1,ncol=3)
sbb_bot_plot_2pqr

# total mass, terrig
plot_2j_2k = grid.arrange(plot_2j,plot_2k, nrow=2,ncol=1)
plot_2j_2k

# PON, POC
plot_2l_2m = grid.arrange(plot_2l,plot_2m, nrow=2,ncol=1)
plot_2l_2m

# CaCO3, Opal
plot_2n_2o = grid.arrange(plot_2n,plot_2o, nrow=2,ncol=1)
plot_2n_2o

# TPP, POP
plot_2p_2q = grid.arrange(plot_2p,plot_2q, nrow=2,ncol=1)
plot_2p_2q

# POP
plot_2r = grid.arrange(plot_2r,nrow=1,ncol=1)
plot_2r

# saves the figures (yearly) as a PDF.
ggsave("figures/SBB_Bot_Yearly_Total_Terrig.pdf", plot = plot_2j_2k, width = 10, height = 8)
ggsave("figures/SBB_Bot_Yearly_PON_POC.pdf", plot = plot_2l_2m, width = 10, height = 8)
ggsave("figures/SBB_Bot_Yearly_Carbonate_Opal.pdf", plot = plot_2n_2o, width = 10, height = 8)
ggsave("figures/SBB_Bot_Yearly_TPP_PIP.pdf", plot = plot_2p_2q, width = 10, height = 8)
ggsave("figures/SBB_Bot_Yearly_POP.pdf", plot = plot_2r, width = 10, height = 8)

# plot yearly --> total, carbon, nitrogen, silica

# plot_2j # total
# plot_2l # nitrogen
# plot_2m # carbon
# plot_2o # silica


ggsave("figures/SBB_Bot_Yearly_TotalFlux.jpg", plot_2j, width = 10, height = 8)
ggsave("figures/SBB_Bot_Yearly_PONFlux.jpg", plot_2l, width = 10, height = 8)
ggsave("figures/SBB_Bot_Yearly_POCFlux.jpg", plot_2m, width = 10, height = 8)
ggsave("figures/SBB_Bot_Yearly_SilicaFlux.jpg", plot_2o, width = 10, height = 8)
ggsave("figures/CtoN_RatioGraph.jpg", plot_CtoN, width = 12, height = 8)

ggsave("figures/SBB_Bot_Monthly_POCFlux.jpg",plot_2d,width = 10, height = 8)

######

#CNP Ratios

# filters out rows with missing values. keeps only rows where both POC and PON flux values are present (i.e., not NA). stores the filtered result in filtered_flux_bot data frame.

filtered_flux_bot = sbb_bot_df %>%
  filter(!is.na(POC_Flux_mmoles_m2_day),
         !is.na(PON_Flux_mmoles_m2_day), 
         !is.na(TPP_Flux_umolesP_m2_day))

# Plot of Carbon to Nitrogen
# create a scatterplot with regression line. this plot has different colors for each year.
plot_CtoN = ggplot(filtered_flux_bot, aes(x = PON_Flux_mmoles_m2_day, y = POC_Flux_mmoles_m2_day, color = as.factor(year))) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred", se = TRUE)+
  geom_abline(slope = 6.6, intercept = 0, linetype = "dashed", color = "blue", linewidth = .7) +
  labs(x = "PON Flux (mmol/m²/day)",
       y = "POC Flux (mmol/m²/day)",
       title = "Carbon vs. Nitrogen Flux")

# this is the same but all the points are the same color and axis size and labels are larger.
plot_CtoN_2 = ggplot(filtered_flux_bot, aes(x = PON_Flux_mmoles_m2_day, y = POC_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  geom_abline(slope = 6.6, intercept = 0, linetype = "dashed", color = "blue", linewidth = .7) +
  labs(
    x = "PON Flux (mmol/m²/day)",
    y = "POC Flux (mmol/m²/day)",
    title = "Carbon vs. Nitrogen Flux"
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14))


# linear model of Carbon to Nitrogen
lm_CN = lm(POC_Flux_mmoles_m2_day ~ PON_Flux_mmoles_m2_day, data = filtered_flux_bot)
summary(lm_CN)

# Ratio = 7.6 (slighlty > than Redfield Ratio) 106:16 = 6.6

######

# Plot of Carbon to Phosphorous
ggplot(filtered_flux_bot, aes(x = TPP_Flux_umolesP_m2_day / 1000 , y = POC_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE)+
  geom_abline(slope = 106, intercept = 0, linetype = "dashed", color = "blue", linewidth = .7) +
  labs(x = "TPP Flux (ummolP/m²/day)",
       y = "POC Flux (mmol/m²/day)",
       title = "Carbon vs. Phosphorous Flux") +
  theme_minimal()

# linear model of Carbon vs. Phosphorous


filtered_flux_bot$TPP_Flux_mmolP_m2_day <- filtered_flux_bot$TPP_Flux_umolesP_m2_day / 1000

lm_CP = lm(POC_Flux_mmoles_m2_day ~ TPP_Flux_mmolP_m2_day, data = filtered_flux_bot)
summary(lm_CP)

# Ratio = 23.681 (much smaller < than  than Redfield Ratio) 106:1

######

# Plot of Nitrogen to Phosphorous

ggplot(filtered_flux_bot, aes(x = TPP_Flux_mmolP_m2_day, y = PON_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  geom_abline(slope = 16, intercept = 0, linetype = "dashed", color = "blue", linewidth = .7) +
  labs(x = "TPP Flux (mmol P/m²/day)",
       y = "PON Flux (mmol N/m²/day)",
       title = "Nitrogen vs. Phosphorus Flux") +
  theme_minimal()

# linear model of Carbon vs. Phosphorous 

lm_NP = lm(PON_Flux_mmoles_m2_day ~ TPP_Flux_mmolP_m2_day, data = filtered_flux_bot)
summary(lm_NP)

# Ratio = 2.6 (much smaller < than Redfield Ratio) 16:1

# Note. when the ratio is smaller than redfield ratio, the dashed ratio line is ABOVE the linear model. when the ratio is is larger than the redfield ratio, the dashed ratio line is BELOW the linear model.


# saves some graphs
ggsave("figures/CtoN_RatioGraph.jpg", plot_CtoN, width = 12, height = 8)
ggsave("figures/SBB_Bot_Monthly_TotalFlux.jpg",plot_2a,width = 10, height = 8)
#############################

#Linear Models: modeling over time
