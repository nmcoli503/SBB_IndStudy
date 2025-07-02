### SBB Ind Study Project ###
## Fall 2024 - Summer 2025 ##

#Santa Barbara Basin !TOP TRAP!

# this takes the data from the excel (converted to a csv file), reads it as a csv, and creates our data frame. This is the data frame we will be using throughout the rest of this  r script. I titled it "sbb_top_df" because it's from the SBB project, is focusing on the top trap, and then df stands for data frame. 
sbb_top_df = read.csv(("data/CSV_files/SBB_SedTrap_Top_Updated.csv"), header = TRUE, sep = ",")
# this makes it show up in the console.
sbb_top_df 

######

# here i loaded all my libraries. previously these have been downloaded and installed which is why i have only loaded them here. 

library(ggplot2)
library(lubridate)
library(tidyverse)   # tidyverse (allows the gaps in data)
library(carData)     # cardata (helps clean data)
library(gridBase)    # gridbase
library(gridExtra)   # gridextra
library(patchwork)   # patchwork
library(broom)
library(dplyr)       #allows r to process data more easily
# these last three work together. Can plot multiple  graphs on one page

######

# here i make the date open column into an as.date format. 
sbb_top_df$Mid_Julian = as.Date(sbb_top_df$Mid_Julian, format = "%d-%b-%y")

unique(sbb_top_df$Mid_Julian)
str(sbb_top_df$Mid_Julian)

# here i extract all the ~months~ from the Date_Open column (as a two digit string) and store it in a new column called month.
sbb_top_df$month = format(sbb_top_df$Mid_Julian,"%m")

# here i extract all the ~years~ from the Date_Open column (as a two digit string) and store it in a new column called year.
# note: use a capital Y to convert to a 4 digit year rather than just a 2 digit year
sbb_top_df$year = format(sbb_top_df$Mid_Julian,"%Y")

#this line removes missing dates: if rows with missing dates aren't essential, you can remove them with filter (), 
#This code removes all rows from sbb_top_df where the Mid_Julian column is NA (missing), ensuring that the dataset contains only rows with valid dates.
# is.na(Mid_Julian) is TRUE for rows where Mid_Julian is missing. ! means "not", so this keeps rows where Date_Open is NOT missing.

# note: the rows went from 353 to 332 meaning 20 rows did not have a date filled out. (this is likely because we have traps where the date got skipped because of clogs and such or because an extra row was added for visual clarity. (one of the rows had 2000 in the date SBB 55-13, i adjusted it quick in R rather than resave another CSV. It was a typo; NMC 6/30/25)
sbb_top_df = sbb_top_df %>% filter(!is.na(Mid_Julian) & year != "2000")

# i just wrote these next three lines to call the information so i could visualize date open, month and year in the console.
sbb_top_df$Mid_Julian
sbb_top_df$month
sbb_top_df$year

######

# this code calculates the average total mass flux for each month from the sbb_top_df dataset (ignoring missing values) and then stores the result in a new data frame called total_massflux_monthlyavg_sbb_bot.

# %>% is a pipe operator from the dplyr package that passes the dataframe sbb_top_df to the next part of the code. 

# group_by(month) groups the data in the sbb_top_df  and groups it based on month. 

# summarise creates a summary statistic for each group. in this case we calcualte a new column in this example called "Avg_Total_Flux_sbb_top to hold the average of the Total_Mass_Flux_g_m2_day values. 

# finally mean() computes the averages of the Total_Mass_Flux_g_m2_day values for each month and na.omit omits missing values so it doesn't affect the mean calculation. 

# the successive lines do the same thing for each constituient. 1) total mass flux, 2) terig flux, 3) PON flux, 4) POC flux, 5) CaCO3 flux, 6) Opal flux, 7) TPP flux, 8) PIP flux, 9) POP flux. After running this section there should be 9 data frames each with two columns --> one column is labelled 1-12 for each month of the year, and the second column is the averaged value of each month throughout the entire time frame of the dataset (all 16 years since this is just top).

totalmassflux_monthlyavg_sbb_top = sbb_top_df%>% 
  group_by(month) %>% 
  summarise (Avg_Total_Mass_Flux_g_m2_day_sbb_top = mean(na.omit(Total_Mass_Flux_g_m2_day)))

terigflux_monthlyavg_sbb_top = sbb_top_df%>% 
  group_by(month) %>% 
  summarise (Avg_Terrig_Flux_g_m2_da_sbb_top = mean(na.omit(Terrigenous_Flux_g_m2_day)))

PONflux_monthlyavg_sbb_top = sbb_top_df%>% 
  group_by(month) %>% 
  summarise (Avg_PON_Flux_mmoles_m2_day_sbb_top = mean(na.omit(PON_Flux_mmoles_m2_day)))

POCflux_monthlyavg_sbb_top = sbb_top_df%>% 
  group_by(month) %>% 
  summarise (Avg_POC_Flux_mmoles_m2_day_sbb_top = mean(na.omit(POC_Flux_mmoles_m2_day)))

CaCO3flux_monthlyavg_sbb_top = sbb_top_df%>% 
  group_by(month) %>% 
  summarise (Avg_CaCO3_Flux_mmoles_m2_day_sbb_top = mean(na.omit(CaCO3_Flux_mmoles_m2_day)))

Opalflux_monthlyavg_sbb_top = sbb_top_df%>% 
  group_by(month) %>% 
  summarise (Avg_Opal_Flux_mmoles_m2_day_sbb_top = mean(na.omit(Opal_Flux_mmoles_m2_day)))

TPPflux_monthlyavg_sbb_top = sbb_top_df%>% 
  group_by(month) %>% 
  summarise (Avg_TPP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(TPP_Flux_umolesP_m2_day)))

PIPflux_monthlyavg_sbb_top = sbb_top_df%>% 
  group_by(month) %>% 
  summarise (Avg_PIP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(PIP_Flux_umolesP_m2_day)))

POPflux_monthlyavg_sbb_top = sbb_top_df%>% 
  group_by(month) %>% 
  summarise (Avg_POP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(POP_Flux_umolesP_m2_day)))

####################################

# this is the same but groups by year to find YEARLY average rather than monthly. 

# this code calculates the average total mass flux for each year from the sbb_top_df dataset (ignoring missing values) and then stores the result in a new data frame called totalmassflux_yearlyavg_sbb_top.

# the successive lines do the same thing for each constituient. 1) total mass flux, 2) terig flux, 3) PON flux, 4) POC flux, 5) CaCO3 flux, 6) Opal flux, 7) TPP flux, 8) PIP flux, 9) POP flux. After running this section there should be 9 data frames each with two columns --> one column is labelled a year (1993-2024) for each year in the 32 year dataset, and the second column is the averaged value of each year (incorporating all the months from that entire year).

unique(sbb_top_df$year)
sbb_top_df %>% filter(is.na(Mid_Julian) | Mid_Julian == "" | year == "2000")


totalmassflux_yearlyavg_sbb_top = sbb_top_df%>% 
  group_by(year) %>% 
  summarise (Avg_Total_Mass_Flux_g_m2_day_sbb_top = mean(na.omit(Total_Mass_Flux_g_m2_day)))

terigflux_yearlyavg_sbb_top = sbb_top_df%>% 
  group_by(year) %>% 
  summarise (Avg_Terrig_Flux_g_m2_day_sbb_top = mean(na.omit(Terrigenous_Flux_g_m2_day)))

PONflux_yearlyavg_sbb_top = sbb_top_df%>% 
  group_by(year) %>% 
  summarise (Avg_PON_Flux_mmoles_m2_day_sbb_top = mean(na.omit(PON_Flux_mmoles_m2_day)))

POCflux_yearlyavg_sbb_top = sbb_top_df%>% 
  group_by(year) %>% 
  summarise (Avg_POC_Flux_mmoles_m2_day_sbb_top = mean(na.omit(POC_Flux_mmoles_m2_day)))

CaCO3flux_yearlyavg_sbb_top = sbb_top_df%>% 
  group_by(year) %>% 
  summarise (Avg_CaCO3_Flux_mmoles_m2_day_sbb_top = mean(na.omit(CaCO3_Flux_mmoles_m2_day)))

Opalflux_yearlyavg_sbb_top = sbb_top_df%>% 
  group_by(year) %>% 
  summarise (Avg_Opal_Flux_mmoles_m2_day_sbb_top = mean(na.omit(Opal_Flux_mmoles_m2_day)))

TPPflux_yearlyavg_sbb_top = sbb_top_df%>% 
  group_by(year) %>% 
  summarise (Avg_TPP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(TPP_Flux_umolesP_m2_day)))

PIPflux_yearlyavg_sbb_top = sbb_top_df%>% 
  group_by(year) %>% 
  summarise (Avg_PIP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(PIP_Flux_umolesP_m2_day)))

POPflux_yearlyavg_sbb_top = sbb_top_df%>% 
  group_by(year) %>% 
  summarise (Avg_POP_Flux_umolesP_m2_day_sbb_top = mean(POP_Flux_umolesP_m2_day,na.rm=TRUE))

#####################################

#str(sbb_top_df)
#class(month)

# this code begins to plot it all. it creates a boxplot of Total_Mass_Flux_g_m2_day by month using the sbb_top_df dataset. It also adds the monthly mean as red dots and error bars showing confidence intervals around those means. The plot is styled with labels and a formatted y-axis. a-i represents the first set of 9 constituents for the monthly graphs. j-r represents the second set off 9 constitudents for the yearly graphs.

plot_a = ggplot(sbb_top_df, aes(x = month, y = Total_Mass_Flux_g_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "Total Mass Flux (g/m^2/day)") +  scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Top: Monthly Average of Total Mass Flux")

plot_b = ggplot(sbb_top_df, aes(x = month, y = Terrigenous_Flux_g_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "Terrigenous Flux (g/m^2/day)") +  scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Top: Monthly Average of Terrigenous Flux")

plot_c = ggplot(sbb_top_df, aes(x = month, y = PON_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "PON Flux (mmoles/m^2/day)") +  scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Top: Monthly Average of PON Flux")

plot_d = ggplot(sbb_top_df, aes(x = month, y = POC_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "POC Flux (mmoles/m^2/day)") +  scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Top: Monthly Average of POC Flux")

plot_e = ggplot(sbb_top_df, aes(x = month, y = CaCO3_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "CaCO3 Flux (mmoles/m^2)/day") +  scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Top: Monthly Average of CaCO3 Flux")

plot_f = ggplot(sbb_top_df, aes(x = month, y = Opal_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "Opal Flux (mmoles/m^2/day)") +  scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Top: Monthly Average of Opal Flux")

plot_g = ggplot(sbb_top_df, aes(x = month, y = TPP_Flux_umolesP_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "TPP Flux (umoles/m^2/day)") +  scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Top: Monthly Average of TPP Flux")

plot_h = ggplot(sbb_top_df, aes(x = month, y = PIP_Flux_umolesP_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "PIP Flux (umoles/m^2/day)") +  scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Top: Monthly Average of PIP Flux")

plot_i = ggplot(data = sbb_top_df, aes(x = month, y = POP_Flux_umolesP_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "POP Flux (umoles/m^2/day)") +  scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Top: Monthly Average of POP Flux")

#################

plot_j = ggplot(sbb_top_df, aes(x = year, y = Total_Mass_Flux_g_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "Total Mass Flux (g/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Top: Yearly Average of Total Mass Flux")

plot_k = ggplot(sbb_top_df, aes(x = year, y = Terrigenous_Flux_g_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "Terrigenous Flux (g/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Top: Yearly Average of Terrigenous Flux")

plot_l = ggplot(sbb_top_df, aes(x = year, y = PON_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "PON Flux (mmoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Top: Yearly Average of PON Flux")

plot_m = ggplot(sbb_top_df, aes(x = year, y = POC_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "POC Flux (mmoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Top: Yearly Average of POC Flux")

plot_n = ggplot(sbb_top_df, aes(x = year, y = CaCO3_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "CaCO3 Flux (mmoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Top: Yearly Average of CaCO3 Flux")

plot_o = ggplot(sbb_top_df, aes(x = year, y = Opal_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "Opal Flux (mmoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Top: Yearly Average of Opal Flux")

plot_p = ggplot(sbb_top_df, aes(x = year, y = TPP_Flux_umolesP_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "TPP Flux (umoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Top: Yearly Average of TPP Flux")

plot_q = ggplot(sbb_top_df, aes(x = year, y = PIP_Flux_umolesP_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "PIP Flux (umoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Top: Yearly Average of PIP Flux")

plot_r = ggplot(sbb_top_df, aes(x = year, y = POP_Flux_umolesP_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "POP Flux (umoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Top: Yearly Average of POP Flux")

######
# Question 1: what is the red point? 
# Answer: mean, thick line = median

# Question 2: the sd is different than the quartile lines??
# Answer: yes

#class(plota)
######

## MONTHLY PLOTS ##

# organizes plots together: total mass, terrig, PON, POC, CaCO3, Opal
sbbtop_plot_a_to_f = grid.arrange(plot_a,plot_b,plot_c,plot_d,plot_e,plot_f, nrow = 2,ncol = 3)
print(sbbtop_plot_a_to_f)

# TPP, PIP, POP
sbbtop_plot_ghi= grid.arrange(plot_g,plot_h,plot_i, nrow=1,ncol=3)
sbbtop_plot_ghi

# total mass, terrig flux
plot_a_b = grid.arrange(plot_a,plot_b, nrow=2,ncol=1)
plot_a_b

# PON, POC
plot_c_d = grid.arrange(plot_c,plot_d, nrow=2,ncol=1)
plot_c_d

# CaCO3, Opal
plot_e_f = grid.arrange(plot_e,plot_f, nrow=2,ncol=1)
plot_e_f

# TPP, PIP
plot_g_h = grid.arrange(plot_g,plot_h, nrow=2,ncol=1)

# POP
plot_i = grid.arrange(plot_i,nrow=1,ncol=1)

# this code saves them as a PDF.
ggsave("figures/SBBTop_Monthly_Total_Terrig.pdf", plot = plot_a_b, width = 10, height = 8)
ggsave("figures/SBBTop_Monthly_PON_POC.pdf", plot = plot_c_d, width = 10, height = 8)
ggsave("figures/SBBTop_Monthly_Carbonate_Opal.pdf", plot = plot_e_f, width = 10, height = 8)
ggsave("figures/SBBTop_Monthly_TPP_PIP.pdf", plot = plot_g_h, width = 10, height = 8)
ggsave("figures/SBBTop_Monthly_POP.pdf", plot = plot_i, width = 10, height = 8)

# this code saves them as a PDF.
ggsave("figures/SBBTop_Monthly_Total_Terrig.png", plot = plot_a_b, width = 10, height = 8)
ggsave("figures/SBBTop_Monthly_PON_POC.png", plot = plot_c_d, width = 10, height = 8)
ggsave("figures/SBBTop_Monthly_Carbonate_Opal.png", plot = plot_e_f, width = 10, height = 8)
ggsave("figures/SBBTop_Monthly_TPP_PIP.png", plot = plot_g_h, width = 10, height = 8)
ggsave("figures/SBBTop_Monthly_POP.png", plot = plot_i, width = 10, height = 8)

######
# # just plots 3 at a time; total, terrig, PON
grid.arrange(plot_a,plot_b,plot_c, nrow=1,ncol=3)
plotall_ac = plot_a+plot_b+plot_c
plotall_ac

grid.arrange(plot_d, plot_e,plot_f, nrow=1,ncol=3)
plotall_ae = plot_d+plot_e+plot_f
plotall_ae

sbbtop_plot_j_to_o = grid.arrange(plot_j,plot_k,plot_l,plot_m, plot_n,plot_o, nrow=2,ncol=3)
sbbbot_plot_j_to_o

sbbtop_plot_pqr = grid.arrange(plot_p,plot_q,plot_r, nrow=1,ncol=3)
sbbtop_plot_pqr

######

## YEARLY PLOTS ##

# total, terrig
plot_j_k = grid.arrange(plot_j,plot_k, nrow=2,ncol=1)
plot_j_k

#PON, POC
plot_l_m = grid.arrange(plot_l,plot_m, nrow=2,ncol=1)
plot_l_m

# CaCO3, Opal
plot_n_o = grid.arrange(plot_n,plot_o, nrow=2,ncol=1)
plot_n_o

# TPP, PIP
plot_p_q = grid.arrange(plot_p,plot_q, nrow=2,ncol=1)
plot_p_q

# POP
plot_r = grid.arrange(plot_r,nrow=1,ncol=1)
plot_r

# saves as PDF again.
ggsave("figures/SBBTop_Yearly_Total_Terrig.png", plot = plot_j_k, width = 10, height = 8)
ggsave("figures/SBBTop_Yearly_PON_POC.png", plot = plot_l_m, width = 10, height = 8)
ggsave("figures/SBBTop_Yearly_Carbonate_Opal.png", plot = plot_n_o, width = 10, height = 8)
ggsave("figures/SBBTop_Yearly_TPP_PIP.png", plot = plot_p_q, width = 10, height = 8)
ggsave("figures/SBBTop_Yearly_POP.png", plot = plot_r, width = 10, height = 8)

########################
########################
########################

## C:N:P RATIOS ##

#install.packages("emmeans") # if not already installed
library(emmeans)

# filters out rows with missing values. keeps only rows where POC, PON, and TPP flux values are present (i.e., not NA). stores the filtered result in filtered_flux_bot data frame.
filtered_flux_top = sbb_top_df %>%
  filter(!is.na(POC_Flux_mmoles_m2_day),
         !is.na(PON_Flux_mmoles_m2_day),
         !is.na(TPP_Flux_umolesP_m2_day))

# PLOTS OF CARBON TO NITROGEN

# creates a scatterplot with a regression line and added slope line (106:16 = 6.6)
plot_CtoN_top = ggplot(filtered_flux_top, aes(x = PON_Flux_mmoles_m2_day, y = POC_Flux_mmoles_m2_day )) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE)+
  geom_abline(slope = 6.6, intercept = 0, linetype = "dashed", color = "blue", linewidth = .7) +
  labs(x = "PON Flux (mmol/m²/day)",
       y = "POC Flux (mmol/m²/day)",
       title = "SBB TOP: Carbon vs. Nitrogen Flux") +
  theme_minimal()
plot_CtoN_top

# linear model of Carbon vs. Nitrogen
lm_CN_top = lm(POC_Flux_mmoles_m2_day ~ PON_Flux_mmoles_m2_day, data = filtered_flux_top)
summary(lm_CN_top)

# ratio = 7.63005 (slightly > than Redfield Ratio of 6.6)
# adjusted r-squared: 0.946 
# p value = < 2.2e-16

# facet wrapped by year (CtoN)
CtoN_by_year = ggplot(filtered_flux_top, aes(x = PON_Flux_mmoles_m2_day, y = POC_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  geom_abline(slope = 6.6, intercept = 0, linetype = "dashed", color = "blue", linewidth = 0.7) +
  labs(x = "PON Flux (mmol/m²/day)",
       y = "POC Flux (mmol/m²/day)",
       title = "SBB Top: Carbon vs. Nitrogen Flux by Year") +
  facet_wrap(~year) +
  theme_minimal()
CtoN_by_year

# fitting models by group (CtoN)
table_of_CN_year = filtered_flux_top %>%
  group_by(year) %>%
  filter(!is.na(PON_Flux_mmoles_m2_day), !is.na(POC_Flux_mmoles_m2_day)) %>%
  do(tidy(lm(POC_Flux_mmoles_m2_day ~ PON_Flux_mmoles_m2_day, data = .)))
# the result of this code is a table that provides the intercept, slope (ratio), and p-values per year 
table_of_CN_year

table_of_CN_slope <- filtered_flux_top %>%
  filter(!is.na(POC_Flux_mmoles_m2_day), !is.na(PON_Flux_mmoles_m2_day)) %>%
  group_by(year) %>%
  do(tidy(lm(POC_Flux_mmoles_m2_day ~ PON_Flux_mmoles_m2_day, data = .))) %>%
  ungroup() %>%
  filter(term == "PON_Flux_mmoles_m2_day") %>%
  select(year,
         slope = estimate,
         pval_slope = p.value,
         stderr_slope = std.error)
table_of_CN_slope

write.table(table_of_CN_slope, "clipboard", sep = "\t", row.names = FALSE)


# Significance of Slopes

# C:N Interaction Model
model_interaction_C_N = lm(POC_Flux_mmoles_m2_day ~ PON_Flux_mmoles_m2_day * factor(year), data = filtered_flux_top)
summary(model_interaction_C_N)

model_no_interaction_C_N = lm(POC_Flux_mmoles_m2_day ~ PON_Flux_mmoles_m2_day + factor(year), data = filtered_flux_top)

anova(model_no_interaction_C_N, model_interaction_C_N)
# justified to move on? yes!

# C:N Emmeans
# get estimated slopes of POC ~ PON for each year.
# Compare C:N ratios (as slopes) by year
emtrends_C_N = emtrends(model_interaction_C_N, specs = "year", var = "PON_Flux_mmoles_m2_day")


pairs(emtrends_C_N, adjust = "tukey")
# this line of code... 1) Compares the C:N slopes between all pairs of years, 2) Tells you whether the difference in slope between (e.g.) 2010 and 2015 is statistically significant, 3) Adjusts the p-values to avoid false positives due to multiple comparisons

pairs(emtrends_C_N, adjust = "tukey") %>%
  as.data.frame() %>%
  mutate(sig = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ ".",
    TRUE ~ ""
  )) %>%
  select(contrast, estimate, p.value, sig)

intercept_emmeans_C_N = emmeans(model_interaction_C_N, specs = "year")
pairs(intercept_emmeans_C_N, adjust = "tukey")  # pairwise intercept comparisons


plot_data = as.data.frame(emtrends_C_N)

ggplot(plot_data, aes(x = year, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), width = 0.2) +
  ylab("C:N Ratio (Slope of POC ~ PON)") +
  xlab("Year") +
  theme_minimal() +
  ggtitle("Estimated C:N Slopes by Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# plots of the ratio in the form of a line.
# this adds another column of the C to N ratio.
filtered_flux_top = filtered_flux_top %>%
  mutate(C_to_N_ratio = POC_Flux_mmoles_m2_day / PON_Flux_mmoles_m2_day)

# this actually plots the line graph. 
CtoN_ratio_line = ggplot(filtered_flux_top, aes(x = Mid_Julian, y = C_to_N_ratio)) +
  geom_line() +
  geom_smooth(method = "loess") +
  labs(title = "SBB Top: C:N Flux Ratio Time Series",
       x = "Date", y = "C:N Ratio") +
  theme_minimal()
CtoN_ratio_line

# plots the ratio in the form of a boxplot
CtoN_ratio_boxplots = ggplot(filtered_flux_top, aes(x = as.factor(year), y = C_to_N_ratio)) +
  geom_boxplot(fill = "skyblue") +
  geom_hline(yintercept = 6.6, linetype = "dashed", color = "blue") +  # Redfield reference
  labs(title = "SBB Top: C:N Flux Ratio by Year Boxplots",
       x = "Year", y = "C:N Ratio") +
  theme_minimal()
CtoN_ratio_boxplots

#########################################

## PLOTS OF CARBON TO PHOSPHOROUS ##

# create a scatterplot with regression line and added slope line (106:1 = 106)
plot_CtoP_top = ggplot(filtered_flux_top, aes(x = TPP_Flux_umolesP_m2_day / 1000, y = POC_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE)+
  geom_abline(slope = 106, intercept = 0, linetype = "dashed", color = "blue", linewidth = .7) +
  labs(x = "TPP Flux (mmolP/m²/day)",
       y = "POC Flux (mmol/m²/day)",
       title = "SBB Top: Carbon vs. Phosphorous Flux") +
  theme_minimal()
plot_CtoP_top

# Now refit with converted P units

# this does the same as below (filters the data putting TPP into mmoles rather than umoles) but using tidy verse "mutate"
filtered_flux_top = filtered_flux_top %>%
  mutate(TPP_Flux_mmolesP_m2_day = TPP_Flux_umolesP_m2_day / 1000)

# this does the same as above but with base R and '$' rather than tidy verse. 
# filtered_flux_bot$TPP_Flux_mmolP_m2_day = filtered_flux_bot$TPP_Flux_umolesP_m2_day / 1000

# linear model of Carbon vs. Phosphorous (data is filtered first, units are converted)
lm_CP_top= lm(POC_Flux_mmoles_m2_day ~ TPP_Flux_mmolesP_m2_day , data = filtered_flux_top)
summary(lm_CP_top)
# Ratio = 36.1745 (which is much < than Redfield Ratio of 106:1)
# adjusted r-squared: 0.5605
# p value = < 2.2e-16

# facet wrapped by year (CtoP)
CtoP_by_year = ggplot(filtered_flux_top, aes(x = TPP_Flux_mmolesP_m2_day, y = POC_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  geom_abline(slope = 106, intercept = 0, linetype = "dashed", color = "blue", linewidth = 0.7) +
  labs(x = "TPP Flux (mmolP/m²/day)",
       y = "POC Flux (mmol/m²/day)",
       title = "SBB Top Trap: Carbon vs. Phosphorous Flux by Year") +
  facet_wrap(~year) +
  theme_minimal()
CtoP_by_year

# fitting models by group (CtoP)
table_of_CP_year = filtered_flux_top %>%
  group_by(year) %>%
  filter(!is.na(TPP_Flux_mmolesP_m2_day), !is.na(POC_Flux_mmoles_m2_day)) %>%
  do(tidy(lm(POC_Flux_mmoles_m2_day ~ TPP_Flux_mmolesP_m2_day, data = .)))
# the result of this code is a table that provides the intercept, slope (ratio), and p-values per year 
table_of_CP_year

table_of_CP_slope <- filtered_flux_top %>%
  filter(!is.na(TPP_Flux_mmolesP_m2_day), !is.na(POC_Flux_mmoles_m2_day)) %>%
  group_by(year) %>%
  do(tidy(lm(POC_Flux_mmoles_m2_day ~ TPP_Flux_mmolesP_m2_day, data = .))) %>%
  ungroup() %>%
  filter(term == "TPP_Flux_mmolesP_m2_day") %>%
  select(year,
         slope = estimate,
         pval_slope = p.value,
         stderr_slope = std.error)
table_of_CP_slope

write.table(table_of_CP_slope, "clipboard", sep = "\t", row.names = FALSE)

# Significance of Slopes

# C:P Interaction Model
model_interaction_C_P = lm(POC_Flux_mmoles_m2_day ~ TPP_Flux_mmolesP_m2_day * factor(year), data = filtered_flux_top)
summary(model_interaction_C_P)

model_no_interaction_C_P = lm(POC_Flux_mmoles_m2_day ~ TPP_Flux_mmolesP_m2_day + factor(year), data = filtered_flux_top)

anova(model_no_interaction_C_P, model_interaction_C_P)
# justified to move on? yes!

# C:P Emmeans
# get estimated slopes of POC ~ PON for each year.
# Compare C:N ratios (as slopes) by year
emtrends_C_P = emtrends(model_interaction_C_P, specs = "year", var = "TPP_Flux_mmolesP_m2_day")


pairs(emtrends_C_P, adjust = "tukey")
# this line of code... 1) Compares the C:N slopes between all pairs of years, 2) Tells you whether the difference in slope between (e.g.) 2010 and 2015 is statistically significant, 3) Adjusts the p-values to avoid false positives due to multiple comparisons

pairs(emtrends_C_P, adjust = "tukey") %>%
  as.data.frame() %>%
  mutate(sig = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ ".",
    TRUE ~ ""
  )) %>%
  select(contrast, estimate, p.value, sig)

intercept_emmeans_C_P = emmeans(model_interaction_C_P, specs = "year")
pairs(intercept_emmeans_C_P, adjust = "tukey")  # pairwise intercept comparisons


# plots of the ratio in the form of a line.
# this adds another column of the C to P ratio.
filtered_flux_top = filtered_flux_top %>%
  mutate(C_to_P_ratio = POC_Flux_mmoles_m2_day / TPP_Flux_mmolesP_m2_day)

# this actually plots the line graph. 
CtoP_ratio_line = ggplot(filtered_flux_top, aes(x = Mid_Julian, y = C_to_P_ratio)) +
  geom_line() +
  geom_smooth(method = "loess") +
  labs(title = "SBB Top: C:P Flux Ratio Time Series",
       x = "Date", y = "C:P Ratio") +
  theme_minimal()
CtoP_ratio_line

# plots the ratio in the form of a boxplot
CtoP_ratio_boxplots = ggplot(filtered_flux_top, aes(x = as.factor(year), y = C_to_P_ratio)) +
  geom_boxplot(fill = "skyblue") +
  geom_hline(yintercept = 106, linetype = "dashed", color = "blue") +  # Redfield reference
  labs(title = "SBB Top: C:P Flux Ratio by Year Boxplots",
       x = "Year", y = "C:P Ratio") +
  theme_minimal()
CtoP_ratio_boxplots

##################################################

## PLOTS OF NITROGEN TO PHOSPHOROUS ##

# create a scatterplot with regression line and added slope line (16:1 = 16)
plot_NtoP_top = ggplot(filtered_flux_top, aes(x = TPP_Flux_mmolesP_m2_day, y = PON_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE)+
  geom_abline(slope = 16, intercept = 0, linetype = "dashed", color = "blue", linewidth = .7) +
  labs(x = "TPP Flux (mmmolP/m²/day)",
       y = "PON Flux (mmol/m²/day)",
       title = "SBB Top: Nitrogen vs. Phosphorous Flux") +
  theme_minimal()
plot_NtoP_top

# linear model of Nitrogen vs. Phosphorous
lm_NP_top = lm(PON_Flux_mmoles_m2_day ~ TPP_Flux_mmolesP_m2_day, data = filtered_flux_top)
summary(lm_NP_top)
# Ratio: 4.54656 (which is < than Redfield Ratio 16:1)
# adjusted r-squared: 0.5447
# p value = < 2.2e-16

# facet wrapped by year (NtoP)
NtoP_by_year = ggplot(filtered_flux_top, aes(x = TPP_Flux_mmolesP_m2_day, y = PON_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  geom_abline(slope = 16, intercept = 0, linetype = "dashed", color = "blue", linewidth = 0.7) +
  labs(x = "TPP Flux (mmolP/m²/day)",
       y = "PON Flux (mmol/m²/day)",
       title = "SBB Top Trap: Nitrogen vs. Phosphorous Flux by Year") +
  facet_wrap(~year) +
  theme_minimal()
NtoP_by_year

table_of_NP_year = filtered_flux_top %>%
  group_by(year) %>%
  filter(!is.na(TPP_Flux_mmolesP_m2_day), !is.na(PON_Flux_mmoles_m2_day)) %>%
  do(tidy(lm(PON_Flux_mmoles_m2_day ~ TPP_Flux_mmolesP_m2_day, data = .)))
# the result of this code is a table that provides the intercept, slope (ratio), and p-values per year 
table_of_NP_year

table_of_NP_slope <- filtered_flux_top %>%
  filter(!is.na(TPP_Flux_mmolesP_m2_day), !is.na(PON_Flux_mmoles_m2_day)) %>%
  group_by(year) %>%
  do(tidy(lm(PON_Flux_mmoles_m2_day ~ TPP_Flux_mmolesP_m2_day, data = .))) %>%
  ungroup() %>%
  filter(term == "TPP_Flux_mmolesP_m2_day") %>%
  select(year,
         slope = estimate,
         pval_slope = p.value,
         stderr_slope = std.error)
table_of_NP_slope

write.table(table_of_NP_slope, "clipboard", sep = "\t", row.names = FALSE)


# Significance of Slopes

# N:P Interaction Model
model_interaction_N_P = lm(PON_Flux_mmoles_m2_day ~ TPP_Flux_mmolesP_m2_day * factor(year), data = filtered_flux_top)
summary(model_interaction_N_P)

model_no_interaction_N_P = lm(PON_Flux_mmoles_m2_day ~ TPP_Flux_mmolesP_m2_day + factor(year), data = filtered_flux_top)

anova(model_no_interaction_N_P, model_interaction_N_P)
# justified to move on? yes!

# N:P Emmeans
# get estimated slopes of POC ~ PON for each year.
# Compare N:P ratios (as slopes) by year
emtrends_N_P = emtrends(model_interaction_N_P, specs = "year", var = "TPP_Flux_mmolesP_m2_day")


pairs(emtrends_N_P, adjust = "tukey")
# this line of code... 1) Compares the C:N slopes between all pairs of years, 2) Tells you whether the difference in slope between (e.g.) 2010 and 2015 is statistically significant, 3) Adjusts the p-values to avoid false positives due to multiple comparisons

pairs(emtrends_N_P, adjust = "tukey") %>%
  as.data.frame() %>%
  mutate(sig = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ ".",
    TRUE ~ ""
  )) %>%
  select(contrast, estimate, p.value, sig)

intercept_emmeans_N_P = emmeans(model_interaction_N_P, specs = "year")
pairs(intercept_emmeans_N_P, adjust = "tukey")  # pairwise intercept comparisons

# plots of the ratio in the form of a line.
# this adds another column of the C to P ratio.
filtered_flux_top = filtered_flux_top %>%
  mutate(N_to_P_ratio = PON_Flux_mmoles_m2_day / TPP_Flux_mmolesP_m2_day) 

# this actually plots the line graph. 
NtoP_ratio_line = ggplot(filtered_flux_top, aes(x = Mid_Julian, y = N_to_P_ratio)) +
  geom_line() +
  geom_smooth(method = "loess") +
  labs(title = "SBB Top: N:P Flux Ratio Time Series",
       x = "Date", y = "N:P Ratio") +
  theme_minimal()
NtoP_ratio_line

# plots the ratio in the form of a boxplot
NtoP_ratio_boxplots = ggplot(filtered_flux_top, aes(x = as.factor(year), y = N_to_P_ratio)) +
  geom_boxplot(fill = "skyblue") +
  geom_hline(yintercept = 16, linetype = "dashed", color = "blue") +  # Redfield reference
  labs(title = "SBB Top: N:P Flux Ratio by Year Boxplots",
       x = "Year", y = "N:P Ratio") +
  theme_minimal()
NtoP_ratio_boxplots

##############################

## PLOTS OF CARBON TO PHOSPHOROUS (PIP) ##

# create a scatterplot with regression line and added slope line (106:1 = 106)
plot_CtoPIP_top = ggplot(filtered_flux_top, aes(x = PIP_Flux_umolesP_m2_day / 1000, y = POC_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE)+
  geom_abline(slope = 106, intercept = 0, linetype = "dashed", color = "blue", linewidth = .7) +
  labs(x = "PIP Flux (mmolP/m²/day)",
       y = "POC Flux (mmol/m²/day)",
       title = "SBB Top: Carbon vs. PIP Flux") +
  theme_minimal()
plot_CtoPIP_top

# Now refit with converted P units

# this does the same as below (filters the data putting TPP into mmoles rather than umoles) but using tidy verse "mutate"
filtered_flux_top = filtered_flux_top %>%
  mutate(PIP_Flux_mmolesP_m2_day = PIP_Flux_umolesP_m2_day / 1000)

# this does the same as above but with base R and '$' rather than tidy verse. 
# filtered_flux_bot$TPP_Flux_mmolP_m2_day = filtered_flux_bot$TPP_Flux_umolesP_m2_day / 1000

# linear model of Carbon vs. Phosphorous (PIP) (data is filtered first, units are converted)
lm_C_PIP_top= lm(POC_Flux_mmoles_m2_day ~ PIP_Flux_mmolesP_m2_day , data = filtered_flux_top)
summary(lm_C_PIP_top)
# Ratio = 53.9167 (which is much < than Redfield Ratio of 106:1)
# adjusted r-squared: 0.6318
# p value = < 2.2e-16

# facet wrapped by year (CtoP)
CtoPIP_by_year = ggplot(filtered_flux_top, aes(x = PIP_Flux_mmolesP_m2_day, y = POC_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  geom_abline(slope = 106, intercept = 0, linetype = "dashed", color = "blue", linewidth = 0.7) +
  labs(x = "PIP Flux (mmolP/m²/day)",
       y = "POC Flux (mmol/m²/day)",
       title = "SBB Top Trap: Carbon vs. PIP Flux by Year") +
  facet_wrap(~year) +
  theme_minimal()
CtoPIP_by_year

# fitting models by group (CtoP)
table_of_C_PIP_year = filtered_flux_top %>%
  group_by(year) %>%
  filter(!is.na(PIP_Flux_mmolesP_m2_day), !is.na(POC_Flux_mmoles_m2_day)) %>%
  do(tidy(lm(POC_Flux_mmoles_m2_day ~ PIP_Flux_mmolesP_m2_day, data = .)))
# the result of this code is a table that provides the intercept, slope (ratio), and p-values per year 
table_of_C_PIP_year

table_of_C_PIP_slope <- filtered_flux_top %>%
  filter(!is.na(PIP_Flux_mmolesP_m2_day), !is.na(POC_Flux_mmoles_m2_day)) %>%
  group_by(year) %>%
  do(tidy(lm(POC_Flux_mmoles_m2_day ~ PIP_Flux_mmolesP_m2_day, data = .))) %>%
  ungroup() %>%
  filter(term == "PIP_Flux_mmolesP_m2_day") %>%
  select(year,
         slope = estimate,
         pval_slope = p.value,
         stderr_slope = std.error)
table_of_C_PIP_slope

write.table(table_of_C_PIP_slope, "clipboard", sep = "\t", row.names = FALSE)

# plots of the ratio in the form of a line.
# this adds another column of the C to P ratio.
filtered_flux_top = filtered_flux_top %>%
  mutate(C_to_PIP_ratio = POC_Flux_mmoles_m2_day / PIP_Flux_mmolesP_m2_day)

# this actually plots the line graph. 
CtoPIP_ratio_line = ggplot(filtered_flux_top, aes(x = Mid_Julian, y = C_to_PIP_ratio)) +
  geom_line() +
  geom_smooth(method = "loess") +
  labs(title = "SBB Top: C:PIP Flux Ratio Time Series",
       x = "Date", y = "C:PIP Ratio") +
  theme_minimal()
CtoPIP_ratio_line

# plots the ratio in the form of a boxplot
CtoPIP_ratio_boxplots = ggplot(filtered_flux_top, aes(x = as.factor(year), y = C_to_PIP_ratio)) +
  geom_boxplot(fill = "skyblue") +
  geom_hline(yintercept = 106, linetype = "dashed", color = "blue") +  # Redfield reference
  labs(title = "SBB Top: C:PIP Flux Ratio by Year Boxplots",
       x = "Year", y = "C:PIP Ratio") +
  theme_minimal()
CtoPIP_ratio_boxplots

###############################

## PLOTS OF CARBON TO PHOSPHOROUS (POP) ##

# create a scatterplot with regression line and added slope line (106:1 = 106)
plot_CtoPOP_top = ggplot(filtered_flux_top, aes(x = POP_Flux_umolesP_m2_day / 1000, y = POC_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE)+
  geom_abline(slope = 106, intercept = 0, linetype = "dashed", color = "blue", linewidth = .7) +
  labs(x = "POP Flux (mmolP/m²/day)",
       y = "POC Flux (mmol/m²/day)",
       title = "SBB Top: Carbon vs. POP Flux") +
  theme_minimal()
plot_CtoPOP_top

# Now refit with converted P units

# this does the same as below (filters the data putting TPP into mmoles rather than umoles) but using tidy verse "mutate"
filtered_flux_top = filtered_flux_top %>%
  mutate(POP_Flux_mmolesP_m2_day = POP_Flux_umolesP_m2_day / 1000)

# this does the same as above but with base R and '$' rather than tidy verse. 
# filtered_flux_bot$POP_Flux_mmolP_m2_day = filtered_flux_bot$POP_Flux_umolesP_m2_day / 1000

# linear model of Carbon vs. Phosphorous (POP) (data is filtered first, units are converted)
lm_C_POP_top= lm(POC_Flux_mmoles_m2_day ~ POP_Flux_mmolesP_m2_day , data = filtered_flux_top)
summary(lm_C_POP_top)
# Ratio = 28.8889 
# adjusted r-squared: 0.1138
# p value = < 2.2e-16

# facet wrapped by year (CtoP)
CtoPOP_by_year = ggplot(filtered_flux_top, aes(x = POP_Flux_mmolesP_m2_day, y = POC_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  geom_abline(slope = 106, intercept = 0, linetype = "dashed", color = "blue", linewidth = 0.7) +
  labs(x = "POP Flux (mmolP/m²/day)",
       y = "POC Flux (mmol/m²/day)",
       title = "SBB Top Trap: Carbon vs. POP Flux by Year") +
  facet_wrap(~year) +
  theme_minimal()
CtoPOP_by_year

# fitting models by group (CtoP)
table_of_C_POP_year = filtered_flux_top %>%
  group_by(year) %>%
  filter(!is.na(POP_Flux_mmolesP_m2_day), !is.na(POC_Flux_mmoles_m2_day)) %>%
  do(tidy(lm(POC_Flux_mmoles_m2_day ~ POP_Flux_mmolesP_m2_day, data = .)))
# the result of this code is a table that provides the intercept, slope (ratio), and p-values per year 
table_of_C_POP_year

table_of_C_POP_slope <- filtered_flux_top %>%
  filter(!is.na(POP_Flux_mmolesP_m2_day), !is.na(POC_Flux_mmoles_m2_day)) %>%
  group_by(year) %>%
  do(tidy(lm(POC_Flux_mmoles_m2_day ~ POP_Flux_mmolesP_m2_day, data = .))) %>%
  ungroup() %>%
  filter(term == "POP_Flux_mmolesP_m2_day") %>%
  select(year,
         slope = estimate,
         pval_slope = p.value,
         stderr_slope = std.error)
table_of_C_POP_slope

write.table(table_of_C_POP_slope, "clipboard", sep = "\t", row.names = FALSE)

# plots of the ratio in the form of a line.
# this adds another column of the C to POP ratio.
filtered_flux_top = filtered_flux_top %>%
  mutate(C_to_POP_ratio = POC_Flux_mmoles_m2_day / POP_Flux_mmolesP_m2_day)

# this actually plots the line graph. 
CtoPOP_ratio_line = ggplot(filtered_flux_top, aes(x = Mid_Julian, y = C_to_POP_ratio)) +
  geom_line() +
  geom_smooth(method = "loess") +
  labs(title = "SBB Top: C:POP Flux Ratio Time Series",
       x = "Date", y = "C:POP Ratio") +
  theme_minimal()
CtoPOP_ratio_line

# plots the ratio in the form of a boxplot
CtoPOP_ratio_boxplots = ggplot(filtered_flux_top, aes(x = as.factor(year), y = C_to_POP_ratio)) +
  geom_boxplot(fill = "skyblue") +
  geom_hline(yintercept = 106, linetype = "dashed", color = "blue") +  # Redfield reference
  labs(title = "SBB Top: C:POP Flux Ratio by Year Boxplots",
       x = "Year", y = "C:POP Ratio") +
  theme_minimal()
CtoPOP_ratio_boxplots

## PLOTS OF NITROGEN TO PHOSPHOROUS (PIP) ##

# create a scatterplot with regression line and added slope line (16:1 = 16)
plot_NtoPIP_top = ggplot(filtered_flux_top, aes(x = PIP_Flux_mmolesP_m2_day, y = PON_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE)+
  geom_abline(slope = 16, intercept = 0, linetype = "dashed", color = "blue", linewidth = .7) +
  labs(x = "PIP Flux (mmmolP/m²/day)",
       y = "PON Flux (mmol/m²/day)",
       title = "SBB Top: Nitrogen vs. PIP Flux") +
  theme_minimal()
plot_NtoPIP_top

# linear model of Nitrogen vs. Phosphorous
lm_N_PIP_top = lm(PON_Flux_mmoles_m2_day ~ PIP_Flux_mmolesP_m2_day, data = filtered_flux_top)
summary(lm_N_PIP_top)
# Ratio: 6.66542 (which is < than Redfield Ratio 16:1)
# adjusted r-squared: 0.6089
# p value = < 2.2e-16

# facet wrapped by year (NtoP)
NtoPIP_by_year = ggplot(filtered_flux_top, aes(x = PIP_Flux_mmolesP_m2_day, y = PON_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  geom_abline(slope = 16, intercept = 0, linetype = "dashed", color = "blue", linewidth = 0.7) +
  labs(x = "PIP Flux (mmolP/m²/day)",
       y = "PON Flux (mmol/m²/day)",
       title = "SBB Top Trap: Nitrogen vs. PIP Flux by Year") +
  facet_wrap(~year) +
  theme_minimal()
NtoPIP_by_year

table_of_N_PIP_year = filtered_flux_top %>%
  group_by(year) %>%
  filter(!is.na(PIP_Flux_mmolesP_m2_day), !is.na(PON_Flux_mmoles_m2_day)) %>%
  do(tidy(lm(PON_Flux_mmoles_m2_day ~ PIP_Flux_mmolesP_m2_day, data = .)))
# the result of this code is a table that provides the intercept, slope (ratio), and p-values per year 
table_of_N_PIP_year

table_of_N_PIP_slope <- filtered_flux_top %>%
  filter(!is.na(PIP_Flux_mmolesP_m2_day), !is.na(PON_Flux_mmoles_m2_day)) %>%
  group_by(year) %>%
  do(tidy(lm(PON_Flux_mmoles_m2_day ~ PIP_Flux_mmolesP_m2_day, data = .))) %>%
  ungroup() %>%
  filter(term == "PIP_Flux_mmolesP_m2_day") %>%
  select(year,
         slope = estimate,
         pval_slope = p.value,
         stderr_slope = std.error)
table_of_N_PIP_slope

write.table(table_of_N_PIP_slope, "clipboard", sep = "\t", row.names = FALSE)

# plots of the ratio in the form of a line.
# this adds another column of the C to P ratio.
filtered_flux_top = filtered_flux_top %>%
  mutate(N_to_PIP_ratio = PON_Flux_mmoles_m2_day / PIP_Flux_mmolesP_m2_day) 

# this actually plots the line graph. 
NtoPIP_ratio_line = ggplot(filtered_flux_top, aes(x = Mid_Julian, y = N_to_PIP_ratio)) +
  geom_line() +
  geom_smooth(method = "loess") +
  labs(title = "SBB Top: N:PIP Flux Ratio Time Series",
       x = "Date", y = "N:PIP Ratio") +
  theme_minimal()
NtoPIP_ratio_line

# plots the ratio in the form of a boxplot
NtoPIP_ratio_boxplots = ggplot(filtered_flux_top, aes(x = as.factor(year), y = N_to_PIP_ratio)) +
  geom_boxplot(fill = "skyblue") +
  geom_hline(yintercept = 16, linetype = "dashed", color = "blue") +  # Redfield reference
  labs(title = "SBB Top: N:PIP Flux Ratio by Year Boxplots",
       x = "Year", y = "N:PIP Ratio") +
  theme_minimal()
NtoPIP_ratio_boxplots


## PLOTS OF NITROGEN TO PHOSPHOROUS (POP) ##

# create a scatterplot with regression line and added slope line (16:1 = 16)
plot_NtoPOP_top = ggplot(filtered_flux_top, aes(x = POP_Flux_mmolesP_m2_day, y = PON_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE)+
  geom_abline(slope = 16, intercept = 0, linetype = "dashed", color = "blue", linewidth = .7) +
  labs(x = "POP Flux (mmmolP/m²/day)",
       y = "PON Flux (mmol/m²/day)",
       title = "SBB Top: Nitrogen vs. POP Flux") +
  theme_minimal()
plot_NtoPOP_top

# linear model of Nitrogen vs. POP
lm_N_POP_top = lm(PON_Flux_mmoles_m2_day ~ POP_Flux_mmolesP_m2_day, data = filtered_flux_top)
summary(lm_N_POP_top)
# Ratio: 3.8907 (which is < than Redfield Ratio 16:1)
# adjusted r-squared: 0.1273
# p value = < 2.2e-16

# facet wrapped by year (NtoP)
NtoPOP_by_year = ggplot(filtered_flux_top, aes(x = POP_Flux_mmolesP_m2_day, y = PON_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  geom_abline(slope = 16, intercept = 0, linetype = "dashed", color = "blue", linewidth = 0.7) +
  labs(x = "POP Flux (mmolP/m²/day)",
       y = "PON Flux (mmol/m²/day)",
       title = "SBB Top Trap: Nitrogen vs. POP Flux by Year") +
  facet_wrap(~year) +
  theme_minimal()
NtoPOP_by_year

table_of_N_POP_year = filtered_flux_top %>%
  group_by(year) %>%
  filter(!is.na(POP_Flux_mmolesP_m2_day), !is.na(PON_Flux_mmoles_m2_day)) %>%
  do(tidy(lm(PON_Flux_mmoles_m2_day ~ POP_Flux_mmolesP_m2_day, data = .)))
# the result of this code is a table that provides the intercept, slope (ratio), and p-values per year 
table_of_N_POP_year

table_of_N_POP_slope <- filtered_flux_top %>%
  filter(!is.na(POP_Flux_mmolesP_m2_day), !is.na(PON_Flux_mmoles_m2_day)) %>%
  group_by(year) %>%
  do(tidy(lm(PON_Flux_mmoles_m2_day ~ POP_Flux_mmolesP_m2_day, data = .))) %>%
  ungroup() %>%
  filter(term == "POP_Flux_mmolesP_m2_day") %>%
  select(year,
         slope = estimate,
         pval_slope = p.value,
         stderr_slope = std.error)
table_of_N_POP_slope

write.table(table_of_N_POP_slope, "clipboard", sep = "\t", row.names = FALSE)

# plots of the ratio in the form of a line.
# this adds another column of the C to P ratio.
filtered_flux_top = filtered_flux_top %>%
  mutate(N_to_POP_ratio = PON_Flux_mmoles_m2_day / POP_Flux_mmolesP_m2_day) 

# this actually plots the line graph. 
NtoPOP_ratio_line = ggplot(filtered_flux_top, aes(x = Mid_Julian, y = N_to_POP_ratio)) +
  geom_line() +
  geom_smooth(method = "loess") +
  labs(title = "SBB Top: N:POP Flux Ratio Time Series",
       x = "Date", y = "N:POP Ratio") +
  theme_minimal()
NtoPOP_ratio_line

# plots the ratio in the form of a boxplot
NtoPOP_ratio_boxplots = ggplot(filtered_flux_top, aes(x = as.factor(year), y = N_to_POP_ratio)) +
  geom_boxplot(fill = "skyblue") +
  geom_hline(yintercept = 16, linetype = "dashed", color = "blue") +  # Redfield reference
  labs(title = "SBB Top: N:POP Flux Ratio by Year Boxplots",
       x = "Year", y = "N:POP Ratio") +
  theme_minimal()
NtoPOP_ratio_boxplots


###################################
###################################

# Overall, this code helps us pull out specific outliers and puts them into a table so that we can then identify outliers. 

## STEP 1: Define which columns we want to analyze. Creating a vector of the column names I want to check for outliers. This avoids hardcoding them repeatedly
flux_variables = c(
  "Total_Mass_Flux_g_m2_day",
  "Terrigenous_Flux_g_m2_day",
  "PON_Flux_mmoles_m2_day", 
  "POC_Flux_mmoles_m2_day",
  "CaCO3_Flux_mmoles_m2_day",
  "Opal_Flux_mmoles_m2_day",
  "TPP_Flux_umolesP_m2_day",
  "PIP_Flux_umolesP_m2_day",
  "POP_Flux_umolesP_m2_day")

# STEP 2: Copy original data
# a working copy of the original dataframe, sbb_top_df, so it is not accidentally overwriten or modified during processing.
all_data_outliers = sbb_top_df

# STEP 3: This makes an empty data frame where we can store the outlier results (T/F).
outlier_flags = data.frame(Mid_Julian = all_data_outliers$Mid_Julian)
# Step 3a: This makes an empty data frame where we can store the outlier types (high/low) or NA if it is not an outlier.
outlier_direction_flags = data.frame(Mid_Julian = all_data_outliers$Mid_Julian)

# STEP 4: This code loops through each variable in flux variables to calculate the IQR based outliers. 
# First it calculates the IQR for each variable, then defines what the cutoff is for the outlier. it will be flagged and stored.
for (var in flux_variables) {
  # 4a check if the variable exists
  if (var %in% names(all_data_outliers)) {
    # 4b. Calculate IQR components
    Q1 <- quantile(all_data_outliers[[var]], 0.25, na.rm = TRUE)
    Q3 <- quantile(all_data_outliers[[var]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    # 4c. Define bounds for outliers
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    # 4d. Flag outliers (TRUE/FALSE)
    values <- all_data_outliers[[var]]
    is_outlier <- values < lower_bound | values > upper_bound
    # Save TRUE/FALSE flags
    outlier_flags[[paste0(var, "_outlier")]] <- is_outlier
    # Save direction of outliers (only where TRUE)
    outlier_type <- ifelse(values < lower_bound, "low",
                           ifelse(values > upper_bound, "high", NA))
    outlier_direction_flags[[paste0(var, "_outlier_type")]] <- outlier_type
  } else {
    warning(paste("Variable", var, "not found in the dataframe"))
  }
}


#  STEP 5. Pivot the wide-format data to a long format. (excluding the date column)
# this takes the selected flux columns and "stacks" them into two columns 1) variable: the name of the flux/column name and 2) value: the actual number for that variable

flux_long = all_data_outliers %>%
  pivot_longer(
    cols = c( "Total_Mass_Flux_g_m2_day",
              "Terrigenous_Flux_g_m2_day",
              "PON_Flux_mmoles_m2_day", 
              "POC_Flux_mmoles_m2_day",
              "CaCO3_Flux_mmoles_m2_day",
              "Opal_Flux_mmoles_m2_day",
              "TPP_Flux_umolesP_m2_day",
              "PIP_Flux_umolesP_m2_day",
              "POP_Flux_umolesP_m2_day"),
    names_to = "variable",
    values_to = "value"
  )

# STEP 6: Pivot the outlier_flags frame (excluding the date)
# this turns the wide-format outlier flag data frame into two columns 1) variable 2) is_outlier (t/f). 
# the sub("_outlier", "", variable) removes "_outlier suffix so it matches the original flux variable names in flux_long. 
outlier_flags_long <- outlier_flags %>%
  pivot_longer(
    cols = c( "Total_Mass_Flux_g_m2_day_outlier",
              "Terrigenous_Flux_g_m2_day_outlier",
              "PON_Flux_mmoles_m2_day_outlier", 
              "POC_Flux_mmoles_m2_day_outlier",
              "CaCO3_Flux_mmoles_m2_day_outlier",
              "Opal_Flux_mmoles_m2_day_outlier",
              "TPP_Flux_umolesP_m2_day_outlier",
              "PIP_Flux_umolesP_m2_day_outlier",
              "POP_Flux_umolesP_m2_day_outlier"),
    names_to = "variable",
    values_to = "is_outlier"
  ) %>%
  # Remove the "_outlier" suffix to match variable names
  mutate(variable = sub("_outlier", "", variable))

# STEP 6B: This does the same thing but for the outlier_type_long determining the variable to be the type of flux and the values to be the type of outlier (high/low)
outlier_type_long <- outlier_direction_flags %>%
  pivot_longer(
    cols = -Mid_Julian,
    names_to = "variable",
    values_to = "outlier_type"
  ) %>%
  mutate(variable = sub("_outlier_type", "", variable))


# STEP 7. Join the long data + flags
# this combines the flux_long data frame (with the actual values) and the outlier_flags_long data frame (with the t/f) by the common factors of Mid_Julian and variable. 
# the result should be one long table with... 1) Mid_Julian, 2) variable, 3) value, is_outlier.

flux_outlier_joined <- flux_long %>%
  left_join(outlier_flags_long, by = c("Mid_Julian", "variable")) %>%
  left_join(outlier_type_long, by = c("Mid_Julian", "variable"))

# STEP 8: Filter to keep only the actual outlier values
# this filters the rows to only show when is_outlier == TRUE and sorts the result by variable and date. 
outlier_table <- flux_outlier_joined %>%
  select(Sample_ID, Mid_Julian, variable, value, is_outlier, outlier_type) %>%
  filter(is_outlier == TRUE) %>%
  arrange(variable, Mid_Julian)

write.table(outlier_table, "clipboard", sep = "\t", row.names = FALSE)

# Summary: 
# pivot_longer()- reshapes the wide columns into a long format
# sub("_outlier","", ...)- aligns the variable names for joining.
# left_join()- combines the values and flags data set into one table
# filter(is_outlier == TRUE)- keep just the rows 

# the final table shows... 1) which sample was an outlier, 2) on what date, 3) for what variable, 4) with what value, 5) and whether it was a high or low outlier.\

#################

## Now pull out monthly outliers rather than just "global" outliers.

# Creates new empty monthly outlier data frames
outlier_flags_month <- data.frame(Mid_Julian = all_data_outliers$Mid_Julian)
outlier_type_flags_month <- data.frame(Mid_Julian = all_data_outliers$Mid_Julian)

for (var in flux_variables) {
  if (var %in% names(all_data_outliers)) {
    outlier_flags_month[[paste0(var, "_outlier_month")]] <- NA
    outlier_type_flags_month[[paste0(var, "_outlier_type_month")]] <- NA
    
    for (m in unique(all_data_outliers$Month)) {
      month_rows <- all_data_outliers$Month == m
      values <- all_data_outliers[[var]][month_rows]
      
      if (all(is.na(values))) next
      
      Q1 <- quantile(values, 0.25, na.rm = TRUE)
      Q3 <- quantile(values, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower <- Q1 - 1.5 * IQR
      upper <- Q3 + 1.5 * IQR
      
      is_outlier_month <- values < lower | values > upper
      outlier_type_month <- ifelse(values < lower, "low",
                                   ifelse(values > upper, "high", NA))
      
      outlier_flags_month[[paste0(var, "_outlier_month")]][month_rows] <- is_outlier_month
      outlier_type_flags_month[[paste0(var, "_outlier_type_month")]][month_rows] <- outlier_type_month
    }
  }
}

# Pivots the month outlier flags: 


##############################################

### I don't think any of this works, right below ###

## Experimenting with Time Series of Total Mass Flux ##

# Experimenting with comparing Monthly vs. Yearly Trends in One Plot
