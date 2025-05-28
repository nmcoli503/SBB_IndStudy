### SBB Ind Study Project ###
## Fall 2024 - Summer 2025 ##

#Santa Barbara Basin !TOP TRAP!

# this here takes the data from the excel (we converted it to a csv file) and reads it as a csv and creates our data frame. This is the data frame we will be using throughout the rest of this  r script. I titled it "sbb_bot_boxplots" because it's from the SBB project, is focusing on the bottom trap, and then df stands for data frame. 
sbb_top_df = read.csv(("data/CSV_files/SBB_Top_Boxplots_moles.csv"), header = TRUE, sep = ",")
# this makes it show up in the console.
sbb_top_df 

######

# here i loaded all my libraries. previously these have been downloaded and installed which is what i only loaded them here. 

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
sbb_top_df$Date_Open = as.Date(sbb_top_df$Date_Open)

# here i extract all the ~months~ from the Date_Open column (as a two digit string) and store it in a new column called month.
sbb_top_df$month = format(sbb_top_df$Date_Open,"%m")

# here i extract all the ~years~ from the Date_Open column (as a two digit string) and store it in a new column called year.
# use a capital Y to convert to a 4 digit year rather than just a 2 digit year
sbb_top_df$year = format(sbb_top_df$Date_Open,"%y")


#this line removes missing dates: if rows with missing dates aren't essential, you can remove them with filter (), 
#This code removes all rows from sbb_top_df where the Date_Open column is NA (missing), ensuring that the dataset contains only rows with valid dates.
# is.na(Date_Open) is TRUE for rows where Date_Open is missing. ! means "not", so this keeps rows where Date_Open is NOT missing.

# note: the rows went from 353 to 333 meaning 20 rows did not have a date filled out. (this is likely because we have traps where the date got skipped because of clogs and such or because an extra row was added for visual clarify).
sbb_top_df = sbb_top_df %>% filter(!is.na(Date_Open))

# i just wrote these next three lines to call the information so I could visualize date open, month and year in the console.
sbb_top_df$Date_Open
sbb_top_df$month
sbb_top_df$year

######

# this code calculates the average total mass flux for each month from the sbb_bot_df dataset (ignoring missing values) and then stores the result in a new data frame called total_massflux_monthlyavg_sbb_bot.

# %>% is a pipe operator form the dplyr package that passes the dataframe sbb_top_boxplots to the next part of the code. 

# group_by(month) groups the data in the sbb_top_boxplots dataframe and groups it based on month. 

# summarise creates a summary statistic for each group. in this case we calcualte a new column in this example called "Avg_Total_Flux_sbb_top to hold the average of the Total_Mass_Flux_g_m2_day values. 

# finally mean() computes the averages of the Total_Mass_Flux_g_m2_day values for each month and na.omit omits missing values so it doesn't affect the mean calculation. 

# the successive lines do the same thing for each constituient. 1) total mass flux, 2) terig flux, 3) PON flux, 4) POC flux, 5) CaCO3 flux, 6) Opal flux, 7) TPP flux, 8) PIP flux, 9) POP flux. After running this section there should be 9 data frames each with two columns --> one column is labelled 1-12 for each month of the year, and the second column is the averaged value of each month throughout the entire time frame of the dataset (all 32 years).

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
  summarise (Avg_Opal_Flux_mmoles_m2_day_sbb_top = mean(na.omit(OPAL_Flux_mmoles_m2_day)))

TPPflux_monthlyavg_sbb_top = sbb_top_df%>% 
  group_by(month) %>% 
  summarise (Avg_TPP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(TPP_Flux_umolesP_m2_day)))

PIPflux_monthlyavg_sbb_top = sbb_top_df%>% 
  group_by(month) %>% 
  summarise (Avg_PIP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(PIP_Flux_umolesP_m2_day)))

POPflux_monthlyavg_sbb_top = sbb_top_df%>% 
  group_by(month) %>% 
  summarise (Avg_POP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(POP_Flux_umolesP_m2_day)))

######

# this is the same but groups by year to find YEARLY average rather than monthly. 

# this code calculates the average total mass flux for each year from the sbb_bot_df dataset (ignoring missing values) and then stores the result in a new data frame called totalmassflux_yearlyavg_sbb_bot.

# the successive lines do the same thing for each constituient. 1) total mass flux, 2) terig flux, 3) PON flux, 4) POC flux, 5) CaCO3 flux, 6) Opal flux, 7) TPP flux, 8) PIP flux, 9) POP flux. After running this section there should be 9 data frames each with two columns --> one column is labelled a year (1993-2024) for each year in the 32 year dataset, and the second column is the averaged value of each year (incorporating all the months from that entire year).

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
  summarise (Avg_Opal_Flux_mmoles_m2_day_sbb_top = mean(na.omit(OPAL_Flux_mmoles_m2_day)))

TPPflux_yearlyavg_sbb_top = sbb_top_df%>% 
  group_by(year) %>% 
  summarise (Avg_TPP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(TPP_Flux_umolesP_m2_day)))

PIPflux_yearlyavg_sbb_top = sbb_top_df%>% 
  group_by(year) %>% 
  summarise (Avg_PIP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(PIP_Flux_umolesP_m2_day)))

POPflux_yearlyavg_sbb_top = sbb_top_df%>% 
  group_by(year) %>% 
  summarise (Avg_POP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(POP_Flux_umolesP_m2_day)))

######

#str(sbb_top_df)
#class(month)

# this code begins to plot it all. it creates a boxplot of Total_Mass_Flux_g_m2_day by month using the sbb_bot_df dataset. It also adds the monthly mean as red dots and error bars showing confidence intervals around those means. The plot is styled with labels and a formatted y-axis. a-i represents the first set of 9 constituents for the monthly graphs. 

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

plot_f = ggplot(sbb_top_df, aes(x = month, y = OPAL_Flux_mmoles_m2_day)) +
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

######

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

plot_o = ggplot(sbb_top_df, aes(x = year, y = OPAL_Flux_mmoles_m2_day)) +
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
ggsave("figures/SBBTop_Yearly_Total_Terrig.pdf", plot = plot_j_k, width = 10, height = 8)
ggsave("figures/SBBTop_Yearly_PON_POC.pdf", plot = plot_l_m, width = 10, height = 8)
ggsave("figures/SBBTop_Yearly_Carbonate_Opal.pdf", plot = plot_n_o, width = 10, height = 8)
ggsave("figures/SBBTop_Yearly_TPP_PIP.pdf", plot = plot_p_q, width = 10, height = 8)
ggsave("figures/SBBTop_Yearly_POP.pdf", plot = plot_r, width = 10, height = 8)

######

## CNP Ratios ##

# filters out rows with missing values. keeps only rows where POC, PON, and TPP flux values are present (i.e., not NA). stores the filtered result in filtered_flux_bot data frame.
filtered_flux_top = sbb_top_df %>%
  filter(!is.na(POC_Flux_mmoles_m2_day),
         !is.na(PON_Flux_mmoles_m2_day),
         !is.na(TPP_Flux_umolesP_m2_day))
###

# Plots of Carbon to Nitrogen
# create a scatterplot with regression line and added slope line (106:16 = 6.6)
plot_CtoN_top = ggplot(filtered_flux_top, aes(x = PON_Flux_mmoles_m2_day, y = POC_Flux_mmoles_m2_day )) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE)+
  geom_abline(slope = 6.6, intercept = 0, linetype = "dashed", color = "blue", linewidth = .7) +
  labs(x = "PON Flux (mmol/m²/day)",
       y = "POC Flux (mmol/m²/day)",
       title = "Carbon vs. Nitrogen Flux") +
  theme_minimal()
plot_CtoN_top

# linear model of Carbon vs. Nitrogen
lm_CN_top = lm(POC_Flux_mmoles_m2_day ~ PON_Flux_mmoles_m2_day, data = filtered_flux_top)
summary(lm_CN_top)

# ratio = 7.2432 (slightly > than Redfield Ratio of 6.6)

# facet wrapped by year (CtoN)
CtoN_by_year = ggplot(filtered_flux_top, aes(x = PON_Flux_mmoles_m2_day, y = POC_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  geom_abline(slope = 6.6, intercept = 0, linetype = "dashed", color = "blue", linewidth = 0.7) +
  labs(x = "PON Flux (mmol/m²/day)",
       y = "POC Flux (mmol/m²/day)",
       title = "SBB Top Trap: Carbon vs. Nitrogen Flux by Year") +
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

# plots of the ratio in the form of a line.
# this adds another column of the C to N ratio.
filtered_flux_top = filtered_flux_top %>%
  mutate(C_to_N_ratio = POC_Flux_mmoles_m2_day / PON_Flux_mmoles_m2_day)

# this actually plots the line graph. 
CtoN_ratio_line = ggplot(filtered_flux_top, aes(x = Date_Open, y = C_to_N_ratio)) +
  geom_line() +
  geom_smooth(method = "loess") +
  labs(title = "Time Series of C:N Flux Ratio",
       x = "Date", y = "C:N Ratio") +
  theme_minimal()
CtoN_ratio_line

# plots the ratio in the form of a boxplot
CtoN_ratio_boxplots = ggplot(filtered_flux_top, aes(x = as.factor(year), y = C_to_N_ratio)) +
  geom_boxplot(fill = "skyblue") +
  geom_hline(yintercept = 6.6, linetype = "dashed", color = "blue") +  # Redfield reference
  labs(title = "C:N Flux Ratio by Year",
       x = "Year", y = "C:N Ratio") +
  theme_minimal()
CtoN_ratio_boxplots

###

# Plots of Carbon to Phosphorous
# create a scatterplot with regression line and added slope line (106:1 = 106)
plot_CtoP_top = ggplot(filtered_flux_top, aes(x = TPP_Flux_umolesP_m2_day / 1000, y = POC_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE)+
  geom_abline(slope = 106, intercept = 0, linetype = "dashed", color = "blue", linewidth = .7) +
  labs(x = "TPP Flux (mmolP/m²/day)",
       y = "POC Flux (mmol/m²/day)",
       title = "Carbon vs. Phosphorous Flux") +
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
# Ratio = 36.1751 (which is much < than Redfield Ratio of 106:1)

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

# plots of the ratio in the form of a line.
# this adds another column of the C to P ratio.
filtered_flux_top = filtered_flux_top %>%
  mutate(C_to_P_ratio = POC_Flux_mmoles_m2_day / TPP_Flux_mmolesP_m2_day)

# this actually plots the line graph. 
CtoP_ratio_line = ggplot(filtered_flux_top, aes(x = Date_Open, y = C_to_P_ratio)) +
  geom_line() +
  geom_smooth(method = "loess") +
  labs(title = "Time Series of C:P Flux Ratio",
       x = "Date", y = "C:P Ratio") +
  theme_minimal()
CtoP_ratio_line

# plots the ratio in the form of a boxplot
CtoP_ratio_boxplots = ggplot(filtered_flux_top, aes(x = as.factor(year), y = C_to_P_ratio)) +
  geom_boxplot(fill = "skyblue") +
  geom_hline(yintercept = 106, linetype = "dashed", color = "blue") +  # Redfield reference
  labs(title = "C:P Flux Ratio by Year",
       x = "Year", y = "C:P Ratio") +
  theme_minimal()
CtoP_ratio_boxplots

##################################################

# Plot of Nitrogen to Phosphorous
# create a scatterplot with regression line and added slope line (16:1 = 16)
plot_NtoP_top = ggplot(filtered_flux_top, aes(x = TPP_Flux_mmolesP_m2_day, y = PON_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE)+
  geom_abline(slope = 16, intercept = 0, linetype = "dashed", color = "blue", linewidth = .7) +
  labs(x = "TPP Flux (mmmolP/m²/day)",
       y = "PON Flux (mmol/m²/day)",
       title = "Nitrogen vs. Phosphorous Flux") +
  theme_minimal()
plot_NtoP_top

# linear model of Nitrogen vs. Phosphorous
lm_NP_top = lm(PON_Flux_mmoles_m2_day ~ TPP_Flux_mmolesP_m2_day, data = filtered_flux_top)
summary(lm_NP_top)
# Ratio: 4.31034 (which is < than Redfield Ratio 16:1)

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

# plots of the ratio in the form of a line.
# this adds another column of the C to P ratio.
filtered_flux_top = filtered_flux_top %>%
  mutate(N_to_P_ratio = PON_Flux_mmoles_m2_day / TPP_Flux_mmolesP_m2_day) 

# this actually plots the line graph. 
NtoP_ratio_line = ggplot(filtered_flux_top, aes(x = Date_Open, y = N_to_P_ratio)) +
  geom_line() +
  geom_smooth(method = "loess") +
  labs(title = "Time Series of N:P Flux Ratio",
       x = "Date", y = "N:P Ratio") +
  theme_minimal()
NtoP_ratio_line

# plots the ratio in the form of a boxplot
NtoP_ratio_boxplots = ggplot(filtered_flux_top, aes(x = as.factor(year), y = N_to_P_ratio)) +
  geom_boxplot(fill = "skyblue") +
  geom_hline(yintercept = 16, linetype = "dashed", color = "blue") +  # Redfield reference
  labs(title = "N:P Flux Ratio by Year",
       x = "Year", y = "N:P Ratio") +
  theme_minimal()
NtoP_ratio_boxplots

##########

### I don't think any of this works, right above or below ###

## Experimenting with Time Series of Total Mass Flux ##

#library(ggplot2)
#library(dplyr)

# Identify outliers using IQR method
sbb_top_boxplots = sbb_top_df %>%
  mutate(Q1 = quantile(Total_Mass_Flux_g_m2_day, 0.25, na.rm = TRUE),
         Q3 = quantile(Total_Mass_Flux_g_m2_day, 0.75, na.rm = TRUE),
         IQR = Q3 - Q1,
         is_outlier = Total_Mass_Flux_g_m2_day < (Q1 - 1.5 * IQR) |
           Total_Mass_Flux_g_m2_day > (Q3 + 1.5 * IQR))

# Create the time series plot
plot_mass_flux_outlier = ggplot(sbb_top_df, aes(x = Date_Open, y = Total_Mass_Flux_g_m2_day)) +
  geom_line(color = "blue", na.rm = TRUE,) +  # Trend line
  geom_point(data = filter(sbb_top_df, is_outlier), aes(x = Date_Open, y = Total_Mass_Flux_g_m2_day), color = "red", size = 2) +  # Highlight outliers
  labs(title = "Time Series of Total Mass Flux with Outliers",
       x = "Date", y = "Total Mass Flux (g/m²/day)") +
  theme_minimal()

plot_mass_flux_outlier

######

# Experimenting with comparing Monthly vs. Yearly Trends in One Plot

sbb_top_boxplots = sbb_top_df %>%
  mutate(TimePeriod = factor(ifelse(!is.na(month), paste("Month:", month), paste("Year:", year)), levels = unique(c(paste("Month:", 1:12), paste("Year:", unique(year))))))

# Combined boxplot for Monthly and Yearly Trends
plot_combined_boxplot = ggplot(sbb_top_df, aes(x = TimePeriod, y = Total_Mass_Flux_g_m2_day, fill = TimePeriod)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  facet_wrap(~Constituent, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Comparison of Monthly vs. Yearly Trends",
       x = "Time Period", y = "Total Mass Flux (g/m²/day)") +
  theme_minimal()

print(plot_combined_boxplot)