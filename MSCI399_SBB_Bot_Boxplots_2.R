#path: C:/Users/ninac/OneDrive - University of South Carolina/Documents/R Studio Documents/MSCI399/MSCI399_SBB_Bot_Boxplots_2.R

sbb_bot_boxplots = read.csv(("data/CSV_files/SBB_Bot_Boxplots_moles.csv"), header = TRUE, sep = ",")
sbb_bot_boxplots  


library(ggplot2)
library(lubridate)
library(tidyverse)
library(carData)
library(gridBase)
library(gridExtra)
library(patchwork)
library (dplyr)

sbb_bot_boxplots$Date_Open = as.Date(sbb_bot_boxplots$Date_Open)

#extracts all the months from the Date_Open and stores it in a new column called month
sbb_bot_boxplots$month = format(sbb_bot_boxplots$Date_Open,"%m")

#extracts the year from the Date_Open column and stores it in a new column called year.
sbb_bot_boxplots$year = format(sbb_bot_boxplots$Date_Open,"%Y")

#this line removes missing dates: if rows with missing dates aren't essential, you can remove them with filter (), 
#This code removes all rows from sbb_bot where the Date_Open column has missing data
sbb_bot_boxplots = sbb_bot_boxplots %>% filter(!is.na(Date_Open))

#I just wrote these next three lines to call the information so I could visualize it.
sbb_bot_boxplots$Date_Open
sbb_bot_boxplots$month
sbb_bot_boxplots$year

#sbb_bot$year <- as.numeric(sbb_bot$year)
#sbb_bot$year_full <- ifelse(sbb_bot$year < 50, 
# paste0("20", sprintf("%02d", sbb_bot$year)), 
# paste0("19", sprintf("%02d", sbb_bot$year)))
#sbb_bot$year_full <- as.Date(sbb_bot$year_full, format = "%Y")

#################

massflux_monthlyavg_sbb_bot = sbb_bot_boxplots %>% 
  group_by(month) %>% 
  summarise (Avg_Total_Mass_Flux_g_m2_day_sbb_bot = mean(na.omit(Total_Mass_Flux_g_m2_day)))

terigflux_monthlyavg_sbb_bot = sbb_bot_boxplots %>% 
  group_by(month) %>% 
  summarise (Avg_Terrig_Flux_g_m2_day_sbb_bot = mean(na.omit(Terrigenous_Flux_g_m2_day)))

PONflux_monthlyavg_sbb_bot = sbb_bot_boxplots %>% 
  group_by(month) %>% 
  summarise (Avg_PON_Flux_mmoles_m2_day_sbb_bot = mean(na.omit(PON_Flux_mmoles_m2_day)))

POCflux_monthlyavg_sbb_bot = sbb_bot_boxplots %>% 
  group_by(month) %>% 
  summarise (Avg_POC_Flux_mmoles_m2_day_sbb_bot = mean(na.omit(POC_Flux_mmoles_m2_day)))

CaCO3flux_monthlyavg_sbb_bot = sbb_bot_boxplots %>% 
  group_by(month) %>% 
  summarise (Avg_CaCO3_Flux_mmoles_m2_day_sbb_bot = mean(na.omit(CaCO3_Flux_mmoles_m2_day)))

Opalflux_monthlyavg_sbb_bot = sbb_bot_boxplots %>% 
  group_by(month) %>% 
  summarise (Avg_Opal_Flux_mmoles_m2_day_sbb_bot = mean(na.omit(OPAL_Flux_mmoles_m2_day)))

TPPflux_monthlyavg_sbb_top = sbb_bot_boxplots %>% 
  group_by(month) %>% 
  summarise (Avg_TPP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(TPP_Flux_umolesP_m2_day)))

PIPflux_monthlyavg_sbb_top = sbb_bot_boxplots %>% 
  group_by(month) %>% 
  summarise (Avg_PIP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(PIP_Flux_umolesP_m2_day)))

POPflux_monthlyavg_sbb_top = sbb_bot_boxplots %>% 
  group_by(month) %>% 
  summarise (Avg_POP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(POP_Flux_umolesP_m2_day)))

##################

massflux_yearlyavg_sbb_bot <- sbb_bot_boxplots %>% 
  group_by(year) %>% 
  summarise (Avg_Total_Mass_Flux_g_m2_day_sbb_bot = mean(na.omit(Total_Mass_Flux_g_m2_day)))

terigflux_yearlyavg_sbb_bot <- sbb_bot_boxplots %>% 
  group_by(year) %>% 
  summarise (Avg_Terrig_Flux_g_m2_day_sbb_bot = mean(na.omit(Terrigenous_Flux_g_m2_day)))

PONflux_yearlyavg_sbb_bot <- sbb_bot_boxplots %>% 
  group_by(year) %>% 
  summarise (Avg_PON_Flux_mmoles_m2_day_sbb_bot = mean(na.omit(PON_Flux_mmoles_m2_day)))

POCflux_yearlyavg_sbb_bot <- sbb_bot_boxplots %>% 
  group_by(year) %>% 
  summarise (Avg_POC_Flux_mmoles_m2_day_sbb_bot = mean(na.omit(POC_Flux_mmoles_m2_day)))

CaCO3flux_yearlyavg_sbb_bot <- sbb_bot_boxplots %>% 
  group_by(year) %>% 
  summarise (Avg_CaCO3_Flux_mmoles_m2_day_sbb_bot = mean(na.omit(CaCO3_Flux_mmoles_m2_day)))

Opalflux_yearlyavg_sbb_bot <- sbb_bot_boxplots %>% 
  group_by(year) %>% 
  summarise (Avg_Opal_Flux_mmoles_m2_day_sbb_bot = mean(na.omit(OPAL_Flux_mmoles_m2_day)))

TPPflux_yearlyyavg_sbb_top <- sbb_bot_boxplots %>% 
  group_by(year) %>% 
  summarise (Avg_TPP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(TPP_Flux_umolesP_m2_day)))

PIPflux_yearlyavg_sbb_top <- sbb_bot_boxplots %>% 
  group_by(year) %>% 
  summarise (Avg_PIP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(PIP_Flux_umolesP_m2_day)))

POPflux_yearlyavg_sbb_top <- sbb_bot_boxplots %>% 
  group_by(year) %>% 
  summarise (Avg_POP_Flux_umolesP_m2_day_sbb_top = mean(na.omit(POP_Flux_umolesP_m2_day)))


#################

plot_2a = ggplot(sbb_bot_boxplots, aes(x = month, y = Total_Mass_Flux_g_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "Total Mass Flux (g/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Monthly Average of Total Mass Flux")


plot_2b = ggplot(sbb_bot_boxplots, aes(x = month, y = Terrigenous_Flux_g_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "Terrigenous Flux (g/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Monthly Average of Terrigenous Flux")


plot_2c = ggplot(sbb_bot_boxplots, aes(x = month, y = PON_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "PON Flux (mmoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Monthly Average of PON Flux")

plot_2d = ggplot(sbb_bot_boxplots, aes(x = month, y = POC_Flux_mmoles_m2_day)) +
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


plot_2e = ggplot(sbb_bot_boxplots, aes(x = month, y = CaCO3_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "CaCO3 Flux (mmoles/m^2)/day") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Monthly Average of CaCO3 Flux")


plot_2f = ggplot(sbb_bot_boxplots, aes(x = month, y = OPAL_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "Opal Flux (mmoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Monthly Average of Opal Flux")


plot_2g = ggplot(sbb_bot_boxplots, aes(x = month, y = TPP_Flux_umolesP_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "TPP Flux (umoles/m^2/day)") +  scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Monthly Average of TPP Flux")


plot_2h = ggplot(sbb_bot_boxplots, aes(x = month, y = PIP_Flux_umolesP_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "PIP Flux (umoles/m^2/day)") +  scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Monthly Average of PIP Flux")

plot_2i = ggplot(sbb_bot_boxplots, aes(x = month, y = POP_Flux_umolesP_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Month", y = "POP Flux (umoles/m^2/day)") +  scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Monthly Average of POP Flux")

###################


plot_2j = ggplot(sbb_bot_boxplots, aes(x = year, y = Total_Mass_Flux_g_m2_day)) +
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

plot_2k = ggplot(sbb_bot_boxplots, aes(x = year, y = Terrigenous_Flux_g_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "Terrigenous Flux (g/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Yearly Average of Terrigenous Flux") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


plot_2l = ggplot(sbb_bot_boxplots, aes(x = year, y = PON_Flux_mmoles_m2_day)) +
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

plot_2m = ggplot(sbb_bot_boxplots, aes(x = year, y = POC_Flux_mmoles_m2_day)) +
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


plot_2n = ggplot(sbb_bot_boxplots, aes(x = year, y = CaCO3_Flux_mmoles_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "CaCO3 Flux (mmoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Yearly Average of CaCO3 Flux") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


plot_2o = ggplot(sbb_bot_boxplots, aes(x = year, y = OPAL_Flux_mmoles_m2_day)) +
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


plot_2p = ggplot(sbb_bot_boxplots, aes(x = year, y = TPP_Flux_umolesP_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "TPP Flux (umoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Yearly Average of TPP Flux") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


plot_2q = ggplot(sbb_bot_boxplots, aes(x = year, y = PIP_Flux_umolesP_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "PIP Flux (umoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Yearly Average of PIP Flux") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


plot_2r = ggplot(sbb_bot_boxplots, aes(x = year, y = POP_Flux_umolesP_m2_day)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(x = "Year", y = "POP Flux (umoles/m^2/day)") + scale_y_continuous(labels = scales::comma)+
  ggtitle("SBB Bot: Yearly Average of POP Flux") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

##########

sbb_bot_plot_2a_to_2f = grid.arrange(plot_2a,plot_2b,plot_2c,plot_2d,plot_2e,plot_2f, nrow = 2,ncol = 3)
sbb_bot_plot_2a_to_2f


sbb_bot_plot_2ghi= grid.arrange(plot_2g,plot_2h,plot_2i, nrow=1,ncol=3)
sbbtop_plot_2ghi

plot_2a_2b = grid.arrange(plot_2a,plot_2b, nrow=2,ncol=1)
plot_2a_2b

plot_2c_2d = grid.arrange(plot_2c,plot_2d, nrow=2,ncol=1)
plot_2c_2d

plot_2e_2f = grid.arrange(plot_2e,plot_2f, nrow=2,ncol=1)
plot_2e_2f

plot_2g_2h = grid.arrange(plot_2g,plot_2h, nrow=2,ncol=1)

plot_2i = grid.arrange(plot_2i,nrow=1,ncol=1)

ggsave("figures/SBB_Bot_Monthly_Total_Terrig.pdf", plot = plot_2a_2b, width = 10, height = 8)
ggsave("figures/SBB_Bot_Monthly_PON_POC.pdf", plot = plot_2c_2d, width = 10, height = 8)
ggsave("figures/SBB_Bot_Monthly_Carbonate_Opal.pdf", plot = plot_2e_2f, width = 10, height = 8)
ggsave("figures/SBB_Bot_Monthly_TPP_PIP.pdf", plot = plot_2g_2h, width = 10, height = 8)
ggsave("figures/SBB_Bot_Monthly_POP.pdf", plot = plot_2i, width = 10, height = 8)



#grid.arrange(plot_2g,plot_2h,plot_2i, nrow=1,ncol=3)
#plotall_2ab <- plot_2g+plot_2h+plot_2i
#plotall_2ab

#grid.arrange(plot_2a,plot_2b,plot_2c, nrow=1,ncol=3)
#plotall_2ac <- plot_2a+plot_2b+plot_2c
#plotall_2ac

#grid.arrange(plot_2d, plot_2e,plot_2f, nrow=1,ncol=3)
#plotall_2ae <- plot_2d+plot_2e+plot_2f
#plotall_2ae

#####

## start back here ##

sbb_bot_plot_2j_to_2o = grid.arrange(plot_2j,plot_2k,plot_2l,plot_2m, plot_2n,plot_2o, nrow=2,ncol=3)
sbb_bot_plot_2j_to_2o


sbb_bot_plot_2pqr= grid.arrange(plot_2p,plot_2q,plot_2r, nrow=1,ncol=3)
sbb_bot_plot_2pqr


plot_2j_2k = grid.arrange(plot_2j,plot_2k, nrow=2,ncol=1)
plot_2j_2k

plot_2l_2m = grid.arrange(plot_2l,plot_2m, nrow=2,ncol=1)
plot_2l_2m

plot_2n_2o = grid.arrange(plot_2n,plot_2o, nrow=2,ncol=1)
plot_2n_2o

plot_2p_2q = grid.arrange(plot_2p,plot_2q, nrow=2,ncol=1)
plot_2p_2q

plot_2r = grid.arrange(plot_2r,nrow=1,ncol=1)
plot_2r

ggsave("figures/SBB_Bot_Yearly_Total_Terrig.pdf", plot = plot_2j_2k, width = 10, height = 8)
ggsave("figures/SBB_Bot_Yearly_PON_POC.pdf", plot = plot_2l_2m, width = 10, height = 8)
ggsave("figures/SBB_Bot_Yearly_Carbonate_Opal.pdf", plot = plot_2n_2o, width = 10, height = 8)
ggsave("figures/SBB_Bot_Yearly_TPP_PIP.pdf", plot = plot_2p_2q, width = 10, height = 8)
ggsave("figures/SBB_Bot_Yearly_POP.pdf", plot = plot_2r, width = 10, height = 8)

# plot yearly --> total, carbon, nitrogen, silica
plot_2j # total
plot_2l # nitrogen
plot_2m # carbon
plot_2o # silica


ggsave("figures/SBB_Bot_Yearly_TotalFlux.jpg", plot_2j, width = 10, height = 8)
ggsave("figures/SBB_Bot_Yearly_PONFlux.jpg", plot_2l, width = 10, height = 8)
ggsave("figures/SBB_Bot_Yearly_POCFlux.jpg", plot_2m, width = 10, height = 8)
ggsave("figures/SBB_Bot_Yearly_SilicaFlux.jpg", plot_2o, width = 10, height = 8)
ggsave("figures/CtoN_RatioGraph.jpg", plot_CtoN, width = 12, height = 8)

ggsave("figures/SBB_Bot_Monthly_POCFlux.jpg",plot_2d,width = 10, height = 8)

#############################

#CNP Ratios

filtered_flux_bot = sbb_bot_boxplots %>%
  filter(!is.na(POC_Flux_mmoles_m2_day),
         !is.na(PON_Flux_mmoles_m2_day))

plot_CtoN = ggplot(filtered_flux_bot, aes(x = PON_Flux_mmoles_m2_day, y = POC_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE)+
  labs(x = "PON Flux (mmol/m²/day)",
       y = "POC Flux (mmol/m²/day)",
       title = "Carbon vs. Nitrogen Flux")

plot_CtoN = ggplot(filtered_flux_bot, aes(x = PON_Flux_mmoles_m2_day, y = POC_Flux_mmoles_m2_day)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  labs(
    x = "PON Flux (mmol/m²/day)",
    y = "POC Flux (mmol/m²/day)",
    title = "Carbon vs. Nitrogen Flux"
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14)  # Optional: increase tick label size
  )


#ggplot(filtered_flux_bot, aes(x = POC_Flux_mmoles_m2_day, y = TPP_Flux_umolesP_m2_day)) +
 # geom_point(color = "steelblue") +
  #geom_smooth(method = "lm", color = "darkred", se = TRUE)+
  #labs(x = "POC Flux (mmol/m²/day)",
   #    y = "TPP Flux (ummolP/m²/day)",
    #   title = "Carbon vs. Phosphorous Flux") +
  #theme_minimal()

# Carbon vs. Nitrogen

lm_CN <- lm(POC_Flux_mmoles_m2_day ~ PON_Flux_mmoles_m2_day, data = filtered_flux_bot)
summary(lm_CN)

# Now refit with converted P units
#lm_CP_mmol <- lm(TPP_mmolP_m2_day ~ POC_Flux_mmoles_m2_day, data = filtered_flux_bot)
#summary(lm_CP_mmol)


filtered_flux = filtered_flux %>%
 mutate(TPP_Flux_mmolesP_m2_day = TPP_Flux_umolesP_m2_day / 1000)

# Carbon vs. Phosphorus
#lm_CP_mmol <- lm(TPP_Flux_mmolesP_m2_day ~ POC_Flux_mmoles_m2_day, data = filtered_flux_bot)
#summary(lm_CP_mmol)


# ggplot(filtered_flux, aes(x = PON_Flux_mmoles_m2_day, y = TPP_Flux_umolesP_m2_day)) +
  #geom_point(color = "steelblue") +
  #geom_smooth(method = "lm", color = "darkred", se = TRUE)+
  #labs(x = "PON Flux (mmol/m²/day)",
    #   y = "TPP Flux (ummolP/m²/day)",
    #  title = "Nitrogen Flux vs. Phosphorous") +
  #theme_minimal()

ggsave("figures/CtoN_RatioGraph.jpg", plot_CtoN, width = 12, height = 8)


ggsave("figures/SBB_Bot_Monthly_TotalFlux.jpg",plot_2a,width = 10, height = 8)


#############################

#Linear Modles: modeling over time

sbb_bot_boxplots$month <- as.factor(sbb_bot_boxplots$month)

table(sbb_bot_boxplots$year)
table(sbb_bot_boxplots$month)

sbb_bot_boxplots$year <- relevel(as.factor(sbb_bot_boxplots$year), ref = "1994")
sbb_bot_boxplots$month <- relevel(as.factor(sbb_bot_boxplots$month), ref = "03")

model_a_factor_year <- lm(Total_Mass_Flux_g_m2_day ~ year + month, data = sbb_bot_boxplots)
summary(model_a_factor_year)

table(sbb_bot_boxplots$year)
unique(sbb_bot_boxplots$year)
