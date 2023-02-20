# Tea bag elevational gradient script
# Eleanor Walker // Haydn Thomas // Isla Myers-Smith, 20/03/2018 - 20/02/2023

# Question 1 - Differences between teas

# Libraries
library(plyr)
library(dplyr)
library(ggplot2)
require(gridExtra)
library(lme4)
library(nlme)
library(effects)
library(stringr)

KL_tea <-
  read.csv(file = "data/Kluane_tea_bag_data.csv")

#Test decay rates (Common garden data) ----
#Check tea decay rates for Common Garden

Daily_Tea <- KL_tea[grep("DT", KL_tea$Plot), ] #Daily tea only
Common_garden <-
  KL_tea[grep("CG", KL_tea$Plot), ] #All common garden ambient plots
two_yr_Kluane <- filter(KL_tea, Site == "Kluane Plateau")
two_yr_Common_Garden <- filter(
  KL_tea,
  Plot == "CG_HT_year" |
    Plot == "CG_HT_year" |
    Plot == "CG_2Y" |
    Plot == "CG_Y1_3m" |
    Plot == "CG_Y2_3m"
)

#Take mean of daily tea
Daily_Tea_mean <- Daily_Tea %>%
  group_by(Tea_Type, Days) %>%
  dplyr::summarise(Loss = mean(Loss))

summary(lm(log((1 - Loss) * 100) ~ Days, Daily_Tea_mean[Daily_Tea_mean$Tea_Type ==
                                                          "Green", ]))
summary(lm(log((1 - Loss) * 100) ~ Days, Daily_Tea_mean[Daily_Tea_mean$Tea_Type ==
                                                          "Rooibos", ]))

(
  daily <-
    ggplot(Daily_Tea_mean, aes(Days, (1 - Loss) * 100, colour = factor(Tea_Type))) +
    geom_point() +
    geom_smooth(
      method = "lm",
      formula = (y ~ (log(x))),
      aes(fill = Tea_Type)
    ) +
    theme_classic() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    scale_colour_manual(values = c("#006400", "#8B2323"), name = "Tea Type") +
    scale_fill_manual(values = c("#006400", "#8B2323"), name = "Tea Type") +
    labs(y = "Mass remaining (%)", x = "Days since burial") +
    scale_x_continuous(breaks = seq(0, 50, 10)) +
    theme(legend.position = "none")
)
#ggtitle("Kluane (warm site):\nTwo months")

#Create figure for El's Manuscript
pdf(file = "figures/Fig_S1.pdf",
    width = 6,
    height = 4)
daily
dev.off()

#Kluane

m1 <-
  lme((1 - Loss) * 100 ~ (log(Days)),
      data = two_yr_Kluane[two_yr_Kluane$Tea_Type == "Green", ],
      random = ~ 1 | Plot)
ef_g <-
  as.data.frame(effect("log(Days)", m1, xlevels = list(Days = seq(
    min(two_yr_Kluane$Days), max(two_yr_Kluane$Days), 1
  ))))

m2 <-
  lme((1 - Loss) * 100 ~ (log(Days)),
      data = two_yr_Kluane[two_yr_Kluane$Tea_Type == "Rooibos", ],
      random = ~ 1 | Plot)
ef_r <-
  as.data.frame(effect("log(Days)", m2, xlevels = list(Days = seq(
    min(two_yr_Kluane$Days), max(two_yr_Kluane$Days), 1
  ))))

(
  Kluane <- ggplot(two_yr_Kluane) +
    geom_point(aes(Days, (1 - Loss) * 100, colour = factor(Tea_Type))) +
    geom_ribbon(
      data = ef_g,
      mapping = aes(x = Days, ymin = lower, ymax = upper),
      fill = "#006400",
      alpha = 0.5
    ) +
    geom_ribbon(
      data = ef_r,
      mapping = aes(x = Days, ymin = lower, ymax = upper),
      fill = "#8B2323",
      alpha = 0.5
    ) +
    geom_line(
      data = ef_g,
      mapping = aes(x = Days, y = fit),
      colour = "#006400",
      lwd = 1.25
    ) +
    geom_line(
      data = ef_r,
      mapping = aes(x = Days, y = fit),
      colour = "#8B2323",
      lwd = 1.25
    ) +
    scale_colour_manual(values = c("#006400", "#8B2323"), name = "Tea Type") +
    theme_classic() +
    theme(legend.position = "none") +
    labs(y = "Mass remaining (%)", x = "Days since burial") +
    ggtitle("Kluane (cold site):\nTwo years")
)

#Common Garden
m1 <-
  lme((1 - Loss) * 100 ~ (log(Days)),
      data = two_yr_Common_Garden[two_yr_Common_Garden$Tea_Type == "Green", ],
      random = ~ 1 | Plot)
ef_g <-
  as.data.frame(effect("log(Days)", m1, xlevels = list(Days = seq(
    min(two_yr_Common_Garden$Days),
    max(two_yr_Common_Garden$Days),
    1
  ))))

m2 <-
  lme((1 - Loss) * 100 ~ (log(Days)),
      data = two_yr_Common_Garden[two_yr_Common_Garden$Tea_Type == "Rooibos", ],
      random = ~ 1 | Plot)
ef_r <-
  as.data.frame(effect("log(Days)", m2, xlevels = list(Days = seq(
    min(two_yr_Common_Garden$Days),
    max(two_yr_Common_Garden$Days),
    1
  ))))

(
  Common <- ggplot(two_yr_Common_Garden) +
    geom_point(aes(Days, (1 - Loss) * 100, colour = factor(Tea_Type))) +
    geom_ribbon(
      data = ef_g,
      mapping = aes(x = Days, ymin = lower, ymax = upper),
      fill = "#006400",
      alpha = 0.5
    ) +
    geom_ribbon(
      data = ef_r,
      mapping = aes(x = Days, ymin = lower, ymax = upper),
      fill = "#8B2323",
      alpha = 0.5
    ) +
    geom_line(
      data = ef_g,
      mapping = aes(x = Days, y = fit),
      colour = "#006400",
      lwd = 1.25
    ) +
    geom_line(
      data = ef_r,
      mapping = aes(x = Days, y = fit),
      colour = "#8B2323",
      lwd = 1.25
    ) +
    scale_colour_manual(values = c("#006400", "#8B2323"), name = "Tea Type") +
    theme_classic() +
    theme(legend.position = "none") +
    labs(y = "Mass remaining (%)", x = "Days since burial") +
    ggtitle("Kluane (warm site):\nTwo years")
)

#Combine warm site
all_Common_Garden <- rbind(Daily_Tea, two_yr_Kluane)

m1 <-
  lme((1 - Loss) * 100 ~ (log(Days)),
      data = all_Common_Garden[all_Common_Garden$Tea_Type == "Green", ],
      random = ~ 1 | Plot)
ef_g <-
  as.data.frame(effect("log(Days)", m1, xlevels = list(Days = seq(
    min(all_Common_Garden$Days), max(all_Common_Garden$Days), 1
  ))))

m2 <-
  lme((1 - Loss) * 100 ~ (log(Days)),
      data = all_Common_Garden[all_Common_Garden$Tea_Type == "Rooibos", ],
      random = ~ 1 | Plot)
ef_r <-
  as.data.frame(effect("log(Days)", m2, xlevels = list(Days = seq(
    min(all_Common_Garden$Days), max(all_Common_Garden$Days), 1
  ))))

(
  Common_all <- ggplot(all_Common_Garden) +
    geom_point(aes(Days, (1 - Loss) * 100, colour = factor(Tea_Type))) +
    geom_ribbon(
      data = ef_g,
      mapping = aes(x = Days, ymin = lower, ymax = upper),
      fill = "#006400",
      alpha = 0.5
    ) +
    geom_ribbon(
      data = ef_r,
      mapping = aes(x = Days, ymin = lower, ymax = upper),
      fill = "#8B2323",
      alpha = 0.5
    ) +
    geom_line(
      data = ef_g,
      mapping = aes(x = Days, y = fit),
      colour = "#006400",
      lwd = 1.25
    ) +
    geom_line(
      data = ef_r,
      mapping = aes(x = Days, y = fit),
      colour = "#8B2323",
      lwd = 1.25
    ) +
    scale_colour_manual(values = c("#006400", "#8B2323"), name = "Tea Type") +
    theme_classic() +
    theme(legend.position = "none") +
    labs(y = "Mass remaining (%)", x = "Days since burial") +
    ggtitle("Kluane (warm site):\nTwo years")
)
