# Tea bag elevational gradient script
# Eleanor Walker // Haydn Thomas // Isla Myers-Smith, 20/03/2018 - 20/02/2023

# Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(scales)
library(lme4) # mixed effects
library(stargazer) # visualisation of mixed effect
library(dotwhisker) # graphical visualisation of mixed effect
library(broom)
library(effects) # plotting mixed effects models
library(lmerTest) # p-values
library (MuMIn)

# Import data ----

moisture.data <-
  read.csv(
    'data/Soil_moisture_data/CG_Soil_Moisture.csv'
  )

temp.data <-
  read.csv(
    "data/iButtons_data/CG_Daily_means.csv"
  )

decomp.data <-
  read.csv("data/Tea_data/El_CG_Tea.csv")

# Clean up moisture data ----
moisture.data <- filter(moisture.data, Probe_type == 'HydroSense')
moisture.data$Treatment <- as.character(moisture.data$Treatment)

# Add catergories
moisture.data$Cover <- ifelse(
  moisture.data$Code == "L/C" |
    moisture.data$Code == "DC" |
    moisture.data$Code == "C" |
    moisture.data$Code == "H/C",
  1,
  0
)

moisture.data$Bed <- ifelse(
  moisture.data$Code == "DC" |
    moisture.data$Code == "N" |
    moisture.data$Code == "H/C" |
    moisture.data$Code == "L/C" |
    moisture.data$Code == "L" |
    moisture.data$Code == "H",
  1,
  0
)

moisture.data$Drainage <- ifelse(moisture.data$Code == "H" |
                                   moisture.data$Code == "H/C",
                                 1, 0)

moisture.data$Watering <- ifelse(
  moisture.data$Code == "A" |
    moisture.data$Code == "D" |
    moisture.data$Code == "C" |
    moisture.data$Code == "N" |
    moisture.data$Code == "DC",
  0,
  ifelse(
    moisture.data$Code == "L/C" |
      moisture.data$Code == "L",
    1.14,
    2.27
  )
)

# Rename treatments#
moisture.data[moisture.data$Treatment == "Cover", ]$Treatment <-
  "Cover only"
moisture.data[moisture.data$Treatment == "Disturbed (no base)", ]$Treatment <-
  "Disturbed"
moisture.data[moisture.data$Treatment == "Disturbed (base)", ]$Treatment <-
  "Bed only"
moisture.data[moisture.data$Treatment == "Disturbed, cover ", ]$Treatment <-
  "Bed and cover"

# Clean up temperature data ----
temp.data$Code <-
  moisture.data$Code[match(temp.data$Plot, moisture.data$Site)]
temp.data$Treatment <-
  moisture.data$Treatment[match(temp.data$Plot, moisture.data$Site)]
temp.data$Cover <-
  moisture.data$Cover[match(temp.data$Plot, moisture.data$Site)]
temp.data$Bed <-
  moisture.data$Bed[match(temp.data$Plot, moisture.data$Site)]
temp.data$Drainage <-
  moisture.data$Drainage[match(temp.data$Plot, moisture.data$Site)]
temp.data$Watering <-
  moisture.data$Watering[match(temp.data$Plot, moisture.data$Site)]

# Clean up decomp data ----
decomp.data$Treatment <-
  moisture.data$Treatment[match(decomp.data$Site, moisture.data$Site)]
decomp.data$Cover <-
  moisture.data$Cover[match(decomp.data$Site, moisture.data$Site)]
decomp.data$Bed <-
  moisture.data$Bed[match(decomp.data$Site, moisture.data$Site)]
decomp.data$Drainage <-
  moisture.data$Drainage[match(decomp.data$Site, moisture.data$Site)]
decomp.data$Watering <-
  moisture.data$Watering[match(decomp.data$Site, moisture.data$Site)]

# Q1: Did treatments have an effect on moisture, temeprature or mass loss? ----

# Moisture ----
lmer.moisture.treat <-
  lmer(Moisture_VWC ~ Code + (1 | Site), data = moisture.data)
summary(lmer.moisture.treat)

# Plot
(
  MOIST.LMM <-
    ggplot(moisture.data, aes(
      x = reorder(Treatment,-Moisture_VWC), y = Moisture_VWC
    )) +
    geom_boxplot(aes(fill = as.factor(Watering))) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    scale_x_discrete() +
    xlab("\nTreatment") +
    ylab("Soil Moisture (%)") +
    scale_fill_manual(values = c("white", "lightblue", "blue3")) +
    theme(
      axis.text.x = element_text(
        angle = 60,
        vjust = 0.9,
        hjust = 1,
        face = 'bold'
      ),
      legend.position = "none"
    )
)

# Was covering significant?
summary(lmer(Moisture_VWC ~ Cover + (1 |
                                       Treatment / Site), data = moisture.data))
# Decrease, but NS

# Were effect types significant?
summary(lmer(Moisture_VWC ~ Bed + Watering + Cover + (1 |
                                                        Treatment / Site), data = moisture.data)) # Yes

# Temperature ----
lmer.temp.treat <-
  lmer(Daily_mean ~ Code + (1 | Plot) + (1 | Day), data = temp.data)
summary(lmer.temp.treat)

(
  TEMP.LMM <-
    ggplot(temp.data, aes(
      x = reorder(Treatment,-Daily_mean), y = Daily_mean
    )) +
    geom_boxplot(aes(fill = factor(Cover))) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    scale_x_discrete() +
    xlab("\nTreatment") +
    ylab("Temperature (°C)") +
    scale_fill_manual(values = c("white", "#F8766D")) +
    theme(
      axis.text.x = element_text(
        angle = 60,
        vjust = 0.9,
        hjust = 1,
        face = 'bold'
      ),
      legend.position = "none"
    )
)

# Were effect types significant?
summary(lmer(
  Daily_mean ~ Bed + Watering + Cover + (1 |
                                           Treatment / Plot) + (1 | Day),
  data = temp.data
))


# Decomposition ----
lmer.decomp.treat <-
  lmer(percent_mass_loss ~ Code + Colour + (1 |
                                              Site), data = decomp.data)
summary(lmer.decomp.treat)


# Plot
(
  DECOMP.LMM <-
    ggplot(decomp.data, aes(
      x = reorder(Treatment,-percent_mass_loss), y = percent_mass_loss
    )) +
    geom_boxplot(aes(fill = Colour)) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    scale_x_discrete() +
    xlab("\nTreatment") +
    ylab("% Mass Loss") +
    # scale_fill_manual(values=c("white","#F8766D","grey50","darkorchid","dodgerblue"))+
    scale_fill_manual(values = c("#006400", "#8B2323")) +
    theme(
      axis.text.x = element_text(
        angle = 60,
        vjust = 0.9,
        hjust = 1,
        face = 'bold'
      ),
      legend.position = "none"
    )
)

# Was disturbance significant?
disturbed <- filter(decomp.data, Treatment == 'Disturbed')
undisturbed <- filter(decomp.data, Treatment ==  'Ambient')

# T-test
t.test(disturbed$Mass_Loss, undisturbed$Mass_Loss, paired = TRUE)

# Q2: Did decomposition (mass loss) vary with environmental variables? ----

# Combine objects ----
# Summarise moisture data
mean_moisture <- moisture.data %>%
  group_by(Site) %>%
  dplyr::summarise(mean_moisture = mean(Moisture_VWC))

# Summarise temperature data
mean_temp <- temp.data %>%
  group_by(Plot) %>%
  dplyr::summarise(mean_temp = mean(Daily_mean))

# Combine with decomp data
decomp.data <-
  merge(decomp.data, mean_moisture, by = "Site", all = TRUE)
decomp.data <-
  merge(decomp.data,
        mean_temp,
        by.x = "Site",
        by.y = "Plot",
        all = TRUE)

# a) Run model - all ----
lmer_all <-
  lmer(percent_mass_loss ~  (Colour * mean_moisture) + (Colour * mean_temp) + (1 |
                                                                                 Site),
       data = decomp.data)
summary(lmer_all)
all_effects <-
  as.data.frame(effect(
    c("mean_moisture", "Colour"),
    lmer_all,
    xlevels = list(mean_moisture = seq(
      min(decomp.data$mean_moisture, na.rm = T),
      max(decomp.data$mean_moisture, na.rm = T),
      .1
    ))
  ))
all_effects_temp <-
  as.data.frame(effect(c("mean_temp", "Colour"), lmer_all, xlevels = list(mean_temp = seq(
    min(decomp.data$mean_temp, na.rm = T),
    max(decomp.data$mean_temp, na.rm = T),
    .01
  ))))

# Plot - individual tea models
(
  decompVmoisture_indiv <- ggplot(decomp.data) +
    geom_point(aes(
      mean_moisture, percent_mass_loss, colour = Colour
    ), size = 1.5) +
    theme_classic() +
    scale_fill_manual(values = c("#006400", "#8B2323")) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "none") +
    ylab('% Mass loss') + xlab('Soil Moisture (%)') +
    geom_ribbon(
      data = all_effects[all_effects$Colour == "G", ],
      mapping = aes(x = mean_moisture, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "#006400"
    ) +
    geom_line(
      data = all_effects[all_effects$Colour == "G", ],
      mapping = aes(x = mean_moisture, y = fit) ,
      size = 0.5,
      colour = "#006400",
      linetype = "dashed"
    ) +
    geom_ribbon(
      data = all_effects[all_effects$Colour == "R", ],
      mapping = aes(x = mean_moisture, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "#8B2323"
    ) +
    geom_line(
      data = all_effects[all_effects$Colour == "R", ],
      mapping = aes(x = mean_moisture, y = fit) ,
      size = 0.5,
      colour = "#8B2323"
    ) +
    scale_colour_manual(values = c("#006400", "#8B2323"))
)

# c) Run model - temperature ----

min(decomp.data$mean_temp, na.rm = T)
max(decomp.data$mean_temp, na.rm = T)

min(decomp.data$mean_moisture, na.rm = T)
max(decomp.data$mean_moisture, na.rm = T)

# Plot - overall model
(
  decompVtemp_indiv <- ggplot(decomp.data) +
    geom_point(aes(mean_temp, percent_mass_loss, colour = Colour), size =
                 1.5) +
    theme_classic() +
    scale_fill_manual(values = c("#006400", "#8B2323")) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "none") +
    ylab('% Mass loss') + xlab('Soil Temperature (°C)') +
    geom_ribbon(
      data = all_effects_temp[all_effects_temp$Colour == "G", ],
      mapping = aes(x = mean_temp, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "#006400"
    ) +
    geom_line(
      data = all_effects_temp[all_effects_temp$Colour == "G", ],
      mapping = aes(x = mean_temp, y = fit) ,
      size = 0.5,
      colour = "#006400",
      linetype = "solid"
    ) +
    geom_ribbon(
      data = all_effects_temp[all_effects_temp$Colour == "R", ],
      mapping = aes(x = mean_temp, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "#8B2323"
    ) +
    geom_line(
      data = all_effects_temp[all_effects_temp$Colour == "R", ],
      mapping = aes(x = mean_temp, y = fit) ,
      size = 0.5,
      colour = "#8B2323",
      linetype = "dashed"
    ) +
    scale_colour_manual(values = c("#006400", "#8B2323"))
)

lmer_all_scaled <-
  lmer(percent_mass_loss ~  (Colour * scale(mean_moisture)) + (Colour * scale(mean_temp)) + (1 |
                                                                                               Site),
       data = decomp.data)
summary(lmer_all_scaled)

# 3. Does temperature correlate with moisture? ----
decomp.data.unique <- decomp.data[!duplicated(decomp.data$Site), ]

# Run model
tempVmoist_mod <-
  lmer(mean_temp ~ mean_moisture + (1 |
                                      Treatment), data = decomp.data.unique)
summary(tempVmoist_mod)
r.squaredGLMM(tempVmoist_mod) # R2 not above 0.7
tempVmoist_effects <-
  as.data.frame(effect(
    "mean_moisture",
    tempVmoist_mod,
    xlevels = list(mean_moisture = seq(
      min(decomp.data$mean_moisture),
      max(decomp.data$mean_moisture),
      .1
    ))
  ))

(
  tempVmoist <- ggplot(decomp.data.unique) +
    geom_point(
      aes(mean_moisture, mean_temp),
      colour = 'black',
      fill = "black",
      size = 1.75,
      pch = 21,
      stroke = 0.75
    ) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    ylab('Soil Temperature (°C)') + xlab('Soil Moisture (%)') +
    ggtitle("b)") +
    geom_ribbon(
      data = tempVmoist_effects,
      mapping = aes(x = mean_moisture, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "black"
    ) +
    geom_line(
      data = tempVmoist_effects,
      mapping = aes(x = mean_moisture, y = fit) ,
      size = 0.5,
      colour = "black",
      linetype = "dashed"
    )
)

# 1b correlation temp v moisture
pdf(file = "figures/Fig_S3b.pdf",
    width = 3,
    height = 3)
grid.arrange(tempVmoist)
dev.off()

# 4. Add TBI variables ----

# Tea bag hydrolisable fractions:
Hg <- 0.842
Hr <- 0.552

# Add S
tea_G <- subset(decomp.data, Colour == "G")
tea_G$S <- 1 - ((tea_G$percent_mass_loss / 100) / Hg)
tea_G$ar <- Hr * (1 - tea_G$S)

# Add back to data
decomp.data$S <-
  tea_G$S[match(decomp.data$Pair_label, tea_G$Pair_label)]
decomp.data$ar <-
  tea_G$ar[match(decomp.data$Pair_label, tea_G$Pair_label)]

# Add k
tea_R <- subset(decomp.data, Colour == "R")
tea_R$k <-
  log(tea_R$ar / ((1 - (
    tea_R$percent_mass_loss / 100
  )) - (1 - tea_R$ar))) / tea_R$Days

decomp.data$k <-
  tea_R$k[match(decomp.data$Pair_label, tea_R$Pair_label)]

# a) Examine realtionship between S & variables ----

# Extract only one tea type to avoid duplicates
sk_data <- subset(decomp.data, Colour == "R")

# Relationship with environment - all ----
S_env <-
  lmer(S ~ (mean_moisture) + (mean_temp) +  (1 | Site), data = sk_data)
summary(S_env)
S_env_effects <-
  as.data.frame(effect("mean_moisture", S_env, xlevels = list(mean_moisture = seq(
    min(decomp.data$mean_moisture, na.rm = T),
    max(decomp.data$mean_moisture, na.rm = T),
    0.01
  ))))
S_env_effects_temp <-
  as.data.frame(effect("mean_temp", S_env, xlevels = list(mean_temp = seq(
    min(decomp.data$mean_temp, na.rm = T),
    max(decomp.data$mean_temp, na.rm = T),
    0.01
  ))))

# Relationship with moisture ----
(
  SVmoisture <- ggplot(sk_data) +
    geom_point(aes(mean_moisture, S), colour = "#006400", size = 1.5) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    theme(legend.position = "none") +
    labs(x = "Soil Moisture (%)", y = expression(
      paste("Stabilisation Factor (", italic("S"), ")")
    )) +
    geom_ribbon(
      data = S_env_effects,
      mapping = aes(x = mean_moisture, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "#006400"
    ) +
    geom_line(
      data = S_env_effects,
      mapping = aes(x = mean_moisture, y = fit) ,
      size = 0.3,
      colour = "#006400",
      linetype = "dashed"
    ) +
    scale_colour_manual(values = c("#006400", "#8B2323"))
)

# Relationship with temperature ----
(
  SVtemp <- ggplot(sk_data) +
    geom_point(aes(mean_temp, S), colour = "#006400", size = 1.5) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    theme(legend.position = "none") +
    labs(x = "Soil Temperature (°C)", y = expression(
      paste("Stabilisation Factor (", italic("S"), ")")
    )) +
    geom_ribbon(
      data = S_env_effects_temp,
      mapping = aes(x = mean_temp, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "#006400"
    ) +
    geom_line(
      data = S_env_effects_temp,
      mapping = aes(x = mean_temp, y = fit) ,
      size = 0.5,
      colour = "#006400",
      linetype = "dashed"
    ) +
    scale_colour_manual(values = c("#006400", "#8B2323"))
)

# b) Examine realtionship between k & variables ----

# Relationship with environment - all ----
k_env <-
  lmer(k ~ (mean_temp) + (mean_moisture) +  (1 | Site), data = sk_data)
summary(k_env)
k_env_effects <-
  as.data.frame(effect("mean_moisture", k_env, xlevels = list(mean_moisture = seq(
    min(decomp.data$mean_moisture),
    max(decomp.data$mean_moisture),
    0.01
  ))))
k_env_effects_temp <-
  as.data.frame(effect("mean_temp", k_env, xlevels = list(mean_temp = seq(
    min(decomp.data$mean_temp, na.rm = T),
    max(decomp.data$mean_temp, na.rm = T),
    0.01
  ))))

# Relationship with moisture ----
(
  kVmoisture <- ggplot(sk_data) +
    geom_point(aes(mean_moisture, k), colour = "#8B2323", size = 1.5) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    theme(legend.position = "none") +
    labs(x = "Soil Moisture (%)", y = expression(
      paste("Decomposition Rate (", italic("k"), ")")
    )) +
    geom_ribbon(
      data = k_env_effects,
      mapping = aes(x = mean_moisture, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "#8B2323"
    ) +
    geom_line(
      data = k_env_effects,
      mapping = aes(x = mean_moisture, y = fit) ,
      size = 0.5,
      colour = "#8B2323"
    ) +
    scale_colour_manual(values = c("#006400", "#8B2323"))
)

# Relationship with temperature ----
(
  kVtemp <- ggplot(sk_data) +
    geom_point(aes(mean_temp, k), colour = "#8B2323", size = 1.5) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    theme(legend.position = "none") +
    labs(x = "Soil Temperature (°C)", y = expression(
      paste("Decomposition Rate (", italic("k"), ")")
    )) +
    geom_ribbon(
      data = k_env_effects_temp,
      mapping = aes(x = mean_temp, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "#8B2323"
    ) +
    geom_line(
      data = k_env_effects_temp,
      mapping = aes(x = mean_temp, y = fit) ,
      size = 0.5,
      colour = "#8B2323",
      linetype = "dashed"
    ) +
    scale_colour_manual(values = c("#006400", "#8B2323"))
)

# Combine final figures ----

# 1) Differences among treatments
pdf(file = "figures/Fig_S5.pdf",
    width = 10,
    height = 4)
grid.arrange(MOIST.LMM, TEMP.LMM, DECOMP.LMM, ncol = 3)
dev.off()

# 2) Differences among treatments
# pdf(file="figures/Fig_S4_old.pdf", width = 3, height = 3)
# tempVmoist
# dev.off()

# 4) S & k with temp and moisture
library(grid)
CG.moist <-
  ggplot_gtable(ggplot_build(decompVmoisture_indiv + ggtitle("c)")))
CG.temp <-
  ggplot_gtable(ggplot_build(decompVtemp_indiv + ggtitle("d)")))
S_temp_manip <- ggplot_gtable(ggplot_build(SVtemp + ggtitle("h)")))
S_moist_manip <-
  ggplot_gtable(ggplot_build(SVmoisture + ggtitle("g)")))
k_temp_manip <- ggplot_gtable(ggplot_build(kVtemp + ggtitle("f)")))
k_moist_manip <-
  ggplot_gtable(ggplot_build(kVmoisture + ggtitle("e)")))

maxWidth = unit.pmax(
  S_temp_manip$widths[2:3],
  S_moist_manip$widths[2:3],
  k_temp_manip$widths[2:3],
  k_moist_manip$widths[2:3],
  CG.moist$widths[2:3],
  CG.temp$widths[2:3]
)

CG.moist$widths[2:3] <- maxWidth
CG.temp$widths[2:3] <- maxWidth
S_temp_manip$widths[2:3] <- maxWidth
S_moist_manip$widths[2:3] <- maxWidth
k_temp_manip$widths[2:3] <- maxWidth
k_moist_manip$widths[2:3] <- maxWidth

pdf(file = "figures/Fig_4.pdf",
    width = 7,
    height = 8)
grid.arrange(CG.moist,
             CG.temp,
             S_moist_manip,
             S_temp_manip,
             k_moist_manip,
             k_temp_manip,
             ncol = 2)
dev.off()

#Alternative figures
#2 - Mass loss figures
pdf(file = "figures/Fig_3_ALT_bottom.pdf",
    width = 6,
    height = 3)
grid.arrange(CG.moist, CG.temp,
             ncol = 2)
dev.off()

#Alternative figures
#3 - S-k figures
pdf(file = "figures/Fig_4_ALT_bottom.pdf",
    width = 6,
    height = 5)
grid.arrange(k_moist_manip,
             k_temp_manip,
             S_moist_manip,
             S_temp_manip,
             ncol = 2)
dev.off()
