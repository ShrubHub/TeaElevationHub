# Tea bag elevational gradient script
# Eleanor Walker // Haydn Thomas // Isla Myers-Smith, 20/03/2018 - 20/02/2023

# Libraries
library(dplyr)
library(tidyr)
library(pbkrtest)
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
`%notin%` <- function(x, y)
  ! (x %in% y)

# Import data ----

moisture.data <-
  read.csv("data/Soil_moisture_data/Plat_moisture_Data.csv")
temp.data <- read.csv("data/iButtons_data/Plat_Daily_means.csv")
decomp.data <- read.csv("data/Tea_data/El_Plateau_Tea_EDIT.csv")


# Q1: Did (supplementary) treatments have an effect on moisture, temeprature or mass loss? ----

# a) Moisture ----

# Add metadata
moisture.data$Plot <-
  paste0(moisture.data$Site, moisture.data$Treatment)

# Remove pre-treatment readings - treatment plots
moisture.raw <- moisture.data
moisture.data <- subset(moisture.data, Date != "24/06/16")

# Extract only moisture treatment sites
moist.treat <-
  filter(
    moisture.data,
    Elevation == "794" |
      Elevation == "1175" | Elevation == "1551" | Elevation == "1926"
  )

# Reorder levels
moist.treat$Treatment <-
  factor(moist.treat$Treatment, levels = c("A", "T", "M", "B"))

# Round elevation headers
# moist.treat[moist.treat$Elevation==794,]$Elevation <- 800
# moist.treat[moist.treat$Elevation==1551,]$Elevation <- 1550
# moist.treat[moist.treat$Elevation==1926,]$Elevation <- 1925

# Plot figure
(
  box.moist <-
    ggplot(
      moist.treat,
      aes(x = Treatment, y = Soil_moisture, fill = Treatment)
    ) +
    geom_boxplot() +
    facet_grid(. ~ Elevation, scales = "fixed", space = "free") +
    scale_fill_manual(values = c(
      "white", "#F8766D", "dodgerblue", "darkorchid"
    )) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
    theme_classic() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = 0.75),
      strip.background = element_rect(color = "black", size = 1)
    ) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "none") +
    ggtitle("a)") +
    scale_x_discrete(labels = c(
      "A" = "A",
      "T" = "T",
      "M" = "M",
      "B" = "C"
    )) +
    ylab('VWC (%)') + xlab("")
)

# Run model (among sites)
lmer_moisture <-
  lmer(Soil_moisture ~ Treatment + (1 | Site / Plot), data = moist.treat)
summary(lmer_moisture)

# b) Temperature ----

# Remove first day as in transit
temp.data <- subset(temp.data, Day != "2016-06-24")

# Add metadata
temp.data$Site <-
  substr(temp.data$Plot, 1, nchar(as.character(temp.data$Plot)) - 1)
temp.data$Elevation <-
  moisture.data$Elevation[match(temp.data$Plot, moisture.data$Plot)]

# Extract treatment sites
temp.treat <-
  filter(
    temp.data,
    Elevation == "794" |
      Elevation == "1175" | Elevation == "1551" | Elevation == "1926"
  )

# Change elevation headers
# temp.treat[temp.treat$Elevation==794,]$Elevation <- 800
# temp.treat[temp.treat$Elevation==1551,]$Elevation <- 1550
# temp.treat[temp.treat$Elevation==1926,]$Elevation <- 1925

# Reorder levels
temp.treat$Treatment <-
  factor(temp.treat$Treatment, levels = c("A", "T", "M", "B"))

# NOTE: Using daily temperature to indicate range of temeprature values within plots ----

# Plot
(
  box.temp <-
    ggplot(temp.treat, aes(
      x = Treatment, y = Daily_mean, fill = Treatment
    )) +
    geom_boxplot() +
    facet_grid(. ~ Elevation, scales = "fixed", space = "free") +
    scale_fill_manual(values = c(
      "white", "#F8766D", "dodgerblue", "darkorchid"
    )) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
    theme_classic() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = 0.75),
      strip.background = element_rect(color = "black", size = 1)
    ) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "none") +
    ggtitle("b)") +
    scale_x_discrete(labels = c(
      "A" = "A",
      "T" = "T",
      "M" = "M",
      "B" = "C"
    )) +
    ylab('Soil Temperature (°C)') + xlab("")
)

# Run model (among sites)
lmer_temperature <-
  lmer(Daily_mean ~ Treatment + (1 |
                                   Site / Plot) + (1 | Day), data = temp.treat)
summary(lmer_temperature)


# c) Mass Loss ----

# Convert mass loss to numeric
decomp.data$Percent_mass_loss <-
  as.numeric(as.character(decomp.data$Percent_mass_loss))
decomp.data <- subset(decomp.data, !is.na(Percent_mass_loss))

# Anomalous green tea
# decomp.data <- decomp.data[-which(decomp.data$Percent_mass_loss == min(filter(decomp.data,tea_type == "G")$Percent_mass_loss)),]

# Split bag
decomp.data <-
  decomp.data[-which(decomp.data$Label == filter(decomp.data, Elevation == 794 &
                                                   Notes == "split")$Label)[1], ]

# Extract only treatment sites
decomp.treat <-
  filter(
    decomp.data,
    Elevation == "794" |
      Elevation == "1175" | Elevation == "1551" | Elevation == "1926"
  )

# Change elevation headers
# decomp.treat[decomp.treat$Elevation==794,]$Elevation <- 800
# decomp.treat[decomp.treat$Elevation==1551,]$Elevation <- 1550
# decomp.treat[decomp.treat$Elevation==1926,]$Elevation <- 1925

# Reorder levels
decomp.treat$Treatment <-
  factor(decomp.treat$Treatment, levels = c("A", "T", "M", "B"))

# Plot figure
(
  box.decomp <-
    ggplot(decomp.treat, aes(factor(Treatment), (Percent_mass_loss))) +
    geom_boxplot(aes(fill = tea_type)) +
    facet_grid(. ~ Elevation, scales = "fixed", space = "free") +
    scale_fill_manual(values = c("#006400", "#8B2323")) +
    theme_classic() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = 0.75),
      strip.background = element_rect(color = "black", size = 1)
    ) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "none") +
    ggtitle("c)") +
    scale_x_discrete(labels = c(
      "A" = "A",
      "T" = "T",
      "M" = "M",
      "B" = "C"
    )) +
    ylab('% Mass loss') + xlab('Treatment')
)

# Run model (among sites) - fixed slopes
lmer_decomp <-
  lmer(Percent_mass_loss ~ Treatment * tea_type + (1 |
                                                     Site), data = decomp.treat)
summary(lmer_decom)


# Q2: Does decomposition change with elevation? ----

# Combine temperature, moisture and decomposition objects

# Summarise moisture data
mean_moisture <- moisture.raw %>%
  group_by(Plot) %>%
  dplyr::summarise(mean_moisture = mean(Soil_moisture))

mean_moisture_post <- moisture.data %>%
  group_by(Plot) %>%
  dplyr::summarise(mean_moisture_post = mean(Soil_moisture))

# Summarise temperature data
mean_temp <- temp.data %>%
  group_by(Plot) %>%
  dplyr::summarise(mean_temp = mean(Daily_mean))

# Combine with decomp data
decomp.data <-
  merge(decomp.data, mean_moisture, by = "Plot", all = TRUE)
decomp.data <-
  merge(decomp.data,
        mean_moisture_post,
        by = "Plot",
        all = TRUE)
decomp.data <- merge(decomp.data, mean_temp, by = "Plot", all = TRUE)

# Make sure elevation is continuous
decomp.data$Elevation <-
  as.numeric(as.character(decomp.data$Elevation))

# Model change in decomposition with elevation - only ambient treatments ----

# Subset ambient treatments
decomp.data.ambient <- subset(decomp.data, Treatment == "A")

# Run model

min(decomp.data.ambient$mean_temp, na.rm = T)
max(decomp.data.ambient$mean_temp, na.rm = T)

min(decomp.data.ambient$mean_moisture, na.rm = T)
max(decomp.data.ambient$mean_moisture, na.rm = T)

lmer_elevation_a <-
  lmer(Percent_mass_loss ~ Elevation * tea_type + (1 |
                                                     Site), data = decomp.data.ambient)
summary(lmer_elevation_a)
elevation_effects_a <-
  as.data.frame(effect(
    c("Elevation", "tea_type"),
    lmer_elevation_a,
    xlevels = list(Elevation = seq(794, 2002, 1))
  ))

# Plot - individual tea lines
(
  decompVele_indiv_ambient <- ggplot(decomp.data.ambient) +
    geom_point(aes(Elevation, Percent_mass_loss, colour = tea_type), size =
                 1.5) +
    theme_classic() +
    scale_fill_manual(values = c("#006400", "#8B2323")) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "none") +
    ylab('% Mass loss') + xlab('Elevation (m a.s.l.)') +
    geom_ribbon(
      data = elevation_effects_a[elevation_effects_a$tea_type == "G", ],
      mapping = aes(x = Elevation, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "#006400"
    ) +
    geom_line(
      data = elevation_effects_a[elevation_effects_a$tea_type == "G", ],
      mapping = aes(x = Elevation, y = fit) ,
      size = 0.5,
      colour = "#006400"
    ) +
    geom_ribbon(
      data = elevation_effects_a[elevation_effects_a$tea_type == "R", ],
      mapping = aes(x = Elevation, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "#8B2323"
    ) +
    geom_line(
      data = elevation_effects_a[elevation_effects_a$tea_type == "R", ],
      mapping = aes(x = Elevation, y = fit) ,
      size = 0.5,
      colour = "#8B2323"
    ) +
    scale_colour_manual(values = c("#006400", "#8B2323"))
)


# Q3. Do env variables vary with elevation ----

# Run model - moisture ----
lmer_moistureEle <-
  lmer(Soil_moisture ~  Elevation + (1 |
                                       Plot), data = moisture.raw[moisture.raw$Treatment == "A", ]) # Add date
summary(lmer_moistureEle)
moistureEle_effects <-
  as.data.frame(effect("Elevation", lmer_moistureEle, xlevels = list(Elevation = seq(
    min(decomp.data$Elevation), max(decomp.data$Elevation), 1
  ))))

# Plot moisture
(
  moistureVele <- ggplot() +
    geom_point(
      data = moisture.raw[moisture.raw$Treatment == "A", ],
      aes(Elevation, Soil_moisture),
      colour = "grey50",
      size = 1.5
    ) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    theme(legend.position = "none") +
    ylab('Soil Moisture (%)') + xlab('Elevation (m a.s.l.)') +
    geom_ribbon(
      data = moistureEle_effects,
      mapping = aes(x = Elevation, ymin = lower, ymax = upper),
      alpha = 0.2
    ) +
    geom_line(
      data = moistureEle_effects,
      mapping = aes(x = Elevation, y = fit) ,
      size = 0.5
    ) +
    geom_point(
      data = decomp.data.ambient,
      aes(Elevation, mean_moisture),
      colour = "black",
      fill = "white",
      size = 1.75,
      pch = 21,
      stroke = 0.75
    )
)

# Run model - temperature ----
lmer_tempEle <-
  lmer(Daily_mean ~  Elevation + (1 |
                                    Plot) + (1 | Day) , data = temp.data[temp.data$Treatment == "A", ])
summary(lmer_tempEle)
tempEle_effects <-
  as.data.frame(effect("Elevation", lmer_tempEle, xlevels = list(Elevation = seq(
    min(decomp.data$Elevation), max(decomp.data$Elevation), 1
  ))))

# Plot temp
(
  tempVele <- ggplot(temp.data[temp.data$Code == "A", ]) +
    geom_point(
      aes(Elevation, Daily_mean),
      colour = "grey50",
      size = 1.5,
      alpha = 0.8,
      pch = 19
    ) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    theme(legend.position = "none") +
    ylab('Soil Temperature (°C)') + xlab('Elevation (m a.s.l.)') +
    geom_ribbon(
      data = tempEle_effects,
      mapping = aes(x = Elevation, ymin = lower, ymax = upper),
      alpha = 0.2
    ) +
    geom_line(
      data = tempEle_effects,
      mapping = aes(x = Elevation, y = fit) ,
      size = 0.5
    ) +
    geom_point(
      data = decomp.data.ambient,
      aes(Elevation, mean_temp),
      colour = "black",
      fill = "white",
      size = 1.75,
      pch = 21,
      stroke = 0.75
    )
)


# Q4. Does decomposition with environmental variables ----

# Run model - overall ----

# Ambient model - fixed slopes
lmer_environment <-
  lmer(Percent_mass_loss ~ (tea_type * mean_moisture) + (tea_type * mean_temp) + (1 |
                                                                                    Site),
       data = decomp.data.ambient)
summary(lmer_environment)
environment_effects <-
  as.data.frame(effect(
    c("mean_moisture", "tea_type"),
    lmer_environment,
    xlevels = list(mean_moisture = seq(
      min(decomp.data.ambient$mean_moisture, na.rm = T),
      max(decomp.data.ambient$mean_moisture, na.rm = T),
      .01
    ))
  ))
environment_effects_temp <-
  as.data.frame(effect(
    c("mean_temp", "tea_type"),
    lmer_environment,
    xlevels = list(mean_temp = seq(
      min(decomp.data.ambient$mean_temp, na.rm = T),
      max(decomp.data.ambient$mean_temp, na.rm = T),
      .01
    ))
  ))

# Plot - ambient plots
(
  decompVmoisture_ambients <- ggplot() +
    geom_point(
      data = decomp.data.ambient[decomp.data.ambient$tea_type == "R", ],
      aes(mean_moisture, Percent_mass_loss, fill = Elevation),
      pch = 21,
      size = 2,
      colour = alpha("red", 0)
    ) +
    geom_point(
      data = decomp.data.ambient[decomp.data.ambient$tea_type == "G", ],
      aes(mean_moisture, Percent_mass_loss, colour = Elevation)
    ) +
    geom_ribbon(
      data = environment_effects[environment_effects$tea_type == "G", ],
      mapping = aes(x = mean_moisture, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "#006400"
    ) +
    geom_line(
      data = environment_effects[environment_effects$tea_type == "G", ],
      mapping = aes(x = mean_moisture, y = fit) ,
      size = 0.5,
      colour = "#006400",
      linetype = "solid"
    ) +
    geom_ribbon(
      data = environment_effects[environment_effects$tea_type == "R", ],
      mapping = aes(x = mean_moisture, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "#8B2323"
    ) +
    geom_line(
      data = environment_effects[environment_effects$tea_type == "R", ],
      mapping = aes(x = mean_moisture, y = fit) ,
      size = 0.5,
      colour = "#8B2323",
      linetype = "solid"
    ) +
    theme_classic() +
    ggtitle("a)") +
    scale_fill_gradient(
      low = "rosybrown1",
      high = "red4",
      name = "Elevation (Rooibos)",
      breaks = c(1000, 1500, 2000)
    ) +
    scale_color_gradient(
      low = "lightgreen",
      high = "darkgreen",
      name = "Elevation (Green)",
      breaks = c(1000, 1500, 2000)
    ) +
    theme(legend.position = "bottom", legend.direction = "horizontal") +
    ylab('% Mass loss') + xlab('Soil Moisture (%)') +
    guides(
      fill = guide_colourbar(
        title.position = "top",
        title.hjust = 0.5,
        order = 2
      ),
      colour = guide_colourbar(
        title.position = "top",
        title.hjust = 0.5,
        order = 1
      )
    )
)

(
  decompVtemp_ambients <- ggplot() +
    geom_point(
      data = decomp.data.ambient[decomp.data.ambient$tea_type == "R", ],
      aes(mean_temp, Percent_mass_loss, fill = Elevation),
      pch = 21,
      size = 2,
      colour = alpha("red", 0)
    ) +
    geom_point(data = decomp.data.ambient[decomp.data.ambient$tea_type ==
                                            "G", ], aes(mean_temp, Percent_mass_loss, colour = Elevation)) +
    theme_classic() +
    scale_fill_gradient(
      low = "rosybrown1",
      high = "red4",
      name = "Elevation (Rooibos)",
      breaks = c(1000, 1500, 2000)
    ) +
    scale_color_gradient(
      low = "lightgreen",
      high = "darkgreen",
      name = "Elevation (Green)",
      breaks = c(1000, 1500, 2000)
    ) +
    geom_ribbon(
      data = environment_effects_temp[environment_effects_temp$tea_type == "G", ],
      mapping = aes(x = mean_temp, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "#006400"
    ) +
    geom_line(
      data = environment_effects_temp[environment_effects_temp$tea_type == "G", ],
      mapping = aes(x = mean_temp, y = fit) ,
      size = 0.5,
      colour = "#006400",
      linetype = "dashed"
    ) +
    geom_ribbon(
      data = environment_effects_temp[environment_effects_temp$tea_type == "R", ],
      mapping = aes(x = mean_temp, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "#8B2323"
    ) +
    geom_line(
      data = environment_effects_temp[environment_effects_temp$tea_type == "R", ],
      mapping = aes(x = mean_temp, y = fit) ,
      size = 0.5,
      colour = "#8B2323",
      linetype = "dashed"
    ) +
    theme(legend.position = "bottom", legend.direction = "horizontal") +
    ylab('% Mass loss') + xlab('Soil Temperature (°C)') +
    ggtitle("b)") +
    guides(
      fill = guide_colourbar(
        title.position = "top",
        title.hjust = 0.5,
        order = 2
      ),
      colour = guide_colourbar(
        title.position = "top",
        title.hjust = 0.5,
        order = 1
      )
    )
)

lmer_environment_scaled <-
  lmer(Percent_mass_loss ~ (tea_type * scale(mean_moisture)) + (tea_type * scale(mean_temp)) + (1 |
                                                                                                  Site),
       data = decomp.data.ambient)
summary(lmer_environment_scaled)

# Effect of treatments across treatment sites ----

treat_Sites <- c(1, 4, 7, 10)
decomp.data.treat <- decomp.data[decomp.data$Site %in% treat_Sites, ]

lmer_environment_treatments <-
  lmer(
    Percent_mass_loss ~ (tea_type * mean_moisture_post) + (tea_type * mean_temp) + (1 |
                                                                                      Site / Plot),
    decomp.data.treat
  )
summary(lmer_environment_treatments)
environment_effects_treatments <-
  as.data.frame(effect(
    c("mean_moisture_post", "tea_type"),
    lmer_environment_treatments,
    xlevels = list(mean_moisture_post = seq(
      min(decomp.data.treat$mean_moisture_post, na.rm = T),
      max(decomp.data.treat$mean_moisture_post, na.rm = T),
      .01
    ))
  ))
environment_effects_treatments_temp <-
  as.data.frame(effect(
    c("mean_temp", "tea_type"),
    lmer_environment_treatments,
    xlevels = list(mean_temp = seq(
      min(decomp.data.treat$mean_temp, na.rm = T),
      max(decomp.data.treat$mean_temp, na.rm = T),
      .01
    ))
  ))

# Plot moisture
(
  decompVmoisture_treatments <- ggplot() +
    stat_smooth(
      data = decomp.data.treat[decomp.data.treat$tea_type == "G", ],
      aes(x = mean_moisture_post, y = Percent_mass_loss, group = Site),
      method = "lm",
      se = F,
      size = 0.5,
      colour = 'black',
      alpha = 0.5
    ) +
    stat_smooth(
      data = decomp.data.treat[decomp.data.treat$tea_type == "R", ],
      aes(x = mean_moisture_post, y = Percent_mass_loss, group = Site),
      method = "lm",
      se = F,
      size = 0.5,
      colour = "black",
      alpha = 0.5
    ) +
    geom_point(
      data = decomp.data.treat[decomp.data.treat$tea_type == "R", ],
      aes(mean_moisture_post, Percent_mass_loss, fill = Elevation),
      pch = 21,
      size = 2,
      colour = alpha("red", 0)
    ) +
    geom_point(
      data = decomp.data.treat[decomp.data.treat$tea_type == "G", ],
      aes(mean_moisture_post, Percent_mass_loss, colour = Elevation)
    ) +
    theme_classic() +
    scale_fill_gradient(
      low = "rosybrown1",
      high = "red4",
      name = "Elevation (Rooibos)",
      breaks = c(1000, 1500, 2000)
    ) +
    scale_color_gradient(
      low = "lightgreen",
      high = "darkgreen",
      name = "Elevation (Green)",
      breaks = c(1000, 1500, 2000)
    ) +
    geom_ribbon(
      data = environment_effects_treatments[environment_effects_treatments$tea_type ==
                                              "G", ],
      mapping = aes(x = mean_moisture_post, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "#006400"
    ) +
    geom_line(
      data = environment_effects_treatments[environment_effects_treatments$tea_type ==
                                              "G", ],
      mapping = aes(x = mean_moisture_post, y = fit) ,
      size = 0.5,
      colour = "#006400",
      linetype = "solid"
    ) +
    geom_ribbon(
      data = environment_effects_treatments[environment_effects_treatments$tea_type ==
                                              "R", ],
      mapping = aes(x = mean_moisture_post, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "#8B2323"
    ) +
    geom_line(
      data = environment_effects_treatments[environment_effects_treatments$tea_type ==
                                              "R", ],
      mapping = aes(x = mean_moisture_post, y = fit) ,
      size = 0.5,
      colour = "#8B2323",
      linetype = "solid"
    ) +
    theme(legend.position = "bottom", legend.direction = "horizontal") +
    ylab('% Mass loss') + xlab('Soil Moisture (%)') +
    ylim(0, 80) +
    guides(
      fill = guide_colourbar(
        title.position = "top",
        title.hjust = 0.5,
        order = 2
      ),
      colour = guide_colourbar(
        title.position = "top",
        title.hjust = 0.5,
        order = 1
      )
    )
)

(
  decompVtemp_treatments <- ggplot() +
    stat_smooth(
      data = decomp.data.treat[decomp.data.treat$tea_type == "G", ],
      aes(x = mean_temp, y = Percent_mass_loss, group = Site),
      method = "lm",
      se = F,
      size = 0.5,
      colour = 'black',
      alpha = 0.5
    ) +
    stat_smooth(
      data = decomp.data.treat[decomp.data.treat$tea_type == "R", ],
      aes(x = mean_temp, y = Percent_mass_loss, group = Site),
      method = "lm",
      se = F,
      size = 0.5,
      colour = 'black',
      alpha = 0.5
    ) +
    geom_point(
      data = decomp.data.treat[decomp.data.treat$tea_type == "R", ],
      aes(mean_temp, Percent_mass_loss, fill = Elevation),
      pch = 21,
      size = 2,
      colour = alpha("red", 0)
    ) +
    geom_point(data = decomp.data.treat[decomp.data.treat$tea_type == "G", ], aes(mean_temp, Percent_mass_loss, colour =
                                                                                    Elevation)) +
    theme_classic() +
    scale_fill_gradient(
      low = "rosybrown1",
      high = "red4",
      name = "Elevation (Rooibos)",
      breaks = c(1000, 1500, 2000)
    ) +
    scale_color_gradient(
      low = "lightgreen",
      high = "darkgreen",
      name = "Elevation (Green)",
      breaks = c(1000, 1500, 2000)
    ) +
    geom_ribbon(
      data = environment_effects_treatments_temp[environment_effects_treatments_temp$tea_type ==
                                                   "G", ],
      mapping = aes(x = mean_temp, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "#006400"
    ) +
    geom_line(
      data = environment_effects_treatments_temp[environment_effects_treatments_temp$tea_type ==
                                                   "G", ],
      mapping = aes(x = mean_temp, y = fit) ,
      size = 0.5,
      colour = "#006400",
      linetype = "solid"
    ) +
    geom_ribbon(
      data = environment_effects_treatments_temp[environment_effects_treatments_temp$tea_type ==
                                                   "R", ],
      mapping = aes(x = mean_temp, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = "#8B2323"
    ) +
    geom_line(
      data = environment_effects_treatments_temp[environment_effects_treatments_temp$tea_type ==
                                                   "R", ],
      mapping = aes(x = mean_temp, y = fit) ,
      size = 0.5,
      colour = "#8B2323",
      linetype = "dashed"
    ) +
    theme(legend.position = "bottom", legend.direction = "horizontal") +
    ylab('% Mass loss') + xlab('Soil Temperature (°C)') +
    ylim(0, 80) +
    guides(
      fill = guide_colourbar(
        title.position = "top",
        title.hjust = 0.5,
        order = 2
      ),
      colour = guide_colourbar(
        title.position = "top",
        title.hjust = 0.5,
        order = 1
      )
    )
)


# 4. Does temperature correlate with moisture? ----
decomp.data.unique <- decomp.data[!duplicated(decomp.data$Plot), ]

# Run model
tempVmoist_mod <-
  lmer(mean_temp ~ mean_moisture + (1 |
                                      Site), data = decomp.data.unique)
summary(tempVmoist_mod)
r.squaredGLMM(tempVmoist_mod) # R2 not above 0.7
tempVmoist_effects <-
  as.data.frame(effect(
    "mean_moisture",
    tempVmoist_mod,
    xlevels = list(mean_moisture = seq(
      min(decomp.data.unique$mean_moisture, na.rm = T),
      max(decomp.data.unique$mean_moisture, na.rm = T),
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
    ggtitle("a)") +
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
      colour = "black"
    )
)


# 5. Add TBI variables ----

# Tea bag hydrolisable fractions:
Hg <- 0.842
Hr <- 0.552

# Add S
tea_G <- subset(decomp.data, tea_type == "G")
tea_G$S <- 1 - ((tea_G$Percent_mass_loss / 100) / Hg)
tea_G$ar <- Hr * (1 - tea_G$S)

# Add back to data
decomp.data$S <- tea_G$S[match(decomp.data$Label, tea_G$Label)]
decomp.data$ar <- tea_G$ar[match(decomp.data$Label, tea_G$Label)]

# Add k
tea_R <- subset(decomp.data, tea_type == "R")
tea_R$k <-
  log(tea_R$ar / ((1 - (
    tea_R$Percent_mass_loss / 100
  )) - (1 - tea_R$ar))) / tea_R$Days

decomp.data$k <- tea_R$k[match(decomp.data$Label, tea_R$Label)]

# a) Examine realtionship between S & variables ----

# Extract only one tea type to avoid duplicates
sk_data <- subset(decomp.data, tea_type == "R")
sk_data_ambient <-
  subset(decomp.data, tea_type == "R" & Treatment == "A")

# Relationship with environment - all ----
S_env <-
  lmer(S ~ mean_moisture + mean_temp + (1 |
                                          Site), data = sk_data_ambient)
summary(S_env)
S_env_effects <-
  as.data.frame(effect("mean_moisture", S_env, xlevels = list(mean_moisture = seq(
    min(sk_data_ambient$mean_moisture, na.rm = T),
    max(sk_data_ambient$mean_moisture, na.rm = T),
    0.1
  ))))
S_env_effects_temp <-
  as.data.frame(effect("mean_temp", S_env, xlevels = list(mean_temp = seq(
    min(sk_data_ambient$mean_temp, na.rm = T),
    max(sk_data_ambient$mean_temp, na.rm = T),
    0.1
  ))))

(
  SVmoisture <- ggplot(sk_data_ambient) +
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
      size = 0.5,
      colour = "#006400",
      linetype = "dashed"
    ) +
    scale_colour_manual(values = c("#006400", "#8B2323"))
)

# Relationship with temperature ----
(
  SVtemp <- ggplot(sk_data_ambient) +
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
  lmer(k ~ mean_moisture + mean_temp +  (1 |
                                           Site), data = sk_data_ambient)
summary(k_env)
k_env_effects <-
  as.data.frame(effect("mean_moisture", k_env, xlevels = list(mean_moisture = seq(
    min(sk_data_ambient$mean_moisture, na.rm = T),
    max(sk_data_ambient$mean_moisture, na.rm = T),
    0.1
  ))))
k_env_effects_temp <-
  as.data.frame(effect("mean_temp", k_env, xlevels = list(mean_temp = seq(
    min(sk_data_ambient$mean_temp, na.rm = T),
    max(sk_data_ambient$mean_temp, na.rm = T),
    0.1
  ))))

# Relationship with moisture ----
(
  kVmoisture <- ggplot(sk_data_ambient) +
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
      colour = "#8B2323",
      linetype = "dashed"
    ) +
    scale_colour_manual(values = c("#006400", "#8B2323"))
)

# Relationship with temperature ----
(
  kVtemp <- ggplot(sk_data_ambient) +
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

# Final Figures ----
# 1) Change in decomposition variables over gradient
pdf(file = "figures/Fig_2_single.pdf",
    width = 4,
    height = 4)
grid.arrange(decompVele_indiv_ambient, ncol = 1)
dev.off()

# Figure 2 - Change in decomposition variables over gradient -

pdf(file = "figures/Fig_2.pdf",
    width = 6,
    height = 3)
grid.arrange(
  decompVmoisture_ambients +
    theme(legend.position = "none"),
  decompVtemp_ambients +
    theme(legend.position = "none")
  ,
  ncol = 2
)
dev.off()


# Figure 3 - four panel correlation temp v moisture
pdf(file = "figures/Fig_3.pdf",
    width = 6,
    height = 5)
grid.arrange(
  moistureVele + ggtitle("a)"),
  tempVele + ggtitle("b)"),
  decompVmoisture_ambients + ggtitle("c)") +
    theme(legend.position = "none"),
  decompVtemp_ambients + ggtitle("d)") +
    theme(legend.position = "none"),
  ncol = 2
)
dev.off()

# Fig S3a - correlation temp v moisture (elevational gradient)
pdf(file = "figures/Fig_S3a.pdf",
    width = 3,
    height = 3)
grid.arrange(tempVmoist)
dev.off()


# 4) Figure S4 S & k with temp and moisture
library(grid)
S_temp <- ggplot_gtable(ggplot_build(SVtemp + ggtitle("d)")))
S_moist <- ggplot_gtable(ggplot_build(SVmoisture + ggtitle("c)")))
k_temp <- ggplot_gtable(ggplot_build(kVtemp + ggtitle("b)")))
k_moist <- ggplot_gtable(ggplot_build(kVmoisture + ggtitle("a)")))

maxWidth = unit.pmax(S_temp$widths[2:3],
                     S_moist$widths[2:3],
                     k_temp$widths[2:3],
                     k_moist$widths[2:3])

S_temp$widths[2:3] <- maxWidth
S_moist$widths[2:3] <- maxWidth
k_temp$widths[2:3] <- maxWidth
k_moist$widths[2:3] <- maxWidth

pdf(file = "figures/Fig_S4.pdf",
    width = 6,
    height = 5)
grid.arrange(S_moist, S_temp, k_moist, k_temp,  ncol = 2)
dev.off()

# Figure S6 - Differences between treatments
a <- grid.arrange(
  decompVmoisture_treatments +
    theme(legend.position = "none"),
  decompVtemp_treatments +
    theme(legend.position = "none"),
  ncol = 2
)

pdf(file = "figures/Fig_S6.pdf",
    width = 6,
    height = 11)
grid.arrange(
  box.moist + ggtitle("a)"),
  box.temp + ggtitle("b)"),
  box.decomp + ggtitle("c)"),
  a,
  ncol = 1
)
dev.off()

# Figure S2 and Supp. analysis: Air temps ----

airtemps <- read.csv("data/KP_airtemps.csv")

airtemp_mod <- lm(Temp ~ Elevation * Shade, data = airtemps)
summary(airtemp_mod)
airtemp_effects <-
  as.data.frame(effect(
    c("Elevation", "Shade"),
    airtemp_mod,
    xlevels = list(Elevation = seq(794, 1926, 1))
  ))

pdf(file = "figures/Fig_S2_Air.pdf",
    width = 5,
    height = 4)

ggplot(airtemps) +
  geom_ribbon(
    data = airtemp_effects[airtemp_effects$Shade == "shaded", ],
    mapping = aes(x = Elevation, ymin = lower, ymax = upper),
    alpha = 0.2
  ) +
  geom_line(
    data = airtemp_effects[airtemp_effects$Shade == "shaded", ],
    mapping = aes(x = Elevation, y = fit) ,
    size = 0.75
  ) +
  geom_ribbon(
    data = airtemp_effects[airtemp_effects$Shade == "unshaded", ],
    mapping = aes(x = Elevation, ymin = lower, ymax = upper),
    alpha = 0.2,
    fill = "grey50"
  ) +
  geom_line(
    data = airtemp_effects[airtemp_effects$Shade == "unshaded", ],
    mapping = aes(x = Elevation, y = fit) ,
    size = 0.75,
    colour = "white"
  ) +
  geom_point(
    aes(Elevation, Temp, fill = Shade, size = Shade),
    pch = 21,
    colour = "black",
    stroke = 0.75
  ) +
  theme_classic() +
  scale_fill_manual(
    values = c("black", "white"),
    name = "Data Source",
    labels = c("Shaded (2017)", "Unshaded (2016)")
  ) +
  scale_size_manual(values = c(1.5, 1.75),
                    guide = 'none') +
  labs(y = "Summer Air Temperature (°C)", x = "Elevation (m a.s.l.)")

dev.off()

#Alternative figures

#1 - Elevation figures
pdf(file = "figures/Fig_2_ALT.pdf",
    width = 4,
    height = 8)
grid.arrange(
  tempVele + ggtitle("a)"),
  moistureVele + ggtitle("b)"),
  decompVele_indiv_ambient + ggtitle("c)"),
  ncol = 1
)
dev.off()

#2 - Mass loss figures
pdf(file = "figures/Fig_3_ALT_top.pdf",
    width = 6,
    height = 3)
grid.arrange(
  decompVmoisture_ambients + ggtitle("a)") +
    theme(legend.position = "none"),
  decompVtemp_ambients + ggtitle("b)") +
    theme(legend.position = "none"),
  ncol = 2
)
dev.off()

#3 - S-k figures
pdf(file = "figures/Fig_4_ALT_top.pdf",
    width = 6,
    height = 5)
grid.arrange(k_moist, k_temp, S_moist, S_temp,
             ncol = 2)
dev.off()
