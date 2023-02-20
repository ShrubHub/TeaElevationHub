# Tea bag elevational gradient script
# Eleanor Walker // Haydn Thomas // Isla Myers-Smith, 20/03/2018 - 20/02/2023

# Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
se <- function(x)
    sqrt(var(x, na.rm = T) / length(x))

# COMBINED: Importing data----
Combined_Tea <-
    read.csv(
        "data/Tea_data/Hypothesis_2_Combined_Data.csv"
    )

# Hypothesis 2; plateau distinction of decomposition rates ----

# Using a t-test to test the signifcance of this distinction
# Subsetting the data
green_Comb <- subset(Combined_Tea, Colour == "G")
red_Comb <- subset(Combined_Tea, Colour == 'R')

# Distribution of mass loss data
hist.comb.g <- (green_Comb$Mass_Loss)
qqnorm(hist.comb.g)
qqline(hist.comb.g)
hist.comb.r <- (red_Comb$Mass_Loss)
qqnorm(hist.comb.r)
qqline(hist.comb.r)

# T-test
Comb_t_test <-
    t.test(
        green_Comb$Mass_Loss,
        red_Comb$Mass_Loss,
        paired = FALSE,
        equal.variance = FALSE
    )
Comb_t_test

# Means of teas
mean(red_Comb$Mass_Loss)
sd(red_Comb$Mass_Loss)
mean(green_Comb$Mass_Loss)
sd(green_Comb$Mass_Loss)

# comparing the variance of the two teas (how far numbers are spread about the mean)
var.test(lm(green_Comb$Mass_Loss ~ 1), lm(red_Comb$Mass_Loss ~ 1))

# Making a histogram of the % mass loss
(
    Combined_Tea_hist <-
        ggplot(
            Combined_Tea,
            aes(x = percentage_mass_loss, colour = Colour, fill = Colour)
        ) +
        geom_histogram(
            aes(y = (..density.. * 100) * 4),
            position = "identity",
            binwidth = 4,
            alpha = .5
        ) +
        geom_density(alpha = .3, adjust = 2, aes(y = (..density.. * 100) * 4)) +
        scale_colour_manual(values = c("#006400", "#8B2323")) +
        scale_fill_manual(values = c("#006400", "#8B2323")) +
        theme_classic() +
        ylab("Proportion of tea (%)") +
        xlim(0, 100) +
        xlab("% Mass loss") +
        ggtitle("c) Combined data") +
        theme(legend.position = "none") +
        theme(legend.title = element_blank())
)

# PLATEAU: Importing data----
Plateau_Tea <-
    read.csv(
        "data/Tea_data/El_Plateau_Tea_EDIT.csv"
    )
Plateau_Tea_a <- filter(Plateau_Tea, Treatment == "A")

# Using a t-test to test the signifcance of this distinction
# Subsetting the data to separate green and red
green_plat <- subset(Plateau_Tea, tea_type == 'G')
red_plat <- subset(Plateau_Tea, tea_type == 'R')

green_plat_a <- subset(Plateau_Tea_a, tea_type == 'G')
red_plat_a <- subset(Plateau_Tea_a, tea_type == 'R')

# Testing the distribution of the data
hist.P.g <- (green_plat$Mass_loss)
qqnorm(hist.P.g)
qqline(hist.P.g)
hist.P.r <- (red_plat$Mass_loss)
qqnorm(hist.P.r)
qqline(hist.P.r)

# Means
mean(green_plat_a$Mass_loss)
sd(green_plat_a$Mass_loss)

mean(red_plat_a$Mass_loss)
sd(red_plat_a$Mass_loss)

# T-test
Plat_t_test <-
    t.test(
        green_plat$Mass_loss,
        red_plat$Mass_loss,
        paired = FALSE,
        equal.variance = FALSE
    )
Plat_t_test

# comparing the variance of the two teas (how far numbers are spread about the mean)
var.test(lm(green_plat$Mass_loss ~ 1), lm(red_plat$Mass_loss ~ 1))

# Making a histogram of the % mass loss
(
    Plateau_Tea_hist <-
        ggplot(
            Plateau_Tea,
            aes(x = Percent_mass_loss, colour = tea_type, fill = tea_type)
        )  +
        geom_histogram(
            aes(y = ..density.. * 100),
            position = "identity",
            binwidth = 4,
            alpha = .5
        ) +
        geom_density(alpha = .2, adjust = 2, aes(y = ..density.. * 100)) +
        scale_colour_manual(values = c("#006400", "#8B2323")) +
        scale_fill_manual(values = c("#006400", "#8B2323")) +
        theme_classic() +
        ylab("Proportion of tea (%)") +
        xlab("% Mass loss") +
        xlim(0, 100) +
        ggtitle("c) Elevational gradient all treatments") +
        theme(legend.position = "none")
)

(
    Plateau_Tea_A_hist <-
        ggplot(
            Plateau_Tea_a,
            aes(x = Percent_mass_loss, colour = tea_type, fill = tea_type)
        )  +
        geom_histogram(
            aes(y = (..density.. * 100) * 4),
            position = "identity",
            binwidth = 4,
            alpha = .5
        ) +
        geom_density(alpha = .2, adjust = 2, aes(y = (..density.. * 100) * 4)) +
        scale_colour_manual(values = c("#006400", "#8B2323")) +
        scale_fill_manual(values = c("#006400", "#8B2323")) +
        theme_classic() +
        ylab("Proportion of tea (%)") +
        xlab("% Mass loss") +
        xlim(0, 100) +
        ggtitle("a) Elevational gradient") +
        theme(legend.position = "none")
)

# CG: Importing data ----
CG_Tea  <-
    read.csv("data/Tea_data/El_CG_Tea.csv")

# Using a t-test to test the signifcance of this distinction
# Subsetting the data
green_CG <- subset(CG_Tea, Colour == 'G')
red_CG <- subset(CG_Tea, Colour == 'R')

# Testing the distribution
hist.CG.g <- (green_CG$Mass_Loss) # skewed to the left
qqnorm(hist.CG.g)
qqline(hist.CG.g)
hist.CG.r <- (red_CG$Mass_Loss)
qqnorm(hist.CG.r)
qqline(hist.CG.r)

# T-test
CG_t_test <-
    t.test(
        green_CG$Mass_Loss,
        red_CG$Mass_Loss,
        paired = FALSE,
        equal.variance = FALSE
    )
CG_t_test

# Means
mean(green_CG$Mass_Loss)
sd(green_CG$Mass_Loss)

mean(red_CG$Mass_Loss)
sd(red_CG$Mass_Loss)

#Comparing across experiments

green_t_test <-
    t.test(
        green_CG$Mass_Loss,
        green_plat_a$Mass_loss,
        paired = FALSE,
        equal.variance = FALSE
    )
green_t_test

red_t_test <-
    t.test(
        red_CG$Mass_Loss,
        red_plat_a$Mass_loss,
        paired = FALSE,
        equal.variance = FALSE
    )
red_t_test

# comparing the variance of the two teas (how far numbers are spread about the mean)
var.test(lm(green_CG$Mass_Loss ~ 1), lm(red_CG$Mass_Loss ~ 1))

# Making a histogram of the % mass loss
(
    CG_Tea_hist <-
        ggplot(CG_Tea, aes(
            x = percent_mass_loss, colour = Colour, fill = Colour
        )) +
        geom_histogram(
            aes(y = (..density.. * 100) * 4),
            position = "identity",
            binwidth = 4,
            alpha = .5
        ) +
        geom_density(alpha = .2, adjust = 2, aes(y = (..density.. * 100) * 4)) +
        scale_colour_manual(values = c("#006400", "#8B2323")) +
        scale_fill_manual(values = c("#006400", "#8B2323")) +
        theme_classic() +
        ylab("Proportion of tea (%)") +
        xlab("% Mass loss") +
        xlim(0, 100) +
        ggtitle("b) Manipulation experiment") +
        theme(legend.position = "none")
)

# Arranging graphs
pdf(file = "figures/Fig_5.pdf",
    width = 14,
    height = 5)
panel <-
    grid.arrange(Plateau_Tea_A_hist, CG_Tea_hist, Combined_Tea_hist, ncol =
                     3)
dev.off()
