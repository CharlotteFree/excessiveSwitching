# Excessive switching in OCD and paranoia arises from different deficits in belief-updating
# Author: Charlotte Freeland, PhD
# Data visualization and analysis of OCD scores and PRL behavioral task parameters
# Data examined was collected and published in Suthaharan et. al, 2021

# clear environment
rm(list=ls())

# Install packages
if(!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
if(!require(stringr)) {install.packages("stingr")}; library(stringr)
if(!require(tidyverse)) {install.packages("tidyverse")}; library(tidyverse)
if(!require(corrplot)) {install.packages("corrplot")}; library(corrplot)
if(!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
if(!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
if(!require(sjPlot)) {install.packages("sjPlot")}; library(sjPlot)
if(!require(rstatix)) {install.packages("rstatix")};library(rstatix)
if (!require(scales)) {install.packages("scales")}; library(scales)
if(!require(gt)) {install.packages('gt')}; library(gt)
if(!require(lme4)) {install.packages('lme4')}; library(lme4)
if(!require(emmeans)) {install.packages('emmeans')}; library(emmeans)


# Wrangle Data ------------------------------------------------------------

# Import Data 
data1 <- read.csv("pandemicPRL.csv")  # 1010 obs of 173 variables

source("wrangle_data.R")            # source custom data wrangling function

result <- wrangleData("pandemicPRL.csv")

# Generate wrangled, clean and uncleaned dataframes
data1 <- result$data1     # no outliers removed
data2 <- result$data2     # only decision-time outliers removed (N = 399)

# convert paranoia  and OCD groups to a factor with 2 levels (low, high)
data2$paranoia_group <- as.factor(data2$paranoia_group)
data2$ocd_group <- as.factor(data2$ocd_group)


# SUPPLEMENTAL FIGURE 1 ------------------------------------------------------------

## Win-Switch and Lose-Stay Rates as a function of total PRL points earned
ggscatter(data2, x = "points_earned", y = "wsr_mean",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Total Points Scored", ylab = "Win-Switch Rate") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(margin = margin(t = 15, unit = "pt")),
        axis.title.y = element_text(margin = margin(r = 15, unit = "pt"))) +
  ylim(0, 1) 

ggsave("S1-etc-points-wsr.jpeg", height = 6, width = 7, dpi = 300)


ggscatter(data2, x = "points_earned", y = "lsr_mean",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman", 
          xlab = "Total Points Scored", ylab = "Lose-Stay Rate") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(margin = margin(t = 15, unit = "pt")),
        axis.title.y = element_text(margin = margin(r = 15, unit = "pt"))) +
  ylim(0, 1) 

ggsave("S-etc-points-lsr.jpeg", height = 6, width = 7, dpi = 300)


# Supplemental Figure 2 ---------------------------------------------------

## Correlation matrix of variables of interest 
vars <- c("rgpts_score", "docs_adjusted_score",
          "wsr_mean", "lsr_mean",
          "mu02_mean", "mu03_mean", 
          "kappa2_mean","omega2_mean", "omega3_mean")

# Create a named vector for label mapping
label_mapping <- c(`rgpts_score` = "Paranoia", 
                   `docs_adjusted_score` = "OCD",
                   `wsr_mean` = "Win-Switch", `lsr_mean` = "Lose-Stay", 
                   `mu02_mean` = "Mu2", `mu03_mean` = "Mu3", 
                   `kappa2_mean` = "Kappa",`omega2_mean` = "Omega2", 
                   `omega3_mean` = "Omega3")

# Subset the dataframe
corr_data <- data2[ , vars]

# Calculate the correlation matrix & select method (data is not normally distributed)
cor_matrix <- cor(corr_data, method = "spearman", use = "complete.obs")

# Assign new labels to variables the correlation matrix
original_labels <- names(label_mapping)

# Update row and column names with new labels
dimnames(cor_matrix) <- list(label_mapping[original_labels],
                             label_mapping[original_labels])

# Plot the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.9, 
         tl.cex = 0.8,          # Adjust this value to change text label size
         col = colorRampPalette(c("blue", "white", "red"))(200))

ggsave("07-s1-corrplot.jpeg", height = 6, width = 8, dpi = 300)


# SUPPLEMENTAL FIGURE 3 ---------------------------------------------------
## Participant Age and Sex as a function of Paranoia and OCD groups

## Sex ~ Paranoia group
data2$demo_1 <- factor(data2$demo_1, 
                       levels = c("1", "2"),
                       labels = c("Male", "Female"))

ggplot(data2, aes(x = demo_1, y = rgpts_score, 
                  fill = as.factor(demo_1))) +
  geom_point(shape= 16, color= "black", alpha= 0.2, 
             position = position_jitterdodge(),   show.legend = FALSE) +
  geom_boxplot(alpha = 0.7, width= 0.5, lwd= 0.8, 
               show.legend = FALSE) +
  stat_compare_means() +                          # Kruskal-Wallis, p = 0.43
  scale_fill_manual(name = "",
                    values = c("Male" = "darkgoldenrod", 
                               "Female" = "gold")) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 16)
  ) +
  ylim(0, 80) +                                     # range of r-GPTS scores
  labs(x = "Sex", y = "Paranoia")

ggsave("BB-sex-p.jpeg", height = 6, width = 7, dpi = 300)


## Sex ~ OCD group
ggplot(data2, aes(x = demo_1, y = docs_adjusted_score, 
                  fill = as.factor(demo_1))) +
  geom_point(shape= 16, color= "black", alpha= 0.2, 
             position = position_jitterdodge(), show.legend = FALSE) +
  geom_boxplot(alpha = 0.7, width= 0.5, lwd= 0.8, 
               show.legend = FALSE) +
  stat_compare_means() +                          # Kruskal-Wallis, p = 0.36
  scale_fill_manual(name = "",
                    values = c("Male" = "dodgerblue3", 
                               "Female" = "lightsteelblue2")) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 16)
  ) +
  ylim(0, 60) +                                     # range of DOCS scores
  labs(x = "Sex", y = "OCD")

ggsave("BB-sex-o.jpeg", height = 6, width = 7, dpi = 300)


## Age ~ Paranoia group
ggplot(data1, aes(x = paranoia_group, y = demo_2, 
                  fill = as.factor(paranoia_group))) +
  geom_point(shape= 16, color= "black", alpha= 0.1, 
             position = position_jitterdodge(), show.legend = FALSE) +
  geom_boxplot(alpha = 0.7, width= 0.5, lwd= 0.8, 
               show.legend = FALSE) +
  stat_compare_means() +                         # Wilcoxon, p < 0.0001
  scale_fill_manual(name = "",
                    values = c("low" = "lightgoldenrod2", 
                               "high" = "coral3")) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 16)
  ) +
  ylim(18, 80) +
  labs(x = "Paranoia", 
       y = "Age")

ggsave("BB-age-p.jpeg", height = 6, width = 7, dpi = 300)


## Age ~ OCD group
ggplot(data1, aes(x = ocd_group, y = demo_2, fill = as.factor(ocd_group))) +
  geom_point(shape= 16, color= "black", alpha= 0.1, 
             position = position_jitterdodge(), show.legend = FALSE) +
  geom_boxplot(alpha = 0.7, width= 0.5, lwd= 0.8, 
               show.legend = FALSE) +
  stat_compare_means() +                         
  scale_fill_manual(name = "",
                    values = c("low" = "cadetblue3", 
                               "high" = "mediumpurple")) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 16)) +
  labs(x = "OCD", 
       y = "Age") +
  ylim(18, 80) 
  
ggsave("BB-age-o.jpeg", height = 6, width = 7, dpi = 300)


# SUPPLEMENTAL FIGURE 4---------------------------------------------------

## Total Points Earned and Choice Latency as a function of Paranoia and OCD groups
## Visualize data distribution
hist(data2$points_earned, 
     main = NULL, 
     xlab = "Points Earned")

# Shapiro-Wilk test to assess normality distributions
shapiro.test(data2$points_earned)  

# Levene's test for homogeneity of variance across groups
lt_points <- leveneTest(points_earned ~ paranoia_group * ocd_group, 
                     data = data2, center = median)

tidy(lt_points, conf.int = TRUE) %>%
  gt()

### GLM  approach without task block as predictor in the model
mod2_glm_points <- glm(points_earned ~ paranoia_group * ocd_group,
                       data = data2, family = gaussian(link = "identity"))
## Assess model fit
summary(mod2_glm_points)

# Plot model diagnostics
plot(mod2_glm_points)

# Plot histogram of residuals
hist(residuals(mod2_glm_points))

# Check multicollinearity with variance inflation factor (VIF)
vif(mod2_glm_points)

# Create and display diagnostic plots
source("createDiagnosticPlots.R")
model <- mod2_glm_points
create_diagnostic_plots(model)

# Tidy table of model coefficients
points_coefficients <- tidy(mod2_glm_points) 

points_tidy_coefficients <- points_coefficients %>%
  mutate_if(is.numeric, round, digits = 3)

points_tidy_coefficients  %>%
  gt() %>%
  tab_header(title = "GLM Summary: Points Earned ~ Paranoia group * OCD group") %>%
  cols_label(
    term = "Predictor",
    estimate = "β",
    std.error = "SE",
    statistic = "t",
    p.value = "p")  

## Post-hoc comparisons using estimated marginal means
points_emm <- emmeans(mod2_glm_points, ~ paranoia_group * ocd_group)
points_emm

# Total Rewards/Points Earned ~ Paranoia group * OCD group
points_emm %>% as_tibble() %>% 
  ggplot(aes(x = paranoia_group, y = emmean,
             color = ocd_group)) +
  geom_point(size = 4,
             position = position_dodge(width = 0.75),
             shape = 5, stroke = 1.5,
             show.legend = F) +
  geom_errorbar(aes(ymin = lower.CL,
                    ymax = upper.CL),
                position = position_dodge(width = 0.75),
                width = 0, size = 1,
                show.legend = T) +
  labs(x = "Paranoia",
       y = "Total Points Earned" ,
       color = "OCD") +
  scale_color_manual(
    values = c("low" = "royalblue2", "high" = "chocolate1"),
    name = "OCD") +
  theme_minimal(base_size = 16) +
  theme(axis.title.y = element_text(size = 18)) +
  ylim(6400,7600)

ggsave("Supp-Points.jpeg", height = 6, width = 8, dpi = 300)

# Pairwise comparisons of EMMeans, with Tukey adjustment for multiple comparisons
points_pairs <- pairs(points_emm, adjust = "tukey")
points_pairs 

points_pairs <- as.data.frame(points_pairs)

# Convert EMMeans contrasts to dataframe
outputP <- data.frame(points_pairs$contrast, round(points_pairs$estimate,2), 
                       round(points_pairs$SE,2), round(points_pairs$t.ratio,2),
                       round(points_pairs$p.value,3))

names(outputP) <- c("Contrasts", "Estimate", "SE", 
                     "t-ratio","p-value")

outputP$`p-value` <- ifelse(outputP$`p-value`<0.001, "<0.001",
                             round(outputP$`p-value`, 3))

# Tidy output table of EMMeans pairwise contrasts
outputP %>%
  gt()%>%
  tab_header(title = "Pairwise Comparisons of Estimated Marginal Means 
             for Points Earned")


# Modeling Average Decision Time/Choice Latency -----------------------------------------------------------------

# Average Decision Time/Choice Latency  ~ Paranoia group
## Visualize data distribution
hist(data2$avg_rt_total, 
     main = NULL, 
     xlab = "Choice Latency")
# left-skewed, positive values

## Gather data into long format
data3 <- data2 %>% 
  pivot_longer(cols = c(avg_rt_block1, avg_rt_block2),
               names_to = "block",
               values_to = "avg_rt") %>% 
  dplyr::select(study_id, avg_rt, block, paranoia_group,
                ocd_group, rgpts_score, docs_adjusted_score) %>% 
  mutate(block = factor(block),
         n = 80)

## Visualize data distribution
hist(data3$avg_rt, 
     main = NULL, 
     xlab = "Choice Latency")
# highly left-skewed, positive values

# Shapiro-Wilk test to assess normality distributions
shapiro.test(data3$avg_rt)  
# non-normal distribution

### GLM  approach without task block as predictor in the model
mod1_glm_rt <- glm(avg_rt ~ paranoia_group * ocd_group,
                   data = data3, family = Gamma(link = "identity"))

## Assess model fit
summary(mod1_glm_rt)

# Plot model diagnostics
plot(mod1_glm_rt)

# Plot histogram of residuals
hist(residuals(mod1_glm_rt))

# Check multicollinearity with variance inflation factor (VIF)
vif(mod1_glm_rt)

# Create and display diagnostic plots
source("createDiagnosticPlots.R")
model <- mod1_glm_rt
create_diagnostic_plots(model)

# Tidy table of model coefficients
rt_mod1_coefficients <- tidy(mod1_glm_rt)

rt1_tidy_coefficients <- rt_mod1_coefficients %>%
  mutate_if(is.numeric, round, digits = 3)

rt1_tidy_coefficients  %>%
  gt() %>%
  tab_header(title = "GLM Summary: RT ~ Paranoia * OCD") %>%
  cols_label(
    term = "Predictor",
    estimate = "β",
    std.error = "SE",
    statistic = "t",
    p.value = "p")  

## Post-hoc comparisons using Estimated Marginal Means
rt_emm <- emmeans(mod1_glm_rt, ~ paranoia_group * ocd_group) 

# Pairwise comparisons of EMMeans, with Tukey adjustment for multiple comparisons 
rt_pairs <- pairs(rt_emm, adjust = "tukey")
rt_pairs

rt_pairs <- as.data.frame(rt_pairs)

# Convert EMMeans contrasts to dataframe
output1P <- data.frame(rt_pairs$contrast, round(rt_pairs$estimate,2), 
                       round(rt_pairs$SE,2), rt_pairs$df,
                       round(rt_pairs$t.ratio,2), round(rt_pairs$p.value,3))

names(output1P) <- c("Contrast", "Estimate", "SE", "df", "t-ratio",
                     "p-value")

output1P$`p-value` <- ifelse(output1P$`p-value`<0.001, "<0.001",
                             round(output1P$`p-value`, 3))

# Tidy output table of EMMeans pairwise contrasts
output1P %>%
  gt()%>%
  tab_header(title = "Pairwise Comparisons of Estimated Marginal Means 
             for Choice Latency")

# REgrid EMMeans
rt_emm <- emmeans(mod1_glm_rt, ~ paranoia_group * ocd_group) 

# Plot EMMeans for Choice Latency by paranoia and OCD groups
rt_emm %>% as_tibble() %>% 
  ggplot(aes(x = paranoia_group, y = emmean,
             color = ocd_group)) +
  geom_point(size = 4,
             position = position_dodge(width = 0.75),
             shape = 9, stroke = 1.5,
             show.legend = T) +
  geom_errorbar(aes(ymin = lower.CL,
                    ymax = upper.CL),
                position = position_dodge(width = 0.75),
                width = 0, linewidth = 1, 
                show.legend = F) +
  labs(x = "Paranoia",
       y = "Choice Latency (ms)",
       color = "OCD") +
  scale_color_manual(
    values = c("low" = "royalblue2", "high" = "chocolate1"),
    name = "OCD") +
  theme_minimal(base_size = 16) +
  theme(axis.title.y = element_text(size = 18)) +
  ylim(300, 800)

ggsave("Supp-CL.jpeg", height = 6, width = 8, dpi = 300)
  
