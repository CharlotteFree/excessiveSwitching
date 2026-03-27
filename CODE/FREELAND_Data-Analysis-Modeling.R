# Excessive switching in OCD and paranoia arises from different deficits in belief-updating
# Author: Charlotte Freeland, PhD
# Data Analysis & Statistics
# Data examined was collected and published in Suthaharan et. al, 2021

# clear environment
rm(list=ls())

# Install packages
if(!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
if(!require(tidyverse)) {install.packages("tidyverse")}; library(tidyverse)
if(!require(stringr)) {install.packages("stingr")}; library(stringr)
if(!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
if(!require(rstatix)) {install.packages("rstatix")}; library(rstatix)
if(!require(sjPlot)) {install.packages("sjPlot")}; library(sjPlot)
if(!require(ggplot2)) {install.packages('ggplot2')}; library(ggplot2)
if(!require(gt)) {install.packages('gt')}; library(gt)
if(!require(lme4)) {install.packages('lme4')}; library(lme4)
if(!require(emmeans)) {install.packages('emmeans')}; library(emmeans)
if(!require(car)) {install.packages("car")}; library(car)
if(!require(broom)) {install.packages('broom')}; library(broom)
if(!require(gridExtra)) {install.packages('gridExtra')}; library(gridExtra)


# Wrangle Data ------------------------------------------------------------
## warangleData is a custom function created to filter this large dataset

# Import Data 
data1 <- read.csv("pandemicPRL.csv")  # 1010 obs of 173 variables

# Source data wrangling function
source("wrangle_data.R")             # source custom data wrangling function

# Generate wrangled, clean and uncleaned dataframes
result <- wrangleData("pandemicPRL.csv")

data1 <- result$data1     # no outliers removed
data2 <- result$data2     # only decision-time outliers removed (N = 399)
data3 <- result$data3     # clean df with 5 outliers removed from 2 variables

# convert paranoia  and OCD groups to a factor with 2 levels (low, high)
data2$paranoia_group <- as.factor(data2$paranoia_group)
data2$ocd_group <- as.factor(data2$ocd_group)

data3$paranoia_group <- as.factor(data3$paranoia_group)
data3$ocd_group <- as.factor(data3$ocd_group)

# Figure 2 -----------------------------------------------------------------
# Correlation scatter plots with Spearman rank correlation test 
# Spearman's correlation coefficient (rho = -1 to 1)

# FIGURE 2A
ggscatter(data3, x = "rgpts_score", y = "docs_adjusted_score",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Paranoia", ylab = "OCD") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

ggsave("2A-corrplot-ocd-paranoia.jpeg", height = 6, width = 8, dpi = 300)

# FIGURE 2B
ggplot(data3, aes(x = paranoia_severity, y = docs_adjusted_score,
                  fill = as.factor(paranoia_severity))) +
  geom_jitter(shape = 16,
              color = "black",
              alpha = 0.1, show.legend = F,
              width = .2, size = 2) +
  geom_boxplot(alpha = 0.7, width= 0.5, lwd= 0.8, show.legend = FALSE) +
  # stat_compare_means() +
  scale_fill_manual(name = "",
                    values = c("Low" = "#F4EDCA", 
                               "Elevated" = "khaki2",
                               "Mod Severe" = "coral1", 
                               "Severe" = "firebrick2", 
                               "Very Severe" = "firebrick4")) +
  theme_classic() +
  theme(text = element_text(size = 14)) +
  theme(axis.text = element_text(size = rel(0.9))) +
  ylim(-0.5, 60) +                              # OCD adjusted score range 0-60
  labs(x = "Paranoia", y = "OCD Score")

ggsave("2B-ocd-paranoia-levels.jpeg", height = 6, width = 8, dpi = 300)



# Modeling Win-Switch Rate ------------------------------------------------

## Gather data into long format
data3.1 <- data2 %>% 
  pivot_longer(cols = c(wsr_block1, wsr_block2),
               names_to = "block",
               values_to = "wsr") %>% 
  dplyr::select(study_id, wsr, block, paranoia_group,
         ocd_group, rgpts_score, docs_adjusted_score) %>% 
  mutate(block = factor(block),
         n = 80)

## Test assumptions for normality and homoscedasticity
# Shapiro-Wilk test to assess normality distributions
shapiro.test(data3.1$wsr)    
# p < 0.0001, non-normally distributed

# Levene's test for homogeneity of variance across groups
lt_wsr <- leveneTest(wsr ~ block * paranoia_group * ocd_group, 
                     data = data3.1, center = median)

tidy(lt_wsr, conf.int = TRUE) %>%
  gt()
# p < 0.0001, equal variances cannot be assumed

## Visualize data distribution
hist(data3.1$wsr, 
     main = NULL, 
     xlab = "Win-Switch Rate")
# not normally distributed, left-skewed, values 0-1


### Generalized Linear Model (GLM) approach for 
### modeling Win-Switch rate as a function of paranoia and OCD
mod1_glm_wsr <- glm(wsr ~ block * paranoia_group * ocd_group,
               data = data3.1, family = quasibinomial())

## Assess model fit
summary(mod1_glm_wsr)
## no effect of block (p = 0.449)

# Plot model diagnostics
plot(mod1_glm_wsr)

# Create and display diagnostic plots
source("createDiagnosticPlots.R")
model <- mod1_glm_wsr
create_diagnostic_plots(model)

# Plot residuals
hist(resid(mod1_glm_wsr),
     main = "Histogram of Model Residuals", 
     xlab = "WSR ~ Block * Paranoia Group * OCD group")

# Check multicollinearity with variance inflation factor (VIF)
vif(mod1_glm_wsr)

# Tidy table of model coefficients
wsr_mod1_coefficients <- tidy(mod1_glm_wsr)

wsr1_tidy_coefficients <- wsr_mod1_coefficients %>%
  mutate_if(is.numeric, round, digits = 3)

wsr1_tidy_coefficients  %>%
  gt() %>%
  tab_header(title = "GLM Summary: Win-Switch Rate ~ Block * Paranoia * OCD") %>%
  cols_label(
    term = "Predictor",
    estimate = "β",
    std.error = "SE",
    statistic = "t",
    p.value = "p")  


### GLM  approach without task block as predictor in the model
mod2_glm_wsr <- glm(wsr ~ paranoia_group * ocd_group,
                data = data3.1, family = quasibinomial())

## Assess model fit
summary(mod2_glm_wsr)

# Plot model diagnostics
plot(mod2_glm_wsr)

# Create and display diagnostic plots
source("createDiagnosticPlots.R")
model <- mod2_glm_wsr
create_diagnostic_plots(model)

# Plot histogram of residuals
hist(residuals(mod2_glm_wsr),
     main = "Histogram of Model Residuals", 
     xlab = "WSR ~ Paranoia Group * OCD group")

# Check multicollinearity with variance inflation factor (VIF)
vif(mod2_glm_wsr)
# vif = 5.9 suggests multicollinearity between paranoia and ocd groups

# Tidy table of model coefficients
wsr_mod2_coefficients <- tidy(mod2_glm_wsr)

wsr2_tidy_coefficients <- wsr_mod2_coefficients %>%
  mutate_if(is.numeric, round, digits = 3)

wsr2_tidy_coefficients  %>%
  gt() %>%
  tab_header(title = "GLM Summary: WSR ~ Paranoia * OCD") %>%
  cols_label(
    term = "Predictor",
    estimate = "β",
    std.error = "SE",
    statistic = "t",
    p.value = "p")  

## Post-hoc comparisons using Estimated Marginal Means
wsr_emm <- emmeans(mod2_glm_wsr, ~ paranoia_group * ocd_group) 
wsr_emm

# Convert EMMean contrasts to dataframe
wsr_emm <- as.data.frame(wsr_emm)

output1 <- data.frame(wsr_emm$paranoia_group, wsr_emm$ocd_group,
                      round(wsr_emm$emmean,2), round(wsr_emm$SE,2), wsr_emm$df,
                      round(wsr_emm$asymp.LCL,2), round(wsr_emm$asymp.UCL,2))

names(output1) <- c("Paranoia", "OCD", "EMMeans", "SE", "df",
                    "Lower CI", "Upper CI")

# Tidy output table of EMMeans coefficients
output1 %>%
  gt()%>%
  tab_header(title = "Estimated Marginal Means for Win-Switch Rate") 
#Note: results on log (not response) scale

# Regrid EMMeans
wsr_emm <- emmeans(mod2_glm_wsr, ~ paranoia_group * ocd_group) 

# Pairwise comparisons of EMMeans, with Tukey adjustment for multiple comparisons 
wsr_pairs <- pairs(wsr_emm, adjust = "tukey")
wsr_pairs

wsr_pairs <- as.data.frame(wsr_pairs)

# Convert EMMeans contrasts to dataframe
output1P <- data.frame(wsr_pairs$contrast, round(wsr_pairs$estimate,2), 
                      round(wsr_pairs$SE,2), wsr_pairs$df,
                      round(wsr_pairs$z.ratio,2), round(wsr_pairs$p.value,3))

names(output1P) <- c("Contrast", "Estimate", "SE", "df", "z-ratio",
                    "p-value")

output1P$`p-value` <- ifelse(output1P$`p-value`<0.001, "<0.001",
                            round(output1P$`p-value`, 3))

# Tidy output table of EMMeans pairwise contrasts
output1P %>%
  gt()%>%
  tab_header(title = "Pairwise Comparisons of Estimated Marginal Means 
             for Win-Switch Rate")
#Note: results on log (not response) scale

## Plot Figure 3A: Estimated Marginal Means for Win-Switch Rate
wsr_emm %>% as_tibble() %>% 
  ggplot(aes(x = paranoia_group, y = exp(emmean),
             color = ocd_group)) +
  geom_point(size = 4,
             position = position_dodge(width = 0.75),
             shape = 9, stroke = 1.5,
             show.legend = F) +
  geom_errorbar(aes(ymin = exp(asymp.LCL),
                    ymax = exp(asymp.UCL)),
                position = position_dodge(width = 0.75),
                width = 0, linewidth = 1, 
                show.legend = F) +
  labs(x = "Paranoia",
       y = "Win-Switch Rate",
       color = "OCD") +
  scale_color_manual(
    values = c("low" = "royalblue2", "high" = "chocolate1"),
    name = "OCD") +
  theme_minimal(base_size = 16) +
  theme(axis.title.y = element_text(size = 20)) +
  ylim(0, 0.35)

ggsave("A-wsr.jpeg", dpi = 300)


# Modeling Lose-Stay Rate -------------------------------------------------

## Gather data into long format
data4 <- data2 %>% 
  pivot_longer(cols = c(lsr_block1, lsr_block2),
               names_to = "block",
               values_to = "lsr") %>% 
  dplyr::select(lsr, block, paranoia_group,
         ocd_group, rgpts_score, docs_adjusted_score) %>% 
  mutate(block = factor(block),
         n = 80)

## Test assumptions for normality and homoscedasticity
# Shapiro-Wilk test to assess normality distributions
shapiro.test(data4$lsr)    
# p < 0.0001, non-normally distributed

# Levene's test for homogeneity of variance across groups
lt_lsr <- leveneTest(lsr ~ paranoia_group * ocd_group, 
                     data = data4, center = median)

tidy(lt_lsr, conf.int = TRUE) %>%
  gt()
# p < 0.0001, equal variances cannot be assumed

# Visualize data distribution
hist(data4$lsr, 
     main = NULL, 
     xlab = "Lose-Stay Rate")
# not normally distributed, left-skewed, values 0-1


### GLM  approach for modeling Lose-Stay rate as a function of block, paranoia and OCD
mod1_glm_lsr <- glm(lsr ~ block * paranoia_group * ocd_group,
                    data = data4, family = quasibinomial())

## Assess model fit
summary(mod1_glm_lsr)
## no effect of block (p = 0.715)

# Plot model diagnostics
plot(mod1_glm_lsr)

# Create and display diagnostic plots
source("createDiagnosticPlots.R")
model <- mod1_glm_lsr
create_diagnostic_plots(model)

# Plot residuals
hist(resid(mod1_glm_lsr))

# Check multicollinearity with variance inflation factor (VIF)
vif(mod1_glm_lsr)
# including block as a factor in the model doubles the VIF


### GLM  approach without task block as predictor in the model
mod2_glm_lsr <- glm(lsr ~ paranoia_group * ocd_group,
                    data = data4, family = quasibinomial())

## Assess model fit
summary(mod2_glm_lsr)

# Plot model diagnostics
plot(mod2_glm_lsr)

# Create and display diagnostic plots
source("createDiagnosticPlots.R")
model <- mod2_glm_lsr
create_diagnostic_plots(model)

# Plot residuals
hist(residuals(mod2_glm_lsr))

# Check multicollinearity with variance inflation factor (VIF)
vif(mod2_glm_lsr)
# vif = 3.5 suggests multicollinearity between paranoia and ocd groups

# Tidy table of model coefficients
lsr_coefficients <- tidy(mod2_glm_lsr)

lsr_tidy_coefficients <- lsr_coefficients %>%
  mutate_if(is.numeric, round, digits = 3)

lsr_tidy_coefficients  %>%
  gt() %>%
  tab_header(title = "GLM Summary: LSR ~ Paranoia group * OCD group") %>%
  cols_label(
    term = "Predictor",
    estimate = "β",
    std.error = "SE",
    statistic = "t",
    p.value = "p")  

## Post-hoc comparisons using Estimated Marginal Means
lsr_emm <- emmeans(mod2_glm_lsr, ~ paranoia_group * ocd_group)
lsr_emm

lsr_emm <- as.data.frame(lsr_emm)

# convert estimated marginal mean contrasts to dataframe
output2 <- data.frame(lsr_emm$paranoia_group, lsr_emm$ocd_group,
                      round(lsr_emm$emmean,2), round(lsr_emm$SE,2), lsr_emm$df,
                      round(lsr_emm$asymp.LCL,2), round(lsr_emm$asymp.UCL,2))

names(output2) <- c("Paranoia","OCD","EMMeans","SE", "df",
                    "Lower CI","UpperCI")

# Tidy table output of EMMeans contrasts
output2 %>%
  gt()%>%
  tab_header(title = "Estimated Marginal Means 
             for Lose-Stay Rate")
#Note: results on log (not response) scale

# Regrid EMMeans
lsr_emm <- emmeans(mod2_glm_lsr, ~ paranoia_group * ocd_group)

# Pairwise comparisons of EMMeans, with Tukey adjustment for multiple comparisons 
lsr_pairs <- pairs(lsr_emm, adjust = "tukey")
lsr_pairs

lsr_pairs <- as.data.frame(lsr_pairs)

# Convert EMMeans contrasts to dataframe
output2P <- data.frame(lsr_pairs$contrast, round(lsr_pairs$estimate,2), 
                      round(lsr_pairs$SE,2), lsr_pairs$df, 
                      round(lsr_pairs$z.ratio,2), round(lsr_pairs$p.value,3))

names(output2P) <- c("Contrast", "Estimate", "SE", "df", "z-ratio",
                    "p-value")

output2P$`p-value` <- ifelse(output2P$`p-value`<0.001, "<0.001",
                            round(output2P$`p-value`, 3))

# Tidy output table of EMMeans pairwise contrasts
output2P %>%
  gt()%>%
  tab_header(title = "Pairwise Comparisons of Estimated Marginal Means 
             for Lose-Stay Rate")
#Note: results on log (not response) scale


## Plot Figure 3B: estimated marginal means for Lose-Stay rate
lsr_emm %>% as_tibble() %>% 
  ggplot(aes(x = paranoia_group, y = exp(emmean),
             color = ocd_group)) +
  geom_point(size = 4,
             position = position_dodge(width = 0.75),
             shape = 5, stroke = 1.5,
             show.legend = F) +
  geom_errorbar(aes(ymin = exp(asymp.LCL),
                    ymax = exp(asymp.UCL)),
                position = position_dodge(width = 0.75),
                width = 0, linewidth = 1, 
                show.legend = T) +
  labs(x = "Paranoia",
       y = "Lose-Stay Rate",
       color = "OCD") +
  scale_color_manual(
    values = c("low" = "royalblue", "high" = "chocolate1"),
    name = "OCD") +
  ylim(0.15, 0.55) +
  theme_minimal(base_size = 16) +
  theme(axis.title.y = element_text(size = 20)) 

ggsave("A-lsr.jpeg", dpi = 300)



# MODELING HGF PARAMETERS  ------------------------------------------------

###### Mu02

## Gather data into long format
data5 <- data2 %>% 
  pivot_longer(cols = c(mu02_1, mu02_2),
               names_to = "block",
               values_to = "mu02") %>% 
  dplyr::select(mu02, block, paranoia_group,
         ocd_group, rgpts_score, docs_adjusted_score) %>% 
  mutate(block = factor(block),
         n = 80)

range(data5$mu02)
# Response variable has negative and positive values

### Test assumptions for normality and homoscedasticity ###
# Visualize data distribution
hist(data5$mu02)

ggscatter(data5, x = "rgpts_score", y = "mu02",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Paranoia", ylab = "Mu02") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

ggscatter(data5, x = "docs_adjusted_score", y = "mu02",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "OCD", ylab = "Mu02") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Shapiro-Wilk test to assess normality distributions
shapiro.test(data5$mu02)    
# Response variable is not normally distributed

# Levene's test for homogeneity of variance across groups
lt_mu02 <- leveneTest(mu02 ~ paranoia_group * ocd_group, 
                      data = data5, center = median)

tidy(lt_mu02, conf.int = TRUE) %>%
  gt()
# p < 0.0001, equal variances cannot be assumed


### GLM  approach for modeling Mu02 as a function of paranoia and OCD
mod1_glm_mu02 <- glm(mu02 ~ paranoia_group * ocd_group,
                     data = data5, family = gaussian(link = "identity"))

mod2_glm_mu02 <- glm(mu02 ~ paranoia_group * ocd_group,
                     data = data5, family = poisson(link = "logit"))
## Assess model fit
summary(mod1_glm_mu02)

# Plot model diagnostics
plot(mod1_glm_mu02)

# Plot residuals
hist(residuals(mod1_glm_mu02))

# Create and display diagnostic plots
source("createDiagnosticPlots.R")
model <- mod1_glm_mu02
create_diagnostic_plots(model)

# Check multicollinearity with variance inflation factor (VIF)
vif(mod1_glm_mu02)

# Tidy table of model coefficients
mu02_coefficients <- tidy(mod1_glm_mu02)

mu02_tidy_coefficients <- mu02_coefficients %>%
  mutate_if(is.numeric, round, digits = 3)

mu02_tidy_coefficients  %>%
  gt() %>%
  tab_header(title = "GLM Summary for Mu02 ~ Paranoia group * OCD group") %>%
  cols_label(
    term = "Predictor",
    estimate = "β",
    std.error = "SE",
    statistic = "t",
    p.value = "p")  

## Post-hoc comparisons using estimated marginal means
mu02_emm <- emmeans(mod1_glm_mu02, ~ paranoia_group * ocd_group)
mu02_emm

mu02_emm <- as.data.frame(mu02_emm)

# Convert estimated marginal means to dataframe
output3 <- data.frame(mu02_emm$paranoia_group, mu02_emm$ocd_group,
                      round(mu02_emm$emmean,2),round(mu02_emm$SE,2), 
                      mu02_emm$df, round(mu02_emm$lower.CL,2), 
                      round(mu02_emm$upper.CL,2))

names(output3) <- c("Paranoia", "OCD", "EMMeans", "SE",
                    "df","Lower CI", "Upper CI")

# Tidy output table of EMMeans coefficients
output3 %>%
  gt()%>%
  tab_header(title = "Estimated Marginal Means for Mu2")

# Regrid EMMeans
mu02_emm <- emmeans(mod1_glm_mu02, ~ paranoia_group * ocd_group)

# Pairwise comparisons of EMMeans, with Tukey adjustment for multiple comparisons 
mu02_pairs <- pairs(mu02_emm, adjust = "tukey")
mu02_pairs

mu02_pairs <- as.data.frame(mu02_pairs)

# Convert EMMeans contrasts to dataframe
output3P <- data.frame(mu02_pairs$contrast, round(mu02_pairs$estimate,2), 
                      round(mu02_pairs$SE,2), round(mu02_pairs$df), 
                      round(mu02_pairs$t.ratio,2), round(mu02_pairs$p.value,3))

names(output3P) <- c("Contrast", "Estimate", "SE", "DF", "t-ratio",
                    "p-value")

output3P$`p-value` <- ifelse(output3P$`p-value`<0.001, "<0.001",
                            round(output3P$`p-value`, 3))

# Tidy output table of EMMeans pairwise contrasts
output3P %>%
  gt()%>%
  tab_header(title = "Pairwise Comparisons of Estimated Marginal Means 
             for Mu2")
#################
#################
###############

###############################
###############################


## Plot Figure : Estimated Marginal Means for Mu02
mu02_emm %>% as_tibble() %>% 
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
       y = expression(bolditalic(mu)[italic("2")]^italic("0")),
       color = "OCD") +
  scale_color_manual(
    values = c("low" = "royalblue2", "high" = "chocolate1"),
    name = "OCD") +
  theme_minimal(base_size = 16) +
  theme(axis.title.y = element_text(size = 24)) +
  ylim(-0.35, -0.01)
 
ggsave("A-mu02.jpeg", dpi = 300)

########


# MODELING Mu03 -----------------------------------------------------------

## Gather data into long format
data6 <- data2 %>% 
  pivot_longer(cols = c(mu03_1, mu03_2),
               names_to = "block",
               values_to = "mu03") %>% 
  dplyr::select(mu03, block, paranoia_group,
         ocd_group, rgpts_score, docs_adjusted_score) %>% 
  mutate(block = factor(block),
         n = 80)

range(data6$mu03)
# Response variable has negative and positive values

## Test assumptions for normality and homoscedasticity ##
# Visualize data distribution
hist(data6$mu03)

ggscatter(data6, x = "rgpts_score", y = "mu03",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Paranoia", ylab = "Mu03") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Shapiro-Wilk test to assess normality distributions
shapiro.test(data6$mu03)    
# Response variable is not normally distributed

# Levene's test for homogeneity of variance across groups
lt_mu03 <- leveneTest(mu03 ~ paranoia_group * ocd_group, 
                      data = data6, center = median)

tidy(lt_mu03, conf.int = TRUE) %>%
  gt()
# p < 0.0001, equal variances cannot be assumed


### GLM  approach for modeling Mu03 as a function of paranoia and OCD
mod1_glm_mu03 <- glm(mu03 ~ paranoia_group * ocd_group,
                     data = data6, family = gaussian)

## Assess model fit
summary(mod1_glm_mu03)

# Plot model diagnostics
plot(mod1_glm_mu03)

# Plot residuals
hist(residuals(mod1_glm_mu03))

# Create and display diagnostic plots
source("createDiagnosticPlots.R")
model <- mod1_glm_mu03
create_diagnostic_plots(model)

# Check multicollinearity with variance inflation factor (VIF)
vif(mod1_glm_mu03)

# Tidy table of model coefficients
mu03_coefficients <- tidy(mod1_glm_mu03)

mu03_tidy_coefficients <- mu03_coefficients %>%
  mutate_if(is.numeric, round, digits = 3)

mu03_tidy_coefficients  %>%
  gt() %>%
  tab_header(title = "GLM Summary for Mu03 ~ Paranoia group * OCD group") %>%
  cols_label(
    term = "Predictor",
    estimate = "β",
    std.error = "SE",
    statistic = "t",
    p.value = "p")  

## Post-hoc comparisons using estimated marginal means
mu03_emm <- emmeans(mod1_glm_mu03, ~ paranoia_group * ocd_group)
mu03_emm

mu03_emm <- as.data.frame(mu03_emm)

# Convert estimated marginal means to dataframe
output4 <- data.frame(mu03_emm$paranoia_group, mu03_emm$ocd_group,
                      round(mu03_emm$emmean,2),round(mu03_emm$SE,2), 
                      mu03_emm$df, round(mu03_emm$lower.CL,2), 
                      round(mu03_emm$upper.CL,2))

names(output4) <- c("Paranoia", "OCD", "EMMeans", "SE",
                    "df","Lower CI", "Upper CI")

# Tidy output table of EMMeans coefficients
output4 %>%
  gt()%>%
  tab_header(title = "Estimated Marginal Means for Mu3")

# Regrid EMMeans
mu03_emm <- emmeans(mod1_glm_mu03, ~ paranoia_group * ocd_group)

# Pairwise comparisons of EMMeans, with Tukey adjustment for multiple comparisons 
mu03_pairs <- pairs(mu03_emm, adjust = "tukey")
mu03_pairs

mu03_pairs <- as.data.frame(mu03_pairs)

# Convert EMMeans contrasts to dataframe
output4P <- data.frame(mu03_pairs$contrast, round(mu03_pairs$estimate,2), 
                      round(mu03_pairs$SE,2), round(mu03_pairs$df), 
                      round(mu03_pairs$t.ratio,2), round(mu03_pairs$p.value,3))

names(output4P) <- c("Contrast", "Estimate", "SE", "df", "t-ratio",
                    "p-value")

output4P$`p-value` <- ifelse(output4P$`p-value`<0.001, "<0.001",
                            round(output4P$`p-value`, 3))

# Tidy output table of EMMeans pairwise contrasts
output4P %>%
  gt()%>%
  tab_header(title = "Pairwise Comparisons of Estimated Marginal Means 
             for Mu03")

# Regrid EMMeans
mu03_emm <- emmeans(mod1_glm_mu03, ~ paranoia_group * ocd_group)

## Plot Figure : Estimated Marginal Means for Mu03
mu03_emm %>% as_tibble() %>% 
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
       y = expression(bolditalic(mu)[italic("3")]^italic("0")),
       color = "OCD") +
  scale_color_manual(
    values = c("low" = "royalblue2", "high" = "chocolate1"),
    name = "OCD") +
  theme_minimal(base_size = 16) +
  theme(axis.title.y = element_text(size = 24)) + 
  ylim(-1.25, 0.5)

ggsave("A-mu03.jpeg", dpi = 300)


######### 


# MODELING KAPPA ----------------------------------------------------------

## Gather data into long format
data7 <- data3 %>% 
  pivot_longer(cols = c(kappa2_1, kappa2_2),
               names_to = "block",
               values_to = "kappa") %>% 
  dplyr::select(kappa, block, paranoia_group,
         ocd_group, rgpts_score, docs_adjusted_score) %>% 
  mutate(block = factor(block),
         n = 80)

range(data7$kappa)
median(data7$kappa)

mad_value <- mad(data7$kappa)
print(mad_value)
# Response variable has negative and positive values

## Test assumptions for normality and homoscedasticity ##
# Visualize data distribution
hist(data7$kappa)

ggscatter(data7, x = "rgpts_score", y = "kappa",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Paranoia", ylab = "Kappa") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))



# Shapiro-Wilk test to assess normality distributions
shapiro.test(data7$kappa)    
# Response variable is not normally distributed

# Levene's test for homogeneity of variance across groups
lt_kappa <- leveneTest(kappa ~ paranoia_group * ocd_group, data = data7, center = mean)

tidy(lt_kappa, conf.int = TRUE) %>%
  gt()
# p < 0.0001, equal variances cannot be assumed


### GLM  approach for modeling Mu02 as a function of paranoia and OCD
# mod1_glm_kappa <- glm(kappa ~ paranoia_group * ocd_group,
#                       data = data7, family = gaussian())

mod2_glm_kappa <- glm(kappa ~ paranoia_group * ocd_group,
                      data = data7, family = quasibinomial())

## Assess model fit
# summary(mod1_glm_kappa)
summary(mod2_glm_kappa)

# Plot model diagnostics
# plot(mod1_glm_kappa)
plot(mod2_glm_kappa)

# Plot residuals
# hist(residuals(mod1_glm_kappa))
hist(residuals(mod2_glm_kappa))

# Check multicollinearity with variance inflation factor (VIF)
# vif(mod1_glm_kappa)
vif(mod2_glm_kappa)

# quasibinomial error distribution better model fit

# Create and display diagnostic plots
source("createDiagnosticPlots.R")
model <- mod2_glm_kappa
create_diagnostic_plots(model)

# Tidy table of model coefficients
kappa_coefficients <- tidy(mod2_glm_kappa) 

kappa_tidy_coefficients <- kappa_coefficients %>%
  mutate_if(is.numeric, round, digits = 3)

kappa_tidy_coefficients  %>%
  gt() %>%
  tab_header(title = "GLM Summary: Kappa ~ Paranoia group * OCD group") %>%
  cols_label(
    term = "Predictor",
    estimate = "β",
    std.error = "SE",
    statistic = "t",
    p.value = "p")  

## Post-hoc comparisons using estimated marginal means
kappa_emm <- emmeans(mod2_glm_kappa, ~ paranoia_group * ocd_group)
kappa_emm

kappa_emm <- as.data.frame(kappa_emm)

# Convert estimated marginal means to dataframe
output5 <- data.frame(kappa_emm$paranoia_group, kappa_emm$ocd_group,
                      round(kappa_emm$emmean,2),round(kappa_emm$SE,2),
                      kappa_emm$df, round(kappa_emm$asymp.LCL,2), 
                      round(kappa_emm$asymp.UCL,2))

names(output5) <- c("Paranoia", "OCD", "EMMeans", "SE",
                    "df", "Lower CI", "Upper CI")

# Tidy output table of EMMeans coefficients
output5 %>%
  gt()%>%
  tab_header(title = "Estimated Marginal Means for Kappa")
# Note: results are on the log (not response) scale

# Regrid EMMeans
kappa_emm <- emmeans(mod2_glm_kappa, ~ paranoia_group * ocd_group) 

# Pairwise comparisons of EMMeans, with Tukey adjustment for multiple comparisons
kappa_pairs <- pairs(kappa_emm, adjust = "tukey")
kappa_pairs 

kappa_pairs <- as.data.frame(kappa_pairs)

# Convert EMMeans contrasts to dataframe
output5P <- data.frame(kappa_pairs$contrast, round(kappa_pairs$estimate,2), 
                       round(kappa_pairs$SE,2), round(kappa_pairs$z.ratio,2),
                       round(kappa_pairs$p.value,3))

names(output5P) <- c("Contrasts", "Estimate", "SE", 
                     "z-ratio","p-value")

output5P$`p-value` <- ifelse(output5P$`p-value`<0.001, "<0.001",
                             round(output5P$`p-value`, 3))

# Tidy output table of EMMeans pairwise contrasts
output5P %>%
  gt()%>%
  tab_header(title = "Pairwise Comparisons of Estimated Marginal Means 
             for Kappa")


## Plot Figure : Estimated Marginal Means for Kappa
kappa_emm %>% as_tibble() %>% 
  ggplot(aes(x = paranoia_group, y = exp(emmean),
             color = ocd_group)) +
  geom_point(size = 4,
             position = position_dodge(width = 0.75),
             shape = 5, stroke = 1.5,
             show.legend = F) +
  geom_errorbar(aes(ymin = exp(asymp.LCL),
                    ymax = exp(asymp.UCL)),
                position = position_dodge(width = 0.75),
                width = 0, size = 1,
                show.legend = T) +
  labs(x = "Paranoia",
       y = expression(bolditalic(kappa)),
       color = "OCD") +
  scale_color_manual(
    values = c("low" = "royalblue2", "high" = "chocolate1"),
    name = "OCD") +
  theme_minimal(base_size = 16) +
  theme(axis.title.y = element_text(size = 24)) +
  ylim(0.875, 1.11)

ggsave("4-kappa.jpeg", dpi = 300)


########

# MODLEING Omega2 ---------------------------------------------------------

## Gather data into long format
data8 <- data2 %>% 
  pivot_longer(cols = c(omega2_1, omega2_2),
               names_to = "block",
               values_to = "omega2") %>% 
  dplyr::select(omega2, block, paranoia_group,
         ocd_group, rgpts_score, docs_adjusted_score) %>% 
  mutate(block = factor(block),
         n = 80)

range(data8$omega2)
# Response variable has negative and positive values

## Test assumptions for normality and homoscedasticity
# Visualize data distribution
hist(data8$omega2)

ggscatter(data8, x = "rgpts_score", y = "omega2",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Paranoia", ylab = "Omega2") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

ggscatter(data8, x = "docs_adjusted_score", y = "omega2",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "OCD", ylab = "Omega2") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Shapiro-Wilk test to assess normality distributions
shapiro.test(data8$omega2)    
# Response variable is not normally distributed

# Levene's test for homogeneity of variance across groups
lt_omega2 <- leveneTest(omega2 ~ paranoia_group * ocd_group, 
                        data = data8, center = median)

tidy(lt_omega2, conf.int = TRUE) %>%
  gt()
# p < 0.0001, equal variances cannot be assumed


### GLM  approach for modeling omega2 as a function of paranoia and OCD
mod1_glm_omega2 <- glm(omega2 ~ paranoia_group * ocd_group,
                     data = data8, family = gaussian())

## Assess model fit
summary(mod1_glm_omega2)

# Plot model diagnostics
plot(mod1_glm_omega2)

# Plot residuals
hist(residuals(mod1_glm_omega2))

# Create and display diagnostic plots
source("createDiagnosticPlots.R")
model <- mod1_glm_omega2
create_diagnostic_plots(model)

# Check multicollinearity with variance inflation factor (VIF)
vif(mod1_glm_omega2)

# Tidy table of model coefficients
omega2_coefficients <- tidy(mod1_glm_omega2)

omega2_tidy_coefficients <- omega2_coefficients %>%
  mutate_if(is.numeric, round, digits = 3)

omega2_tidy_coefficients  %>%
  gt() %>%
  tab_header(title = "GLM Summary: Omega2 ~ Paranoia group * OCD group") %>%
  cols_label(
    term = "Predictor",
    estimate = "β",
    std.error = "SE",
    statistic = "t",
    p.value = "p")  

## Post-hoc comparisons using estimated marginal means
omega2_emm <- emmeans(mod1_glm_omega2, ~ paranoia_group * ocd_group)
omega2_emm

omega2_emm <- as.data.frame(omega2_emm)

# Convert estimated marginal means to dataframe
output6 <- data.frame(omega2_emm$paranoia_group, omega2_emm$ocd_group,
                      round(omega2_emm$emmean,2), round(omega2_emm$SE,2),
                      omega2_emm$df, round(omega2_emm$lower.CL,2), 
                      round(omega2_emm$upper.CL,2))

names(output6) <- c("Paranoia", "OCD", "EMMeans", "SE",
                     "df", "Lower CI", "Upper CI")

# Tidy output table of EMMeans coefficients
output6 %>%
  gt()%>%
  tab_header(title = "Estimated Marginal Means for Omega2")

# Regrid EMMeans
omega2_emm <- emmeans(mod1_glm_omega2, ~ paranoia_group * ocd_group) 

## Pairwise comparisons of EMMeans, adjusting for multiple comparisons (tukey)
omega2_pairs <- pairs(omega2_emm, adjust = "tukey")
omega2_pairs

omega2_pairs <- as.data.frame(omega2_pairs)

# Convert EMMeans contrasts to dataframe
output6P <- data.frame(omega2_pairs$contrast, round(omega2_pairs$estimate,2), 
                      round(omega2_pairs$SE,2), omega2_pairs$df, 
                      round(omega2_pairs$t.ratio,2), round(omega2_pairs$p.value,3))

names(output6P) <- c("Contrast", "Estimate", "SE", "df", "t-ratio",
                    "p-value")

output6P$`p-value` <- ifelse(output6P$`p-value`<0.001, "<0.001",
                            round(output6P$`p-value`, 3))

# Tidy output table of EMMeans pairwise contrasts
output6P %>%
  gt()%>%
  tab_header(title = "Pairwise Comparisons of Estimated Marginal Means 
             for Omega2")

## Plot Figure : Estimated marginal means for Omega2
omega2_emm %>% as_tibble() %>% 
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
       y = expression(bolditalic(omega)[italic("2")]^italic("0")),
       color = "OCD") +
  scale_color_manual(
    values = c("low" = "royalblue2", "high" = "chocolate1"),
    name = "OCD") +
  theme_minimal(base_size = 16) +
  theme(axis.title.y = element_text(size = 24)) +
  ylim(0.35, 1.35)

ggsave("4-omega2.jpeg", dpi = 300)


########

# MODELING OMEGA3 ---------------------------------------------------------

## Gather data into long format
data9 <- data2 %>% 
  pivot_longer(cols = c(omega3_1, omega3_2),
               names_to = "block",
               values_to = "omega3") %>% 
  dplyr::select(omega3, block, paranoia_group,
         ocd_group, rgpts_score, docs_adjusted_score) %>% 
  mutate(block = factor(block),
         n = 80)

range(data9$omega3)
# Response variable has negative and positive values

## Test assumptions for normality and homoscedasticity
# Visualize data distribution
hist(data9$omega3)

ggscatter(data9, x = "rgpts_score", y = "omega3",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Paranoia", ylab = "Omega3") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

ggscatter(data9, x = "docs_adjusted_score", y = "omega3",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "OCD", ylab = "Omega3") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Shapiro-Wilk test to assess normality distributions
shapiro.test(data9$omega3)    
# Response variable is not normally distributed

# Levene's test for homogeneity of variance across groups
lt_omega3 <- leveneTest(omega3 ~ paranoia_group * ocd_group, 
                        data = data9, center = median)

tidy(lt_omega3, conf.int = TRUE) %>%
  gt()
# p > 0.05, equal variances can be assumed


### GLM  approach for modeling Omega3 as a function of paranoia and OCD
mod1_glm_omega3 <- glm(omega3 ~ paranoia_group * ocd_group,
                       data = data9, family = gaussian())

## Assess model fit
summary(mod1_glm_omega3)

# Plot model diagnostics
plot(mod1_glm_omega3)

# Plot residuals
hist(residuals(mod1_glm_omega3))

# Create and display diagnostic plots
source("createDiagnosticPlots.R")
model <- mod1_glm_omega3
create_diagnostic_plots(model)

# Check multicollinearity with variance inflation factor (VIF)
vif(mod1_glm_omega3)

# Tidy table of model coefficients
omega3_coefficients <- tidy(mod1_glm_omega3)

omega3_tidy_coefficients <- omega3_coefficients %>%
  mutate_if(is.numeric, round, digits = 3)

omega3_tidy_coefficients  %>%
  gt() %>%
  tab_header(title = "GLM Summary: Omega3 ~ Paranoia group * OCD group") %>%
  cols_label(
    term = "Predictor",
    estimate = "β",
    std.error = "SE",
    statistic = "t",
    p.value = "p")  

## Post-hoc comparisons using estimated marginal means
omega3_emm <- emmeans(mod1_glm_omega3, ~ paranoia_group * ocd_group)
omega3_emm

omega3_emm <- as.data.frame(omega3_emm)

# Convert estimated marginal means to dataframe
output7 <- data.frame(omega3_emm$paranoia_group, omega3_emm$ocd_group,
                      round(omega3_emm$emmean,2), round(omega3_emm$SE,2), 
                      omega3_emm$df,
                      round(omega3_emm$lower.CL,2), round(omega3_emm$upper.CL,2))

names(output7) <- c("Paranoia", "OCD", "EMMeans", "SE",
                   "df", "Lower CI", "Upper CI")

# Tidy output table of EMMeans coefficients
output7%>%
  gt()%>%
  tab_header(title = "Estimated Marginal Means for Omega3")

# Regrid EMMeans
omega3_emm <- emmeans(mod1_glm_omega3, ~ paranoia_group * ocd_group) 

## Pairwise comparisons of EMMeans, adjusting for multiple comparisons (tukey)
omega3_pairs <- pairs(omega3_emm, adjust = "tukey")
omega3_pairs

omega3_pairs <- as.data.frame(omega3_pairs)

# Convert EMMeans contrasts to dataframe
output7P <- data.frame(omega3_pairs$contrast, round(omega3_pairs$estimate,2), 
                      round(omega3_pairs$SE,2), round(omega3_pairs$t.ratio,2), 
                      round(omega3_pairs$p.value,3))

names(output7P) <- c("Contrasts", "Estimate", "SE",
                     "t-ratio", "p-value")

output7P$`p-value` <- ifelse(output7P$`p-value`<0.001, "<0.001",
                            round(output7P$`p-value`, 3))

# Tidy output table of EMMeans pairwise contrasts
output7P %>%
  gt()%>%
  tab_header(title = "Pairwise Comparisons of Estimated Marginal Means 
             for Omega3")

## Plot Figure : estimated marginal means for Omega3
omega3_emm %>% as_tibble() %>% 
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
       y = expression(bolditalic(omega)[italic("3")]^italic("0")),
       color = "OCD") +
  scale_color_manual(
    values = c("low" = "royalblue2", "high" = "chocolate1"),
    name = "OCD") +
  theme_minimal(base_size = 16) +
  theme(axis.title.y = element_text(size = 24)) 

ggsave("4-omega3.jpeg", dpi = 300)
