### Excessive switching in OCD and paranoia arises from different deficits in belief-updating
### Freeland et al. 2026

## Author: C.M. Freeland, PhD
## Data source: Suthaharan et. al, 2021

#### wrangleData FUNCTION #####

# Purpose: the wrangleData function filters & cleans raw csv data file 
# to generate a clean dataframe that is sourced at the beginning of each R script 
# for plotting, analyzing & modeling the data

wrangleData <- function(file_path = "pandemicPRL.csv") {
 
  
  # Step 1: Import data
  data <- read.csv(file_path)
  
  # Step 2: Filter for pandemic dataset & period
  data1 <- data %>% 
    filter(dataset == "pandemic", period %in% c("lockdown", "postlockdown"))
  
  # Step 3: Create new variables with score sums and means and assign groups
  data1 <- data1 %>%
    rowwise() %>%
    mutate(
      bai_score = sum(c_across(starts_with("bai_")), na.rm = TRUE),
      bdi_score = sum(c_across(starts_with("bdi_")), na.rm = TRUE),
      rgpts_score = sum(c_across(starts_with("rgpts_")), na.rm = TRUE),
      rgpts_ref_score = sum(c_across(starts_with("rgpts_ref_")), na.rm = TRUE),
      rgpts_per_score = sum(c_across(starts_with("rgpts_per_")), na.rm = TRUE),
      docs_total_score = sum(c_across(starts_with("docs_")), na.rm = TRUE),
      docs_contamination = sum(c_across("docs_1":"docs_5"), na.rm = TRUE),
      docs_adjusted_score = sum(c_across("docs_6":"docs_20"), na.rm = TRUE),
      reversals_total = sum(c_across(starts_with("reversals_")), na.rm = TRUE),
      wsr_mean = mean(c(wsr_block1, wsr_block2), na.rm = TRUE),
      lsr_mean = mean(c(lsr_block1, lsr_block2), na.rm = TRUE),
      mu02_mean = mean(c(mu02_1, mu02_2), na.rm = TRUE),
      mu03_mean = mean(c(mu03_1, mu03_2), na.rm = TRUE),
      kappa2_mean = mean(c(kappa2_1, kappa2_2), na.rm = TRUE),
      omega2_mean = mean(c(omega2_1, omega2_2), na.rm = TRUE),
      omega3_mean = mean(c(omega3_1, omega3_2), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      paranoia_group = if_else(rgpts_score > 10, "high", "low"),
      paranoia_severity = case_when(
        rgpts_score > 27 ~ "very_severe",
        rgpts_score > 17 ~ "severe",
        rgpts_score > 10 ~ "mod_severe",
        rgpts_score > 5  ~ "elevated",
        TRUE             ~ "low"
      ),
      ocd_group = if_else(docs_adjusted_score > 13, "high", "low")
    ) %>%
    ungroup() %>%
    mutate(
      glm_paranoia_group = case_when(paranoia_group == "low" ~ 0,
                                     paranoia_group == "high" ~ 1),
      glm_ocd_group = case_when(ocd_group == "low" ~ 0,
                                ocd_group == "high" ~ 1)
    )
      
  # Convert to factors with levels
  data1 <- data1 %>%
    mutate(
      paranoia_group = factor(paranoia_group, levels = c("low", "high")),
      paranoia_severity = factor(paranoia_severity,
                                 levels = c("low", "elevated", "mod_severe", "severe", "very_severe"),
                                 labels = c("Low", "Elevated", "Mod Severe", "Severe", "Very Severe")),
      ocd_group = factor(ocd_group, levels = c("low", "high"))
    )
  
  # Step 4: Outlier removal — Remove observations with average decision time > 4 seconds
  data2 <- data1 %>% 
    filter(avg_rt_total < 3000)          # outliers n = 4
  
  # Step 5: Outlier removal — kappa2_mean using IQR
  Q1 <- quantile(data2$kappa2_mean, 0.25, na.rm = TRUE)
  Q3 <- quantile(data2$kappa2_mean, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  upper_bound <- Q3 + 1.5 * IQR_val
  
  data3_cleaner <- data2 %>%
    filter(kappa2_mean <= upper_bound)  # outliers n = 2
  
  # Step 6: Return both dataframes
  return(list(
    data1 = data1,
    data2 = data2,          # only decision-time outliers removed (N = 399)
    data3 = data3_cleaner   # decision-time and Kappa outliers removed (N = 397)
  ))
}
