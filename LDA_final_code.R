
########## Data Source: 
# https://dasil.sites.grinnell.edu/downloadable-data/
# https://www.nlsinfo.org/content/cohorts/nlsy97


library(tidyverse)
library(readr)
library(ggplot2)
library(patchwork)
library(cluster)
library(lme4)
library(nnet)
library(forcats)
library(lmtest)
library(eoffice)
library(officer)
library(fpc)
library(mclust)
library(gtsummary)
library(kableExtra)



##########  Preprocessing
# Main goal is to evaluate how predictors affect the wage across the study period
# and to identify latent clustering through k-means clustering. (also LCMM if possible)
# outcome:  wage 
# predictors: sex, age, race, pov ratio, residential type (urban/rural), region, marital status

# 1) The predictors are determined based on standard wage-determinants practice: 
# https://www.nber.org/system/files/working_papers/w21913/w21913.pdf
# Variables that have shown influence on wages from past literature are selected, 
# while maintaining model parsimony and interpretability. 
# demographic covariates (sex, age, race/ethnicity) 
# socioeconomic backround (poverty ratio)
# geographic/contextual variables (residential type: urban/rural, region)
# marital status 

# 2) The data is limited to subjects with 5 or more measurements: 
# https://www.sciencedirect.com/science/article/pii/S002239560000042X?
  

data = read.csv("./data/NLYS97.csv") |>
  janitor::clean_names() 

data_clean = data |>
  select(pubid, year, key_sex, age, key_race, key_race_ethnicity, cv_hh_pov_ratio, 
         cv_urban_rural, cv_census_region, wage, cv_marstat, degree) |>
  filter(wage > 0,  # remove missing predictors 
         !cv_marstat %in% c(-3, -4, -5), 
         cv_hh_pov_ratio > 0, 
         cv_urban_rural %in% c("Rural", "Urban"), 
         !cv_census_region %in% c(-3, -4, -5)
  ) |>
  mutate(
    race_ethnicity = case_when(
      # Hispanic overrides race 
      key_race_ethnicity == "Hispanic" ~ "Hispanic",         
      # Non-Hispanic classified by race  
      key_race_ethnicity != "Hispanic" & key_race == "White" ~ "NH White",
      key_race_ethnicity != "Hispanic" & key_race == "Black or African American" ~ "NH Black",
      key_race_ethnicity != "Hispanic" & key_race == "Asian or Pacific Islander" ~ "NH Asian/Pacific Islander",
      key_race_ethnicity != "Hispanic" & key_race == "American Indian, Eskimo, or Aleut" ~ "NH American Indian/Alaska Native",
      # Mixed race or unknown as Others 
      key_race %in% c("-1", "-2", "Mixed Race (Non-Hispanic)", "Something else? (SPECIFY)") ~ "Other",
      TRUE ~ "Other"), 
    cv_census_region = case_when(
      cv_census_region == "North Central (IL, IN, IA, KS, MI, MN, MO, NE, OH, ND, SD, WI)" ~ "North Central", 
      cv_census_region == "Northeast (CT, ME, MA, NH, NJ, NY, PA, RI, VT)" ~ "Northeast", 
      cv_census_region == "South (AL, AR, DE, DC, FL, GA, KY, LA, MD, MS, NC, OK, SC, TN , TX, VA, WV)" ~ "South", 
      cv_census_region == "West (AK, AZ, CA, CO, HI, ID, MT, NV, NM, OR, UT, WA, WY)" ~ "West"),
    degree = case_when(
      degree %in% c("Added in - GED", 
                    "Vocational or technical certificate", 
                    "UNCODABLE", 
                    "OTHER (SPECIFY)") ~ "HS_or_less",
      degree %in% c("Associate/Junior College or two-year associate degree (AA)") ~ "Some_college",
      degree %in% c("Bachelor's degree (BA, BS or unspecified)", 
                    "Master's degree (MA, MBA, MS, MSW)", 
                    "Doctoral Degree (PhD)", 
                    "Professional Degree (MD, LLD, DDS, JD)") ~ "Bachelor’s_higher",
      TRUE ~ NA_character_),
    marital = case_when(
      cv_marstat %in% c("Married, spouse absent", "Married, spouse present") ~ "Married",
      cv_marstat %in% c("Never married, cohabiting", "Never married, not cohabiting") ~ "Never married",
      TRUE ~ "Previously married"), 
    cv_hh_pov_ratio = cv_hh_pov_ratio / 100 
  ) |>
  rename(id = pubid, 
         sex = key_sex, 
         pov = cv_hh_pov_ratio, 
         urban_rural = cv_urban_rural,
         region = cv_census_region) |>
  mutate(
    degree = factor(degree,
                       levels = c("HS_or_less", "Some_college", "Bachelor’s_higher"),
                       ordered = TRUE)) |> 
  select(-c(key_race, key_race_ethnicity, cv_marstat))


# degree information: recode to the highest attainment 
education_id = data_clean |>
  group_by(id) |>
  summarize(education = max(degree, na.rm = TRUE))

data_clean = data_clean |>
  left_join(education_id, by = "id") |>
  select(-degree) |>
  drop_na()


# limit to subjects with at least 5 measurements 
id_elig = data_clean |> 
  group_by(id) |> 
  summarize(count = n()) |>
  filter(count >= 5)   # 1359 eligible subjects  

data_elig = data_clean |> 
  filter(id %in% id_elig$id) |>
  select(id, year, sex, age, pov, urban_rural, region, wage, race_ethnicity, marital, education) |>
  mutate(
    age = as.numeric(age), 
    sex = factor(sex, labels = c("male", "female")), 
    urban_rural = factor(urban_rural),
    region = factor(region), 
    race_ethnicity = factor(race_ethnicity), 
    marital = factor(marital), 
    race_ethnicity = fct_relevel(race_ethnicity, "NH White")) 

write.csv(data_elig, "./data/data_elig_final.csv", row.names = FALSE)


########## EDA and visualization 

# histogram of the data 
p_hist = ggplot(data_elig, aes(x = wage)) +
  geom_histogram(bins = 40) +
  theme_minimal() 

# log transform 
data_elig = data_elig |>
  mutate(log_wage = log(wage))
p_loghist = ggplot(data_elig, aes(x = log(wage))) +
  geom_histogram(bins = 40) +
  theme_minimal()

# Wage vs age by sex
p3 = ggplot(data_elig, aes(x = age, y = log(wage), color = sex)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE) +
  theme_minimal() 

# trajectories of the data 
set.seed(12)
sample_id = sample(id_elig$id, 10) # 10 random samples 
sample_id_data = data_elig |>
  group_by(id) |> 
  filter(id %in% sample_id) 

p_001 = ggplot(sample_id_data, 
               aes(x = age, y = wage, group = id)) + 
  geom_line() + 
  geom_point(alpha = 0.8) + 
  labs(x = "age", y = "wage") + 
  theme_minimal()
p_002 = ggplot(sample_id_data, 
               aes(x = age, y = log_wage, group = id)) + 
  geom_line() + 
  geom_point(alpha = 0.8) + 
  labs(x = "age", y = "log(wage)") + 
  theme_minimal()

# average wage trajectories per sex  
data_elig_sex = data_elig |> 
  group_by(sex, age) |> 
  summarize(mean_log_wage = mean(log_wage, na.rm = TRUE), .groups = "drop")
p_mean_traj = ggplot(data_elig_sex, aes(x = age, y = mean_log_wage, group = sex, color = sex)) + 
  geom_line(alpha = 0.3) + 
  geom_point() + 
  labs(x = "age", y = "log(wage)") + 
  theme_minimal()

# EDA summary table for baseline 
table_data = data_elig |> 
  group_by(id) |> 
  slice_min(year, n = 1) |>  # first observation per subject
  ungroup()

table1_subj_summary = table_data |>
  select(wage, age, pov, sex, race_ethnicity, education, urban_rural, region, marital) |> 
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"),
    digits = list(
      all_continuous() ~ c(2, 2, 2),
      all_categorical() ~ c(0, 1)))

table1_overall_summary = data_elig |>
  select(wage, age, pov, urban_rural, region, marital) |> 
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"),
    digits = list(
      all_continuous() ~ c(2, 2, 2),
      all_categorical() ~ c(0, 1)))


files = "~/Documents/2_school/3_fall2025/P8157_LDA/LDA/files2.pptx"

topptx(p_hist, files, append = TRUE)
topptx(p_loghist, files, append = TRUE)
topptx(p3, files, append = TRUE) 
topptx(p_001, files, append = TRUE) 
topptx(p_002, files, append = TRUE) 
topptx(p_mean_traj, files, append = TRUE) 



########## fitting LMM   
# center age 
data_elig = data_elig |>
  mutate(age_c = age - mean(age, na.rm = TRUE))  

contrasts(data_elig$education) = "contr.treatment"


# random int model  
fit_lmm_ri = lmer(log_wage ~ age_c + sex + race_ethnicity + pov + urban_rural + region + 
                    marital + education + (1 | id),
                  data = data_elig, 
                  REML = FALSE)         # for likelihood ratio test 

# random int/slope model 
fit_lmm_ris = lmer(log_wage ~ age_c + sex + race_ethnicity + pov + urban_rural + region + 
                     marital + education + (age_c | id),
                   data = data_elig, 
                   REML = FALSE,        # for likelihood ratio test 
                   control = lmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)))        

# compare 
anova(fit_lmm_ri, fit_lmm_ris) 
summary(fit_lmm_ris)


##########  Model Diagnostics for the chosen model (random intercept/slope)
ris_res_raw = residuals(fit_lmm_ris, type = "response")
ris_res_p = residuals(fit_lmm_ris, type = "pearson")
fitted_ris = fitted(fit_lmm_ris)

# residual vs fitted plot 
df_resfit = data.frame(fitted = fitted_ris,
                       resid  = ris_res_p)
std_resid_plot = ggplot(df_resfit, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Standardized residuals") +
  theme_minimal()
topptx(std_resid_plot, files, append = TRUE)

# QQ plot of residuals 
qqplot = ggplot(df_resfit, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "(2) QQ Plot of Standardized Residuals for model 2",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()
topptx(qqplot, files, append = TRUE)


# Check variance and covariance 
df_resfit2 = data.frame(id = data_elig$id, 
                        time = data_elig$age_c, 
                        resid = ris_res_p)
df_resfit2_wide = df_resfit2 |> 
  pivot_wider(id_cols = id, 
              names_from = time, 
              values_from = resid) |> 
  select(-id)
mat_res = as.matrix(df_resfit2_wide) 

# empirical covariance matrix of residuals 
cov_res = cov(mat_res, use = "pairwise.complete.obs")

# diagonal and off-diagonal values 
diag_val = diag(cov_res)
off_diag_val = cov_res[lower.tri(cov_res)]
df_diag = data.frame(variance = diag_val)
df_off_diag = data.frame(off_val = off_diag_val)

# plot 
resid_diag1 = ggplot(df_diag, aes(x = variance)) +
  geom_histogram(bins = 20, color = "black") +
  labs(
    title = "Diagonal elements (residual variance by time)",
    x = "Variance",
    y = "Count"
  ) +
  theme_minimal()

resid_diag2 = ggplot(df_off_diag, aes(x = off_val)) +
  geom_histogram(bins = 20, color = "black") +
  labs(
    title = "Off-diagonal elements (residual covariance by time)",
    x = "Covariance",
    y = "Count"
  ) +
  theme_minimal()

topptx(resid_diag1, files, append = TRUE)
topptx(resid_diag2, files, append = TRUE)


########## Clustering on the random effects from the LMM 
# https://pmc.ncbi.nlm.nih.gov/articles/PMC2909466/pdf/nihms-214614.pdf 
# https://www.tandfonline.com/doi/full/10.1080/03610918.2020.1861464

re = ranef(fit_lmm_ris)$id
re_mat = scale(as.matrix(re))

# number of clusters for random effects - elbow plots 
set.seed(12)

re_wss = sapply(1:10, function(k){
  kmeans(re_mat, centers = k, nstart = 20)$tot.withinss
})
re_elbow_df = data.frame(k = 1:10, wss = re_wss)
ggplot(re_elbow_df, aes(k, wss)) +
  geom_point(size = 3) +
  geom_line() +
  theme_minimal() +
  labs(title = "Elbow Plot for K-means on Random Effects",
       x = "Number of clusters k",
       y = "Total within-cluster sum of squares") # 3 clusters 

# number of clusters for random effects - silhouette analysis 
re_sil_width = sapply(2:10, function(k){
  km = kmeans(re_mat, centers = k, nstart = 20)
  si = silhouette(km$cluster, dist(re_mat))
  mean(si[, 3])   # average silhouette width
})
data.frame(k = 2:10, sil = re_sil_width) |> 
  ggplot(aes(k, sil)) +
  geom_point(size = 3) +
  geom_line() +
  theme_minimal() +
  labs(title = "Silhouette Analysis",
       x = "Number of clusters",
       y = "Average silhouette width")  # 3 clusters 

# clustering with 3 clusters 
re_km3 = kmeans(re_mat, centers = 3)   
cluster_assignments = data.frame(id = as.numeric(rownames(re)),
                                 cluster = factor(re_km3$cluster))
data_re_clustered = data_elig |>
  left_join(cluster_assignments, by = "id")

# plot clustering results 
## 10 random samples per cluster  
set.seed(12)
re_sample_id = data_re_clustered |>
  select(id, cluster) |>
  distinct() |>
  group_by(cluster) |>
  sample_n(10) |>
  ungroup()
re_sample = data_re_clustered |>
  filter(id %in% re_sample_id$id)

ggplot(re_sample, aes(x = age, y = log_wage, group = id, color = cluster)) +
  geom_line(alpha = 0.8) +
  theme_minimal() +
  labs(title = "Trajectories: 10 Random Individuals per Cluster")


## cluster trajectories by mean or gam 
cluster_means = data_re_clustered |>
  group_by(cluster, age) |>
  summarize(mean_log_wage = mean(log_wage, na.rm = TRUE), .groups = "drop")

re_mean = ggplot(cluster_means, aes(x = age, y = mean_log_wage, color = cluster)) +
  geom_line(linewidth = 1.3) +
  theme_minimal() +
  labs(title = "Mean Wage Trajectory by Cluster",
       x = "Age", y = "Mean Wage")
re_gam = ggplot(data_re_clustered, aes(age, log_wage, color = cluster)) +
  geom_smooth(se = FALSE, size = 1.5, method = "gam") +
  theme_minimal() +
  labs(title = "Smoothed Wage Trajectories by Cluster")

topptx(re_mean, files, append = TRUE)
topptx(re_gam, files, append = TRUE)



# cluster level distribution 
re_cluster_box = ggplot(data_re_clustered, aes(cluster, wage, fill = cluster)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Wage Distribution by Cluster")

topptx(re_cluster_box, files, append = TRUE)


# cluster visualization 
re_df = data.frame(id = as.numeric(rownames(ranef(fit_lmm_ris)$id)),
                   intercept = ranef(fit_lmm_ris)$id[, "(Intercept)"],
                   slope = ranef(fit_lmm_ris)$id[, "age_c"],
                   re_cluster = factor(re_km3$cluster))
re_plot = ggplot(re_df, aes(intercept, slope, color = re_cluster)) +
  geom_point(alpha = 0.6) +
  stat_ellipse(level = 0.95) +
  theme_minimal() +
  labs(title = "Random Effects by Cluster",
       x = "Random Intercept",
       y = "Random Slope")
topptx(re_plot, files, append = TRUE)


# cluster covariates summary 
table_cluster = data_re_clustered |> 
  group_by(id) |> 
  slice_min(year, n = 1) |>  # first observation per subject
  ungroup()

table1_cluster = table_cluster |>
  select(cluster, wage, age, pov, sex, race_ethnicity, education) |> 
  tbl_summary(
    by = cluster, 
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"),
    digits = list(
      all_continuous() ~ c(2, 2, 2),
      all_categorical() ~ c(0, 1)))

subject_level = data_re_clustered |>
  group_by(id, cluster) |>
  summarise(
    prop_urban = mean(urban_rural == "Urban"),
    most_region = names(which.max(table(region))),
    ever_married = any(marital != "Never married"),
    .groups = "drop")   

subject_level |> 
  select(-id) |> 
  tbl_summary(by = cluster,
              statistic = list(prop_urban ~ "{mean} ({sd})",
                               most_region ~ "{n} ({p}%)",
                               ever_married ~ "{n} ({p}%)"))



