# data
df1 <- readr::read_rds("/Users/suesong/R/ods/dat/analysis_df.RDS")
df2 <- readr::read_rds("/Users/suesong/R/odd/data/analysis_df.RDS")
df3 <- readr::read_rds("/Users/suesong/R/regulatory_fit_study/data/analysis_df.RDS")
df4 <- readr::read_rds("/Users/suesong/R/rf_followup/data/analysis_df.RDS")
df5 <- readr::read_rds("/Users/suesong/R/study5/data/analysis_df.RDS")

# participant eligibility coding
df2 <- df2 %>%
  mutate(exclusion = case_when(
    attn_check == "pass" & 
      var_low =="acceptable" ~ "eligible",
    TRUE ~ "ineligible"))

df3 <- df3 %>%
  mutate(exclusion = case_when(
    attn_check == "pass" &
      var_low == "acceptable" &
      duplicate == "nonduplicated" ~ "eligible",
    TRUE ~ "ineligible"))

df4 <- df4 %>%
  mutate(exclusion = case_when(
    attn_check == "pass" ~ "eligible",
    TRUE ~ "ineligible"))

df5 <- df5 %>%
  mutate(exclusion = case_when( # Participant eligibility
    attn_check == "pass" & 
      duplicate == "nonduplicated" ~ "eligible",
    TRUE ~ "ineligible"))

# data prep
## mean-centre predictors  
source(here::here("function", "centre_pred.R"))

### df1 
df1_pred <- colnames(df1)[c(556:566, 570:577, 583:584)] # defining names of predictors 
df1 <- centre_pred(df1, df1_pred)                       # apply the function

### df2

