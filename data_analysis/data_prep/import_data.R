# packages
library("tidyverse")

#' anonymizing datasets
#' set the seed to 12345 to use the algorithm 
seed <- 12345
suppressPackageStartupMessages({
  library("data.table")
  library("digest")
})

# anonymize function (source: jangorecki)
# https://jangorecki.github.io/blog/2014-11-07/Data-Anonymization-in-R.html
set.seed(seed)

anonymize <- function(x, algo="crc32"){
  unq_hashes <- vapply(unique(x), function(object) digest(object, algo=algo), FUN.VALUE="", USE.NAMES=TRUE)
  
  unq_hashes[x]
}


df <- df %>%
  mutate_at("MID", anonymize)

rm(anonymize)

# write_csv(df, here::here("raw_data.csv"))


#' 1. importing data 
s2 = readr::read_csv(here::here("data", "s2_anonymized_processed_data.csv"))
s2_old = readr::read_csv(here::here("data","double_checking_purposes", "study2a_processed_data.csv"))

anti_join(s2, s2_old) %>% View()
# 
s2 = s2 %>% mutate_at("MID", anonymize)
write_csv(s2, here::here("data", "s2_anonymized_processed_data.csv"))

# df1 <- readr::read_rds("../ods/dat/analysis_df.RDS")
df2 <- readr::read_rds("../odd/data/analysis_df.RDS")
df3 <- readr::read_rds("../regulatory_fit_study/data/analysis_df.RDS")
df4 <- readr::read_rds("../rf_followup/data/analysis_df.RDS")
df5 <- readr::read_rds("../study5/data/analysis_df.RDS")

# participant eligibility coding
df2 <- df2 %>%
  mutate(exclusion = case_when(
    attn_check == "pass" & 
      var_low =="acceptable" &
      Finished == 1 ~ "eligible",
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

excluded_n <- list(df2, df3, df4, df5) %>% 
  map_dfr(count, exclusion) %>% 
  rownames_to_column(var = "study") %>% 
  mutate(study = rep(paste("study", sep = "_", c(1, "2A", "2B", 3)), each = 2)) %>% 
  spread(exclusion, n)

# data prep
## mean-centre predictors  
source(here::here("function", "centre_pred.R"))

### df1 
# df1_pred <- colnames(df1)[c(556:566, 570:577, 583:584)] # defining names of predictors

# df1 <- df1 %>% 
#   filter(exclusion == "eligible") %>% 
#   centre_pred(df1_pred)

### df2
df2 <- df2 %>% 
  filter(exclusion == "eligible") %>% 
  centre_pred(var = c("promotion", "prevention"))

### df3
df3 <- df3 %>% 
  filter(exclusion == "eligible") %>% 
  centre_pred(var = c("promotion", "prevention"))

df3$condition = ifelse(df3$condition == 0, -1, df3$condition) # effect-coding condition  

### df4
df4 <- df4 %>% 
  filter(exclusion == "eligible") %>% 
  centre_pred(var = c("promotion", "prevention"))

### df5
df5 <- df5 %>% 
  filter(exclusion == "eligible") %>% 
  centre_pred(var = c("promotion", "prevention", "pos_af", "neg_af"))

df5 <- df5 %>% 
  rename(ods = od_success) # making online dating success variable name same as other dataframes 

# cleaning up
# rm(list = c("df1_pred"))
