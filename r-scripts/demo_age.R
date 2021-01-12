age <- function(df){
  df %>% 
    group_by(exclusion) %>% 
    # filter(exclusion == "eligible") %>% 
    summarize(n = n(),
              age.m = mean(age, na.rm = T),
              age.sd = sd(age, na.rm = T),
              age.min = min(age, na.rm = T),
              age.max = max(age, na.rm = T))
}
