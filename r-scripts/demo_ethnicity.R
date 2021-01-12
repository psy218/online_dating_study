ethnicity <- function(df) {
  df %>% 
    pivot_longer(cols = ethnicity_1: ethnicity_9, 
                 names_to = "ethnicity", 
                 values_to = "count",
                 values_drop_na = TRUE) %>% 
    count(ethnicity, sort = T) %>% 
    mutate(cum_perc = paste( round((n / nrow(df))*100, 2), "%"))
}
