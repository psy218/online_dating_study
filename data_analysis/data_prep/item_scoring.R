#### item scoring ####
#' some engagement and ease items have different variable names across four datasets
#' aggregate scores for engagement & ease concist of different items 
#' (e.g., in S2a: overall_engagement_1:3 vs. in s4: engage_1:3)

#---- engagement -----
#' @param ease the extent to which profile browsing is engaging
#' #' pertains to profile-browsing experience during the experimental manipulation in S2a, S2b, and S3; general ease in S4
# s2a <- s2a %>% 
#   mutate(engage = s2a %>% 
#            select(paste("overall_engagement", 1:3, sep = "_")) %>% 
#            # psych::alpha(check.keys = TRUE)
#            rowMeans(na.rm = T))

engage_scoring <- function(df) {
  df %>% 
    select(paste("engage", 1:3, sep = "_")) %>% 
    # psych::alpha(check.keys = TRUE)
    rowMeans(na.rm = T)
}

# s2b <- s2b %>% 
#   mutate(engage = engage_scoring(s2b))
# s3 <- s3 %>% 
#   mutate(engage = engage_scoring(s3))
# s4 <- s4 %>% 
#   mutate(engage = engage_scoring(s4))

#---- ease ----
#' @param ease the extent to which profile browsing feels effortless 
#' pertains to profile-browsing experience during the experimental manipulation in S2a, S2b, and S3; general ease in S4
# s2a <- s2a %>% 
#   mutate(ease = s2a %>% 
#            select(paste("overall_ease", c(1, 3, 5), sep = "_")) %>% 
#            mutate_at(vars("overall_ease_3", "overall_ease_5"), ~reverse_code(., 7)) %>% 
#            # psych::alpha(check.keys = TRUE)
#            rowMeans(na.rm = T))

ease_scoring <- function(df) {
  df %>% 
    select(paste("ease", c(1:2, 4), sep = "_")) %>% 
    mutate_at(vars("ease_2", "ease_4"), ~reverse_code(., 7)) %>% 
    # psych::alpha(check.keys = TRUE)
    rowMeans(na.rm = T)
}

# s2b <- s2b %>% 
#   mutate(ease = ease_scoring(s2b))
# s3 <- s3 %>% 
#   mutate(ease = ease_scoring(s3))
# s4 <- s4 %>% 
#   mutate(ease = ease_scoring(s4))

#---- strategy success ----
#' @param success_strategy the perceived success after an experimental condition (i.e., using a specific strategy) in S2A, S2B, and S3
# success_scoring <- function(df) {
#   df %>% 
#     select(matches("success_[1-6]")) %>% 
#     # psych::alpha(check.keys = TRUE)
#     rowMeans(na.rm = T)
# }
# 
# 
# s2a <- s2a %>% 
#   mutate(success_strategy = s2a %>% 
#            select(paste("overall_success", 1:3, sep = "_"), paste("strategy_success", 1:3, sep = "_")) %>% 
#            # psych::alpha(check.keys = TRUE)
#            rowMeans(na.rm = T))
# 
# s2b <- s2b %>% 
#   mutate(success_strategy = success_scoring(s2b))
# s3 <- s3 %>% 
#   mutate(success_strategy = success_scoring(s3))


#---- overall success in online dating -----
#' @param ods perceived success in achieving romantic outcomes through online dating in S1, S3, and S4

ods_scoring <- function(df) {
  df %>% 
    select(matches("suc_[0-9]")) %>% 
    mutate_at(vars(paste("suc", c(3, 7, 10), sep = "_")), ~reverse_code(., 7)) %>% 
    # psych::alpha(check.keys = TRUE)
    rowMeans(na.rm = T) 
}


# s1 <- s1 %>% 
#   mutate(ods = ods_scoring(s1))
# s3 <- s3 %>% 
#   mutate(ods = ods_scoring(s3))
# s4 <- s4 %>% 
#   mutate(ods = ods_scoring(s4))


  
# clean up
rm(list = c("success_scoring", "ods_scoring", "engage_scoring", "ease_scoring"))
