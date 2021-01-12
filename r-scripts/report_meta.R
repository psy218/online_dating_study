#' function that meta-analyze main effects of promotion and prevention on a given outcome (exluding the follow-up data, i.e., Study 5

report_meta = function(meta_data, term, method = "FE") {
  meta_s1to4_data = meta_data %>%
    filter(Study != 5) %>% # excluding the follow-up study for the meta-analysis
    dplyr::select(ti = !!term,
                  r2i = r2,
                  ni = ni,
                  Study) %>% 
    mutate(mi = rep(3, nrow(.)))
  
  result  = metafor::rma(data = meta_s1to4_data,
                         method = method, # fixe-effects model
                         measure = "SPCOR", # semi-partial correlation
                         ti = ti, 
                         r2i = r2i,
                         ni =  ni,
                         mi = mi, # number of predictors
                         slab = paste0(Study, " [N = ", ni, "]"))
  return(result)
}
