meta_summary <- function(meta_model) {
  meta_model = meta_model %>% 
    mutate_if(is.double, round, 2)
  
  p.val = ifelse(meta_model$p.value < 0.001, "< .001", gsub("^0.", "= .", sprintf("%0.2f", meta_model$p.value)))
  
  str_c("$M_{sr}$ = ", meta_model$estimate,", $SE$ = ", meta_model$std.error, ", $Z$ = ", meta_model$statistic, ",  $p_{two-tailed}$ ", p.val)
}

