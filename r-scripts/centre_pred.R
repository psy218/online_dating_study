centre_pred <- function(df, var = c("promotion", "prevention", ...)) {
  for (i in seq_along(var)) {
    x = df[[var[i]]]
    centred_varName = paste(var[i], ".c", sep = "")
    
    df$dummy_var = x - mean(x, na.rm = T)
    names(df)[names(df)=="dummy_var"] = centred_varName
  }
  return(df)
}
