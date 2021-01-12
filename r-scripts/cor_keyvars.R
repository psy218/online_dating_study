#' create a correlation table between variables in `key_vars` dataset. 
cor_keyvars = function(key_vars) {
  key_vars %>% 
    corrr::correlate() %>% 
    shave() %>% 
    fashion()
}
