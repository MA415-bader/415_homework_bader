drop_single_value_col <- function(df) {
  only_one_or_zero <- sapply(df, function(x) length(unique(na.omit(x))) <= 1)
  
  df[, !only_one_or_zero, drop = FALSE]
}

convert_numeric <- function(df, cols_to_convert) {
  df <- df %>%
    mutate(across(all_of(cols_to_convert), 
                  ~ ifelse(grepl("^[0-9.,]+$", .),
                           as.numeric(gsub(",", "", .)),
                           NA)))
  return(df)
}

