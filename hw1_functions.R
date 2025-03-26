drop_single_value_col <- function(df) {
  only_one_or_zero <- sapply(df, function(x) length(unique(na.omit(x))) <= 1)
  
  df[, !only_one_or_zero, drop = FALSE]
}
