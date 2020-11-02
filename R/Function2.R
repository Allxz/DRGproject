#' Statistical Calculation for Average Medicare Payments
#'
#'This function calculates mean, median, and standard deviation
#'over all of the DRG codes for Average Medicare Payments.
#'
#' @param df a dataframe
#' @param method statistical methods: mean, median, and standard deviation
#'
#' @return A table of the statistical result over all of the DRG codes
#' for Average Medicare Payments based on the methods
#' @export
#'
#' @examples
#' medicare_stat (DRG, mean)
#' medicare_stat (DRG, median)
#' medicare_stat (DRG, sd)
#'
### Function 2:
# Calculates statistics over all of the DRG codesfor Average Medicare payments
medicare_stat <- function (df, method) {
  table <- df %>%
    select(`DRG Code`, `DRG Description`, Average.Medicare.Payments) %>%
    group_by(`DRG Code`, `DRG Description`) %>%
    summarize(summ = method(Average.Medicare.Payments)) %>% # apply functions to data
    knitr::kable(caption = "Average Medicare Payment Summarize", # add data in table with title
          # change colume names
          col.names = c("DRG Code", "DRG Description", "Average Medicare Payments"))
  return(table)
}
