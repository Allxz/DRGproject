#' Boxplot for Payment Methods
#'
#'This function produces a boxplot based on DRG payment methods by DRG code:
#'average Medicare payments, the average total payment, or the average covered charges.
#'
#' @param df a dataframe
#' @param payments three types payment methods:
#' average Medicare payments, the average total payment, or the average covered charges
#'
#' @return a list of boxplots of payment method by DRG code.
#' @export
#'
#' @examples
#' DRG_box(df = DRG, payments = "Average.Medicare.Payments")
#' DRG_box(df = DRG, payments = "Average.Covered.Charges")
#' DRG_box(df = DRG, payments = "Average.Total.Payments")
#'
### Function 1:
# Boxplot for average Medicare payments, the average total payment, or the average covered charges
DRG_box <- function(df, payments){
  scaled_df <- df %>% # scaled payments unit into k
    mutate(scaled_payments = (get(payments) / 1000)) # scaled variale names scaled_payments

  x_title <- str_to_title(gsub("\\.", " ", payments)) # remove dot from string payments

  scaled_df %>%
    ggplot(aes(x = scaled_payments, y = reorder(`DRG Code`, desc(`DRG Code`)))) +
    geom_boxplot(outlier.size = 0.1) + # set outliers' size
    ggtitle(paste("Boxplot for", x_title)) + # add title
    theme(plot.title = element_text(face = "bold")) + # change title to bold
    xlab(paste(x_title, "Unit(k)")) + # add x lab
    ylab("DRG Code") + # add y lab
    theme(legend.position = "none") + # remove color legend
    theme(axis.text.y = element_text(size = 5)) # set y label size to be clear
}
