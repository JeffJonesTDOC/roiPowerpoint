# This function will modify the spend summary table to include pharmaceutical
# data. It essentially tacks on pharmaceutical data to the bottom of the
# already-existing spend summary table. It does NOT check for existence
# of rx data; it already assumes the data is there.

include_rx_table <- function (post_period_length, pooled_phar_spending_table, claims_detail_table)
{
  require(flextable)
  for (i in seq(1, nrow(pooled_phar_spending_table))) {
    for (j in 2:ncol(pooled_phar_spending_table)) {
      if (abs(as.numeric(pooled_phar_spending_table[i,
                                                    j])) < 1) {
        pooled_phar_spending_table[i, j] = percent(as.numeric(pooled_phar_spending_table[i,
                                                                                         j]))
      }
      else {
        pooled_phar_spending_table[i, j] = round(as.numeric(pooled_phar_spending_table[i,
                                                                                       j]), 0)
      }
      pooled_phar_spending_table[i, j] = gsub("\\$-", "-$",
                                              pooled_phar_spending_table[i, j])
    }
  }
  names(pooled_phar_spending_table) = names(claims_detail_table)
  claims_detail_table_with_phar = rbind(claims_detail_table,
                                        pooled_phar_spending_table)
  numeric_claims_detail_table_with_phar = claims_detail_table_with_phar
  for (k in 2:ncol(claims_detail_table_with_phar)) {
    if (!is.na(as.numeric(claims_detail_table_with_phar[2,
                                                        k]))) {
      claims_detail_table_with_phar[, k] = paste("$", claims_detail_table_with_phar[,
                                                                                    k], sep = "")
      claims_detail_table_with_phar[, k] = gsub("\\$-",
                                                "-$", claims_detail_table_with_phar[, k])
    }
  }
  claims_detail_table_with_phar_ft <- flextable::flextable(claims_detail_table_with_phar)
  claims_detail_table_with_phar_ft <- flextable::add_header_row(claims_detail_table_with_phar_ft,
                                                                values = c(" ", "Non-member", "Member", " "), colwidths = c(1,
                                                                                                                            2 * post_period_length + 1, 2 * post_period_length +
                                                                                                                              1, post_period_length * 2))
  claims_detail_table_with_phar_ft <- flextable::bg(claims_detail_table_with_phar_ft,
                                                    bg = "#66478F", part = "header")
  claims_detail_table_with_phar_ft <- flextable::color(claims_detail_table_with_phar_ft,
                                                       color = "white", part = "header")
  claims_detail_table_with_phar_ft <- flextable::color(claims_detail_table_with_phar_ft,
                                                       color = "white", part = "body", j = 1)
  for (i1 in 1:nrow(claims_detail_table_with_phar)) {
    for (j1 in (ncol(claims_detail_table_with_phar) - 2 *
                post_period_length + 1):ncol(claims_detail_table_with_phar)) {
      if (numeric_claims_detail_table_with_phar[i1, j1] > 0) {
        claims_detail_table_with_phar_ft <- flextable::bg(claims_detail_table_with_phar_ft,
                                                          i = i1, j = j1, bg = "#E1DDE5", part = "body")
      }
      else {
        claims_detail_table_with_phar_ft <- flextable::bg(claims_detail_table_with_phar_ft,
                                                          i = i1, j = j1, bg = "#c6efcd", part = "body")
      }
    }
  }
  claims_detail_table_with_phar_ft <- flextable::bg(claims_detail_table_with_phar_ft,
                                                    j = 1, bg = "#55437d", part = "body")
  claims_detail_table_with_phar_ft <- flextable::align(claims_detail_table_with_phar_ft,
                                                       align = c("center"), part = "all")
  claims_detail_table_with_phar_ft <- flextable::width(claims_detail_table_with_phar_ft,
                                                       j = 1, width = 1.5)
  claims_detail_table_with_phar_ft <- flextable::width(claims_detail_table_with_phar_ft,
                                                       j = 2:ncol(pooled_phar_spending_table), width = 0.5)
  claims_detail_table_with_phar_ft <- flextable::fontsize(claims_detail_table_with_phar_ft,
                                                          size = 8, part = "all")
  claims_detail_table_with_phar_ft <- flextable::font(claims_detail_table_with_phar_ft,
                                                      fontname = "century gothic", part = "all")
  claims_detail_table_with_phar_ft <- flextable::border_inner(claims_detail_table_with_phar_ft,
                                                              border = officer::fp_border(color = "black", style = "solid",
                                                                                          width = 1), part = "all")
  claims_detail_table_with_phar_ft <- flextable::border_outer(claims_detail_table_with_phar_ft,
                                                              border = officer::fp_border(color = "black", style = "solid",
                                                                                          width = 2), part = "body")
  claims_detail_table_with_phar_ft <- flextable::border_outer(claims_detail_table_with_phar_ft,
                                                              border = officer::fp_border(color = "black", style = "solid",
                                                                                          width = 2), part = "all")
  claims_detail_table_with_phar_ft <- flextable::hline(claims_detail_table_with_phar_ft,
                                                       i = nrow(claims_detail_table_with_phar) - nrow(pooled_phar_spending_table), border = officer::fp_border(color = "black",
                                                                                                                                                               style = "solid", width = 3))
  return(claims_detail_table_with_phar_ft)
}
