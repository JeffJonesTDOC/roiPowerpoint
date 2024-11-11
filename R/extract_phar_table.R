# This function extracts the pharmacy claims spending table from the file. It
# also assigns appropriate column names to the table and calculates the
# difference-in-difference dollar amounts and appends them to the table.

extract_phar_table <- function (has_rx, post_period_length, ROI_sheet, pharmacy_costs_sheet,
                                claims_detail_table_column_names, claims_detail_table, program) {
  if (has_rx) {
    if (post_period_length > 1) {
      rx_costs_row_index = which(ROI_sheet[, 1] == "Pharmacy costs") +
        3
      rx_costs_column_index = which(ROI_sheet[rx_costs_row_index -
                                                3, ] == "Combined Result")
      pooled_phar_spending_table = ROI_sheet[rx_costs_row_index:(rx_costs_row_index +
                                                                   2), rx_costs_column_index:(rx_costs_column_index +
                                                                                                (7 + 5 * (post_period_length - 1)))]
      if (!is.na(pooled_phar_spending_table[1, 1])) {
        if (pooled_phar_spending_table[1, 1] == "Total costs") {
          rx_costs_row_index = rx_costs_row_index - 1
          pooled_phar_spending_table = ROI_sheet[rx_costs_row_index:(rx_costs_row_index +
                                                                       2), rx_costs_column_index:(rx_costs_column_index +
                                                                                                    (7 + 5 * (post_period_length - 1)))]
        }
      }
      for (k in (ncol(pooled_phar_spending_table) - post_period_length +
                 1):ncol(pooled_phar_spending_table)) {
        pooled_phar_spending_table[1, k] = paste("DID Y",
                                                 abs(k - ncol(pooled_phar_spending_table) +
                                                       post_period_length), " vs. Y0", sep = "")
      }
      colnames(pooled_phar_spending_table) = pooled_phar_spending_table[1,
      ]
      colnames(pooled_phar_spending_table)[1] = " "
      if (pooled_phar_spending_table[2, 1] == "Total costs") {
        pooled_phar_spending_table = pooled_phar_spending_table[2:3,
        ]
      }
    }
    else {
      pooled_phar_spending_table = pharmacy_costs_sheet[min(which(pharmacy_costs_sheet[,2] == "Total Rx costs")):(min(which(grepl("Pharmacy cost calculated by",pharmacy_costs_sheet[,2])))-1), 2:ncol(pharmacy_costs_sheet)]
      if (program == "Hypertension"){pooled_phar_spending_table = pooled_phar_spending_table[1:2,]; pooled_phar_spending_table[2,1] = "Hypertension Rx costs"}
      colnames(pooled_phar_spending_table) = claims_detail_table_column_names
    }
    for (l in 2:ncol(pooled_phar_spending_table)) {
      m = l + 1
      while (m < ncol(pooled_phar_spending_table)) {
        if (colnames(pooled_phar_spending_table)[l] ==
            colnames(pooled_phar_spending_table)[m]) {
          colnames(pooled_phar_spending_table)[l] = paste(colnames(pooled_phar_spending_table)[l],
                                                          " ", sep = "")
        }
        m = m + 1
      }
    }
    {
      starting_column_count = ncol(pooled_phar_spending_table)
      for (i in 1:post_period_length) {
        pooled_phar_spending_table[, starting_column_count +
                                     i] = (as.numeric(pooled_phar_spending_table[,
                                                                                 3 + post_period_length * 2 + i]) - as.numeric(pooled_phar_spending_table[,
                                                                                                                                                          3 + post_period_length * 2])) - (as.numeric(pooled_phar_spending_table[,
                                                                                                                                                                                                                                 2 + i]) - as.numeric(pooled_phar_spending_table[,
                                                                                                                                                                                                                                                                                 2]))
        colnames(pooled_phar_spending_table)[starting_column_count +
                                               i] = colnames(claims_detail_table)[starting_column_count +
                                                                                    i]
      }
    }
    #    for (j in which(grepl("%",names(phar_detail_table)))) {
    #      pooled_phar_spending_table[,j] = percent(as.numeric(pooled_phar_spending_table[,j]))
    #    }

  }
  else {
    pooled_phar_spending_table = NULL
  }
  return(pooled_phar_spending_table)
}
