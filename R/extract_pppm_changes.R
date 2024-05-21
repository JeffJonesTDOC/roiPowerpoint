# This function extracts the year-over-year PPPM difference in differences,
# for both medical costs and pharmacy costs, if they exist.

extract_pppm_changes <- function(has_rx, post_period_length, ROI_sheet,claims_detail_table, program, pharmacy_costs_sheet,claims_detail_table_column_names, pooled_phar_spending_table) {
  if (has_rx) {
    pppm_changes = as.data.frame(matrix(nrow=3,ncol=post_period_length*2))
    row.names(pppm_changes) = c("Net Medical Costs","Net Pharmacy Costs","Net Diabetes Rx Costs")
  } else {
    pppm_changes = as.data.frame(matrix(nrow=1,ncol=post_period_length*2))
    row.names(pppm_changes) = c("Net Medical Costs")
  }
  temporary_index = 1
  for (i in 1:post_period_length){colnames(pppm_changes)[temporary_index]=paste("Year",i,"Change");colnames(pppm_changes)[temporary_index+1]=paste("Year",i,"% Change");temporary_index=temporary_index+2}

  for (i in 1:post_period_length) {
    # Total Costs
    nonmember_cost_difference = as.numeric(claims_detail_table[1,(2+i)])-as.numeric(claims_detail_table[1,2])
    member_cost_difference = as.numeric(claims_detail_table[1,(2*post_period_length)+3+i])-as.numeric(claims_detail_table[1,(2*post_period_length)+3])
    if (program == "Hypertension" && !(study %in% c("YOY","1YR"))) {
      #if (program == "Hypertension" && "HTN_population" != "All" && study %in% c("YOY","1YR")) {
      pppm_changes[1,(2*i)-1] = round(member_cost_difference-nonmember_cost_difference,0)
    } else {
      pppm_changes[1,(2*i)-1] = round(nonmember_cost_difference-member_cost_difference,0)
    }
    if (typeof(claims_detail_table[1,ncol(claims_detail_table)-2*post_period_length+i]) == "character") {
      pppm_changes[1,(2*i)] = claims_detail_table[1,ncol(claims_detail_table)-2*post_period_length+i]
    } else { pppm_changes[1,(2*i)] = percent(as.numeric(claims_detail_table[1,ncol(claims_detail_table)-2*post_period_length+i]))}

    # Total Pharmacy Costs
    if (has_rx == T) {
      if (post_period_length == 1) { # a 1-year or YOY allows the pharmacy data to be extracted directly from the "Pharmacy costs" Sheet.
        nonmember_rx_cost_difference = as.numeric(pharmacy_costs_sheet[4,4])-as.numeric(pharmacy_costs_sheet[4,3])
        member_rx_cost_difference = as.numeric(pharmacy_costs_sheet[4,7])-as.numeric(pharmacy_costs_sheet[4,6])
        nonmember_condition_rx_cost_change = as.numeric(pharmacy_costs_sheet[5,4])-as.numeric(pharmacy_costs_sheet[5,3])
        member_condition_rx_cost_change = as.numeric(pharmacy_costs_sheet[5,7])-as.numeric(pharmacy_costs_sheet[5,6])
        pppm_changes[2,1] = round(nonmember_rx_cost_difference-member_rx_cost_difference,0)
        pppm_changes[2,2] = percent(as.numeric(pharmacy_costs_sheet[4,9]))
        pppm_changes[3,1] = round(nonmember_condition_rx_cost_change-member_condition_rx_cost_change,0)
        pppm_changes[3,2] = percent(as.numeric(pharmacy_costs_sheet[5,9]))
      }
      if (post_period_length != 1) { # Multi-year pooled Pharmacy costs take more effort to extract from the ROI sheet.

        # Extract the net $ pppm change for total phar costs and diabetic phar costs
        member_rx_cost_difference = as.numeric(pooled_phar_spending_table[1,(2*post_period_length)+3])-as.numeric(pooled_phar_spending_table[1,(2*post_period_length)+3+i])
        nonmember_rx_cost_difference = as.numeric(pooled_phar_spending_table[1,2])-as.numeric(pooled_phar_spending_table[1,(2+i)])
        pppm_changes[2,(2*i)-1] = round(member_rx_cost_difference-nonmember_rx_cost_difference,0)
        member_condition_rx_cost_change = as.numeric(pooled_phar_spending_table[2,(2*post_period_length)+3])-as.numeric(pooled_phar_spending_table[2,(2*post_period_length)+3+i])
        nonmember_condition_rx_cost_change = as.numeric(pooled_phar_spending_table[2,2])-as.numeric(pooled_phar_spending_table[2,(2+i)])
        pppm_changes[3,(2*i)-1] = round(member_condition_rx_cost_change-nonmember_condition_rx_cost_change,0)

        # Extract the % DID for total phar costs.
        tryCatch({
          pppm_changes[2,(2*i)] = percent(as.numeric(pooled_phar_spending_table[2,ncol(pooled_phar_spending_table)-2*post_period_length+i]))
        },error=function(e){
          pppm_changes[2,(2*i)] = pooled_phar_spending_table[2,ncol(pooled_phar_spending_table)-2*post_period_length+i]
        })

        # Extract the % DID for diabetic phar costs.
        tryCatch({
          pppm_changes[3,(2*i)] = percent(as.numeric(pooled_phar_spending_table[2,ncol(pooled_phar_spending_table)-2*post_period_length+i]))
        },error=function(e){
          pppm_changes[3,(2*i)] = pooled_phar_spending_table[2,ncol(pooled_phar_spending_table)-2*post_period_length+i]
        })
      }
    }
  }
  return(pppm_changes)
}
