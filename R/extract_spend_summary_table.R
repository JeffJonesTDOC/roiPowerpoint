extract_spend_summary_table <- function(post_period_length, ROI_sheet,combine_ip_op,year0,year1,remove_POS,HTN_population) {
  suppressWarnings({
    if (post_period_length > 2) {
      table_string_match = paste("All",post_period_length,"years pooled together")
    } else if (post_period_length == 2) {
      table_string_match = "Combined Result"
    } else if (post_period_length == 1) {
      table_string_match = "Medical spending per member per month"
    }

    if (post_period_length == 2 && program=="Hypertension") {
      roi_table_column_index = 15
    } else {roi_table_column_index = 1}
    while (roi_table_column_index < ncol(ROI_sheet)) {
      roi_table_row_index = which(ROI_sheet[,roi_table_column_index]==table_string_match)
      if (length(roi_table_row_index) > 1) {roi_table_row_index = roi_table_row_index[1];break}
      else if (length(roi_table_row_index) == 1) {break}
      else {roi_table_column_index = roi_table_column_index+1}
    }
    # Find "Total Costs" and "PCP" within the bounds of roi_table_row/column_index to determine the
    # dimensions of the table.
    {
      total_cost_index = roi_table_row_index
      while (total_cost_index<roi_table_row_index+50) {
        total_cost_check = ROI_sheet[total_cost_index,roi_table_column_index]
        if (is.na(total_cost_check)) {
          total_cost_index=total_cost_index+1
        } else if (total_cost_check == "Total costs") {
          break
        } else {total_cost_index=total_cost_index+1}
      }

      pcp_index = roi_table_row_index
      while (pcp_index<roi_table_row_index+50) {
        pcp_check = ROI_sheet[pcp_index,roi_table_column_index]
        if (is.na(pcp_check)) {
          pcp_index=pcp_index+1
        } else if (pcp_check == "PCP") {
          break
        } else {pcp_index=pcp_index+1}
      }
    }

    spend_summary_table = ROI_sheet[total_cost_index:pcp_index,roi_table_column_index:(roi_table_column_index+(7+5*(post_period_length-1)))]
    spend_summary_table[is.na(spend_summary_table)] <- 0;


    # Remove POS if remove_POS is true.
    if (remove_POS) {
      er_visits_index = which(spend_summary_table[,1] == 'ER visits')
      office_visits_index = which(spend_summary_table[,1] == "Office visits")
      spend_summary_table = spend_summary_table[c(1:(er_visits_index-1),(office_visits_index+1):nrow(spend_summary_table)),]
    }

    # Remove Hypoglycemia, Lab, Ambulance, and PCP.
    spend_summary_table = spend_summary_table[-c(which(spend_summary_table[,1] %in% c("Hypoglycemia-related","Lab","Ambulance","PCP"))),]

    # Create a flextable object, using the spend_summary_table as reference.
    {

      # Define some variables for use in IP and OP combining, if combine_ip_op true.
      if (combine_ip_op && !remove_POS) {
        combine_ip_op_array = array(dim=ncol(spend_summary_table))
        ip_index = which(spend_summary_table[,1] == "Inpatient hospital, non-ER visits")
        op_index = which(spend_summary_table[,1] == "Outpatient hospital, non-ER visits")
      }

      # Rounding and percentage-conversions. For each column of the table,
      # if the median value is <1 then it is assumed to be a percentage column.
      # Otherwise, it is a $$$ spending column.
      for (i in 2:ncol(spend_summary_table)) {
        if (median(na.omit(as.numeric(spend_summary_table[which(spend_summary_table[,i]!=0),i]))) < 1) {
          spend_summary_table[,i] = percent(as.numeric(spend_summary_table[,i]))
        } else {
          spend_summary_table[,i] = round(as.numeric(spend_summary_table[,i]),0)
          # If combine_ip_op true, then sum together into a new array to be attached to spend_summary_table.
          # Percent changes and DID are calculated further below.
          if (combine_ip_op && !remove_POS) {
            combine_ip_op_array[i] = sum(spend_summary_table[ip_index:op_index,i])
          }
        }
      }

      # Calculate new percent changes and did for combined ip and op, then attach to spend_summary_table
      if (combine_ip_op && !remove_POS) {
        if (study == "YOY" | study == "1YR"){
          combine_ip_op_array[4] = (combine_ip_op_array[3]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[7] = (combine_ip_op_array[6]-combine_ip_op_array[5])/combine_ip_op_array[5]
          combine_ip_op_array[8] = combine_ip_op_array[7]-combine_ip_op_array[4]
        }
        if (study == "2YR") {
          combine_ip_op_array[5] = (combine_ip_op_array[3]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[6] = (combine_ip_op_array[4]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[10] = (combine_ip_op_array[8]-combine_ip_op_array[7])/combine_ip_op_array[7]
          combine_ip_op_array[11] = (combine_ip_op_array[9]-combine_ip_op_array[7])/combine_ip_op_array[7]
          combine_ip_op_array[12] = combine_ip_op_array[10]-combine_ip_op_array[5]
          combine_ip_op_array[13] = combine_ip_op_array[11]-combine_ip_op_array[6]
        }
        if (study == "3YR") {
          combine_ip_op_array[6] = (combine_ip_op_array[3]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[7] = (combine_ip_op_array[4]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[8] = (combine_ip_op_array[5]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[13] = (combine_ip_op_array[10]-combine_ip_op_array[9])/combine_ip_op_array[9]
          combine_ip_op_array[14] = (combine_ip_op_array[11]-combine_ip_op_array[9])/combine_ip_op_array[9]
          combine_ip_op_array[15] = (combine_ip_op_array[12]-combine_ip_op_array[9])/combine_ip_op_array[9]
          combine_ip_op_array[16] = combine_ip_op_array[13]-combine_ip_op_array[6]
          combine_ip_op_array[17] = combine_ip_op_array[14]-combine_ip_op_array[7]
          combine_ip_op_array[18] = combine_ip_op_array[15]-combine_ip_op_array[8]
        }
        if (study == "4YR") {
          combine_ip_op_array[7] = (combine_ip_op_array[3]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[8] = (combine_ip_op_array[4]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[9] = (combine_ip_op_array[5]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[10] = (combine_ip_op_array[6]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[16] = (combine_ip_op_array[12]-combine_ip_op_array[11])/combine_ip_op_array[11]
          combine_ip_op_array[17] = (combine_ip_op_array[13]-combine_ip_op_array[11])/combine_ip_op_array[11]
          combine_ip_op_array[18] = (combine_ip_op_array[14]-combine_ip_op_array[11])/combine_ip_op_array[11]
          combine_ip_op_array[19] = (combine_ip_op_array[15]-combine_ip_op_array[11])/combine_ip_op_array[11]
          combine_ip_op_array[20] = combine_ip_op_array[16]-combine_ip_op_array[7]
          combine_ip_op_array[21] = combine_ip_op_array[17]-combine_ip_op_array[8]
          combine_ip_op_array[22] = combine_ip_op_array[18]-combine_ip_op_array[9]
          combine_ip_op_array[23] = combine_ip_op_array[19]-combine_ip_op_array[10]
        }
        for (i in 2:ncol(spend_summary_table)) {if (is.character(spend_summary_table[1,i])) {combine_ip_op_array[i]=percent(as.numeric(combine_ip_op_array[i]))}}
        combine_ip_op_array[1] = "Hospital Visits, IP and OP"

        spend_summary_table = rbind(spend_summary_table[1:(which(spend_summary_table[,1]=="Inpatient hospital, non-ER visits")-1),],
                                  combine_ip_op_array,
                                  spend_summary_table[(which(spend_summary_table[,1]=="Outpatient hospital, non-ER visits")+1):nrow(spend_summary_table),])
      }


      # Define column names of the table, dynamically since it varies by post_period_length.
      {
        spend_summary_table_colnames = array(dim=(post_period_length*5)+3)
        if (post_period_length == 1) {
          spend_summary_table_colnames = c("PMPM Costs",year0,year1,paste0("% Diff ",year0," vs ",year1),paste0(" ",year0),paste0(" ",year1),paste0(" % Diff ",year0," vs ",year1),"DID %")
        } else {
          spend_summary_table_colnames[(1:2)] = c("PMPM Costs","Y0"); spend_summary_table_colnames[3+(post_period_length*2)] = " Y0"
          for (i in 1:post_period_length) {
            spend_summary_table_colnames[2+i] = paste("Y",i,sep="")
            spend_summary_table_colnames[2+(post_period_length)+i] = paste("% Diff Y",i," vs Y0",sep="")
            spend_summary_table_colnames[3+(post_period_length*2)+i] = paste(" Y",i,sep="")
            spend_summary_table_colnames[3+(post_period_length*3)+i] = paste(" % Diff Y",i," vs Y0",sep="")
            spend_summary_table_colnames[3+(post_period_length*4)+i] = paste("DID Y",i," vs Y0",sep="")
          }
        }
        colnames(spend_summary_table) = spend_summary_table_colnames
      }

      # Add DID amount columns to the end of the summary spending table
      {
        starting_column_count = ncol(spend_summary_table)
        for (i in 1:post_period_length) {
          spend_summary_table[,starting_column_count+i] = (as.numeric(spend_summary_table[,3+post_period_length*2+i])-as.numeric(spend_summary_table[,3+post_period_length*2]))-(as.numeric(spend_summary_table[,2+i])-as.numeric(spend_summary_table[,2]))
          if (post_period_length == 1) {
            colnames(spend_summary_table)[starting_column_count+i] = paste0("DID $ ",year1," vs ",year0)
          } else {colnames(spend_summary_table)[starting_column_count+i] = paste0("DID $ Y",i," vs Y0")}
        }
      }

      spend_summary_ft_prep = spend_summary_table

      # Add on the "$" to the costs
      for (k in 2:ncol(spend_summary_ft_prep)) {
        if (!is.na(as.numeric(spend_summary_ft_prep[2,k]))) {
          spend_summary_ft_prep[,k] = paste("$",spend_summary_ft_prep[,k],sep="")
          spend_summary_ft_prep[,k] = gsub("\\$-","-$",spend_summary_ft_prep[,k])
        }
      }

      # Replace any NA values with 0
      spend_summary_table[is.na(spend_summary_table)] = 0

      # Generate the flextable and specify properties.
      {
        spend_summary_ft <- flextable::flextable(spend_summary_ft_prep)
        if (program == "Hypertension" && HTN_population != "All" && study %in% c("YOY","1YR")) {
          spend_summary_ft <- flextable::add_header_row(spend_summary_ft,values = c(" ","Member","Non-Member"," "), colwidths = c(1,2*post_period_length+1,2*post_period_length+1,post_period_length*2))
        } else {
          spend_summary_ft <- flextable::add_header_row(spend_summary_ft,values = c(" ","Non-member","Member"," "), colwidths = c(1,2*post_period_length+1,2*post_period_length+1,post_period_length*2))
        }
        spend_summary_ft <- flextable::color(spend_summary_ft,color="white", part="header")
        spend_summary_ft <- flextable::color(spend_summary_ft,j=1,color="white",part="body")
        spend_summary_ft <- flextable::bg(spend_summary_ft,bg = "#66478F", part="header")
        spend_summary_ft <- flextable::bg(spend_summary_ft,j=1,bg = "#55437d", part="body")
        for (i1 in 1:nrow(spend_summary_table)) {
          for (j1 in (ncol(spend_summary_table)-2*post_period_length+1):ncol(spend_summary_table)) {
            if (spend_summary_table[i1,j1] > 0) {
              spend_summary_ft <- flextable::bg(spend_summary_ft, i=i1, j=j1, bg="#E1DDE5",part="body")
            } else {
              spend_summary_ft <- flextable::bg(spend_summary_ft, i=i1, j=j1, bg="#c6efcd",part="body")
            }
          }
        }
        spend_summary_ft <- flextable::align(spend_summary_ft,align=c("center"),part="all")
        spend_summary_ft <- flextable::width(spend_summary_ft,j=1,width=1.5)
        spend_summary_ft <- flextable::width(spend_summary_ft,j=2:ncol(spend_summary_table),width=0.5)
        spend_summary_ft <- flextable::height(spend_summary_ft,height = 0.05,part="body")
        spend_summary_ft <- flextable::fontsize(spend_summary_ft,size=8,part="all")
        spend_summary_ft <- flextable::font(spend_summary_ft,fontname="century gothic",part="all")
        spend_summary_ft <- flextable::border_outer(spend_summary_ft,border=officer::fp_border(color="black",style="solid",width=2),part="all")
        spend_summary_ft <- flextable::border_inner(spend_summary_ft,border=officer::fp_border(color="black",style="solid",width=1),part="all")
      }

    }

  })
  return_list = list(spend_summary_table,spend_summary_ft,spend_summary_table_colnames)
  names(return_list) = c("table","flex_table","column_names")
  return(return_list)
}
