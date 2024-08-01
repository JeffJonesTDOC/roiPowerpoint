extract_claims_detail_table <- function(post_period_length, roi_sheet, year0, remove_POS, HTN_population = "All", combine_ip_op = FALSE) {
  suppressWarnings({

    if(typeof(year0) != "character") {year0 = as.character(year0)}
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
    while (roi_table_column_index < ncol(roi_sheet)) {
      roi_table_row_index = which(roi_sheet[,roi_table_column_index]==table_string_match)
      if (length(roi_table_row_index) > 1) {roi_table_row_index = roi_table_row_index[1];break}
      else if (length(roi_table_row_index) == 1) {break}
      else {roi_table_column_index = roi_table_column_index+1}
    }
    # Find "Total Costs" and "PCP" within the bounds of roi_table_row/column_index to determine the
    # dimensions of the table.
    {
      total_cost_index = roi_table_row_index
      while (total_cost_index<roi_table_row_index+50) {
        total_cost_check = roi_sheet[total_cost_index,roi_table_column_index]
        if (is.na(total_cost_check)) {
          total_cost_index=total_cost_index+1
        } else if (total_cost_check == "Total costs") {
          break
        } else {total_cost_index=total_cost_index+1}
      }

      pcp_index = roi_table_row_index
      while (pcp_index<roi_table_row_index+50) {
        pcp_check = roi_sheet[pcp_index,roi_table_column_index]
        if (is.na(pcp_check)) {
          pcp_index=pcp_index+1
        } else if (pcp_check == "PCP") {
          break
        } else {pcp_index=pcp_index+1}
      }
    }

    claims_detail_table = roi_sheet[total_cost_index:pcp_index,roi_table_column_index:(roi_table_column_index+(7+5*(post_period_length-1)))]
    claims_detail_table[is.na(claims_detail_table)] <- 0;


    # Remove POS if remove_POS is true.
    if (remove_POS) {
      er_visits_index = which(claims_detail_table[,1] == 'ER visits')
      office_visits_index = which(claims_detail_table[,1] == "Office visits")
      claims_detail_table = claims_detail_table[c(1:(er_visits_index-1),(office_visits_index+1):nrow(claims_detail_table)),]
    }

    # Remove Hypoglycemia, Lab, Ambulance, and PCP.
    claims_detail_table = claims_detail_table[-c(which(claims_detail_table[,1] %in% c("Hypoglycemia-related","Lab","Ambulance","PCP"))),]

    # Create a flextable object, using the claims_detail_table as reference.
    {

      # Define some variables for use in IP and OP combining, if combine_ip_op true.
      if (combine_ip_op && !remove_POS) {
        combine_ip_op_array = array(dim=ncol(claims_detail_table))
        ip_index = which(claims_detail_table[,1] == "Inpatient hospital, non-ER visits")
        op_index = which(claims_detail_table[,1] == "Outpatient hospital, non-ER visits")
      }

      # Rounding and percentage-conversions. For each column of the table,
      # if the median value is <1 then it is assumed to be a percentage column.
      # Otherwise, it is a $$$ spending column.
      for (i in 2:ncol(claims_detail_table)) {
        if (median(na.omit(as.numeric(claims_detail_table[which(claims_detail_table[,i]!=0),i]))) < 1) {
          claims_detail_table[,i] = percent(as.numeric(claims_detail_table[,i]))
        } else {
          claims_detail_table[,i] = round(as.numeric(claims_detail_table[,i]),0)
          # If combine_ip_op true, then sum together into a new array to be attached to claims_detail_table.
          # Percent changes and DID are calculated further below.
          if (combine_ip_op && !remove_POS) {
            combine_ip_op_array[i] = sum(claims_detail_table[ip_index:op_index,i])
          }
        }
      }

      # Calculate new percent changes and did for combined ip and op, then attach to claims_detail_table
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
        for (i in 2:ncol(claims_detail_table)) {if (is.character(claims_detail_table[1,i])) {combine_ip_op_array[i]=percent(as.numeric(combine_ip_op_array[i]))}}
        combine_ip_op_array[1] = "Hospital Visits, IP and OP"

        claims_detail_table = rbind(claims_detail_table[1:(which(claims_detail_table[,1]=="Inpatient hospital, non-ER visits")-1),],
                                  combine_ip_op_array,
                                  claims_detail_table[(which(claims_detail_table[,1]=="Outpatient hospital, non-ER visits")+1):nrow(claims_detail_table),])
      }


      # Define column names of the table, dynamically since it varies by post_period_length.
      {
        claims_detail_table_colnames = array(dim=(post_period_length*5)+3)
        if (post_period_length == 1) {
          claims_detail_table_colnames = c("PMPM Costs",year0,as.character(as.numeric(year0)+1),paste0("% Diff ",year0," vs ",as.character(as.numeric(year0)+1)),paste0(" ",year0),paste0(" ",as.character(as.numeric(year0)+1)),paste0(" % Diff ",year0," vs ",as.character(as.numeric(year0)+1)),"DID %")
        } else {
          claims_detail_table_colnames[(1:2)] = c("PMPM Costs","Y0"); claims_detail_table_colnames[3+(post_period_length*2)] = " Y0"
          for (i in 1:post_period_length) {
            claims_detail_table_colnames[2+i] = paste("Y",i,sep="")
            claims_detail_table_colnames[2+(post_period_length)+i] = paste("% Diff Y",i," vs Y0",sep="")
            claims_detail_table_colnames[3+(post_period_length*2)+i] = paste(" Y",i,sep="")
            claims_detail_table_colnames[3+(post_period_length*3)+i] = paste(" % Diff Y",i," vs Y0",sep="")
            claims_detail_table_colnames[3+(post_period_length*4)+i] = paste("DID Y",i," vs Y0",sep="")
          }
        }
        colnames(claims_detail_table) = claims_detail_table_colnames
      }

      # Add DID amount columns to the end of the summary spending table
      {
        starting_column_count = ncol(claims_detail_table)
        for (i in 1:post_period_length) {
          claims_detail_table[,starting_column_count+i] = (as.numeric(claims_detail_table[,3+post_period_length*2+i])-as.numeric(claims_detail_table[,3+post_period_length*2]))-(as.numeric(claims_detail_table[,2+i])-as.numeric(claims_detail_table[,2]))
          if (post_period_length == 1) {
            colnames(claims_detail_table)[starting_column_count+i] = paste0("DID $ ",as.character(as.numeric(year0)+1)," vs ",year0)
          } else {colnames(claims_detail_table)[starting_column_count+i] = paste0("DID $ Y",i," vs Y0")}
        }
      }

      claims_detail_ft_prep = claims_detail_table

      # Add on the "$" to the costs
      for (k in 2:ncol(claims_detail_ft_prep)) {
        if (!is.na(as.numeric(claims_detail_ft_prep[2,k]))) {
          claims_detail_ft_prep[,k] = paste("$",claims_detail_ft_prep[,k],sep="")
          claims_detail_ft_prep[,k] = gsub("\\$-","-$",claims_detail_ft_prep[,k])
        }
      }

      # Replace any NA values with 0
      claims_detail_table[is.na(claims_detail_table)] = 0

      # Generate the flextable and specify properties.
      {
        claims_detail_ft <- flextable::flextable(claims_detail_ft_prep)
        if (program == "Hypertension" && HTN_population != "All" && study %in% c("YOY","1YR")) {
          claims_detail_ft <- flextable::add_header_row(claims_detail_ft,values = c(" ","Member","Non-Member"," "), colwidths = c(1,2*post_period_length+1,2*post_period_length+1,post_period_length*2))
        } else {
          claims_detail_ft <- flextable::add_header_row(claims_detail_ft,values = c(" ","Non-member","Member"," "), colwidths = c(1,2*post_period_length+1,2*post_period_length+1,post_period_length*2))
        }
        claims_detail_ft <- flextable::color(claims_detail_ft,color="white", part="header")
        claims_detail_ft <- flextable::color(claims_detail_ft,j=1,color="white",part="body")
        claims_detail_ft <- flextable::bg(claims_detail_ft,bg = "#66478F", part="header")
        claims_detail_ft <- flextable::bg(claims_detail_ft,j=1,bg = "#55437d", part="body")
        for (i1 in 1:nrow(claims_detail_table)) {
          for (j1 in (ncol(claims_detail_table)-2*post_period_length+1):ncol(claims_detail_table)) {
            if (claims_detail_table[i1,j1] > 0) {
              claims_detail_ft <- flextable::bg(claims_detail_ft, i=i1, j=j1, bg="#E1DDE5",part="body")
            } else {
              claims_detail_ft <- flextable::bg(claims_detail_ft, i=i1, j=j1, bg="#c6efcd",part="body")
            }
          }
        }
        claims_detail_ft <- flextable::align(claims_detail_ft,align=c("center"),part="all")
        claims_detail_ft <- flextable::width(claims_detail_ft,j=1,width=1.5)
        claims_detail_ft <- flextable::width(claims_detail_ft,j=2:ncol(claims_detail_table),width=0.5)
        claims_detail_ft <- flextable::height(claims_detail_ft,height = 0.05,part="body")
        claims_detail_ft <- flextable::fontsize(claims_detail_ft,size=8,part="all")
        claims_detail_ft <- flextable::font(claims_detail_ft,fontname="century gothic",part="all")
        claims_detail_ft <- flextable::border_outer(claims_detail_ft,border=officer::fp_border(color="black",style="solid",width=2),part="all")
        claims_detail_ft <- flextable::border_inner(claims_detail_ft,border=officer::fp_border(color="black",style="solid",width=1),part="all")
      }

    }

  })
  return_list = list(claims_detail_table,claims_detail_ft,claims_detail_table_colnames)
  names(return_list) = c("table","flex_table","column_names")
  return(return_list)
}
