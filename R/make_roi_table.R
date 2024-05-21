# This function's primary purpose is to generate the ROI table used in the
# ROI results slide. However, it also produces and returns several other ROI-related
# tables and arrays used in other slides or for data capture.


make_roi_table <- function(has_rx, nYear, program, final_cohort_size,price_of_program,PPPM_changes, supply_cost, pooled_cohort_table2) {
  # Define final_roi_table, which becomes a flextable object, formatted
  # and injected into the powerpoint slide.
  {
    if (has_rx == T) {
      final_roi_table = as.data.frame(matrix(nrow=6,ncol=nYear+1))
      final_roi_table[,1] = c("Net Medical Costs","Net Rx Costs","Net Diabetes Rx Costs","ROI","Rx-Adjusted ROI","DM Rx-Adjusted ROI")
      final_roi_table_headers <- array(" ")
    } else {
      final_roi_table = as.data.frame(matrix(nrow=2,ncol=nYear+1))
      final_roi_table[,1] = c("Net Medical Costs","ROI")
      final_roi_table_headers <- array(" ")
    }
    executive_summary_roi_array = array() # used later on in the executive summary slide generation
    rx_roi_array = array(); no_rx_roi_array = array(); diabetes_rx_roi_array = array()
    for (i in 1:nYear) {
      if (nYear > 1) {final_roi_table_headers[i+1] = paste("Year ",i," (N=",as.numeric(pooled_cohort_table2[3,i+1]),")",sep="")}
      if (nYear == 1){
        if (study=="YOY") {
          final_roi_table_headers[i+1] = paste0("YoY (N=",final_cohort_size[i],")")
        } else {final_roi_table_headers[i+1] = paste("Year ",i," (N=",final_cohort_size[i],")",sep="")}}
      roi_table_column_match = which(colnames(PPPM_changes) == paste("Year",i,"Change"))
      if (has_rx == T) {
        final_roi_table[1,1+i] = paste(PPPM_changes[1,roi_table_column_match+1],"\n($",PPPM_changes[1,roi_table_column_match]," PMPM medical savings)",sep="")
        final_roi_table[2,1+i] = paste(PPPM_changes[2,roi_table_column_match+1],"\n($",PPPM_changes[2,roi_table_column_match]," PMPM medical savings)",sep="")
        final_roi_table[3,1+i] = paste(PPPM_changes[3,roi_table_column_match+1],"\n($",PPPM_changes[3,roi_table_column_match]," PMPM medical savings)",sep="")
        ROI = as.numeric(PPPM_changes[1,roi_table_column_match]) / (as.numeric(price_of_program)-as.numeric(supply_cost))
        rxROI = (as.numeric(PPPM_changes[1,roi_table_column_match]) + as.numeric(PPPM_changes[2,roi_table_column_match])) / (as.numeric(price_of_program)-as.numeric(supply_cost))
        diabetes_rxROI = (as.numeric(PPPM_changes[1,roi_table_column_match]) + as.numeric(PPPM_changes[3,roi_table_column_match])) / (as.numeric(price_of_program)-as.numeric(supply_cost))
        if (program == "Diabetes") {
          final_roi_table[4,1+i] = paste("$",PPPM_changes[1,roi_table_column_match],' ÷ ($',price_of_program,"-$",round(as.numeric(supply_cost),0),") = ",round(ROI,1),sep="")
          final_roi_table[5,1+i] = paste("($",PPPM_changes[1,roi_table_column_match],"+","$",PPPM_changes[2,roi_table_column_match],') ÷ ($',price_of_program,"-$",round(as.numeric(supply_cost),0),") = ",round(rxROI,1),sep="")
          final_roi_table[6,1+i] = paste("($",PPPM_changes[1,roi_table_column_match],"+","$",PPPM_changes[3,roi_table_column_match],') ÷ ($',price_of_program,"-$",round(as.numeric(supply_cost),0),") = ",round(diabetes_rxROI,1),sep="")
        } else if (program == "Hypertension") {
          final_roi_table[4,1+i] = paste("$",PPPM_changes[1,roi_table_column_match],' ÷ $',price_of_program," = ",round(ROI,1),sep="")
          final_roi_table[5,1+i] = paste("($",PPPM_changes[1,roi_table_column_match],"+","$",PPPM_changes[2,roi_table_column_match],') ÷ $',price_of_program," = ",round(rxROI,1),sep="")
          final_roi_table[6,1+i] = paste("($",PPPM_changes[1,roi_table_column_match],"+","$",PPPM_changes[3,roi_table_column_match],') ÷ $',price_of_program," = ",round(diabetes_rxROI,1),sep="")
        }
      } else {
        final_roi_table[1,1+i] = paste(PPPM_changes[1,roi_table_column_match+1],"\n($",PPPM_changes[1,roi_table_column_match]," PMPM medical savings)",sep="")
        ROI = as.numeric(PPPM_changes[1,roi_table_column_match]) / (as.numeric(price_of_program)-as.numeric(supply_cost))
        if (program == "Diabetes") {
          final_roi_table[2,1+i] = paste("$",PPPM_changes[1,roi_table_column_match],' ÷ ($',price_of_program,"-$",round(as.numeric(supply_cost),0),") = ",round(ROI,1),sep="")
        } else if (program == "Hypertension") {
          final_roi_table[2,1+i] = paste("$",PPPM_changes[1,roi_table_column_match],' ÷ $',price_of_program," = ",round(ROI,1),sep="")
        }
      }
      if (has_rx) {
        executive_summary_roi_array[i] = round(rxROI,1)
        rx_roi_array[i] = round(rxROI,1); no_rx_roi_array[i] = round(ROI,1); diabetes_rx_roi_array[i] = round(diabetes_rxROI,1)
      } else {
        executive_summary_roi_array[i] = round(ROI,1)
      }
    }
    colnames(final_roi_table) = final_roi_table_headers
  }

  # Now, turn final_roi_table into a flextable
  {
    roi_flextable <- flextable::flextable(final_roi_table)
    roi_flextable <- flextable::bold(roi_flextable,j=1,bold=TRUE)
    roi_flextable <- flextable::bold(roi_flextable,bold=TRUE,part="header")
    roi_flextable <- flextable::width(roi_flextable,width=(5/(nYear+1)))
    roi_flextable <- flextable::bg(roi_flextable, bg="#66478F",part="header")
    roi_flextable <- flextable::color(roi_flextable,color="white",part="header")
    roi_flextable <- flextable::border_outer(roi_flextable,border=officer::fp_border(color="black",style="solid",width=1.5))
    roi_flextable <- flextable::border_inner(roi_flextable,border=officer::fp_border(color="black",style="solid",width=1))
    roi_flextable <- flextable::fontsize(roi_flextable,size=9,part="all")
    roi_flextable <- flextable::font(roi_flextable,fontname = "century gothic",part="all")
  }
  returnList = list(roi_flextable,executive_summary_roi_array,no_rx_roi_array,rx_roi_array,diabetes_rx_roi_array,ROI,final_roi_table,roi_flextable)
  names(returnList) = c("roi_flextable","executive_summary_roi_array","no_rx_roi_array","rx_roi_array","diabetes_rx_roi_array","ROI","final_roi_table","roi_flextable")
  return(returnList)
}
