# This function takes various data pieces and generates population attrition tables, one for each cohort. The tables
# are returned as flextable objects.


make_attrition_table <- function(post_period_length,program,summary_stats_sheet,min_activation_length,population_conditions,study_start_date) {
require(flextable)
  # Initialize a few arrays that are population in the following for loop.
  final_cohort_size = array(dim=post_period_length)
  n_activated = array(dim=post_period_length) # n_activated will be used in the result overview savings flextable.
  risk_score_array = array(dim=post_period_length); age_array = array(dim=post_period_length); member_cohort_array = array(dim=post_period_length); nonmember_cohort_array = array(dim=post_period_length)


  for (l in 1:post_period_length) {

    # Locate the starting row of summary_stats_sheet to extract the attrition table, for both variations of text.
    attrition_table_row_start = which(summary_stats_sheet[,3] == "Enrolled within study period")
    if (length(attrition_table_row_start) == 0) {attrition_table_row_start = which(summary_stats_sheet[,3] == "Ever enrolled members") }

    attrition_dataframe = summary_stats_sheet[attrition_table_row_start:(attrition_table_row_start+16),(3+7*(l-1)):(7+7*(l-1))] # Extract the lth attrition table.

    # Populate a few of the arrays with data pieces.
    risk_score_array[l] = as.numeric(summary_stats_sheet[which(summary_stats_sheet$X3=="Risk Score"),(4+7*(l-1))])
    age_array[l] = as.numeric(summary_stats_sheet[which(summary_stats_sheet$X3=="Age"),(4+7*(l-1))])

    # Remove the non-applicable "Enrolled for more than n months" rows
    if (any(grepl("9 months",attrition_dataframe[,1]))) {
      if (min_activation_length == 3) {
        attrition_dataframe = attrition_dataframe[-which(grepl("6 months|9 months",attrition_dataframe[,1])),]
      } else if (min_activation_length == 6) {
        attrition_dataframe = attrition_dataframe[-which(grepl("3 months|9 months",attrition_dataframe[,1])),]
      } else if (min_activation_length == 9) {
        attrition_dataframe = attrition_dataframe[-which(grepl("3 months|6 months",attrition_dataframe[,1])),]
      }
    } else {
      if (min_activation_length == 3) {
        attrition_dataframe = attrition_dataframe[-which(grepl("6 months|9 months",attrition_dataframe[,1])),]
      } else {
        attrition_dataframe = attrition_dataframe[-which(grepl("3 months|9 months",attrition_dataframe[,1])),]
      }
    }

    # Extract the "activated within study period" data piece.
    n_activated[l] = as.numeric(attrition_dataframe[2,4])

    # Remove the non-applicable population_conditions rows, and replace the last row with "Final Number"
    if (tolower(population_conditions) == "100k annual") {
      attrition_dataframe = attrition_dataframe[-which(grepl("50k|50,000|95th|All|Final",attrition_dataframe[,1])),]
    } else if (tolower(population_conditions) == "50k monthly") {
      attrition_dataframe = attrition_dataframe[-which(grepl("100k|100,000|All|Final",attrition_dataframe[,1])),]
    } else if (tolower(population_conditions) == "capped") {
      attrition_dataframe = attrition_dataframe[-which(grepl("50k|50,000|100k|than 100,000|All|Final",attrition_dataframe[,1])),]
    }
    attrition_dataframe[nrow(attrition_dataframe),1] = "Final Population Sample Size"


    # Remove the "Pre-launch eligible" row and rename the "Post-launch eligible" row to "Pre-launch and Post-launch eligible"
    attrition_dataframe = attrition_dataframe[-which(grepl("Pre-launch eligible",attrition_dataframe[,1])),]
    attrition_dataframe[which(grepl("Post-launch eligible",attrition_dataframe[,1])),1] = "Pre-launch and post-launch eligible"


    # Remove the first row, "Enrolled in <study timeframe>", the new conditions row, and the final row.
    if (length(which(grepl("Enrolled within study period",attrition_dataframe[,1]))) > 0) {
      attrition_dataframe = attrition_dataframe[-c(which(grepl("Enrolled within study period",attrition_dataframe[,1]))),]
    } else if (length(which(grepl("Ever enrolled",attrition_dataframe[,1]))) > 0) {
      attrition_dataframe = attrition_dataframe[-c(which(grepl("Ever enrolled",attrition_dataframe[,1]))),]
    }
    attrition_dataframe = attrition_dataframe[-c(which(grepl("Remove populations with",attrition_dataframe[,1]))),]

    # Flip the outlier removal row from % and # removed to % and # remaining.
    if (population_conditions != "Capped") {
      n_member_outlier =  attrition_dataframe[grepl("with more than",attrition_dataframe[,1]),4]
      n_nonmember_outlier = attrition_dataframe[grepl("with more than",attrition_dataframe[,1]),2]
      attrition_dataframe[which(grepl("Enrolled",attrition_dataframe[,1]))+1,2] = as.numeric(attrition_dataframe[grepl("Enrolled",attrition_dataframe[,1]),2]) - as.numeric(n_nonmember_outlier)
      attrition_dataframe[which(grepl("Enrolled",attrition_dataframe[,1]))+1,4] = as.numeric(attrition_dataframe[grepl("Enrolled",attrition_dataframe[,1]),4]) - as.numeric(n_member_outlier)
      attrition_dataframe[which(grepl("Enrolled",attrition_dataframe[,1]))+1,3] = as.numeric(attrition_dataframe[grepl("Enrolled",attrition_dataframe[,1]),2]) / as.numeric(attrition_dataframe[which(grepl("Total unique",attrition_dataframe[,1])),2])
      attrition_dataframe[which(grepl("Enrolled",attrition_dataframe[,1]))+1,5] = as.numeric(attrition_dataframe[grepl("Enrolled",attrition_dataframe[,1]),4]) / as.numeric(attrition_dataframe[which(grepl("Total unique",attrition_dataframe[,1])),4])
      for (i in 1:nrow(attrition_dataframe)) {attrition_dataframe[i,1] = str_replace(attrition_dataframe[i,1],"with more than","not with more than")}
    }

    # Add a final "Matched Row" where member sample size = non-member sample size for clarity.
    matched_row = as.data.frame(t(c("Final Matched Population",0," ",0," ")))
    matched_row[1,2] = attrition_dataframe[nrow(attrition_dataframe),4]; matched_row[1,4] = matched_row[1,2]
    colnames(matched_row) = colnames(attrition_dataframe)
    attrition_dataframe = rbind(attrition_dataframe,matched_row)

    # Extract cohort size data pieces.
    member_cohort_array[l] = as.numeric(attrition_dataframe[6,4]); nonmember_cohort_array[l] = as.numeric(attrition_dataframe[6,2])

    # For YOY, rename the "activated in" row to "Ever activated".
    if (study=="YOY") {attrition_dataframe[grepl("Activated",attrition_dataframe[,1]),1] = "Ever activated prior to launch date."}

    # Reformat some percentages and include study time periods in relevant table rows, for clarity.
    final_cohort_size[l] = attrition_dataframe[nrow(attrition_dataframe),4]
    for (i in 1:(nrow(attrition_dataframe)-1)) {
      if (attrition_dataframe[i,1]=="Activated within study period") {
        attrition_dataframe[i,1] = paste("Activated within study period (",
                                 format(ymd(as.Date(study_start_date)+(365*(l-1))),"%m/%Y"),
                                 "-",
                                 format(ymd(as.Date(study_start_date)+(365*l)-30),"%m/%Y"),
                                 ")",
                                 sep="")
      }
      if (attrition_dataframe[i,1]=="Enrolled within study period") {
        attrition_dataframe[i,1] = paste("Enrolled within study period (",
                                 format(ymd(as.Date(study_start_date)+(365*(l-1))),"%m/%Y"),
                                 "-",
                                 format(ymd(as.Date(study_start_date)+(365*l)-30),"%m/%Y"),
                                 ")",
                                 sep="")
      }
      for(j in 1:ncol(attrition_dataframe)) {
        if ((j == 3|j ==5)&!is.na(attrition_dataframe[i,j])) {attrition_dataframe[i,j] = percent(as.numeric(attrition_dataframe[i,j]),digits = 3)}
      }
    }

    # Replace PwD with PwH for hypertension ROIs.
    if (program=="Hypertension") attrition_dataframe[2,1] = gsub("PwD","PwH",attrition_dataframe[2,1])

    # Generate the flextable for the lth attrition table.
    colnames(attrition_dataframe) = c("Criteria","Count","Percent","Count ","Percent ")
    ft <- flextable::flextable(attrition_dataframe)
    ft <- flextable::add_header_row(ft,colwidths = c(1,2,2),values = c(" ","Non-Member","Member"),)
    ft <- flextable::width(ft,j=1,width=5.2)
    ft <- flextable::width(ft,j=2:5,width=1)
    ft <- flextable::height(ft,height=0.3)
    ft <- flextable::bg(ft,i=1:2,j=1:5, bg = "#66478F", part = "header")
    ft <- flextable::bg(ft,i=1,j=1:5, bg = "#55437d", part = "header")
    ft <- flextable::bg(ft,i=1:nrow(attrition_dataframe),j=1, bg = "#6A696C"); ft <- flextable::bg(ft,i=1,j=1, bg = "#6A696C", part="footer")
    ft <- flextable::bg(ft,i=1:nrow(attrition_dataframe),j=2:5, bg = "#E1DDE5"); ft <-flextable::bg(ft,i=1,j=2:5, bg = "#E1DDE5",part="footer")
    ft <- flextable::fontsize(ft, i = 1:2, size = 16, part="header"); ft <- flextable::bold(ft,i=1:2,part="header")
    ft <- flextable::fontsize(ft,size=12,part="footer"); ft <- flextable::bold(ft,part="footer")
    ft <- flextable::fontsize(ft,size=12, part="body")
    ft <- flextable::font(ft,fontname = "Century Gothic",part="all")
    ft <- flextable::align(ft,align="center",part="header")
    ft <- flextable::bold(ft,i=nrow(attrition_dataframe),j=c(2,4),bold=T)
    ft <- flextable::color(ft,color="white", part="header"); ft <- flextable::color(ft,color="white",j=1, part="all")
    ft <- flextable::border_outer(ft,border=officer::fp_border(color="black",style="solid",width=2),part="all")
    ft <- flextable::border_inner(ft,border=officer::fp_border(color="black",style="solid",width=2),part="header")
    ft <- flextable::border_inner(ft,border=officer::fp_border(color="white",style="solid",width=1),part="body")
    ft <- flextable::border_inner(ft,border=officer::fp_border(color="white",style="solid",width=1),part="footer")
    assign(paste("attrition_table_y",l,sep=""),ft)
  }

  if (post_period_length == 1) {
    return_list = list(risk_score_array, age_array, member_cohort_array,nonmember_cohort_array, final_cohort_size, attrition_table_y1)
    names(return_list) = c("risk_score_array","age_array","member_cohort_array","nonmember_cohort_array","final_cohort_size","attrition_table_y1")
  } else if (post_period_length == 2) {
    return_list = list(risk_score_array, age_array, member_cohort_array,nonmember_cohort_array, final_cohort_size, attrition_table_y1, attrition_table_y2)
    names(return_list) = c("risk_score_array","age_array","member_cohort_array","nonmember_cohort_array","final_cohort_size","attrition_table_y1","attrition_table_y2")
  } else if (post_period_length == 3) {
    return_list = list(risk_score_array, age_array, member_cohort_array,nonmember_cohort_array, final_cohort_size, attrition_table_y1, attrition_table_y2,attrition_table_y3)
    names(return_list) = c("risk_score_array","age_array","member_cohort_array","nonmember_cohort_array","final_cohort_size","attrition_table_y1","attrition_table_y2","attrition_table_y3")
  } else if (post_period_length == 4) {
    return_list = list(risk_score_array, age_array, member_cohort_array,nonmember_cohort_array, final_cohort_size, attrition_table_y1, attrition_table_y2,attrition_table_y3,attrition_table_y4)
    names(return_list) = c("risk_score_array","age_array","member_cohort_array","nonmember_cohort_array","final_cohort_size","attrition_table_y1","attrition_table_y2","attrition_table_y3","attrition_table_y4")
  }
return(return_list)
}
