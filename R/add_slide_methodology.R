add_slide_methodology <- function(ppt, post_period_length, format1 = centurygothic12, format2 = centurygothic16bold, population_conditions, data_overview_sheet, study_time_period_launch_date, year0, branding = "Teladoc") {
  require(officer)
  if (branding == "Teladoc") {master_slide = "Teladoc Slide Template 2020 Q3"} else {master_slide = "Livongo Slide Template 2020 Q3"}
  # Approach Text
  {
    approachTextYearArray=array()
    for(i in 1:post_period_length) {
      if (i != post_period_length) {approachTextYearArray= append(approachTextYearArray,paste("Year ",i,", ",sep=""))}
      if (i == post_period_length) {approachTextYearArray=na.omit(append(approachTextYearArray,paste("Year ",i,sep="")))}
    }
    approachText <- officer::fpar(officer::ftext(paste0("Difference-in-difference (DID) comparison of total allowed amount of medical spending (PMPM) one year prior to index date (Year 0) compared to year(s) following index date (",paste(approachTextYearArray,collapse=""),") for members vs. non-members.")
                                                 ,centurygothic12)) # format1
    methodology1 <- officer::block_list(officer::fpar(officer::ftext("Approach",centurygothic16bold)),approachText) # format2
  }

  # Inclusion Criteria Text
  {
    if (population_conditions == "Capped") {
      pop_criteria = "Capped: Annual medical costs exceeding $100K or 95th percentile"
    } else if (population_conditions == "100K Annual") {
      pop_criteria = "Annual medical costs not exceeding $100,000"
    } else if (population_conditions == "50K Monthly") {
      pop_criteria = "Monthly medical costs not exceeding $50,000"
    } else {pop_criteria = population_conditions}
    criteriaArray = c("Eligible for health benefits for entire study period",
                      "Age < 65",
                      paste("Members activated in Livongo >",data_overview_sheet[which(data_overview_sheet[,1] == "joined_months"),2],"months"),
                      "Cancer or newly developed severe condition (e.g., stroke, HF)",
                      pop_criteria)
    CCS_flag = data_overview_sheet[which(data_overview_sheet[,1] == "CCS_method"),2]
    if (CCS_flag == "Matching, cancer excluded") {
      inclusionCriteria <- officer::block_list(officer::fpar(officer::ftext("Inclusion Criteria (Members & Non-Members):",centurygothic16bold)), #format2
                                               officer::fpar(officer::ftext(criteriaArray[1],centurygothic12)), #format1
                                               officer::fpar(officer::ftext(criteriaArray[2],centurygothic12)), #format1
                                               officer::fpar(officer::ftext(criteriaArray[3],centurygothic12)), #format1
                                               officer::fpar(officer::ftext(criteriaArray[4],centurygothic12)), #format1
                                               officer::fpar(officer::ftext(criteriaArray[5],centurygothic12))) #format1
    } else {
      inclusionCriteria <- officer::block_list(officer::fpar(officer::ftext("Inclusion Criteria (Members & Non-Members):",centurygothic16bold)), #format2
                                               officer::fpar(officer::ftext(criteriaArray[1],centurygothic12)), #format1
                                               officer::fpar(officer::ftext(criteriaArray[2],centurygothic12)), #format1
                                               officer::fpar(officer::ftext(criteriaArray[3],centurygothic12)), #format1
                                               officer::fpar(officer::ftext(criteriaArray[5],centurygothic12))) #format1
    }

  }

  # Matching Text
  {
    matchRatio = data_overview_sheet$data_overview[which(data_overview_sheet$X1=="match_ratio")]
    matchCriteria = str_trim(strsplit(data_overview_sheet$data_overview[which(data_overview_sheet$X1=="matching_var_list")],",")[[1]])
    matchCriteria = str_replace_all(matchCriteria,"female","gender")
    matchCriteria = str_replace_all(matchCriteria,"male","gender")
    matchCriteria = str_replace_all(matchCriteria,"risk_score","Charlson Comorbidity Score")
    matchCriteria = str_replace_all(matchCriteria,"pre_total_costs","pre-period total medical costs")
    matchCriteria = str_replace_all(matchCriteria,"pre_diabetes_total_costs","pre-period diabetic costs")
    matchCriteria = str_replace_all(matchCriteria,"pre_diabetes_cost","pre-period diabetic costs")
    matchCriteria = str_replace_all(matchCriteria,"pre_pharmacy_cost","pre-period pharmaceutical costs")
    matchCriteria = str_replace_all(matchCriteria,"pre_hypertension_total_costs","pre-period hypertension costs")
    matchCriteriaString <-"";
    for(i in 1:length(matchCriteria)) {
      if (i==(length(matchCriteria)-1))  {matchCriteriaString=paste(matchCriteriaString,matchCriteria[i]," and ",sep="")}
      else if (i==length(matchCriteria)) {matchCriteriaString=paste(matchCriteriaString,matchCriteria[i],".",sep="")}
      else {matchCriteriaString=paste(matchCriteriaString,matchCriteria[i],", ",sep="")}
    }
    methodology3 <- officer::block_list(officer::fpar(officer::ftext("Matching",centurygothic16bold)), #format2
                                        officer::fpar(officer::ftext(paste("Members propensity score matched ",matchRatio,":1 with non-members using ",matchCriteriaString,sep=""),centurygothic12))) #format1
  }

  # Study Time Period Text
  {
    studyTimePeriodArray = array(dim=post_period_length+2)
    studyTimePeriodArray[1] = paste("Study Index Date:",str_replace_all(study_time_period_launch_date,"-","/"))
    if (as.numeric(year0) %% 4 != 0) {
      studyTimePeriodArray[2] = paste("\nPre-Period, Year 0:",str_replace_all(study_time_period_launch_date-365,"-","/"),"-",str_replace_all(as.Date(study_time_period_launch_date)-1,"-","/"))
    } else {
      studyTimePeriodArray[2] = paste("\nPre-Period, Year 0:",str_replace_all(study_time_period_launch_date-366,"-","/"),"-",str_replace_all(as.Date(study_time_period_launch_date)-1,"-","/"))
    }
    for (i in 3:(post_period_length+2)) {
      studyTimePeriodArray[i] = paste("\nPost-Period, Year ",(i-2),": ",as.numeric(year0)+(i-2),"/01/01 - ",as.numeric(year0)+(i-2),"/12/31",sep="")
    }

    methodology4 <- officer::block_list(officer::fpar(officer::ftext("Study Time Periods",centurygothic16bold)),  #format2
                                        officer::fpar(officer::ftext(studyTimePeriodArray,centurygothic12)))  #format1
  }

  # Slide Creation
  {
    ppt <- officer::add_slide(ppt,layout="Methodology",master=master_slide)
    ppt <- officer::ph_with(x=ppt,value=methodology1,location=officer::ph_location(left=2.43,top=0.8,width=5.85,height=1))
    ppt <- officer::ph_with(x=ppt,value=inclusionCriteria,location=officer::ph_location(left=2.43,top=1.96,width=5.85,height=2), level_list = c(1L, 2L, 3L))
    ppt <- officer::ph_with(x=ppt,value=methodology3,location=officer::ph_location(left=2.43,top=3.3,width=5.85,height=0.66))
    ppt <- officer::ph_with(x=ppt,value=methodology4,location=officer::ph_location(left=2.43,top=4.3,width=5.85,height=0.66))
  }

  return(ppt)
}
