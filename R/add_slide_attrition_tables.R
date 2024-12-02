add_slide_attrition_tables <- function(ppt, post_period_length, attrition_table_data, format1 = centurygothic24purple, branding = "Teladoc") {
  require(officer)
  if (branding == "Teladoc") {master_slide = "Teladoc Slide Template 2020 Q3"} else {master_slide = "Livongo Slide Template 2020 Q3"}
  if (post_period_length == 1) {
    ppt <- officer::add_slide(x=ppt, layout = "Blank Layout", master = master_slide)
    ppt <- officer::ph_with(x=ppt,value=officer::fpar(officer::ftext("Population Attrition Description",format1)),location = officer::ph_location(left=0.69,top=0.17,width=8.6,height=1.1)) #format1
    ppt <- officer::ph_with(x=ppt, value = attrition_table_data$attrition_table_y1, location = officer::ph_location(left=0.4,top=0.64,width = 3.5, height = 1.8))
  } else {
    if (exists("attrition_table_y1",where = attrition_table_data)) {
      ppt <- officer::add_slide(x=ppt, layout = "Blank Layout", master = master_slide)
      ppt <- officer::ph_with(x=ppt,value=officer::fpar(officer::ftext(paste("Population Attrition Description-",program," Cohort 1",sep=""),format1)),location = officer::ph_location(left=0.69,top=0.17,width=8.6,height=1.1))
      ppt <- officer::ph_with(x=ppt,value=attrition_table_data$attrition_table_y1,location=officer::ph_location(left=0.4,top=0.64,width = 3.5, height = 1.8))
    }
    if (exists("attrition_table_y2",where = attrition_table_data)) {
      ppt <- officer::add_slide(x=ppt, layout = "Blank Layout", master = master_slide)
      ppt <- officer::ph_with(x=ppt,value=officer::fpar(officer::ftext(paste("Population Attrition Description-",program," Cohort 2",sep=""),format1)),location = officer::ph_location(left=0.69,top=0.17,width=8.6,height=1.1))
      ppt <- officer::ph_with(x=ppt,value=attrition_table_data$attrition_table_y2,location=officer::ph_location(left=0.4,top=0.64,width = 3.5, height = 1.8))
    }
    if (exists("attrition_table_y3",where = attrition_table_data)) {
      ppt <- officer::add_slide(x=ppt, layout = "Blank Layout", master = master_slide)
      ppt <- officer::ph_with(x=ppt,value=officer::fpar(officer::ftext(paste("Population Attrition Description-",program," Cohort 3",sep=""),format1)),location = officer::ph_location(left=0.69,top=0.17,width=8.6,height=1.1))
      ppt <- officer::ph_with(x=ppt,value=attrition_table_data$attrition_table_y3,location=officer::ph_location(left=0.4,top=0.64,width = 3.5, height = 1.8))
    }
    if (exists("attrition_table_y4",where = attrition_table_data)) {
      ppt <- officer::add_slide(x=ppt, layout = "Blank Layout", master = master_slide)
      ppt <- officer::ph_with(x=ppt,value=officer::fpar(officer::ftext(paste("Population Attrition Description-",program," Cohort 4",sep=""),format1)),location = officer::ph_location(left=0.69,top=0.17,width=8.6,height=1.1))
      ppt <- officer::ph_with(x=ppt,value=attrition_table_data$attrition_table_y4,location=officer::ph_location(left=0.4,top=0.64,width = 3.5, height = 1.8))
    }
  }
  return(ppt)
}
