add_slide_attrition_tables <- function(ppt, post_period_length, attrition_table_data, format1 = centurygothic24purple) {
  if (post_period_length == 1) {
    ppt <- officer::add_slide(x=ppt, layout = "Blank Layout", master = "Livongo Slide Template 2020 Q3")
    ppt <- officer::ph_with(x=ppt,value=officer::fpar(officer::ftext("Population Attrition Description",format1)),location = officer::ph_location(left=0.69,top=0.17,width=8.6,height=1.1)) #format1
    ppt <- officer::ph_with(x=ppt, value = attrition_table_data$attrTableY1, location = officer::ph_location(left=0.4,top=0.64,width = 3.5, height = 1.8))
  } else {
    if (exists("attrTableY1",where = attrition_table_data)) {
      ppt <- officer::add_slide(x=ppt, layout = "Blank Layout", master = "Livongo Slide Template 2020 Q3")
      ppt <- officer::ph_with(x=ppt,value=officer::fpar(officer::ftext(paste("Population Attrition Description-",program," Cohort 1",sep=""),format1)),location = officer::ph_location(left=0.69,top=0.17,width=8.6,height=1.1))
      ppt <- officer::ph_with(x=ppt,value=attrition_table_data$attrTableY1,location=officer::ph_location(left=0.4,top=0.64,width = 3.5, height = 1.8))
    }
    if (exists("attrTableY2",where = attrition_table_data)) {
      ppt <- officer::add_slide(x=ppt, layout = "Blank Layout", master = "Livongo Slide Template 2020 Q3")
      ppt <- officer::ph_with(x=ppt,value=officer::fpar(officer::ftext(paste("Population Attrition Description-",program," Cohort 2",sep=""),format1)),location = officer::ph_location(left=0.69,top=0.17,width=8.6,height=1.1))
      ppt <- officer::ph_with(x=ppt,value=attrition_table_data$attrTableY2,location=officer::ph_location(left=0.4,top=0.64,width = 3.5, height = 1.8))
    }
    if (exists("attrTableY3",where = attrition_table_data)) {
      ppt <- officer::add_slide(x=ppt, layout = "Blank Layout", master = "Livongo Slide Template 2020 Q3")
      ppt <- officer::ph_with(x=ppt,value=officer::fpar(officer::ftext(paste("Population Attrition Description-",program," Cohort 3",sep=""),format1)),location = officer::ph_location(left=0.69,top=0.17,width=8.6,height=1.1))
      ppt <- officer::ph_with(x=ppt,value=attrition_table_data$attrTableY3,location=officer::ph_location(left=0.4,top=0.64,width = 3.5, height = 1.8))
    }
    if (exists("attrTableY4",where = attrition_table_data)) {
      ppt <- officer::add_slide(x=ppt, layout = "Blank Layout", master = "Livongo Slide Template 2020 Q3")
      ppt <- officer::ph_with(x=ppt,value=officer::fpar(officer::ftext(paste("Population Attrition Description-",program," Cohort 4",sep=""),format1)),location = officer::ph_location(left=0.69,top=0.17,width=8.6,height=1.1))
      ppt <- officer::ph_with(x=ppt,value=attrition_table_data$attrTableY4,location=officer::ph_location(left=0.4,top=0.64,width = 3.5, height = 1.8))
    }
  }
  return(ppt)
}
