add_slide_yoy_table <- function(ppt,YOY_flextable,format1 = centurygothic24purple) {
  require(officer)
  ppt <- officer::add_slide(x=ppt,layout = "Blank Layout", master = "Livongo Slide Template 2020 Q3")
  ppt <- officer::ph_with(x=ppt, value = yoy_ft, location = officer::ph_location(left=0.4,top=0.9,width=8.5,height=3))
  ppt <- officer::ph_with(x=ppt, value = officer::fpar(officer::ftext("Year Over Year Cohort Details"),format1),location= officer::ph_location_type(type="title"))
  return(ppt)
}

