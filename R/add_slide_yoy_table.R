add_slide_yoy_table <- function(ppt,YOY_flextable,format1 = centurygothic24purple, branding = "Teladoc") {
  require(officer)
  if (branding == "Teladoc") {master_slide = "Teladoc Slide Template 2020 Q3"} else {master_slide = "Livongo Slide Template 2020 Q3"}
  ppt <- officer::add_slide(x=ppt,layout = "Blank Layout", master = master_slide)
  ppt <- officer::ph_with(x=ppt, value = YOY_flextable, location = officer::ph_location(left=0.4,top=0.9,width=8.5,height=3))
  ppt <- officer::ph_with(x=ppt, value = officer::fpar(officer::ftext("Year Over Year Cohort Details",format1)),location= officer::ph_location_type(type="title"))
  return(ppt)
}

