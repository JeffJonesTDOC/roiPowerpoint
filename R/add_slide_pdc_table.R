add_slide_pdc_table <- function(ppt, pdc_flextable,format1 = centurygothic24purple) {
  require(officer)
  ppt <- officer::add_slide(x = ppt, layout = "Blank Layout", master = "Livongo Slide Template 2020 Q3")
  ppt <- officer::ph_with(x = ppt, value = pdc_flextable, location = ph_location(left=0.38,top=1.3,width = 9.5,height = 2.5))
  ppt <- officer::ph_with(x = ppt, value = officer::fpar(officer::ftext("Medication Usage Details",format1)), location = ph_location(left=0.69,top=0.17,width=8.6,height = 1.1))
  return(ppt)
}

