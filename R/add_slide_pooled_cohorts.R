add_slide_pooled_cohorts <- function(ppt, post_period_length,format1 = centurygothic24purple, pooled_cohort_tables) {
  if (post_period_length > 1) {
    ppt <- officer::add_slide(x=ppt, layout = "Blank Layout", master = "Livongo Slide Template 2020 Q3")
    ppt <- officer::ph_with(x=ppt,value=officer::fpar(officer::ftext("Diabetes Pooled Cohort",format1)),location = officer::ph_location_type(type="title"))
    ppt <- officer::ph_with(x=ppt, value = pooled_cohort_tables$flextable1, location = officer::ph_location(left=2.3,top=0.88,width = 7.17, height = 2.2))
    ppt <- officer::ph_with(x=ppt, value = pooled_cohort_tables$flextable2, location = officer::ph_location(left=2.41,top=3.18,width = 5.2, height = 2.1))
  } else {warning("ROI is not multi-year. Skipping creation of pooled cohort tables.")}
  return(ppt)
}
