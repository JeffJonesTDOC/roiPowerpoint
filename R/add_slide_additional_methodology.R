add_slide_additional_methodology <- function(ppt, post_period_length, study,branding = "Teladoc") {
  require(officer)
  if (branding == "Teladoc") {master_slide = "Teladoc Slide Template 2020 Q3"} else {master_slide = "Livongo Slide Template 2020 Q3"}
  if (post_period_length > 1) {ppt <- officer::add_slide(ppt, layout="Multiyear Baseline Slide",master=master_slide)}
  if (study == 'YOY') {ppt <- officer::add_slide(ppt, layout="Multiyear YOY Slide",master=master_slide)}
  return(ppt)
}


