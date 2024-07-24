add_slide_additional_methodology <- function(ppt, post_period_length, study) {
  if (post_period_length > 1) {ppt <- officer::add_slide(ppt, layout="Multiyear Baseline Slide",master="Livongo Slide Template 2020 Q3")}
  if (study == 'YOY') {ppt <- officer::add_slide(ppt, layout="Multiyear YOY Slide",master="Livongo Slide Template 2020 Q3")}
  return(ppt)
}
