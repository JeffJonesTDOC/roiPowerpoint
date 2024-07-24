add_slide_title <- function(ppt, client_name, program, format1 = calibri14italwhite) {
  slide1Text <- officer::fpar(officer::ftext(paste("Prepared for ",client_name,"\n",program," ROI",sep=""),format1)) #format1 = calibri14italwhite
  ppt <- officer::add_slide(ppt, layout='Title_2',master = 'Slide Templates')
  ppt <- officer::ph_with(x=ppt,value=slide1Text,location=officer::ph_location_type(type="subTitle"))
  return(ppt)
}
