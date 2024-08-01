add_slide_claims_detail_table <- function(ppt, program, claims_detail, format1 = centurygothic8gray) {
  ppt <- officer::add_slide(x=ppt, layout = "Blank Layout", master = "Livongo Slide Template 2020 Q3")
  ppt <- officer::ph_with(x=ppt,value=officer::fpar(officer::ftext(paste("Pooled Spending Summary -",program))),location = officer::ph_location_type(type="title"))
  ppt <- officer::ph_with(x=ppt, value = claims_detail$flex_table, location = officer::ph_location(left=0.1,top=1,width = 9.5, height = 3.5))
  ppt <- officer::ph_with(x=ppt, value = officer::fpar(officer::ftext("Disease-related costs are identified by ICD-10 codes and one claim can have multiple ICD-10 codes associated to it, therefore, they are not mutually exclusive groups and will not sum to the Total Medical Costs (PMPM).\nPlace of Service costs are mutually exclusive groups and will have a sum close to the Total Medical Costs (PMPM). Not all categories are represented in the table.",format1)) , location = officer::ph_location(left=0.3,top=4.75,width=9,height=0.5))
  return(ppt)
}
