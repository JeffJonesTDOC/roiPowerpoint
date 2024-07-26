add_slide_exec_summary <- function(ppt, has_rx, post_period_length, pmpm_phar_tables, program, year0, roi_table) {
  # Create the slide "skeleton".
  if (!has_rx) {ppt <- officer::add_slide(ppt, layout = "Executive Summary No Phar", master = "Livongo Slide Template 2020 Q3")}
  if (has_rx) {ppt <- officer::add_slide(ppt, layout = "Executive Summary Phar", master = "Livongo Slide Template 2020 Q3")}

  for (i in 1:post_period_length) {

    # Define the text within the ROI box
    text_box_changes = array(dim=post_period_length)
    if (has_rx) {
      text_box_changes[i] = pmpm_phar_tables$PMPM_changes[1,(2*i)-1] + pmpm_phar_tables$PMPM_changes[2,(2*i)-1]
    } else {
      text_box_changes[i] = pmpm_phar_tables$PMPM_changes[1,(2*i)-1]
    }
    if (program == "Hypertension") {
      if (post_period_length == 1) slide2DMft1Text <- paste0("HTN ",year1,":\n$",text_box_changes[i]," PPPM\n",roi_table$executiveSummaryRoiArray[i],"x ROI")
      if (post_period_length > 1) slide2DMft1Text <- paste0("HTN Year ",i,":\n$",text_box_changes[i]," PPPM\n",roi_table$executiveSummaryRoiArray[i],"x ROI")

    } else if (program == "Diabetes") {
      if (post_period_length == 1) slide2DMft1Text <- paste0("DM ",year1,":\n$",text_box_changes[i]," PPPM\n",roi_table$executiveSummaryRoiArray[i],"x ROI")
      if (post_period_length > 1) slide2DMft1Text <- paste0("DM Year ",i,":\n$",text_box_changes[i]," PPPM\n",roi_table$executiveSummaryRoiArray[i],"x ROI")
    }

    # Create the ROI box flextable
    slide2ft1 <- flextable::flextable(as.data.frame(slide2DMft1Text));
    slide2ft1 <- flextable::bg(slide2ft1,bg ="#55437d"); slide2ft1 <- flextable::align(slide2ft1,align="center")
    slide2ft1 <- flextable::color(slide2ft1,color="white"); slide2ft1 <- flextable::fontsize(slide2ft1,size=14);
    slide2ft1 <- flextable::delete_part(slide2ft1,part="header"); slide2ft1 <- flextable::width(slide2ft1, width=2)
    slide2ft1 <- flextable::font(slide2ft1,fontname="Century Gothic",part="all")

    # Add the flextable object to the executive summary slide, with a linear equation defining the coordinates.
    if (post_period_length == 1) {ppt <- officer::ph_with(x=ppt,slide2ft1,location=officer::ph_location(left=4,top=4.1,width = 2, height = 0.78))}
    if (post_period_length == 2) {ppt <- officer::ph_with(x=ppt,slide2ft1,location=officer::ph_location(left=(3.9*i)-2,top=4.1,width = 2, height = 0.78))}
    if (post_period_length == 3) {ppt <- officer::ph_with(x=ppt,slide2ft1,location=officer::ph_location(left=(2.81*i)-1.73,top=4.1,width = 2, height = 0.78))}
    if (post_period_length == 4) {ppt <- officer::ph_with(x=ppt,slide2ft1,location=officer::ph_location(left=(2.53*i)-2.36,top=4.1,width = 2, height = 0.78))}
  }
  return(ppt)
}
