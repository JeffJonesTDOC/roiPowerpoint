add_slide_graph_results <- function(ppt, post_period_length, format1 = centurygothic24purple, format2 = centurygothic8gray, graphs, did_table, program, supply_cost, price_of_program, roi_calculation_table, roi_table) {
  if (post_period_length==1) {
    ppt <- officer::add_slide(x=ppt, layout = "Blank Layout", master = "Livongo Slide Template 2020 Q3")
    ppt <- officer::ph_with(x=ppt,value=officer::fpar(officer::ftext(paste("Total Spending -",study,program,"ROI"),format1)),location = officer::ph_location_type(type="title")) #format1
    ppt <- officer::ph_with(x=ppt, value = graphs$ROIgraph, location = officer::ph_location(left=2.6,top=2.38,width = 4.6, height = 2.8))
    ppt <- officer::ph_with(x=ppt,value=did_table, location = officer::ph_location(left=0.85,top=0.34,width=9.46,height=1.47))
    if (program == "Diabetes") {
      ppt <- officer::ph_with(x=ppt, value = officer::fpar(officer::ftext(paste("$",price_of_program,": Livongo PPPM fee\n$",round(as.numeric(supply_cost),0),": Diabetes Supply Cost",sep=""),format2)), location = officer::ph_location(left=8.3,top=5.1,width=1.5,height=0.3)) #format2
    } else if (program == "Hypertension") {
      ppt <- officer::ph_with(x=ppt, value = officer::fpar(officer::ftext(paste("$",price_of_program,": Livongo PPPM fee",sep=""),format2)), location = officer::ph_location(left=8.3,top=5.1,width=1.5,height=0.3))
    }
    ppt <- officer::ph_with(x=ppt,value=roi_calculation_table,location = officer::ph_location(left=0.25,top=3.2,width=4.15,height=0.6))
  } else {
    ppt <- officer::add_slide(x=ppt, layout = "Blank Layout", master = "Livongo Slide Template 2020 Q3")
    ppt <- officer::ph_with(x=ppt,value=officer::fpar(officer::ftext(paste("Total Spending -",study,program,"ROI"),format1)),location = officer::ph_location_type(type="title")) #format1
    ppt <- officer::ph_with(x=ppt, value = graphs$ROIgraph, location = officer::ph_location(left=0.15,top=0.9,width = 4.6, height = 3.75))
    ppt <- officer::ph_with(x=ppt, value = roi_table$roiFlextable, location = officer::ph_location(left=4.75,top=0.9,width = 5, height = 3.75))
    ppt <- officer::ph_with(x=ppt, value = graphs$graphDataFlexTable, location = officer::ph_location(left=4.85,top=3.57,width=5,height=1.56))
    if (program == "Diabetes") {
      ppt <- officer::ph_with(x=ppt, value = officer::fpar(officer::ftext(paste("$",price_of_program,": Livongo PPPM fee\n$",round(as.numeric(supplyCost),0),": Diabetes Supply Cost",sep=""),format2)), location = officer::ph_location(left=8.3,top=5.1,width=1.5,height=0.3))
    } else if (program == "Hypertension") {
      ppt <- officer::ph_with(x=ppt, value = officer::fpar(officer::ftext(paste("$",price_of_program,": Livongo PPPM fee",sep=""),format2)), location = officer::ph_location(left=8.3,top=5.1,width=1.5,height=0.3))
    }

  }
  return(ppt)
}
