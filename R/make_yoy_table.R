make_yoy_table <- function(yoy_sheet,study_launch_date) {
  require(flextable)
  yoy_table = as.data.frame(matrix(nrow=nrow(yoy_sheet),ncol=3))
  names(yoy_table) = c("Cohorts","Activation Timeframe","Number of Members in Analysis from Cohort")
  for (i in 1:nrow(yoy_table)) {
    yoy_table[i,1] = paste0("Year ",i," on program")
    yoy_table[i,2] = paste0(format(study_launch_date-(365*i-1),"%m/%Y"),"-",format(study_launch_date+((365*(i-1))-31), "%m/%Y"))
    yoy_table[i,3] = yoy_sheet[i,3]
  }
  yoy_table[i+1,] = c("Total"," ",sum(yoy_table[,3]))
  yoy_ft = flextable::flextable(yoy_table)
  yoy_ft = flextable::bold(yoy_ft,j=1)
  yoy_ft = flextable::bold(yoy_ft,i=nrow(yoy_table),j=3)
  yoy_ft = flextable::font(yoy_ft, fontname = "Century Gothic",
                           part = "all")
  yoy_ft = flextable::bg(yoy_ft,bg = '#66478F',part="header")
  yoy_ft = flextable::width(yoy_ft,width = 2)
  yoy_ft = flextable::color(yoy_ft,color="white",part="header")
  yoy_ft = flextable::border_outer(yoy_ft, border = officer::fp_border(color = "black", style = "solid", width = 2), part = "all")
  yoy_ft = flextable::border_inner(yoy_ft, border = officer::fp_border(color = "black", style = "solid", width = 1), part = "all")
  return(yoy_ft)
}
