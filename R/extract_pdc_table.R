extract_pdc_table <- function(program,has_rx,pdc_sheet) {
  require(officer)
  require(flextable)
  if (has_rx & program == 'Diabetes') {
    table_start_col = which(grepl("gpi4_drug_class",pdc_sheet))
    table_start_row = which(grepl("gpi4_drug_class",pdc_sheet[,table_start_col]))
    pdc_table = pdc_sheet[(table_start_row+1):(table_start_row+12),table_start_col:(table_start_col+10)]
    names(pdc_table) = c("GPI4 Drug Class","Member","N Pre","N Post","N Both","N Term","% Term","N New","% New","Net New","New/Term Ratio")
    #names(pdc_table) = pdc_sheet[table_start_row,table_start_col:(table_start_col+10)]

    for (j in 1:ncol(pdc_table)) {if (grepl("%",names(pdc_table)[j]) | grepl("Ratio",names(pdc_table[j])))  {pdc_table[,j] = percent(as.numeric(pdc_table[,j]),digits = 2)}}

    pdc_ft = flextable(pdc_table)
    pdc_ft = bold(pdc_ft,part="header")
    pdc_ft = bg(pdc_ft, bg = "#55437d",part="header")
    pdc_ft = color(pdc_ft,color="white",part="header")
    pdc_ft = bg(pdc_ft,i=seq(1,12,2),bg="#bec9d4")
    pdc_ft = bg(pdc_ft,i=seq(2,12,2),bg="#deeaf7")
    pdc_ft = width(pdc_ft,j=1,width=2,unit="in")
    pdc_ft = font(pdc_ft,fontname="Calibri",part="all")
    pdc_ft = fontsize(pdc_ft,size=9,part="all")
    pdc_ft = border_outer(pdc_ft, border = officer::fp_border(color="black",style="solid",width=2),part="all")
    pdc_ft = border_inner(pdc_ft,border=officer::fp_border(color="black",style="solid",width=1),part="all")
  } else {pdc_ft = NULL}

  return(pdc_ft)

}
