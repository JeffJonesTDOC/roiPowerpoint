create_roi_calculation_table <- function(roi_table,has_rx, program) {
  if (!has_rx) {
    textBoxFT = flextable::flextable(as.data.frame((c("Total claims ROI :",substr(roi_table$final_roi_table[nrow(roi_table$final_roi_table),2],start=1,stop=nchar(roi_table$final_roi_table[nrow(roi_table$final_roi_table),2])-3),paste0(substr(roi_table$final_roi_table[nrow(roi_table$final_roi_table),2],start=nchar(roi_table$final_roi_table[nrow(roi_table$final_roi_table),2])-3,stop=500),"x")))))
    textBoxFT = flextable::delete_part(textBoxFT,part="header")
    textBoxFT = flextable::width(textBoxFT,width=3,unit="in")
    textBoxFT = flextable::fontsize(textBoxFT,i=3,size=24)
    textBoxFT = flextable::fontsize(textBoxFT,i=c(1,2),size=13.5)
    textBoxFT = flextable::bold(textBoxFT,i=3)
    textBoxFT = flextable::font(textBoxFT,fontname="Century Gothic",part="all")
    textBoxFT = flextable::color(textBoxFT,i=3,color="#55437d")
    textBoxFT = flextable::border_remove(textBoxFT)
  }
  if (has_rx) {
    textBox = as.data.frame(matrix(nrow=3,ncol=3)); colnames(textBox) = c("Results","Calculation"," ")
    textBox[3,1] = "Overall ROI"
    if(program=="Diabetes") {textBox[2,1] = "Diabetes Rx-adjusted ROI"}
    if(program=="Hypertension") {textBox[2,1] = "Hypertension Rx-adjusted ROI"}
    textBox[1,1] = "Rx-adjusted ROI"
    textBox[3,2] = substr(roi_table$final_roi_table[5,2],1,nchar(roi_table$final_roi_table[5,2])-3)
    textBox[2,2] = substr(roi_table$final_roi_table[6,2],1,nchar(roi_table$final_roi_table[6,2])-3)
    textBox[1,2] = substr(roi_table$final_roi_table[4,2],1,nchar(roi_table$final_roi_table[4,2])-3)
    textBox[3,3] = substr(roi_table$final_roi_table[5,2],nchar(roi_table$final_roi_table[5,2])-3,nchar(roi_table$final_roi_table[5,2]))
    textBox[2,3] = substr(roi_table$final_roi_table[6,2],nchar(roi_table$final_roi_table[6,2])-3,nchar(roi_table$final_roi_table[6,2]))
    textBox[1,3] = substr(roi_table$final_roi_table[4,2],nchar(roi_table$final_roi_table[4,2])-3,nchar(roi_table$final_roi_table[4,2]))

    textBoxFT = flextable::flextable(textBox)
    textBoxFT = flextable::width(textBoxFT,width=1.5,unit="in",j=1)
    textBoxFT = flextable::width(textBoxFT,width=1.5,unit="in",j=2)
    textBoxFT = flextable::bold(textBoxFT,j=3)
    textBoxFT = flextable::color(textBoxFT,j=3,color="#55437d")
    textBoxFT = flextable::font(textBoxFT,fontname="Century Gothic",part="all")
    textBoxFT = flextable::merge_v(textBoxFT,j=2:3)
    textBoxFT = flextable::align(textBoxFT,align="center",part="all")
    textBoxFT = flextable::color(textBoxFT,color="white",part="header")
    textBoxFT = flextable::bg(textBoxFT,bg="#66478F",part="header")
    textBoxFT = flextable::border_outer(textBoxFT,border=officer::fp_border(color="black",style="solid",width=2),part="all")
    textBoxFT
  }
  return(textBoxFT)
}
