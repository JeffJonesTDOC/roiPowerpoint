# Generates the overall claims spending graph for members (Actual), members (Expected), and non-members.
# Also generates the table showcasing ROI results.

make_roi_graph_and_table <- function(roiSheet,study,program,has_rx,zero_yaxis,year0,year1,nYear) {
  if (length(which(roiSheet[1,] == "Year 0") > 1) || study =="YOY") {graphIndex = 1} else {graphIndex = 2}
  if (program == "Diabetes") {
    if (study == "1YR" | study == "YOY") {
      if (has_rx) {
        graphDataRaw = as.data.frame(t(roiSheet[graphIndex:(graphIndex+4),16:18]))
        if (all(is.na(graphDataRaw[1,]))) {graphDataRaw = as.data.frame(t(roiSheet[graphIndex:(graphIndex+4),17:19]))}
      } else {
        graphDataRaw = as.data.frame(t(roiSheet[graphIndex:(graphIndex+4),1:3]))
        if (all(is.na(graphDataRaw[1,]))) {graphDataRaw = as.data.frame(t(roiSheet[graphIndex:(graphIndex+4),2:4]))}

      }
    } else if (study == "2YR") {
      if (has_rx) {
        graphDataRaw = as.data.frame(t(roiSheet[graphIndex:(graphIndex+4),(which(colnames(roiSheet)=="Total")+1):((which(colnames(roiSheet)=="Total")+1)+3)]))
      } else {
        graphDataRaw = as.data.frame(t(roiSheet[graphIndex:(graphIndex+4),which(colnames(roiSheet)=="Combined.Result"):(which(colnames(roiSheet)=="Combined.Result")+3)]))
      }
    } else if (study == "3YR") {
      graphDataRaw = as.data.frame(t(roiSheet[(which(roiSheet[,1]=="pooled 3 year")+1):(which(roiSheet[,1]=="pooled 3 year")+4),8:(8+nYear+1)]))
    } else if (study == "4YR") {
      graphDataRaw = as.data.frame(t(roiSheet[(which(roiSheet[,1]=="pooled 4 year")+1):(which(roiSheet[,1]=="pooled 4 year")+4),8:(8+nYear+1)]))
    }
  }
  if (program == "Hypertension") {
    if (study == "1YR" | study == "YOY") {
      graphDataRaw = as.data.frame(t(roiSheet[graphIndex:(graphIndex+4),25:28]))
    } else if (study == "2YR") {
      if (has_rx) {
        graphDataRaw = as.data.frame(t(roiSheet[graphIndex:(graphIndex+4),(which(colnames(roiSheet)=="Total")+1):((which(colnames(roiSheet)=="Total")+1)+3)]))
      } else {
        graphDataRaw = as.data.frame(t(roiSheet[graphIndex:(graphIndex+4),which(colnames(roiSheet)=="Combined.Result"):(which(colnames(roiSheet)=="Combined.Result")+3)]))
      }
    } else if (study == "3YR") {
      graphDataRaw = as.data.frame(t(roiSheet[(which(roiSheet[,1]=="pooled 3 year")+1):(which(roiSheet[,1]=="pooled 3 year")+4),8:(8+nYear+1)]))
    } else if (study == "4YR") {
      graphDataRaw = as.data.frame(t(roiSheet[(which(roiSheet[,1]=="pooled 4 year")+1):(which(roiSheet[,1]=="pooled 3 year")+4),8:(8+nYear+1)]))
    }
  }
  if (all(is.na(graphDataRaw[,1]))) {graphDataRaw = graphDataRaw[,-c(1)]}
  graphDataNames = tidyr::crossing(c("Non-Livongo","Livongo, expected","Livongo, actual"),na.omit(graphDataRaw[,1]))
  graphData = array();
  for(i in 1:nrow(graphDataNames)){
    graphData[i]=as.numeric(graphDataRaw[which(graphDataRaw[,1]==as.character(graphDataNames[i,2])),which(graphDataRaw[1,]==as.character(graphDataNames[i,1]))])
  }
  graphDataFull = cbind(graphDataNames,graphData);
  colnames(graphDataFull) = c("Group","Year","Data");
  #Come up with a better solution for this below code. For now, just make it work.
  {
    for (i in 1:nrow(graphDataFull)) {
      graphDataFull$Year[i] = gsub("Year 0",as.numeric(year0),graphDataFull$Year[i])
      graphDataFull$Year[i] = gsub("Year 1",as.numeric(year0)+1,graphDataFull$Year[i])
      graphDataFull$Year[i] = gsub("Year 2",as.numeric(year0)+2,graphDataFull$Year[i])
      graphDataFull$Year[i] = gsub("Year3",as.numeric(year0)+3,graphDataFull$Year[i])
      graphDataFull$Year[i] = gsub("Year4",as.numeric(year0)+4,graphDataFull$Year[i])
    }
    graphDataFull = transform(graphDataFull,Year = as.numeric(Year))
    if (study == "YOY")
    {
      graphDataFull$Labels = rep(c(year0,year1),3)
    } else {graphDataFull$Labels = rep(paste("Year",seq(0,nYear)),3)}
  }
  #Come up with a better solution for the above code. For now, just make it work.

  if (zero_yaxis) {axisStart = 0} else {axisStart = 0.75*min(graphDataFull$Data)}
  if (has_rx) {graphTitle = "PMPM Medical & Pharmaceutical Spending"} else {graphTitle = "PMPM Medical Spending"}
  # This graph uses normal x-axis year tick marks.
  ROIgraph <- ggplot2::ggplot(graphDataFull,aes(x=Year,Data,color=Group)) +
    #theme_minimal() +
    scale_linetype_manual(values=c("dashed","dotted","solid")) +
    geom_line(aes(linetype=Group)) +
    ggtitle(labs(title = graphTitle)) +
    scale_x_continuous(breaks = c(seq(as.numeric(year0),as.numeric(year0)+nYear)),labels = c(sort(unique(graphDataFull$Labels)))) +
    scale_color_manual(values=c("purple","purple","blue")) +
    ylim(axisStart,1.1*max(graphDataFull$Data)) +
    theme(legend.position=c(0.5,0.1),
          legend.direction="horizontal",
          legend.title=element_blank(),
          legend.key.width = unit(0.5, 'cm'))

  graphDataRaw[1,1] = "Group"
  graphDataModified = t(graphDataRaw[,1:4])
  for (i in 2:ncol(graphDataModified)) {
    graphDataModified[2:4,i] = paste("$",round(as.numeric(graphDataModified[2:4,i],0)),sep="")
  }
  if (nYear == 1) {graphDataModified[1,2] = gsub("Year 0",year0,graphDataModified[1,2]); graphDataModified[1,3] = gsub("Year 1",year1,graphDataModified[1,3]); }
  graphDataFlexTable <- flextable::flextable(as.data.frame(graphDataModified))
  graphDataFlexTable <- flextable::delete_part(graphDataFlexTable,part="header")
  graphDataFlexTable <- flextable::bold(graphDataFlexTable,j=1,part="body")
  graphDataFlexTable <- flextable::bg(graphDataFlexTable,i=1,bg="#66478F")
  graphDataFlexTable <- flextable::color(graphDataFlexTable,i=1,color="white",part="body")
  graphDataFlexTable <- flextable::width(graphDataFlexTable,j=1,width=1.5)
  graphDataFlexTable <- flextable::font(graphDataFlexTable,fontname="Century Gothic",part="all")
  graphDataFlexTable <- flextable::align(graphDataFlexTable,align=c("center"))
  graphDataFlexTable <- flextable::border_outer(graphDataFlexTable,border=officer::fp_border(color="black",style="solid",width=2),part="all")
  graphDataFlexTable <- flextable::border_inner(graphDataFlexTable,border=officer::fp_border(color="black",style="solid",width=1),part="body")

  returnList = list(ROIgraph,graphDataFlexTable)
  names(returnList) = c("ROIgraph","graphDataFlexTable")
  return(returnList)
}
