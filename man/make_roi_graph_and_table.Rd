\name{make_roi_graph_and_table}
\alias{make_roi_graph_and_table}
\title{Generates ROI Graph and Table for ROI Results Slide}
\usage{
make_roi_graph_and_table(roi_sheet,study,program,has_rx,zero_yaxis,year0,year1,post_period_length)
}
\description{Uses ROI results to create the results graph and associated table.}
\arguments{
\itemize{
\item{roi_sheet}{"ROI" or "ROI_pooled" sheet from the relevant Excel workbook. It is returned by read_excel().}
\item{study}{the timeframe of the ROI analysis. Set to "YOY", "1YR", "2YR", or "3YR".}
\item{program}{The Teladoc program the analysis is looking at. Set to "Diabetes" or "Hypertension".}
\item{has_rx}{Boolean to check if the ROI analysis includes rx claims. If FALSE, the functions returns a NULL object.}
\item{zero_yaxis}{AAA}
\item{year0}{Year 0, or pre-period, of the analysis. Can be specified as an integer or character. It is returned by extract_sheet_data().}
\item{year1}{Year 1, or first post-period, of the analysis. Can be specified as an integer or character. It is returned by extract_sheet_data().}
}
