\name{read_excel}
\alias{read_excel}
\title{Read Excel Sheets Pertinent to ROI Presentation}
\usage{
read_excel(study,file_name,directory)
}
\description{
Reads all relevant sheets used to generate the ROI PowerPoint from the given ROI workbook specified by file_name. The sheets are:
\itemize{
\item ROI - returned as ROI_sheet
\item Data summary - returned as data_summary_sheet
\item Summary stats - returned as summary_stats_sheet
\item data_overview1 - returned as data_overview_sheet
\item Pharmacy costs - returned as pharmacy_costs_sheet
\item PDC - returned as PDC_sheet
}
}
\arguments{
\item{study}{the timeframe of the ROI analysis. Set to "YOY", "1YR", "2YR", or "3YR".}
\item{file_name}{The name of the ROI workbook from which the sheets are extracted. Must include .xlsm extension.}
\item{directory}The function reads the Excel workbook specified by file_name from this specified directory.}
}
\examples{

### Reads in the ROI workbook generated from running a year-over-year
### ROI analysis for Aperture Science
read_excel("YOY","ApertureScience_2024-01-01_launch_date_YOY_final.xlsm")

### Reads in the ROI workbook generated from running a 2-year
### ROI analysis for Los Pollos Hermanos.
read_excel("2YR","Los_Pollos_Hermanos_ROI_great_results.xlsm")
}
