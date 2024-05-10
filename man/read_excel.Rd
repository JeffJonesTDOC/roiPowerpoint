\name{read_excel}
\alias{read_excel}
\title{Read Pertinent Excel Sheets}
\usage{
read_excel(study,file_name)
}
\description{
Reads all relevant sheets used to generate the ROI PowerPoint from the given ROI workbook specified by file_name. The sheets are:
\begin{itemize}
\item ROI
\item Data summary
\item Summary stats
\item data_overview1
\item Pharmacy costs
\item PDC
\end{itemize}
}
\arguments{
\item{study}{the timeframe of the ROI analysis. Set to "YOY", "1YR", "2YR", or "3YR".}
\item{file_name}{The name of the ROI workbook from which the sheets are extracted.}
}
\examples{
### Reads in the ROI workbook generated from running a year-over-year ROI analysis on Aperture Science
read_excel(YOY,"ApertureScience_2024-01-01_launch_date_YOY_final.xlsm")

### Reads in the ROI workbook generated from running a 2-year ROI analysis on Los Pollos Hermanos.
read_excel(2YR,"Los_Pollos_Hermanos_ROI_great_results.xlsm")
}
