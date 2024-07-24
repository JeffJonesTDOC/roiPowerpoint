\name{extract_sheet_data}
\alias{extract_sheet_data}
\title{Extracts various data from the ROI workbook.}
\usage{
extract_sheet_data(program, study, roi_sheet, data_summary_sheet, data_overview_sheet, HTN_population)
}
\description{
Extracts pertinent data pieces required to create the PowerPoint document from several different worksheets:
\itemize{
\item ROI/ROI_pooled: PMPM cost of program (price_of_program), supply costs (supply_costs)
\item Data summary: study start date (study_start_date), year of study launch date (year0)
\item data_overview_1: minimum required member activation length (min_activation_length), client name (client_name)
}
If the program is Hypertension, then the ROI sheet needs to be modified for other functions to behave nicely. If this is the case, extract_sheet_data() will make these modifications to the dataframe specified by ROI_sheet, and return a new dataframe of the same name.
}
\arguments{
\item{program}{The Teladoc program the analysis is looking at. Set to "Diabetes" or "Hypertension".}
\item{study}{the timeframe of the ROI analysis. Set to "YOY", "1YR", "2YR", or "3YR".}
\item{roi_sheet}{"ROI" or "ROI_pooled" sheet from the relevant Excel workbook. It is returned by read_excel().}
\item{data_summary_sheet}{"Data summary" sheet from the relevant Excel workbook. It is returned by read_excel().}
\item{data_overview_sheet}{"data_overview_1" sheet from the relevant Excel workbook. It is returned by read_excel().}
\item{HTN_population}{The population used for Hypertension ROIs. Argument only relevant when program == "Hypertension", but it currently still  required. Set to "All","HTN & DM", or "HTN only".}
}

\examples{

### Extracts from all of the dataframes in the all_sheets object for a YOY analysis on a Diabetes program.
extract_sheet_data(ROI_sheet = all_sheets$ROI_sheet, program = "Diabetes", study = "YOY", data_summary_sheet = all_sheets$data_summary_sheet, data_overview_sheet = all_sheets$data_overview_sheet,HTN_population = "All")

}
