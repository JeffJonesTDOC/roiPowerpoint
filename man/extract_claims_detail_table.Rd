\name{extract_claims_detail_table}
\alias{extract_claims_detail_table}
\title{Extract Claims Spending Detail Table from ROI Workbook}
\usage{
extract_claims_detail_table(post_period_length, roi_sheet, year0, remove_POS = FALSE, HTN_population = "All", combine_ip_op = FALSE)
}
\description{
This function extracts the claims spending detail table from the workbook, specifically the "ROI" or "ROI_pooled" sheet. It will remove place of service visits if remove_POS is set to TRUE. It will combine inpatient and outpatient spending into a single category if combine_ip_op is set to TRUE. It returns three different objects related to this table:
\itemize{
\item{table}{A dataframe of the claims spending detail table.}
\item{flextable}{A flextable object formatted to Teladoc branding.}
\item{column_names}{An array of column names for the dataframe and flextable.}
}
}
\arguments{
\item{post_period_length}{The number of years of the ROI analysis (post-period). For year-over-year, set to 1. For a two-year analysis (with 2 cohorts), set to 2, and et cetera.}
\item{roi_sheet}{"ROI" or "ROI_pooled" sheet from the relevant Excel workbook. It is returned by read_excel().}
\item{year0}{Year 0, or pre-period, of the analysis. Can be specified as an integer or character. It is returned by extract_sheet_data().}
\item{remove_POS} {Indicates whether or not to remove place-of-service spending from the claims detail table.}
\item{HTN_population}{Indicates which population to use for hypertension ROIs. One of c("All","HTN & DM","HTN only").}
\item{combine_ip_op}{Indicates whether or not to combine inpatient and outpatient claims into a single category in the claims detail table.}
}

\examples{

### Will extract the claims spending detail table from all_sheets$ROI_sheet for an ROI analysis with a 1-year post period beginning in 2023.
### It will not remove place-of-service visit details, but will combine inpatient and outpatient spending into a single row.
extract_claims_detail_table(post_period_length = 1, ROI_sheet = all_sheets$ROI_sheet, combine_ip_op = TRUE, year0 = "2022", HTN_population = "All", remove_POS=FALSE)
}
