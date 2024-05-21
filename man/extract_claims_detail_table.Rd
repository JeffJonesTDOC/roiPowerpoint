\name{extract_claims_detail_table}
\alias{extract_claims_detail_table}
\title{Extract Claims Spending Detail Table from ROI Workbook}
\usage{
extract_claims_detail_table(post_period_length, ROI_sheet,combine_ip_op, year0, HTN_population, remove_POS)
}
\description{
This function extracts the claims spending detail table from the workbook, specifically the "ROI" or "ROI_pooled" sheet. It will remove place of service visits if remove_POS is set to TRUE. It will combine inpatient and outpatient spending into a single category if combine_ip_op is set to TRUE. It returns three different objects related to this table:
\itemize{
\item{table}{A dataframe of the claims spending detail table.}
\item{flextable}{A flextable object formatted to Teladoc branding.}
\item{column_names}{An array of column names for the dataframe and flextable.}
}
}
\examples{

### Will extract the claims spending detail table from all_sheets$ROI_sheet for an ROI analysis with a 1-year post period beginning in 2023.
### It will not remove place-of-service visit details, but will combine inpatient and outpatient spending into a single row.
extract_claims_detail_table(post_period_length = 1, ROI_sheet = all_sheets$ROI_sheet, combine_ip_op = TRUE, year0 = "2022", HTN_population = "All", remove_POS=FALSE)
}
