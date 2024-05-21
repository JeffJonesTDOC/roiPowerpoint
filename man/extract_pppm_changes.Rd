\name{extract_pppm_changes}
\alias{extract_pppm_changes}
\title{Extract PPPM changes from the ROI workbook}
\usage{
get_pppm_changes(has_rx, post_period_length, ROI_sheet, spend_summary_table, program, pharmacy_costs_sheet, spend_summary_table_column_names)
}
\description{
Extracts and calculates the difference-in-difference PPPM changes for all years of the ROI analysis. If pharmaceutical data is present (specified with has_rx == T), then this function will also operate on pharmaceutical and condition-related pharmaceutical claim costs.
}
\arguments{
  \item{has_rx}{Boolean to check if the ROI analysis includes rx claims. If FALSE, the functions returns a NULL object.}
  \item{post_period_length}{The number of years of the ROI analysis (post-period). For year-over-year, set to 1. For a two-year analysis (with 2 cohorts), set to 2, and et cetera.}
  \item{ROI_sheet}{"ROI" or "ROI_pooled" sheet from the relevant Excel workbook. It is returned by read_excel().}
  \item{spend_summary_table}{The spending table of various condition-related and POS claims. Use extract_spend_summary_table() to generate it.}
  \item{program}{The Teladoc program the analysis is looking at. Set to "Diabetes" or "Hypertension".}
  \item{pharmacy_costs_sheet}{"Pharmacy costs"" sheet from the relevant Excel workbook. It is returned by read_excel().}

}

\examples{
get_pmpm_changes(has_rx, post_period_length, ROI_sheet = all_sheets$ROI_sheet,spend_summary_table = spend_summary$table, program, pharmacy_costs_sheet = all_sheets$pharmacy_costs_sheet,spend_summary_table_column_names = spend_summary$column_names)
}
