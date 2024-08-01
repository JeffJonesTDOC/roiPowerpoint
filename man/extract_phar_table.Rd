\name{extract_phar_table}
\alias{extract_phar_table}
\title{Extracts the pharmacy claims spending table from the workbook.}
\usage{
extract_phar_table(has_rx, post_period_length, ROI_sheet, pharmacy_costs_sheet, claims_detail_table_column_names, claims_detail_table)
}
\description{
Extracts the pharmacy claims spending table from the sheet specified by ROI_sheet. If the study is multi-year, the function will account for the pooled table dimensions.
}
\arguments{
\item{has_rx}{Boolean to check if the ROI analysis includes rx claims. If FALSE, the functions returns a NULL object.}
\item{post_period_length}{The number of years of the ROI analysis (post-period). For year-over-year, set to 1. For a two-year analysis (with 2 cohorts), set to 2, and et cetera.}
\item{ROI_sheet}{"ROI" or "ROI_pooled" sheet from the relevant Excel workbook. It is returned by read_excel().}
\item{claims_detail_table_column_names}{The column names of the spend summary table object. This function sets the pharmacy table column names to be the same. It is returned by extract_claims_detail.}
}

\examples{

extract_phar_table(has_rx, post_period_length = 1, ROI_sheet = all_sheets$ROI_sheet, pharmacy_costs_sheet = all_sheets$pharmacy_costs_sheet, claims_detail_table_column_names = claims_detail$column_names)
}
