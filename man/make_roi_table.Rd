\name{make_roi_table}
\alias{make_roi_table}
\title{Generates ROI Flextable and Extracts ROI-related Metrics.}
\usage{
make_roi_table(has_rx, post_period_length, program, final_cohort_size, price_of_program, PPPM_changes, supply_cost, pooled_cohort_table_2)
}
\description{
Creates and formats the ROI results flextable used in the Result Overview slide (with the graph). The flextable is formatted to proper Teladoc branding. If has_rx == T to specify inclusion of pharmaceutical data, then the resulting flextable will display three different calculations: Overall, Rx-adjusted, and condition-related Rx-adjusted. The function also returns several ROI related metrics for use in data capture, designated with nomenclature that includes "roi_array".
}
\arguments{
  \item{has_rx}{Boolean to check if the ROI analysis includes rx claims. If TRUE, all three ROIs will be calculated. If FALSE, only the medical claims ROI is calculated.}
  \item{post_period_length}{The number of years of the ROI analysis (post-period). For year-over-year, set to 1. For a two-year analysis (with 2 cohorts), set to 2, and et cetera.}
  \item{program}{The Teladoc program the analysis is looking at. Set to "Diabetes" or "Hypertension".}
  \item{final_cohort_size}{An array of sample size(s) for each cohort of the analysis. Returned by make_attrition_table().}
  \item{price_of_program}{PPPM cost of program. Returned by extract_sheet_data().}
  \item{PPPM_changes}{A table of PPPM changes for medical costs and rx costs if applicable. Returned by extract_pppm_changes().}
  \item{supply_cost}{The cost of supplies that is covered by our Diabetes program. Returned by extract_sheet_data().}
  \item{pooled_cohort_table2}{The pooled sample size for multi-year analyses. For 1-year analyses, the pooled size is simply the cohort size. Returned by make_pooled_cohort_tables().}
}
