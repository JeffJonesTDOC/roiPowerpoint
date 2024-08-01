\name{create_roi_calculation_table}
\alias{create_roi_calculation_table}
\title{Creates the ROI calculation table}
\usage{
create_roi_calculation_table(roi_table,has_rx, program)
}
\description{
Creates the flextable object containing ROI calculation(s) for Rx-adjusted, diabetic Rx-adjusted, and overall results.}
\arguments{
  \item{roi_table}{A list object containing various information related to ROI results. It is returned by make_roi_table().}
  \item{has_rx}{Boolean to check if the ROI analysis includes rx claims. If FALSE, the functions returns a NULL object.}
  \item{program}{The Teladoc program the analysis is looking at. Set to "Diabetes" or "Hypertension".}
}

\examples{
get_pmpm_changes(has_rx, post_period_length, ROI_sheet = all_sheets$ROI_sheet,claims_detail_table = claims_detail$table, program, pharmacy_costs_sheet = all_sheets$pharmacy_costs_sheet,claims_detail_table_column_names = claims_detail$column_names)
}
