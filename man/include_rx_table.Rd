\name{include_rx_table}
\alias{include_rx_table}
\title{Attaches Pharmacy Detail to Claims Detail Table}
\usage{
include_rx_table(pooled_phar_spending_table, claims_detail_table)
}
\description{
Only use this function when the ROI analysis also includes pharmaceutical data. This function will amend the claims detail specified by claims_detail_table dataframe with the pharmaceutical claims spending specified by pooled_phar_spending_table. It returns a flextable object that is formatted to Teladoc branding specifications.
}
\arguments{
\itemize{
  \item{pooled_phar_spending_table}{The pooled spending table of total and condition-related pharmaceutical costs. It is returned by extract_phar_table.}
  \item{claims_detail_table}{The spending table of various condition-related and POS claims. It is returned by extract_claims_detail_table..}
}
\examples{
include_rx_table(pooled_phar_spending_table = pmpm_phar_tables$pooled_phar_spending_table, claims_detail_table = claims_detail$table)
}
