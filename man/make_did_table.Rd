\name{make_did_table}
\alias{make_did_table}
\title{Generates Difference-in-Difference Table}
\usage{
make_did_table(nYear, spendSummaryTable,pooledPharSpendingTable,year0, year1)
}
\description{
Creates and formats the Difference-in-difference claims spending table used in the ROI results slide. Requires
the correct length of the post-period(s) to be specified via nYear; may error out if this is not correct. That is, setting nYear = 2 for
a year-over-year analysis will likely cause errors, as a YOY analysis has only 1 post period. Returns a flextable object formatted and suited for PowerPoint presentations.
}
\arguments{
\itemize{
  \item{nYear}{The number of years of the ROI analysis (post-period). For year-over-year, set to 1. For a two-year analysis (with 2 cohorts), set to 2, and et cetera.}
  \item{spendSummaryTable}{The spending table of various condition-related and POS claims. Use extract_spend_summary_table() to generate it.}
  \item{pooledPharSpendingTable}{The pooled spending table of total and condition-related pharmaceutical costs. Use extract_phar_table() to generate it.}
  \item{year0}{Year 0, or pre-period, of the analysis. Can be specified as an integer or character. It is returned by sheet_data_extract().}
  \item{year1}{Year 1, or first post-period, of the analysis. Can be specified as an integer or character. It is returned by sheet_data_extract().}
  }
}
\examples{

### Generates the difference-in-difference table for a 2-year study with pre-period of 2021.
### Medical and Pharmaceutical summary tables are specified with spend_summary and pmpm_phar_tables objects, which are returned via
### extract_spend_summary_table() and extract_phar_table() respectively.
make_did_table(nYear = 2, spendSummaryTable = spend_summary$table,year0 = 2021, year1 = 2022, pooledPharSpendingTable = pmpm_phar_tables$pooled_phar_spending_table)
}
