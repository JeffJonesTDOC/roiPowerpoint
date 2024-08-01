\name{make_did_table}
\alias{make_did_table}
\title{Generates Difference-in-Difference Table}
\usage{
make_did_table(post_period_length, claims_detail_table,pooled_phar_spending_table,year0, year1)
}
\description{
Creates and formats the Difference-in-difference claims spending table used in the ROI results slide. Requires
the correct length of the post-period(s) to be specified via post_period_length; may error out if this is not correct. That is, setting post_period_length = 2 for
a year-over-year analysis will likely cause errors, as a YOY analysis has only 1 post period. Returns a flextable object formatted and suited for PowerPoint presentations.
}
\arguments{
  \item{post_period_length}{The number of years of the ROI analysis (post-period). For year-over-year, set to 1. For a two-year analysis (with 2 cohorts), set to 2, and et cetera.}
  \item{claims_detail_table}{The spending table of various condition-related and POS claims. It is returned by extract_claims_detail_table.}
  \item{pooled_phar_spending_table}{The pooled spending table of total and condition-related pharmaceutical costs. It is returned by extract_phar_table().}
  \item{year0}{Year 0, or pre-period, of the analysis. Can be specified as an integer or character. It is returned by extract_sheet_data().}
  \item{year1}{Year 1, or first post-period, of the analysis. Can be specified as an integer or character. It is returned by extract_sheet_data().}
  }
\examples{

### Generates the difference-in-difference table for a 2-year study with pre-period of 2021.
### Medical and Pharmaceutical summary tables are specified with claims_detail and pmpm_phar_tables objects, which are returned via
### extract_claims_detail_table() and extract_phar_table() respectively.
make_did_table(post_period_length = 2, claims_detail_table = claims_detail$table,year0 = 2021, year1 = 2022, pooled_phar_spending_table = pmpm_phar_tables$pooled_phar_spending_table)
}
