\name{make_pooled_cohort_tables}
\alias{make_pooled_cohort_tables}
\title{Generates Two Pooled Cohort Tables}
\usage{
make_pooled_cohort_tables(post_period_length,
                          final_cohort_size,
                          study_start_date,
                          summary_stats_sheet)
}
\description{
Creates the pooled cohort tables displayed in the pooled cohort slide. The first table shows the logic behind pooling cohorts together by year of tenure. The second table shows the calculation of the cohort sizes for each analysis year.

For 1-year analyses, these two tables are still generated as the trivial case, and the pooled cohort slide is simply not generated. This function must still run for use in other functions.
}
\arguments{
  \item{post_period_length}{The number of years of the ROI analysis (post-period). For year-over-year, set to 1. For a two-year analysis (with 2 cohorts), set to 2, and et cetera.}
  \item{final_cohort_size}{An array of sample size(s) for each cohort of the analysis. Returned by make_attrition_table().}
  \item{study_start_date}{The launch date of the ROI analysis. Returned via extract_sheet_data().}
  \item{summary_stats_sheet}{The data frame containing data from the "Summary stats" sheets in the excel file. Returned with read_excel().}
}
