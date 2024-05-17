\name{make_attrition_table}
\alias{make_attrition_table}
\title{Generates Population Attrition Table(s)}
\usage{
make_attrition_table(nYear, program, summaryStatsSheet, minEnrollmentLength, population_conditions, studyTimePeriodStartDate)
}
\description{
Creates and formats a population attrition table for each cohort. This includes the following:
\itemize{
\item Removes "pre-launch eligible" row and renames the "post-launch eligible" row with "pre-launch and post-launch eligible" always, for brevity.
\item Removes the "Enrolled in <study timeframe>" row always, for brevity.
\item Includes a "Final Matched Population" row at the bottom of the table, to help clarify the injective matching of non-members to members.
\item Removes "Enrolled for more than \textit{n} months" rows where \textit{n}!=minEnrollmentLength
\item Removes the non-applicable outlier rows, as specified by population_condition.
\item When population_conditions != "Capped", flips the outlier remove row from percentage and number removed to percentage and number remaining.
\item For YOY analyses, renames the "activated in <study timeframe>" row to "Ever activated", for clarity.
}

A flextable object with Teladoc formatting and coloring is created from the resulting table(s). Additionally, various data pieces located in the summary stats sheet are also extracted during this process. Risk scores, ages, and cohort sizes are returned for use in our separate ROI data extraction process.
}
\arguments{
  \item{nYear}{The number of years of the ROI analysis (post-period). For year-over-year, set to 1. For a two-year analysis (with 2 cohorts), set to 2, and et cetera.}
  \item{program}{The Teladoc program the analysis is looking at. Set to "Diabetes" or "Hypertension".}
  \item{summaryStatsSheet}{The data frame containing data from the "Summary stats" sheets in the excel file. Returned with read_excel().}
  \item{minEnrollmentLength}{The minimum member enrollment length to be considered a member. Returned via extract_sheet_data().}
  \item{population_conditions}{The outlier removal criteria used in the analysis. Set to "50K Monthly", "100K Annual", or "Capped".}
  \item{studyTimePeriodStartDate}{The launch date of the ROI analysis. Returned via extract_sheet_data().}
}
\examples{

### Generates one attrition table for an ROI analysis looking at a Diabetes program. The analysis removes members with annual claim costs over $100,000, and members must be enrolled for at least 3 months.
attrition_table_data =  make_attrition_table(nYear = 1, program="Diabetes", summaryStatsSheet=all_sheets$summaryStatsSheet,minEnrollmentLength = 3,population_conditions = "100K Annual",studyTimePeriodStartDate = sheet_data_extract$studyTimePeriodStartDate)

### Generates two attrition tables (one for each cohort) for an ROI analysis looking at a Hypertension program. The analysis uses the outlier removal method of capping high costs claimants at the 95th percentile, and members must be enrolled for at least 6 months.
attrition_table_data =  make_attrition_table(nYear = 2, program="Hypertension", summaryStatsSheet=all_sheets$summaryStatsSheet,minEnrollmentLength = 6,population_conditions = "Capped",studyTimePeriodStartDate = sheet_data_extract$studyTimePeriodStartDate)

}
