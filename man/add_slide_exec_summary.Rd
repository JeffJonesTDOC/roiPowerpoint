\name{add_slide_exec_summary}
\alias{add_slide_exec_summary}
\title{Creates Executive Summary Slide}
\usage{
add_slide_exec_summary(ppt, has_rx, post_period_length, pmpm_phar_tables, program, year0, roi_table)
}
\description{
Creates the executive summary slide, excluding outcomes results graphs. The created slide is then appended to ppt, the PowerPoint object. The outcomes results graphs must be manually added to the slide.
}
\arguments{
\item{ppt}{A PowerPoint object. Read in initially using get_pptx_template.}
\item{has_rx}{Boolean to check if the ROI analysis includes rx claims.}
\item{post_period_length}{The number of years of the ROI analysis (post-period). For year-over-year, set to 1. For a two-year analysis (with 2 cohorts), set to 2, and et cetera.}
\item{pmpm_phar_tables}{List object containing two data frame related to PMPM changes. It is returned using get_pmpm_changes().}
\item{program}{Program the ROI is analyzing. Diabetes, Hypertension, DPP, etc.}
\item{year0}{The integer year of the analysis pre-period.}
\item{roi_table}{The list object containing various information on ROI results. It is returned using make_roi_table().}
}
\examples{
"/Users/janedoe/Downloads","TemplatePPTX.pptx"
}
