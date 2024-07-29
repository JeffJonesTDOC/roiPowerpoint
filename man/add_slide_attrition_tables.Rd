\name{add_slide_attrition_tables}
\alias{add_slide_attrition_tables}
\title{Creates Population Attrition Tables Slide(s)}
\usage{
add_slide_attrition_tables(ppt, post_period_length, study)
}
\description{
Creates the population attrition table slides, one for each cohort. The created slide(s) are then appended to ppt, the PowerPoint object.
}
\arguments{
\item{ppt}{A PowerPoint object. Read in initially using get_pptx_template.}
\item{post_period_length}{The number of years of the ROI analysis (post-period). For year-over-year, set to 1. For a two-year analysis (with 2 cohorts), set to 2, and et cetera.}
\item{study}{the timeframe of the ROI analysis. Set to "YOY", "1YR", "2YR", or "3YR".}
}
\examples{
"/Users/janedoe/Downloads","TemplatePPTX.pptx"
}
