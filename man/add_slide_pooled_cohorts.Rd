\name{add_slide_pooled_cohorts}
\alias{add_slide_pooled_cohorts}
\title{Creates Pooled Cohorts Slide}
\usage{
add_slide_pooled_cohorts(ppt, study,format1 = centurygothic24purple, pooled_cohort_tables)
}
\description{
Creates the slide containing the two tables/visuals showing pooled cohort size and calculation. Only used for year-over-year analyses, as no other analyses have pooled cohorts. The created slide is then appended to ppt, the PowerPoint object.
}
\arguments{
\item{ppt}{A PowerPoint object. Read in initially using get_pptx_template.}
\item{study}{the timeframe of the ROI analysis. Set to "YOY", "1YR", "2YR", or "3YR".}
\item{format1}{Text format used for the slide title. Default is Century Gothic font, size 24, purple color.}
\item{pooled_cohort_tables}{Returned using make_pooled_cohort_tables. Contains two flextables related to pooled cohort calculations.}
}
\examples{
"/Users/janedoe/Downloads","TemplatePPTX.pptx"
}
