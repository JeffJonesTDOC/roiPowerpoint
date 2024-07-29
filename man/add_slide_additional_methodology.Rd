\name{add_slide_additional_methodology}
\alias{add_slide_additional_methodology}
\title{Creates YOY or Multi-year Methodology Slides.}
\usage{
add_slide_additional_methodology(ppt, post_period_length, study)
}
\description{
Creates either year-over-year methodology slide or the multi-year methodology slide.
}
\arguments{
\item{ppt}{A PowerPoint object. Read in initially using get_pptx_template.}
\item{post_period_length}{The number of years of the ROI analysis (post-period). For year-over-year, set to 1. For a two-year analysis (with 2 cohorts), set to 2, and et cetera.}
\item{study}{the timeframe of the ROI analysis. Set to "YOY", "1YR", "2YR", or "3YR".}
}
\examples{
"/Users/janedoe/Downloads","TemplatePPTX.pptx"
}
