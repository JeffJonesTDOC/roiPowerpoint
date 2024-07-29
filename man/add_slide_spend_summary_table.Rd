\name{add_slide_spend_summary_table}
\alias{add_slide_spend_summary_table}
\title{Creates Claims Spending Summary Slide}
\usage{
add_slide_spend_summary_table(ppt, program, spend_summary, format1 = centurygothic8gray)
}
\description{
Creates the slide containing the claims spending summary table, as well as accompanying disclaimer text and Title. The created slide is then appended to ppt, the PowerPoint object.
}
\arguments{
\item{ppt}{A PowerPoint object. Read in initially using get_pptx_template.}
\item{program}{Program the ROI is analyzing. Diabetes, Hypertension, DPP, etc.}
\item{spend_summary}{The list object returned using extract_spend_summary_table(). Contains a data frame and flextable of claims spending summary by condition and place of service.}
\item{format1}{Text format used for the disclaimer text underneath the table. Default is Century Gothic font, size 8, gray color.}
}
\examples{
"/Users/janedoe/Downloads","TemplatePPTX.pptx"
}
