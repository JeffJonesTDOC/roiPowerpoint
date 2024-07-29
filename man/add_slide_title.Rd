\name{add_slide_title}
\alias{add_slide_title}
\title{Creates ROI Title Slide}
\usage{
add_slide_title(ppt, client_name, program, format1 = calibri14italwhite)
}
\description{
Creates ROI Title Slide, which includes client name (taken from sheet_data_extract) and program. The created slide is then appended to ppt, the PowerPoint object.
}
\arguments{
\item{ppt}{A PowerPoint object. Read in using get_pptx_template.}
\item{client_name}{Name of the client.}
\item{program}{Program the ROI is analyzing. Diabetes, Hypertension, DPP, etc.}
\item{format1}{Text format used for the title slide text. Default is Calibri font, size 14, italized, white color.}
}
\examples{
"/Users/janedoe/Downloads","TemplatePPTX.pptx"
}
