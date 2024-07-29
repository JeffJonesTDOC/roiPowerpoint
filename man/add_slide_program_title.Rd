\name{add_slide_program_title}
\alias{add_slide_program_title}
\title{Creates Title Slide for Specified Program.}
\usage{
add_slide_program_title(ppt,program)
}
\description{
Creates the title slide for the given Teladoc/Livongo program. The created slide is then appended to ppt, the PowerPoint object.
}
\arguments{
\item{ppt}{A PowerPoint object. Read in initially using get_pptx_template.}
\item{program}{Program the ROI is analyzing. Diabetes, Hypertension, DPP, etc.}
}
\examples{
"/Users/janedoe/Downloads","TemplatePPTX.pptx"
}
