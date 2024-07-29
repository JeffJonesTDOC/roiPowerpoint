\name{add_slide_methodology}
\alias{add_slide_methodology}
\title{Creates Methodology Slide}
\usage{
add_slide_methodology(ppt, post_period_length, population_conditions, data_overview_sheet, study_time_period_launch_date, year0, format1 = centurygothic12, format2 = centurygothic16bold)
}
\description{
Creates the slide containing the methodology information. Approach, inclusion criteria, matching, and study time period text boxes are dynamically created. The created slide is then appended to ppt, the PowerPoint object.
}
\arguments{
\item{ppt}{A PowerPoint object. Read in initially using get_pptx_template.}
\item{post_period_length}{The number of years of the ROI analysis (post-period). For year-over-year, set to 1. For a two-year analysis (with 2 cohorts), set to 2, and et cetera.}
\item{population_conditions}{The outlier removal criteria used in the analysis. Set to "50K Monthly", "100K Annual", or "Capped".}
\item{data_overview_sheet}{"data_overview_1" sheet from the relevant Excel workbook. It is returned by read_excel().}
\item{study_time_period_launch_date}{The launch date of the analysis. It is returned using extract_sheet_data().}
\item{year0}{The integer year of the analysis pre-period.}
\item{format1}{Text format used for the bulk of the text in each text box. Default is Century Gothic font, size 12, black color.}
\item{format2}{Text format used for each text box's header text. Default is Century Gothic font, size 16, black color, bold.}
}
\examples{
"/Users/janedoe/Downloads","TemplatePPTX.pptx"
}
