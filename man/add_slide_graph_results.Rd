\name{add_slide_graph_results}
\alias{add_slide_graph_results}
\title{Creates the Graph Results Slide}
\usage{
add_slide_graph_results(ppt, post_period_length, graphs, did_table, program, supply_cost, price_of_program, text_box, roi_table, format1 = centurygothic24purple, format2 = centurygothic8gray)
}
\description{
Creates the slide containing the PMPM changes line graph, table of ROI results, and, for year-over-year analyses, the difference-in-difference table of pre and post period spending. The created slide is then appended to ppt, the PowerPoint object.
}
\arguments{
\item{ppt}{A PowerPoint object. Read in initially using get_pptx_template.}
\item{post_period_length}{The number of years of the ROI analysis (post-period). For year-over-year, set to 1. For a two-year analysis (with 2 cohorts), set to 2, and et cetera.}
\item{graphs}{The list object containing the ROI graph. It is returned by make_roi_graph_and_table().}
\item{did_table}{The flextable object containing the difference-in-difference table. It is returned by make_did_table().}
\item{program}{Program the ROI is analyzing. Diabetes, Hypertension, DPP, etc.}
\item{supply_cost}{The cost of supplies that is covered by our Diabetes program. Returned by extract_sheet_data().}
\item{price_of_program}{PPPM cost of program. Returned by extract_sheet_data().}
\item{roi_calculation_table}{The flextable object containing detailed ROI calculation. It is returned by create_roi_calculation_table().}
\item{roi_table}{The list object containing various information on ROI results. It is returned using make_roi_table().}
\item{format1}{Text format used for the title text. Default is Century Gothic font, size 24, purple color.}
\item{format2}{Text format used for the program and supply costs footer. Default is Century Gothic font, size 8, gray color.}
}
\examples{
"/Users/janedoe/Downloads","TemplatePPTX.pptx"
}
