\name{test_function}
\title{Testing}
\usage{
test_function()
}
\description{
Testing
}
\arguments{
\item{study}{the timeframe of the ROI analysis. Set to "YOY", "1YR", "2YR", or "3YR".}
\item{file_name}{The name of the ROI workbook from which the sheets are extracted. Must include .xlsm extension.}
}
\examples{

testing = read.csv("/data/data_overview_sheet_dm.csv")
print(testing)
}

data_overview_sheet_dm = read.csv("/opt/home/jeff.jones/roiPowerpoint/data/data_overview_sheet_dm.csv")
data_overview_sheet_htn = read.csv("/opt/home/jeff.jones/roiPowerpoint/data/data_overview_sheet_htn.csv")
data_summary_sheet_dm = read.csv("/opt/home/jeff.jones/roiPowerpoint/data/data_summary_sheet_dm.csv")
data_summary_sheet_htn = read.csv("/opt/home/jeff.jones/roiPowerpoint/data/data_summary_sheet_htn.csv")
pdc_sheet_dm = read.csv("/opt/home/jeff.jones/roiPowerpoint/data/pdc_sheet_dm.csv")
pdc_sheet_htn = read.csv("/opt/home/jeff.jones/roiPowerpoint/data/pdc_sheet_htn.csv")
pharmacy_costs_sheet_dm = read.csv("/opt/home/jeff.jones/roiPowerpoint/data/pharmacy_costs_sheet_dm.csv")
pharmacy_costs_sheet_htn = read.csv("/opt/home/jeff.jones/roiPowerpoint/data/pharmacy_costs_sheet_htn.csv")
roi_sheet_dm =  read.csv("/opt/home/jeff.jones/roiPowerpoint/data/roi_sheet_dm.csv")
roi_sheet_htn =  read.csv("/opt/home/jeff.jones/roiPowerpoint/data/roi_sheet_htn.csv")
summary_stats_sheet_dm = read.csv("/opt/home/jeff.jones/roiPowerpoint/data/summary_stats_sheet_dm.csv")
summary_stats_sheet_htn = read.csv("/opt/home/jeff.jones/roiPowerpoint/data/summary_stats_sheet_htn.csv")

use_data(data_overview_sheet_dm)
use_data(data_overview_sheet_htn)
use_data(data_summary_sheet_dm)
use_data(data_summary_sheet_htn)
use_data(pdc_sheet_dm)
use_data(pdc_sheet_htn)
use_data(pharmacy_costs_sheet_dm)
use_data(pharmacy_costs_sheet_htn)
use_data(roi_sheet_dm)
use_data(roi_sheet_htn)
use_data(summary_stats_sheet_dm)
use_data(summary_stats_sheet_htn)

