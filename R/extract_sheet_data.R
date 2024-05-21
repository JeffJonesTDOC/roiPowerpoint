# This function extracts all relevant data pieces from various sheets in the ROI excel file that were extracted using read_excel.
# Relevant data pieces are price_of_program, post_period_length (number of years of the analysis), study time period start and launch date,
# year 1 and year 0, minimum enrollment length (3, 6, or 9 months), client name, and supply cost (for DM).
#

extract_sheet_data <- function(ROI_sheet, program, study, data_summary_sheet, data_overview_sheet,HTN_population) {
  # Extract the price of livongo before modifying the file in some cases.
  price_of_program = round(as.numeric(ROI_sheet[which(ROI_sheet[,1] == 'Price of Livongo'),3][1]),0)

  # Modify the sheets for some oddball cases involving Hypertension
  {
    if (program=="Hypertension") {
      if (study == "2YR" | study == "3YR" | study == "4YR") {
        if (HTN_population == "All") {ROI_sheet=ROI_sheet[1:which(ROI_sheet[,1]=="DMHTN"),]}
        if (HTN_population == "HTN & DM") {ROI_sheet=ROI_sheet[(which(ROI_sheet[,1]=="DMHTN")+3):which(ROI_sheet[,1]=="HTN only"),]}
        if (HTN_population == "HTN only") {ROI_sheet=ROI_sheet[(which(ROI_sheet[,1]=="HTN only")+3):nrow(ROI_sheet),]}
      }
      if (study == "YOY" | study == "1YR") {
        if (HTN_population == "HTN & DM") {
          ROI_sheet[1:7,] = ROI_sheet[21:27,]
          spend_table_to_use = ROI_sheet[which(ROI_sheet[,15]=="Total costs")[1]:which(ROI_sheet[,15]=="Office visits")[1],15:24]
          ROI_sheet[which(ROI_sheet[,2]=="Total costs")[1]:which(ROI_sheet[,2]=="Office visits")[1],2:11] = spend_table_to_use
        }
        if (HTN_population == "HTN only") {
          ROI_sheet[1:7,] = ROI_sheet[41:47,]
          spend_table_to_use = ROI_sheet[which(ROI_sheet[,28]=="Total costs")[1]:which(ROI_sheet[,28]=="Office visits")[1],28:37]
          ROI_sheet[which(ROI_sheet[,2]=="Total costs")[1]:which(ROI_sheet[,2]=="Office visits")[1],2:11] = spend_table_to_use
        }
      }
    }
  }

  # Extract all relevant variables from the excel file.
  {
    ##### post_period_length extract #####
    if (study=="YOY") {
      post_period_length = 1
    } else {
      post_period_length = as.numeric(substr(study,1,1))
    }

    ##### start and launch date extract #####
    study_start_date=data_summary_sheet$Column1[which(data_summary_sheet$Client=="Study period start date")]
    study_launch_date=data_summary_sheet$Column1[which(data_summary_sheet$Client=="Study Launch Date")]
    # Sometimes, for various reasons, launch and start dates are missing or out of place. Default to 1900-01-01 in this case
    # in order to prevent breaks in other functions.
    if (study_start_date == 0) {
      study_start_date="1900-01-01"
      study_launch_date="1900-01-01"
    }
    study_launch_date = as.Date(study_launch_date,"%Y-%m-%d")

    ##### year 0 and year 1 extract #####
    year1 = (substr(data_summary_sheet$Column1[which(data_summary_sheet$Client=='Launch Date')],1,4))
    year0 = as.character(as.integer(year1)-1)
    year_index = 2
    while (year_index <= post_period_length) {
      assign(paste("year",year_index,sep=""),as.character(as.integer(year1)+(year_index)-1))
      year_index=year_index+1
    }

    ##### minimum enrollment length extract #####
    if (program == "Diabetes") {
      min_activation_length = data_overview_sheet[which(data_overview_sheet[,1] == "joined_months"),2]
    } else if (program == "Hypertension") {
      min_activation_length = data_overview_sheet[which(data_overview_sheet[,1] == "chosen_pop_list"),2]
    }

    #####  client name extract #####
    client_name = data_overview_sheet$data_overview[which(data_overview_sheet$X1=="company_name")]
    if (length(client_name)==0) {client_name = data_overview_sheet$data_overview[which(data_overview_sheet[,1] =="company_name")]}
    if (length(client_name)==0) {client_name = "not found"}

    ##### supply cost extract #####
    if (program == "Diabetes") {
      supply_cost = ROI_sheet[which(grepl("Diabetes Supplies Cost",ROI_sheet[,1])),2][1]
      if (supply_cost == "0" || is.na(supply_cost)) {supply_cost = "30"}
    } else if (program == "Hypertension") {
      supply_cost = "0"
    }
  }

  return(list(price_of_program = price_of_program,ROI_sheet = ROI_sheet, post_period_length = post_period_length,study_start_date = study_start_date, study_launch_date = study_launch_date, year1 = year1, year0 = year0, min_activation_length = min_activation_length,client_name = client_name,supply_cost = supply_cost))

}
