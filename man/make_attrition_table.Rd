\name{make_attrition_table}
\alias{make_attrition_table}
\title{Generates Population Attrition Table(s)}
\usage{
make_attrition_table(post_period_length, program, summary_stats_sheet, min_activation_length, population_conditions, study_start_date)
}
\description{
Creates and formats a population attrition table for each cohort. This includes the following:
\itemize{
\item Removes "pre-launch eligible" row and renames the "post-launch eligible" row with "pre-launch and post-launch eligible" always, for brevity.
\item Removes the "Enrolled in <study timeframe>" row always, for brevity.
\item Includes a "Final Matched Population" row at the bottom of the table, to help clarify the injective matching of non-members to members.
\item Removes "Enrolled for more than \textit{n} months" rows where \textit{n}!=min_activation_length
\item Removes the non-applicable outlier rows, as specified by population_condition.
\item When population_conditions != "Capped", flips the outlier remove row from percentage and number removed to percentage and number remaining.
\item For YOY analyses, renames the "activated in <study timeframe>" row to "Ever activated", for clarity.
}

A flextable object with Teladoc formatting and coloring is created from the resulting table(s). Additionally, various data pieces located in the summary stats sheet are also extracted during this process. Risk scores, ages, and cohort sizes are returned for use in our separate ROI data extraction process.
}
\arguments{
  \item{post_period_length}{The number of years of the ROI analysis (post-period). For year-over-year, set to 1. For a two-year analysis (with 2 cohorts), set to 2, and et cetera.}
  \item{program}{The Teladoc program the analysis is looking at. Set to "Diabetes" or "Hypertension".}
  \item{summary_stats_sheet}{The data frame containing data from the "Summary stats" sheets in the excel file. Returned with read_excel().}
  \item{min_activation_length}{The minimum member enrollment length to be considered a member. Returned via extract_sheet_data().}
  \item{population_conditions}{The outlier removal criteria used in the analysis. Set to "50K Monthly", "100K Annual", or "Capped".}
  \item{study_start_date}{The launch date of the ROI analysis. Returned via extract_sheet_data().}
}
\examples{
# This example walks through an ROI Powerpoint creation from start to finish. When running this yourself,
# you must modify the arguments in read_excel() to the correct file_name and directory.

# Set formats for flextable objects
{
  calibri14italwhite = officer::fp_text(font.family = "Calibri", font.size = 14, ital = T, color = "#FFFFFF")
  centurygothic12 = officer::fp_text(font.family = "Century Gothic", font.size=12)
  centurygothic16bold = officer::fp_text(font.family = "Century Gothic", font.size=16, bold=T)
  centurygothic20bold = officer::fp_text(font.family = "Century Gothic", font.size=20, bold=T)
  centurygothic14 = officer::fp_text(font.family = "Century Gothic", font.size=14)
  centurygothic24purple = officer::fp_text(font.family = "Century Gothic", font.size=24, color = "#55437d")
  centurygothic14purple = officer::fp_text(font.family = "Century Gothic", font.size=14, color = "#55437d")
  centurygothic18purple = officer::fp_text(font.family = "Century Gothic", font.size=18, color = "#55437d")
  centurygothic8gray = officer::fp_text(font.family = "Century Gothic", font.size = 8, color = "#808080")
}

# Function Arguments
{
  study = "YOY" # Possible values: one of c("YOY","1YR","2YR","3YR")
  file_name = "Aperture_Science_Diabetes_YOY.xlsm"
  program = "Diabetes"
  has_rx = F
  zero_yaxis = F
  combine_ip_op = F
  HTN_population = "All"
  remove_POS = F
  population_conditions = "100K Annual"

}


all_sheets = roiPowerpoint::read_excel(study,file_name,directory=getwd())
sheet_data = roiPowerpoint::extract_sheet_data(program,
                                               study,
                                               roi_sheet = all_sheets$roi_sheet,
                                               data_summary_sheet = all_sheets$data_summary_sheet,
                                               data_overview_sheet = all_sheets$data_overview_sheet,
                                               HTN_population = "All")
all_sheets$roiSheet = sheet_data$roiSheet # Overwrite the original ROI sheet with the modified one created from extract_sheet_data
attrition_table_data = roiPowerpoint::make_attrition_table(post_period_length = sheet_data$post_period_length,
                                                           program = program,
                                                           summary_stats_sheet =all_sheets$summary_stats_sheet,
                                                           min_activation_length = sheet_data$min_activation_length,
                                                           population_conditions = population_conditions,
                                                           study_start_date = sheet_data$study_start_date)
pooled_cohort_tables = roiPowerpoint::make_pooled_cohort_tables(post_period_length = sheet_data$post_period_length,
                                                                final_cohort_size = attrition_table_data$final_cohort_size,
                                                                study_start_date = sheet_data$study_start_date,
                                                                summary_stats_sheet = all_sheets$summary_stats_sheet)
graphs = roiPowerpoint::make_roi_graph_and_table(roi_sheet = all_sheets$roi_sheet,
                                                 post_period_length = sheet_data$post_period_length,
                                                 study = study,
                                                 program = program,
                                                 has_rx = has_rx,
                                                 zero_yaxis = zero_yaxis,
                                                 year0 = sheet_data$year0,
                                                 year1 = sheet_data$year1)
claims_detail = roiPowerpoint::extract_claims_detail_table(post_period_length = sheet_data$post_period_length,
                                                           roi_sheet = all_sheets$roi_sheet,
                                                           combine_ip_op = combine_ip_op,
                                                           year0 = sheet_data$year0,
                                                           HTN_population = HTN_population,
                                                           remove_POS=remove_POS)
pppm_tables = roiPowerpoint::extract_pppm_changes(has_rx = has_rx,
                                                  post_period_length = sheet_data$post_period_length,
                                                  ROI_sheet = all_sheets$roi_sheet,
                                                  claims_detail_table = claims_detail$table,
                                                  program = program,
                                                  pharmacy_costs_sheet = all_sheets$pharmacy_costs_sheet)
roi_table = roiPowerpoint::make_roi_table(has_rx = has_rx,
                                          post_period_length = sheet_data$post_period_length,
                                          program = program,
                                          final_cohort_size = attrition_table_data$final_cohort_size,
                                          price_of_program = sheet_data$price_of_program,
                                          pppm_changes = pppm_tables,
                                          supply_cost = sheet_data$supply_cost,
                                          pooled_cohort_table2 = pooled_cohort_tables$table2)
phar_detail_table = roiPowerpoint::extract_phar_table(has_rx = has_rx,
                                                      post_period_length = sheet_data$post_period_length,
                                                      pharmacy_costs_sheet = all_sheets$pharmacy_costs_sheet,
                                                      ROI_sheet = all_sheets$roi_sheet,
                                                      claims_detail_table_column_names = claims_detail$column_names,
                                                      claims_detail_table = claims_detail$table)
did_table = roiPowerpoint::make_did_table(post_period_length = sheet_data$post_period_length,
                                          claims_detail_table = claims_detail$table,
                                          pooled_phar_spending_table = phar_detail_table,
                                          year0 = sheet_data$year0)
if (has_rx) {
  claims_detail$flex_table = roiPowerpoint::include_rx_table(pooled_phar_spending_table = phar_detail_table,
                                                             claims_detail_table = claims_detail$table,
                                                             post_period_length = sheet_data$post_period_length)
}

roi_calculation_table = make_roi_calculation_table(roi_table = roi_table, has_rx = has_rx, program = program)

ppt = get_pptx_template("~/sharedFolders/jeff.jones/PowerPoint_Automation/","TemplatePPTX.pptx")
ppt = add_slide_title(ppt,
                      client_name = sheet_data$client_name,
                      program = program,
                      format1 = calibri14italwhite)
ppt = add_slide_exec_summary(ppt,
                             has_rx = has_rx,
                             post_period_length = sheet_data$post_period_length,
                             pmpm_phar_tables = pppm_tables,
                             program = program,
                             year0 = sheet_data$year0,
                             roi_table = roi_table)
ppt = add_slide_methodology(ppt,
                            post_period_length = sheet_data$post_period_length,
                            format1 = centurygothic12,
                            format2 = centurygothic16bold,
                            population_conditions = population_conditions,
                            data_overview_sheet = all_sheets$data_overview_sheet,
                            study_time_period_launch_date = sheet_data$study_launch_date,
                            year0 = sheet_data$year0)
ppt = add_slide_additional_methodology(ppt, post_period_length = sheet_data$post_period_length, study = study)
ppt = add_slide_program_title(ppt,program = program)
ppt = add_slide_attrition_tables(ppt,
                                 post_period_length = sheet_data$post_period_length,
                                 format1 = centurygothic24purple,
                                 attrition_table_data = attrition_table_data)
ppt = add_slide_pooled_cohorts(ppt,
                               study = study,
                               format1 = centurygothic24purple,
                               pooled_cohort_tables = pooled_cohort_tables)
ppt = add_slide_graph_results(ppt,
                              post_period_length = sheet_data$post_period_length,
                              graphs = graphs,
                              did_table = did_table,
                              program = program,
                              price_of_program = sheet_data$price_of_program,
                              roi_calculation_table = roi_calculation_table,
                              roi_table = roi_table,
                              supply_cost = sheet_data$supply_cost,
                              format1 = centurygothic24purple,
                              format2 = centurygothic8gray)
ppt = add_slide_claims_detail_table(ppt,
                                    program = program,
                                    claims_detail = claims_detail,
                                    format1 = centurygothic8gray)

print(ppt,paste(substr(file_name,1,nchar(file_name)-5),"_",Sys.Date(),"_Powerpoint.pptx",sep=""))
}
