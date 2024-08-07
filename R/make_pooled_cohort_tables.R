# This function generates the pooled cohort tables to show combined cohort sizes for multi-year analyses.
# The first table shows the general structure of how cohort sizes are calculated, and the second table shows the actual data.

make_pooled_cohort_tables <- function(post_period_length,final_cohort_size, summary_stats_sheet, study_start_date) {
require(lubridate)
  # Generate the first table data frame.
  pooled_cohort_table1 = as.data.frame(matrix(nrow=post_period_length,ncol=2+post_period_length))
  names_table1 = c("Cohorts","Activation Timeframe")
  for (i in 1:post_period_length) {names_table1 = append(names_table1,paste("Year",i))}
  colnames(pooled_cohort_table1) = names_table1
  for (i in 1:post_period_length) {
    pooled_cohort_table1$Cohorts[i] = paste("Cohort",i)
    pooled_cohort_table1$`Activation Timeframe`[i] = paste(format(lubridate::ymd(as.Date(study_start_date)+(365*(i-1))),"%m/%Y"),"-",format(lubridate::ymd(as.Date(study_start_date)+(365*i)-30),"%m/%Y"))
    pooled_cohort_table1[i,3:(ncol(pooled_cohort_table1)-i+1)] = 'X'
  }

  # Generate the second table data frame.
  pooled_cohort_table2 = as.data.frame(matrix(nrow=post_period_length,ncol=1+post_period_length))
  table2Names = c("Groups")
  for (i in 1:post_period_length) {table2Names = append(table2Names,paste("Year",i))}
  colnames(pooled_cohort_table2) = table2Names
  nSizeRow = which(summary_stats_sheet[,3] == "All")
  for (i in 1:post_period_length) {
    pooled_cohort_table2$Groups[i] = paste("Cohort",i)
    pooled_cohort_table2[i,2:(ncol(pooled_cohort_table2)-i+1)] = final_cohort_size[i]
  }
  footer_row = c("Pooled Cohort"); for(i in 2:ncol(pooled_cohort_table2)){footer_row=append(footer_row,sum(na.omit(as.numeric(pooled_cohort_table2[,i]))))}
  pooled_cohort_table2 = rbind(pooled_cohort_table2,footer_row)

  # Generate the first table flextable.
  pooled_cohort_ft1 <- flextable(pooled_cohort_table1)
  pooled_cohort_ft1 <- add_header_row(pooled_cohort_ft1,colwidths = c(2,post_period_length),values = c(" ","Analysis groupings by Year on Program"),)
  pooled_cohort_ft1 <- bg(pooled_cohort_ft1, bg = "#66478F", part = "header")
  pooled_cohort_ft1 <- bold(pooled_cohort_ft1,part="header")
  pooled_cohort_ft1 <- color(pooled_cohort_ft1,color="white", part="header")
  pooled_cohort_ft1 <- align(pooled_cohort_ft1,align="center",part="header")
  pooled_cohort_ft1 <- width(pooled_cohort_ft1,width=1)
  pooled_cohort_ft1 <- width(pooled_cohort_ft1,j=2,width=1.5)
  pooled_cohort_ft1 <- font(pooled_cohort_ft1,fontname="Century Gothic",part="all")
  pooled_cohort_ft1 <- border_outer(pooled_cohort_ft1,border=fp_border(color="black",style="solid",width=2),part="all")
  pooled_cohort_ft1 <- border_inner(pooled_cohort_ft1,border=fp_border(color="black",style="solid",width=2),part="header")
  pooled_cohort_ft1 <- border_inner(pooled_cohort_ft1,border=fp_border(color="black",style="solid",width=1),part="body")

  # Generate the second table flextable.
  pooled_cohort_ft2 <- flextable(pooled_cohort_table2)
  pooled_cohort_ft2 <- bg(pooled_cohort_ft2, bg = "#55437d", part = "header")
  pooled_cohort_ft2 <- bold(pooled_cohort_ft2,part="header")
  pooled_cohort_ft2 <- color(pooled_cohort_ft2,color="white", part="header")
  pooled_cohort_ft2 <- bg(pooled_cohort_ft2,i=post_period_length+1, bg="#66478F",part="body")
  pooled_cohort_ft2 <- color(pooled_cohort_ft2,i=post_period_length+1,color="white",part="body")
  pooled_cohort_ft2 <- bold(pooled_cohort_ft2,i=post_period_length+1,part="body")
  pooled_cohort_ft2 <- width(pooled_cohort_ft2,j=1,width=1.5)
  pooled_cohort_ft2 <- font(pooled_cohort_ft2,fontname="Century Gothic",part="all")
  pooled_cohort_ft2 <- align(pooled_cohort_ft2,align="center",part="all")
  pooled_cohort_ft2 <- border_outer(pooled_cohort_ft2,border=fp_border(color="black",style="solid",width=2),part="all")
  pooled_cohort_ft2 <- border_inner(pooled_cohort_ft2,border=fp_border(color="black",style="solid",width=2),part="header")
  pooled_cohort_ft2 <- border_inner(pooled_cohort_ft2,border=fp_border(color="black",style="solid",width=1),part="body")

  returnList = list(pooled_cohort_table1,pooled_cohort_table2,pooled_cohort_ft1,pooled_cohort_ft2)
  names(returnList) = c("table1","table2","flextable1","flextable2")
  return(returnList)
}
