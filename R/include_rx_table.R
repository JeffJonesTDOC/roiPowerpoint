# This function will modify the spend summary table to include pharmaceutical
# data. It essentially tacks on pharmaceutical data to the bottom of the
# already-existing spend summary table. It does NOT check for existence
# of rx data; it already assumes the data is there.

include_rx_table <- function(pooled_phar_spending_table, spend_summary_table) {
  for (i in c(1,2)) {
    for (j in 2:ncol(pooled_phar_spending_table)) {
      if (abs(as.numeric(pooled_phar_spending_table[i,j])) < 1) {
        pooled_phar_spending_table[i,j] = percent(as.numeric(pooled_phar_spending_table[i,j]))
      } else {pooled_phar_spending_table[i,j] = round(as.numeric(pooled_phar_spending_table[i,j]),0)}
      pooled_phar_spending_table[i,j] = gsub("\\$-","-$",pooled_phar_spending_table[i,j])
    }
  }
  pooled_phar_spending_table[,1] = c("Total pharmaceutical costs","Diabetes-related pharmaceutical costs")
  names(pooled_phar_spending_table) = names(spend_summary_table)
  spend_summary_table_with_phar = rbind(spend_summary_table,pooled_phar_spending_table)
  for (k in 2:ncol(spend_summary_table_with_phar)) {
    if (!is.na(as.numeric(spend_summary_table_with_phar[2,k]))) {
      spend_summary_table_with_phar[,k] = paste("$",spend_summary_table_with_phar[,k],sep="")
      spend_summary_table_with_phar[,k] = gsub("\\$-","-$",spend_summary_table_with_phar[,k])
    }
  }
  spend_summary_table_with_phar_ft <- flextable::flextable(spend_summary_table_with_phar)
  spend_summary_table_with_phar_ft <- flextable::add_header_row(spend_summary_table_with_phar_ft,values = c(" ","Non-member","Member"," "), colwidths = c(1,2*nYear+1,2*nYear+1,nYear*2))
  spend_summary_table_with_phar_ft <- flextable::bg(spend_summary_table_with_phar_ft,bg="#66478F",part="header")
  spend_summary_table_with_phar_ft <- flextable::color(spend_summary_table_with_phar_ft,color="white", part="header")
  spend_summary_table_with_phar_ft <- flextable::color(spend_summary_table_with_phar_ft,color="white", part="body",j=1)
  for (i1 in 1:nrow(spend_summary_table_with_phar)) {
    for (j1 in (ncol(spend_summary_table_with_phar)-2*nYear+1):ncol(spend_summary_table_with_phar)) {
      if (spend_summary_table_with_phar[i1,j1] > 0) {
        spend_summary_table_with_phar_ft <- flextable::bg(spend_summary_table_with_phar_ft, i=i1, j=j1, bg="#E1DDE5",part="body")
      } else {
        spend_summary_table_with_phar_ft <- flextable::bg(spend_summary_table_with_phar_ft, i=i1, j=j1, bg="#c6efcd",part="body")
      }
    }
  }
  spend_summary_table_with_phar_ft <- flextable::bg(spend_summary_table_with_phar_ft,j=1,bg = "#55437d", part="body")
  spend_summary_table_with_phar_ft <- flextable::align(spend_summary_table_with_phar_ft,align=c("center"),part="all")
  spend_summary_table_with_phar_ft <- flextable::width(spend_summary_table_with_phar_ft,j=1,width=1.5)
  spend_summary_table_with_phar_ft <- flextable::width(spend_summary_table_with_phar_ft,j=2:ncol(pooled_phar_spending_table),width=0.5)
  spend_summary_table_with_phar_ft <- flextable::fontsize(spend_summary_table_with_phar_ft,size=8,part="all")
  spend_summary_table_with_phar_ft <- flextable::font(spend_summary_table_with_phar_ft,fontname="century gothic",part="all")
  spend_summary_table_with_phar_ft <- flextable::border_inner(spend_summary_table_with_phar_ft,border=officer::fp_border(color="black",style="solid",width=1),part="all")
  spend_summary_table_with_phar_ft <- flextable::border_outer(spend_summary_table_with_phar_ft,border=officer::fp_border(color="black",style="solid",width=2),part = "body")
  spend_summary_table_with_phar_ft <- flextable::border_outer(spend_summary_table_with_phar_ft,border=officer::fp_border(color="black",style="solid",width=2),part = "all")
  spend_summary_table_with_phar_ft <- flextable::hline(spend_summary_table_with_phar_ft,i=nrow(spend_summary_table_with_phar)-2,border=officer::fp_border(color="black",style="solid",width=3))

  return(spend_summary_table_with_phar_ft)
}
