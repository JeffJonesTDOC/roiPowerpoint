# This function creates the difference-in-difference flextable used in the ROI
# results slide.

make_did_table <- function(post_period_length, claims_detail_table,pooled_phar_spending_table,year0) {
  require(flextable)
  if (post_period_length == 1) {
    DID_table = as.data.frame(t(c("Medical",
                                  claims_detail_table[1,2],claims_detail_table[1,3],as.numeric(claims_detail_table[1,3])-as.numeric(claims_detail_table[1,2]), #Non-member med costs & difference
                                  claims_detail_table[1,5],claims_detail_table[1,6],as.numeric(claims_detail_table[1,6])-as.numeric(claims_detail_table[1,5]), # Member med costs & difference
                                  (as.numeric(claims_detail_table[1,5])-as.numeric(claims_detail_table[1,6]))-(as.numeric(claims_detail_table[1,2])-as.numeric(claims_detail_table[1,3]))))) # DiD
    if (has_rx) {
      DID_table = rbind(DID_table,t(c("Pharmaceutical",
                                      round(as.numeric(pooled_phar_spending_table[1,2])),round(as.numeric(pooled_phar_spending_table[1,3])),round(as.numeric(pooled_phar_spending_table[1,3])-as.numeric(pooled_phar_spending_table[1,2])),
                                      round(as.numeric(pooled_phar_spending_table[1,5])),round(as.numeric(pooled_phar_spending_table[1,6])),round(as.numeric(pooled_phar_spending_table[1,6])-as.numeric(pooled_phar_spending_table[1,5])),
                                      round((as.numeric(pooled_phar_spending_table[1,5])-as.numeric(pooled_phar_spending_table[1,6]))-(as.numeric(pooled_phar_spending_table[1,2])-as.numeric(pooled_phar_spending_table[1,3]))))))
      total_row = c("Total",
                    round(claims_detail_table[1,2]+as.numeric(pooled_phar_spending_table[1,2])),round(claims_detail_table[1,3]+as.numeric(pooled_phar_spending_table[1,3])),
                    round(claims_detail_table[1,3]+as.numeric(pooled_phar_spending_table[1,3]))-round(claims_detail_table[1,2]+as.numeric(pooled_phar_spending_table[1,2])),
                    round(claims_detail_table[1,5]+as.numeric(pooled_phar_spending_table[1,5])),round(claims_detail_table[1,6]+as.numeric(pooled_phar_spending_table[1,6])),
                    round(claims_detail_table[1,6]+as.numeric(pooled_phar_spending_table[1,6]))-round(claims_detail_table[1,5]+as.numeric(pooled_phar_spending_table[1,5])),
                    (as.numeric(claims_detail_table[1,5])-as.numeric(claims_detail_table[1,6]))-(as.numeric(claims_detail_table[1,2])-as.numeric(claims_detail_table[1,3]))+round((as.numeric(pooled_phar_spending_table[1,5])-as.numeric(pooled_phar_spending_table[1,6]))-(as.numeric(pooled_phar_spending_table[1,2])-as.numeric(pooled_phar_spending_table[1,3]))))
      DID_table = rbind(DID_table,total_row)
    }
    for (i in 2:8) {
      for (j in 1:nrow(DID_table)) {
        DID_table[j,i] = paste0("$",DID_table[j,i]); DID_table[j,i] = gsub("\\$-","-$",DID_table[j,i]);
        if (i %in% c(4,7)) if (substr(DID_table[j,i],1,1) != "-") DID_table[j,i] = paste0("+",DID_table[j,i])
      }
    }

    colnames(DID_table) = c("Total allowed costs",year0,as.character(as.numeric(year0)+1),"Difference",paste0(" ",year0),paste0(" ",as.character(as.numeric(year0)+1))," Difference","Savings")
    DID_flextable = flextable::flextable(DID_table)
    DID_flextable <- flextable::add_header_row(DID_flextable,values = c(" ","Non-member","Member","DID"), colwidths = c(1,3,3,1))
    DID_flextable <- flextable::bg(DID_flextable,bg="#66478F",i=1,j=2:8,part="header")
    DID_flextable <- flextable::bg(DID_flextable,bg="#696b71",i=2,j=2:8,part="header")
    DID_flextable <- flextable::color(DID_flextable,color="white",j=2:8, part="header")
    DID_flextable <- flextable::align(DID_flextable,align="center",part="all")
    DID_flextable <- flextable::bold(DID_flextable,j=1,part="all")
    DID_flextable <- flextable::bold(DID_flextable,part="header")
    DID_flextable <- flextable::font(DID_flextable,fontname="Century Gothic",part="all")
    DID_flextable <- flextable::height(DID_flextable,i=2,height=1,part="header",unit="in")
    DID_flextable <- flextable::border_remove(DID_flextable)
    DID_flextable <- flextable::surround(DID_flextable,j=2:8,border=officer::fp_border(color="black",style="solid",width=1),part="header")
    DID_flextable <- flextable::border_inner(DID_flextable,border=officer::fp_border(color="black",style="solid",width=1),part="body")
    DID_flextable <- flextable::surround(DID_flextable,i=2,j=1,border=officer::fp_border(color="black",style="solid",width=2),part="header")
    DID_flextable <- flextable::surround(DID_flextable,i=2,j=1,border.right=officer::fp_border(color="black",style="solid",width=1),part="header")
    DID_flextable <- flextable::border_outer(DID_flextable,border=officer::fp_border(color="black",style="solid",width=2),part="body")
    DID_flextable <- flextable::bg(DID_flextable,bg="#e6e7e8",i=2,j=1,part="header")
    DID_flextable <- flextable::bg(DID_flextable,bg="#c6efcd",part="body",j=8)
    DID_flextable <- flextable::width(DID_flextable,j=1,width=2,unit="in")
    DID_flextable <- flextable::fontsize(DID_flextable,size=9,part="all")
    DID_flextable
  }
  if (post_period_length != 1) {DID_flextable = NULL}

  return(DID_flextable)
}
