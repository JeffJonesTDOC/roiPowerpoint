# Generates the overall claims spending graph for members (Actual), members (Expected), and non-members.
# Also generates the table showcasing ROI results.

make_roi_graph_and_table <- function(numeric_did_table, post_period_length, year1) {
  require(flextable)
  year0 = as.character(as.numeric(year1)-1)
  col_index = which(names(numeric_did_table) == year0)
  nm_total_costs = numeric_did_table[nrow(numeric_did_table),col_index:(col_index+post_period_length)]
  nm_diffs = as.numeric(nm_total_costs[,-1]) - as.numeric(nm_total_costs[1,1])
  m_total_costs = as.numeric(numeric_did_table[nrow(numeric_did_table),(col_index+(2*post_period_length)+1):(col_index+(3*post_period_length)+1)])
  graph_data_table = as.data.frame(matrix(nrow=3,ncol=post_period_length+2))
  names(graph_data_table) = c("Group",as.numeric(year0):(as.numeric(year0)+post_period_length))
  graph_data_table[,1] = c("Non-Member","Member (expected)","Member (actual)")
  graph_data_table[1,-1] = nm_total_costs
  graph_data_table[2,-1] = c(m_total_costs[1],m_total_costs[1]+nm_diffs)
  graph_data_table[3,-1] = m_total_costs
  graph_data_table[,-1] = lapply(graph_data_table[,-1],as.numeric)
  # Set up some order vectors to intelligently place text on the plot in good places
  orders = matrix(nrow=3,ncol=ncol(graph_data_table))
  for (j in 2:ncol(orders)) {orders[,j] = order(graph_data_table[,j],decreasing=T)}


  png("roi_plot.png",width = 750,height = 450, units = "px")
  roi_graph = plot(as.numeric(year0):(as.numeric(year0)+post_period_length),graph_data_table[1,-1],type="l",lwd=4,col=4,lty=1,
                   xlim=c(as.numeric(year0)-0.2,(as.numeric(year0)+post_period_length+0.2)),
                   ylim=c(min(graph_data_table[,-1])*0.9,max(graph_data_table[,-1])*1.1),
                   xlab="Year",ylab = "PPPM Costs",xaxt='n')
  axis(1,at=seq(as.numeric(year0),(as.numeric(year0)+post_period_length)))
  lines(as.numeric(year0):(as.numeric(year0)+post_period_length),graph_data_table[3,-1],lwd=4,col=6,lty=1)
  points(as.numeric(year0):(as.numeric(year0)+post_period_length),graph_data_table[1,2:3],pch=19,col=4)
  points(as.numeric(year0):(as.numeric(year0)+post_period_length),graph_data_table[3,2:3],pch=19,col=6)
  if (!(graph_data_table[1,2] == graph_data_table[3,2])) {
    lines(as.numeric(year0):(as.numeric(year0)+post_period_length),graph_data_table[2,-1],lwd=4,col=rgb(0.827,0.161,0.768,alpha=0.3),lty=1)
    points(as.numeric(year0):(as.numeric(year0)+post_period_length),graph_data_table[2,2:3],pch=19,col=rgb(0.827,0.161,0.768,alpha=0.3))
    legend(as.numeric(year0)-0.1,max(graph_data_table[,-1]*1.1),legend=c("Non-Member","Member, expected","Member, actual"),col=c(4,rgb(0.827,0.161,0.768,alpha=0.30),6),lty=c(1,1,1),lwd=c(3,3,3),cex=0.75)
    for (i in 1:3) {
      for (j in 2:ncol(orders)) {
        if (!(j==2 && orders[i,j]==2)) { # Don't plot livongo expected in pre-period, it will always be the same as livongo actual pre-period
          if (floor(log10(graph_data_table[orders[i,j],j]))+1 == 4) {offset_mult_pos = 1.01; offset_mult_neg = 0.99} else {offset_mult_pos = 1.03; offset_mult_neg = 0.97}
          text(x = (seq(as.numeric(year0),(as.numeric(year0)+post_period_length))+c(-0.03,rep(1,post_period_length-1),0.03))[j-1], y=graph_data_table[orders[i,j],j]*c(offset_mult_pos,1,offset_mult_neg)[i],labels = paste0("$",graph_data_table[orders[i,j],j]))
        }
      }
    }
  } else {
    legend(as.numeric(year0)-0.1,max(graph_data_table[,-1]*1.1),legend=c("Non-Member","Member"),col=c(4,6),lty=c(1,1),lwd=c(3,3),cex=0.75)
    for (i in c(1,3)) {
      for (j in 2:ncol(orders)) {
        if (!(j==2 && orders[i,j]==2)) { # Don't plot livongo expected in pre-period, it will always be the same as livongo actual pre-period
          if (floor(log10(graph_data_table[orders[i,j],j]))+1 == 4) {offset_mult_pos = 1.01; offset_mult_neg = 0.99} else {offset_mult_pos = 1.03; offset_mult_neg = 0.97}
          text(x = (seq(as.numeric(year0),(as.numeric(year0)+post_period_length))+c(-0.03,rep(1,post_period_length-1),0.03))[j-1], y=graph_data_table[orders[i,j],j]*c(offset_mult_pos,1,offset_mult_neg)[i],labels = paste0("$",graph_data_table[orders[i,j],j]))
        }
      }
    }
  }
  dev.off()
  # Now need to make the flextable from graph_data_table
  for (i in 1:3){for(j in 2:3){graph_data_table[i,j] = paste("$",graph_data_table[i,j],sep="")}}
  graph_data_flextable <- flextable::flextable(as.data.frame(graph_data_table))
  graph_data_flextable <- flextable::bold(graph_data_flextable,
                                          j = 1, part = "body")
  graph_data_flextable <- flextable::bg(graph_data_flextable, i = 1,
                                        bg = "#66478F", part = "header")
  graph_data_flextable <- flextable::color(graph_data_flextable,
                                           i = 1, color = "white", part = "header")
  graph_data_flextable <- flextable::width(graph_data_flextable,
                                           j = 1, width = 1.5)
  graph_data_flextable <- flextable::font(graph_data_flextable,
                                          fontname = "Century Gothic", part = "all")
  graph_data_flextable <- flextable::align(graph_data_flextable,
                                           align = c("center"),part="all")
  graph_data_flextable <- flextable::border_outer(graph_data_flextable,
                                                  border = officer::fp_border(color = "black", style = "solid",
                                                                              width = 2), part = "all")
  graph_data_flextable <- flextable::border_inner(graph_data_flextable,
                                                  border = officer::fp_border(color = "black", style = "solid",
                                                                              width = 1), part = "body")
  return_list = list(roi_graph,graph_data_flextable); names(return_list) = c("roi_graph","graph_data_flextable")
  return(return_list)
}
