f_getACchart_ACGroup <- function(fullStats, sbtle, selFrame, selGroups, refAM) {
  
  fullStats %>%
    filter(Frame == selFrame,
           Group %in% selGroups) %>%
    mutate(linesize = ifelse(AM == refAM, "1", "0.5"))  %>%
    ggplot(aes(x = Date, y = Perc, color = AM, linewidth = linesize)) +
    geom_line() +
    geom_point() +
    geom_text_repel(aes(x = Date, y = Perc, label = No), size = 3)+
    facet_grid(Group ~ AssetClass) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_discrete_manual("linewidth", values = c(0.2, 1), guide = "none") +
    scale_y_continuous(label= scales::percent, limit = c(0,1)) +
    scale_x_date(breaks = "1 month", date_labels = "%h-%y") +
    labs(title = "",
         subtitle = sbtle,
         x = "", y = "")
}

