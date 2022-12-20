

f_getPGchart_PGGroup <- function(fullStats, sbtle, selFrame, selGroups, refAM, selPG) {
  fullStats %>%
    filter(Frame == selFrame,
           Group %in% selGroups,
           PeerGroup %in% selPG) %>%
    mutate(linesize = ifelse(AM == refAM, "1", "0.5"))  %>%
    ggplot(aes(x = Date, y = Perc, color = AM, linewidth = linesize)) +
    geom_line() +
    geom_point() +
    geom_text_repel(aes(x = Date, y = Perc, label = No), size = 4, max.overlaps = 15)+
    facet_grid(Group ~ PeerGroup) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="bottom") +
    scale_discrete_manual("linewidth", values = c(0.2, 1), guide = "none") +
    scale_y_continuous(label= scales::percent) +
    scale_x_date(breaks = "1 month", date_labels = "%h-%y") +
    labs(title = "",
         subtitle = sbtle,
         x = "", y = "")
  
}

