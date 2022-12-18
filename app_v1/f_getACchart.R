f_getACchart <- function(fullStats, AUMinput, refAM, compAM, MGF) {
  frameFilter = 'Net22J'
  ChartGroups <- c("H1", "Q3", "Q4")
  
  AMs <- c(refAM, compAM)
  
  sbtle <- paste0("Filters: AUM greater than €", AUMinput, "mn, MGF is ", 
                  case_when(MGF ~ "in Mediolanum",
                            !("MGF" %in% AMs) ~ "excluded",
                            TRUE ~ "a competitor"))
  
  fullStats %>%
    filter(Group %in% ChartGroups) %>%
    mutate(linesize = ifelse(AM == refAM, "1", "0.5"))  %>%
    ggplot(aes(x = Date, y = Perc, color = AM, linewidth = linesize)) +
    geom_line() +
    geom_point() +
    geom_text_repel(aes(x = Date, y = Perc, label = No), size = 2)+
    facet_grid(Group ~ Frame) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_discrete_manual("linewidth", values = c(0.2, 1), guide = "none") +
    scale_y_continuous(label= scales::percent, limit = c(0,1)) +
    scale_x_date(breaks = "1 month", date_labels = "%h-%y") +
    labs(title = frameFilter,
         subtitle = sbtle,
         x = "", y = "")
}

