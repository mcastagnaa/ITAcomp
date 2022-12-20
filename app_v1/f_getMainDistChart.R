

f_getMainDistChart <- function(distMedioComp, Frames, refAMFilter, AUMFilter) {
  distMedioComp %>%
    filter(Frame %in% Frames) %>%
    group_by(AssetClass, Date, Frame) %>%
    summarise(AvgDist = mean(Dist)) %>%
    ggplot(aes(x = Date, y = AvgDist)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_point() +
    facet_grid(Frame ~ AssetClass, scales = "free_y") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_x_date(breaks = "1 month", date_labels = "%h-%y") +
    #scale_y_continuous(limit = c(-100, 100)) +
    labs(title = paste(refAMFilter, "vs. competitors - mean by Asset Class"), 
         subtitle = paste("Competitors percentiles less", refAMFilter, "percentile by PG.\nFilter AUM:", AUMFilter), 
         caption = "Positive = reference better than competitors",
         x = "", y = "")
}