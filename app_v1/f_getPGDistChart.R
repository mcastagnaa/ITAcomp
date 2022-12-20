

f_getPGDistChart <- function(distMedioComp, selFrame, selPG, refAM, sbtle) {
  
  distMedioComp %>%
    filter(PeerGroup %in% selPG,
           Frame == selFrame) %>%
    ggplot(aes(x = Date, y = Dist, label = paste(PeerGroup, paste(Count_Main, Count_Comp, sep = "/")), color = AssetClass)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_text_repel(color = "dark grey", size = 4) +
    geom_jitter(width = 3, color = "black") +
    facet_grid(Frame ~ .) +
    theme_bw() +
    scale_x_date(breaks = "1 month", date_labels = "%h-%y") +
    #scale_y_continuous(limit = c(-100, 100)) +
    labs(title = paste(refAM, "vs. competitors - mean by Peer Group"), 
         subtitle = sbtle, 
         caption = "Positive = reference better than competitors",
         x = "", y = "")
  
}
