library(dplyr)
library(tidyr)
library(janitor)
library(openxlsx)
library(ggplot2)
library(ggrepel)
library(plotly)
library(gganimate)
library(kableExtra)

rm(list = ls())
options(dplyr.summarise.inform = FALSE)

### SETUP ######################################
dateFilter <- as.Date("2022-11-30")
frameFilter <- "Net22J"
refAMFilter <- "Mediolanum"

MGFisMedio <- FALSE

AMFilter <- unique(
  c(refAMFilter,
    "Mediolanum",
    "Generali",
    "Fineco",
    "Azimut",
    "MGF",
    "Fideuram")
)
  
AUMFilter <- 100
ACFilter <- "Fixed Income"
ChartGroups <- c("H1", "Q3", "Q4")
################################################
dataset <- read.xlsx("ITApg_dataset.xlsx", detectDates = T) %>%
  mutate(PeerGroup = gsub("EAA Fund ", "", PeerGroup),
         AM = gsub(" .*", "", AM),
         Date = as.Date(format(Date, "%Y-%m-01"))) %>%
  filter(AM %in% AMFilter,
         AUM >= AUMFilter) %>%
  mutate(AM = case_when(MGFisMedio & AM == "MGF" ~"Mediolanum",
                        TRUE ~ AM)) %>%
  rename(Net22J = Net3M, Grs22J = Grs3M) %>%
  mutate_at(vars(starts_with("Net"), starts_with("Grs")), as.numeric)

### GROUPS BY AC and PG #######################
QuartHalves <- dataset %>%
  pivot_longer(-c(Date, AM, ISIN, Fondo, PeerGroup, AssetClass, AUM), names_to = "Frame", values_to = "Percentile") %>%
  filter(!is.na(Percentile)) %>%
  mutate(Quartile = cut(Percentile, breaks = c(0, 25, 50, 75, 100), right = T, labels = c("Q1", "Q2", "Q3", "Q4")),
         Halves = cut(Percentile, breaks = c(0, 50, 100), right = T, labels = c("H1", "H2")),
         Frame = factor(Frame, levels = c("Net22J", "NetYtD", "Net1y", "Net3y", "Net5y",
                                          "Grs22J", "GrsYtD", "Grs1y", "Grs3y", "Grs5y"))) 

fullStats <- rbind(QuartHalves %>%
                     group_by(Date, Frame, AM, Group = Quartile) %>%
                     summarise(No = n()) %>%
                     mutate(Perc = No/sum(No)),
                   QuartHalves %>%
                     group_by(Date, Frame, AM, Group = Halves) %>%
                     summarise(No = n()) %>%
                     mutate(Perc = No/sum(No))) %>%
  mutate(Group = factor(Group, levels = c("Q1", "Q2", "H1", "Q3", "Q4", "H2"))) %>%
  #complete(Group, fill= list(No = 0, Perc = 0)) %>%
  arrange(Date, Frame, AM, Group)

AMstats <- rbind(QuartHalves %>%
                 group_by(Date, Frame, AssetClass, AM, Group = Quartile) %>%
                 summarise(No = n()) %>%
                 mutate(Perc = No/sum(No)),
               QuartHalves %>%
                 group_by(Date, Frame, AssetClass, AM, Group = Halves) %>%
                 summarise(No = n()) %>%
                 mutate(Perc = No/sum(No))) %>%
  mutate(Group = factor(Group, levels = c("Q1", "Q2", "H1", "Q3", "Q4", "H2"))) %>%
  #complete(Group, fill= list(No = 0, Perc = 0)) %>%
  arrange(Date, AssetClass, Frame, AM, Group)

PGstats <- rbind(QuartHalves %>%
                   group_by(Date, Frame, AssetClass, PeerGroup, AM, Group = Quartile) %>%
                   summarise(No = n()) %>%
                   mutate(Perc = No/sum(No)),
                 QuartHalves %>%
                   group_by(Date, Frame, AssetClass, PeerGroup, AM, Group = Halves) %>%
                   summarise(No = n()) %>%
                   mutate(Perc = No/sum(No))) %>%
  mutate(Group = factor(Group, levels = c("Q1", "Q2", "H1", "Q3", "Q4", "H2"))) %>%
  group_by(Frame, Date, AssetClass, PeerGroup) %>%
  mutate(NoAM = n_distinct(AM)) %>%
  filter(NoAM > 1) %>%
  select(-NoAM) %>%
  #complete(Group, fill= list(No = 0, Perc = 0)) %>%
  arrange(Date, AssetClass, PeerGroup, Frame, AM, Group)

AMtable <- AMstats %>%
  mutate(Label = paste(No, scales::label_percent()(Perc), sep = "/")) %>%
  select(-c(No, Perc)) %>%
  pivot_wider(names_from = Group, values_from = Label, names_sort = T) 

options(knitr.kable.NA = '')
AMtable %>%
  ungroup() %>%
  filter(Date == "2022-11-01", Frame == "Net22J") %>%
  select(-c(Date, Frame)) %>%
  kable(striped = T, condensed = T, align = "r", caption = "Table by Asset Class") %>%
  kable_styling(latex_option = c("hold_position"), font_size = 9, full_width = F) %>%
  column_spec(1, bold = T) %>%
  collapse_rows(1)
  
  
PGtable <- PGstats %>%
  mutate(Label = paste(No, scales::label_percent()(Perc), sep = "/")) %>%
  select(-c(No, Perc)) %>%
  pivot_wider(names_from = Group, values_from = Label, names_sort = T) 


PGtable %>%
  ungroup() %>%
  filter(AssetClass == "Fixed Income", Date == "2022-11-01", Frame == "Net22J") %>%
  select(-c(AssetClass, Date, Frame)) %>%
  kable(striped = T, condensed = T, align = "r", caption = "Table by Peer Group", linesep = "") %>%
  kable_styling(font_size = 9, full_width = F) %>%
  column_spec(1, bold = T) %>%
  collapse_rows(1) %>%
  row_spec(1:41,
           extra_css = "padding: 1px")

sbtle <- paste0("Filters: AUM greater than €", AUMFilter, "mn, MGF is ", 
                case_when(!("MGF" %in% AMFilter) ~ "OUT",
                          MGFisMedio ~ "in Mediolanum",
                          TRUE ~ "IN"))

fullStats %>%
  filter(Group %in% ChartGroups) %>%
  mutate(linesize = ifelse(AM == "Mediolanum", "1", "0.5"))  %>%
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


AMstats %>%
  filter(Frame == frameFilter,
         Group %in% ChartGroups) %>%
  mutate(linesize = ifelse(AM == "Mediolanum", "1", "0.5"))  %>%
  ggplot(aes(x = Date, y = Perc, color = AM, linewidth = linesize)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(x = Date, y = Perc, label = No), size = 2)+
  facet_grid(Group ~ AssetClass) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_discrete_manual("linewidth", values = c(0.2, 1), guide = "none") +
  scale_y_continuous(label= scales::percent, limit = c(0,1)) +
  scale_x_date(breaks = "1 month", date_labels = "%h-%y") +
  labs(title = frameFilter,
       subtitle = sbtle,
       x = "", y = "")
         

PGstats %>%
  filter(Frame == frameFilter,
         Group %in% ChartGroups,
         AssetClass == ACFilter) %>%
  mutate(linesize = ifelse(AM == "Mediolanum", "1", "0.5"))  %>%
  ggplot(aes(x = Date, y = Perc, color = AM, linewidth = linesize)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(x = Date, y = Perc, label = No), size = 2, max.overlaps = 15)+
  facet_grid(Group ~ PeerGroup) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="bottom") +
  scale_discrete_manual("linewidth", values = c(0.2, 1), guide = "none") +
  scale_y_continuous(label= scales::percent) +
  scale_x_date(breaks = "1 month", date_labels = "%h-%y") +
  labs(title = frameFilter,
       subtitle = paste0("Filter: AUM greater than €", AUMFilter, "mn"),
       x = "", y = "")


### Distance to competitors ####################
distMedioComp <- dataset %>%
  #filter(Date == "2022-09-01", PeerGroup == "EUR Aggressive Allocation - Global") %>%
  pivot_longer(-c(Date, AM, ISIN, Fondo, PeerGroup, AssetClass, AUM), names_to = "Frame", values_to = "Percentile") %>%
  #filter(Frame == 'Net22J') %>%
  select(AssetClass, PeerGroup, Date, AM, Frame, Percentile) %>%
  mutate(Side = ifelse(AM == refAMFilter, "Main", "Comp")) %>%
  group_by(AssetClass, PeerGroup, Date, Side, Frame) %>%
  summarise(AvgRank = mean(Percentile, na.rm = T),
            Count = n()) %>%
  pivot_wider(names_from = Side, values_from = c(AvgRank, Count)) %>%
  mutate(Dist = AvgRank_Comp-AvgRank_Main,
         Frame = factor(Frame, levels = c("Net22J", "NetYtD", "Net1y", "Net3y", "Net5y",
                                          "Grs22J", "GrsYtD", "Grs1y", "Grs3y", "Grs5y"))) %>%
  filter(!is.na(Dist)) %>%
  select(-c(AvgRank_Main, AvgRank_Comp))


distMedioComp %>%
  filter(Frame %in% c("Net22J", "Grs22J")) %>%
  group_by(AssetClass, Date, Frame) %>%
  summarise(AvgDist = mean(Dist)) %>%
  ggplot(aes(x = Date, y = AvgDist)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_point() +
  #geom_jitter(width = 10) +
  facet_grid(Frame ~ AssetClass, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_date(breaks = "1 month", date_labels = "%h-%y") +
  #scale_y_continuous(limit = c(-100, 100)) +
  labs(title = paste(refAMFilter, "vs. competitors - mean by Asset Class"), 
       subtitle = paste("Competitors percentiles less", refAMFilter, "percentile by PG.\nFilter AUM:", AUMFilter), 
       caption = "Positive = good",
       x = "", y = "")

p <- distMedioComp %>%
  filter(AssetClass == "Fixed Income",
         Frame == "Net22J") %>%
  #ggplot(aes(x = Date, y = Dist, text = paste("Ref/Comp:", PeerGroup, paste(Count_Main, Count_Comp, sep = "/")), color = AssetClass)) +
  ggplot(aes(x = Date, y = Dist, label = paste(PeerGroup, paste(Count_Main, Count_Comp, sep = "/")), color = AssetClass)) +
  geom_hline(yintercept = 0, color = "black") +
  #geom_line() +
  geom_text_repel(color = "dark grey", size = 2) +
  geom_jitter(width = 3, color = "black") +
  facet_grid(Frame ~ .) +
  theme_bw() +
  scale_x_date(breaks = "1 month", date_labels = "%h-%y") +
  #scale_y_continuous(limit = c(-100, 100)) +
  labs(title = paste(refAMFilter, "vs. competitors"), 
       #subtitle = paste("Competitors percentiles less", refAMFilter, "percentile by PG"), 
       caption = "Positive = good",
       x = "", y = "") +
  transition_time(Date) #+
  #labs(subtitle = "Date: {frame_time}") +
  #shadow_wake(wake_length = 0.1)

animate(p, renderer = gifski_renderer(loop = T), height = 900, width = 1200)
anim_save("anim.gif")

distMedioComp %>%
  filter(AssetClass == "MultiAsset",
         Frame == "Net22J") %>%
  ggplot(aes(x = Date, y = Dist, text = paste("Ref/Comp:", PeerGroup, paste(Count_Main, Count_Comp, sep = "/")), color = AssetClass)) +
  geom_hline(yintercept = 0, color = "black") +
  #geom_point() +
  geom_jitter(width = 5) +
  facet_grid(Frame ~ .) +
  theme_bw() +
  scale_x_date(breaks = "1 month", date_labels = "%h-%y") +
  scale_y_continuous(limit = c(-100, 100)) +
  labs(title = paste(refAMFilter, "vs. competitors"), 
       subtitle = paste("Competitors percentiles less", refAMFilter, "percentile by PG"), 
       caption = "Positive = good",
       x = "", y = "")

ggplotly(p, tooltip = c("text"))
