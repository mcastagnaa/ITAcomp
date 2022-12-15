library(dplyr)
library(tidyr)
library(janitor)
library(openxlsx)
library(odbc)
library(ggplot2)
library(plotly)

rm(list = ls())
options(dplyr.summarise.inform = FALSE)

### SETUP ###############################################
userName <- Sys.getenv("USERNAME")
CommonFields <- read.csv(paste0("C:/Users/", userName,
                                "/Mediolanum International Funds/Performance Team - Documents/Stuff/",
                                "CommonFields.csv"),
                         stringsAsFactors = F)

con <- dbConnect(odbc(), 
                 driver = "SQL Server", 
                 server = with(CommonFields, Value[Field == "SQLserverURL"]), 
                 database = "MED_EDW")

# userName <- Sys.getenv("USERNAME")
# mapsFolder <-  paste0("C:/Users/", userName, "/Mediolanum International Funds/Performance Team - Documents/Stuff/")  
# mapsFile <- paste0(mapsFolder,"mapTables.xlsx")
# rm(mapsFolder)

#########################################################
dataset <- read.xlsx("./ItaComp/ITApg_dataset.xlsx", detectDates = T) %>%
  mutate_at(vars(starts_with("AUM"), starts_with("Net"), starts_with("Grs")), as.numeric) %>%
  mutate(pShape = as.factor(ifelse(grepl("Mediolanum", AM), "Medio", "Comp")),
         pSize = as.factor(ifelse(grepl("Mediolanum", AM), "Medio", "Comp")))

CHECK <- dataset %>%
  select(PeerGroup, AssetClass) %>%
  distinct() %>%
  group_by(PeerGroup) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

if(max(CHECK$count) > 1) stop("Issues with Asset classes") else rm(CHECK)


refDate <- max(dataset$Date)

retSelect <- "Net1y"

distMedioComp <- dataset %>%
  #filter(Date == refDate) %>%
  select(PeerGroup, all_of(retSelect), pShape, Date) %>%
  group_by(pShape, PeerGroup, Date) %>%
  summarise(AvgRank = mean(get(retSelect), na.rm = T)) %>%
  pivot_wider(names_from = pShape, values_from = AvgRank) %>%
  mutate(Dist = Medio-Comp) %>%
  select(-c(Comp, Medio))

p <- dataset %>%
  #filter(Date == refDate) %>%
  select(AM, AUM, Fondo, PeerGroup, AssetClass, retSelect, pShape, pSize, Date, ISIN) %>%
  left_join(distMedioComp, by = c("PeerGroup", "Date")) %>%
  filter(!is.na(Dist)) %>%
  mutate(PeerGroup = forcats::fct_reorder(PeerGroup, Dist)) %>%
  ggplot(aes(x = PeerGroup, y = get(retSelect), color = AM, shape = pShape, size = AUM, 
             text = paste("Reference:",paste0(ISIN, "-", Fondo)))) +
  geom_hline(yintercept = 50, color = "orange") +
  geom_hline(yintercept = 75, color = "red") +
  geom_hline(yintercept = 25, color = "green") +
  geom_point() +
  scale_shape_manual(values = c(19,4)) +
  #scale_size_manual(values = c(1.5,2)) +
  #guides(size = "none") +
  guides(shape = "none") +
  facet_grid(AssetClass~Date, scales = "free_y") +#, nrow = 3, scales = "free_y") +
  coord_flip() +
  theme_bw() +
  scale_y_reverse(limits = c(100,1)) +
  labs(title = retSelect,
       #subtitle = refDate,
       x = "", y = "")

ggsave("./ItaComp/WorstCluster.png", device = "png", width = 1800, height = 1000, units = "px", dpi = 100)

ggplotly(p, tooltip = c("AM", "ISIN", "text", "AUM"))
