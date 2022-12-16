pwd <- read.csv("pwd.csv") 

user_base <- tibble::tibble(
  user = pwd$user,
  password = sapply(pwd$password, sodium::password_store),
  permissions = pwd$permission,
  name = pwd$name
)

dataset <- read.xlsx("ITApg_dataset.xlsx", detectDates = T) %>%
  mutate(PeerGroup = gsub("EAA Fund ", "", PeerGroup),
         AM = gsub(" .*", "", AM),
         Date = as.Date(format(Date, "%Y-%m-01")))

rm(pwd)



