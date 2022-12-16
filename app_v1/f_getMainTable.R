AClass = "Equity"
thisDate = as.Date('2022-07-29')

f_getMainTable <- function(AClass, thisDate) {
  if(AClass == "Equity") {
    addCols <- EqFields
  } else if(AClass == "MultiAsset") {
      addCols <- c(EqFields, FIFields)
      } else addCols <- FIFields
  
  table <- dataScope %>%
    filter(AssetClass == AClass,
           (ClosedDate >= thisDate)|is.na(ClosedDate),
           Start_date < thisDate) %>%
    left_join(dataTop, by = c("ShortCode"="Name")) %>%
    filter(repDate == thisDate) %>%
    select(ShortCode, FundName, all_of(c(commonFields, addCols)))
  
  return(table)
}