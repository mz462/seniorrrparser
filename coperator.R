# Load libraries
if (!require(pacman)) install.packages("pacman")
pacman::p_load(magrittr, dplyr, readxl, janitor, stringr, parsedate, readr, rstudioapi)

# Define function
res <- function(path = path) {
  
  # Read data
  LivingCare <- read_excel(path = path, skip = 4, sheet = "Sheet2", .name_repair = "universal")
  
  # Clean data
  CLivingCare <- LivingCare %>% 
    rename_all(~ str_remove_all(., pattern = "[\r\n]") %>%
                 str_replace_all(., "[^[:alnum:]]", "") %>%
                 str_to_upper(.)) %>% 
    mutate_all(as.character) %>% 
    select(UNIT, UNITSQFT, UNITTYPE, PRIVACYLEVEL, RESIDENT, RESIDENTBIRTHDATE, RESIDENTMOVEINDATE, CARELEVEL, UNITMARKETRATE, RATETYPE, 31, RESIDENTDEPOSIT, OTHERCHARGESMONTHLY) %>%
    filter(RESIDENT != "NA", PRIVACYLEVEL != "NA") %>%
    tidyr::fill(UNIT, UNITSQFT, UNITTYPE) %>% 
    set_names(c("Unit", "SQFT", "RoomType", "PrivacyLevel", "ResiName", "ResiBDate", "MIDate", "Product", "MarketRate", "RateType", "RentalIncome", "Deposit", "CareIncome"))
  
  # Reclass and further cleanup
  CLivingCare <- CLivingCare %>% 
    mutate(across(c(ResiBDate, MIDate), as.Date, format = "%m/%d/%Y")) %>% 
    mutate(across(c(MarketRate, RentalIncome, Deposit, CareIncome), parse_number)) %>% 
    mutate(across(c(Unit, SQFT), ~ as.character(as.double(.)))) %>% 
    mutate(Occupied = if_else(is.na(ResiName) | ResiName == "*Vacant", "Vacant", "Occupied")) %>% 
    mutate(Product = if_else(Product %in% c("IL", "AL", "EAL", "MC"), Product, NA_character_))
  
  # Add name
  CLivingCare$Name <- "LivingCare"
  
  # Return result
  CLivingCare
}