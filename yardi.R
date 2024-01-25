# Load necessary libraries with pacman
if (!require(pacman)) install.packages('pacman')
pacman::p_load(magrittr, dplyr, readxl, janitor, stringr, parsedate, readr)

# Function to process data
process_data <- function(path) {
  
  # Read data from Excel
  data <- read_excel(path = path, skip = 4, sheet = "Sheet2", .name_repair = "universal")
  
  # Clean column names and select necessary columns
  cleaned_data <- data %>%
    rename_all(~ str_remove_all(., "[\r\n]") %>%
                 str_replace_all(., "[^[:alnum:]]", "") %>%
                 str_to_upper(.)) %>%
    mutate_all(as.character) %>%
    select(UNIT, UNITSQFT, UNITTYPE, PRIVACYLEVEL, RESIDENT, RESIDENTBIRTHDATE, RESIDENTMOVEINDATE, CARELEVEL, UNITMARKETRATE, RATETYPE, 31, RESIDENTDEPOSIT, OTHERCHARGESMONTHLY) %>%
    filter(RESIDENT != "NA", PRIVACYLEVEL != "NA") %>%
    tidyr::fill(UNIT, UNITSQFT, UNITTYPE) %>%
    set_names(c("Unit", "SQFT", "RoomType", "PrivacyLevel", "ResiName", "ResiBDate", "MIDate", "Product", "MarketRate", "RateType", "RentalIncome", "Deposit", "CareIncome"))
  
  # Convert data types and clean up further
  cleaned_data <- cleaned_data %>%
    mutate(across(c(ResiBDate, MIDate), as.Date, format = "%m/%d/%Y")) %>%
    mutate(across(c(MarketRate, RentalIncome, Deposit, CareIncome), parse_number)) %>%
    mutate(across(c(Unit, SQFT), as.double)) %>%
    mutate(Occupied = if_else(is.na(ResiName) | ResiName == "*Vacant", "Vacant", "Occupied")) %>%
    mutate(Product = case_when(Product %in% c("IL", "AL") ~ "AL",
                               Product == "EAL" ~ "EAL",
                               Product == "MC" ~ "MC"))
  
  # Add name
  cleaned_data$Name <- "Yardi"
  
  return(cleaned_data)
}