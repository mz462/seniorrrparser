res <- function(path = path) {
  # Check if the 'pacman' package is installed. If not, install it.
  if (!require(pacman)) install.packages("pacman")
  # Load the necessary packages using the 'pacman' package
  pacman::p_load(magrittr, dplyr, readxl, janitor, stringr, parsedate, lubridate, readr, rstudioapi)
  
  # Read an Excel file from the provided path, skip the first 2 rows, and select the 'Report' sheet
  # Then, select specific columns and filter rows based on certain conditions
  # Rename the columns, convert date columns to Date type, convert character columns to double
  # Modify the 'RoomType' and 'ResiName' columns, convert the 'Unit' column to character
  # Group the data by 'Unit', 'RoomType', and 'Product'
  # Summarise the data by taking the max of 'SQFT' and 'MarketRate', the sum of 'ALCare', 'OtherIncome', and 'RentalIncome'
  # and the first non-NA value of 'ResiName', 'MIDate', and 'ResiBDate'
  # Create a new column 'Occupied' based on the 'ResiName' column, and modify the 'Product' and 'ResiBDate' columns
  CSLC <- read_excel(path = path, skip = 2, sheet = "Report", .name_repair = "universal") %>%
    select(Unit.., Unit.Style, SqFt, Product, Resident.Name, Resident...Birthdate, Start...Date, Market.Rate, Base.Rent, Care.Fees.1st.Pers, Ancillary.Rev) %>%
    filter_at(vars(Unit.., Base.Rent, Unit.Style), any_vars(. != "NA" & . != "WAITAL" & . != "WAITMC" & . != "retail")) %>%
    set_colnames(c("Unit", "RoomType", "SQFT", "Product", "ResiName", "ResiBDate", "MIDate", "MarketRate", "RentalIncome", "ALCare", "OtherIncome")) %>%
    mutate_at(vars(MIDate, ResiBDate), list(~as.Date(parse_number(.), origin = "1899-12-30"))) %>%
    mutate_if(is.character, as.double) %>%
    mutate(RoomType = case_when(str_detect(RoomType, "2bd|1bd|studio") ~ RoomType),
           ResiName = na_if(ResiName, "VACANT"),
           Unit = as.character(as.double(Unit))) %>%
    group_by(Unit, RoomType, Product) %>%
    summarise(across(c(SQFT, MarketRate), max, .names = "max_{.col}"),
              across(c(ALCare, OtherIncome, RentalIncome), sum, .names = "sum_{.col}"),
              across(c(ResiName, MIDate, ResiBDate), ~first(na.omit(.)), .names = "first_{.col}")) %>%
    mutate(Occupied = case_when(is.na(ResiName) ~ "Vacant",
                                ResiName == "Vacant*" ~ "Vacant",
                                TRUE ~ "Occupied"),
           Product = case_when(Product == "AL" ~ "AL",
                               Product == "SC" ~ "MC"),
           ResiBDate = case_when(ResiBDate >= ymd(19801231) ~ ResiBDate %m-% months(1200),
                                 TRUE ~ ResiBDate))
  
  # Add a new column 'Name' to the data frame and assign it the value 'SLC'
  CSLC$Name <- "SLC"
  
  # Return the data frame
  CSLC
}