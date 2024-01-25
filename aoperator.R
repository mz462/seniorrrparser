# Load libraries outside the function
# Check if the 'pacman' package is installed. If not, install it.
if (!require(pacman)) install.packages("pacman")  
# Load the required packages using the 'p_load' function from the 'pacman' package.
pacman::p_load(magrittr, dplyr, readxl, janitor, stringr, parsedate, readr, rstudioapi)

# Define a function 'clean_data' to clean and transform the data
clean_data <- function(data) {
  # Use the pipe operator '%>%' to chain multiple operations on the 'data' dataframe.
  data %>%
    # Select specific columns from the dataframe.
    select(RoomNumber,RoomType,CareType,GrossUnitArea,PriceSpecial,BillingID,MoveInDT,CareRate,RentRate,MedLevel,MedRate) %>%
    # Filter out rows where 'RoomNumber' is "NA".
    filter(RoomNumber != "NA") %>%
    # Rename the columns of the dataframe.
    set_colnames(c("Unit","RoomType","Product","SQFT","MarketRate","ResiName","MIDate","CareIncome","RentalIncome","MedLevel","CareIncomeII")) %>%
    # Convert all columns to character type.
    mutate_all(as.character) %>%
    # Create new columns or modify existing ones.
    mutate(MIDate = parse_date(as.character(MIDate), format("%Y-%m-%d")),  # Convert 'MIDate' to date format.
           SQFT = as.double(SQFT),  # Convert 'SQFT' to double.
           MarketRate = parse_number(MarketRate),  # Extract numbers from 'MarketRate'.
           RentalIncome = parse_number(RentalIncome),  # Extract numbers from 'RentalIncome'.
           CareIncome = parse_number(CareIncome)+parse_number(CareIncomeII),  # Add 'CareIncome' and 'CareIncomeII' after extracting numbers.
           # If 'Product' is "LG", add 'RentalIncome' and 'CareIncome', otherwise keep 'RentalIncome' as is.
           RentalIncome = case_when(Product == "LG" ~ RentalIncome + CareIncome, TRUE ~ RentalIncome),
           # If 'Product' is "LG", set 'CareIncome' to 0, otherwise keep 'CareIncome' as is.
           CareIncome = case_when(Product == "LG" ~ 0, TRUE ~ CareIncome),
           # If 'Product' is "LG", set 'Product' to "MC", otherwise set 'Product' to "AL".
           Product = case_when(Product == "LG" ~ "MC", TRUE ~ "AL"),
           Unit = as.character(parse_number(Unit)),  # Extract numbers from 'Unit' and convert to character.
           # If 'ResiName' is NA or "Vacant*", set 'Occupied' to "Vacant", otherwise set 'Occupied' to "Occupied".
           Occupied = case_when(is.na(ResiName) ~ "Vacant", ResiName == "Vacant*" ~ "Vacant", TRUE ~ "Occupied")) %>%
    # Group the dataframe by 'Unit', 'RoomType', and 'Product'.
    group_by(Unit,RoomType,Product) %>%
    # Summarise each group by calculating the maximum of 'SQFT', the first non-NA 'ResiName', the first 'MIDate', the maximum of 'MarketRate', the sum of 'CareIncome', and the sum of 'RentalIncome'.
    summarise(SQFT = max(SQFT),
              ResiName = first(na.omit(ResiName)),
              MIDate = first(MIDate),
              MarketRate = max(MarketRate),
              CareIncome = sum(CareIncome),
              RentalIncome = sum(RentalIncome)) %>%
    # If 'ResiName' is NA, set 'Occupied' to "Vacant", otherwise set 'Occupied' to "Occupied".
    mutate(Occupied = case_when(is.na(ResiName) ~ "Vacant", TRUE ~ "Occupied"))
  
  # Rename the first column of 'cleaned_data' to "Unit".
  colnames(cleaned_data)[1] = "Unit"
  # Add a new column 'Name' to 'cleaned_data' and set all its values to "Westminster".
  cleaned_data$Name = "atria"
  
  # Return the cleaned and transformed data.
  return(cleaned_data)
}

# Define a function 'process_data' to read the data and call the 'clean_data' function.
process_data <- function(path){
  # Read the Excel file specified by 'path' into a dataframe 'data'.
  data = read_excel(path = path, skip = 0, sheet = "Sheet1",.name_repair = "universal")
  # Call the 'clean_data' function on 'data' and store the result in 'cleaned_data'.
  cleaned_data = clean_data(data)
  # Return the cleaned data.
  return(cleaned_data)
}