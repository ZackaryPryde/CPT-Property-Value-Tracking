# Packages, Libraries, Dependencies, options, ...
library(pacman)
p_load(rvest, dplyr, tidyr, sp, purrr, readr)

# Scraping PROPERTIES FOR SALE  ----

page1 = read_html("https://www.property24.com/for-sale/advanced-search/results?sp=s%3d10158%2c10157%2c8683%2c8677%2c9143%2c8682%2c8679%2c11741%2c14225%2c8661%2c10102%2c14224%2c8669%2c10094%2c10109%2c10195%2c10170%2c10207%2c9040%2c9034%2c10213%2c9036%2c9025%2c9039%2c9067%2c10174%2c11740%2c10178%2c9044%2c10203%2c10189%2c11742%2c10052%2c10164%2c10161%2c11017%2c9169%2c11018%2c11022%2c11021%2c11016%2c11013%2c11015%2c11014%2c11012%2c9145%2c9163%2c9149%2c9155%2c9138%2c9136%2c9141%2c10166%2c9166%2c16541%2c10163%2c9118%2c10124%2c10144%2c10145%2c12942%2c10123%2c10131%2c10146%2c10130%2c10137%2c32751%2c33119%2c16665%2c16664%2c10151%2c10129%2c10134%2c10133%2c10135%2c10132%2c10120%2c16259%2c10149%2c16694%2c10093%2c10055%2c10150%2c10121%2c10128%2c10139%2c10152%2c10126%2c10053%2c10122%2c10119%2c32925%2c10153%2c15623%2c16185%2c9083%2c16186%2c17652%2c16392%2c8159%2c9088%2c17526%2c16187%2c16016%2c9123%2c9073%2c16699%2c10947%2c9108%2c9094%2c9085%2c9107%2c10603%2c9062%2c17448%2c15627%2c15626%2c33126%2c9090%2c10609%2c9082%2c16223%2c16222%2c10997%2c13916%2c9068%2c10487%2c17447%2c9103%2c9101%2c9106%2c15622%2c9104%2c9095%2c16197%2c17446%2c7851%2c9102%2c9097%2c16194%2c9091%26so%3dPriceLow&PropertyCategory=House%2cApartmentOrFlat%2cTownhouse%2cCommercial#SortOrder")
nresults <- as.numeric(page1 %>% 
                         html_nodes(".pagination li:nth-child(8) a") %>% 
                         html_text())

y = data.frame()

for (page_result in seq(from = 1, to = nresults, by = 1)) {
  listings = paste0("https://www.property24.com/for-sale/advanced-search/results/p",page_result ,"?sp=s%3d10158%2c10157%2c8683%2c8677%2c9143%2c8682%2c8679%2c11741%2c14225%2c8661%2c10102%2c14224%2c8669%2c10094%2c10109%2c10195%2c10170%2c10207%2c9040%2c9034%2c10213%2c9036%2c9025%2c9039%2c9067%2c10174%2c11740%2c10178%2c9044%2c10203%2c10189%2c11742%2c10052%2c10164%2c10161%2c11017%2c9169%2c11018%2c11022%2c11021%2c11016%2c11013%2c11015%2c11014%2c11012%2c9145%2c9163%2c9149%2c9155%2c9138%2c9136%2c9141%2c10166%2c9166%2c16541%2c10163%2c9118%2c10124%2c10144%2c10145%2c12942%2c10123%2c10131%2c10146%2c10130%2c10137%2c32751%2c33119%2c16665%2c16664%2c10151%2c10129%2c10134%2c10133%2c10135%2c10132%2c10120%2c16259%2c10149%2c16694%2c10093%2c10055%2c10150%2c10121%2c10128%2c10139%2c10152%2c10126%2c10053%2c10122%2c10119%2c32925%2c10153%2c15623%2c16185%2c9083%2c16186%2c17652%2c16392%2c8159%2c9088%2c17526%2c16187%2c16016%2c9123%2c9073%2c16699%2c10947%2c9108%2c9094%2c9085%2c9107%2c10603%2c9062%2c17448%2c15627%2c15626%2c33126%2c9090%2c10609%2c9082%2c16223%2c16222%2c10997%2c13916%2c9068%2c10487%2c17447%2c9103%2c9101%2c9106%2c15622%2c9104%2c9095%2c16197%2c17446%2c7851%2c9102%2c9097%2c16194%2c9091%26so%3dPriceLow&PropertyCategory=House%2cApartmentOrFlat%2cTownhouse%2cCommercial#SortOrder") %>%
    read_html()
  
  # Clear the tibble object which stores the data 
  x = tibble()
  
  # Pipe each scraped row of data into the tibble object
  x = listings %>% 
    html_elements(".p24_regularTile .p24_content") %>%
    map_dfr(~ tibble(
      price = .x %>%
        html_element(".p24_price") %>%
        html_text2(),
      area = .x %>%
        html_element(".p24_location") %>%
        html_text2(),
      size = .x %>%
        html_element(".p24_size span") %>%
        html_text2()))
  
  # Convert the tibble object to a dataframe
  x = as.data.frame(x)
  
  # Perform some data-cleaning operations
  x$price <- gsub(" ","", x$price)
  x$price <- gsub("\n","", x$price)
  x$price <- gsub("\r","", x$price)
  x$price <- gsub("R","", x$price)
  x$size <- gsub(" m²","", x$size)
  x$size <- gsub(" ","", x$size)
  x
  
  # Assign correct schema
  x$price = as.numeric(x$price)
  x$size = as.numeric(x$size)
  
  # Working out a general bond repayment
  # Payment = Loan Amount * (Monthly Interest Rate) / (1 - (1 + Monthly Interest Rate)^(-Number of Months))
  # Monthly Interest Rate = Annual Interest Rate / 12 (expressed as a decimal)
  # Number of Months = Loan Term in Years * 12
  prime_lending_rate = 11.75
  Expected_Annual_interest_rate = prime_lending_rate + 1.5
  loan_period_years = 30
  Monthly_interest_rate = (Expected_Annual_interest_rate/100)/12
  loan_period_months = loan_period_years * 12
  x$Monthly_Payment = x$price * (Monthly_interest_rate) / (1- (1 + Monthly_interest_rate)^(-loan_period_months))
  
  # Create a date column and record the date
  x$Date_Of_Scrape = paste0(Sys.Date())
  
  # Tag these data as "For Sale"
  x$Type = "For Sale"
  
  y = rbind(y,x)
  
  print(paste("page:", page_result))
}

# Read in the master dataset
dat_forsale <- read_csv("Property24_COCT_Properties_For_Sale.csv")
#dat_forsale <- data.frame()

# Store the result in our master dataframe object
dat_forsale = rbind(dat_forsale, y)

# Write the result to a csv
write.csv(dat_forsale, paste0(getwd(),"/","Property24_COCT_Properties_For_Sale.csv"), row.names = FALSE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Scraping PROPERTIES FOR RENT ----

page1 = read_html("https://www.property24.com/to-rent/advanced-search/results?sp=s%3d10158%2c10157%2c8683%2c8677%2c9143%2c8682%2c8679%2c11741%2c14225%2c8661%2c10102%2c14224%2c8669%2c10094%2c10109%2c10195%2c10170%2c10207%2c9040%2c9034%2c10213%2c9036%2c9025%2c9039%2c9067%2c10174%2c11740%2c10178%2c9044%2c10203%2c10189%2c11742%2c10052%2c10164%2c10161%2c11017%2c9169%2c11018%2c11022%2c11021%2c11016%2c11013%2c11015%2c11014%2c11012%2c9145%2c9163%2c9149%2c9155%2c9138%2c9136%2c9141%2c10166%2c9166%2c16541%2c10163%2c9118%2c10124%2c10144%2c10145%2c12942%2c10123%2c10131%2c10146%2c10130%2c10137%2c32751%2c33119%2c16665%2c16664%2c10151%2c10129%2c10134%2c10133%2c10135%2c10132%2c10120%2c16259%2c10149%2c16694%2c10093%2c10055%2c10150%2c10121%2c10128%2c10139%2c10152%2c10126%2c10053%2c10122%2c10119%2c32925%2c10153%2c15623%2c16185%2c9083%2c16186%2c17652%2c16392%2c8159%2c9088%2c17526%2c16187%2c16016%2c9123%2c9073%2c16699%2c10947%2c9108%2c9094%2c9085%2c9107%2c10603%2c9062%2c17448%2c15627%2c15626%2c33126%2c9090%2c10609%2c9082%2c16223%2c16222%2c10997%2c13916%2c9068%2c10487%2c17447%2c9103%2c9101%2c9106%2c15622%2c9104%2c9095%2c16197%2c17446%2c7851%2c9102%2c9097%2c16194%2c9091%26so%3dPriceLow&PropertyCategory=House%2cApartmentOrFlat%2cTownhouse%2cCommercial#SortOrder")
nresults <- as.numeric(page1 %>% 
                         html_nodes(".pagination li:nth-child(8) a") %>% 
                         html_text())

y = data.frame()

for (page_result in seq(from = 1, to = nresults, by = 1)) {
  listings = paste0("https://www.property24.com/to-rent/advanced-search/results/p",page_result ,"?sp=s%3d10158%2c10157%2c8683%2c8677%2c9143%2c8682%2c8679%2c11741%2c14225%2c8661%2c10102%2c14224%2c8669%2c10094%2c10109%2c10195%2c10170%2c10207%2c9040%2c9034%2c10213%2c9036%2c9025%2c9039%2c9067%2c10174%2c11740%2c10178%2c9044%2c10203%2c10189%2c11742%2c10052%2c10164%2c10161%2c11017%2c9169%2c11018%2c11022%2c11021%2c11016%2c11013%2c11015%2c11014%2c11012%2c9145%2c9163%2c9149%2c9155%2c9138%2c9136%2c9141%2c10166%2c9166%2c16541%2c10163%2c9118%2c10124%2c10144%2c10145%2c12942%2c10123%2c10131%2c10146%2c10130%2c10137%2c32751%2c33119%2c16665%2c16664%2c10151%2c10129%2c10134%2c10133%2c10135%2c10132%2c10120%2c16259%2c10149%2c16694%2c10093%2c10055%2c10150%2c10121%2c10128%2c10139%2c10152%2c10126%2c10053%2c10122%2c10119%2c32925%2c10153%2c15623%2c16185%2c9083%2c16186%2c17652%2c16392%2c8159%2c9088%2c17526%2c16187%2c16016%2c9123%2c9073%2c16699%2c10947%2c9108%2c9094%2c9085%2c9107%2c10603%2c9062%2c17448%2c15627%2c15626%2c33126%2c9090%2c10609%2c9082%2c16223%2c16222%2c10997%2c13916%2c9068%2c10487%2c17447%2c9103%2c9101%2c9106%2c15622%2c9104%2c9095%2c16197%2c17446%2c7851%2c9102%2c9097%2c16194%2c9091%26so%3dPriceLow&PropertyCategory=House%2cApartmentOrFlat%2cTownhouse%2cCommercial#SortOrder") %>%
    read_html()
  
  # Clear the tibble object which stores the data 
  x = tibble()
  
  # Pipe each scraped row of data into the tibble object
  x = listings %>% 
    html_elements(".p24_regularTile .p24_content") %>%
    map_dfr(~ tibble(
      price = .x %>%
        html_element(".p24_price") %>%
        html_text2(),
      area = .x %>%
        html_element(".p24_location") %>%
        html_text2(),
      size = .x %>%
        html_element(".p24_size span") %>%
        html_text2()))
  
  # Convert the tibble object to a dataframe
  x = as.data.frame(x)
  
  # Perform some data-cleaning operations
  x$price <- gsub(" ","", x$price)
  x$price <- gsub("\n","", x$price)
  x$price <- gsub("\r","", x$price)
  x$price <- gsub("R","", x$price)
  x$size <- gsub(" m²","", x$size)
  x$size <- gsub(" ","", x$size)
  
  x$size = as.numeric(x$size)
  
  # Assuming your dataframe is named rental_data
  x <- x %>%
    mutate(
      # Extract numeric part from the column and convert to numeric
      price_numeric = as.numeric(gsub("perm²", "", price)),
      
      # Multiply by another column's value
      Monthly_Payment = if_else(
        grepl("perm²", price), 
        price_numeric*(size), 
        as.numeric(price)
      ))
  
  x = x[,c(2,3,5)]
  
  x$Monthly_Payment = as.numeric(x$Monthly_Payment)
  
  # Create a date column and record the date
  x$Date_Of_Scrape = paste0(Sys.Date())
  
  # Tag these data as "For Rent"
  x$Type = "For Rent"
  
  y = rbind(y,x)
  
  print(paste("page:", page_result))
}

# Read in the master dataset
dat_forrent <- read_csv("Property24_COCT_Properties_For_Rent.csv")
#dat_forrent <- data.frame()

# Store the result in our master dataframe object
dat_forrent = rbind(dat_forrent, y)

# Write the result to a csv
write.csv(dat_forrent, paste0(getwd(),"/","Property24_COCT_Properties_For_Rent.csv"), row.names = FALSE)
