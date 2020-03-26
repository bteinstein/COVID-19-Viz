# Scrapping data from Web About corona virus

## Trying various sources

# Github Repository of Johns Hopkins CSSE
# CSSE COVID-19 Dataset - Time Series Data
csse_raw_url <-  'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_'

confirm_c_df = readr::read_csv(paste0(csse_raw_url,'confirmed_global.csv')) # confirmed cases
deaths_df <-readr::read_csv(paste0(csse_raw_url,'deaths_global.csv')) # confirmed cases
recovered_df <-readr::read_csv(paste0(csse_raw_url,'recovered_global.csv')) # confirmed cases


head(confirm_c_df)

# Some data cleaning
confirm_c_df_c <-  confirm_c_df 