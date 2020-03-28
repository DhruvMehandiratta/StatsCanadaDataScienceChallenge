library(dplyr)
library(ggplot2)
library(reshape2)
library(Information)
library(DescTools)



## Loading the International Payments File
IP_DF <- read.csv("36100014.csv")

## Filtering and selecting only values of Total current Account Payments from the IP_DF data drame
IP_DF_filter <- select (IP_DF, ï..REF_DATE, VALUE) %>%
  filter(IP_DF$Receipts..payments.and.balances == "Payments", 
         IP_DF$Countries.or.regions == "All countries",
         IP_DF$Current.account.and.capital.account == "Total current account")

names(IP_DF_filter)[c(2)] <- c("InternatIonal_Payments")


## Loading the unempolyment rate file
UR_DF <- read.csv("14100020.csv")

## Filtering and selecting only values of Unmemployemnt Rate from the UR_DF data drame
UR_DF_filter <- select(UR_DF , ï..REF_DATE, VALUE ) %>%
  filter( UR_DF$GEO == "Canada" ,
          UR_DF$Labour.force.characteristics == "Unemployment rate" ,
          UR_DF$Educational.attainment == "Total, all education levels" ,
          UR_DF$Sex == "Both sexes" ,
          UR_DF$Age.group == "15 years and over")


names(UR_DF_filter)[c(2)] <- c("Unemployment_Rate")


## Loading the International Reserves file
International_Reserves <- read.csv("10100127.csv")

## Filtering and selecting only values for US Dollar reserves
USD_filter <- select(International_Reserves , ï..REF_DATE, VALUE ) %>%
  filter( International_Reserves$Type.of.reserve == "Convertible foreign currencies, United States dollars",
          International_Reserves$ï..REF_DATE %like% "%-01") %>%
  mutate (ï..REF_DATE = substr(ï..REF_DATE, 1,4))


names(USD_filter)[c(2)] <- c("US_Dollars")



## Filtering and selecting only values for Reserve position in the International Monetary Fund (IMF)
IMF_position <- select(International_Reserves , ï..REF_DATE, VALUE ) %>%
  filter( International_Reserves$Type.of.reserve == "Reserve position in the International Monetary Fund (IMF)",
          International_Reserves$ï..REF_DATE %like% "%-01") %>%
  mutate (ï..REF_DATE = substr(ï..REF_DATE, 1,4))

names(IMF_position)[c(2)] <- c("IMF_Position")


## Filtering and selecting only values for Total, Canada's official international reserves
IR_official <-select(International_Reserves , ï..REF_DATE, VALUE ) %>%
  filter( International_Reserves$Type.of.reserve == "Total, Canada's official international reserves",
          International_Reserves$ï..REF_DATE %like% "%-01") %>%
  mutate (ï..REF_DATE = substr(ï..REF_DATE, 1,4))
  
names(IR_official)[c(2)] <- c("International_Reserves")

  
## Loading the Investments file
Investments <- read.csv("36100009.csv")

## Filtering and selecting only values for Canadian direct investment abroad - Total Book Value
CDI <- select (Investments, ï..REF_DATE, VALUE) %>%
  filter(Investments$North.American.Industry.Classification.System..NAICS. == "Total, all industries", 
         Investments$Canadian.and.foreign.direct.investment == "Canadian direct investment abroad - Total Book Value",
         Investments$Countries.or.regions == "Total, all countries")

names(CDI)[c(2)] <- c("CDI_TBV")


## Filtering and selecting only values for Foreign direct investment in Canada - Total Book Value
FDI <- select (Investments, ï..REF_DATE, VALUE) %>%
  filter(Investments$North.American.Industry.Classification.System..NAICS. == "Total, all industries", 
         Investments$Canadian.and.foreign.direct.investment == "Foreign direct investment in Canada - Total Book Value",
         Investments$Countries.or.regions == "Total, all countries")

names(FDI)[c(2)] <- c("FDI_TBV")


## Loading the stock market file
STOCK <- read.csv ("10100125.csv")

## Filtering and selecting only values for For Toronto Stock Exchange Canadian Financial Index
Stock_Market_FI <-select(STOCK , ï..REF_DATE, VALUE ) %>%
  filter( STOCK$Toronto.Stock.Exchange.Statistics == "Standard and Poor's/Toronto Stock Exchange Canadian Financial Index",
          STOCK$ï..REF_DATE %like% "%-01") %>%
  mutate (ï..REF_DATE = substr(ï..REF_DATE, 1,4))


names(Stock_Market_FI)[c(2)] <- c("TSE_FI")



####################################  RETAIL ##########################
## Loading the Retail database
Retail_data <- read.csv("Retail.csv")



## Filtering and selecting only year and salary values
Retail_DF <- select (Retail_data, ï..REF_DATE, VALUE) %>% 
  filter (Retail_data$North.American.Industry.Classification.System..NAICS. == "Retail trade [44-45]", 
          Retail_data$GEO == "Canada", 
          Retail_data$Adjustments == "Unadjusted", 
          Retail_data$ï..REF_DATE %like% "%-01") %>%
  mutate (ï..REF_DATE = substr(ï..REF_DATE, 1,4))

Retail_DF <- mutate(Retail_DF, VALUE = VALUE / 1000)

names(Retail_DF)[c(2)] <- c("RETAIL")


####################################  TOURISM ##########################
## Loading the Tourism database
Tourism_data <- read.csv("Tourism.csv")

## Investigating about the structure of the file
str(Tourism_data)



## Filtering and selecting only year and salary values
Tourism_df <- select (Tourism_data, ï..REF_DATE, VALUE) %>% 
  filter (Tourism_data$Activities == "Tourism gross domestic product (GDP)", 
          Tourism_data$GEO == "Canada", 
          Tourism_data$Seasonal.adjustment == "Unadjusted", 
          Tourism_data$ï..REF_DATE %like% "%-01")%>%
  mutate (ï..REF_DATE = substr(ï..REF_DATE, 1,4))

names(Tourism_df)[c(2)] <- c("TOURISM")



##  --------------------------------- Salary file work ---------------------

## Loading the Salary file
Salary <- read.csv("Salary.csv")

## Investigating about the structure of the file
str(Salary)


## Filtering and selecting only year and salary values
Salary_filter <- select (Salary, ï..REF_DATE, VALUE) %>% 
  filter (Salary$Sector == "Wages and salaries", 
          Salary$GEO == "Canada", 
          Salary$Seasonal.adjustment == "Unadjusted", 
          Salary$ï..REF_DATE %like% "%-01") %>%
  mutate (ï..REF_DATE = substr(ï..REF_DATE, 1,4))

## rename to i..ï..REF_DATE to ï..REF_DATE
names(Salary_filter)[c(2)] <- c("Salary_Value")


##------------------ SCALING ------------------
#the value in Trade Data is in millions and Canada GDP is in millions while Salary is in Thousands.
#Coverting Salary into millions: -  (1 million = 1000 thousand)To get values in millions through out.

Salary_filter <- mutate(Salary_filter,Salary_Value = Salary_Value/1000)

## Verifying the filtered data
Salary_filter


##  --------------------------------- Trades file work ---------------------
## Loading the Trade file
Trade_unfiltered <- read.csv("Trade.csv")

## Investigating about the structure of the file
str(Trade_unfiltered)


## Filtering and selecting only year and trade values
Trade_df  <- select(Trade_unfiltered, ï..REF_DATE, Trade, VALUE) %>% 
  filter (Trade_unfiltered$Seasonal.adjustment == "Unadjusted", 
          Trade_unfiltered$Basis=="Customs", 
          Trade_unfiltered$North.American.Product.Classification.System..NAPCS. == "Total of all merchandise", 
          Trade_unfiltered$ï..REF_DATE %like% "%-01") %>%
  mutate (ï..REF_DATE = substr(ï..REF_DATE, 1,4))

Import_filter <- select(Trade_df, ï..REF_DATE, VALUE) %>% 
  filter(Trade_df$Trade == "Import")

names(Import_filter)[c(2)] <- c("Import")


Export_filter <- select(Trade_df, ï..REF_DATE, VALUE) %>% 
  filter(Trade_df$Trade == "Export")

names(Export_filter)[c(2)] <- c("Export")



##---------------------------------------------------------------------------------##
##                INFLATION RATE
##---------------------------------------------------------------------------------##

## Loading the Inflation data File
IN_DF <- read.csv("Inflation_data.csv")

## investing the structure of the Inflation data frame
str(IN_DF)

names(IN_DF)[c(1)] <- c("ï..REF_DATE")


##---------------------------------------------------------------------------------##
##             REAL ESTATE 
##---------------------------------------------------------------------------------##

## loading the real estate file
RE_DF <- read.csv("36100434.csv")

## investing the structure of the inflation data frame
str(RE_DF)

##Filtering and selecting only year and values from the RE_DF_filter where NAICS. == "Real estate [531]" 
RE_DF_filter <- select(RE_DF, ï..REF_DATE, VALUE) %>%
  filter( RE_DF$Seasonal.adjustment == "Seasonally adjusted at annual rates",
          RE_DF$North.American.Industry.Classification.System..NAICS. == "Real estate [531]",
          RE_DF$Prices == "2012 constant prices",
          RE_DF$ï..REF_DATE %like% "%-01") %>%
  mutate (ï..REF_DATE = substr(ï..REF_DATE, 1,4))

names(RE_DF_filter)[c(2)] <- c("Real_Estate")



##Filtering and selecting only year and values from the RE_DF_filter where NAICS. == "Real estate and rental and leasing [53]" 
RE_LR_DF_filter <- select(RE_DF, ï..REF_DATE, VALUE) %>%
  filter( RE_DF$Seasonal.adjustment == "Seasonally adjusted at annual rates",
          RE_DF$North.American.Industry.Classification.System..NAICS. == "Real estate and rental and leasing [53]",
          RE_DF$Prices == "2012 constant prices",
          RE_DF$ï..REF_DATE %like% "%-01") %>%
  mutate (ï..REF_DATE = substr(ï..REF_DATE, 1,4))

names(RE_LR_DF_filter)[c(2)] <- c("Real_Estate_Lease_Rental")


########################################


## Loading the Canada GDP file
Canada_GDP <- read.csv("canada_gdp.csv")

## Investigating about the structure of the file
str(Canada_GDP)

names(Canada_GDP)[c(1)] <- c("ï..REF_DATE")

#######################################################################
###################Creating Master Data file###########################
#######################################################################


IP_DF_filter
UR_DF_filter
USD_filter
IMF_position
IR_official
Stock_Market_FI
CDI
FDI
Retail_DF
Tourism_df
Salary_filter
Import_filter
Export_filter
RE_DF_filter
RE_LR_DF_filter

#### Merging all Data Frames with OUTER JOIN
#### SO that all data is retained
MASTER_DATA <- merge(Canada_GDP , IP_DF_filter, by ="ï..REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , UR_DF_filter, by ="ï..REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , USD_filter, by ="ï..REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , IMF_position, by ="ï..REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , IR_official, by ="ï..REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , Stock_Market_FI, by ="ï..REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , CDI, by ="ï..REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , FDI, by ="ï..REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , Retail_DF, by ="ï..REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , Tourism_df, by ="ï..REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , Salary_filter, by ="ï..REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , Import_filter, by ="ï..REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , Export_filter, by ="ï..REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , RE_DF_filter, by ="ï..REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , RE_LR_DF_filter, by ="ï..REF_DATE", all = TRUE)


str(MASTER_DATA)
MASTER_DATA

model1 <- lm(MASTER_DATA$Annual_GDP ~ MASTER_DATA$InternatIonal_Payments + MASTER_DATA$Unemployment_Rate + 
               MASTER_DATA$US_Dollars + MASTER_DATA$IMF_Position + MASTER_DATA$International_Reserves + 
               MASTER_DATA$TSE_FI + MASTER_DATA$CDI_TBV + MASTER_DATA$FDI_TBV + MASTER_DATA$RETAIL + 
               MASTER_DATA$TOURISM + MASTER_DATA$Salary_Value + MASTER_DATA$Import + MASTER_DATA$Export + 
               MASTER_DATA$Real_Estate + MASTER_DATA$Real_Estate_Lease_Rental)

model1 <- lm(Annual_GDP ~ InternatIonal_Payments + Unemployment_Rate + 
               US_Dollars + IMF_Position + International_Reserves + 
               TSE_FI + CDI_TBV + FDI_TBV + RETAIL + 
               TOURISM + Salary_Value + Import + Export + 
               Real_Estate + Real_Estate_Lease_Rental, data=MASTER_DATA)

summary(model1)



#new_data <- data.frame(InternatIonal_Payments=915377, Unemployment_Rate=5.8)
new.data <- data.frame(InternatIonal_Payments=915377, Unemployment_Rate=5.8, US_Dollars=50615, IMF_Position=2084,
                       International_Reserves=86748, TSE_FI=309.5, CDI_TBV=1288869, FDI_TBV=876856,RETAIL=41458.023,
                      TOURISM=7972, Salary_Value=77515.063, Import=43878.6, Export=45270, Real_Estate=229136, Real_Estate_Lease_Rental=242512)
predict(model1, newdata = new.data)


############################# irrelevant code below....ignore....################

## predicting the value of Annual_GDP for year 2018 on the basis of above model: -
MASTER_DATA
write.csv(MASTER_DATA, "MASTER_DATA.csv", row.names = FALSE)

#predict(model1, data.frame(MASTER_DATA$InternatIonal_Payments=915377, MASTER_DATA$Unemployment_Rate=5.8, 
 #                          MASTER_DATA$US_Dollars=50615))
#predict(model1, data.frame(MASTER_DATA$InternatIonal_Payments=915377))
#helper <- expand.grid(MASTER_DATA$InternatIonal_Payments==915377, MASTER_DATA$Unemployment_Rate=5.8)
#predict(model1,newdata = helper)
