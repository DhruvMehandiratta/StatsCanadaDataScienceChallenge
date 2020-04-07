library(dplyr)
library(ggplot2)
library(reshape2)
library(Information)
library(DescTools)
library(randomForest)
library(ggcorrplot)
library(FactoMineR)
library(rpart)
library(rpart.plot)
library(Metrics)
library(factoextra)


environment(select)

## Loading the International Payments File
IP_DF <- read.csv("36100014.csv")

## Filtering and selecting only values of Total current Account Payments from the IP_DF data drame
IP_DF_filter <- select (IP_DF, REF_DATE, VALUE) %>%
  filter(IP_DF$Receipts..payments.and.balances == "Payments", 
         IP_DF$Countries.or.regions == "All countries",
         IP_DF$Current.account.and.capital.account == "Total current account")

names(IP_DF_filter)[c(2)] <- c("InternatIonal_Payments")


## Loading the unempolyment rate file
UR_DF <- read.csv("14100020.csv")

## Filtering and selecting only values of Unmemployemnt Rate from the UR_DF data drame
UR_DF_filter <- select(UR_DF , REF_DATE, VALUE ) %>%
  filter( UR_DF$GEO == "Canada" ,
          UR_DF$Labour.force.characteristics == "Unemployment rate" ,
          UR_DF$Educational.attainment == "Total, all education levels" ,
          UR_DF$Sex == "Both sexes" ,
          UR_DF$Age.group == "15 years and over")


names(UR_DF_filter)[c(2)] <- c("Unemployment_Rate")


## Loading the International Reserves file
International_Reserves <- read.csv("10100127.csv")

## Filtering and selecting only values for US Dollar reserves
USD_filter <- select(International_Reserves , REF_DATE, VALUE ) %>%
  filter( International_Reserves$Type.of.reserve == "Convertible foreign currencies, United States dollars",
          International_Reserves$REF_DATE %like% "%-01") %>%
  mutate (REF_DATE = substr(REF_DATE, 1,4))


names(USD_filter)[c(2)] <- c("US_Dollars")



## Filtering and selecting only values for Reserve position in the International Monetary Fund (IMF)
IMF_position <- select(International_Reserves , REF_DATE, VALUE ) %>%
  filter( International_Reserves$Type.of.reserve == "Reserve position in the International Monetary Fund (IMF)",
          International_Reserves$REF_DATE %like% "%-01") %>%
  mutate (REF_DATE = substr(REF_DATE, 1,4))

names(IMF_position)[c(2)] <- c("IMF_Position")


## Filtering and selecting only values for Total, Canada's official international reserves
IR_official <-select(International_Reserves , REF_DATE, VALUE ) %>%
  filter( International_Reserves$Type.of.reserve == "Total, Canada's official international reserves",
          International_Reserves$REF_DATE %like% "%-01") %>%
  mutate (REF_DATE = substr(REF_DATE, 1,4))

names(IR_official)[c(2)] <- c("International_Reserves")


## Loading the Investments file
Investments <- read.csv("36100009.csv")

## Filtering and selecting only values for Canadian direct investment abroad - Total Book Value
CDI <- select (Investments, REF_DATE, VALUE) %>%
  filter(Investments$North.American.Industry.Classification.System..NAICS. == "Total, all industries", 
         Investments$Canadian.and.foreign.direct.investment == "Canadian direct investment abroad - Total Book Value",
         Investments$Countries.or.regions == "Total, all countries")

names(CDI)[c(2)] <- c("CDI_TBV")


## Filtering and selecting only values for Foreign direct investment in Canada - Total Book Value
FDI <- select (Investments, REF_DATE, VALUE) %>%
  filter(Investments$North.American.Industry.Classification.System..NAICS. == "Total, all industries", 
         Investments$Canadian.and.foreign.direct.investment == "Foreign direct investment in Canada - Total Book Value",
         Investments$Countries.or.regions == "Total, all countries")

names(FDI)[c(2)] <- c("FDI_TBV")


## Loading the stock market file
STOCK <- read.csv ("10100125.csv")

## Filtering and selecting only values for For Toronto Stock Exchange Canadian Financial Index
Stock_Market_FI <-select(STOCK , REF_DATE, VALUE ) %>%
  filter( STOCK$Toronto.Stock.Exchange.Statistics == "Standard and Poor's/Toronto Stock Exchange Canadian Financial Index",
          STOCK$REF_DATE %like% "%-01") %>%
  mutate (REF_DATE = substr(REF_DATE, 1,4))


names(Stock_Market_FI)[c(2)] <- c("TSE_FI")



####################################  RETAIL ##########################
## Loading the Retail database
Retail_data <- read.csv("Retail.csv")



## Filtering and selecting only year and salary values
Retail_DF <- select (Retail_data, REF_DATE, VALUE) %>% 
  filter (Retail_data$North.American.Industry.Classification.System..NAICS. == "Retail trade [44-45]", 
          Retail_data$GEO == "Canada", 
          Retail_data$Adjustments == "Unadjusted", 
          Retail_data$REF_DATE %like% "%-01") %>%
  mutate (REF_DATE = substr(REF_DATE, 1,4))

Retail_DF <- mutate(Retail_DF, VALUE = VALUE / 1000)

names(Retail_DF)[c(2)] <- c("RETAIL")


####################################  TOURISM ##########################
## Loading the Tourism database
Tourism_data <- read.csv("Tourism.csv")

## Investigating about the structure of the file
str(Tourism_data)



## Filtering and selecting only year and salary values
Tourism_df <- select (Tourism_data, REF_DATE, VALUE) %>% 
  filter (Tourism_data$Activities == "Tourism gross domestic product (GDP)", 
          Tourism_data$GEO == "Canada", 
          Tourism_data$Seasonal.adjustment == "Unadjusted", 
          Tourism_data$REF_DATE %like% "%-01")%>%
  mutate (REF_DATE = substr(REF_DATE, 1,4))

names(Tourism_df)[c(2)] <- c("TOURISM")



##  --------------------------------- Salary file work ---------------------

## Loading the Salary file
Salary <- read.csv("Salary.csv")

## Investigating about the structure of the file
str(Salary)


## Filtering and selecting only year and salary values
Salary_filter <- select (Salary, REF_DATE, VALUE) %>% 
  filter (Salary$Sector == "Wages and salaries", 
          Salary$GEO == "Canada", 
          Salary$Seasonal.adjustment == "Unadjusted", 
          Salary$REF_DATE %like% "%-01") %>%
  mutate (REF_DATE = substr(REF_DATE, 1,4))

## rename to i..REF_DATE to REF_DATE
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
Trade_df  <- select(Trade_unfiltered, REF_DATE, Trade, VALUE) %>% 
  filter (Trade_unfiltered$Seasonal.adjustment == "Unadjusted", 
          Trade_unfiltered$Basis=="Customs", 
          Trade_unfiltered$North.American.Product.Classification.System..NAPCS. == "Total of all merchandise", 
          Trade_unfiltered$REF_DATE %like% "%-01") %>%
  mutate (REF_DATE = substr(REF_DATE, 1,4))

Import_filter <- select(Trade_df, REF_DATE, VALUE) %>% 
  filter(Trade_df$Trade == "Import")

names(Import_filter)[c(2)] <- c("Import")


Export_filter <- select(Trade_df, REF_DATE, VALUE) %>% 
  filter(Trade_df$Trade == "Export")

names(Export_filter)[c(2)] <- c("Export")



##---------------------------------------------------------------------------------##
##                INFLATION RATE
##---------------------------------------------------------------------------------##

## Loading the Inflation data File
IN_DF <- read.csv("Inflation_data.csv")

## investing the structure of the Inflation data frame
str(IN_DF)

names(IN_DF)[c(1)] <- c("REF_DATE")


##---------------------------------------------------------------------------------##
##             REAL ESTATE 
##---------------------------------------------------------------------------------##

## loading the real estate file
RE_DF <- read.csv("36100434.csv")

## investing the structure of the inflation data frame
str(RE_DF)

##Filtering and selecting only year and values from the RE_DF_filter where NAICS. == "Real estate [531]" 
RE_DF_filter <- select(RE_DF, REF_DATE, VALUE) %>%
  filter( RE_DF$Seasonal.adjustment == "Seasonally adjusted at annual rates",
          RE_DF$North.American.Industry.Classification.System..NAICS. == "Real estate [531]",
          RE_DF$Prices == "2012 constant prices",
          RE_DF$REF_DATE %like% "%-01") %>%
  mutate (REF_DATE = substr(REF_DATE, 1,4))

names(RE_DF_filter)[c(2)] <- c("Real_Estate")



##Filtering and selecting only year and values from the RE_DF_filter where NAICS. == "Real estate and rental and leasing [53]" 
RE_LR_DF_filter <- select(RE_DF, REF_DATE, VALUE) %>%
  filter( RE_DF$Seasonal.adjustment == "Seasonally adjusted at annual rates",
          RE_DF$North.American.Industry.Classification.System..NAICS. == "Real estate and rental and leasing [53]",
          RE_DF$Prices == "2012 constant prices",
          RE_DF$REF_DATE %like% "%-01") %>%
  mutate (REF_DATE = substr(REF_DATE, 1,4))

names(RE_LR_DF_filter)[c(2)] <- c("Real_Estate_Lease_Rental")


########################################


## Loading the Canada GDP file
Canada_GDP <- read.csv("canada_gdp.csv")

## Investigating about the structure of the file
str(Canada_GDP)

names(Canada_GDP)[c(1)] <- c("REF_DATE")

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
MASTER_DATA <- merge(Canada_GDP , IP_DF_filter, by ="REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , UR_DF_filter, by ="REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , USD_filter, by ="REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , IMF_position, by ="REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , IR_official, by ="REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , Stock_Market_FI, by ="REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , CDI, by ="REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , FDI, by ="REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , Retail_DF, by ="REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , Tourism_df, by ="REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , Salary_filter, by ="REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , Import_filter, by ="REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , Export_filter, by ="REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , RE_DF_filter, by ="REF_DATE", all = TRUE)
MASTER_DATA <- merge(MASTER_DATA , RE_LR_DF_filter, by ="REF_DATE", all = TRUE)


str(MASTER_DATA)
MASTER_DATA


############ SAMPLE EDA #################

MASTER_DATA_filter <- filter(MASTER_DATA ,
                             MASTER_DATA$REF_DATE >= 1999,
                             MASTER_DATA$REF_DATE <= 2018)




MASTER_DATA_filter$REF_DATE <- as.numeric(MASTER_DATA_filter$REF_DATE)
str(MASTER_DATA_filter)

md_correl <- cor(MASTER_DATA_filter[c(4:18)] , use = "complete.obs")

ggcorrplot(md_correl)

ggcorrplot_clustered <- ggcorrplot(md_correl, hc.order = TRUE, type = "lower")


####################  Univariate Analysis ####################  


#===================#
#   Color palette   #
#===================#

cp_2 <- c("#FEA47F", "#F97F51")
cp_3 <- c("#2A363B", "#E84A5F", "#FF847C")
cp_5 <- c("#2A363B", "#E84A5F", "#FF847C", "#FECEAB", "#99B898")
cp_6 <- c("#2A363B", "#E84A5F", "#FF847C", "#FECEAB", "#99B898", "#BDC581")
cp_8 <- c("#FEA47F", "#F97F51", "#B33771", "#3B3B98", "#58B19F", "#BDC581", "#2C3A47", "#82589F")
cp_10 <- c("#2A363B", "#E84A5F","#FEA47F", "#F97F51", "#B33771", "#3B3B98", "#58B19F", "#BDC581", "#2C3A47", "#82589F")


####################  creating a correlation matrix
MASTER_DATA_filter$REF_DATE <- as.numeric(MASTER_DATA_filter$REF_DATE)

cor_matrix <- cor(MASTER_DATA_filter[,c(2, 4:18)])
round (cor_matrix,2)

###                              Annual_GDP 
###  Annual_GDP                     1.00                   
###  InternatIonal_Payments         0.89                   
###  Unemployment_Rate             -0.14                  
###  US_Dollars                     0.66                  
###  IMF_Position                   0.17                  
###  International_Reserves         0.84                  
###  TSE_FI                         0.85                 
###  CDI_TBV                        0.83                  
###  FDI_TBV                        0.91                   
###  RETAIL                         0.95                   
###  TOURISM                        0.94                  
###  Salary_Value                   0.94                 
###  Import                         0.82                  
###  Export                         0.59                  
###  Real_Estate                    0.94                  
###  Real_Estate_Lease_Rental       0.94                  


####################  InternatIonal_Payments

boxplot(MASTER_DATA_filter$InternatIonal_Payments, main="Boxplot for InternatIonal_Payments", ylab="InternatIonal_Payments", col="#FF847C")

hist(MASTER_DATA_filter$InternatIonal_Payments, col="#FF847C" ,main="Histogram for InternatIonal_Payments", xlab="InternatIonal_Payments")

MASTER_DATA_filter %>%
  ggplot( aes(x=REF_DATE, y=InternatIonal_Payments, color = Annual_GDP, group = 1)) +
  geom_point()+
  geom_line() +
  scale_fill_manual(values = cp_2) +
  labs(x=" YEAR ", y="InternatIonal_Payments  (in million $ )", 
       fill="Annual_GDP_Canada (in Million $)", 
       title="InternatIonal_Payments to Annual_GDP PLOT") +
  theme_minimal()

####################  Unemployment_Rate

boxplot(MASTER_DATA_filter$Unemployment_Rate, main="Boxplot for Unemployment_Rate", ylab="Unemployment_Rate", col="#FF847C")

hist(MASTER_DATA_filter$Unemployment_Rate, col="#FF847C" ,main="Histogram for Unemployment_Rate", xlab="Unemployment_Rate")

MASTER_DATA_filter %>%
  ggplot( aes(x=REF_DATE, y=Unemployment_Rate, color = Annual_GDP, group = 1)) +
  geom_point()+
  geom_line() +
  scale_fill_manual(values = cp_2) +
  labs(x=" YEAR ", y="Unemployment_Rate  (in % )", 
       fill="Annual_GDP_Canada (in Million $)", 
       title="Unemployment_Rate to Annual_GDP PLOT") +
  theme_minimal()

####################  US_Dollars

boxplot(MASTER_DATA_filter$US_Dollars, main="Boxplot for US_Dollars", ylab="US_Dollars", col="#FF847C")

hist(MASTER_DATA_filter$US_Dollars, col="#FF847C" ,main="Histogram for US_Dollars", xlab="US_Dollars")


MASTER_DATA_filter %>%
  ggplot( aes(x=REF_DATE, y=US_Dollars, color = Annual_GDP, group = 1)) +
  geom_point()+
  geom_line() +
  scale_fill_manual(values = cp_2) +
  labs(x=" YEAR ", y="US_Dollars  (in million $ )", 
       fill="Annual_GDP_Canada (in Million $)", 
       title="US_Dollars to Annual_GDP PLOT") +
  theme_minimal()

####################  IMF_Position

boxplot(MASTER_DATA_filter$IMF_Position, main="Boxplot for IMF_Position", ylab="IMF_Position", col="#FF847C")

hist(MASTER_DATA_filter$IMF_Position, col="#FF847C" ,main="Histogram for IMF_Position", xlab="IMF_Position")

MASTER_DATA_filter %>%
  ggplot( aes(x=REF_DATE, y=IMF_Position, color = Annual_GDP, group = 1)) +
  geom_point()+
  geom_line() +
  scale_fill_manual(values = cp_2) +
  labs(x=" YEAR ", y="IMF_Position  (in million $ )", 
       fill="Annual_GDP_Canada (in Million $)", 
       title="IMF_Position to Annual_GDP PLOT") +
  theme_minimal()

####################  International_Reserves

boxplot(MASTER_DATA_filter$International_Reserves, main="Boxplot for International_Reserves", ylab="International_Reserves", col="#FF847C")

hist(MASTER_DATA_filter$International_Reserves, col="#FF847C" ,main="Histogram for International_Reserves", xlab="International_Reserves")

MASTER_DATA_filter %>%
  ggplot( aes(x=REF_DATE, y=International_Reserves, color = Annual_GDP, group = 1)) +
  geom_point()+
  geom_line() +
  scale_fill_manual(values = cp_2) +
  labs(x=" YEAR ", y="International_Reserves  (in million $ )", 
       fill="Annual_GDP_Canada (in Million $)", 
       title="International_Reserves to Annual_GDP PLOT") +
  theme_minimal()

####################  TSE_FI

boxplot(MASTER_DATA_filter$TSE_FI, main="Boxplot for TSE_FI", ylab="TSE_FI", col="#FF847C")

hist(MASTER_DATA_filter$TSE_FI, col="#FF847C" ,main="Histogram for TSE_FI", xlab="TSE_FI")

MASTER_DATA_filter %>%
  ggplot( aes(x=REF_DATE, y=TSE_FI, color = Annual_GDP, group = 1)) +
  geom_point()+
  geom_line() +
  scale_fill_manual(values = cp_2) +
  labs(x=" YEAR ", y="TSE_FI  (index )", 
       fill="Annual_GDP_Canada (in Million $)", 
       title="TSE_FI to Annual_GDP PLOT") +
  theme_minimal()

####################  CDI_TBV

boxplot(MASTER_DATA_filter$CDI_TBV, main="Boxplot for CDI_TBV", ylab="CDI_TBV", col="#FF847C")

hist(MASTER_DATA_filter$CDI_TBV, col="#FF847C" ,main="Histogram for CDI_TBV", xlab="CDI_TBV")

MASTER_DATA_filter %>%
  ggplot( aes(x=REF_DATE, y=CDI_TBV, color = Annual_GDP, group = 1)) +
  geom_point()+
  geom_line() +
  scale_fill_manual(values = cp_2) +
  labs(x=" YEAR ", y="CDI_TBV  (in million $ )", 
       fill="Annual_GDP_Canada (in Million $)", 
       title="CDI_TBV to Annual_GDP PLOT") +
  theme_minimal()

####################  FDI_TBV

boxplot(MASTER_DATA_filter$FDI_TBV, main="Boxplot for FDI_TBV", ylab="FDI_TBV", col="#FF847C")

hist(MASTER_DATA_filter$FDI_TBV, col="#FF847C" ,main="Histogram for FDI_TBV", xlab="FDI_TBV")

MASTER_DATA_filter %>%
  ggplot( aes(x=REF_DATE, y=FDI_TBV, color = Annual_GDP, group = 1)) +
  geom_point()+
  geom_line() +
  scale_fill_manual(values = cp_2) +
  labs(x=" YEAR ", y="FDI_TBV  (in million $ )", 
       fill="Annual_GDP_Canada (in Million $)", 
       title="FDI_TBV to Annual_GDP PLOT") +
  theme_minimal()


####################  RETAIL

boxplot(MASTER_DATA_filter$RETAIL, main="Boxplot for RETAIL", ylab="RETAIL", col="#FF847C")

hist(MASTER_DATA_filter$RETAIL, col="#FF847C" ,main="Histogram for RETAIL", xlab="RETAIL")

MASTER_DATA_filter %>%
  ggplot( aes(x=REF_DATE, y=RETAIL, color = Annual_GDP, group = 1)) +
  geom_point()+
  geom_line() +
  scale_fill_manual(values = cp_2) +
  labs(x=" YEAR ", y="RETAIL  (in million $ )", 
       fill="Annual_GDP_Canada (in Million $)", 
       title="RETAIL to Annual_GDP PLOT") +
  theme_minimal()

####################  TOURISM 

boxplot(MASTER_DATA_filter$TOURISM , main="Boxplot for TOURISM ", ylab="TOURISM", col="#FF847C")

hist(MASTER_DATA_filter$TOURISM , col="#FF847C" ,main="Histogram for TOURISM", xlab="TOURISM")

MASTER_DATA_filter %>%
  ggplot( aes(x=REF_DATE, y=TOURISM, color = Annual_GDP, group = 1)) +
  geom_point()+
  geom_line() +
  scale_fill_manual(values = cp_2) +
  labs(x=" YEAR ", y="TOURISM  (in million $ )", 
       fill="Annual_GDP_Canada (in Million $)", 
       title="TOURISM to Annual_GDP PLOT") +
  theme_minimal()

####################  Salary_Value 

boxplot(MASTER_DATA_filter$Salary_Value , main="Boxplot for Salary_Value ", ylab="Salary_Value", col="#FF847C")

hist(MASTER_DATA_filter$Salary_Value , col="#FF847C" ,main="Histogram for Salary_Value", xlab="Salary_Value")

MASTER_DATA_filter %>%
  ggplot( aes(x=REF_DATE, y=Salary_Value, color = Annual_GDP, group = 1)) +
  geom_point()+
  geom_line() +
  scale_fill_manual(values = cp_2) +
  labs(x=" YEAR ", y="Salary_Value  (in million $ )", 
       fill="Annual_GDP_Canada (in Million $)", 
       title="Salary_Value to Annual_GDP PLOT") +
  theme_minimal()

####################  Import  

boxplot(MASTER_DATA_filter$Import  , main="Boxplot for Import  ", ylab="Import ", col="#FF847C")

hist(MASTER_DATA_filter$Import  , col="#FF847C" ,main="Histogram for Import ", xlab="Import ")

MASTER_DATA_filter %>%
  ggplot( aes(x=REF_DATE, y=Import, color = Annual_GDP, group = 1)) +
  geom_point()+
  geom_line() +
  scale_fill_manual(values = cp_2) +
  labs(x=" YEAR ", y="Import  (in million $ )", 
       fill="Annual_GDP_Canada (in Million $)", 
       title="Import to Annual_GDP PLOT") +
  theme_minimal()

####################  Export   

boxplot(MASTER_DATA_filter$Export   , main="Boxplot for Export   ", ylab="Export  ", col="#FF847C")

hist(MASTER_DATA_filter$Export   , col="#FF847C" ,main="Histogram for Export  ", xlab="Export  ")

MASTER_DATA_filter %>%
  ggplot( aes(x=REF_DATE, y=Export, color = Annual_GDP, group = 1)) +
  geom_point()+
  geom_line() +
  scale_fill_manual(values = cp_2) +
  labs(x=" YEAR ", y="Export  (in million $ )", 
       fill="Annual_GDP_Canada (in Million $)", 
       title="Export to Annual_GDP PLOT") +
  theme_minimal()

####################  Real_Estate   

boxplot(MASTER_DATA_filter$Real_Estate   , main="Boxplot for Real_Estate   ", ylab="Real_Estate  ", col="#FF847C")

hist(MASTER_DATA_filter$Real_Estate   , col="#FF847C" ,main="Histogram for Real_Estate  ", xlab="Real_Estate  ")

MASTER_DATA_filter %>%
  ggplot( aes(x=REF_DATE, y=Real_Estate, color = Annual_GDP, group = 1)) +
  geom_point()+
  geom_line() +
  scale_fill_manual(values = cp_2) +
  labs(x=" YEAR ", y="Real_Estate  (in million $ )", 
       fill="Annual_GDP_Canada (in Million $)", 
       title="Real_Estate to Annual_GDP PLOT") +
  theme_minimal()

####################  Real_Estate_Lease_Rental   

boxplot(MASTER_DATA_filter$Real_Estate_Lease_Rental   , main="Boxplot for Real_Estate_Lease_Rental   ", ylab="Real_Estate_Lease_Rental  ", col="#FF847C")

hist(MASTER_DATA_filter$Real_Estate_Lease_Rental   , col="#FF847C" ,main="Histogram for Real_Estate_Lease_Rental  ", xlab="Real_Estate_Lease_Rental  ")

MASTER_DATA_filter %>%
  ggplot( aes(x=REF_DATE, y=Real_Estate_Lease_Rental, color = Annual_GDP, group = 1)) +
  geom_point()+
  geom_line() +
  scale_fill_manual(values = cp_2) +
  labs(x=" YEAR ", y="Real_Estate_Lease_Rental  (in million $ )", 
       fill="Annual_GDP_Canada (in Million $)", 
       title="Real_Estate_Lease_Rental to Annual_GDP PLOT") +
  theme_minimal()


####################  PCA   ####################  
str(MASTER_DATA_filter[,c(4:18)])

colnames(MASTER_DATA_filter[,c(4:18)])

MASTER_DATA_filter_PCA <- prcomp(MASTER_DATA_filter[,c(4:18)], center = TRUE,scale. = TRUE)

summary(MASTER_DATA_filter_PCA)

str(MASTER_DATA_filter_PCA)

names (MASTER_DATA_filter_PCA)

print(MASTER_DATA_filter_PCA)

?PCA

pca_result <- PCA(MASTER_DATA_filter[,c(4:18)], graph = TRUE,scale.unit = TRUE)

pca_result$eig
pca_result$var$cos2
pca_result$var$contrib
dimdesc(pca_result)

summary(pca_result, nbelements = 5)

pca_result$eig[,2][1:3]
pca_result$eig[,3][1:3]

xx <- get_eigenvalue(pca_result)
round(xx,4)


#####   Kaiser-Guttman Rule eigenvalue >1 should be kept hance only dimensions



#Screeplot
fviz_screeplot(pca_result)

# Create a factor map for the variables.
fviz_pca_var(pca_result, select.var = list(cos2 = 0.7), repel = TRUE)

# Modify the code to create a factor map for the individuals.
fviz_pca_ind(pca_result, select.ind = list(cos2 = 0.7), repel = TRUE)

# Create a barplot for the variables with the highest cos2 in the 1st PC.
fviz_cos2(pca_result, choice = "var", axes = 1)

# Create a barplot for the variables with the highest cos2 in the 2nd PC.
fviz_cos2(pca_result, choice = "var", axes = 2)

# Create a factor map for the top 5 variables with the highest contributions.
fviz_pca_var(pca_result, select.var = list(contrib = 5), repel = TRUE)

# Create a factor map for the top 5 individuals with the highest contributions.
fviz_pca_ind(pca_result, select.ind = list(contrib = 5), repel = TRUE)


# Create a barplot for the variables with the highest contributions to the 1st PC.
fviz_contrib(pca_result, choice = "var", axes = 1)
fviz_contrib(pca_result, choice = "var", axes = 1, top = 5)


# Create a barplot for the variables with the highest contributions to the 2nd PC.
fviz_contrib(pca_result, choice = "var", axes = 2, top = 5)

# Create a biplot with no labels for all individuals with the geom argument.
fviz_pca_biplot(pca_result)

# Create ellipsoids with habillage for wheeltype
fviz_pca_ind(pca_result,  addEllipses = TRUE)

# Create the biplot with ellipsoids
fviz_pca_biplot(pca_result,  addEllipses = TRUE, alpha.var = "cos2")

pca_result$eig

#####Generating Train, Test and Predict data sets
#################################################

MASTER_DATA_TRAIN <- MASTER_DATA %>%
  filter( MASTER_DATA$REF_DATE >=1999 , MASTER_DATA$REF_DATE <=2012)


MASTER_DATA_TEST <- MASTER_DATA %>%
  filter( MASTER_DATA$REF_DATE >=2013 , MASTER_DATA$REF_DATE <=2015)


pred_2016 <- select (MASTER_DATA , InternatIonal_Payments, Unemployment_Rate, US_Dollars, IMF_Position,
                     International_Reserves, TSE_FI, CDI_TBV, FDI_TBV, RETAIL,
                     TOURISM, Salary_Value, Import, Export, Real_Estate, Real_Estate_Lease_Rental) %>%
  filter(MASTER_DATA$REF_DATE == 2016)


pred_2017 <- select (MASTER_DATA , InternatIonal_Payments, Unemployment_Rate, US_Dollars, IMF_Position,
                     International_Reserves, TSE_FI, CDI_TBV, FDI_TBV, RETAIL,
                     TOURISM, Salary_Value, Import, Export, Real_Estate, Real_Estate_Lease_Rental) %>%
  filter(MASTER_DATA$REF_DATE == 2017)


pred_2018 <- select (MASTER_DATA , InternatIonal_Payments, Unemployment_Rate, US_Dollars, IMF_Position,
                     International_Reserves, TSE_FI, CDI_TBV, FDI_TBV, RETAIL,
                     TOURISM, Salary_Value, Import, Export, Real_Estate, Real_Estate_Lease_Rental) %>%
  filter(MASTER_DATA$REF_DATE == 2018)



#######################################################################
###########Linear Regression Model#####################################



mlr_model <- lm(Annual_GDP ~ InternatIonal_Payments + Unemployment_Rate + 
                  US_Dollars + IMF_Position + International_Reserves + 
                  TSE_FI + CDI_TBV + FDI_TBV + RETAIL + 
                  TOURISM + Salary_Value + Import + Export + 
                  Real_Estate + Real_Estate_Lease_Rental, data=MASTER_DATA)

summary(mlr_model)

######  Residual standard error: 52460 on 4 degrees of freedom
######  (48 observations deleted due to missingness)
######  Multiple R-squared:  0.9965,	Adjusted R-squared:  0.9836 
######  F-statistic:  76.9 on 15 and 4 DF,  p-value: 0.000375



predict(mlr_model, newdata = pred_2016)
####  Actual = 1530024    Predicted = 1530627 

rmse (actual = 1530024 , predicted = 1530627)
#### 603



predict(mlr_model, newdata = pred_2017)
####  Actual = 1649934    Predicted = 1634087 

rmse (actual = 1649934 , predicted = 1634087)
#### 15847



predict(mlr_model, newdata = pred_2018)
####  Actual = 1712479    Predicted = 1729176 

rmse (actual = 1712479 , predicted = 1729176)
#### 16697




########### Random forest Model ###########  


nrow(MASTER_DATA_TRAIN)
####    14

nrow(MASTER_DATA_TEST)
####    3

rf_model <- randomForest(Annual_GDP ~ InternatIonal_Payments + Unemployment_Rate + 
                           US_Dollars + IMF_Position + International_Reserves + 
                           TSE_FI + CDI_TBV + FDI_TBV + RETAIL + 
                           TOURISM + Salary_Value + Import + Export + 
                           Real_Estate + Real_Estate_Lease_Rental, data=MASTER_DATA, 
                         na.action=na.roughfix, mtry=4, ntree=2001, importance=TRUE)

summary(rf_model)
####    Type of random forest: regression
####    Number of trees: 2001
####    No. of variables tried at each split: 4

####    Mean of squared residuals: 35260190932
####    % Var explained: 87.94

plot(rf_model)

result <- data.frame(MASTER_DATA_TEST$Annual_GDP, predict(rf_model, MASTER_DATA_TEST, type = "response"))
#                      Actual                        Predicted            
#                      1846595                       1786687
#                      1805745                       1759556
#                      1556506                       1630753


head(result)
plot(result)

pred_set <- MASTER_DATA %>%
  filter(MASTER_DATA$REF_DATE >= 2016, MASTER_DATA$REF_DATE <= 2018)

predicted <- data.frame(pred_set$Annual_GDP, predict(rf_model, pred_set, type = "response"))
#             Actual                  Predicted              
#             1530024                 1598421
#             1649934                 1596030
#             1712479                 1480757


str(predicted)

rmse(actual = predicted$pred_set.Annual_GDP , 
     predicted = predicted$predict.rf_model..pred_set..type....response..)

##### 142920.5

################ Regression Tree Model ############################

MASTER_DATA_filter <- filter(MASTER_DATA ,
                             MASTER_DATA$REF_DATE >= 1999,
                             MASTER_DATA$REF_DATE < 2016)

Test <- filter(MASTER_DATA ,
               MASTER_DATA$REF_DATE >= 2016,
               MASTER_DATA$REF_DATE < 2019)

set.seed(123)
assignment <- sample(1:3, size = nrow(MASTER_DATA_filter), prob = c(0.7, 0.15, 0.15), replace = TRUE)

# Create a train, validation and tests from the original data frame 
mdf_train <- MASTER_DATA_filter[assignment == 1, ]   # subset Master_DATA to training indices only
mdf_valid <- MASTER_DATA_filter[assignment == 2, ]   # subset Master_DATA to validation indices only
mdf_test  <- MASTER_DATA_filter[assignment == 3, ]   # subset Master_DATA to test indices only

# Train the model
rt_model <- rpart(formula = Annual_GDP ~ InternatIonal_Payments + Unemployment_Rate + 
                    US_Dollars + IMF_Position + International_Reserves + 
                    TSE_FI + CDI_TBV + FDI_TBV + RETAIL + 
                    TOURISM + Salary_Value + Import + Export + 
                    Real_Estate + Real_Estate_Lease_Rental, 
                  data = MASTER_DATA_filter, 
                  method = "anova")


# Look at the model output                      
summary(rt_model)



# Generate predictions on a test set
pred <- predict(object = rt_model,   
                newdata = Test)   

pred_valid <- predict(object = rt_model,   
                      newdata = mdf_valid)  

# Compute the RMSE
rmse(actual = Test$Annual_GDP, 
     predicted = pred)
##### 644579.1

rmse(actual = mdf_valid$Annual_GDP, 
     predicted = pred_valid)
#####  404454.1



########################################## Alternate Approach########################################## 
####################################################################################################### 
####################################################################################################### 
####################################################################################################### 
####################################################################################################### 
####################################################################################################### 
####################################################################################################### 
####################################################################################################### 
####################################################################################################### 
####################################################################################################### 




MLR_TRAIN <- MASTER_DATA %>%
  filter( MASTER_DATA$REF_DATE >=1999 , MASTER_DATA$REF_DATE <=2015)


###########     Linear Regression Model with filetered Data ##################



mlr_model_new <- lm(Annual_GDP ~ InternatIonal_Payments  + Unemployment_Rate +
                      US_Dollars + IMF_Position + International_Reserves + 
                      TSE_FI + CDI_TBV + FDI_TBV + RETAIL + 
                      TOURISM + Salary_Value + Import + Export + 
                      Real_Estate + Real_Estate_Lease_Rental, data=MLR_TRAIN)

summary(mlr_model_new)

###########     Residual standard error: 78250 on 1 degrees of freedom
###########     Multiple R-squared:  0.9979,	Adjusted R-squared:  0.9662 
###########     F-statistic: 31.48 on 15 and 1 DF,  p-value: 0.1391




predict(mlr_model_new, newdata = pred_2016)
####  Actual = 1530024    Predicted = 1604077 

rmse (actual = 1530024 , predicted = 1604077 )
####  74053



predict(mlr_model_new, newdata = pred_2017)
####  Actual = 1649934    Predicted = 1758862 

rmse (actual = 1649934 , predicted = 1758862  )
#### 108928



predict(mlr_model_new, newdata = pred_2018)
####  Actual = 1712479    Predicted = 2105717  

rmse (actual = 1712479 , predicted = 2105717 )
#### 393238



############################ Linear Regression Model with SCALING ##########################################

MLR_SCALED <- MASTER_DATA %>%
  filter( MASTER_DATA$REF_DATE >=1999 , MASTER_DATA$REF_DATE <=2018)

MLR_SCALED[,2] <-scale(MLR_SCALED[,2] )
MLR_SCALED[,4:18] <-scale(MLR_SCALED[,4:18] )

MLR_SCALED_TRAIN <- MLR_SCALED %>%
  filter(MLR_SCALED$REF_DATE >=1999 ,  MLR_SCALED$REF_DATE <=2015)

pred_2016_scaled <- select (MLR_SCALED , InternatIonal_Payments,  US_Dollars, IMF_Position,
                            International_Reserves, TSE_FI, CDI_TBV, FDI_TBV, RETAIL,
                            TOURISM, Salary_Value, Import, Export, Real_Estate, Real_Estate_Lease_Rental) %>%
  filter(MLR_SCALED$REF_DATE == 2016)


pred_2017_scaled <- select (MLR_SCALED , InternatIonal_Payments,  US_Dollars, IMF_Position,
                            International_Reserves, TSE_FI, CDI_TBV, FDI_TBV, RETAIL,
                            TOURISM, Salary_Value, Import, Export, Real_Estate, Real_Estate_Lease_Rental) %>%
  filter(MLR_SCALED$REF_DATE == 2017)


pred_2018_scaled <- select (MLR_SCALED , InternatIonal_Payments,  US_Dollars, IMF_Position,
                            International_Reserves, TSE_FI, CDI_TBV, FDI_TBV, RETAIL,
                            TOURISM, Salary_Value, Import, Export, Real_Estate, Real_Estate_Lease_Rental) %>%
  filter(MLR_SCALED$REF_DATE == 2018)



mlr_model_scaled <- lm(Annual_GDP ~ InternatIonal_Payments  + 
                         US_Dollars + IMF_Position + International_Reserves + 
                         TSE_FI + CDI_TBV + FDI_TBV + RETAIL + 
                         TOURISM + Salary_Value + Import + Export + 
                         Real_Estate + Real_Estate_Lease_Rental, data=MLR_SCALED_TRAIN)

summary(mlr_model_scaled)

###########     Residual standard error: 0.1359 on 2 degrees of freedom
###########     Multiple R-squared:  0.9979,	Adjusted R-squared:  0.9829 
###########     F-statistic: 66.67 on 14 and 2 DF,  p-value: 0.01487

coef(mlr_model_scaled)


predict(mlr_model_scaled, newdata = pred_2016_scaled)
####  Actual = 0.43044068	    Predicted = 0.6270653 



rmse (actual = 0.43044068 , predicted = 0.6270653)
#### 0.1966246



predict(mlr_model_scaled, newdata = pred_2017_scaled)
####  Actual = 0.72329693	    Predicted = 1.024896 


rmse (actual = 0.72329693	 , predicted = 1.024896 )
#### 0.3015991



predict(mlr_model_scaled, newdata = pred_2018_scaled)
####  Actual = 0.87605062    Predicted = 1.836856 



rmse (actual = 0.87605062 , predicted = 1.836856 )
#### 0.9608054


xxxx <-cor(MLR_SCALED[,c(2, 4:18)])
round (xxxx,2)


############################ Linear Regression Model with SCALING after PCA ##########################################

##### Features Selected after PCA 
names(MASTER_DATA[,c(13,4,17,11,18,14,10,12,15,8)])


MLR_SCALED_PCA <- MASTER_DATA[,c (1,2,13,4,17,11,18,14,10,12,15,8)] %>%
  filter( MASTER_DATA$REF_DATE >=1999 , MASTER_DATA$REF_DATE <=2018)

MLR_SCALED_PCA[,2:12] <-scale(MLR_SCALED_PCA[,2:12] )

names (MLR_SCALED_PCA)
str(MLR_SCALED_PCA)

MLR_SCALED_PCA_TRAIN <- MLR_SCALED_PCA %>%
  filter(MLR_SCALED_PCA$REF_DATE >=1999 ,  MLR_SCALED_PCA$REF_DATE <=2015)

pred_2016_scaled_pca <- MLR_SCALED_PCA %>%
  filter(MLR_SCALED_PCA$REF_DATE == 2016)

pred_2017_scaled_pca <-MLR_SCALED_PCA %>%
  filter(MLR_SCALED$REF_DATE == 2017)

pred_2018_scaled_pca <-MLR_SCALED_PCA %>%
  filter(MLR_SCALED_PCA$REF_DATE == 2018)

pred_2016_scaled_pca <- within (pred_2016_scaled_pca , rm (REF_DATE, Annual_GDP))
pred_2017_scaled_pca <- within (pred_2017_scaled_pca , rm (REF_DATE, Annual_GDP))
pred_2018_scaled_pca <- within (pred_2018_scaled_pca , rm (REF_DATE, Annual_GDP))



mlr_model_scaled_pca <- lm(Annual_GDP ~ TOURISM + InternatIonal_Payments + Real_Estate  + 
                             FDI_TBV +  Real_Estate_Lease_Rental + Salary_Value + 
                             CDI_TBV  +  RETAIL + Import + International_Reserves, data=MLR_SCALED_PCA_TRAIN)

summary(mlr_model_scaled_pca)

###########     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
###########     Residual standard error: 0.1212 on 6 degrees of freedom
###########     Multiple R-squared:  0.9949,	Adjusted R-squared:  0.9864 
###########     F-statistic: 117.1 on 10 and 6 DF,  p-value: 4.567e-06

coef(mlr_model_scaled_pca)

###########     (Intercept)                  TOURISM   InternatIonal_Payments              Real_Estate 
###########     0.04475269               1.51775872               0.23200432               0.58871156 
###########     FDI_TBV Real_Estate_Lease_Rental             Salary_Value                  CDI_TBV 
###########     -0.68083465               0.52855570               0.53108814              -1.26620035 
###########     RETAIL                   Import   International_Reserves 
###########     0.19217244              -0.39235160              -0.35996658 

predict(mlr_model_scaled_pca, newdata = pred_2016_scaled_pca)
####  Actual = 0.43044068	    Predicted = 0.5547406   

rmse (actual = 0.43044068 , predicted = 0.5547406 )
#### 0.1242999

predict(mlr_model_scaled_pca, newdata = pred_2017_scaled_pca)
####  Actual = 0.72329693	    Predicted = 1.046634  

rmse (actual = 0.72329693	 , predicted =  1.046634 )
#### 0.3233371

predict(mlr_model_scaled_pca, newdata = pred_2018_scaled_pca)
####  Actual = 0.87605062    Predicted = 1.323468  

rmse (actual = 0.87605062 , predicted =  1.323468  )
#### 0.4474174


########### Random forest Model with Scaling and PCA ###########  


##### Features Selected after PCA 
names(MASTER_DATA[,c(13,4,17,11,18,14,10,12,15,8)])


RF_SCALED_PCA <- MASTER_DATA[,c (1,2,13,4,17,11,18,14,10,12,15,8)] %>%
  filter( MASTER_DATA$REF_DATE >=1999 , MASTER_DATA$REF_DATE <=2018)

RF_SCALED_PCA[,2:12] <-scale(RF_SCALED_PCA[,2:12] )


RF_SCALED_PCA_TRAIN <- RF_SCALED_PCA %>%
  filter(RF_SCALED_PCA$REF_DATE >=1999 ,  RF_SCALED_PCA$REF_DATE <=2012)

RF_SCALED_PCA_TEST <- RF_SCALED_PCA %>%
  filter(RF_SCALED_PCA$REF_DATE >=2013 ,  RF_SCALED_PCA$REF_DATE <=2015)





nrow(RF_SCALED_PCA_TRAIN)
####    14

nrow(RF_SCALED_PCA_TEST)
####    3

rf_model_scaled_pca <- randomForest(Annual_GDP ~ TOURISM + InternatIonal_Payments + Real_Estate  + 
                                      FDI_TBV +  Real_Estate_Lease_Rental + Salary_Value + 
                                      CDI_TBV  +  RETAIL + Import + International_Reserves, data=RF_SCALED_PCA_TRAIN, 
                                    na.action=na.roughfix, mtry=4, ntree=2001, importance=TRUE)

summary(rf_model_scaled_pca)
####    Type of random forest: regression
####    Number of trees: 2001
####    No. of variables tried at each split: 4

####    Mean of squared residuals: 0.07983514
####    % Var explained: 91.37

plot(rf_model_scaled_pca)

result_rf <- data.frame(RF_SCALED_PCA_TEST$Annual_GDP, predict(rf_model_scaled_pca, RF_SCALED_PCA_TEST, type = "response"))
#                      Actual                        Predicted            
#                     1.2036022                                                           0.8860223
#                     1.1038342                                                           0.8860223
#                     0.4951177                                                           0.8860223


head(result_rf)
plot(result_rf)

pred_set <- RF_SCALED_PCA %>%
  filter(RF_SCALED_PCA$REF_DATE >= 2016, RF_SCALED_PCA$REF_DATE <= 2018)

predicted <- data.frame(pred_set$Annual_GDP, predict(rf_model_scaled_pca, pred_set, type = "response"))
#           0.4304407                                                 0.8860223
#           0.7232969                                                 0.8860223
#           0.8760506                                                 0.8860223


str(predicted)

rmse(actual = predicted$pred_set.Annual_GDP , 
     predicted = predicted$predict.rf_model..pred_set..type....response..)

##### 

