### This file replicates the data collection and plotting for the
### RESSSAKS Data Challenge, 2019. Done by Luke Korir.
### Last updated. February 2019.
setwd("G:/My Drive/MoreMilk/resakss data chllenge/") 
rm(list = ls())
# Load libraries required ------------------------------------------------------------------------------------------------------------------------------
library(tibble)
library(openxlsx)
library(readxl)
library(dplyr)
library(tidyr)
library(countrycode) 
library(dplyr)
library(purrr)
library(gdata)
library(plotrix)
library(ggplot2)
library(grid)
library(ggrepel)
library(ggridges)
library(extrafont)
library(magick)
library(sp)
library(rworldmap) 
library(rgdal)


# Generate necessary functions ------------------------------------------------------------------------------------------------------------------------------
# Reshaping function
reshapeFunc <- function(x)
{x <-x[1:(nrow(x)-3),]         %>%
  rename(indic = 1 )           %>%
  rownames_to_column           %>% 
  gather(var, value, -rowname) %>% 
  spread(rowname,  value)      
}
#Function to make first row column names
changecolFunc <- function(y) {
  y <- y %>%  setNames(as.character(y[1,])) %>%
    filter(indic != 'indic') #%>%
  }


# Renaming and selecting only variables of interest function
renameFunc <- function(z)
{z <-z %>%  dplyr::select ( 
  year                      = "Indicator",
  gini                      = "GINI index",
  poverty                   = "Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)",
  employ_rate_per_over15    = "Employment rate (% of population, 15+ years)",
  agric_VA_abs              = "Agriculture, value added (constant 2010 USD, million)",
  agric_prod_index          = "Agrciulture Production Index Number (2004-2006 = 100) (Int. $)",
  agric_VA_worker_abs       = "Agriculture value added per worker (constant 2010 USD)",
  agric_VA_acre_abs         = "Agriculture value added per hectare of agricultural land (constant 2010 USD)",
  meat_yield_ind_kg_head    = " Yield, meat (indigenous cattle, kilograms per head)",
  milk_yield_kg_head        = "Yield, milk (whole fresh cow, kilograms per head)",
  afr_trade_agri_export     = "Intra-African agricultural trade, exports (constant 2010 US$, million)",
  gdp_capita                = "GDP per capita (constant 2010 USD)",
  afr_trade_agri_import     = "Intra-African agricultural trade, imports (constant 2010 USD, million)",
  dom_food_price_index      = "Domestic food price volatility (index)",
  gov_exp_agric_abs         = "Government agriculture expenditure (constant 2010 US$, billion)",
  gov_exp_agric_per_tot_exp = "Government agriculture expenditure (% of total expenditure)",
  gov_exp_agric_per_VA      = "Government agriculture expenditure (% of agriculture value added)",
  agric_ODA_per_tot_ODA     = "Agricultural ODA (% total ODA)",
  gov_gross_debt_per_gdp    = "General government gross debt (% of GDP)",
  gov_revenue_per_gdp       = "General government gross revenue (% of GDP)",
  hh_cons_exp_capita        = "Household consumption expenditure per capita (constant 2010 USD)",
  annual_inflation_per      = "Annual inflation, GDP deflator (%)",
  agric_export_per_tot_mech = "Agricultural exports (% of total merchandise exports)",
  agric_import_per_tot_mech = "Agricultural imports (% of total merchandise imports)",
  fert_cons_kg_acre         = "Total fertilizer consumption (kilogram per hectare)" ,
  agric_VA_per_gdp          = "Agricultural value added (% GDP)",
  gdp_abs                   = "Gross Domestic Product (constant 2010 US$, billion)",
  glob_hunger_index         = "Global Hunger Index",
  undernutrition_per_pop    = "Prevalance of undernourishment (% of population)",
  underweight_per_under5    = "Prevalence of underweight, weight for age (% of children under 5)",
  stunting_per_under5       = "Prevalence of stunting, height for age (% of children under 5)",
  wasting_per_under5        = "Prevalence of wasting (% of children under 5)",
  cereal_imp_dep_ratio      = "Cereal import dependency ratio (%)",
  employ_rate_per15_65      = "Employment rate (% of labor force, 15-64 years)") %>%
  arrange(year)
}



#Import data from excel manually downloaded from RESSAKS website ---------------------------------------------------------------
path <- loadWorkbook("exp-2019-02-25_14_00_03.xlsx")
sheetnames      <- readxl::excel_sheets ("exp-2019-02-25_14_00_03.xlsx")

for(i in 1:length(sheetnames))
{
  assign(sheetnames[i], 
         readWorkbook(path, sheet = i))
}



#countries <- list(AGO , BDI, BEN, BFA ,  BWA ,  CAF ,  CIV ,  CMR ,  COD ,  COG ,  COM ,  CPV ,  DJI ,  DZA ,  EGY ,  ERI , 
#                  ETH ,  GAB ,  GHA ,  GIN ,  GMB , GNB ,  GNQ ,  KEN ,  LBR ,  LBY ,  LSO ,  MAR ,  MDG ,  MLI ,  MOZ ,  MRT , 
#                  MUS ,  MWI ,  NAM ,  NER ,  NGA ,  RWA ,  SDN ,  SEN ,  SLE ,  SOM ,  SSD ,  STP ,  SWZ ,  SYC ,  TCD ,  TGO , 
#                  TUN ,  UGA ,  ZAF ,  ZMB ,  ZWE ) 

#Prepare the dataframes
#Run Reshaping function
AGO  <- reshapeFunc   (AGO)
BDI  <- reshapeFunc   (BDI)
BEN  <- reshapeFunc   (BEN)
BFA  <- reshapeFunc   (BFA)
BWA  <- reshapeFunc   (BWA)
CAF  <- reshapeFunc   (CAF)
CIV  <- reshapeFunc   (CIV)
CMR  <- reshapeFunc   (CMR)
COD  <- reshapeFunc   (COD)
COG  <- reshapeFunc   (COG)
COM  <- reshapeFunc   (COM)
CPV  <- reshapeFunc   (CPV)
DJI  <- reshapeFunc   (DJI)
DZA  <- reshapeFunc   (DZA)
EGY  <- reshapeFunc   (EGY)
ERI  <- reshapeFunc   (ERI)
ETH  <- reshapeFunc   (ETH)
GAB  <- reshapeFunc   (GAB)
GHA  <- reshapeFunc   (GHA)
GIN  <- reshapeFunc   (GIN)
GMB  <- reshapeFunc   (GMB)
GNB  <- reshapeFunc   (GNB)
GNQ  <- reshapeFunc   (GNQ)
KEN  <- reshapeFunc   (KEN)
LBR  <- reshapeFunc   (LBR)
LBY  <- reshapeFunc   (LBY)
LSO  <- reshapeFunc   (LSO)
MAR  <- reshapeFunc   (MAR)
MDG  <- reshapeFunc   (MDG)
MLI  <- reshapeFunc   (MLI)
MOZ  <- reshapeFunc   (MOZ)
MRT  <- reshapeFunc   (MRT)
MUS  <- reshapeFunc   (MUS)
MWI  <- reshapeFunc   (MWI)
NAM  <- reshapeFunc   (NAM)
NER  <- reshapeFunc   (NER)
NGA  <- reshapeFunc   (NGA)
RWA  <- reshapeFunc   (RWA)
SDN  <- reshapeFunc   (SDN)
SEN  <- reshapeFunc   (SEN)
SLE  <- reshapeFunc   (SLE)
SOM  <- reshapeFunc   (SOM)
SSD  <- reshapeFunc   (SSD)
STP  <- reshapeFunc   (STP)
SWZ  <- reshapeFunc   (SWZ)
SYC  <- reshapeFunc   (SYC)
TCD  <- reshapeFunc   (TCD)
TGO  <- reshapeFunc   (TGO)
TUN  <- reshapeFunc   (TUN)
UGA  <- reshapeFunc   (UGA)
ZAF  <- reshapeFunc   (ZAF)
ZMB  <- reshapeFunc   (ZMB)
ZWE  <- reshapeFunc   (ZWE)


#Run Function to make first row column names ---------------------------------------------------------------

AGO  <-  changecolFunc   (AGO)
BDI  <-  changecolFunc   (BDI)
BEN  <-  changecolFunc   (BEN)
BFA  <-  changecolFunc   (BFA)
BWA  <-  changecolFunc   (BWA)
CAF  <-  changecolFunc   (CAF)
CIV  <-  changecolFunc   (CIV)
CMR  <-  changecolFunc   (CMR)
COD  <-  changecolFunc   (COD)
COG  <-  changecolFunc   (COG)
COM  <-  changecolFunc   (COM)
CPV  <-  changecolFunc   (CPV)
DJI  <-  changecolFunc   (DJI)
DZA  <-  changecolFunc   (DZA)
EGY  <-  changecolFunc   (EGY)
ERI  <-  changecolFunc   (ERI)
ETH  <-  changecolFunc   (ETH)
GAB  <-  changecolFunc   (GAB)
GHA  <-  changecolFunc   (GHA)
GIN  <-  changecolFunc   (GIN)
GMB  <-  changecolFunc   (GMB)
GNB  <-  changecolFunc   (GNB)
GNQ  <-  changecolFunc   (GNQ)
KEN  <-  changecolFunc   (KEN)
LBR  <-  changecolFunc   (LBR)
LBY  <-  changecolFunc   (LBY)
LSO  <-  changecolFunc   (LSO)
MAR  <-  changecolFunc   (MAR)
MDG  <-  changecolFunc   (MDG)
MLI  <-  changecolFunc   (MLI)
MOZ  <-  changecolFunc   (MOZ)
MRT  <-  changecolFunc   (MRT)
MUS  <-  changecolFunc   (MUS)
MWI  <-  changecolFunc   (MWI)
NAM  <-  changecolFunc   (NAM)
NER  <-  changecolFunc   (NER)
NGA  <-  changecolFunc   (NGA)
RWA  <-  changecolFunc   (RWA)
SDN  <-  changecolFunc   (SDN)
SEN  <-  changecolFunc   (SEN)
SLE  <-  changecolFunc   (SLE)
SOM  <-  changecolFunc   (SOM)
SSD  <-  changecolFunc   (SSD)
STP  <-  changecolFunc   (STP)
SWZ  <-  changecolFunc   (SWZ)
SYC  <-  changecolFunc   (SYC)
TCD  <-  changecolFunc   (TCD)
TGO  <-  changecolFunc   (TGO)
TUN  <-  changecolFunc   (TUN)
UGA  <-  changecolFunc   (UGA)
ZAF  <-  changecolFunc   (ZAF)
ZMB  <-  changecolFunc   (ZMB)
ZWE  <-  changecolFunc   (ZWE)

#Some  country datasets lack some variables, adding them where missing ---------------------------------------------------------------
BDI <- BDI %>% mutate (`Global Hunger Index` = NA, `Prevalance of undernourishment (% of population)` = NA, `Cereal import dependency ratio (%)` = NA)
CAF <- CAF %>% mutate (`Domestic food price volatility (index)` = NA, `Total fertilizer consumption (kilogram per hectare)`  = NA)
CIV <- CIV %>% mutate (`Domestic food price volatility (index)` = NA, `Prevalance of undernourishment (% of population)` = NA)
COD <- COD %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA, `Domestic food price volatility (index)` = NA, `Global Hunger Index` = NA, `Prevalance of undernourishment (% of population)` = NA, 
                       `Cereal import dependency ratio (%)` = NA)
COM <- COM %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA, `Domestic food price volatility (index)` = NA, `Total fertilizer consumption (kilogram per hectare)` = NA, `Global Hunger Index` = NA, 
                       `Prevalance of undernourishment (% of population)` = NA, `Cereal import dependency ratio (%)`  = NA, `GINI index` = NA)
CPV <- CPV %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA, `Total fertilizer consumption (kilogram per hectare)`  = NA, `Global Hunger Index` = NA, `Prevalance of undernourishment (% of population)` = NA,
                       `Prevalence of underweight, weight for age (% of children under 5)` = NA, `Prevalence of stunting, height for age (% of children under 5)` = NA, 
                       `Prevalence of wasting (% of children under 5)`= NA, `GINI index` = NA)
DJI <- DJI %>% mutate (`Agriculture, value added (constant 2010 USD, million)`  = NA, `Agriculture value added per worker (constant 2010 USD)` = NA,
                       `Prevalence of underweight, weight for age (% of children under 5)`  = NA, `Agriculture value added per hectare of agricultural land (constant 2010 USD)` = NA,
                       `Domestic food price volatility (index)`  = NA, `Government agriculture expenditure (% of agriculture value added)`= NA, 
                       `Household consumption expenditure per capita (constant 2010 USD)` = NA, `Total fertilizer consumption (kilogram per hectare)` = NA, 
                       `Agricultural value added (% GDP)` = NA, `Gross Domestic Product (constant 2010 US$, billion)` = NA)
ERI <- ERI %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA, `Domestic food price volatility (index)`  = NA, `Global Hunger Index` = NA, `Prevalance of undernourishment (% of population)` = NA,
                       `Government agriculture expenditure (% of agriculture value added)`  = NA, `Household consumption expenditure per capita (constant 2010 USD)` = NA,
                       `Total fertilizer consumption (kilogram per hectare)` = NA, `Agricultural value added (% GDP)` = NA, `Gross Domestic Product (constant 2010 US$, billion)` = NA,
                       `Cereal import dependency ratio (%)` = NA, `GINI index` = NA)
GAB <- GAB %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA, `Government agriculture expenditure (constant 2010 US$, billion)`  = NA, `Government agriculture expenditure (% of total expenditure)` = NA,
                       `Government agriculture expenditure (% of agriculture value added)`= NA, `GINI index` = NA)
GNB <- GNB %>% mutate (`Domestic food price volatility (index)`  = NA, `Total fertilizer consumption (kilogram per hectare)`  = NA)
GNQ <- GNQ %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA, `GINI index` = NA,`Yield, milk (whole fresh cow, kilograms per head)` = NA, `Domestic food price volatility (index)` = NA, `Total fertilizer consumption (kilogram per hectare)`  = NA,
                       `Global Hunger Index` = NA, `Prevalance of undernourishment (% of population)` = NA, `Cereal import dependency ratio (%)` = NA)
LBR <- LBR %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA, `GINI index` = NA, `Domestic food price volatility (index)` = NA, `Total fertilizer consumption (kilogram per hectare)` = NA)  
LBY <- LBY %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA, `GINI index` = NA, `Domestic food price volatility (index)` = NA, `Government agriculture expenditure (% of total expenditure)` = NA, `Government agriculture expenditure (% of total expenditure)` = NA,
                       `Government agriculture expenditure (constant 2010 US$, billion)` = NA, `Government agriculture expenditure (% of agriculture value added)`  = NA,
                       `Global Hunger Index` = NA, `Prevalance of undernourishment (% of population)`  = NA, `Cereal import dependency ratio (%)` = NA)
LSO <- LSO %>% mutate (`Total fertilizer consumption (kilogram per hectare)`  = NA)
MRT <- MRT %>% mutate (`Total fertilizer consumption (kilogram per hectare)`  = NA)
MUS <- MUS %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA, `Prevalence of underweight, weight for age (% of children under 5)`   = NA, `Prevalence of stunting, height for age (% of children under 5)` = NA,
                       `Prevalence of wasting (% of children under 5)` = NA)
SDN <- SDN %>% mutate (`Domestic food price volatility (index)` = NA)
SLE <- SLE %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA, `Total fertilizer consumption (kilogram per hectare)`  = NA)  
SOM <- SOM %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA, `GINI index` = NA, `Agriculture, value added (constant 2010 USD, million)`  = NA, `Agriculture value added per worker (constant 2010 USD)` = NA, 
                       `Agriculture value added per hectare of agricultural land (constant 2010 USD)` = NA, `Intra-African agricultural trade, exports (constant 2010 US$, million)` = NA,
                       `GDP per capita (constant 2010 USD)` = NA, `Intra-African agricultural trade, imports (constant 2010 USD, million)` = NA,
                       `Domestic food price volatility (index)` = NA, `Government agriculture expenditure (constant 2010 US$, billion)` = NA,
                       `Government agriculture expenditure (% of total expenditure)` = NA, `Government agriculture expenditure (% of agriculture value added)` = NA, 
                       `Agricultural ODA (% total ODA)` = NA, `General government gross debt (% of GDP)` = NA, `General government gross revenue (% of GDP)` = NA, 
                       `Household consumption expenditure per capita (constant 2010 USD)` = NA, `Agricultural exports (% of total merchandise exports)`  = NA,
                       `Agricultural imports (% of total merchandise imports)` = NA, `Total fertilizer consumption (kilogram per hectare)` = NA,
                       `Agricultural value added (% GDP)` = NA, `Gross Domestic Product (constant 2010 US$, billion)` = NA, `Global Hunger Index` = NA, `Prevalance of undernourishment (% of population)` =NA,
                       `Cereal import dependency ratio (%)` = NA)  
SSD <- SSD %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA, `GINI index` = NA, `Agriculture, value added (constant 2010 USD, million)`  = NA, `Agrciulture Production Index Number (2004-2006 = 100) (Int. $)` = NA, 
                       `Agriculture value added per worker (constant 2010 USD)` = NA, `Agriculture value added per hectare of agricultural land (constant 2010 USD)`= NA,
                       ` Yield, meat (indigenous cattle, kilograms per head)`=NA, `Yield, milk (whole fresh cow, kilograms per head)` = NA,
                       `Intra-African agricultural trade, exports (constant 2010 US$, million)` = NA, `GDP per capita (constant 2010 USD)` = NA, `Intra-African agricultural trade, imports (constant 2010 USD, million)` = NA,
                       `Domestic food price volatility (index)` = NA, `Government agriculture expenditure (constant 2010 US$, billion)` = NA, `Government agriculture expenditure (% of total expenditure)` = NA, 
                       `Government agriculture expenditure (% of agriculture value added)` = NA, `Agricultural ODA (% total ODA)` = NA, `General government gross debt (% of GDP)` = NA, 
                       `General government gross revenue (% of GDP)`  = NA, `Household consumption expenditure per capita (constant 2010 USD)` = NA, 
                       `Agricultural exports (% of total merchandise exports)` = NA, `Agricultural imports (% of total merchandise imports)`= NA, `Total fertilizer consumption (kilogram per hectare)`= NA,
                       `Agricultural value added (% GDP)` = NA, `Gross Domestic Product (constant 2010 US$, billion)` = NA, `Global Hunger Index` = NA, 
                       `Prevalance of undernourishment (% of population)` = NA, `Cereal import dependency ratio (%)` = NA)  
STP <- STP %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA, `Household consumption expenditure per capita (constant 2010 USD)`   = NA, `Total fertilizer consumption (kilogram per hectare)` = NA, 
                       `Gross Domestic Product (constant 2010 US$, billion)` = NA, `Global Hunger Index` = NA, `Cereal import dependency ratio (%)` =NA)  
SWZ <- SWZ %>% mutate (`Domestic food price volatility (index)` = NA, `Total fertilizer consumption (kilogram per hectare)` = NA)
SYC <- SYC %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA, `Employment rate (% of population, 15+ years)`  = NA, `Global Hunger Index` = NA, `Prevalance of undernourishment (% of population)` = NA,
                       `Prevalence of underweight, weight for age (% of children under 5)`= NA, `Prevalence of stunting, height for age (% of children under 5)` = NA, 
                       `Prevalence of wasting (% of children under 5)` = NA, `Cereal import dependency ratio (%)` = NA, `Employment rate (% of labor force, 15-64 years)` = NA)
TCD <- TCD %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA, `Total fertilizer consumption (kilogram per hectare)`  = NA)
ZWE <- ZWE %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA, `Domestic food price volatility (index)` = NA, `GINI index` = NA)
AGO <- AGO %>% mutate (`GINI index` = NA)
DZA <- DZA %>% mutate (`GINI index` = NA)
ETH <- ETH %>% mutate (`GINI index` = NA)
SDN <- SDN %>% mutate (`GINI index` = NA, `Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA)
AGO <- AGO %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA)
COG <- COG %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA)
DZA <- DZA %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA)
NAM <- NAM %>% mutate (`Poverty Headcount Ratio at $1.90 a day (2011 PPP) (%)` = NA)
#Run renameFunc  ---------------------------------------------------------------
AGO  <-  renameFunc   (AGO)
BDI  <-  renameFunc   (BDI)
BEN  <-  renameFunc   (BEN)
BFA  <-  renameFunc   (BFA)
BWA  <-  renameFunc   (BWA)
CAF  <-  renameFunc   (CAF)
CIV  <-  renameFunc   (CIV)
CMR  <-  renameFunc   (CMR)
COD  <-  renameFunc   (COD)
COG  <-  renameFunc   (COG)
COM  <-  renameFunc   (COM)
DJI  <-  renameFunc   (DJI)
CPV  <-  renameFunc   (CPV)
DZA  <-  renameFunc   (DZA)
EGY  <-  renameFunc   (EGY)
ERI  <-  renameFunc   (ERI)
ETH  <-  renameFunc   (ETH)
GAB  <-  renameFunc   (GAB)
GHA  <-  renameFunc   (GHA)
GIN  <-  renameFunc   (GIN)
GMB  <-  renameFunc   (GMB)
GNB  <-  renameFunc   (GNB)
GNQ  <-  renameFunc   (GNQ)
KEN  <-  renameFunc   (KEN)
LBR  <-  renameFunc   (LBR)
LBY  <-  renameFunc   (LBY)
LSO  <-  renameFunc   (LSO)
MAR  <-  renameFunc   (MAR)
MDG  <-  renameFunc   (MDG)
MLI  <-  renameFunc   (MLI)
MOZ  <-  renameFunc   (MOZ)
MRT  <-  renameFunc   (MRT)
MUS  <-  renameFunc   (MUS)
MWI  <-  renameFunc   (MWI)
NAM  <-  renameFunc   (NAM)
NER  <-  renameFunc   (NER)
NGA  <-  renameFunc   (NGA)
RWA  <-  renameFunc   (RWA)
SDN  <-  renameFunc   (SDN)
SEN  <-  renameFunc   (SEN)
SLE  <-  renameFunc   (SLE)
SOM  <-  renameFunc   (SOM)
SSD  <-  renameFunc   (SSD)
STP  <-  renameFunc   (STP)
SWZ  <-  renameFunc   (SWZ)
SYC  <-  renameFunc   (SYC)
TCD  <-  renameFunc   (TCD)
TGO  <-  renameFunc   (TGO)
TUN  <-  renameFunc   (TUN)
UGA  <-  renameFunc   (UGA)
ZAF  <-  renameFunc   (ZAF)
ZMB  <-  renameFunc   (ZMB)
ZWE  <-  renameFunc   (ZWE)

#Combine all the country datasets  --------------------------------------------------------------------------------------------
africa <- as_tibble(combine(AGO ,BDI ,BEN ,BFA ,BWA ,CAF ,CIV ,CMR, COD ,COG ,COM ,CPV ,DJI ,DZA ,EGY ,ERI ,ETH , GAB ,
                            GHA ,GIN ,GMB ,GNB ,GNQ ,KEN ,LBR, LBY ,LSO ,MAR ,MDG ,MLI ,MOZ ,MRT ,MUS ,MWI,NAM ,NER ,
                            NGA ,RWA ,SDN,SEN,  SLE ,SOM ,SSD ,STP ,SWZ ,SYC ,TCD ,TGO,  TUN ,UGA,  ZAF ,ZMB ,ZWE)  ) %>%
  rename(country_code = source) %>%
  mutate(country_code = as.character(country_code))

#Population data from World bank   ------------------------------------------------------------------------------------------
african_countries <- c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD", "COM", "COD", "COG", "CIV", "DJI", "EGY", "GNQ", "ERI", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB",
                       "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM", "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "SWZ",
                       "TZA", "TGO", "TUN", "UGA", "ZMB", "ZWE")

population_2017 <- API_SP_POP_TOTL_DS2_en_excel_v2_10499320 <- read_excel("API_SP.POP.TOTL_DS2_en_excel_v2_10499320.xls") %>%
  setNames(as.character(.[3,])) %>%
  select(country_code = 'Country Code', '2017',
         population   = '2017') %>%
  slice(-1:-3) %>%
  filter(country_code %in% african_countries)


#Import World Bank's country classification by income   ------------------------------------------------------------------------------------------
# Middle east countries to filter out
middle_east <- c('Jordan', 'Bahrain', 'Lebanon', 'Yemen, Rep.', 'Bahrain','Iran, Islamic Rep.', 'Kuwait', 'Saudi Arabia', 'United Arab Emirates', 'West Bank and Gaza', 'Malta', 'Syrian Arab Republic', 'Iran', 'Islamic Rep.', 'Oman', 'Iraq', 'Qatar', 'Israel')

library(readxl)
income_class <-  read_excel("CLASS.xls") %>%
  setNames(as.character(.[4,]))%>%
  slice(-1:-5) %>%
  filter()

income_class <- income_class[,3:9] %>%
  select(country      = 'Economy',
         country_code = 'Code',
         region       = 'Region', 
         income_group = 'Income group') %>%
  filter(region       == 'Sub-Saharan Africa' | region == 'Middle East & North Africa',
         !country %in% middle_east) 

#Join population and income class
income_class <-  full_join(x = income_class, y = population_2017, by = ('country_code'))

#Join africa with country_class
africa <- inner_join (x = africa, y = income_class, by = ('country_code')) %>%
  arrange(country, year) %>%
  mutate_at(vars(poverty, stunting_per_under5, gov_exp_agric_per_tot_exp, gdp_capita, glob_hunger_index, 
            cereal_imp_dep_ratio,population, gov_gross_debt_per_gdp), funs(as.numeric)) %>%
  mutate(econ_growth= gdp_capita - lag(gdp_capita)) %>%
  mutate(econ_growth_rate = (econ_growth/lag(gdp_capita))*100) %>% 
  mutate(country = case_when(country == "Swaziland" ~ "Eswatini",
                             country == "Gambia, The" ~ "Gambia",
                             country == "Egypt, Arab Rep." ~ "Egypt", 
                             TRUE ~ country))

#Remove all objects except africa dataframe
rm(list=setdiff(ls(), "africa" ))


#Household consumption expenditure and stunting of under 5
hhconsexp <- africa    %>%
  filter(year == 2017) %>% 
  select(year, country, stunting_per_under5, income_group, hh_cons_exp_capita, population)  %>% 
  drop_na() %>% 
  mutate(ln_hh_cons_exp_capita = log(as.numeric(hh_cons_exp_capita))) %>% 
  ggplot(aes(ln_hh_cons_exp_capita, stunting_per_under5, fill = income_group)) + geom_point(shape = 21,  size= 20, show.legend = FALSE, alpha=0.7) +
  scale_fill_manual(breaks  = c("Low income", "Lower middle income", "Upper middle income"),
                     values = c("#e1191b", "#ffdc45", "#0775ea"))    +
  theme_minimal(base_size   = 10)                                    +
  scale_y_continuous(limits = c(-20,64), breaks  = c(0, 20, 40, 60)) +
  scale_x_continuous(limits = c(4.8,9.3), breaks   = c(4,5,6,7,8)) +
  theme_bw()                                                         +
  theme(panel.background    = element_rect(fill = "#fffbf5"))        +
  theme(plot.background     = element_rect(fill = "#fffbf5"))        +
  theme(panel.grid.major    = element_line(colour = "grey80", size = 1) ) +
  theme(panel.grid.minor    = element_blank())                       +
  theme(panel.border = element_blank())                              +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.text.y  = element_blank()) +
  geom_segment( aes(x = -Inf, xend = 4.9, y = 0, yend = 0),   color = "#fffbf5", size =1 ) +
  geom_segment( aes(x = -Inf, xend = 5, y = 20, yend = 20),   color = "#fffbf5", size =1 ) +
  geom_segment( aes(x = -Inf, xend = 5, y = 40, yend = 40),   color = "#fffbf5", size =1 ) +
  geom_segment( aes(x = -Inf, xend = 4.9, y = 60, yend = 60), color = "#fffbf5", size =1 ) +
  #geom_segment( aes(x = 4, xend = 4, y = -2, yend  = -Inf),   color = "#fffbf5", size =1 ) +
  geom_segment( aes(x = 5, xend = 5, y = -2, yend  = -Inf),   color = "#fffbf5", size =1 ) +
  geom_segment( aes(x = 6, xend = 6, y = 0,  yend  = -Inf),   color = "#fffbf5", size =1 ) +
  geom_segment( aes(x = 7, xend = 7, y = 0,  yend  = -Inf),   color = "#fffbf5", size =1 ) +
  geom_segment( aes(x = 8, xend = 8, y = -2,  yend  = -Inf),   color = "#fffbf5", size =1 ) +
  geom_segment( aes(x = 9, xend = 9, y = -2, yend  = -Inf),   color = "#fffbf5", size =1 ) +
  geom_segment( aes(x = 4, xend = 4, y = 62, yend  =  Inf),   color = "#fffbf5", size =1 ) +
  geom_segment( aes(x = 5, xend = 5, y = 62, yend  =  Inf),   color = "#fffbf5", size =1 ) +
  geom_segment( aes(x = 6, xend = 6, y = 62, yend  =  Inf),   color = "#fffbf5", size =1 ) +
  geom_segment( aes(x = 7, xend = 7, y = 62, yend  =  Inf),   color = "#fffbf5", size =1 ) +
  geom_segment( aes(x = 8, xend = 8, y = 62, yend  =  Inf),   color = "#fffbf5", size =1 ) +
  #
  geom_segment( aes(x = 7, xend = 7, y = 50, yend = 59),   color = "#fffbf5", size =1 ) +
  geom_segment( aes(x = 8.6, xend = Inf, y = 0, yend = 0),   color = "#fffbf5", size =1 ) +
  geom_segment( aes(x = 8.6, xend = Inf, y = 20, yend = 20),   color = "#fffbf5", size =1 ) +
  geom_segment( aes(x = 8.6, xend = Inf, y = 40, yend = 40),   color = "#fffbf5", size =1 ) +
  geom_segment( aes(x = 8.6, xend = Inf, y = 60, yend = 60), color = "#fffbf5", size =1 ) +
  geom_segment( aes(x = 8, xend = 8, y = 41, yend = 59), color = "#fffbf5", size =1 ) +
  annotate(geom = 'text', x = 4.81,   y = 30, label = "Prevalence of stunting (%) (2017)", angle = 90, size = 16) + 
  annotate(geom = "text", x = 6.5,     y = -2.2, label = "Log of household consumption expenditure per capita (2017)", color = "black", size = 16) +
  annotate(geom = "text", x = 5,     y = -3, label = "5",  color = "black", size = 12) +
  annotate(geom = "text", x = 8,     y = -3, label = "8",  color =" black", size = 12) +
  annotate(geom = "text", x = 4.8,   y =  0, label = "0",  color =" black", size = 12) +
  annotate(geom = "text", x = 4.8,   y = 60, label = "60", color =" black", size = 12) +
  annotate(geom = "text", x = 6.93, y = 64, label = "African Countries Have Made Progess in Reducing Child Malnutrition", color =" black", size = 20, fontface = "bold") +
  annotate(geom = "text", x = 6.2, y = 58, hjust = 0, parse = T, label = '"But " * phantom("low income countries ")', color = "black", size = 12, family = "Andale Mono") +
  annotate(geom = "text", x = 7.70, y = 58, label = "still have high rates of child stunting", color = "black", size = 12, family = "Andale Mono") +
  annotate(geom = "text", x = 6.2, y = 58, hjust = 0, parse = T, label = 'phantom("But ")*"low income countries "', color = "#e1191b", size = 12, family = "Andale Mono") +
  annotate(geom = "text", x = 7.3, y = 55, label = "What is the implication of the association between stunting", color = "black", size = 12, family = "Andale Mono") +
  annotate(geom = "text", x = 7.5, y = 52, label = "rates and household consumption expenditure?", color = "black", size = 12, family = "Andale Mono") +
  annotate(geom = "text", x = 5.49, y = 12, label = "Consumer goods price hikes could", color = "black", size = 7, family = "Andale Mono") +
  annotate(geom = "text", x = 5.44, y = 10, label = "negatively affect child nutrition", color = "black", size = 7, family = "Andale Mono") +
  annotate(geom = "text", x = 5.48, y = 8, label  = "especially in low income countries", color = "black", size = 7, family = "Andale Mono") +
  annotate(geom = "text", x = 7.8, y = 48, label = "Countries are grouped and coloured by World Bank 2018 income groups:", color = "black", size = 7, family = "Andale Mono") +
  annotate(geom = "text", x = 7.05, y = 46, hjust = 0, parse = T, label = '"23 countries are classfied as " * phantom("low income,")', color = "black", size = 7, family = "Andale Mono") +
  annotate(geom = "text", x = 7.05, y = 46, hjust = 0, parse = T, label = 'phantom("23 countries are classfied as ") * "low income,"', color = "#e1191b", size = 7, family = "Andale Mono")+
  annotate(geom = "text", x = 7.05, y = 44, hjust = 0, parse = T, label = '"15 countries are classfied as " * phantom("lower middle income,")', color = "black", size = 7, family = "Andale Mono") +
  annotate(geom = "text", x = 7.05, y = 44, hjust = 0, parse = T, label = 'phantom("15 countries are classfied as ") * "lower middle income,"', color = "#ffdc45", size = 7, family = "Andale Mono") +
  annotate(geom = "text", x = 7.05, y = 42, hjust = 0, parse = T, label = '"and 6 countries as " * phantom("upper middle income ")', color = "black", size = 7, family = "Andale Mono") +
  annotate(geom = "text", x = 7.05, y = 42, hjust = 0, parse = T, label = 'phantom("and 6 countries as ") * "upper middle income "', color = "#0775ea", size = 7, family = "Andale Mono") +
  annotate(geom = "text", x = 5.39,  y = 58,  label = "Burundi", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 5.33,  y = 41,  label = "Zambia", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 8.17,  y = 7,  label = "Tunisia", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 5.46,  y = 47.5,  label = "Niger", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 5.98,  y = 41,  label = "Congo, Dem. Rep.", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 5.97,  y = 43,  label = "Central Afican Rep.", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 5.97,  y = 46.4,  label = "Madagascar", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 6.4,  y = 43,  label = "Sierra Leone", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 5.73,  y = 37,  label = "Ethiopia", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 6.23,  y = 38.5,  label = "Rwanda", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 6.6,  y = 38.7,  label = "Chad", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 5.73,  y = 35,  label = "Mozambique", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 6.34,  y = 36.5,  label = "Benin", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 6.32,  y = 34.5,  label = "Quinea", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 5.78,  y = 29,  label = "Burkina Faso", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 6.13,  y = 33,  label = "Liberia", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 6.1,  y = 29.9,  label = "Uganda", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 6.4,  y = 27,  label = "Quinnea-Bissau", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 6.33,  y = 30.2,  label = "Mali", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 6.6,  y = 30.4,  label = "Zimbabwe", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 6.65,  y = 35.5,  label = "Comoros", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 6.23,  y = 24,  label = "Togo", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 6.8,  y = 29,  label = "Kenya", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 6.8,  y = 34,  label = "Cote d'Ivoire", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 7.1,  y = 30.4,  label = "Cameroon", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 6.31,  y = 19,  label = "Gambia", size= 6, fontface = "italic", family='Montserrat')+
  annotate(geom = "text", x = 6.5,  y = 23,  label = "Mauritania", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 6.82,  y = 19,  label = "Senegal", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 7.13,  y = 23,  label = "Congo Rep.", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 7.1,  y = 38.7,  label = "Lesotho", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 7.38,  y = 36.5,  label = "Sudan", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 7.75,  y = 36.5,  label = "Nigeria", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 7.99,  y = 29,  label = "Angola", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 7.85,  y = 26,  label = "Eswatini", size= 6, fontface = "italic", family='Montserrat')+
  annotate(geom = "text", x = 7.73,  y = 10.85,  label = "Morocco", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 7.91,  y = 21,  label = "Egypt", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 7.91,  y = 15,  label = "Algeria", size= 6, fontface = "italic", family='Montserrat')+
  annotate(geom = "text", x = 8.32,  y = 30,  label = "Botswana", size= 6, fontface = "italic", family='Montserrat')+
  annotate(geom = "text", x = 8.43,  y = 25,  label = "South Africa", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 8.43,  y = 23,  label = "Namibia", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 8.4,  y = 19,  label = "Equaotorial Quinea", size= 6, fontface = "italic", family='Montserrat') +
  annotate(geom = "text", x = 8.36,  y = 14,  label = "Gabon", size= 6, fontface = "italic", family='Montserrat')+
  annotate(geom = "text", x = 7.47,  y = 18,  label = "Ghana", size= 6, fontface = "italic", family='Montserrat') 

png('rdc.png', width=3000, height=1680, res=100)
print(hhconsexp)    
dev.off() 
#------------------------------------------------------------------

# Plotting map
countries_2017 <- africa %>%
  filter(year  == 2017)  %>% 
  select(country, stunting_per_under5, income_group, country_code)  %>% 
  drop_na()

sPDF <- joinCountryData2Map(countries_2017, joinCode = "ISO3", nameJoinColumn = "country_code", mapResolution='low', verbose=T)
#sPDF <-sPDF[which(sPDF$ADMIN!='Antarctica'),]
sPDF <- spTransform(sPDF, CRS=CRS("+proj=robin +ellps=WGS84"))

#pdf('F:/wmap.pdf', width=12, height=7) 
#png('F:/wmap.png', width=1680, height=1050, res=96) 

col1<-rgb(225, 25, 27, alpha=255, max=255)
col2<-rgb(30, 93, 160, alpha=255, max=255)
col3<-rgb(255, 220, 69, alpha=255, max=255)
bg.col<-rgb(255, 251, 245, alpha=255,max=255)

png('amap_stunt.png', width=3000, height=1550, res=96) 
par(mai=c(0,0,0,0),xaxs="i",yaxs="i")

mapCountryData( sPDF, 
                nameColumnToPlot="income_group",
                addLegend=F,
                colourPalette=c(col1, col3,col2), borderCol='grey82',
                mapTitle='', 
                oceanCol=bg.col, 
                missingCountryCol='white',
                lwd=0.1,
                catMethod = 'categorical')
dev.off() 

#------------------------------------------------------------------
#Changes in stunting rates 2017-2007
png("stunt_change_ridges.png",width=600, height=400, res=96)

col1a<-rgb(225, 25, 27, alpha=195, max=255)
col2a<-rgb(30, 93, 160, alpha=195, max=255)
col3a<-rgb(255, 220, 69, alpha=195, max=255)
col3b<-rgb(248, 202, 8, alpha=255, max=255)

change_stunt <- africa %>%
  filter(year  == 2017 | year  == 2007)  %>% 
  select(year, country, stunting_per_under5, income_group, country_code) %>%
  drop_na() %>%
  spread(year, stunting_per_under5) %>% 
  rename(stunted_2007 = "2007",
         stunted_2017 = "2017") %>% 
  mutate(change_stunt = stunted_2017-stunted_2007) %>% 
  ggplot(aes(x=change_stunt, y=income_group, fill = income_group, color = income_group)) + 
  geom_density_ridges(scale = 4) +
  scale_fill_cyclical(values = c(col1a,col3a, col2a)) +
  scale_color_cyclical(values = c(col1,col2, col3))   +
  scale_x_continuous(breaks = c(-12, -8,  -4,  0)) +#(limits=c(-13,2), expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) + 
  labs(title = 'Stunting rates have reduced', x='Change in stunting rates (%), 2007-2017')+
  theme_ridges(font_size = 20, grid = TRUE) + theme(plot.margin = unit(c(1,1,1,1), "lines"),
                                                    #axis.title=element_text(size=16.5),
                                                    plot.title = element_text(color='black', size=24, face="bold", hjust=0),
                                                    plot.subtitle = element_text(color='black', size=14, face="italic"),
                                                    plot.background = element_rect(fill='#fffbf5', colour='black'),
                                                    axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                                                    axis.title.y = element_blank(), axis.title.x = element_text(color='black',hjust=0.5)) +
  coord_cartesian(ylim = c(1, 7), clip = 'off') 
 
print(change_stunt)    
dev.off() 

#------------------------------------------------------------------
#Changes in hunger rates 2017-2007
png("hunger_change_ridges.png",width=600, height=400, res=96)
change_hungerind <- africa %>%
  filter(year  == 2017 | year  == 2007)  %>% 
  select(year, country, glob_hunger_index, income_group, country_code) %>%
  drop_na() %>%
  spread(year, glob_hunger_index) %>% 
  rename(hungry_2007 = "2007",
         hungry_2017 = "2017") %>% 
  mutate(change_hunger = hungry_2017-hungry_2007) %>% 
  filter(!is.na(change_hunger)) %>% 
  ggplot(aes(x=change_hunger, y=income_group, fill = income_group, color = income_group)) + 
  geom_density_ridges(scale = 4) +
  scale_fill_cyclical(values = c(col1a,col3a, col2a)) +
  scale_color_cyclical(values = c(col1,col2, col3))   +
  scale_x_continuous(breaks = c(-12, -8,  -4,  0)) +#(limits=c(-13,2), expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) + 
  labs(title = 'Hunger has reduced', x='Change in global hunger index, 2007-2017')+
  theme_ridges(font_size = 20, grid = TRUE) + theme(plot.margin = unit(c(1,1,1,1), "lines"), 
                                                    plot.title = element_text(color='black', size=24, face="bold", hjust=0),
                                                    plot.subtitle = element_text(color='black', size=14, face="italic"),
                                                    plot.background = element_rect(fill='#fffbf5', colour='black'),
                                                    axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                                                    axis.title.y = element_blank(), axis.title.x = element_text(color='black',hjust=0.5)) +
  coord_cartesian(ylim = c(1, 7), clip = 'off') 

print(change_hungerind)    
dev.off() 

#------------------------------------------------------------------
#Changes in wasting rates 2017-2007
png("wasting_ridges.png",width=600, height=400, res=96)
change_wasting <- africa %>%
  filter(year  == 2017 | year  == 2007)  %>% 
  select(year, country, wasting_per_under5, income_group, country_code) %>%
  mutate(wasting_per_under5 = as.numeric(wasting_per_under5)) %>% 
  drop_na() %>%
  spread(year, wasting_per_under5) %>% 
  rename(wasted_2007 = "2007",
         wasted_2017 = "2017") %>% 
  mutate(change_wasting = wasted_2017-wasted_2007) %>% 
  filter(!is.na(change_wasting)) %>% 
  ggplot(aes(x=change_wasting, y=income_group, fill = income_group, color = income_group)) + 
  geom_density_ridges(scale = 4) +
  scale_fill_cyclical(values = c(col1a,col3a, col2a)) +
  scale_color_cyclical(values = c(col1,col2, col3))   +
  scale_x_continuous(breaks = c(-12, -8,  -4,  0, 4)) +#(limits=c(-13,2), expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.2, 0)) + 
  #annotate(geom = "text", x = -4.5,   y = 7.5, label = "Wasting rates increased",  color = "black", size = 9) +
  #annotate(geom = "text", x = -6.7,   y = 6.8, label = "in 11 countries",  color = "black", size = 9) +
  annotate(geom = "text", x = -8,     y = 0.8, label = "-8",  color = "black", size = 6) +
  annotate(geom = "text", x = -4,     y = 0.8, label = "-4",  color = "black", size = 6) +
  annotate(geom = "text", x =  0,     y = 0.8, label = "0",  color = "black", size = 6) +
  annotate(geom = "text", x =  4,     y = 0.8, label = "4",  color = "black", size = 6) +
  annotate(geom = "text", x = -2,     y = 0.3, label = "Change in rates of wasting (%), 2007-2017",  color = "black", size = 6) +
  labs(x='')+
  theme_ridges(font_size = 20, grid = TRUE) + theme(plot.margin = unit(c(1,1,1,1), "lines"),
                                                    plot.background = element_rect(fill='#fffbf5', colour='black'),
                                                    axis.text.y=element_blank(), 
                                                    axis.ticks.y=element_blank(),
                                                    axis.text.x=element_blank(),
                                                    axis.ticks.x=element_blank(),
                                                    axis.title.x = element_blank(),
                                                    axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(1, 7), clip = 'off') 

print(change_wasting)    
dev.off() 


#Checking change in wasting
x <- africa %>%
  filter(year  == 2017 | year  == 2007)  %>% 
  select(year, country, wasting_per_under5, income_group, country_code) %>%
  mutate(wasting_per_under5 = as.numeric(wasting_per_under5)) %>% 
  drop_na() %>%
  spread(year, wasting_per_under5) %>% 
  rename(underwt_2007 = "2007",
         underwt_2017 = "2017") %>% 
  mutate(change_ws = underwt_2017-underwt_2007) %>% 
  filter(!is.na(change_ws)) 
a <- x %>% filter(change_ws>0) %>% group_by(income_group) %>%  summarise(mn = min(change_ws), mx=max(change_ws), nn=n())
#------------------------------------------------------------------
#Changes in underweight rates 2017-2007
png("underweight_ridges.png",width=600, height=400, res=96)
change_underwt <- africa %>%
  filter(year  == 2017 | year  == 2007)  %>% 
  select(year, country, underweight_per_under5, income_group, country_code) %>%
  mutate(underweight_per_under5 = as.numeric(underweight_per_under5)) %>% 
  drop_na() %>%
  spread(year, underweight_per_under5) %>% 
  rename(underwt_2007 = "2007",
         underwt_2017 = "2017") %>% 
  mutate(change_underwt = underwt_2017-underwt_2007) %>% 
  filter(!is.na(change_underwt)) %>% 
  ggplot(aes(x=change_underwt, y=income_group, fill = income_group, color = income_group)) + 
  geom_density_ridges(scale = 4) +
  scale_fill_cyclical(values = c(col1a,col3a, col2a)) +
  scale_color_cyclical(values = c(col1,col2, col3))   +
  scale_x_continuous(breaks = c(-12, -8,  -4,  0, 4)) +#(limits=c(-13,2), expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.7, 0)) +
  #labs(x='Change in rates of underweight (%), 2007-2017')+
  theme_ridges(font_size = 20, grid = TRUE) + 
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        plot.background = element_rect(fill='#fffbf5', colour='black'),
        axis.text.y=element_text(size=20), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
  coord_cartesian(ylim = c(1, 7), clip = 'off') 
print(change_underwt)    
dev.off()
  
  
  
  
  
  
  
  theme(plot.margin = unit(c(1,1,1,1), "lines"), 
                                                    plot.background = element_rect(fill='#fffbf5', colour='black'),
                                                    axis.text.y=element_blank(),
                                                    axis.text.x=element_blank(),
                                                    axis.ticks.y=element_blank(),
                                                    axis.ticks.x=element_blank(),
                                                    axis.title.y = element_blank(), 
                                                    axis.title.x = element_blank(),
                                                    axis.title.x = element_text(color=dark.color,hjust=0.5)) +
  coord_cartesian(ylim = c(1, 4), clip = 'off') +
  annotate(geom = "text", x = 4,  y = -0.00000001,  label = "4", size= 4, family='Montserrat') 

  

print(change_underwt)    
dev.off() 

#Checking distribution of underweight changes
x <- africa %>%
  filter(year  == 2017 | year  == 2007)  %>% 
  select(year, country, underweight_per_under5, income_group, country_code) %>%
  mutate(underweight_per_under5 = as.numeric(underweight_per_under5)) %>% 
  drop_na() %>%
  spread(year, underweight_per_under5) %>% 
  rename(underwt_2007 = "2007",
         underwt_2017 = "2017") %>% 
  mutate(change_underwt = underwt_2017-underwt_2007) %>% 
  filter(!is.na(change_underwt)) 

a <- x %>% filter(change_underwt>0) %>% group_by(income_group) %>%  summarise(mn = min(change_underwt), mx=max(change_underwt), nn=n())
####combining charts
plot <- image_read(paste0("rdc.png"))
# Bring in map
map <- image_read("amap_stunt.png") %>% 
  image_resize(geometry_size_percent(50)) %>% 
  image_crop("15000x8000+500") %>% 
  image_crop("500x5000+50") %>% 
  image_crop("500x5000+50") %>% 
  image_crop("400x550+20") %>% 
  image_crop("300x-50+-200") 

#Changes in rates
stunt <- image_read("stunt_change_ridges.png")
hunger <- image_read("hunger_change_ridges.png")
resakss <- image_read("resakss.png") %>%  
  image_resize(geometry_size_percent(91)) 
resakss <- image_fill(resakss, "#fffbf5") %>% 
  image_background("#fffbf5")
  
underweight <-image_read("underweight_ridges.png") 
wasting <-image_read("wasting_ridges.png") 

  #image_resize(geometry_size_percent(100))
# Stack them on top of each other
luke_afr_nutrition_state <- image_composite(plot, map, offset = "+2600+130") 
luke_afr_nutrition_state <- image_composite(luke_afr_nutrition_state, stunt, offset = "+2450+475")
luke_afr_nutrition_state <- image_composite(luke_afr_nutrition_state, hunger, offset = "+2440+870")
luke_afr_nutrition_state <- image_composite(luke_afr_nutrition_state, resakss, offset = "+2560+1320")
luke_afr_nutrition_state <- image_composite(luke_afr_nutrition_state, underweight, offset = "+1080+1310")
luke_afr_nutrition_state <- image_composite(luke_afr_nutrition_state, wasting, offset = "+1800+1305")

luke_afr_nutrition_state <- luke_afr_nutrition_state %>% 
  image_annotate("-12", gravity = "None", size= 25, font='Montserrat', location = "+1390+1630") %>% 
  image_annotate("-8", gravity = "None", size= 25, font='Montserrat', location = "+1440+1630") %>% 
  image_annotate("-4", gravity = "None", size= 25, font='Montserrat', location = "+1490+1630") %>% 
  image_annotate("0", gravity = "None", size= 25, font='Montserrat', location = "+1530+1630") %>% 
  image_annotate("4", gravity = "None", size= 25, font='Montserrat', location = "+1570+1630") %>% 
  image_annotate("Change in underweight rates, 2007-2018", gravity = "None", size= 25, font='Montserrat', location = "+1200+1660") %>% 
  image_annotate("Underweight rates have reduced",             gravity = "None", size= 35, font='Montserrat', location = "+980+1350") %>%  
  image_annotate("Wasting rates increased in 11 countries", gravity = "None", size= 35, font='Montserrat', location = "+1780+1350") %>%
  image_annotate("Notes", gravity = "None", size= 45, font='Montserrat', location = "+150+1540") %>% 
  image_annotate("1. Prevalence indicators refer to % of children under the age of 5 years", gravity = "None", size= 25, font='Montserrat', location = "+150+1570") %>% 
  image_annotate("2. Household consumption expenditure per capita is constant 2010 USD", gravity = "None", size= 25, font='Montserrat', location = "+150+1600") %>%
  image_annotate("3. The map of Africa is colored by income group, white space indicates missing data", gravity = "None", size= 25, font='Montserrat', location = "+150+1630") %>% 
  image_annotate("4. Data: Regional Strategic Analysis and Knowledge Support Support Systems (ReSAKSS)", gravity = "None", size= 25, font='Montserrat', location ="+150+1660")

image_write(luke_afr_nutrition_state, path = "luke_afr_nutrition_state.png", format = "png")
rm(luke_afr_nutrition_state)
