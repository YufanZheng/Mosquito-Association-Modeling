setwd(dirname(parent.frame(2)$filename))
path <-  getwd()


library(scales)
# load INLA
library(INLA)

#  select other packages
packages <- c("data.table", "tidyverse", "sf", "sp", "spdep",
              "dlnm", "tsModel", "hydroGOF","RColorBrewer", 
              "geofacet", "ggpubr", "ggthemes")

# load packages
lapply(packages, library, character.only = TRUE)

## load data
## Climate data and mosquitoes index data included for 2010 to 2022.08
setwd(paste(path,"/Data/",sep=""))
df <- read.csv("Three districts all dataset 20230129.csv")

df$Date <- as.Date(df$Date)
df$year <- as.factor(format(as.Date(df$Date), "%Y"))
df$month <- as.factor(format(as.Date(df$Date), "%m"))
df$District <- as.factor(df$District)
str(df)

## Create lagged variables
## Data from 2020.04 to 2022.08, limited the data period
df1 <- df[df$Date > "2020-03-01",]

lag_variable_temp_3 <- function(df,nlag){
  lag_temp <- tsModel::Lag(df$Meantemp, k = 0:nlag, group=df$District)
  lag_temp <- lag_temp[df$Date > "2020-03-01",]
  lagknot = equalknots(0:nlag, 2)
  var <- lag_temp
  basis_mt1 <- crossbasis(var,
                          argvar = list(fun = "ns", knots = equalknots(df$Meantemp, 2)),
                          arglag = list(fun = "ns", knots = lagknot))
  colnames(basis_mt1) = paste0("basis_mt1.", colnames(basis_mt1))
  return(basis_mt1)
}



lag_variable_rainfall_3 <- function(df,nlag){
  lag_train <- tsModel::Lag(df$Totalrain, k = 0:nlag, group=df$District)
  lag_train <- lag_train[df$Date > "2020-03-01",]
  lagknot = equalknots(0:nlag, 2)
  var <- lag_train
  basis_tr1 <- crossbasis(var,
                          argvar = list(fun = "ns", knots = equalknots(df$Totalrain, 2)),
                          arglag = list(fun = "ns", knots = lagknot))
  colnames(basis_tr1) = paste0("basis_tr1.", colnames(basis_tr1))
  return(basis_tr1)
}


# Mean Temperature
nlag_t <- 2
basis_mt1 <- lag_variable_temp_3(df=df,nlag=nlag_t)

# Total Rainfall
nlag_r <- 6
basis_tr1 <- lag_variable_rainfall_3(df=df,nlag=nlag_r)

# set up data and priors for INLA model
# set data for models
Y1  <- df$Number.of.positive.traps # model 1 response variable of prediction model for mosquito extensiveness
Y2 <- df$Nummosquito.per.1000.traps # model 2 response variable of prediction model for mosquito abundance
# random variable
T1 <- df$month # for random effect to account for inter-annual variability 
T2 <- df$year # for random effect to account for annual cycle (seasonality)
S <- df$District # for district difference
Date <- df$Date
G <- df$meanAOI

trap <- df$Total.traps

# create dataframe for model testing
df2 <- data.frame(Y1, Y2, T1, T2, S, Date,G)

# dataset for model during the period from April 2020 to August 2022
df3 <- df2[df2$Date > "2020-03-01",]

# add the human mobility index to dataset
df_mobility <- read.csv("combined_with_mobility_AOI.csv")
df3$P <- df_mobility$parks
df3$RR <- df_mobility$retail_and_recreation
df3$GP <- df_mobility$grocery_and_pharmacy
df3$R <- df_mobility$residential
df3$TS <- df_mobility$transit_stations
df3$W <- df_mobility$workplaces


df4 <- df3
df4$Total.traps <- df1$Total.traps
df4$meanAOI <- df1$meanAOI
df4$Nummosquito.per.1000.traps <- df1$Nummosquito.per.1000.traps
df4$Totalrain <- df1$Totalrain
df4$Meantemp <- df1$Meantemp

write.csv(df4, "dataset_model.csv",row.names=F)

df_sorted <- df4 %>% arrange(S, Date)
