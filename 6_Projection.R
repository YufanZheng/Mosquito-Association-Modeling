setwd(dirname(parent.frame(2)$filename))
path <-  getwd()

library(scales)
# load INLA
library(INLA)

# set the random seed
set.seed(124)

source(paste(path,"/1_Load_packages_data.R",sep=""))

setwd(path)

# perpare the data
df_original <- df1
df0 <- df1

df0$parks <- df3$P
df0$retail_and_recreation <- df3$RR
df0$grocery_and_pharmacy <- df3$GP
df0$residential <- df3$R
df0$transit_stations <- df3$TS
df0$workplaces <- df3$W

df1$parks <- df3$P
df1$retail_and_recreation <- df3$RR
df1$grocery_and_pharmacy <- df3$GP
df1$residential <- df3$R
df1$transit_stations <- df3$TS
df1$workplaces <- df3$W

rownames(df1) <- 1:nrow(df1)
rownames(df2) <- 1:nrow(df2)
rownames(df3) <- 1:nrow(df3)

# projection based on the Model E-R
projection_E <- function(df0,lab,name,i_low=63,i_up=87,basis_mt1,basis_tr1){
  file_name <- paste0("Model/AOI full model/",lab,".RData")
  df_result <- read.csv(paste("Results/Model",name,"_loocv_normal_result.csv",sep=""))
  sd_value <- sd(df_result$meanAOI)
  load(file_name)
  
  T1_list <- list("1"=model$summary.random$T1$mean[1],"2"=model$summary.random$T1$mean[2],"3"=model$summary.random$T1$mean[3],"4"=model$summary.random$T1$mean[4],"5"=model$summary.random$T1$mean[5],
                  "6"=model$summary.random$T1$mean[6],"7"=model$summary.random$T1$mean[7],"8"=model$summary.random$T1$mean[8],"9"=model$summary.random$T1$mean[9],"10"=model$summary.random$T1$mean[10],
                  "11"=model$summary.random$T1$mean[11],"12"=model$summary.random$T1$mean[12])
  T2_list <- list("2020"=model$summary.random$T2$mean[11],"2021"=model$summary.random$T2$mean[12],"2022"=model$summary.random$T2$mean[13])
  S_list <- list("Hong Kong Island & Kowloon"=model$summary.random$S$mean[1],"New Territories East"=model$summary.random$S$mean[2],"New Territories West"=model$summary.random$S$mean[3])
  
  df4 <- df0
  df4[df4$Date > "2021-12-01",]$residential <- df4[df4$Date > "2020-12-01" & df4$Date < "2021-09-01",]$residential
  projection_link <- numeric(i_up-i_low)
  projection <- numeric(i_up-i_low)
  for (i in i_low:i_up){
    month <- as.character(as.numeric(df4$month[i]))
    year <- as.character(df4$year[i])
    S <- as.character(df4$District[i])
    projection_link[i-i_low+1] <- sum(model$summary.fixed$mean * c(1,basis_tr1[i,],basis_mt1[i,],df4$residential[i])) + T1_list[[month]] + T2_list[[year]] + S_list[[S]]
    projection[i-i_low+1] <- 1 / (1 + exp(-projection_link[i-i_low+1]))
  }
  df_result$projection_1 <- df_result$fittednum
  df_result$projection_1_normal <- df_result$fittednum_normal
  df_result$projection_1[i_low:i_up] <- projection
  df_result$projection_1_normal[i_low:i_up] <- projection / sd_value
  
  df4 <- df0
  df4[df4$Date > "2021-12-01",]$residential <- 0
  projection_link <- numeric(i_up-i_low)
  projection <- numeric(i_up-i_low)
  for (i in i_low:i_up){
    month <- as.character(as.numeric(df4$month[i]))
    year <- as.character(df4$year[i])
    S <- as.character(df4$District[i])
    projection_link[i-i_low+1] <- sum(model$summary.fixed$mean * c(1,basis_tr1[i,],basis_mt1[i,],df4$residential[i])) + T1_list[[month]] + T2_list[[year]] + S_list[[S]]
    projection[i-i_low+1] <- 1 / (1 + exp(-projection_link[i-i_low+1]))
  }
  df_result$projection_2 <- df_result$fittednum
  df_result$projection_2_normal <- df_result$fittednum_normal
  df_result$projection_2[i_low:i_up] <- projection
  df_result$projection_2_normal[i_low:i_up] <- projection / sd_value
  
  write.csv(df_result, paste("results/Model",name,"_loocv_result_projection.csv",sep=""),row.names=F)
}

# projection based on the Model A-R
projection_A <- function(df0,lab,name,i_low=64,i_up=87,basis_mt1,basis_tr1){
  file_name <- paste0("Model/Number of mosquitoes per 1000 traps/nbinomial/",lab,".RData")
  df_result <- read.csv(paste("Results/Model",name,"_loocv_normal_result.csv",sep=""))
  sd_value <- sd(df_result$Nummosquito.per.1000.traps)
  load(file_name)
  
  T1_list <- list("1"=model$summary.random$T1$mean[1],"2"=model$summary.random$T1$mean[2],"3"=model$summary.random$T1$mean[3],"4"=model$summary.random$T1$mean[4],"5"=model$summary.random$T1$mean[5],
                  "6"=model$summary.random$T1$mean[6],"7"=model$summary.random$T1$mean[7],"8"=model$summary.random$T1$mean[8],"9"=model$summary.random$T1$mean[9],"10"=model$summary.random$T1$mean[10],
                  "11"=model$summary.random$T1$mean[11],"12"=model$summary.random$T1$mean[12])
  T2_list <- list("2020"=model$summary.random$T2$mean[11],"2021"=model$summary.random$T2$mean[12],"2022"=model$summary.random$T2$mean[13])
  S_list <- list("Hong Kong Island & Kowloon"=model$summary.random$S$mean[1],"New Territories East"=model$summary.random$S$mean[2],"New Territories West"=model$summary.random$S$mean[3])
  
  df4 <- df0
  df4[df4$Date > "2021-12-01",]$residential <- df4[df4$Date > "2020-12-01" & df4$Date < "2021-09-01",]$residential
  projection_link <- numeric(i_up-i_low)
  projection <- numeric(i_up-i_low)
  for (i in i_low:i_up){
    month <- as.character(as.numeric(df4$month[i]))
    year <- as.character(df4$year[i])
    S <- as.character(df4$District[i])
    projection_link[i-i_low+1] <- sum(model$summary.fixed$mean * c(1,basis_tr1[i,],basis_mt1[i,],df4$residential[i])) + T1_list[[month]] + T2_list[[year]] + S_list[[S]]
    projection[i-i_low+1] <- round(exp(projection_link[i-i_low+1]))
  }
  df_result$projection_1 <- df_result$fittednum
  df_result$projection_1_normal <- df_result$fittednum_normal
  df_result$projection_1[i_low:i_up] <- projection
  df_result$projection_1_normal[i_low:i_up] <- projection / sd_value
  
  df4 <- df0
  df4[df4$Date > "2021-12-01",]$residential <- 0
  projection_link <- numeric(i_up-i_low)
  projection <- numeric(i_up-i_low)
  for (i in i_low:i_up){
    month <- as.character(as.numeric(df4$month[i]))
    year <- as.character(df4$year[i])
    S <- as.character(df4$District[i])
    projection_link[i-i_low+1] <- sum(model$summary.fixed$mean * c(1,basis_tr1[i,],basis_mt1[i,],df4$residential[i])) + T1_list[[month]] + T2_list[[year]] + S_list[[S]]
    projection[i-i_low+1] <- round(exp(projection_link[i-i_low+1]))
  }
  df_result$projection_2 <- df_result$fittednum
  df_result$projection_2_normal <- df_result$fittednum_normal
  df_result$projection_2[i_low:i_up] <- projection
  df_result$projection_2_normal[i_low:i_up] <- projection / sd_value
  
  write.csv(df_result, paste("results/Model",name,"_loocv_result_projection.csv",sep=""),row.names=F)
}


###### Projection results of extensiveness based on the best model
lab <- "model5"
name <- "E-R_1"
projection_E(df0,lab,name,i_low=63,i_up=87,basis_mt1,basis_tr1)


###### Projection results of abundance based on the best model
lab <- "model5"
name <- "A-R_1"
projection_A(df0,lab,name,i_low=63,i_up=87,basis_mt1,basis_tr1)
