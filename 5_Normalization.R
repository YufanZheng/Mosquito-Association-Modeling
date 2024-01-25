setwd(dirname(parent.frame(2)$filename))
path <-  getwd()


## Note: To compare the predictiveness of ovitrap and gravidtrap, we rescale the number of mosquitoes per 1000 traps and AOI to a 0-1 range.
## Then compare the mean squared error after leave-one-out cross validation
library(scales)
library(ggplot2)

# The function for assessment of normalize the result (Z-score, Min-max normalized, Proposed normalize method) and calculate the MSE result
# Args:
  # p1: The file path of the result
  # y: The target variable of model, if y=1, the target variable is AOI/AGI, else the target variable is mosquito numbers
  # num: The model label number
  # name: The name of model
# Returns:
#   result: It is a list including the normalized result and MSE result
assessment <- function(p1,y,num,name){
  df <- read.csv(p1)
  df$Date <- as.Date(df$Date)
  # if y=1, then model is predicting AOI/AGI, else is predicting Mosquito numbers
  if(y==1){
    sd_value <- sd(df$meanAOI)
    min_value <- min(df$meanAOI)
    max_value <- max(df$meanAOI)
    mean_value <- mean(df$meanAOI)
    real_value <- df$meanAOI
    df$meanAOI_normal <- df$meanAOI / sd_value
  }else{
    sd_value <- sd(df$Nummosquito.per.1000.traps)
    min_value <- min(df$Nummosquito.per.1000.traps)
    max_value <- max(df$Nummosquito.per.1000.traps)
    mean_value <- mean(df$Nummosquito.per.1000.traps)
    real_value <- df$Nummosquito.per.1000.traps
    df$Nummosquito.per.1000.traps_normal <- df$Nummosquito.per.1000.traps / sd_value
  } 
  # proposed normalize method
  df$fittednum_normal <- df$fittednum / sd_value
  df$predictednum_normal  <- df$predictednum / sd_value
  df$predictedmin_normal <- df$predictedmin / sd_value
  df$predictedmax_normal <- df$predictedmax / sd_value
  
  if(y==1){
    MSE_pre <- mean((df$meanAOI_normal - df$predictednum_normal) ^ 2)
    MSE_fit <- mean((df$meanAOI_normal - df$fittednum_normal) ^ 2)

  }else{
    MSE_pre <- mean((df$Nummosquito.per.1000.traps_normal - df$predictednum_normal) ^ 2)
    MSE_fit <- mean((df$Nummosquito.per.1000.traps_normal - df$fittednum_normal) ^ 2)
  }
  mse_result <- c(paste("Model",num,sep=""),MSE_pre,MSE_fit,paste("Model",name,sep=" "))
  result <- list(df=df,mse_result=mse_result)
  return(result)
}


# set the random seed
set.seed(1234)

# setwd("D:/Bio/Dengue-Gravidtrap-and-Ovitrap-HK-main_20230211-add_mobility-all/")
setwd(path)

MSE <- c()
n_row <- 0

## Model E
num = "E_1"
y <- 1
name <- "E_1"
result <- assessment(p1=paste("results/Model",num,"_loocv_result.csv",sep=""),y=y,num=num,name=name)
MSE <- append(result$mse_result,MSE)
write.csv(result$df,paste("results/Model",num,"_loocv_normal_result.csv",sep=""),row.names=F)
n_row <- n_row+1

## Model E-R
num = "E-R_1"
y <- 1
name <- "E-R_1"
result <- assessment(p1=paste("results/Model",num,"_loocv_result.csv",sep=""),y=y,num=num,name=name)
MSE <- append(result$mse_result,MSE)
write.csv(result$df,paste("results/Model",num,"_loocv_normal_result.csv",sep=""),row.names=F)
n_row <- n_row+1

## Model E-P
num = "E-P_1"
y <- 1
name <- "E-P_1"
result <- assessment(p1=paste("results/Model",num,"_loocv_result.csv",sep=""),y=y,num=num,name=name)
MSE <- append(result$mse_result,MSE)
write.csv(result$df,paste("results/Model",num,"_loocv_normal_result.csv",sep=""),row.names=F)
n_row <- n_row+1

## Model E-W
num = "E-W_1"
y <- 1
name <- "E-W_1"
result <- assessment(p1=paste("results/Model",num,"_loocv_result.csv",sep=""),y=y,num=num,name=name)
MSE <- append(result$mse_result,MSE)
write.csv(result$df,paste("results/Model",num,"_loocv_normal_result.csv",sep=""),row.names=F)
n_row <- n_row+1

######################################
# save all the result into csv file
MSE_matrix <- matrix(MSE, nrow=n_row, ncol=4, byrow=TRUE)
df_result <- as.data.frame(MSE_matrix)
colnames(df_result) <- c("Model","MSE_pre","MSE_fit","Name")

write.csv(df_result, "Results/MSE_normal_result_1.csv",row.names=F)



MSE <- c()
n_row <- 0

## Model A
num = "A_1"
y <- 2
name <- "A_1"
result <- assessment(p1=paste("results/Model",num,"_loocv_result.csv",sep=""),y=y,num=num,name=name)
MSE <- append(result$mse_result,MSE)
write.csv(result$df,paste("results/Model",num,"_loocv_normal_result.csv",sep=""),row.names=F)
n_row <- n_row+1

## Model A-R
num = "A-R_1"
y <- 2
name <- "A-R_1"
result <- assessment(p1=paste("results/Model",num,"_loocv_result.csv",sep=""),y=y,num=num,name=name)
MSE <- append(result$mse_result,MSE)
write.csv(result$df,paste("results/Model",num,"_loocv_normal_result.csv",sep=""),row.names=F)
n_row <- n_row+1

## Model A-P
num = "A-P_1"
y <- 2
name <- "A-P_1"
result <- assessment(p1=paste("results/Model",num,"_loocv_result.csv",sep=""),y=y,num=num,name=name)
MSE <- append(result$mse_result,MSE)
write.csv(result$df,paste("results/Model",num,"_loocv_normal_result.csv",sep=""),row.names=F)
n_row <- n_row+1


## Model A-W
num = "A-W_1"
y <- 2
name <- "A-W_1"
result <- assessment(p1=paste("results/Model",num,"_loocv_result.csv",sep=""),y=y,num=num,name=name)
MSE <- append(result$mse_result,MSE)
write.csv(result$df,paste("results/Model",num,"_loocv_normal_result.csv",sep=""),row.names=F)
n_row <- n_row+1


######################################
# save all the result into csv file
MSE_matrix <- matrix(MSE, nrow=n_row, ncol=4, byrow=TRUE)
df_result <- as.data.frame(MSE_matrix)
colnames(df_result) <- c("Model","MSE_pre","MSE_fit","Name")

write.csv(df_result, "Results/MSE_normal_result_2.csv",row.names=F)


