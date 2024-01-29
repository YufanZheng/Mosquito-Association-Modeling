setwd(dirname(parent.frame(2)$filename))
path <-  getwd()

library(scales)

# set the random seed
set.seed(124)

source(paste(path,"/1_Load_packages_data.R",sep=""))

setwd(path)

dir.create("results/")

df1$lag1_Meantemp <- df4$lag1_Meantemp
df1$lag2_Meantemp <- df4$lag2_Meantemp

# perpare the data
df_original <- df1


df1$parks <- df3$P
df1$retail_and_recreation <- df3$RR
df1$grocery_and_pharmacy <- df3$GP
df1$residential <- df3$R
df1$transit_stations <- df3$TS
df1$workplaces <- df3$W

df1$lag1_parks <- df3$lag1_P
df1$lag1_retail_and_recreation <- df3$lag1_RR
df1$lag1_grocery_and_pharmacy <- df3$lag1_GP
df1$lag1_residential <- df3$lag1_R
df1$lag1_transit_stations <- df3$lag1_TS
df1$lag1_workplaces <- df3$lag1_W

df1$lag2_parks <- df3$lag2_P
df1$lag2_retail_and_recreation <- df3$lag2_RR
df1$lag2_grocery_and_pharmacy <- df3$lag2_GP
df1$lag2_residential <- df3$lag2_R
df1$lag2_transit_stations <- df3$lag2_TS
df1$lag2_workplaces <- df3$lag2_W

df0 <- df1


# LOOCV function for abundance prediction model and save all the result in csv file
# Args:
  # df0: used for perparing the target variable of data input of LOOCV
  # df1: used for data input of LOOCV
  # formula: The formula of the fitted model
  # model_all: The fitted model which trained by all the data
  # num: The model label number
  # k_fold: The parameter for setting k-fold, for example, if k_fold=1, which means that use LOOCV
loocv_m <- function(df0,df1,formula,model_all,num,k_fold=1){
  filename <- paste("results/Model",num,"_loocv_result.csv",sep="")
  
  if(!file.exists(filename)){
    print(num)
    n <- dim(df1)[1] # number of samples
    loo_pred <- rep(NA, n)
    loo_predmin <- rep(NA, n)
    loo_predmax <- rep(NA, n)
    
    ## model function
    mymodel <- function(formula, data = df2, family = "nbinomial", config = FALSE){
      model <- inla(formula = formula, data = data, family = family,control.family = list(link = "log"),
                    control.inla = list(strategy = 'adaptive'), 
                    control.compute = list(dic = TRUE, config = config, 
                                           cpo = TRUE, return.marginals = FALSE, waic = TRUE),
                    control.fixed = list(correlation.matrix = TRUE, 
                                         prec.intercept = 1, prec = 1),
                    control.predictor = list(link = 1, compute = TRUE), 
                    verbose = FALSE)
      model <- inla.rerun(model)
      return(model)
    }
    
    ## loocv
    for(i in 1:n) {
      # The number "3" in here means that 3 different districts
      if(i%%(3*k_fold) == 1){
        i_up <- i+(3*k_fold-1)
        if(i+3*k_fold>n){
          i_up <- n
        }
        print(i_up)
        # If y[i] = NA, this means that y[i] is not observed, hence gives no contribution to the likelihood.
        # reference: https://www.r-inla.org/faq    please see question 7
        nummosquito_i <- replace(df0$Nummosquito.per.1000.traps, c(i:i_up), "NA")
        df1$Nummosquito.per.1000.traps <- as.numeric(nummosquito_i)
        dlnm_i <- mymodel(formula, data = df1, family = "nbinomial") #glm with out ith row
        loo_pred[i:i_up] <- dlnm_i$summary.fitted.values$mean[i:i_up]
        loo_predmin[i:i_up] <- dlnm_i$summary.fitted.values$`0.025quant`[i:i_up]
        loo_predmax[i:i_up] <- dlnm_i$summary.fitted.values$`0.975quant`[i:i_up]
      }
    }
    
    df0$fittednum <- round(model_all$summary.fitted.values$mean)
    df0$predictednum <- loo_pred
    df0$predictedmin <- loo_predmin
    df0$predictedmax <- loo_predmax
    
    write.csv(df0, filename,row.names=F)
  }
  
}


# LOOCV function for extensiveness prediction model and save all the result in csv file
# Args:
  # df0: used for perparing the target variable of data input of LOOCV
  # df1: used for data input of LOOCV
  # formula: The formula of the fitted model
  # model_all: The fitted model which trained by all the data
  # num: The model label number
  # k_fold: The parameter for setting k-fold, for example, if k_fold=1, which means that use LOOCV
loocv_a <- function(df0,df1,formula,model_all,num,k_fold=1){
  filename <- paste("results/Model",num,"_loocv_result.csv",sep="")
  
  if(!file.exists(filename)){
    print(num)
    n <- dim(df1)[1] # number of samples
    loo_pred <- rep(NA, n)
    loo_predmin <- rep(NA, n)
    loo_predmax <- rep(NA, n)
    
    # model function
    mymodel <- function(formula, data = df3, family = "binomial", config = FALSE){
      model <- inla(formula = formula, data = data, family = family, Ntrials = df1$Total.traps,control.family = list(link = "logit"),
                    control.inla = list(strategy = 'adaptive'), 
                    control.compute = list(dic = TRUE, config = config, 
                                           cpo = TRUE, return.marginals = FALSE, waic = TRUE),
                    control.fixed = list(correlation.matrix = TRUE, 
                                         prec.intercept = 1, prec = 1),
                    control.predictor = list(link = 1, compute = TRUE), 
                    verbose = FALSE)
      model <- inla.rerun(model)
      return(model)
    }
    
    ## loocv
    for(i in 1:n) {
      if(i%%(3*k_fold) == 1){
        i_up <- i+(3*k_fold-1)
        if(i+3*k_fold>n){
          i_up <- n
        }
        # If y[i] = NA, this means that y[i] is not observed, hence gives no contribution to the likelihood.
        # reference: https://www.r-inla.org/faq    please see question 7
        print(i_up)
        nummosquito_i <- replace(df0$Number.of.positive.traps, c(i:i_up), "NA")
        df1$Number.of.positive.traps <- as.numeric(nummosquito_i)
        dlnm_i <- mymodel(formula, data = df1, family = "binomial") #glm with out ith row
        loo_pred[i:i_up] <- dlnm_i$summary.fitted.values$mean[i:i_up]
        loo_predmin[i:i_up] <- dlnm_i$summary.fitted.values$`0.025quant`[i:i_up]
        loo_predmax[i:i_up] <- dlnm_i$summary.fitted.values$`0.975quant`[i:i_up]
      }
    }
    
    df0$fittednum <- model_all$summary.fitted.values$mean
    df0$predictednum <- loo_pred
    df0$predictedmin <- loo_predmin
    df0$predictedmax <- loo_predmax
    
    write.csv(df0, filename,row.names=F)
  }
}



## CV for Model E
load("Model/AOI full model/model3.RData")
model_all <- model
formula <- Number.of.positive.traps ~ 1 + f(year, model = "iid") + f(month, model = "iid") + basis_tr1 + basis_mt1 + f(District, model = "iid")
num = "E_1"
loocv_a(df0,df1,formula,model_all,num,k_fold=1)

## CV for Model E-P
load("Model/AOI full model/model4.RData")
model_all <- model
formula <- Number.of.positive.traps ~ 1 + f(year, model = "iid") + f(month, model = "iid") + basis_tr1 + basis_mt1 + f(District, model = "iid") + parks
num = "E-P_1"
loocv_a(df0,df1,formula,model_all,num,k_fold=1)

## CV for Model E-R
load("Model/AOI full model/model5.RData")
model_all <- model
formula <- Number.of.positive.traps ~ 1 + f(year, model = "iid") + f(month, model = "iid") + basis_tr1 + basis_mt1 + f(District, model = "iid") + residential
num = "E-R_1"
loocv_a(df0,df1,formula,model_all,num,k_fold=1)

## CV for Model E-W
load("Model/AOI full model/model6.RData")
model_all <- model
formula <- Number.of.positive.traps ~ 1 + f(year, model = "iid") + f(month, model = "iid") + basis_tr1 + basis_mt1 + f(District, model = "iid") + workplaces
num = "E-W_1"
loocv_a(df0,df1,formula,model_all,num,k_fold=1)



###############################################

## CV for Model A
load("Model/Number of mosquitoes per 1000 traps/nbinomial/model3.RData")
model_all <- model
formula <- Nummosquito.per.1000.traps ~ 1 + f(year, model = "iid") + f(month, model = "iid") + basis_tr1 + basis_mt1 + f(District, model = "iid")
num = "A_1"
loocv_m(df0,df1,formula,model_all,num,k_fold=1)

## CV for Model A-P
load("Model/Number of mosquitoes per 1000 traps/nbinomial/model4.RData")
model_all <- model
formula <- Nummosquito.per.1000.traps ~ 1 + f(year, model = "iid") + f(month, model = "iid") + basis_tr1 + basis_mt1 + f(District, model = "iid") + parks
num = "A-P_1"
loocv_m(df0,df1,formula,model_all,num,k_fold=1)

## CV for Model A-R
load("Model/Number of mosquitoes per 1000 traps/nbinomial/model5.RData")
model_all <- model
formula <- Nummosquito.per.1000.traps ~ 1 + f(year, model = "iid") + f(month, model = "iid") + basis_tr1 + basis_mt1 + f(District, model = "iid") + residential
num = "A-R_1"
loocv_m(df0,df1,formula,model_all,num,k_fold=1)

## CV for Model A-W
load("Model/Number of mosquitoes per 1000 traps/nbinomial/model6.RData")
model_all <- model
formula <- Nummosquito.per.1000.traps ~ 1 + f(year, model = "iid") + f(month, model = "iid") + basis_tr1 + basis_mt1 + f(District, model = "iid") + workplaces
num = "A-W_1"
loocv_m(df0,df1,formula,model_all,num,k_fold=1)

