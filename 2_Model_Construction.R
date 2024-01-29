setwd(dirname(parent.frame(2)$filename))
path <-  getwd()

#############################################
## load packages and data
source(paste(path,"/1_Load_packages_data.R",sep=""))
# set the random seed
set.seed(124)

setwd(path)


###################################################
# The meaning of variables:
#   Y1: AOI/AGI
#   Y2: mosquito numbers
#   f(T1, model = "iid"): for random effect to account for inter-annual variability 
#   f(T2, model = "iid"): for random effect to account for annual cycle (seasonality)
#   S: for district difference
#   basis_mt1: lagged variable of mean temperature
#   basis_tr1: lagged variable of total rainfall
#   P: Parks index
#   R: Residential index
#   W: Workplaces index
#   

###################################################
## Model: To estimate the number of mosquitoes per 1000 traps during the period from April 2020 to August 2022

formula1 <- Y2 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + f(S, model = "iid")
formula2 <- Y2 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1 + f(S, model = "iid")
formula3 <- Y2 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1 + basis_mt1 + f(S, model = "iid")
formula4 <- Y2 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1 + basis_mt1 + f(S, model = "iid") + P
formula5 <- Y2 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1 + basis_mt1 + f(S, model = "iid") + R
formula6 <- Y2 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1 + basis_mt1 + f(S, model = "iid") + W

# create a list of formulas
f1 <- list(formula1,formula2,formula3,formula4,formula5,formula6)
formulas1 <- c(f1)

# create model label string
lab1 <- c("model1","model2", "model3", "model4","model5","model6")
lab <- c(lab1)

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
mymodel <- function(formula, data = df3, family = "nbinomial", config = FALSE){
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

dir.create("Model/")
dir.create("Model/Number of mosquitoes per 1000 traps/")
dir.create("Model/Number of mosquitoes per 1000 traps/nbinomial/")

models <- lapply(1:length(formulas1), 
                 function(i) {
                   print(i)
                   model <- mymodel(formulas1[[i]], data = df4, family = "nbinomial")
                   save(model, file = paste0("Model/Number of mosquitoes per 1000 traps/nbinomial/", lab[i],".RData"))})

tab1 <- c("T2+T1+S","T2+T1+S+tr","T2+T1+S+tr+mt","T2+T1+S+tr+mt+P","T2+T1+S+tr+mt+R","T2+T1+S+tr+mt+W")

table_nb <- data.table(Model  = c(tab1), 
                       DIC = NA,
                       logscore = NA,
                       WAIC = NA)

for(i in 1:length(formulas1)){
  load(paste0("Model/Number of mosquitoes per 1000 traps/nbinomial/",lab[i],".RData"))
  table_nb$DIC[i] <- round(model$dic$dic, 4)
  table_nb$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
  table_nb$WAIC[i] <- round(model$waic$waic, 4)
}


fwrite(table_nb, file = "Model/Number of mosquitoes per 1000 traps/nummosquitoes_model_selection(nbinomial)-all-1.csv", quote = FALSE,
       row.names = FALSE)


#############################################################
## Model: To estimate AOI during the period from April 2020 to August 2022
formula1 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + f(S, model = "iid")
formula2 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1 + f(S, model = "iid")
formula3 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1 + basis_mt1 + f(S, model = "iid")
formula4 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1 + basis_mt1 + f(S, model = "iid") + P
formula5 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1 + basis_mt1 + f(S, model = "iid") + R
formula6 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1 + basis_mt1 + f(S, model = "iid") + W

# create a list of formulas
f2 <- list(formula1,formula2,formula3,formula4,formula5,formula6)
formulas2 <- c(f2)

# create model label string
lab2 <- c("model1","model2", "model3", "model4","model5","model6")
lab <- c(lab2)

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
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

dir.create("Model/")
dir.create("Model/AOI full model/")

models <- lapply(1:length(formulas2), 
                 function(i) {
                   print(i)
                   model <- mymodel(formulas2[[i]], data = df4, family = "binomial")
                   save(model, file = paste0("Model/AOI full model/", lab[i],".RData"))})

tab2 <- c("T2+T1+S","T2+T1+S+tr","T2+T1+S+tr+mt","T2+T1+S+tr+mt+P","T2+T1+S+tr+mt+R","T2+T1+S+tr+mt+W")

table_aoi <- data.table(Model  = c(tab2), 
                         DIC = NA,
                         logscore = NA,
                         WAIC = NA)

for(i in 1:length(formulas2)){
  load(paste0("Model/AOI full model/",lab[i],".RData"))
  table_aoi$DIC[i] <- round(model$dic$dic, 4)
  table_aoi$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
  table_aoi$WAIC[i] <- round(model$waic$waic, 4)
}


fwrite(table_aoi, file = "Model/AOI full model/AOI full model selection-all-1.csv", quote = FALSE, row.names = FALSE)


