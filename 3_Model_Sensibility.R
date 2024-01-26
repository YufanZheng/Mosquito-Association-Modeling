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
#   RR: Retail and recreation index
#   GP: Grocery and pharmacy index
#   R: Residential index
#   TS: Transit stations index
#   W: Workplaces index
#   

dir.create("Model/Number of mosquitoes per 1000 traps/nbinomial/Sen/")
dir.create("Model/AOI full model/Sen/")

###################################################
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

formula1 <- Y2 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1 + basis_mt1 + f(S, model = "iid") + R

##########sensitivity of the mean temperature lag within the best model (Model A-R) ################################
name1 <- "A-R_temp"

nlag_r <- 6
basis_tr1 <- lag_variable_rainfall_3(df=df,nlag=nlag_r)
# ----------
nlag_t <- 2
lab1 <- paste0(name1,nlag_t)
basis_mt1 <- lag_variable_temp_3(df=df,nlag=nlag_t)
model <- mymodel(formula1, data = df3, family = "nbinomial")
save(model, file = paste0("Model/Number of mosquitoes per 1000 traps/nbinomial/Sen/", lab1,".RData"))

# ----------
nlag_t <- 1
lab2 <- paste0(name1,nlag_t)
basis_mt1 <- lag_variable_temp_3(df=df,nlag=nlag_t)
model <- mymodel(formula1, data = df3, family = "nbinomial")
save(model, file = paste0("Model/Number of mosquitoes per 1000 traps/nbinomial/Sen/", lab2,".RData"))


labs <- c(lab1,lab2)

table_nb <- data.table(Model  = labs,
                       DIC = NA,
                       logscore = NA,
                       WAIC = NA)

for(i in 1:length(labs)){
  load(paste0("Model/Number of mosquitoes per 1000 traps/nbinomial/Sen/",labs[i],".RData"))
  table_nb$DIC[i] <- round(model$dic$dic, 4)
  table_nb$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
  table_nb$WAIC[i] <- round(model$waic$waic, 4)
}

fwrite(table_nb, file = paste0("Model/Number of mosquitoes per 1000 traps/nbinomial/Sen/",name1,"sensibility_temp.csv"), quote = FALSE, row.names = FALSE)



##########sensitivity of the total rainfall lag within the best model (Model A-R) ################################
name1 <- "A-R_rain"

nlag_t <- 2
basis_mt1 <- lag_variable_temp_3(df=df,nlag=nlag_t)

# ----------
nlag_r <- 6
basis_tr1 <- lag_variable_rainfall_3(df=df,nlag=nlag_r)
lab1 <- paste0(name1,nlag_r)
model <- mymodel(formula1, data = df4, family = "nbinomial")
save(model, file = paste0("Model/Number of mosquitoes per 1000 traps/nbinomial/Sen/", lab1,".RData"))

# ----------
nlag_r <- 5
basis_tr1 <- lag_variable_rainfall_3(df=df,nlag=nlag_r)
lab2 <- paste0(name1,nlag_r)
model <- mymodel(formula1, data = df4, family = "nbinomial")
save(model, file = paste0("Model/Number of mosquitoes per 1000 traps/nbinomial/Sen/", lab2,".RData"))

# ----------
nlag_r <- 4
basis_tr1 <- lag_variable_rainfall_3(df=df,nlag=nlag_r)
lab3 <- paste0(name1,nlag_r)
model <- mymodel(formula1, data = df4, family = "nbinomial")
save(model, file = paste0("Model/Number of mosquitoes per 1000 traps/nbinomial/Sen/", lab3,".RData"))

# ----------
nlag_r <- 3
basis_tr1 <- lag_variable_rainfall_3(df=df,nlag=nlag_r)
lab4 <- paste0(name1,nlag_r)
model <- mymodel(formula1, data = df4, family = "nbinomial")
save(model, file = paste0("Model/Number of mosquitoes per 1000 traps/nbinomial/Sen/", lab4,".RData"))

# ----------
nlag_r <- 2
basis_tr1 <- lag_variable_rainfall_3(df=df,nlag=nlag_r)
lab5 <- paste0(name1,nlag_r)
model <- mymodel(formula1, data = df4, family = "nbinomial")
save(model, file = paste0("Model/Number of mosquitoes per 1000 traps/nbinomial/Sen/", lab5,".RData"))

# ----------
nlag_r <- 1
basis_tr1 <- lag_variable_rainfall_3(df=df,nlag=nlag_r)
lab6 <- paste0(name1,nlag_r)
model <- mymodel(formula1, data = df4, family = "nbinomial")
save(model, file = paste0("Model/Number of mosquitoes per 1000 traps/nbinomial/Sen/", lab6,".RData"))


labs <- c(lab1,lab2,lab3,lab4,lab5,lab6)


table_nb <- data.table(Model  = labs, 
                       DIC = NA,
                       logscore = NA,
                       WAIC = NA)

for(i in 1:length(labs)){
  load(paste0("Model/Number of mosquitoes per 1000 traps/nbinomial/Sen/",labs[i],".RData"))
  table_nb$DIC[i] <- round(model$dic$dic, 4)
  table_nb$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
  table_nb$WAIC[i] <- round(model$waic$waic, 4)
}

fwrite(table_nb, file = paste0("Model/Number of mosquitoes per 1000 traps/nbinomial/Sen/",name1,"sensibility_rain.csv"), quote = FALSE, row.names = FALSE)



#############################################################
dir.create("Model/")
dir.create("Model/AOI full model/")

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


formula2 <- Y1 ~ 1 + f(T1, model = "iid") + f(T2, model = "iid") + basis_tr1 + basis_mt1 + f(S, model = "iid") + R

##########sensitivity of the mean temperature lag within the best model (Model E-R) ################################
name2 <- "E-R_temp"

nlag_r <- 6
basis_tr1 <- lag_variable_rainfall_3(df=df,nlag=nlag_r)

# ----------
nlag_t <- 2
lab1 <- paste0(name2,nlag_t)
basis_mt1 <- lag_variable_temp_3(df=df1,nlag=nlag_t)
model <- mymodel(formula2, data = df4, family = "binomial")
save(model, file = paste0("Model/AOI full model/Sen/", lab1,".RData"))

# ----------
nlag_t <- 1
lab2 <- paste0(name2,nlag_t)
basis_mt1 <- lag_variable_temp_3(df=df1,nlag=nlag_t)
model <- mymodel(formula2, data = df4, family = "binomial")
save(model, file = paste0("Model/AOI full model/Sen/", lab2,".RData"))


labs <- c(lab1,lab2)

table_aoi <- data.table(Model  = labs,
                         DIC = NA,
                         logscore = NA,
                         WAIC = NA)

for(i in 1:length(labs)){
  load(paste0("Model/AOI full model/Sen/",labs[i],".RData"))
  table_aoi$DIC[i] <- round(model$dic$dic, 4)
  table_aoi$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
  table_aoi$WAIC[i] <- round(model$waic$waic, 4)
}

fwrite(table_aoi, file = paste0("Model/AOI full model/Sen/",name2,"sensibility_temp.csv"), quote = FALSE, row.names = FALSE)


##########sensitivity of the total rainfall lag within the best model (Model E-R) ################################
name2 <- "E-R_rain"
#####################################################
nlag_t <- 2
basis_mt1 <- lag_variable_temp_3(df=df,nlag=nlag_t)

# ----------
nlag_r <- 6
basis_tr1 <- lag_variable_rainfall_3(df=df,nlag=nlag_r)
lab1 <- paste0(name2,nlag_r)
model <- mymodel(formula2, data = df4, family = "binomial")
save(model, file = paste0("Model/AOI full model/Sen/", lab1,".RData"))

# ----------
nlag_r <- 5
basis_tr1 <- lag_variable_rainfall_3(df=df,nlag=nlag_r)
lab2 <- paste0(name2,nlag_r)
model <- mymodel(formula2, data = df4, family = "binomial")
save(model, file = paste0("Model/AOI full model/Sen/", lab2,".RData"))

# ----------
nlag_r <- 4
basis_tr1 <- lag_variable_rainfall_3(df=df,nlag=nlag_r)
lab3 <- paste0(name2,nlag_r)
model <- mymodel(formula2, data = df4, family = "binomial")
save(model, file = paste0("Model/AOI full model/Sen/", lab3,".RData"))

# ----------
nlag_r <- 3
basis_tr1 <- lag_variable_rainfall_3(df=df,nlag=nlag_r)
lab4 <- paste0(name2,nlag_r)
model <- mymodel(formula2, data = df4, family = "binomial")
save(model, file = paste0("Model/AOI full model/Sen/", lab4,".RData"))

# ----------
nlag_r <- 2
basis_tr1 <- lag_variable_rainfall_3(df=df,nlag=nlag_r)
lab5 <- paste0(name2,nlag_r)
model <- mymodel(formula2, data = df4, family = "binomial")
save(model, file = paste0("Model/AOI full model/Sen/", lab5,".RData"))

# ----------
nlag_r <- 1
basis_tr1 <- lag_variable_rainfall_3(df=df,nlag=nlag_r)
lab6 <- paste0(name2,nlag_r)
model <- mymodel(formula2, data = df4, family = "binomial")
save(model, file = paste0("Model/AOI full model/Sen/", lab6,".RData"))


labs <- c(lab1,lab2,lab3,lab4,lab5,lab6)

table_aoi <- data.table(Model  = labs, 
                        DIC = NA,
                        logscore = NA,
                        WAIC = NA)

for(i in 1:length(labs)){
  load(paste0("Model/AOI full model/Sen/",labs[i],".RData"))
  table_aoi$DIC[i] <- round(model$dic$dic, 4)
  table_aoi$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
  table_aoi$WAIC[i] <- round(model$waic$waic, 4)
}

fwrite(table_aoi, file = paste0("Model/AOI full model/Sen/",name2,"sensibility_rain.csv"), quote = FALSE, row.names = FALSE)

