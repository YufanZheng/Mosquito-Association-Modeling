setwd(dirname(parent.frame(2)$filename))
path <-  getwd()


library(ggplot2)
library(scales)
library(latex2exp)

#############################################
## load packages and data
# source("D:/Bio/Dengue-Gravidtrap-and-Ovitrap-HK-main_20230211-add_mobility-all//01_load_packages_data.R")
source(paste(path,"/01_load_packages_data.R",sep=""))
# set the random seed
set.seed(1234)

# setwd("D:/Bio/Dengue-Gravidtrap-and-Ovitrap-HK-main_20230211-add_mobility-all/")
setwd(path)

####### Figure 1 #######

dir.create("Paper/")

setwd(path)

filename <- "Data/Three districts all dataset.csv"


# load the climate data
df <- read.csv(filename)
df$Date <- as.Date(df$Date)

####### Figure 2 #######
df_weather <- df[df$Date > "2019-09-01",]
df_weather$District[df_weather$District=="HKK"] = "Hong Kong Island & Kowloon"
df_weather$District[df_weather$District=="NTE"] = "New Territories East"
df_weather$District[df_weather$District=="NTW"] = "New Territories West"

colors <- c("Mean temperature" = "#FFC70C", "Total rainfall" = "#5F679E")
png(file = "Paper/fig_rainfall_line.png", width = 6100, height = 3300, res = 200)
p1 <- ggplot(df_weather, aes(x = Date))+
  geom_line(aes(y = Totalrain, color=District), size = 2.5)+
  labs(y = "Total rainfall (mm)",
       x = "Date")+
  theme(legend.position = c(0.8,0.910),
        legend.title= element_blank(),
        axis.title = element_text(size=60), 
        axis.text.x = element_text(size=60,angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=60),
        legend.text = element_text(size=60),
        axis.ticks.length = unit(.5, "cm"),
        axis.ticks = element_line(size=3),
        strip.text = element_text(size=45),
        panel.grid.major.x = element_line(colour = "black", size = 0.5),
        panel.grid.major.y = element_line(colour = "black", size = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color = "black", size = 1.5),
        axis.line.y = element_line(color = "black", size = 1.5))+ylim(0,780)+
  scale_x_date(date_breaks = "3 months", labels = label_date(format = "%m/%Y"), expand = c(0,0))
print(p1)
dev.off()


png(file = "Paper/fig_temperature_line.png", width = 6100, height = 3300, res = 200)
p1 <- ggplot(df_weather, aes(x = Date))+
  geom_line(aes(y = Meantemp, color=District), size = 2.5)+
  labs(y = "Mean Temperature (°C)",
       x = "Date")+
  theme(legend.position = c(0.8,0.910),
        legend.title= element_blank(),
        axis.title = element_text(size=60), 
        axis.text.x = element_text(size=60,angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=60),
        legend.text = element_text(size=60),
        axis.ticks.length = unit(.5, "cm"),
        axis.ticks = element_line(size=3),
        strip.text = element_text(size=45),
        panel.grid.major.x = element_line(colour = "black", size = 0.5),
        panel.grid.major.y = element_line(colour = "black", size = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color = "black", size = 1.5),
        axis.line.y = element_line(color = "black", size = 1.5))+ylim(14.2,33)+
  scale_x_date(date_breaks = "3 months", labels = label_date(format = "%m/%Y"), expand = c(0,0))
print(p1)
dev.off()


####### Figure 2 (A)#######
# load the climate data
filename <- "results/ModelE_1_loocv_result.csv"
df <- read.csv(filename)
df$Date <- as.Date(df$Date)

png(file = "Paper/fig_AOI_line.png", width = 5000, height = 3300, res = 200)
p1 <- ggplot(df, aes(x = Date))+
  geom_line(aes(y = meanAOI, color=District), size = 2.5)+
  labs(y = "Extensiveness of mosquitoes (%)",
       x = "Date")+
  theme(legend.position = c(0.75,0.910),
        legend.title= element_blank(),
        axis.title = element_text(size=60), 
        axis.text.x = element_text(size=60,angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=60),
        legend.text = element_text(size=60),
        axis.ticks.length = unit(.5, "cm"),
        axis.ticks = element_line(size=3),
        strip.text = element_text(size=45),
        panel.grid.major.x = element_line(colour = "black", size = 0.5),
        panel.grid.major.y = element_line(colour = "black", size = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color = "black", size = 1.5),
        axis.line.y = element_line(color = "black", size = 1.5))+ylim(0,0.32)+
  scale_x_date(date_breaks = "3 months", labels = label_date(format = "%m/%Y"), expand = c(0,0))
print(p1)
dev.off()

####### Figure 2 (B)#######
# load the climate data
filename <- "results/ModelE_1_loocv_result.csv"
df <- read.csv(filename)
df$Date <- as.Date(df$Date)


png(file = "Paper/fig_Abundance_line.png", width = 5000, height = 3300, res = 200)
p1 <- ggplot(df, aes(x = Date))+
  geom_line(aes(y = Nummosquito.per.1000.traps, color=District), size = 2.5)+
  labs(y = "Abundance of mosquitoes",
       x = "Date")+
  theme(legend.position = c(0.75,0.910),
        legend.title= element_blank(),
        axis.title = element_text(size=60), 
        axis.text.x = element_text(size=60,angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=60),
        legend.text = element_text(size=60),
        axis.ticks.length = unit(.5, "cm"),
        axis.ticks = element_line(size=3),
        strip.text = element_text(size=45),
        panel.grid.major.x = element_line(colour = "black", size = 0.5),
        panel.grid.major.y = element_line(colour = "black", size = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color = "black", size = 1.5),
        axis.line.y = element_line(color = "black", size = 1.5))+ylim(0,510)+
  scale_x_date(date_breaks = "3 months", labels = label_date(format = "%m/%Y"), expand = c(0,0))
print(p1)
dev.off()


####### Figure 2 (E)#######
df_mo_hk_month <- read.csv("Data/monthly_mobility_data_2.csv")
df_mo_hk_month$date <- as.Date(df_mo_hk_month$date,'%d/%m/%Y')
df_mo_hk_month <- df_mo_hk_month[df_mo_hk_month$date > "2020-03-01",]
df_mo_hk_month <- df_mo_hk_month[df_mo_hk_month$date < "2022-09-01",]
# draw the change of human mobility index

png(file = "Paper/fig_hk_mobility.png", width = 5000, height = 3300, res = 200)
colors <- c("Retail and recreation" = "#eccc68", "Grocery and pharmacy" = "#ff7f50", "Residential" = "#7bed9f",
            "Transit stations"="#70a1ff", "Workplaces"="#57606f","Parks"="#ff6b81")
p1 <- ggplot(df_mo_hk_month)+
  geom_line(aes(x=date, y=residential, color = "Residential"), size = 2.5) +
  geom_line(aes(x=date, y=parks, color = "Parks"), size = 2.5)+
  geom_line(aes(x=date, y=workplaces, color = "Workplaces"), size = 2.5)+
  labs(x = "Date",
       y = "Human mobility indices",
       color = "Legend")+
  theme(legend.position = c(0.893,0.915), 
        legend.title= element_blank(),
        axis.title = element_text(size=60),
        axis.text.x = element_text(size=60,angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=60),
        legend.text = element_text(size=60),
        axis.ticks.length = unit(.5, "cm"),
        axis.ticks = element_line(size=3),
        strip.text = element_text(size=45),
        panel.grid.major.x = element_line(colour = "black", size = 0.5),
        panel.grid.major.y = element_line(colour = "black", size = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color = "black", size = 1.5),
        axis.line.y = element_line(color = "black", size = 1.5))+ylim(-50,34)+
  scale_color_manual(values = colors)+
  scale_x_date(breaks=as.Date(c("2020-04-01","2020-07-01","2020-10-01","2021-01-01","2021-04-01",
                                "2021-07-01","2021-10-01","2022-01-01","2022-04-01","2022-07-01")), labels = label_date(format = "%m/%Y"), expand = c(0,1))
print(p1)
dev.off()


####### Figure 3 #######

plot_result_E <- function(df,num,leg="LOOCV"){
  colors <- c("Observed" = "blue", "Model fitted value" = "red", "LOOCV" = "green3", "LTOCV" = "green3")
  
  df$Date <- as.Date(df$Date)
  
  png(file = paste("Paper/fig_normalized_model_",num,".png",sep=""), width = 5000, height = 7000, res = 300)
  p1 <- ggplot(df)+
    geom_line(aes(x=Date, y=meanAOI_normal, color = "Observed"), linetype="longdash", size = 2.2)+
    geom_line(aes(x=Date, y=predictednum_normal, color = leg), size = 2.2)+
    geom_ribbon(aes(x=Date, ymin=predictedmin_normal, ymax=predictedmax_normal), alpha=0.2)+
    labs(x = "Date",
         y = "Standardized extensiveness index of mosquitoes",
         color = "Legend")+
    facet_wrap( ~ District, ncol = 1)+
    theme(legend.position = c(0.86,0.952), 
          legend.title= element_blank(),
          axis.title = element_text(size=60), 
          axis.text.x = element_text(size=60,angle = 54, hjust = 1, vjust = 1), # , angle = 90
          axis.text.y = element_text(size=60),
          legend.text = element_text(size=60),
          axis.ticks.length = unit(.5, "cm"),
          axis.ticks = element_line(size=3),
          strip.text = element_text(size=45),
          panel.grid.major.x = element_line(colour = "black", size = 0.5),
          panel.grid.major.y = element_line(colour = "black", size = 0.5),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_line(color = "black", size = 1.5),
          axis.line.y = element_line(color = "black", size = 1.5))+
    scale_color_manual(values = colors)+
    ylim(0,9)+
    scale_x_date(date_breaks = "3 months", labels = label_date(format = "%m/%Y"), expand = c(0,0))
  print(p1)
  dev.off()
}


plot_result_A <- function(df,num,leg="LOOCV"){
  colors <- c("Observed" = "blue", "Model fitted value" = "red", "LOOCV" = "green3", "LTOCV" = "green3")
  
  df$Date <- as.Date(df$Date)
  
  png(file = paste("Paper/fig_normalized_model_",num,".png",sep=""), width = 5000, height = 7000, res = 300)
  p1 <- ggplot(df)+
    geom_line(aes(x=Date, y=Nummosquito.per.1000.traps_normal, color = "Observed"), linetype="longdash", size = 2.2)+
    geom_line(aes(x=Date, y=predictednum_normal, color = leg), size = 2.2)+
    geom_ribbon(aes(x=Date, ymin=predictedmin_normal, ymax=predictedmax_normal), alpha=0.2)+
    labs(x = "Date",
         y = "Standardized abundance index of mosquitoes",
         color = "Legend")+
    facet_wrap( ~ District, ncol = 1)+
    theme(legend.position = c(0.865,0.952), 
          legend.title= element_blank(),
          axis.title = element_text(size=60), 
          axis.text.x = element_text(size=60,angle = 54, hjust = 1, vjust = 1), # , angle = 90
          axis.text.y = element_text(size=60),
          legend.text = element_text(size=60),
          axis.ticks.length = unit(.5, "cm"),
          axis.ticks = element_line(size=3),
          strip.text = element_text(size=45),
          panel.grid.major.x = element_line(colour = "black", size = 0.5),
          panel.grid.major.y = element_line(colour = "black", size = 0.5),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_line(color = "black", size = 1.5),
          axis.line.y = element_line(color = "black", size = 1.5))+
    scale_color_manual(values = colors)+
    ylim(0,9)+
    scale_x_date(date_breaks = "3 months", labels = label_date(format = "%m/%Y"), expand = c(0,0))
  print(p1)
  dev.off()
}

####### Figure 3 (A)#######
num = "E_1"
df <- read.csv(paste("Results/Model",num,"_loocv_normal_result.csv",sep=""))
plot_result_E(df,num,leg="LOOCV")

####### Figure 3 (B)#######
num = "A_1"
df <- read.csv(paste("Results/Model",num,"_loocv_normal_result.csv",sep=""))
plot_result_A(df,num,leg="LOOCV")

####### Figure 3 (A)#######
num = "E-R_1"
df <- read.csv(paste("Results/Model",num,"_loocv_normal_result.csv",sep=""))
plot_result_E(df,num,leg="LOOCV")

####### Figure 3 (B)#######
num = "A-R_1"
df <- read.csv(paste("Results/Model",num,"_loocv_normal_result.csv",sep=""))
plot_result_A(df,num,leg="LOOCV")



####### Figure 4 #######

plot_lag_rain <- function(model,df,num){
  ## plot lag effect of total rainfall
  nlag <- 6
  # extract full coef and vcov and create indicators for each term
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # find position of the terms associated with totalrain crossbasis
  indt <- grep("basis_tr1", model$names.fixed)
  
  # extract predictions from the totalrain DLNM centred on overall mean totalrain (173mm)
  predt <- crosspred(basis_tr1, coef = coef[indt], vcov=vcov[indt,indt],
                     model.link = "log", bylag = 0.25, cen = 300)
  
  # show the result in the figure
  png(file = paste("Paper/single_rain_model_mean_",num,".png",sep = ""), width = 3000, height = 2500, res = 450)
  y <- predt$predvar
  x <- seq(0, nlag, 0.25)
  z <- t(predt$matRRfit)
  pal <- rev(brewer.pal(11, "RdBu"))
  levels <- pretty(c(0.4, z, 2.3), 20)
  col1 <- colorRampPalette(pal[1:6])
  col2 <- colorRampPalette(pal[6:11])
  cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))
  par(mar=c(5,4,4,2)+0.5)
  filled.contour(x, y, z,
                 xlab = "Lag (months)", ylab = "Total rainfall (mm)", main = "", col = cols,levels = levels,key.title=title("RR"),cex.axis=1.3,
                 key.axes = axis(4,cex.axis=1.3),
                 plot.axes = { axis(1.3, at = 0:nlag, seq(0,nlag,1),cex.axis=1.3) 
                   axis(2,cex.axis=1.3)},plot.title=title(xlab="Lag (months)",ylab="Total rainfall (mm)",cex.main=0.8,cex.lab=1.8))
  dev.off()
  
  png(file = paste("Paper/rain_lag_c_mean_",num,".png",sep = ""), width = 3000, height = 2500, res = 450)
  # plot overall response across all lags
  par(cex.axis=1.3, cex.lab=1.8,mar=c(5,4,4,2)+0.5)
  plot(predt,"overall",xlab = "Total rainfall (mm)", lty=2, 
       ylab = "Relative risk", main = "", 
       ylim = c(range(0,6)))
  # axis(2,0:2)
  box()
  dev.off()
}

plot_lag_temp <- function(model,df,num){
  ## plot lag effect of mean temperature
  nlag <- 2
  # extract full coef and vcov and create indicators for each term
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # find position of the terms associated with meantemp crossbasis
  indt <- grep("basis_mt1", model$names.fixed)
  
  # extract predictions from the meantemp DLNM centred on overall mean meantemp (24 deg C)
  predt <- crosspred(basis_mt1, coef = coef[indt], vcov=vcov[indt,indt],
                     model.link = 'log', bylag = 0.25, cen = 20)
  
  # show the result in the figure
  png(file = paste("Paper/single_temp_model_mean_",num,".png",sep = ""), width = 3000, height = 2500, res = 450)
  y <- predt$predvar
  x <- seq(0, nlag, 0.25)
  z <- t(predt$matRRfit)
  pal <- rev(brewer.pal(11, "RdBu"))
  levels <- pretty(c(0.4, z, 2.3), 20)
  col1 <- colorRampPalette(pal[1:6])
  col2 <- colorRampPalette(pal[6:11])
  cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))
  par(mar=c(5,4,4,2)+0.5)
  filled.contour(x, y, z,
                 xlab = "Lag (months)", ylab = "Mean temperature (°C)", main = "", col = cols,levels = levels,key.title=title("RR"),cex.axis=1.3,
                 key.axes = axis(4,cex.axis=1.3),
                 plot.axes = { axis(1.3, at = 0:nlag, seq(0,nlag,1),cex.axis=1.3) 
                   axis(2,cex.axis=1.3)},plot.title=title(xlab="Lag (months)",ylab="Mean temperature (°C)",cex.main=0.8,cex.lab=1.8))
  dev.off()
  
  png(file = paste("Paper/temp_lag_c_mean_",num,".png",sep = ""), width = 3000, height = 2500, res = 450)
  # plot overall response across all lags 
  par(cex.axis=1.3, cex.lab=1.8,mar=c(5,4,4,2)+0.5)
  plot(predt,"overall",xlab = "Mean temperature (°C)", lty=2, 
       ylab = "Relative risk", main = "", 
       ylim = c(range(0,3)))
  axis(1,c(17.5,22.5,27.5))
  box()
  dev.off()
}



# setwd("D:/Bio/Dengue-Gravidtrap-and-Ovitrap-HK-main_20230211-add_mobility-all/")
# source("01_load_packages_data.R")
source(paste(path,"/01_load_packages_data.R",sep=""))

# setwd("D:/Bio/Dengue-Gravidtrap-and-Ovitrap-HK-main_20230211-add_mobility-all/")
setwd(path)


load("Model/AOI full model/model5.RData")
model_all <- model
num = "E-R_1"
plot_lag_rain(model_all,df1,num)
plot_lag_temp(model_all,df1,num)

####### Figure 4 (C) (D)#######
load("Model/Number of mosquitoes per 1000 traps/nbinomial/model5.RData")
model_all <- model
num = "A-R_1"
plot_lag_rain(model_all,df1,num)
plot_lag_temp(model_all,df1,num)


####### Figure S1#######
  
df <- read.csv(filename)
df$Date <- as.Date(df$Date)

png(file = "Paper/fig_scat_extensiveness_abundance.png", width = 6000, height = 2000, res = 200)
p <- ggplot(df)+
  geom_smooth(aes(x = Nummosquito.per.1000.traps, y = meanAOI, color=District),method="loess", se=FALSE, formula = y ~ x)+
  geom_point(aes(x = Nummosquito.per.1000.traps, y = meanAOI, color=District, shape=District),size=9)+
  labs(y = "Extensiveness",
       x = "Abundance")+
  facet_wrap( ~ District)+
  theme(legend.position = "none",
        legend.title= element_blank(),
        axis.title = element_text(size=60), 
        axis.text.x = element_text(size=60),
        axis.text.y = element_text(size=60),
        legend.text = element_text(size=60),
        axis.ticks.length = unit(.5, "cm"),
        axis.ticks = element_line(size=3),
        strip.text = element_text(size=45),
        panel.grid.major.x = element_line(colour = "gray50", size = 0.5),
        panel.grid.major.y = element_line(colour = "gray50", size = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color = "black", size = 1.5),
        axis.line.y = element_line(color = "black", size = 1.5))
print(p)
dev.off()


####### Figure S2#######
df <- read.csv(filename)
df$Date <- as.Date(df$Date)
df$SAI <- df$Nummosquito.per.1000.traps / sd(df$Nummosquito.per.1000.traps)
df$SEI <- df$meanAOI / sd(df$meanAOI)

png(file = "Paper/fig_SAI_line.png", width = 5000, height = 3300, res = 200)
p <- ggplot(df, aes(x = Date))+
  geom_line(aes(y = SAI, color=District), size = 2.5)+
  labs(y = "SAI",
       x = "Date")+
  ylim(0,6)+
  theme(legend.position = c(0.76,0.910),
        legend.title= element_blank(),
        axis.title = element_text(size=60), 
        axis.text.x = element_text(size=60,angle = 55, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=60),
        legend.text = element_text(size=60),
        axis.ticks.length = unit(.5, "cm"),
        axis.ticks = element_line(size=3),
        strip.text = element_text(size=45),
        panel.grid.major.x = element_line(colour = "gray50", size = 0.5),
        panel.grid.major.y = element_line(colour = "gray50", size = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color = "black", size = 1.5),
        axis.line.y = element_line(color = "black", size = 1.5))+
  scale_x_date(date_breaks = "3 months", labels = label_date(format = "%m/%Y"), expand = c(0,0))
print(p)
dev.off()


png(file = "Paper/fig_SEI_line.png", width = 5000, height = 3300, res = 200)
p <- ggplot(df, aes(x = Date))+
  geom_line(aes(y = SEI, color=District), size = 2.5)+
  labs(y = "SEI",
       x = "Date")+
  ylim(0,6)+
  theme(legend.position = c(0.76,0.910),
        legend.title= element_blank(),
        axis.title = element_text(size=60), 
        axis.text.x = element_text(size=60,angle = 55, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=60),
        legend.text = element_text(size=60),
        axis.ticks.length = unit(.5, "cm"),
        axis.ticks = element_line(size=3),
        strip.text = element_text(size=45),
        panel.grid.major.x = element_line(colour = "gray50", size = 0.5),
        panel.grid.major.y = element_line(colour = "gray50", size = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color = "black", size = 1.5),
        axis.line.y = element_line(color = "black", size = 1.5))+
  scale_x_date(date_breaks = "3 months", labels = label_date(format = "%m/%Y"), expand = c(0,0))
print(p)
dev.off()


####### Figure S9#######
plot_result_projection_E <- function(df,num,i=63,i_up=87){
  df$Date <- as.Date(df$Date,format = '%Y-%m-%d')
  df$projection_1_normal[1:i] <- NA
  df$projection_2_normal[1:i] <- NA
  # df <- df[i:i_up,]
  colors <- c("Observed" = "blue", "Fitted" = "#663300", "Same human mobility as 2021" = "red", "Human mobility returns to normal"="orange")
  png(file = paste("Paper/projection_",num,".png",sep=""), width = 5000, height = 7000, res = 300)
  g3<-ggplot(df)+
    geom_line(aes(x=Date, y=meanAOI_normal, color = "Observed"), linetype="longdash", size = 2)+
    geom_line(aes(x=Date, y=fittednum_normal, color = "Fitted"), size = 2)+
    geom_line(aes(x=Date, y=projection_1_normal, color = "Same human mobility as 2021"), size = 2)+
    geom_line(aes(x=Date, y=projection_2_normal, color = "Human mobility returns to normal"), size = 2)+
    labs(x = "Date",
         y = "Standardized extensivenss index of mosquitoes",
         color = "Legend")+
    facet_wrap( ~District, ncol = 1)+
    geom_vline(xintercept = c(i),linetype=2,size=1.5)+
    theme(legend.position = c(0.449,0.911), 
          legend.title= element_blank(),
          axis.title = element_text(size=60), 
          axis.text.x = element_text(size=60,angle = 54, hjust = 1, vjust = 1), # , angle = 90
          axis.text.y = element_text(size=60),
          legend.text = element_text(size=60),
          axis.ticks.length = unit(.5, "cm"),
          axis.ticks = element_line(size=3),
          strip.text = element_text(size=45),
          panel.grid.major.x = element_line(colour = "gray50", size = 0.5),
          panel.grid.major.y = element_line(colour = "gray50", size = 0.5),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_line(color = "black", size = 1.5),
          axis.line.y = element_line(color = "black", size = 1.5))+
    scale_color_manual(values = colors)+
    ylim(0,9)+
    scale_x_date(date_breaks = "3 months", labels = label_date(format = "%m/%Y"), expand = c(0,0))
  print(g3)
  dev.off()
}


lab <- "E-R_1"
data <- read.csv(paste("results/Model",lab,"_loocv_result_projection.csv",sep=""))
num <- "E-R"
plot_result_projection_E(df=data,num=num,i=63,i_up=87)


plot_result_projection_A <- function(df,num,i=63,i_up=87){
  df$Date <- as.Date(df$Date,format = '%Y-%m-%d')
  df$projection_1_normal[1:i] <- NA
  df$projection_2_normal[1:i] <- NA
  # df <- df[i:i_up,]
  colors <- c("Observed" = "blue", "Fitted" = "#663300", "Same human mobility as 2021" = "red", "Human mobility returns to normal"="orange")
  png(file = paste("Paper/projection_",num,".png",sep=""), width = 5000, height = 7000, res = 300)
  g3<-ggplot(df)+
    geom_line(aes(x=Date, y=Nummosquito.per.1000.traps_normal, color = "Observed"), linetype="longdash", size = 2)+
    geom_line(aes(x=Date, y=fittednum_normal, color = "Fitted"), size = 2)+
    geom_line(aes(x=Date, y=projection_1_normal, color = "Same human mobility as 2021"), size = 2)+
    geom_line(aes(x=Date, y=projection_2_normal, color = "Human mobility returns to normal"), size = 2)+
    labs(x = "Date",
         y = "Standardized abundance index of mosquitoes",
         color = "Legend")+
    facet_wrap( ~District, ncol = 1)+
    geom_vline(xintercept = c(i),linetype=2,size=1.5)+
    theme(legend.position = c(0.449,0.911), 
          legend.title= element_blank(),
          axis.title = element_text(size=60), 
          axis.text.x = element_text(size=60,angle = 54, hjust = 1, vjust = 1), # , angle = 90
          axis.text.y = element_text(size=60),
          legend.text = element_text(size=60),
          axis.ticks.length = unit(.5, "cm"),
          axis.ticks = element_line(size=3),
          strip.text = element_text(size=45),
          panel.grid.major.x = element_line(colour = "gray50", size = 0.5),
          panel.grid.major.y = element_line(colour = "gray50", size = 0.5),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_line(color = "black", size = 1.5),
          axis.line.y = element_line(color = "black", size = 1.5))+
    scale_color_manual(values = colors)+
    ylim(0,9)+
    scale_x_date(date_breaks = "3 months", labels = label_date(format = "%m/%Y"), expand = c(0,0))
  print(g3)
  dev.off()
}

lab <- "A-R_1"
data <- read.csv(paste("results/Model",lab,"_loocv_result_projection.csv",sep=""))
num <- "A-R"
plot_result_projection_A(df=data,num=num,i=63,i_up=87)


