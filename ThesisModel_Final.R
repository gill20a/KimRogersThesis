#### CHECKING CORRELATIONS OF MODEL ####

## Call necessary packages
library(corrplot)

## Set working directory appropriately
setwd("/Users/kimberlyrogers/Downloads")

# read in CSV with raster data across HMF
alldata <- read.csv("summary_historicalvars.csv")

alldata <- alldata[5:41]
colnames(alldata) <- c("aspect", "elevation", "AHM", "bFFP", "CMD", "CMI", "DD5", "DD18", 
                       "DD1040", "DD_0", "DD_18", "eFFP", "EMT", "Eref", "EXT", "FFP", 
                       "MAP", "MAR", "MAT", "MCMT", "MSP","MWMT", "NFFD", "PAS", "PPT_at", 
                       "PPT_sm", "PPT_sp", "PPT_wt", "RH", "SHM", "Tave_at", "Tave_sm", 
                       "Tave_sp", "Tave_wt", "TD", "slope", "soilpH")

correlations <- abs(cor(alldata, method = c("pearson")))
par(mfrow=c(1,1))

# plot correlations of original vars
corrplot(correlations, method = c("color"), col=colorRampPalette(c("white","brown"))(10), 
         col.lim = c(0,1), is.corr = FALSE, outline="darkgrey", tl.col="black", 
         title = "Pearson correlations for all variables", mar = c(1,1,2,1))

# subset and plot uncorrelated variables
uncor_variables <- alldata[,c(1, 2, 3, 15, 17, 23, 29, 36, 37)]
uncor_correlations <- abs(cor(uncor_variables, method = c("pearson")))
corrplot(uncor_correlations, method = c("color"), col=colorRampPalette(c("white","brown"))(10), 
         col.lim = c(0,1), is.corr = FALSE, outline="darkgrey", tl.col="black",
         title = "Pearson correlations for selected variables", mar = c(1,1,2,1))

#### PLOTS OF FUTURE CLIMATE ####

## Call necessary packages
library(ggplot2)

## Set working directory appropriately
setwd("/Users/kimberlyrogers/Downloads")

## Read in data
### HMF historical data:
localdata_hist <- read.csv("HMF_hist.csv")
### HMF future data:
localdata_SSP1 <- read.csv("HMF_SSP1.csv")
localdata_SSP2 <- read.csv("HMF_SSP2.csv")
localdata_SSP3 <- read.csv("HMF_SSP3.csv")
localdata_SSP5 <- read.csv("HMF_SSP5.csv")

## Organize and relabel columns:
### HMF historical data:
localdata_hist <- localdata_hist[c(7:39)]
colnames(localdata_hist) <- c("AHM", "bFFP","CMD", "CMI", "DD5", "DD18", "DD1040", 
                              "DD_0", "DD_18", "eFFP", "EMT", "Eref", "EXT", "FFP", 
                              "MAP", "MAR", "MAT", "MCMT", "MSP","MWMT", "NFFD", "PAS", 
                              "PPT_at", "PPT_sm", "PPT_sp", "PPT_wt", "RH", "SHM", 
                              "Tave_at", "Tave_sm", "Tave_sp", "Tave_wt", "TD")
localdata_hist <- na.omit(localdata_hist)
### HMF future data:
#### SSP1:
localdata_SSP1 <- localdata_SSP1[c(6:38)]
colnames(localdata_SSP1) <-  c("AHM", "bFFP","CMD", "CMI", "DD5", "DD18", "DD1040", 
                               "DD_0", "DD_18", "eFFP", "EMT", "Eref", "EXT", "FFP", 
                               "MAP", "MAR", "MAT", "MCMT", "MSP","MWMT", "NFFD", "PAS", 
                               "PPT_at", "PPT_sm", "PPT_sp", "PPT_wt", "RH", "SHM", 
                               "Tave_at", "Tave_sm", "Tave_sp", "Tave_wt", "TD")
#### SSP2:
localdata_SSP2 <- localdata_SSP2[c(6:38)]
colnames(localdata_SSP2) <- c("AHM", "bFFP","CMD", "CMI", "DD5", "DD18", "DD1040", 
                              "DD_0", "DD_18", "eFFP", "EMT", "Eref", "EXT", "FFP", 
                              "MAP", "MAR", "MAT", "MCMT", "MSP","MWMT", "NFFD", "PAS", 
                              "PPT_at", "PPT_sm", "PPT_sp", "PPT_wt", "RH", "SHM", 
                              "Tave_at", "Tave_sm", "Tave_sp", "Tave_wt", "TD")
#### SSP3:
localdata_SSP3 <- localdata_SSP3[c(6:38)]
colnames(localdata_SSP3) <- c("AHM", "bFFP","CMD", "CMI", "DD5", "DD18", "DD1040", 
                              "DD_0", "DD_18", "eFFP", "EMT", "Eref", "EXT", "FFP", 
                              "MAP", "MAR", "MAT", "MCMT", "MSP","MWMT", "NFFD", "PAS", 
                              "PPT_at", "PPT_sm", "PPT_sp", "PPT_wt", "RH", "SHM", 
                              "Tave_at", "Tave_sm", "Tave_sp", "Tave_wt", "TD")
#### SSP5:
localdata_SSP5 <- localdata_SSP5[c(6:38)]
colnames(localdata_SSP5) <- c("AHM", "bFFP","CMD", "CMI", "DD5", "DD18", "DD1040", 
                              "DD_0", "DD_18", "eFFP", "EMT", "Eref", "EXT", "FFP", 
                              "MAP", "MAR", "MAT", "MCMT", "MSP","MWMT", "NFFD", "PAS", 
                              "PPT_at", "PPT_sm", "PPT_sp", "PPT_wt", "RH", "SHM", 
                              "Tave_at", "Tave_sm", "Tave_sp", "Tave_wt", "TD")


## Clean final data
### HMF hist data
localdata_hist <- localdata_hist[,c(1,13, 15, 21, 27)]
colnames(localdata_hist) <- c("heat_moist_index", "temp_max_30yrs", "annual_precip", 
                              "frost_free_days", "perc_ann_rel_humid")
### HMF fut data
#### SSP 1
localdata_SSP1 <- localdata_SSP1[,c(1,13, 15, 21, 27)]
colnames(localdata_SSP1) <- c("heat_moist_index", "temp_max_30yrs", "annual_precip", 
                              "frost_free_days", "perc_ann_rel_humid")
#### SSP 2
localdata_SSP2 <- localdata_SSP2[,c(1,13, 15, 21, 27)]
colnames(localdata_SSP2) <- c("heat_moist_index", "temp_max_30yrs", "annual_precip", 
                              "frost_free_days", "perc_ann_rel_humid")
#### SSP 3
localdata_SSP3 <- localdata_SSP3[,c(1,13, 15, 21, 27)]
colnames(localdata_SSP3) <- c("heat_moist_index", "temp_max_30yrs", "annual_precip", 
                              "frost_free_days", "perc_ann_rel_humid")
#### SSP 5
localdata_SSP5 <- localdata_SSP5[,c(1, 13, 15, 21, 27)]
colnames(localdata_SSP5) <- c("heat_moist_index", "temp_max_30yrs", "annual_precip", 
                              "frost_free_days", "perc_ann_rel_humid")

# calculate "overall change" or total change in each var in HMF
overallchange <- data.frame(matrix(data = NA, nrow = 5, ncol = 5))
colnames(overallchange) <- c("heat_moist_index", "temp_max_30yrs", "annual_precip", 
                             "frost_free_days", "perc_ann_rel_humid")
rownames(overallchange) <- c("historical", "SSP1", "SSP2", "SSP3", "SSP5")
overallchange[1,] <- colMeans(localdata_hist)
overallchange[2,] <- colMeans(localdata_SSP1)
overallchange[3,] <- colMeans(localdata_SSP2)
overallchange[4,] <- colMeans(localdata_SSP3)
overallchange[5,] <- colMeans(localdata_SSP5)

namevec <- c("Heat Moisture Index","Maximum Temp (* C)", 
             "Annual Precipitation (mm)", "# of Frost-free Days", 
             "% Annual Relative Humidity")


#### SPECIES FOR LOOP FOR DATA PROCESSING####

## Enter list of speciesnames
speciesnames <- c("Acer pensylvanicum", "Acer rubrum", "Acer saccharum", "Betula alleghaniensis",
                  "Betula lenta", "Betula papyrifera", "Carpinus caroliniana","Carya cordiformis", 
                  "Castanea dentata", "Fagus grandifolia", "Fraxinus americana", "Ostrya virginiana",
                  "Populus tremuloides", "Prunus serotina", "Quercus rubra", "Ulmus americana")

## Call necessary packages
library(randomForest)
library(corrplot)
library(gt)
library(webshot2)

## Set working directory appropriately
setwd("/Users/kimberlyrogers/Downloads")

predchange <- data.frame(matrix(data = NA, nrow = 7, ncol = 16))
colnames(predchange) <- c("Acer pensylvanicum", "Acer rubrum", "Acer saccharum", "Betula alleghaniensis",
                          "Betula lenta", "Betula papyrifera", "Carpinus caroliniana","Carya cordiformis", 
                          "Castanea dentata", "Fagus grandifolia", "Fraxinus americana", "Ostrya virginiana",
                          "Populus tremuloides", "Prunus serotina", "Quercus rubra", "Ulmus americana")
rownames(predchange) <- c("historical", "predicted_historical", "SSP1", "SSP2", "SSP3", "SSP5", "error")

# create data frame for use in model prior to altering these data
varimp <- data.frame(matrix(data = NA, nrow = 9, ncol = 16))
colnames(varimp) <- colnames(predchange)
rownames(varimp) <- c("aspect", "elevation", "heat_moist_index", "temp_max_30yrs",
                      "annual_precip", "frost_free_days", "perc_ann_rel_humid", "slope", "soil_pH")
var = 1

for (i in (1:length(speciesnames))) {
  spp <- speciesnames[i]
  spp_abbr <- paste(substring(strsplit(tolower(spp), split = " ")[[1]][1], 1, 4), 
                    substring(strsplit(spp, split = " ")[[1]][2], 1, 4), sep = "")
  ## Read in data
  ### full model data:
  data <- read.csv(paste(spp_abbr, "_hist.csv", sep = ""))
  ### HMF historical data:
  localdata_hist <- read.csv("HMF_hist.csv")
  ### HMF future data:
  localdata_SSP1 <- read.csv("HMF_SSP1.csv")
  localdata_SSP2 <- read.csv("HMF_SSP2.csv")
  localdata_SSP3 <- read.csv("HMF_SSP3.csv")
  localdata_SSP5 <- read.csv("HMF_SSP5.csv")
  
  ## Organize and relabel columns:
  ### full model data:
  data <- data[c("ACTUAL", "Point_Coun", "percforest", "v_raster_1",  "v_raster_2",  
                 "v_raster_3",  "v_raster_4",  "v_raster_5",  "v_raster_6", "v_raster_7",
                 "v_raster_8", "v_raster_9", "v_raste_10", "v_raste_11", "v_raste_12",
                 "v_raste_13", "v_raste_14", "v_raste_15", "v_raste_16", "v_raste_17",
                 "v_raste_18", "v_raste_19", "v_raste_20", "v_raste_21", "v_raste_22", 
                 "v_raste_23", "v_raste_24", "v_raste_25", "v_raste_26", "v_raste_27", 
                 "v_raste_28", "v_raste_29", "v_raste_30", "v_raste_31", "v_raste_32", 
                 "v_raste_33", "v_raste_34", "v_raste_35", "v_raste_36", "v_raste_37")]
  colnames(data) <- c("IV", "plotcount", "percforest", "aspect", "elevation", "AHM", 
                      "bFFP", "CMD", "CMI", "DD5", "DD18", "DD1040", "DD_0", "DD_18", 
                      "eFFP", "EMT", "Eref", "EXT", "FFP", "MAP", "MAR", "MAT", "MCMT", 
                      "MSP","MWMT", "NFFD", "PAS", "PPT_at", "PPT_sm", "PPT_sp", 
                      "PPT_wt", "RH", "SHM", "Tave_at", "Tave_sm", "Tave_sp", "Tave_wt", 
                      "TD", "slope", "soilpH")
  data <- na.omit(data)
  ### HMF historical data:
  #### Code to add in IV values for HMF plots:
  IVvals <- read.csv("ALLHMFDATA_PROCESSED.csv")
  tempval <- subset(IVvals, speciesname == spp & treesurveynumber == 4)
  for (i in (1:nrow(localdata_hist))) {
    if (length(which(tempval$plotnumber == localdata_hist$PLOT[i])) > 0){
      localdata_hist$IV[i] <- tempval$importancevalue[which(tempval$plotnumber == localdata_hist$PLOT[i])]
    }
    else {
      localdata_hist$IV[i] <- NA
    }
  }
  hist_IVs <- localdata_hist[c(3,42)]
  hist_IVs <- na.omit(hist_IVs)
  #### Continue data cleaning
  localdata_hist <- localdata_hist[c(4:42)]
  colnames(localdata_hist) <- c("percforest", "aspect", "elevation", "AHM", "bFFP", 
                                "CMD", "CMI", "DD5", "DD18", "DD1040", "DD_0", "DD_18", 
                                "eFFP", "EMT", "Eref", "EXT", "FFP", "MAP", "MAR", "MAT", 
                                "MCMT", "MSP","MWMT", "NFFD", "PAS", "PPT_at", "PPT_sm", 
                                "PPT_sp", "PPT_wt", "RH", "SHM", "Tave_at", "Tave_sm", 
                                "Tave_sp", "Tave_wt", "TD", "slope", "soilpH", "IV")
  localdata_hist <- localdata_hist[,c(2:ncol(localdata_hist),1)]
  localdata_hist <- na.omit(localdata_hist)
 
  ### HMF future data:
  #### SSP1:
  localdata_SSP1$IV <- NA
  store_fut_1 <- localdata_SSP1[c(2,41)]
  localdata_SSP1 <- localdata_SSP1[c(3:41)]
  colnames(localdata_SSP1) <- c("percforest", "aspect", "elevation", "AHM", "bFFP", 
                                "CMD", "CMI", "DD5", "DD18", "DD1040", "DD_0", "DD_18", 
                                "eFFP", "EMT", "Eref", "EXT", "FFP", "MAP", "MAR", "MAT", 
                                "MCMT", "MSP","MWMT", "NFFD", "PAS", "PPT_at", "PPT_sm", 
                                "PPT_sp", "PPT_wt", "RH", "SHM", "Tave_at", "Tave_sm", 
                                "Tave_sp", "Tave_wt", "TD", "slope", "soilpH", "IV")
  localdata_SSP1 <- localdata_SSP1[,c(2:ncol(localdata_SSP1),1)]
  #### SSP2:
  localdata_SSP2$IV <- NA
  store_fut_2 <- localdata_SSP2[c(2,41)]
  localdata_SSP2 <- localdata_SSP2[c(3:41)]
  colnames(localdata_SSP2) <- c("percforest", "aspect", "elevation", "AHM", "bFFP", 
                                "CMD", "CMI", "DD5", "DD18", "DD1040", "DD_0", "DD_18", 
                                "eFFP", "EMT", "Eref", "EXT", "FFP", "MAP", "MAR", "MAT", 
                                "MCMT", "MSP","MWMT", "NFFD", "PAS", "PPT_at", "PPT_sm", 
                                "PPT_sp", "PPT_wt", "RH", "SHM", "Tave_at", "Tave_sm", 
                                "Tave_sp", "Tave_wt", "TD", "slope", "soilpH", "IV")
  localdata_SSP2 <- localdata_SSP2[,c(2:ncol(localdata_SSP2),1)]
  #### SSP3:
  localdata_SSP3$IV <- NA
  store_fut_3 <- localdata_SSP3[c(2,41)]
  localdata_SSP3 <- localdata_SSP3[c(3:41)]
  colnames(localdata_SSP3) <- c("percforest", "aspect", "elevation", "AHM", "bFFP", 
                                "CMD", "CMI", "DD5", "DD18", "DD1040", "DD_0", "DD_18", 
                                "eFFP", "EMT", "Eref", "EXT", "FFP", "MAP", "MAR", "MAT", 
                                "MCMT", "MSP","MWMT", "NFFD", "PAS", "PPT_at", "PPT_sm", 
                                "PPT_sp", "PPT_wt", "RH", "SHM", "Tave_at", "Tave_sm", 
                                "Tave_sp", "Tave_wt", "TD", "slope", "soilpH", "IV")
  localdata_SSP3 <- localdata_SSP3[,c(2:ncol(localdata_SSP3),1)]
  #### SSP5:
  localdata_SSP5$IV <- NA
  store_fut_5 <- localdata_SSP5[c(2,41)]
  localdata_SSP5 <- localdata_SSP5[c(3:41)]
  colnames(localdata_SSP5) <- c("percforest", "aspect", "elevation", "AHM", "bFFP", 
                                "CMD", "CMI", "DD5", "DD18", "DD1040", "DD_0", "DD_18", 
                                "eFFP", "EMT", "Eref", "EXT", "FFP", "MAP", "MAR", "MAT", 
                                "MCMT", "MSP","MWMT", "NFFD", "PAS", "PPT_at", "PPT_sm", 
                                "PPT_sp", "PPT_wt", "RH", "SHM", "Tave_at", "Tave_sm", 
                                "Tave_sp", "Tave_wt", "TD", "slope", "soilpH", "IV")
  localdata_SSP5 <- localdata_SSP5[,c(2:ncol(localdata_SSP5),1)]
  
  ## Clean final data
  ### Eastern US data
  data <- subset(data, data$IV > 0)
  if (IQR(data$IV) <= 1) {
    IQR_pre <- 1
  }
  if (IQR(data$IV) > 1) {
    IQR_pre <- IQR(data$IV)
  }
  if (abs(((mean(localdata_hist$IV) - mean(data$IV))/IQR_pre)) > IQR_pre) {
    upper <- mean(data$IV) + 2*(abs(((mean(localdata_hist$IV) - mean(data$IV))/IQR_pre)))*IQR_pre
    lower <- mean(data$IV) - 2*(abs(((mean(localdata_hist$IV) - mean(data$IV))/IQR_pre)))*IQR_pre
  }
  if (abs(((mean(localdata_hist$IV) - mean(data$IV))/IQR_pre)) <= IQR_pre) {
    upper <- mean(data$IV) + 2*IQR_pre
    lower <- mean(data$IV) - 2*IQR_pre
  }
  data <- subset(data, data$IV > lower)
  data <- subset(data, data$IV < upper)
  data <- subset(data, data$plotcount > 2)
  data <- subset(data, data$percforest > 5)
  data <- data[,c(1,4:6,18,20,26,32,39,40)]
  colnames(data) <- c("IV", "aspect", "elevation", "heat_moist_index", "temp_max_30yrs",
                      "annual_precip", "frost_free_days",
                      "perc_ann_rel_humid", "slope", "soil_pH")
  ### HMF hist data
  localdata_hist <- localdata_hist[,c(38,39,1:37)]
  localdata_hist <- localdata_hist[,c(1,3:5,17,19,25,31,38,39)]
  colnames(localdata_hist) <- c("IV", "aspect", "elevation", "heat_moist_index", "temp_max_30yrs",
                                "annual_precip", "frost_free_days",
                                "perc_ann_rel_humid", "slope", "soil_pH")
  ### HMF fut data
  #### SSP 1
  localdata_SSP1 <- localdata_SSP1[,c(38,39,1:37)]
  localdata_SSP1 <- localdata_SSP1[,c(1,3:5,17,19,25,31,38,39)]
  colnames(localdata_SSP1) <- c("IV", "aspect", "elevation", "heat_moist_index", "temp_max_30yrs",
                                "annual_precip", "frost_free_days",
                                "perc_ann_rel_humid", "slope", "soil_pH")
  #### SSP 2
  localdata_SSP2 <- localdata_SSP2[,c(38,39,1:37)]
  localdata_SSP2 <- localdata_SSP2[,c(1,3:5,17,19,25,31,38,39)]
  colnames(localdata_SSP2) <- c("IV", "aspect", "elevation", "heat_moist_index", "temp_max_30yrs",
                                "annual_precip", "frost_free_days",
                                "perc_ann_rel_humid", "slope", "soil_pH")
  #### SSP 3
  localdata_SSP3 <- localdata_SSP3[,c(38,39,1:37)]
  localdata_SSP3 <- localdata_SSP3[,c(1,3:5,17,19,25,31,38,39)]
  colnames(localdata_SSP3) <- c("IV", "aspect", "elevation", "heat_moist_index", "temp_max_30yrs",
                                "annual_precip", "frost_free_days",
                                "perc_ann_rel_humid", "slope", "soil_pH")
  #### SSP 5
  localdata_SSP5 <- localdata_SSP5[,c(38,39,1:37)]
  localdata_SSP5 <- localdata_SSP5[,c(1,3:5,17,19,25,31,38,39)]
  colnames(localdata_SSP5) <- c("IV", "aspect", "elevation", "heat_moist_index", "temp_max_30yrs",
                                "annual_precip", "frost_free_days",
                                "perc_ann_rel_humid", "slope", "soil_pH")
  ## Fit RF model on data
  set.seed(1)
  ### Define training and testing data
  train <- data[1:(nrow(data)*.7),]
  test <- data[(nrow(data)*.7):nrow(data),]
  ### Fit model
  RFmodel <- randomForest(IV ~ ., data = train, ntree = 1500, 
                          mtry = (ncol(data)-1)/3, importance = TRUE)
  plot(RFmodel)
  colors <- c("red", "orange", "gold1", "forestgreen", "darkturquoise", 
              "blue3", "purple", "deeppink", "sienna")
  MSE <- as.data.frame(RFmodel$importance[,1])
  MSE[,2] <- colors
  MSE[,3] <- rownames(MSE)
  stored <- rownames(MSE)
  colnames(MSE) <- c("%IncMSE", "Color", "Variable")
  MSE <- MSE[order(MSE$`%IncMSE`, decreasing = TRUE),]
  par(mfrow=c(1,1))
  jpeg(file = paste(spp_abbr, "_RF.jpeg", sep = ""), width = 3000, height = 2000, units = "px", res = 300)
  par(mar = c(10,4,4,4))
  plot(MSE$`%IncMSE`, col = MSE$Color, pch = 15, cex = 1.5,  
       xlab = "", xaxt = "n", ylab = "% Increase MSE", main = paste(spp, " variable importance", sep = ""))
  axis(1, at = 1:9, labels=rownames(MSE), las = 2)
  legend(x = "topright", legend = stored, fill = colors) 
  dev.off()
  par(mar = c(5.1, 4.1, 4.1, 2.1))
  ### Check model on training data
  RF_trained <- predict(RFmodel, train)
  error_trained <- mean(abs(RF_trained - train$IV))
  MSE_trained <- sum((train$IV - RF_trained)^2)/nrow(train)
  ### Check model on testing data
  RF_tested <- predict(RFmodel, test)
  error_tested <- mean(abs(RF_tested - test$IV))
  MSE_tested <- sum((test$IV - RF_tested)^2)/nrow(test)
  ### Check model on individual plot data
  RF_local_individual <- predict(RFmodel, localdata_hist)
  error_individual <- mean(abs(RF_local_individual - localdata_hist$IV))
  MSE_individual <- sum((localdata_hist$IV - RF_local_individual)^2)/nrow(localdata_hist)
  ### Check model on averaged HMF historical data
  data[(nrow(data)+1),] <- colMeans(localdata_hist)
  localdata_hist_avg <- data[nrow(data),]
  RF_local_hist_avg <- predict(RFmodel, localdata_hist_avg)
  error_local_hist_avg <- mean(abs(RF_local_hist_avg - localdata_hist_avg$IV))
  MSE_local_hist_avg <- (localdata_hist_avg$IV - RF_local_hist_avg)^2/nrow(localdata_hist_avg)
  ### Use model to predict HMF IVs in future
  #### SSP1
  data[(nrow(data)+1),] <- colMeans(localdata_SSP1)
  localdata_SSP1_avg <- data[nrow(data),]
  RF_local_fut_SSP1_avg <- predict(RFmodel, localdata_SSP1_avg)
  #### SSP2
  data[(nrow(data)+1),] <- colMeans(localdata_SSP2)
  localdata_SSP2_avg <- data[nrow(data),]
  RF_local_fut_SSP2_avg <- predict(RFmodel, localdata_SSP2_avg)
  #### SSP3
  data[(nrow(data)+1),] <- colMeans(localdata_SSP3)
  localdata_SSP3_avg <- data[nrow(data),]
  RF_local_fut_SSP3_avg <- predict(RFmodel, localdata_SSP3_avg)
  #### SSP5
  data[(nrow(data)+1),] <- colMeans(localdata_SSP5)
  localdata_SSP5_avg <- data[nrow(data),]
  RF_local_fut_SSP5_avg <- predict(RFmodel, localdata_SSP5_avg)
  ### Calculate % expected change
  #### SSP1
  HMF_projected_change_1 <- RF_local_hist_avg - RF_local_fut_SSP1_avg
  HMF_projected_change_percent_1 <- HMF_projected_change_1/RF_local_hist_avg*100
  #### SSP2
  HMF_projected_change_2 <- RF_local_hist_avg - RF_local_fut_SSP2_avg
  HMF_projected_change_percent_2 <- HMF_projected_change_2/RF_local_hist_avg*100
  #### SSP3
  HMF_projected_change_3 <- RF_local_hist_avg - RF_local_fut_SSP3_avg
  HMF_projected_change_percent_3 <- HMF_projected_change_3/RF_local_hist_avg*100
  #### SSP5
  HMF_projected_change_5 <- RF_local_hist_avg - RF_local_fut_SSP5_avg
  HMF_projected_change_percent_5 <- HMF_projected_change_5/RF_local_hist_avg*100
  
  ## Store predicted changes and error
  predchange[which(is.na(predchange), arr.ind = TRUE)[1,1], which(is.na(predchange), arr.ind = TRUE)[1,2]] <- localdata_hist_avg$IV
  predchange[which(is.na(predchange), arr.ind = TRUE)[1,1], which(is.na(predchange), arr.ind = TRUE)[1,2]] <- RF_local_hist_avg
  predchange[which(is.na(predchange), arr.ind = TRUE)[1,1], which(is.na(predchange), arr.ind = TRUE)[1,2]] <- localdata_hist_avg$IV + HMF_projected_change_1
  predchange[which(is.na(predchange), arr.ind = TRUE)[1,1], which(is.na(predchange), arr.ind = TRUE)[1,2]] <- localdata_hist_avg$IV + HMF_projected_change_2
  predchange[which(is.na(predchange), arr.ind = TRUE)[1,1], which(is.na(predchange), arr.ind = TRUE)[1,2]] <- localdata_hist_avg$IV + HMF_projected_change_3
  predchange[which(is.na(predchange), arr.ind = TRUE)[1,1], which(is.na(predchange), arr.ind = TRUE)[1,2]] <- localdata_hist_avg$IV + HMF_projected_change_5
  predchange[which(is.na(predchange), arr.ind = TRUE)[1,1], which(is.na(predchange), arr.ind = TRUE)[1,2]] <- error_local_hist_avg
  
  ## Build table with projected IV values for HMF
  ### SSP1
  for (i in (1:nrow(store_fut_1))) {
    if (length(which(hist_IVs$PLOT == store_fut_1$PLOT[i])) > 0){
      if (HMF_projected_change_1 > 0) {
        store_fut_1$IV[i] <- hist_IVs$IV[which(hist_IVs$PLOT == store_fut_1$PLOT[i])] + 
          (HMF_projected_change_percent_1/100*hist_IVs$IV[which(hist_IVs$PLOT == store_fut_1$PLOT[i])])
      }
      else if (HMF_projected_change_1 < 0) {
        store_fut_1$IV[i] <- hist_IVs$IV[which(hist_IVs$PLOT == store_fut_1$PLOT[i])] - 
          (HMF_projected_change_percent_1/100*hist_IVs$IV[which(hist_IVs$PLOT == store_fut_1$PLOT[i])])
      }
    }
    else {
      store_fut_1$IV[i] <- NA
    }
  }
  #### Write table to csv
  write.csv(store_fut_1, file = paste("HMF_SSP1_", spp_abbr, ".csv", sep = ""), row.names = FALSE)
  ### SSP2
  for (i in (1:nrow(store_fut_2))) {
    if (length(which(hist_IVs$PLOT == store_fut_2$PLOT[i])) > 0){
      if (HMF_projected_change_2 > 0) {
        store_fut_2$IV[i] <- hist_IVs$IV[which(hist_IVs$PLOT == store_fut_2$PLOT[i])] + 
          (HMF_projected_change_percent_2/100*hist_IVs$IV[which(hist_IVs$PLOT == store_fut_2$PLOT[i])])
      }
      else if (HMF_projected_change_2 < 0) {
        store_fut_2$IV[i] <- hist_IVs$IV[which(hist_IVs$PLOT == store_fut_2$PLOT[i])] - 
          (HMF_projected_change_percent_2/100*hist_IVs$IV[which(hist_IVs$PLOT == store_fut_2$PLOT[i])])
      }
    }
    else {
      store_fut_2$IV[i] <- NA
    }
  }
  #### Write table to csv
  write.csv(store_fut_2, file = paste("HMF_SSP2_", spp_abbr, ".csv", sep = ""), row.names = FALSE)
  ### SSP3
  for (i in (1:nrow(store_fut_3))) {
    if (length(which(hist_IVs$PLOT == store_fut_3$PLOT[i])) > 0){
      if (HMF_projected_change_3 > 0) {
        store_fut_3$IV[i] <- hist_IVs$IV[which(hist_IVs$PLOT == store_fut_3$PLOT[i])] + 
          (HMF_projected_change_percent_3/100*hist_IVs$IV[which(hist_IVs$PLOT == store_fut_3$PLOT[i])])
      }
      else if (HMF_projected_change_3 < 0) {
        store_fut_3$IV[i] <- hist_IVs$IV[which(hist_IVs$PLOT == store_fut_3$PLOT[i])] - 
          (HMF_projected_change_percent_3/100*hist_IVs$IV[which(hist_IVs$PLOT == store_fut_3$PLOT[i])])
      }
    }
    else {
      store_fut_3$IV[i] <- NA
    }
  }
  #### Write table to csv
  write.csv(store_fut_3, file = paste("HMF_SSP3_", spp_abbr, ".csv", sep = ""), row.names = FALSE)
  ### SSP5
  for (i in (1:nrow(store_fut_5))) {
    if (length(which(hist_IVs$PLOT == store_fut_5$PLOT[i])) > 0){
      if (HMF_projected_change_5 > 0) {
        store_fut_5$IV[i] <- hist_IVs$IV[which(hist_IVs$PLOT == store_fut_5$PLOT[i])] + 
          (HMF_projected_change_percent_5/100*hist_IVs$IV[which(hist_IVs$PLOT == store_fut_5$PLOT[i])])
      }
      else if (HMF_projected_change_5 < 0) {
        store_fut_5$IV[i] <- hist_IVs$IV[which(hist_IVs$PLOT == store_fut_5$PLOT[i])] - 
          (HMF_projected_change_percent_5/100*hist_IVs$IV[which(hist_IVs$PLOT == store_fut_5$PLOT[i])])
      }
    }
    else {
      store_fut_5$IV[i] <- NA
    }
  }
  #### Write table to csv
  write.csv(store_fut_5, file = paste("HMF_SSP5_", spp_abbr, ".csv", sep = ""), row.names = FALSE)
  
  
  ## Save model importance
  varimp[,var] <- RFmodel$importance[,1]/max(RFmodel$importance[,1])
  var <- var + 1
}

#### ACCOUNT FOR PEST IMPACT ####
predchange_climate <- predchange
predchange$`Fraxinus americana`[3:6] <- 0
predchange$`Fagus grandifolia`[3:6] <- 0

#### GENERATE MODEL ERROR PLOTS ####
modelerror <- data.frame(matrix(data = NA, nrow = 32, ncol = 3))
colnames(modelerror) <- c("Species", "ImportanceValue", "Type")
modelerror$Species <- rep(colnames(predchange), each = 2)
for (i in (2*(1:16)-1)) {
  j = (i + 1)/2
  modelerror$Type[i] <- c("Historical")
  modelerror$ImportanceValue[i] <- predchange[1,j]
}
for (i in (2*(1:16))) {
  j = i/2
  modelerror$Type[i] <- c("Model Estimate")
  modelerror$ImportanceValue[i] <- predchange[2,j]
}
# modelerror <- modelerror[order(modelerror$ImportanceValue, decreasing = TRUE),]
modelerror$Species = with(modelerror, reorder(Species, ImportanceValue, decreasing = TRUE))
ggplot(data = modelerror, mapping = aes(x = Species, y = ImportanceValue)) + 
  geom_point(aes(color = Type, shape = Type), size = 3) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), plot.margin = unit(c(.2,.2,.2,.5), "inches")) +
  labs(x = "", y = "Importance Value \n")
ggsave("modelerrors.jpeg")


### EXPORT MODEL IMPORTANCE ####
install.packages("webshot2","gt")
library(webshot2)
library(gt)
gtsave(data_color(gt((round(varimp, digits = 2)), rownames_to_stub = TRUE),
                  method = c("quantile"), quantiles= 9, palette = "YlGnBu"),
       filename = "allsppvarimp.png")


#### GENERATE BAR CHART FOR HMF ####
library(ggplot2)
library(RColorBrewer)
bar <- as.data.frame(matrix(data = NA, nrow = 16, ncol = 3))
colnames(bar) <- c("Species", "ImportanceValue", "Time")
for (i in (1:ncol(predchange))) {
  bar[i,1] <- colnames(predchange[i])
  bar[i,2] <- predchange[1,i]
  bar[i,3] <- c("1960-1990")
  bar[(i+16),1] <- colnames(predchange[i])
  bar[(i+16),2] <- predchange[5,i]
  bar[(i+16),3] <- c("2070-2100")
}

ggplot(data = bar, aes(x = Species, y = ImportanceValue, fill = Time)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), plot.margin = unit(c(.2,.2,.2,.5), "inches"))
ggsave("HMFbeforeandafter.jpeg")

#### GENERATE PREDICTED CHANGE CHART ####
library(dplyr)
library(ggplot2)
perc_change <- as.data.frame(matrix(data = NA, nrow = 32, ncol = 3))
colnames(perc_change) <- c("species", "Cause", "change")
perc_change$species <- rep(colnames(predchange_climate), each = 2)
for (i in (2*(1:16))) {
  perc_change$Cause[i] <- c("Climate")
  perc_change$change[i] <- (predchange_climate[5,(i/2)] - predchange_climate[1,(i/2)])/predchange_climate[1,(i/2)]*100
}
for (i in (2*(1:16)-1)) {
  perc_change$Cause[i] <- c("Pests")
  perc_change$change[i] <- 0
  if (perc_change$species[i] == "Fraxinus americana") {
    perc_change$change[i] <- - 100
  }
  else if (perc_change$species[i] == "Fagus grandifolia") {
    perc_change$change[i] <- - 100
  }
}
ggplot(data=perc_change, aes(x=reorder(species, -change), y=change, fill=Cause)) +
  geom_bar(stat="identity") + labs(y = "Projected % change in IV", x = "") + 
  coord_flip() + ylim(-100,100) + theme_bw()
ggsave("spplevelchange.jpeg")

#### WRITE PREDICTION TABLE TO CSV ####
write.csv(predchange, file = "allsppchange.csv", row.names = TRUE)