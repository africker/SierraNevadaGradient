library(ggplot2)
library(gtools)
library(gbm)
library(tidyr)
library(caret)
library(gridExtra)
library(doParallel)

cl<-makeCluster(4)
registerDoParallel(cl)


# Get the functions which I have stored in a separate file
source("D:/DEV/Synes/SensorDataCleaning/M2NEON_Rfunctions.R")

setwd("E:/D17_GIS/PAPER_1_ANALYSIS/GBMs/MAX_CH/25m")

####################
# Boosted regression tree work

# Parameter recommendations from: 
# http://www.listendata.com/2015/07/gbm-boosted-models-tuning-parameters.html
##########
# interaction.depth = 6,
# n.trees; unclear: "Increasing N reduces the error on training set, but setting it too high may lead to over-fitting."
# shrinkage = 0.001; 0.01 for datasets with <10,000 records, 0.1 for >10,000 records, but we only have <200, so
#... the extra shrinkage will not make the models too slow to run...
#... One typically chooses the shrinkage parameter beforehand and varies the number of
#... iterations (trees) N with respect to the chosen shrinkage. Small shrinkage generally gives a better result,
#... but at the expense of more iterations (number of trees) required.
# n.minobsinnode = 10; "the minimum number of observations in trees' terminal nodes. Set n.minobsinnode = 10.
#... When working with small training samples it may be vital to lower this setting to five or even three."
# bag.fraction = 0.5 "the fraction of the training set observations randomly selected to propose the next
#... tree in the expansion. By default, it is 0.5"
# train.fraction = 1.0

set.seed(1)
#Any subsetting could be done here...
res <- 25 # The resolution of the study cells (to select the correct csv file, and to label outputs)
dfAll <- read.csv(sprintf("E:/D17_GIS/PAPER_1_ANALYSIS/GBMs/MAX_CH/25m/D17_%sm.csv", res))
#Data File  = D:\ASU\GRADIENT_PAPER\GBDs\CLEAN\D17_1000m.csv
ynames <- c("chm_MAXIMUM")

xnames <- colnames(dfAll[!(colnames(dfAll) %in% append(c("OBJECTID",
                                                         "TARGET_FID",
                                                         "Name",
                                                         "CENTROID_X",
                                                         "CENTROID_Y",
                                                         "Shape_Length",
                                                         "Shape_Area",
                                                         "COUNT",
                                                         "chm_MEAN",
                                                         "chm_SD",
                                                         "vg1_MEAN",
                                                         "vg1_STD",
                                                         "vg1_MAXIMUM",
                                                         "vg2_MEAN",
                                                         "vg2_STD",
                                                         "vg2_MAXIMUM",
                                                         "vg7_MEAN",
                                                         "vg7_STD",
                                                         "vg7_MAXIMUM",
                                                         "vg8_MEAN",
                                                         "vg8_STD",
                                                         "vg8_MAXIMUM",
                                                         "b01_MAXIMUM",
                                                         "b02_MAXIMUM",
                                                         "b03_MAXIMUM",
                                                         "b04_MAXIMUM",
                                                         "b05_MAXIMUM",
                                                         "b06_MAXIMUM",
                                                         "b07_MAXIMUM",
                                                         "b08_MAXIMUM",
                                                         "b09_MAXIMUM",
                                                         "b10_MAXIMUM",
                                                         "b11_MAXIMUM",
                                                         "b12_MAXIMUM",
                                                         "b13_MAXIMUM",
                                                         "b14_MAXIMUM",
                                                         "b15_MAXIMUM",
                                                         "b16_MAXIMUM",
                                                         "b17_MAXIMUM",
                                                         "b18_MAXIMUM",
                                                         "b19_MAXIMUM",
                                                         "f01_MAXIMUM",
                                                         "f02_MAXIMUM",
                                                         "f03_MAXIMUM",
                                                         "f04_MAXIMUM",
                                                         "f05_MAXIMUM",
                                                         "f06_MAXIMUM",
                                                         "f07_MAXIMUM",
                                                         "f08_MAXIMUM",
                                                         "f09_MAXIMUM",
                                                         "f10_MAXIMUM",
                                                         "f11_MAXIMUM",
                                                         "f12_MAXIMUM",
                                                         "f13_MAXIMUM",
                                                         "f14_MAXIMUM",
                                                         "tp1_MAXIMUM",
                                                         "tp2_MAXIMUM",
                                                         "tp3_MAXIMUM",
                                                         "tp4_MAXIMUM",
                                                         "tp5_MAXIMUM",
                                                         "tp6_MAXIMUM",
                                                         "tp7_MAXIMUM",
                                                         "tp8_MAXIMUM",
                                                         "tp9_MAXIMUM",
                                                         "t10_MAXIMUM",
                                                         "t11_MAXIMUM",
                                                         "t12_MAXIMUM",
                                                         "t13_MAXIMUM"),
                                                         ynames))])
for (yname in ynames) {

  # Remove any NAs in the dependent variables
  df<-dfAll[!(is.na(dfAll[,yname])),]
        
  # NOTE: Needed this line for some winter months when very few sensors had data,
  # but the cut quantity was arbitrary
  # Check how many unique y values are in dataset
  if (length(unique(df[,yname])) > 60) {
    
    # Create a grid of parameter space to run gbm for:
    gbmGrid <-  expand.grid(interaction.depth = 5,
                            n.trees = 10000, 
                            shrinkage = 0.001,
                            n.minobsinnode = 10)
    
    # Set up training control
    fitControl <- trainControl(method = "repeatedcv",
                               number=10, # <^ 10fold cross validation
                               repeats = 5, # do 5 repititions of cv
                               preProcOptions = list(thresh = 0.95),
                               #classProbs = TRUE, # Estimate class probabilities
                               #summaryFunction = twoClassSummary, # Use AUC to pick the best model
                               allowParallel = TRUE)
    
    # Separate into training and test data
    # NOTE: I'm not sure this section is working correctly,
    # we may need to look at the post-hoc use of testing data
    # as I haven't looked at making predictions for the microclimate paper
    inTraining <- createDataPartition(df[,yname], p=.75, list=FALSE)
    trainingdata <- df[ inTraining,]
    testingdata  <- df[-inTraining,]
    
    # This is where the model is run
    gbm.tune <- caret::train(x = df[, xnames],
                      y = df[, yname],
                      #get(yname)~.,
                      #data = trainingdata[c(xnames,yname)],
                      distribution = "gaussian",
                      method = "gbm", bag.fraction = 0.5,
                      #nTrain = round(nrow(df) *.75), #not sure how this works...already split train and test manually
                      trControl = fitControl,
                      verbose = TRUE,
                      tuneGrid = gbmGrid,
                      ## Specify which metric to optimize
                      metric = "RMSE") # "Rsquared")
    
    
    # Create output directories
    dir.create(sprintf("%sm_%s", res, yname))
    dir.create(sprintf("%sm_%s/PartialDependence", res, yname))

    # Write csvs containing the best model, and the list of tunining models
    write.csv(gbm.tune$bestTune, sprintf("%sm_%s/BestModel.csv", res, yname))
    write.csv(gbm.tune$results, sprintf("%sm_%s/TuningResults.csv", res, yname))
    
    
    plot_RelInf <- ggplot(data=summary(gbm.tune$finalModel)) +
      geom_bar(aes(x=reorder(var, rel.inf), y=rel.inf, fill=var), color="black", stat="identity") +
      scale_fill_discrete(guide=FALSE) +
      theme_bw() +
      labs(title = sprintf("Resolution = %sm\nDependent variable = %s", res, yname),
           x="Independent variables",
           y="Relative Influence") +
      coord_flip()
    
    ggsave(file=sprintf("%sm_%s/RelativeInfluence.png", res, yname),
           plot_RelInf, width=12,height=10, dpi=500) #change to make it bigger
    dev.off()

    # Make predictions using the test data set
    # NOTE: Issue here is I'm not sure if the R^2 values come from training or testing data
    #gbm.pred <- predict.train(gbm.tune,df[,xnames])
    #gbm.pred <- extractPrediction(list(gbm.tune), testX=testingdata[,xnames], testY=testingdata[,yname])
    gbm.pred <- extractPrediction(list(gbm.tune))
    gbm.pred$TARGET_FID <- df$TARGET_FID
    gbm.pred$ObsValue <- df[,yname]
    write.csv(gbm.pred,
    sprintf("%sm_%s/Predictions.csv", res, yname))
    
    # Relative influence values
    write.csv(summary(gbm.tune$finalModel),
              sprintf("%sm_%s/RelativeInfluence.csv", res, yname))
    
    nplots <- 0
    group <- 1
    for (varname in summary(gbm.tune$finalModel)$var) {
      nplots <- nplots + 1
      if (nplots == 1) {
        png(sprintf("%sm_%s/PartialDependence/Plot_%s.png", res, yname, group),
            width=800, height=800, res=100)
        par(mfrow=c(2,2))
        par(mar = c(4, 4, 2, 2), oma = c(1, 1, 1, 1))
        group <- group + 1
      }
      plot.gbm(gbm.tune$finalModel, i.var = varname, lwd=2, col = "blue")
      write.csv(plot.gbm(gbm.tune$finalModel, i.var = varname, lwd=2, col = "blue", return.grid=TRUE),
                sprintf("%sm_%s/PartialDependence/%s.csv", res, yname, varname))
      if (nplots == 4) {
        nplots <- 0
        dev.off()
      }
    }
    if (length(dev.list()) > 0) dev.off()
    graphics.off()
  }
}
     
stopCluster(cl)




