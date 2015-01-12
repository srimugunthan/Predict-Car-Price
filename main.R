library("car")
library("lmtest")
library("caret")
library("lattice")
library("MASS")
mainDir="/home/srimugunthan/Predict-Car-Price"



construct_indicator <- function(tabname, varname)
{
  factors = levels(tabname[[varname]])
  print( factors)
  
  #commented below
  #   
  for(i in 1:length(factors)) 
  {
    newvarname <- paste(varname,".",(factors[i]),sep="")
    tabname[, newvarname] <- NA
    
    
    
    thiscol <- tabname[,newvarname]
    cmpcol <- tabname[[varname]]
    for (j in 1:nrow(tabname))
    {
      
      if(cmpcol[j] == factors[i])
      {
        thiscol[j] <- 1
      }
      else
      {
        thiscol[j] <- 0
      }
    }
    tabname[, newvarname] <- thiscol
    
  }
  return (tabname)
}


collapse_modeltype <- function(carpricetab)
{
  
  #   newvarnames = c("modelgroup1","modelgroup2","modelgroup3")
  #   for (i in 1:length(newvarnames))
  #   {
  #     carpricetab[, newvarnames[i]] <- NA
  #   }
  str(carpricetab)
  factors = levels(carpricetab$Model)
  modelgroup <- rep("modelgroup1", length(factors))
  # everything in modelgroup1
  for (i in 1:length(factors))
  {
    modelgroup[factors[i]] = "modelgroup1"
  }
  var <- "Corvette"
  print(modelgroup[[var]])
  for (row in 1:nrow(carpricetab))
  {
    modeltype <- carpricetab$Model[row] 
    if(carpricetab$Price[row] < 20000)
    {
      # do nothing
    }
    else if(carpricetab$Price[row] < 40000)
    {
      if( modelgroup[[modeltype]] == "modelgroup1")
      {
        #upgrade to modelgroup2
        modelgroup[modeltype] = "modelgroup2"
      }
    }
    else
    {
      #upgrade to modelgroup3
      if( modelgroup[[modeltype]] != "modelgroup3")
      {
        modelgroup[modeltype] = "modelgroup3"
      }
    }  
    
  }
  carpricetab[,"model.group"] <- NA
  for (row in 1:nrow(carpricetab))
  {
    modeltype <- carpricetab$Model[row] 
    carpricetab$model.group[row] <- (modelgroup[modeltype] )
    
  }
  return (carpricetab)
}

collapse_trimtype <- function(carpricetab)
{
  
  #   newvarnames = c("modelgroup1","modelgroup2","modelgroup3")
  #   for (i in 1:length(newvarnames))
  #   {
  #     carpricetab[, newvarnames[i]] <- NA
  #   }
  str(carpricetab)
  factors = levels(carpricetab$Trim)
  trimgroup <- rep("trimgroup1", length(factors))
  # everything in modelgroup1
  for (i in 1:length(factors))
  {
    trimgroup[factors[i]] = "trimgroup1"
  }
  
  
  for (row in 1:nrow(carpricetab))
  {
    trimtype <- carpricetab$Trim[row] 
    if(carpricetab$Price[row] < 20000)
    {
      # do nothing
    }
    else if(carpricetab$Price[row] < 40000)
    {
      if( trimgroup[[trimtype]] == "trimgroup1")
      {
        #upgrade to modelgroup2
        trimgroup[trimtype] = "trimgroup2"
      }
    }
    else
    {
      #upgrade to modelgroup3
      if( trimgroup[[trimtype]] != "trimgroup3")
      {
        trimgroup[trimtype] = "trimgroup3"
      }
    }  
    
  }
  carpricetab[,"trim.group"] <- NA
  for (row in 1:nrow(carpricetab))
  {
    trimtype <- carpricetab$Trim[row] 
    carpricetab$trim.group[row] <- ( trimgroup[trimtype])
    
  }
  return (carpricetab)
}

model1 <- function(x) {
  
  set.seed(23)
  index <- 1:nrow(carpricetab)
  #training.rows <- sample(index, trunc(length(index)/2))
  
  training.rows <- createDataPartition(carpricetab$Price, p = 0.8, list = FALSE)
  traindataset <- carpricetab[training.rows, ]
  testdataset <- carpricetab[-training.rows, ]
  
  subDir="model1"  
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  setwd(file.path(mainDir, subDir))
  model1<-lm(traindataset$Price ~ traindataset$Leather
             +traindataset$Cruise+traindataset$Cylinder
             +traindataset$Type+traindataset$Make)
  
  red.model <- step(model1,direction = "backward")
  sink(file = "analysis-output.txt")
  cat("=====================================================\n")
  
  print(summary(red.model))
  
  cat("=====================================================\n")
  cat("========Multicollinearity tests======================\n")
  cat("========VIF (>5 high collinearity=====================\n")
  print(vif(red.model))
  cat("=====================================================\n")
  #summary(model1)
  # Start writing to an output file
  
  cat("===========Heteroskedasticity plot==================\n")
  
  cat("=======studentized Breusch-Pagan test===============\n")
  cat("If the test is positive (low p value), you should
      see if any transformation of the dependent
      variable helps you eliminate heteroscedasticity.
      Also check if the right hand side of the model is okay.\n")
  cat("=====================================================\n")
  
  bp <- bptest(red.model)
  print(bp)
  cat("=========================hccm========================\n")
  print(hccm(red.model))
  cat("========================coeff test===================\n")
  
  cat("If bptest does not work, you can use the
      white’s heteroscedasticity-corrected 
      covariance matrices to make inference.\n")
  
  print(coeftest(red.model,vcov=hccm(red.model)))
  # Append to the file
  
#residual plot
  pdf("residualplot.pdf", width=7, height=7)
  # eruption.lm = lm(eruptions ~ waiting, data=faithful) 
  eruption.res = resid(red.model)
  plot(traindataset$Price, eruption.res, 
              ylab="Residuals", xlab="Price", 
              main="Residual plot ") 
  abline(0, 0)                  # the horizon
  #http://www.r-tutor.com/elementary-statistics/simple-linear-regression/residual-plot
  dev.off()
  
  #Observed vs predicted graph
  pdf("actualvspred.pdf", width=7, height=7)
#png("image.png", width = 2000, height = 1500, res = 85);
  pred <- fitted(red.model)
  predtab<-as.data.frame(pred)
  names(predtab)[1]<-paste("Price")
  
  #pred <- predict(red.model, testdataset)
  #print(head(pred))


#ggplotXY <- ggplot(scatterPlotData, aes(x=x, y=y, colour=labels, label=labels)) +
 # geom_point() +
  #geom_text(hjust=0, vjust=0)
#ggplotXY
#dev.off()


#  plot(x=predtab$Price, y=traindataset$Price, 
 #      col=c(predtab$Price+traindataset$Price,predtab$Price+traindataset$Price), 
  #     pch=c(16,17))
  #par(new=TRUE)
  df <- cbind(predtab$Price, traindataset$Price)
  #xyplot(predtab$Price~traindataset$Price,type=c('l','p'),groups= variable,data=df,auto.key=T)
par(mar=c(5, 6, 2, 4))
  plot(predtab$Price,type="l",col="red")
  mtext("Predicted", side=2, line=3.5)
  par(new=TRUE)
  #lines(traindataset$Price,col="blue")
  plot(traindataset$Price,type="l",col="blue" )
  mtext("Actual", side=4, line=2)
title("Using par(new=TRUE) \n same X and different or not Y axis ")
axis(4)
#legend('topright', names(df)[-1] ,        lty=1, col=c('red', 'blue'), bty='n', cex=.75)
  
  #abline(red.model, col="red")
  dev.off()
  
  sink(NULL)
  #res <- stack(data.frame(Observed = dat$y, Predicted = fitted(red.model)))
  #res <- cbind(res, x = rep(dat$x, 2))
  #xyplot(values ~ x, data = res, group = ind, auto.key = TRUE)
  #xyplot(y ~ x, data = dat, type = c("p","r"), col.line = "red")
  #http://stackoverflow.com/questions/17432142/plot-the-observed-and-fitted-values-from-a-linear-regression-using-xyplot-from
  
  pdf("mygraph.pdf", width=7, height=7)
  oldpar <- par(mfrow = c(2, 2))
  plot(red.model)
  par(oldpar)
  dev.off()
  return(red.model)
}









  


model2 <- function(x) {
  subDir="model2"  
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  setwd(file.path(mainDir, subDir))
  
  collapse_modeltype()
  
  # create indicator variables
  model1<-lm(carpricetab$Price ~ carpricetab$Leather
             +carpricetab$Cruise+carpricetab$Cylinder
             +carpricetab$Type+carpricetab$Make)
  
  red.model <- step(model1,direction = "backward")
  sink(file = "analysis-output.txt")
  cat("=============================\n")
  print(summary(red.model))
  #summary(model1)
  # Start writing to an output file
  
  
  
  
  
  # Append to the file
  
  
  sink(NULL)
  pdf("mygraph.pdf", width=7, height=7)
  oldpar <- par(mfrow = c(2, 2))
  plot(red.model)
  par(oldpar)
  dev.off()
  return(red.model)
  
}


model3 <- function(x) {
  subDir="model3"  
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  setwd(file.path(mainDir, subDir))
  
  # create new transformed vars
  
  
}
printmodel_diagnostics2 <- function(lmmodel,ontabl,targetv)
{
  
  cat("=====================================================\n")
  cat("========Multicollinearity tests======================\n")
  cat("========VIF (>5 high collinearity=====================\n")
  print(vif(lmmodel))
  cat("=====================================================\n")
  #summary(model1)
  # Start writing to an output file
}

printmodel_diagnostics3 <- function(lmmodel,ontabl,targetv)
{
  
  cat("===========Heteroskedasticity plot==================\n")
  
  cat("=======studentized Breusch-Pagan test===============\n")
  cat("If the test is positive (low p value), you should
      see if any transformation of the dependent
      variable helps you eliminate heteroscedasticity.
      Also check if the right hand side of the model is okay.\n")
  cat("=====================================================\n")
  
  bp <- bptest(lmmodel)
  print(bp)
  cat("=========================hccm========================\n")
  print(hccm(lmmodel))
  cat("========================coeff test===================\n")
  
  cat("If bptest does not work, you can use the
      white’s heteroscedasticity-corrected 
      covariance matrices to make inference.\n")
  
  print(coeftest(lmmodel,vcov=hccm(lmmodel)))
  # Append to the file
}
printmodel_diagnostics1 <- function(lmmodel,ontabl,targetv)
{
  
  
  sink(file = "analysis-output.txt")
  cat("=====================================================\n")
  
  print(summary(lmmodel))
  

  
  #residual plot
  pdf("residualplot.pdf", width=7, height=7)
  # eruption.lm = lm(eruptions ~ waiting, data=faithful) 
  eruption.res = resid(lmmodel)
  plot(ontabl[[targetv]], eruption.res, 
       ylab="Residuals", xlab=targetv, 
       main="Residual plot ") 
  abline(0, 0)                  # the horizon
  #http://www.r-tutor.com/elementary-statistics/simple-linear-regression/residual-plot
  dev.off()
  
  
#   
#   #Observed vs predicted graph
#   pdf("actualvspred.pdf", width=7, height=7)
#   #png("image.png", width = 2000, height = 1500, res = 85);
#   pred <- fitted(lmmodel)
#   predtab<-as.data.frame(pred)
#   names(predtab)[1]<-paste(targetv)
#   
#   #pred <- predict(red.model, testdataset)
#   #print(head(pred))
#   
#   
#   #ggplotXY <- ggplot(scatterPlotData, aes(x=x, y=y, colour=labels, label=labels)) +
#   # geom_point() +
#   #geom_text(hjust=0, vjust=0)
#   #ggplotXY
#   #dev.off()
#   
#   
#   #  plot(x=predtab$Price, y=traindataset$Price, 
#   #      col=c(predtab$Price+traindataset$Price,predtab$Price+traindataset$Price), 
#   #     pch=c(16,17))
#   #par(new=TRUE)
#   df <- cbind(predtab$Price, ontabl$Price)
#   #xyplot(predtab$Price~traindataset$Price,type=c('l','p'),groups= variable,data=df,auto.key=T)
#   par(mar=c(5, 6, 2, 4))
#   plot(predtab$Price,type="l",col="red")
#   mtext("Predicted", side=2, line=3.5)
#   par(new=TRUE)
#   #lines(traindataset$Price,col="blue")
#   plot(ontabl$Price,type="l",col="blue" )
#   mtext("Actual", side=4, line=2)
#   title("Using par(new=TRUE) \n same X and different or not Y axis ")
#   axis(4)
#   #legend('topright', names(df)[-1] ,        lty=1, col=c('red', 'blue'), bty='n', cex=.75)
#   
#   #abline(red.model, col="red")
#   dev.off()
  
  sink(NULL)
  #res <- stack(data.frame(Observed = dat$y, Predicted = fitted(red.model)))
  #res <- cbind(res, x = rep(dat$x, 2))
  #xyplot(values ~ x, data = res, group = ind, auto.key = TRUE)
  #xyplot(y ~ x, data = dat, type = c("p","r"), col.line = "red")
  #http://stackoverflow.com/questions/17432142/plot-the-observed-and-fitted-values-from-a-linear-regression-using-xyplot-from
  
  pdf("mygraph.pdf", width=7, height=7)
  oldpar <- par(mfrow = c(2, 2))
  plot(lmmodel)
  par(oldpar)
  dev.off()
  
}
modelsv1 <- function(x)
{
  subDir="modelsv1"  
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  setwd(file.path(mainDir, subDir))
  
  modelsv1<-lm(carpricetab$Price ~ carpricetab$Trim+carpricetab$Model)
  
  printmodel_diagnostics1(modelsv1,carpricetab,"Price")
  return (modelsv1)
  
}

modelsv2 <- function(carpricetab)
{
  subDir="modelsv2"  
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  setwd(file.path(mainDir, subDir))
  
  
  carpricetab <-collapse_modeltype(carpricetab)
  carpricetab <-collapse_trimtype(carpricetab)
  
  print(str(carpricetab))
  modelsv1<-lm(carpricetab$Price ~ carpricetab$trim.group+carpricetab$model.group)
  
  printmodel_diagnostics1(modelsv1,carpricetab,"Price")
  return (modelsv1)
  
}


modelsv3 <- function(carpricetab)
{
  subDir="modelsv3"  
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  setwd(file.path(mainDir, subDir))
  
  
  
  
  print(str(carpricetab))
  modelsv <-lm(carpricetab$Price ~ carpricetab$Type)
  
  printmodel_diagnostics1(modelsv,carpricetab,"Price")
  return (modelsv)
  
}

modelsv4 <- function(carpricetab)
{
  subDir="modelsv4"  
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  setwd(file.path(mainDir, subDir))
  
  
  
  
  
  carpricetab <- construct_indicator(carpricetab, "Type")
  print(str(carpricetab))
  modelsv <-lm(carpricetab$Price ~ carpricetab$Type.Convertible
               + carpricetab$Type.Coupe 
               + carpricetab$Type.Hatchback
               + carpricetab$Type.Sedan
               + carpricetab$Type.Wagon)
  
  printmodel_diagnostics1(modelsv,carpricetab,"Price")
  return (modelsv)
  
}

model_mileage1<- function(carpricetab)
{
  subDir="modelmileage1"  
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  setwd(file.path(mainDir, subDir))
  
  
  
  
  
  carpricetab <- construct_indicator(carpricetab, "Type")
  print(str(carpricetab))
  modelsv <-lm(carpricetab$Price ~ carpricetab$Mileage)
               
  
  printmodel_diagnostics1(modelsv,carpricetab,"Price")
  return (modelsv)
  
}


model_mileage2<- function(carpricetab)
{
  subDir="modelmileage2"  
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  setwd(file.path(mainDir, subDir))
  
  
  
  
  
  carpricetab <- construct_indicator(carpricetab, "Type")
  print(str(carpricetab))
  m1 <-lm(carpricetab$Price ~ carpricetab$Mileage)
  
  bc <- boxcox(m1)
  lambda <-bc$x[which.max(bc$y)]
  z <- (carpricetab$Price)^lambda
  m2 <- lm(z ~ carpricetab$Mileage)
  printmodel_diagnostics1(m2,carpricetab,"Price")
  return (m2)
  
}
setwd(file.path(mainDir))
carpricetab <- read.csv("./Cars_Retail_Price.csv", sep=",")
# lmfit1 <- model1(1)
 #lmfit2 <- model2(2)
 #model3(3)
model_mileage2(carpricetab)
setwd(file.path(mainDir))
# compFit1Fit2 <- anova(lmfit1, lmfit2)
# sink(file = "anovalomodel12.txt")
# cat("=============================\n")
# print(compFit1Fit2)
# cat("=============================\n")
# sink(NULL)
