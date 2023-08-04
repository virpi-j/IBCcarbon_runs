# Uncertainty run settings for regional and segment level PREBAS-runs.
# In segment level runs, only no-harvest scenario is used in three regions. 
# In each region, The parameter and weather sets are the same in the xth 
# repetition. Local initial values are sampled independent of those.
# In segment level runs, the parameter, weather and initial value uncertainty
# samples are generated in baseline scenario run, and those are used also for 
# other harvest  scenarios.

#rm(list=ls())
#sampleID <- 4
print(paste("start UQ_run, date",Sys.Date()))
if(!exists("toRaster")) toRaster<-F
rcpfile="CurrClim"
library(data.table)
library(devtools)
library(MASS)
library(stringr)

CSCrun=T
devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/loadPackages.r")

if(zon10){
  library(Rprebasso)
  library(raster)
  library(sf)
  library(stars)
  library(dplyr)
}
ststDeadW<-FALSE
#source("localSettings.r")

outType <- "uncRun" # Setting for the runModel-function
if(uncSeg) outType <- "uncSeg"
#harvscen<- "Base" #"Base", adapt","protect","protectNoAdH","adaptNoAdH","adaptTapio"
#harvinten<- "Base" # "Base", "Low", "MaxSust", "NoHarv" 

if(unc100) nYears <-100
##### From GitHub
#source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/virpi-j/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
#if(nYears<50) nYears<-nYears+1

#HcFactor <- 1
funXX<-funX
#source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
#source_url("https://raw.githubusercontent.com/virpi-j/IBCcarbon_runs/master/general/functions.r")
source_url("https://raw.githubusercontent.com/virpi-j/IBCcarbon_runs/master/general/functions_UQ.r")


nSitesRun <- nSitesRunr
print(paste("start region",r_no,"run",outType,"- harvest scenario",harvscen,"-harvest intensity", harvinten,
            "- set size",nSitesRun,"- no of repetitions", nSamplesr))

# Climate model and RCP names0
climMod <- c("CanESM2.","CNRM.","GFDL.","HadGEM2.","MIROC.")
rcpx <- c("rcp26","rcp45","rcp85")

# Give new set of outputs ------------------------------------------------
if(uncSeg){
  if(!toRaster){
  varOuts <- c("NEP","DeadWoodVolume","soilC","V","D","BA","H") 
  } else {
    varOuts <- c("NEP","DeadWoodVolume","soilC","V","D","BA","H","VroundWood","WroundWood") 
  }
  # added; Wtot, age, Vdec, species, sitetype!
} else {
  varOuts <- c("NEP","V","npp","VroundWood","WroundWood",
               "grossGrowth","soilC","Litter_fol","Litter_fr",
               "Litter_fWoody","Litter_cWoody") # Wtot!
}
#cS <- c(-100^2*44/12, 1, 1, 1) # multipliers of areas (&NEE C->CO2eq) for tot.sums

varSel <- match(varOuts,varNames)
funX <- rep("sum",length(varSel))
funX[match(varNames[c(7,11:12)],varNames[varSel])] <- "baWmean"
#----------------------------------------------------------------------------
parPath <- "/scratch/project_2000994/PREBASruns/metadata/paramUnc/"

load(paste0("input/maakunta/maakunta_",r_no,"_IDsTab.rdata"))
data.all <- cbind(data.all,data.IDs[match(data.all$segID, data.IDs$maakuntaID),4:5])
finPeats <- raster("/scratch/project_2000994/MVMIsegments/segment-IDs/pseudopty.img")
drPeatID <- 400 # ID = 400 for luke database; drained peatland
print(drPeatID)
undrPeatID <- 700  ### ID = 700 for luke database; undrained peatland
if(file.exists(paste0("uncRuns/peatID_reg",r_no,".rdata"))){
  load(paste0("uncRuns/peatID_reg",r_no,".rdata"))
} else {
  print("Extract peatIDs...")
  peatIDs <-extract(finPeats, cbind(data.all$x,data.all$y))
  print("Save peatIDs.")
  save(peatIDs, file=paste0("uncRuns/peatID_reg",r_no,".rdata"))
}
data.all[,peatID:=peatIDs]

if(zon10){
  print("Start 10% cons preprosessing")
  load(paste0("input/maakunta/maakunta_",r_no,"_IDsCons10.rdata"))
  xDat <- cons10Dat
  consOut<-data.table()
  data.all<-copy(data.all)
  data.all[,newCons:=0]
  data.all[,oldMaakID:=maakuntaID]
  setnames(xDat,"nPix","N")
  xDat[,area:=N*16^2/10000]
  tmp1<-which(xDat$maakuntaID%in%data.all$maakuntaID)
  tmp2<-match(xDat$maakuntaID[tmp1],data.all$maakuntaID)
  xDat[tmp1,peatID:=data.all$peatID[tmp2]]
  xDat[tmp1,x:=data.all$x[tmp2]]
  xDat[tmp1,y:=data.all$y[tmp2]]
  xDat[tmp1-1,peatID:=data.all$peatID[tmp2]]
  xDat[tmp1-1,x:=data.all$x[tmp2]]
  xDat[tmp1-1,y:=data.all$y[tmp2]]
  xx<-match(names(data.all),names(xDat))
  data.all[tmp2,]<-xDat[tmp1,..xx]
  print(nrow(data.all))
  data.all<-rbind(data.all,xDat[tmp1-1,..xx])  
  print(nrow(data.all))
}
# Cut the joutomaa off
#extrLandclass <- 3
data.all <- data.all[which(!data.all$landclass%in%extrLandclass),]#!=3 & data.all$landclass!=2),]
print(paste("Leave landclass(es)",extrLandclass,"out"))

areas_all <- data.table(areatot = sum(data.all$area), 
                        areacons = sum(data.all[cons==1]$area),
                        area_min = sum(data.all$area[data.all$peatID==100]),
                        area_drpeat = sum(data.all$area[data.all$peatID==400]),
                        area_undrpeat = sum(data.all$area[data.all$peatID==700]),
                        area_nonfor = sum(data.all$area[data.all$peatID==0]),
                        area_min_cons = sum(data.all$area[data.all$peatID==100 & data.all$cons==1]),
                        area_drpeat_cons = sum(data.all$area[data.all$peatID==400 & data.all$cons==1]),
                        area_undrpeat_cons = sum(data.all$area[data.all$peatID==700 & data.all$cons==1]),
                        area_nonfor_cons = sum(data.all$area[data.all$peatID==0 & data.all$cons==1]))
print(areas_all)

if(ExcludeUndrPeatlands){
  # Exclude undrained peatlands
  undrpeatX <- data.all$peatID==undrPeatID
  print(paste0("Discard undrained ", sum(data.all$area[(which(undrpeatX))])," ha (",
               round(sum(data.all$area[(which(undrpeatX))])/sum(data.all$area)*1000)/10,
               " percent of the whole area)"))
  data.all <- copy(data.all[!undrpeatX,]) # cut off 
}
data.all[,consArea:=cons]
#print(data.all[1:2,])
# No harvests on "kitumaa", set here as conservation area
#data.all$cons[which(data.all$landclass==2)]<-1

filee <- paste0("uncRuns/regRuns/samplexout_reg",r_no,
                "_RCP45_Base_Base_samplesize",nSitesRunr,"_iters",nSamplesr,
                "_pr",uncPCrobas,"_Xr",uncInput,"_ager",uncAge,
                "_Cr",uncClim,"_str",uncSiteType,".rdata")

if(file.exists(filee)) {loadUnc<-TRUE
print("Old results found, use the same input sets to continue")}
set.seed(10)
if(loadParids){ 
  if(!uncSeg | toRaster) {
    load(paste0("uncRuns/regRuns/loadParids.rdata"))
  } else {
    load(paste0("uncRuns/segRuns/loadParids.rdata"))
  }
  print("Parameter set indexes loaded.")
} else {
  parids1 <- sample(1:999,1000, replace = TRUE)
  parids2 <- sample(1:999,1000, replace = TRUE)
  parids3 <- sample(1:999,1000, replace = TRUE)
  climModids <- sample(1:length(climMod), 1000,replace=TRUE)
}

if(uncRun){ # load distribution data
  
  if(loadUnc){
    if(!uncSeg){
      load(paste0("uncRuns/regRuns/opsInd_reg",r_no,"_uncSeg",uncSeg,".rdata")) 
      sampleIDs <- 1:nSamplesr
      area_total <- sum(data.all$area)#areas_all[1,1]
      areas <- data.all$area
      areas <- areas/area_total
    } 
  }
  if(uncPCrobas){
    load(paste0(parPath,"pCROB_unc.rdata"))
    parindCrob <- parind
    pCrobdim <- nrow(pCROBbirch)
  }
  if(uncPPrel){
    load(paste0(parPath,"pPREL_unc.rdata"))
    parindPrel <- parind
    pPreldim <- nrow(pPREL_unc)
  }
  if(uncPYas){
    data <- read.delim(paste0(parPath,"Yasso15.dat"), header = TRUE, sep="\t") 
    pYasdim <- length(data[[1]])
    pYas_unc <- matrix(0,pYasdim,35)
    for(ind in 1:pYasdim){ # read parameter values to matrix
      pYas_unc[ind,] <-as.numeric(unlist(str_split(data[[1]][[ind]], pattern = "  ")))[2:36]
    }
  }
} # if(uncRun){ # load distribution data
#----------------------------------------------------------------------------
# Generate nSamples sample initial value sets
if(!uncSeg & !loadUnc){ # sample pixel indices
  ops <- list()
  
  if(!toRaster){
    sampleIDs <- 1:nSamplesr
    area_total <- sum(data.all$area)
    areas <- data.all$area
    areas <- areas/area_total
    opsInd <- list() 
    for(ij in 1:nSamplesr){ 
      opsInd[[ij]] <- sample(1:nrow(data.all), nSitesRunr, 
                             replace = sampleReplace, prob = areas)
      ops[[ij]] <- copy(data.all[opsInd[[ij]],])
    }
  } else {
    nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
    sampleIDs <- split(1:nSamples,             # Applying split() function
                       cut(seq_along(1:nSamples),
                           10,#nSetRuns,
                           labels = FALSE))[[setX]]
    ops_orig <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
  }
} else if(uncSeg){ # if(!uncSeg & !loadUnc)
  if(!loadUnc){
    nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
    sampleIDs <- split(1:nSamples,             # Applying split() function
                       cut(seq_along(1:nSamples),
                           10,#nSetRuns,
                           labels = FALSE))[[setX]]
    ops_orig <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
    save(ops_orig,nSamples,sampleIDs,file=paste0("uncRuns/segRuns/opsdata",r_no,".rdata"))
  } else {
    load(paste0("uncRuns/segRuns/opsdata",r_no,".rdata"))
  }
  if(testRun){
    sampleIDs <- c(1:min(sampleID,nSamples))
  } else {
    sampleIDs <- c(1:nSamples)
  }
}

# Load peatland post-processing raster for uncRun and allocate emission factor vectors
if(toRaster | (uncRun & !uncSeg & uncPeat)){
  soilSyke <- FALSE  ####If TRUE uses Syke peatland database if FALSE uses luke database
  # luke database pseudoptyp.img: Whole Finland, 100 = mineral soil, 400 = drained peatland, 700=other peatland, 0=non-forest
  # syke database peatSyke16res.tif: 1 = Undrained peatland; 2 = Drained peatland; 3 = Peat extraction area 
  if(soilSyke){
    finPeats <- raster("/scratch/project_2000994/MVMIsegments/segment-IDs/peatSyke16res.tif")
    drPeatID <- 2  ### ID = 2 for syke database
  }else{
    finPeats <- raster("/scratch/project_2000994/MVMIsegments/segment-IDs/pseudopty.img")
    drPeatID <- 400  ### ID = 400 for luke database; 
  }
  #Emission coefficients for peatland post-processing
  if(!loadParids){  
    EC1 <- matrix(-240, 1, 1000)#270  
    EC2 <- matrix(70, 1, 1000)#70
  }
}

if((uncRun & uncHarv) & !loadParids){
  harvestLimsr <- data.table(t(matrix(1,2,1000)))
  print("Uncertain harvest targets")
  harvestLimsr[-1,1] <- (hlimf[1]+0.02*rnorm(1000-1)) #roundwood
  harvestLimsr[-1,2] <- (hlimf[2]+0.02*rnorm(1000-1)) #energywood
} else if(uncRun & !uncHarv) {
  print("Harvest targets constant")
  harvestLimsr <- data.table(t(matrix(1,2,1000)))
}
#print(harvestLimsr[1:2,])

if(uncRun & !loadParids){
  if(uncClim){ # weather for iterations
    set.seed(NULL)
    resampleYears <- matrix(sample(1:nYears,nYears*1000,replace=T),
                            ncol = nYears,nrow = 1000)
    resampleYears[,1:(2021-2015)] <- matrix(1:(2021-2015),ncol = (2021-2015),nrow = 1000, byrow = T)
    
    if(uncSeg) resampleYears1 <- resampleYears
  }
  if(uncSeg){
    save(resampleYears1, file=paste0("uncRuns/segRuns/resampleyears.rdata"))
  } else {
    save(resampleYears, file=paste0("uncRuns/regRuns/resampleyears.rdata"))
  }
} else {
  if(uncSeg & !toRaster){
    load("uncRuns/segRuns/resampleyears.rdata")
    #resampleYears1 <- resampleYears
  } else {
    load("uncRuns/regRuns/resampleyears.rdata")
  }
}

if(unc100 & !loadParids){
  print("Generate random sets of years")
  set.seed(NULL)
  resampleYears <- matrix(sample(1:nYears,nYears*1000,replace=T),
                          ncol = nYears,nrow = 1000)
  resampleYears[,1:(2021-2015)] <- matrix(1:(2021-2015),ncol = (2021-2015),nrow = 1000, byrow = T)
  save(resampleYears, file=paste0("uncRuns/regRuns/resampleyears100.rdata"))
} else if(unc100) {
  print("load random set of 100 years")
  load(paste0("uncRuns/regRuns/resampleyears100.rdata"))
}


if(uncRun){# sample model parameters, HcFactor and peatland emission coefficients
  pCROBASr <- list()
  pPRELr <-  t(matrix(pPREL,ncol=nSamplesr,nrow=length(pPREL)))#data.frame()
  pYASr <- t(matrix(pYAS,ncol=nSamplesr,nrow=length(pYAS)))#data.frame()
  pCROBmean <- rbind(colMeans(pCROBpine),colMeans(pCROBspruce),
                     colMeans(pCROBbirch))
  if(!exists("biasCorr")) biasCorr<-T
  print(paste("Bias correction to parameters",biasCorr))
  for(ij in 1:nSamplesr){ 
    pCROBASr[[ij]] <- pCrobasX#pCROB
    if(uncPCrobas & ij>1){
      if(biasCorr){
        pCROBASr[[ij]][parindCrob,1:3]<-t(rbind(pCROBpine[parids1[ij],],
                                                pCROBspruce[parids1[ij],],
                                                pCROBbirch[parids1[ij],]))+
          pCrobasX[parindCrob,1:3]-t(pCROBmean)
      } else {
        pCROBASr[[ij]][parindCrob,1:3]<-t(rbind(pCROBpine[parids1[ij],],
                                                pCROBspruce[parids1[ij],],
                                                pCROBbirch[parids1[ij],]))}
    }
    if(uncPPrel & ij>1){
      if(biasCorr){
        pPRELr[ij,parindPrel] <- pPREL_unc[parids2[ij],]+
          pPREL[parindPrel] - colMeans(pPREL_unc)
      } else {
        pPRELr[ij,parindPrel] <- pPREL_unc[parids2[ij],]
      }
    }
    if(uncPYas & ij>1){
      if(biasCorr){
        pYASr[ij,] <- pYas_unc[parids3[ij],]+pYAS-colMeans(pYas_unc)
      } else {
        pYASr[ij,] <- pYas_unc[parids3[ij],]}
    }
    
    if(uncPeat & !uncSeg & !loadParids){
      EC1[ij] <- -240
      EC2[ij] <- 70
      if(ij>1){  
        EC1[ij] <- -240 + 70*rnorm(1)#270  
        EC2[ij] <- 70 + 30*rnorm(1)#70
      }
    }
  }
  if(uncHcFactor & !loadParids){
    HcFactorr <- matrix(1,1,1000)
    HcFactorr[-1] <- 1 + rHcFactor*rnorm(1000-1)
  } else if(!uncHcFactor) {
    HcFactorr <- matrix(1,1,1000)
  }

  if(!uncSeg & !loadUnc){ # if region level uncertainty run, sample input variables
    # sample input values for the samples
    #if(uncInput){
    print(paste0("Calculate input uncertainties for ",length(sampleIDs)," sample sets..."))
    ops <-  uncVariables(ops=copy(ops), sampleIDs = sampleIDs,
                         rage = rage,
                         uncInput = uncInput, uncSiteType = uncSiteType, 
                         uncAge = uncAge) 
    print("... done.")
    #}
    
    
    #if(!loadUnc){#testRun){
    save(opsInd,ops,file=paste0("uncRuns/regRuns/opsInd_reg",r_no,"_uncSeg",uncSeg,".rdata"))
    print(paste("opsInds saved for region",r_no))
    #}
  } else if(!uncSeg & loadUnc){ # if region level uncertainty run, sample input variables
    # sample input values for the samples
    #if(uncInput){
    print(paste0("Load input uncertainties for ",length(sampleIDs)," sample sets..."))
    load(paste0("uncRuns/regRuns/opsInd_reg",r_no,"_uncSeg",uncSeg,".rdata"))
    print(paste("opsInds and ops loaded for region",r_no))
  } #if(!uncSeg & !loadUnc)   
}

if(!loadParids){
  if(!uncSeg){
    save(opsInd,parids1,parids2,parids3,harvestLimsr,climModids,resampleYears,EC1,EC2,HcFactorr,
         file=paste0("uncRuns/regRuns/loadParids.rdata")) 
  } else {
    save(parids1,parids2,parids3,harvestLimsr,climModids,resampleYears1,HcFactorr,
         file=paste0("uncRuns/segRuns/loadParids.rdata")) 
  }
  print("Indexes of parameter sets saved.")
}
RCPnames <- c("CurrClim","RCP26","RCP45","RCP85")
# load weather data  
if(uncRCP == 0){
  rcpfile = rcps
  rcpsname <- rcpfile
  if(rcpfile=="CurrClim"){
    load(paste(climatepath, rcpfile,".rdata", sep=""))  
    maxRday <- max(dat$rday)
    xday <- c(dat$rday,(dat$rday+maxRday),(dat$rday+maxRday*2))
    dat = rbind(dat,dat,dat)
    dat[,rday:=xday]
  }
  #} else if(uncRCP==1){
  #  rcpsname <- "RCP45"
  #} else if(uncRCP==2){
  #  rcpsname <- "RCP85"
} else {
  rcpsname <- RCPnames[uncRCP+1]
} 
##
sampleOutput <- list()

if(uncSeg){
  pCROBASrseg <- copy(pCROBASr)
  pPRELrseg <- copy(pPRELr)
  pYASrseg <- copy(pYASr)
  HcFactorrOr <- HcFactorr
}

#if(loadUnc){ # if needed to load previous sample
#  load(paste0("uncRuns/opsInd_reg",r_no,"_uncSeg",uncSeg,".rdata")) 
#}
nii0 <- 1
if(uncSeg && file.exists(paste0("uncRuns/segRuns/samplexout_uncSeg_reg",r_no,
                                "_iters",nSamplesr,"_NoHarv.rdata"))){
  load(paste0("uncRuns/segRuns/samplexout_uncSeg_reg",r_no,
              "_iters",nSamplesr,"_NoHarv.rdata"))
  #nii0<-(length(sampleOutput[[1]])-1)/3+1
  nii0<-as.numeric(strsplit(colnames(sampleOutput[[1]])[length(sampleOutput[[1]])],"per3.")[[1]][2])+1
}
if(!uncSeg & !unc100){
  nSamplesr0<-nSamplesr 
  nSamplesr<-min(nSamplesr,nSamplesSaved) # file where the old results are
  if(zon10){
    filee <- paste0("uncRuns/regRuns/samplexoutzon10_reg",r_no,
                    "_RCP45_Base_Base_samplesize",nSitesRunr,"_iters",nSamplesr0,
                    "_pr",uncPCrobas,"_Xr",uncInput,"_ager",uncAge,
                    "_Cr",uncClim,"_str",uncSiteType,".rdata")
  } else {
    filee <- paste0("uncRuns/regRuns/samplexout_reg",r_no,
                    "_RCP45_Base_Base_samplesize",nSitesRunr,"_iters",nSamplesr0,
                    "_pr",uncPCrobas,"_Xr",uncInput,"_ager",uncAge,
                    "_Cr",uncClim,"_str",uncSiteType,".rdata")
  }
  if(file.exists(filee)){
    sampleOutput<-list()
    for(uncRCPi in 1:length(uncRCPs)){
      uncRCP <- uncRCPs[uncRCPi]
      if(uncRCP==0){
        rcpsname <- "CurrClim"
        #rcps <- rcpsname
      } else {
        rcpsname <- RCPnames[uncRCPi]
      }
      sampleOutputtmp<-list()
      for(harvind in 1:length(harvintens)){
        harvinten <- harvintens[harvind]  
        harvindRCP <- length(harvintens)*(uncRCPi-1)+harvind
        
        if(zon10){
          filee <- paste0("uncRuns/regRuns/samplexoutzon10_reg",r_no,
                          "_",rcpsname,"_Base_",harvintens[harvind],
                          "_samplesize",nSitesRunr,"_iters",nSamplesr0,
                          "_pr",uncPCrobas,"_Xr",uncInput,"_ager",uncAge,
                          "_Cr",uncClim,"_str",uncSiteType,".rdata")
        } else {
          filee <- paste0("uncRuns/regRuns/samplexout_reg",r_no,
                          "_",rcpsname,"_Base_",harvintens[harvind],
                          "_samplesize",nSitesRunr,"_iters",nSamplesr0,
                          "_pr",uncPCrobas,"_Xr",uncInput,"_ager",uncAge,
                          "_Cr",uncClim,"_str",uncSiteType,".rdata")
        }
        load(filee)
        sampleOutput[[harvindRCP]]<-sampleOutputb
        #names(sampleOutput)[harvindRCP] <- names(sampleXs[[1]])[harvindRCP]
      }
      #sampleOutput[[uncRCP+1]]<-sampleOutputtmp
      #names(sampleOutput)[uncRCP+1]<-rcpsname
    }
    nii0 <- nrow(sampleOutput[[1]][[1]])
    #nii0 <- ceiling(nii0/nParRuns)+1
    print(paste("number of runs in old files:",nii0))
  }
  nSamplesr<-nSamplesr0 
}
#  if(file.exists(filee)){
#    sampleOutput<-list()
#    for(uncRCP in uncRCPs){
#      if(uncRCP==0){
#        rcpsname <- "CurrClim"
#      } else if(uncRCP==1){
#        rcpsname <- "RCP45"
##      } else if(uncRCP==2){
#        rcpsname <- "RCP85"
#      } 
#      sampleOutputtmp<-list()
#      for(harvind in 1:length(harvintens)){
#        harvinten <- harvintens[harvind]  
#        harvindRCP <- length(harvintens)*uncRCP+harvind
#        
#        filee <- paste0("uncRuns/regRuns/samplexout_reg",r_no,
#                        "_",rcpsname,"_Base_",harvintens[harvind],
#                        "_samplesize",nSitesRunr,"_iters",nSamplesr,
#                        "_pr",uncPCrobas,"_Xr",uncInput,"_ager",uncAge,
#                        "_Cr",uncClim,"_str",uncSiteType,".rdata")
#        load(filee)
#        sampleOutput[[harvindRCP]]<-sampleOutputb
#        #names(sampleOutput)[harvindRCP] <- names(sampleXs[[1]])[harvindRCP]
#      }
#    }
#    nii0 <- nrow(sampleOutput[[1]][[1]][[1]])
#  } else {
#    print("No previous results available!")
#  }
#}
rspecial <- 50
if(!uncSeg & nii0!=1){
  niter <- ceiling(nSamplesr/nParRuns)
  nii0 <- ceiling(nii0/nParRuns)+1
  if(r_no==rspecial){ 
    nii0 <- 10
    sampleOutput<-list()
  }
} else {
  niter <- max(1,ceiling(nSamplesr/nParRuns))
  #  niter <- nSamplesr
}
if(nii0 > niter) nii0<-1
print(paste("start from iteration",nii0))
niter2 <- niter
if(uncSeg) niter2 <- 53
if(toRaster) niter2<-1
#if(!exists("mortMod")) 
mortMod <- 13
if(!exists("landClassUnman")) landClassUnman <- 2
print(paste("landClassUnman=",landClassUnman))
print(paste("mortMod=",mortMod))

#if(HcFactor!=1) HcFactorr[1]<-as.numeric(HcFactor*HcFactorr[1])

############################################################################################################
for(nii in nii0:niter2){
  toMem <- ls()
  startRun <- Sys.time() 
  print(paste0("Start running iter ",nii,"/",niter2,"..."))
  if(uncSeg | toRaster){ # load random input data
    if(!toRaster){
      resampleYears<-matrix(resampleYears1[nii,], nrow= tail(sampleIDs,n=1), 
                            ncol=length(resampleYears1[nii,]), byrow=TRUE)
    } 
    ops <- copy(ops_orig)
    
    if(uncInput & nii>1){
      print(paste0("Calculate input uncertainties for ",length(sampleIDs),"..."))
      ops <-  uncVariables(ops=ops, sampleIDs = sampleIDs)
      print("... done.")
    }
    if(uncInput & nii==1){
      print("First iteration without input uncertainty.")
    }
    HcFactorr <- matrix(1,1,length(sampleIDs))
    if(uncHcFactor & nii>1){
      HcFactorr <- matrix(HcFactorrOr[nii],1,length(sampleIDs))#(1 + rHcFactor*rnorm(1))*matrix(1,1,length(sampleIDs))#length(sampleIDs))
    }
    for(ij in sampleIDs){ 
      pCROBASr[[ij]] <- pCROBASrseg[[nii]]
      pPRELr[ij,] <- pPRELrseg[nii,]
      pYASr[ij,] <- pYASrseg[nii,]
    }
    
  }
  #set.seed(.Random.seed[r_no])
  
  #sampleXs <- lapply(sampleIDs[1:3], function(jx) { runModel(jx, outType=outType)})      
  #sampleXs <- mclapply(sampleIDs[(1+(nii-1)*nParRuns):(nii*nParRuns)], function(jx) {
  #source_url("https://raw.githubusercontent.com/virpi-j/IBCcarbon_runs/master/general/functions.r")
  reStartYearUnc <- 7
  source_url("https://raw.githubusercontent.com/virpi-j/IBCcarbon_runs/master/general/functions.r")
  #source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
  print("start runModel")
  if(testRun){# | (toRaster & uncSeg)){ # if needed to test an individual sample
    if(uncSeg){
      harvScen<-harvscen
      harvInten<-harvInten
      manualRun<-F
      if(manualRun){
        uncRCP=0 
        segScen="Base"
        easyInit=FALSE
        forceSaveInitSoil=F 
        cons10run = F
        procDrPeat=F
        coeffPeat1=-240
        coeffPeat2=70
        coefCH4 = 0.34 #g m-2 y-1
        coefN20_1 = 0.23 
        coefN20_2 = 0.077 #g m-2 y-1
        landClassUnman=2 
        compHarvX = 2
        funPreb = regionPrebas
        initSoilCreStart=NULL
        outModReStart=NULL
        reStartYear=1
        sampleX=NULL  
      }
      if(toRaster){
        sampleXs <- lapply(sampleIDs, function(jx) { 
          runModel(jx,outType="uncRun", harvScen=harvscen,uncRCP=uncRCP,nYears=nYears,
                   compHarvX = compHarvX, landClassUnman=landClassUnman,
                   harvInten=harvinten, procDrPeat = uncPeat, toRaster = toRaster)})
      } else {
      sampleXs <- lapply(sampleIDs, function(jx) { 
        runModel(jx,outType=outType, harvScen=harvscen,uncRCP=uncRCP,nYears=nYears,
                 compHarvX = compHarvX, landClassUnman=landClassUnman,
                 harvInten=harvinten, procDrPeat = uncPeat, toRaster = toRaster)})
      }
    }else{
      harvScen<-harvscen
      harvInten<-harvInten
      manualRun<-F
      if(manualRun){
        easyInit=FALSE
        forceSaveInitSoil=F 
        cons10run = F
        funPreb = regionPrebas
        procDrPeat=T
        coeffPeat1=-240
        coeffPeat2=70
        coefCH4 = 0.34#g m-2 y-1
        coefN20_1 = 0.23
        coefN20_2 = 0.077#g m-2 y-1
        landClassUnman=2
        compHarvX = 2
        initVar=NULL
        initSoilC=NULL
        reInit=F
        sampleX=NULL
        #funX = regionPrebas
        initSoilCreStart=NULL
        outModReStart=NULL
        reStartYear=1
      }
      if(unc100){
        uncRCPs <- uncRCPs[1]
        harvintens <- c("Base","NoHarv")
        print("Run 100 year")
      }
      sampleXs <- lapply(sampleIDs, function(jx) { 
        outXcc <- list()
        ind <-1
        
        # Initialize soil and deadwood
        rcpsname <- "CurrClim"
        rcps <- rcpsname
        print(harvinten)
        if(toVemala){
          nYears<-2050-2015
          #toVemala<-F
          #source_url("https://raw.githubusercontent.com/virpi-j/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
          #source_url("https://raw.githubusercontent.com/virpi-j/IBCcarbon_runs/master/general/functions.r")
          print(paste("initialize for period 2015-2050 =",nYears,"/ testRun"))
          outtmp <- runModel(jx, outType=outType, harvScen="Base",uncRCP=0,nYears=nYears,
                             compHarvX = compHarvX, landClassUnman=landClassUnman,
                             harvInten="Base", procDrPeat = uncPeat)
          nYears<-2100-2015
          #toVemala<-T
          #source_url("https://raw.githubusercontent.com/virpi-j/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
          #source_url("https://raw.githubusercontent.com/virpi-j/IBCcarbon_runs/master/general/functions.r")
        } else {
        outtmp <- runModel(jx, outType=outType, harvScen="Base",uncRCP=0,nYears=nYears,
                           compHarvX = compHarvX, landClassUnman=landClassUnman,
                           harvInten="Base", procDrPeat = uncPeat)
        }
        for(uncRCP in uncRCPs){
          harvscen<-"Base" 
          harvinten <- "Base"
          if(uncRCP==0){
            rcpsname <- "CurrClim"
            rcps <- rcpsname
          } else {
            rcpsname <- RCPnames[uncRCP+1]
          }
          print(harvinten)
          
          #source_url("https://raw.githubusercontent.com/virpi-j/IBCcarbon_runs/master/general/functions.r")
          #source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
          outtmp <- runModel(jx, outType=outType, harvScen=harvscen,uncRCP=uncRCP,nYears=nYears,
                             compHarvX = compHarvX, landClassUnman=landClassUnman,
                             harvInten=harvinten, procDrPeat = uncPeat, toRaster = toRaster)
          outXcc[[ind]] <- outtmp
          print(outtmp$WroundWood)
          print(outtmp$NEP)
          names(outXcc)[ind] <- paste0(harvscen,"_",harvinten,"_",rcpsname)
          if(!toRaster){
          ind<-ind+1
          print(paste("Load 2015-2021 results for sampleID",jx))
          load(file=paste0("uncRuns/regRuns/restartRun_uncRun",r_no,"_",jx,".rdata"))
          for(harvind in 2:length(harvintens)){
            harvinten <- harvintens[harvind]
            print(harvinten)
            harvscen <- "Base"
            if(harvinten=="NoHarv") harvscen<-"NoHarv"
            print(harvinten)
            manualRun<-F
            if(manualRun){
              easyInit=FALSE
              forceSaveInitSoil=F 
              cons10run = F
              funPreb = regionPrebas
              procDrPeat=T
              coeffPeat1=-240
              coeffPeat2=70
              coefCH4 = 0.34#g m-2 y-1
              coefN20_1 = 0.23
              coefN20_2 = 0.077#g m-2 y-1
              landClassUnman=2
              compHarvX = 
              initVar=NULL
              initSoilC=NULL
              reInit=F
              sampleX=NULL
              #funX = regionPrebas
              initSoilCreStart=NULL
              outModReStart=NULL
              reStartYear=1
              outType=outType 
              harvScen=harvscen
              uncRCP=uncRCP
              harvInten=harvinten 
              landClassUnman=NULL
              compHarvX = 0
              funPreb = regionPrebas
              initSoilCreStart=NULL
              outModReStart=NULL
              sampleX=NULL              
              procDrPeat = uncPeat
              reStartYear = reStartYearUnc
            }
            outtmp <- runModel(jx, outType=outType, harvScen=harvscen,uncRCP=uncRCP,
                               harvInten=harvinten, procDrPeat = uncPeat,nYears=nYears,
                               compHarvX = compHarvX, landClassUnman=landClassUnman,
                               outModReStart = reStartMod, initSoilCreStart = reStartSoil,
                               funPreb = reStartRegionPrebas,reStartYear = reStartYearUnc, toRaster = toRaster)
            outXcc[[ind]] <- outtmp
            print(outtmp$VroundWood)
            names(outXcc)[ind] <- paste0(harvscen,"_",harvinten,"_",rcpsname)
            ind<-ind+1
          }
          }
        }
        unlink(paste0("uncRuns/regRuns/restartRun_uncRun",r_no,"_",jx,".rdata"))
        print("2015-2021 file is deleted")
        if(uncRCP==3 & harvinten==harvintens[4]){
          unlink(paste0("initSoilCunc/forCent",r_no,"/initSoilC_",
                        outType,"_",jx,".rdata"))
          print(paste0("initsoilID",jx," deleted"))}
        return(outXcc)
      })
    }
  } else if(uncSeg){
    sampleXs <- mclapply(sampleIDs, function(jx) {
      runModel(jx, outType=outType, harvScen="Base" ,harvInten="Base",toRaster=toRaster, procDrPeat = uncPeat)}, 
      mc.cores = nCores,mc.silent=FALSE)      ## Split this job across 10 cores
  } else {
    if(unc100){
      uncRCPs <- uncRCPs[1]
      harvintens <- c("Base","NoHarv")
      print("Run 100 years")
    }
    if(toVemala) print(paste("Run",nYears,"years"))
    sampleXs <- mclapply(sampleIDs[(1+(nii-1)*nParRuns):min(length(sampleIDs),(nii*nParRuns))], 
                         function(jx){ 
                           outXcc <- list()
                           ind <-1
                           # Initialize soil and deadwood
                           rcpsname <- "CurrClim"
                           rcps <- rcpsname
                           print(harvinten)
                           if(toVemala){
                             nYears<-2050-2015
                             toVemala<-F
                             source_url("https://raw.githubusercontent.com/virpi-j/IBCcarbon_runs/master/general/functions.r")
                             source_url("https://raw.githubusercontent.com/virpi-j/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
                             print("initialize for period 2015-2050")
                             outtmp <- runModel(jx, outType=outType, harvScen="Base",uncRCP=0,nYears=nYears,
                                                compHarvX = compHarvX, landClassUnman=landClassUnman,
                                                harvInten="Base", procDrPeat = uncPeat)
                             nYears<-2100-2015
                             toVemala<-T
                             source_url("https://raw.githubusercontent.com/virpi-j/IBCcarbon_runs/master/general/functions.r")
                             source_url("https://raw.githubusercontent.com/virpi-j/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
                           } else {
                           outtmp <- runModel(jx, outType=outType, harvScen="Base",uncRCP=0,nYears=nYears,
                                              compHarvX = compHarvX, landClassUnman=landClassUnman,
                                              harvInten="Base", procDrPeat = uncPeat)
                           }
                           for(uncRCP in uncRCPs){
                             harvinten <- "Base"
                             harvscen<-"Base" 
                             if(uncRCP==0){
                               rcpsname <- "CurrClim"
                               rcps <- rcpsname
                             } else {
                               rcpsname <- RCPnames[uncRCP+1]
                             }
                             #source_url("https://raw.githubusercontent.com/virpi-j/IBCcarbon_runs/master/general/functions.r")
                             outtmp <- runModel(jx, outType=outType, harvScen=harvscen,uncRCP=uncRCP,nYears=nYears,
                                                compHarvX = compHarvX, landClassUnman=landClassUnman,
                                                harvInten=harvinten,  procDrPeat = uncPeat)
                             print(outtmp$VroundWood)
                             outXcc[[ind]] <- outtmp
                             names(outXcc)[ind] <- paste0(harvscen,"_",harvinten,"_",rcpsname)
                             ind<-ind+1
                             print(paste("Load 2015-2021 results for sampleID",jx))
                             load(file=paste0("uncRuns/regRuns/restartRun_uncRun",r_no,"_",jx,".rdata"))
                             for(harvind in 2:length(harvintens)){
                               harvinten<-harvintens[harvind]
                               harvscen <- "Base"
                               if(harvinten=="NoHarv") harvscen<-"NoHarv"
                               outtmp <- runModel(jx, outType=outType, harvScen=harvscen,uncRCP=uncRCP,
                                                  harvInten=harvinten, procDrPeat = uncPeat,nYears=nYears,
                                                  compHarvX = compHarvX, landClassUnman=landClassUnman,
                                                  outModReStart = reStartMod, initSoilCreStart = reStartSoil,
                                                  funPreb = reStartRegionPrebas,reStartYear = reStartYearUnc)
                               print(outtmp$VroundWood)
                               outXcc[[ind]] <- outtmp
                               names(outXcc)[ind] <- paste0(harvscen,"_",harvinten,"_",rcpsname)
                               ind<-ind+1
                             }
                           }
                           unlink(paste0("uncRuns/regRuns/restartRun_uncRun",r_no,"_",jx,".rdata"))
                           print("2015-2021 file is deleted")
                           unlink(paste0("initSoilCunc/forCent",r_no,"/initSoilC_",
                                         outType,"_",jx,".rdata"))
                           print(paste0("initsoilID",jx," deleted"))
                           return(outXcc)
                         },              
                         mc.cores = 4)
    
  }
  timeRun <- Sys.time() - startRun
  print(paste("time for runs",timeRun))
  if(!uncSeg & nSamplesr>1){
    save(area_total,areas_all,sampleXs,
         file=paste0("uncRuns/regRuns/samplexout_reg",r_no,
                     "_tmp.rdata")) 

    for(uncRCPi in 1:length(uncRCPs)){
      uncRCP <- uncRCPs[uncRCPi]
      harvinten <- "Base"
      if(uncRCP==0){
        rcpsname <- "CurrClim"
        #rcps <- rcpsname
      } else {
        rcpsname <- RCPnames[uncRCP+1]
      }
      for(harvind in 1:length(harvintens)){
        harvinten <- harvintens[harvind]  
        harvindRCP <- length(harvintens)*(uncRCPi-1)+harvind
        #print(sampleXs[[1]][[harvindRCP]][1,])
        #print(harvindRCP)
        hscen <- harvscen
        #if(zon10) hscen <- paste0(hscen,"zon10")
        m <- ncol(sampleXs[[1]][[1]]) # how many variables
        n <- length(sampleXs) # how many new similations
        varNams <- names(sampleXs[[1]][[1]])#names(sampleXs[[1]])
        #varNams <-  sampleXs[[1]][,"vari"]
        # g /m2 /year -> -44/12*16^2/10^12
        errorsList <- data.frame()
        sampleOutputb <- list()
        if(nii>1) sampleOutputb <- sampleOutput[[harvindRCP]]
        for(j in 1:m){
          x <- data.frame()
          for(k in 1:n){
            if(length(sampleXs[[k]][[harvindRCP]]) > 1){
              xx <- t(as.matrix(sampleXs[[k]][[harvindRCP]])[,j])
              if(harvind>1 & (grepl("round",varNams[j]) | grepl("energy",varNams[j]))){
                xx[1:reStartYearUnc] <- t(as.matrix(sampleXs[[k]]
                                                    [[length(harvintens)*uncRCPi]])[1:reStartYearUnc,j])
              }
              x <- rbind(x, xx)#,with=FALSE]))
            } else if(k == 1) {
              errorsList <- rbind(errorsList, sampleID = k)
            }
          }
          if(nii == 1){
            sampleOutputb[[j]] <- x
          } else {
            sampleOutputb[[j]] <- rbind(sampleOutputb[[j]], x)
          }
          names(sampleOutputb)[j] <- varNams[j]
        }
        sampleOutput[[harvindRCP]]<-sampleOutputb
        names(sampleOutput)[harvindRCP] <- names(sampleXs[[1]])[harvindRCP]
        if(grepl("Curr", names(sampleXs[[1]]))[harvindRCP]) rcpsname <- "CurrClim"
        if(grepl("RCP26", names(sampleXs[[1]]))[harvindRCP]) rcpsname <- "RCP26"
        if(grepl("RCP45", names(sampleXs[[1]]))[harvindRCP]) rcpsname <- "RCP45"
        if(grepl("RCP85", names(sampleXs[[1]]))[harvindRCP]) rcpsname <- "RCP85"
        if(unc100){
          save(area_total,areas_all,sampleOutputb,file=paste0("uncRuns/regRuns/samplexout100_reg",r_no,
                                                              "_",rcpsname,"_",hscen,"_",harvinten,                                    
                                                              "_samplesize",nSitesRunr,"_iters",nSamplesr,
                                                              "_pr",uncPCrobas,"_Xr",uncInput,"_ager",uncAge,
                                                              "_Cr",uncClim,"_str",uncSiteType,".rdata")) 
        } else {
          if(zon10){ 
            save(area_total,areas_all,sampleOutputb,
                 file=paste0("uncRuns/regRuns/samplexoutzon10_reg",r_no,
                             "_",rcpsname,"_",hscen,"_",harvinten,                                    
                             "_samplesize",nSitesRunr,"_iters",nSamplesr,
                             "_pr",uncPCrobas,"_Xr",uncInput,"_ager",uncAge,
                             "_Cr",uncClim,"_str",uncSiteType,".rdata")) 
          } else {
            save(area_total,areas_all,sampleOutputb,file=paste0("uncRuns/regRuns/samplexout_reg",r_no,
                                                                "_",rcpsname,"_",hscen,"_",harvinten,                                    
                                                                "_samplesize",nSitesRunr,"_iters",nSamplesr,
                                                                "_pr",uncPCrobas,"_Xr",uncInput,"_ager",uncAge,
                                                                "_Cr",uncClim,"_str",uncSiteType,".rdata")) 
          }
        }
        #print("make plots")
        m <- length(sampleOutputb)
        #print(paste(m,"variables"))
        n <- nrow(sampleOutputb[[1]])
        hscen <- harvscen
        pdf(file = paste0("uncRuns/regRuns/plots_reg",r_no,"_",rcpsname,
                          "_",hscen,"_",harvinten,"_samplesize",nSitesRunr,"_iters",nSamplesr,
                          "_pr",uncPCrobas,"_Xr",uncInput,"_ager",uncAge,
                          "_Cr",uncClim,"_str",uncSiteType,
                          "_uncPeat",uncPeat,".pdf"))
        
        for(indj in 1:m){
          x <- sampleOutputb[[indj]]
          varNams <- rownames(x)[1]
          xnas <- which(is.na(x[,1]))
          x <- x[which(!is.na(x[,1])),]
          #png(file = paste0("uncRuns/plots_regionID",r_no,"_",varNams,
          #                  "_",nSitesRunr,"_",harvscen,"_",
          #                  "_pr",uncPCrobas,"_Xr",uncInput,"_ager",uncAge,
          #                  "_Cr",uncClim,"_str",uncSiteType,
          #                  "_uncPeat",uncPeat,".png"))
          time <- 2015 + 1:ncol(x)
          for(iter in 1:n){
            if(iter==1){
              plot(time,x[iter,], ylab = rownames(x)[1], xlab = "year")
            } else{
              points(time,x[iter,])
            }
          }
        }
        dev.off()
        #print("plots made")
      }
    }
    print("Result set and plots saved")
  } else if(nSamplesr>1) { # if uncSeg
    save(sampleXs, ops,file = paste0("uncRuns/segRuns/samplexouttmp_uncSeg_reg",
                                     r_no,"_NoHarv.rdata"))
    n <- length(sampleXs)
    if(length(sampleXs[[1]])>1){ # if results exist, save to sampleOutput
      #if(nii==1){
      varNams <- names(sampleXs[[1]])
      #} else {
      #  varNams <- names(sampleOutput)
      #}
      for(j in 1:length(varNams)){
        x <- data.frame()
        for(k in 1:n){
          if(j==1 & length(sampleXs[[k]])==1){
            sampleIDtmp <- k
            print(paste("k id",k,"no result, run"))
            sampleXss <- lapply(sampleIDtmp, function(jx) { 
              runModel(jx, outType=outType, harvScen="Base",
                       harvInten="Base")})
            sampleXs[[k]]<-sampleXss[[1]]
          }
          x <- rbind(x, sampleXs[[k]][[j]])
          #rownames(x)[k] <- paste0(varNams[j],k)
        }
        if(ncol(x)==4){
          setnames(x, c("segID",paste0(c("per1.","per2.","per3."),rep(nii,each=3))))
        } else {
          setnames(x, c("segID",paste0("iter",nii)))
        }
        if(nii>1){
          aa<-match(x$segID,sampleOutput[[j]]$segID)
        }
        if(nii==1){
          sampleOutput[[j]] <- x
          names(sampleOutput)[j]<-varNams[j]
        } else {
          if(ncol(x)==2){
            xx<-matrix(NA,nrow(sampleOutput[[j]]),1)
            xx[aa,]<-as.matrix(x[,-1])
            sampleOutput[[j]] <- cbind(sampleOutput[[j]], xx)
            niitmp <- ncol(sampleOutput[[j]])
            names(sampleOutput[[j]])[niitmp] <- paste0("iter",nii)
          }
          if(ncol(x)==4){
            xx<-matrix(NA,nrow(sampleOutput[[j]]),3)
            xx[aa,]<-as.matrix(x[,-1])
            sampleOutput[[j]] <- cbind(sampleOutput[[j]], xx)
            niitmp <- ncol(sampleOutput[[j]])
            names(sampleOutput[[j]])[(niitmp-2):niitmp]<-paste0(c("per1.","per2.","per3."),rep(nii,each=3))        
          }   
        }
      }
      hscen <- harvscen
      
      save(sampleOutput,file=paste0("uncRuns/segRuns/samplexout_uncSeg_reg",r_no,
                                    "_iters",nSamplesr,"_NoHarv.rdata")) 
    }# If results exist  
  }
  print(paste0("Run time for ",length(sampleIDs)," samples of size ", nSitesRunr," = ",timeRun))
  print("End running...")
}

setwd("Rsrc/virpiSbatch/")