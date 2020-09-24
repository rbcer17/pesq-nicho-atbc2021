library(biomod2);
library(raster);
library(RColorBrewer);
library(dismo);

setwd("~/AnalysisFolder/Localities/")
#Get data points
points <- read.csv(file = "GadMa.csv", header = T);
points <- cbind(points, rep.int(1, length(nrow(points)))); #Adds another column indicating these are presence points
colnames(points) <- c("Species", "X", "Y", "Response");

#Get environmental variables
setwd("~/AnalysisFolder/Ms/GadMa/");
envtList <- list.files(pattern = ".asc");
envt.st <- stack(envtList);

#Get projection variables
setwd("~/AnalysisFolder/EnvironmentalData/Future/rcp8_5/2100/")
projectionList <- list.files(pattern = ".asc");
proj.st <- stack(projectionList);

#Setting up data file for Biomod2
bmData <- BIOMOD_FormatingData(resp.var = points[,4],
                               resp.xy = points[,2:3], 
                               resp.name = as.character(points[1,1]),
                               expl.var = envt.st,
                               PA.nb.rep=1
);

#Setting up Maxent run
myBiomodOption <- Print_Default_ModelingOptions();
myBiomodOption@MAXENT.Phillips$path_to_maxent.jar = paste(system.file(package="dismo"), "/java", sep='');
myBiomodOption@MAXENT.Phillips$memory_allocated = 2048; #Allocates 2048 MB/2 GB of memory to modeling
myBiomodOption@MAXENT.Phillips$maximumiterations = 10000;
myBiomodOption@MAXENT.Phillips$threshold = F;
myBiomodOption@MAXENT.Phillips$hinge = F;
myBiomodOption@MAXENT.Phillips$visible = F;
myBiomodOption@MAXENT.Phillips$beta_lqp = .95;

#Running Maxent
setwd("~/AnalysisFolder/TestModelRun/R/")
myMaxentModel <- BIOMOD_Modeling(data=bmData,
                                    models=c('MAXENT.Phillips'),
                                    models.options=myBiomodOption,
                                    NbRunEval=10,
                                    do.full.models = F,
                                    DataSplit=50,
                                    models.eval.meth = c('KAPPA','TSS','ROC'),
                                    SaveObj = T
);

#Ensemble of all models--combines model runs using a user-selected evaluation metric
myMaxentEnsemble <- BIOMOD_EnsembleModeling( modeling.output = myMaxentModel,
                                   chosen.models = 'all',
                                   em.by = 'all',
                                   eval.metric = c('TSS'),
                                   eval.metric.quality.threshold = NULL,
                                   models.eval.meth = c('TSS','ROC','KAPPA'),
                                   prob.median = TRUE )

#Projecting your model to the present
myBiomodProjPres <- BIOMOD_Projection(modeling.output = myMaxentModel,
                                    new.env = envt.st,
                                    proj.name = 'Present',
                                    selected.models = 'all',
                                    compress = 'gzip',
                                    clamping.mask = T,
                                    output.format = '.grd',
                                    do.stack=T
);

mod_projPres <- get_predictions(myBiomodProjPres);
presentResult <- calc(mod_projPres,fun = median); #Choose whatever descriptive statistic you'd like
plot(presentResult);
writeRaster(presentResult, filename = "GadusMacrocephalusPresent", format = "ascii", overwrite = T);

#Projecting the ensemble model in the present
myBiomodProjPresEnsemble <- BIOMOD_EnsembleForecasting(myMaxentEnsemble,
                            projection.output = myBiomodProjPres,
                            selected.models = 'all',
                            compress = 'gzip'
);
mod_projPresEnsemble <- get_predictions(myBiomodProjPresEnsemble);
presentEnsembleResult <- mod_projPresEnsemble[[2]] #This is the median model ensemble
plot(presentEnsembleResult);
writeRaster(presentEnsembleResult, filename = "GadusMacrocephalusPresentEnsemble", format = "ascii", overwrite = T);

#Projecting your model to the future
myBiomodProj2100 <- BIOMOD_Projection(modeling.output = myMaxentModel,
                                    new.env = proj.st,
                                    proj.name = 'In2100',
                                    selected.models = 'all',
                                    compress = 'gzip',
                                    clamping.mask = T,
                                    output.format = '.grd',
                                    do.stack=T
);

mod_proj2100 <- get_predictions(myBiomodProj2100);
result2100 <- calc(mod_proj2100,fun = median); #Choose whatever descriptive statistic you'd like
plot(result2100);
writeRaster(result2100, filename = "GadusMacrocephalus2100", format = "ascii", overwrite = T);

#Projecting the ensemble model in 2100
myBiomodProj2100Ensemble <- BIOMOD_EnsembleForecasting(myMaxentEnsemble,
                                                       projection.output = myBiomodProj2100,
                                                       selected.models = 'all',
                                                       compress = 'gzip'
);
mod_proj2100Ensemble <- get_predictions(myBiomodProj2100Ensemble);
ensembleResult2100 <- mod_proj2100Ensemble[[2]] #This is the median model ensemble
plot(ensembleResult2100);
writeRaster(ensembleResult2100, filename = "GadusMacrocephalus2100Ensemble", format = "ascii", overwrite = T);


#Evaluating models
## Variable response curves
response.plot2(models = BIOMOD_LoadModels(myMaxentModel, models='MAXENT.Phillips'),
               Data = get_formal_data(myMaxentModel,'expl.var'),
               show.variables= get_formal_data(myMaxentModel,'expl.var.names'),
               do.bivariate = FALSE,
               fixed.var.metric = 'median',
               col = brewer.pal(10, "Spectral"),
               legend = TRUE,
               data_species = get_formal_data(myMaxentModel,'resp.var')
);

##Varible contributions; only for Maxent, not possible for other models
forSetup <- read.csv(file = paste("/Users/imac/Documents/rstudio-projects/hannah/AnalysisFolder/TestModelRun/R/GadusMacrocephalus/models/1457123817/GadusMacrocephalus_PA1_RUN1_MAXENT.Phillips_outputs/maxentResults.csv", sep = ""), header = T)#Choose the appropriate model folder with the seed of the analysis you want
variableContributions <- matrix(data = NA, nrow = length(forSetup[, grep('.contribution', names(forSetup))]), ncol = 10);
rownames(variableContributions) <- names(forSetup[, grep('.contribution', names(forSetup))])
colnames(variableContributions) <- c("Run1", "Run2", "Run3", "Run4", "Run5", "Run6", "Run7", "Run8", "Run9", "Run10")
variablePermutationImportance <- matrix(data = NA, nrow = length(forSetup[, grep('.permutation.importance', names(forSetup))]), ncol = 10);
colnames(variablePermutationImportance) <- c("Run1", "Run2", "Run3", "Run4", "Run5", "Run6", "Run7", "Run8", "Run9", "Run10")
count <- 1;
while (count <= 10){
  temporary <- read.csv(file = paste("/Users/imac/Documents/rstudio-projects/hannah/AnalysisFolder/TestModelRun/R/GadusMacrocephalus/models/1457123817/GadusMacrocephalus_PA1_RUN", count, "_MAXENT.Phillips_outputs/maxentResults.csv", sep = ""), header = T);
  variableContributions[,count] <- unlist(unname(temporary[, grep('.contribution', names(temporary))]))
  variablePermutationImportance[,count] <- unlist(unname(temporary[, grep('.permutation.importance', names(temporary))]))
  count <- count + 1;
}
write.csv(variableContributions, "VariableContributions.csv", quote = F);
write.csv(variablePermutationImportance, "VariablePermutationImportance.csv", quote = F);

##Calculate MESS for 2100
mess2100 <- mess(proj.st, rasterToPoints(envt.st)[,-1:-2],-9999);
writeRaster(mess2100, filename = "GadusMacrocephalus2100MESS", format = "ascii", overwrite = T);
summary(mess2100)
View(mess2100)
##Create dataset for evaluation
###"Cutoff" gives threshold to optimize evaluation metric, "Sensitivity" and "Specificity" are based on this threshold
myMaxentModelEval <- get_evaluations(myMaxentModel, as.data.frame = F); 
write.csv(myMaxentModelEval["TSS",,,,],"TSSEvaluation.csv", quote = F);#Results for True Skill Score
write.csv(myMaxentModelEval["ROC",,,,],"TSSEvaluation.csv", quote = F);#Results for Area Under the Curve
write.csv(myMaxentModelEval["Kappa",,,,],"TSSEvaluation.csv", quote = F);#Results for Cohen's Kappa
