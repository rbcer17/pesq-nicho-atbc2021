library(biomod2);
library(raster);
library(RColorBrewer);
library(dismo);

setwd("C:/projetosR/Modelagem/analise/Crowned/Localities/")
#Get data points
peiticaver <- read.csv(file = "gaverao_corrigido.csv", header = T);
peiticaver <- cbind(peiticaver, rep.int(1, length(nrow(peiticaver)))); #Adds another column indicating these are presence points
colnames(peiticaver) <- c("X", "Y", "Response");

#Get environmental variables
setwd("C:/projetosR/Modelagem/analise/Crowned/Ms/Crowned/");
envtList <- list.files(pattern = ".asc");
envt.st <- stack(envtList);
crs(envt.st) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#Setting up data file for Biomod2
bmData <- BIOMOD_FormatingData(resp.var = peiticaver[,3],
                               resp.xy = peiticaver[,1:2], 
                               resp.name = "GriseoAura",
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
setwd("C:/projetosR/Modelagem/analise/Crowned/TestModelRun/R/")
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
plot(presentResult, main = "Griseotyrannus Aurantioatrocristatus - Verão (Out.- Fev.)");
writeRaster(presentResult, filename = "griseoaurPresent", format = "GTiff", overwrite = T);
#Projecting the ensemble model in the present
myBiomodProjPresEnsemble <- BIOMOD_EnsembleForecasting(myMaxentEnsemble,
                                                       projection.output = myBiomodProjPres,
                                                       selected.models = 'all',
                                                       compress = 'gzip'
);
mod_projPresEnsemble <- get_predictions(myBiomodProjPresEnsemble);
presentEnsembleResult <- mod_projPresEnsemble[[2]] #This is the median model ensemble
plot(presentEnsembleResult, main = "Griseotyrannus Aurantioatrocristatus - Verão (Out.- Fev.)");
writeRaster(presentEnsembleResult, filename = "griseoaurPresent", format = "GTiff", overwrite = T)
