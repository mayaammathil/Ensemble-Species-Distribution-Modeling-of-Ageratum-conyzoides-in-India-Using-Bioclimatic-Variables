# Install if you haven't
install.packages("sdm")
install.packages(c("mda", "gam", "earth", "maxnet", "xgboost"))

library(raster)
library(sp)
library(sf)
library(terra)
library(sdm)
library(dismo)
library(dplyr)
library(tidyr)
install.packages("usdm")
library(usdm)
install.packages("sp")  # if not already installed
library(sp)
memory.limit()
df = read.csv("D:/A.conyzoidesocindia/ACXY.csv")
df
class(df)
head(df)

coordinates(df) <- ~ X + Y
class(df)

head(df)
folder_path <- "D:/india after vif selected past bio MIROC62.5"
# List all TIFF files in the folder
tif_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
# Create a raster stack from the TIFF files
env_stack <- stack(tif_files)
print(env_stack)  # View summary of the raster stack
plot(env_stack)

remove.packages("biomod2")
install.packages("biomod2")

library(biomod2)
ls("package:biomod2")


exists("BIOMOD_ModelingOptions", where = "package:biomod2")
myBiomodData <- BIOMOD_FormatingData(
  resp.var = df$Ageratum.conyzoides.L.,  # Response variable from your data frame
  resp.xy = coordinates(df),  # Coordinates
  resp.name = "Ageratum.conyzoides.L.",
  expl.var = env_stack,  # Environmental predictors
  PA.nb.rep = 1,  # Number of pseudo-absences to generate
  PA.strategy = 'random'  # Pseudo-absence strategy
)
save.image(file = "D:/A.conyzoidesocindia/R/ACNEWbiomod2/my_workspace.RData")
save(myBiomodData, file = "D:/A.conyzoidesocindia/R/ACNEWbiomod2/my_model_data.RData")
write.csv(df, "D:/A.conyzoidesocindia/R/ACNEWbiomod2/my_dataframe.csv", row.names = FALSE)
exists("BIOMOD_ModelingOptions", where = "package:biomod2")
# Define modeling options
bm.options <- BIOMOD_ModelingOptions(
  GLM = list(typical = "default"),
  RF = list(ntree = 500, mtry = 2),
  BRT = list(n.trees = 1000, interaction.depth = 5, shrinkage = 0.01)
)
exists("BIOMOD_Modeling", where = "package:biomod2")
arg()
print(ModelsTable)
args(BIOMOD_Modeling)


myBiomodModel <- BIOMOD_Modeling(
  bm.format = myBiomodData,           # The formatted data for modeling (previously prepared)
  modeling.id = "my_model",            # A unique identifier for this model run
  models = c("RF", "ANN", "GLM"),  # Specify the models to be used: Random Forest, Maxent, and Generalized Linear Model
  CV.strategy = "random",              # Use random strategy for cross-validation
  CV.nb.rep = 10,                      # Number of repetitions for cross-validation
  CV.perc = 0.3,                       # Percentage of data for cross-validation (30% as a decimal)
  metric.eval = c("KAPPA", "TSS", "ROC")  # Metrics to evaluate model performance: Kappa, True Skill Statistic, and ROC
)

evaluation_metrics <- get_evaluations(myBiomodModel)

# Plot response curves for the models
plot(myBiomodModel, "response")
summary(myBiomodModel)
# Convert evaluation metrics to a data frame
evaluation_df <- as.data.frame(evaluation_metrics)


# Save the data frame as a CSV file
write.csv(evaluation_df, "D:/A.conyzoidesocindia/R/ACNEWbiomod2/evaluation_metrics.csv", row.names = FALSE)
# Plotting TSS for each model
library(ggplot2)

# Inspect the evaluation metrics data frame
head(evaluation_metrics)


library(ggplot2)
library(dplyr)

# Filter the evaluation metrics for TSS
tss_metrics <- evaluation_metrics %>%
  filter(metric.eval == "TSS")

# Create the bar plot
ggplot(tss_metrics, aes(x = full.name, y = run, fill = algo)) +  # Adjust y-axis based on what you want to plot
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Model TSS Comparison", x = "Models", y = "TSS") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save.image(file = "D:/A.conyzoidesocindia/R/ACNEWbiomod2/2ndsavemy_modelrun10.RData")

save(myBiomodData, file = "D:/A.conyzoidesocindia/R/ACNEWbiomod2/2ndsavemy_modelrun10.RData")
save(myBiomodModel, evaluation_df, file = "D:/A.conyzoidesocindia/R/ACNEWbiomod2/2ndsaveobjects.RData")

library(biomod2)
myBiomodModel00 <- BIOMOD_Modeling(
  bm.format = myBiomodData,           # The formatted data for modeling (previously prepared)
  modeling.id = "my_model",            # A unique identifier for this model run
  models = c("RF", "ANN", "GLM"),  # Specify the models to be used: Random Forest, Maxent, and Generalized Linear Model
  CV.strategy = "random",              # Use random strategy for cross-validation
  CV.nb.rep = 100,                      # Number of repetitions for cross-validation
  CV.perc = 0.3,                       # Percentage of data for cross-validation (30% as a decimal)
  metric.eval = c("KAPPA", "TSS", "ROC"),
  var.import=3,
  seed.val = 42)# Metrics to evaluate model performance: Kappa, True Skill Statistic, and ROC

myBiomodModel00

# Model ensemble models
myBiomodEM1 <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModel00,
                                      models.chosen = "all",
                                      em.by = 'all',
                                      em.algo = c('EMmean', 'EMca'),
                                      metric.select = c('TSS'),
                                      metric.select.thresh = c(0.7),
                                      metric.eval = c('TSS', 'ROC', 'KAPPA'),
                                      var.import = 3,
                                      seed.val = 42)


myBiomodEM1
# Get evaluation scores & variables importance
evaluation_scores <- get_evaluations(myBiomodEM1)
variable_importance <- get_variables_importance(myBiomodEM1)

# Convert evaluation scores to data frame
evaluation_scores_df <- as.data.frame(evaluation_scores)

# Convert variable importance to data frame
variable_importance_df <- as.data.frame(variable_importance)


# Define the folder path (update with your actual path)
folder_path <- "D:/A.conyzoidesocindia/R/ACNNEW3"

# Save evaluation scores as CSV
write.csv(evaluation_scores_df, paste0(folder_path, "evaluation_scores1.csv"), row.names = FALSE)

# Save variable importance as CSV
write.csv(variable_importance_df, paste0(folder_path, "variable_importance1.csv"), row.names = FALSE)

# Represent evaluation scores
bm_PlotEvalMean(bm.out = myBiomodEM, dataset = 'calibration')
bm_PlotEvalBoxplot(bm.out = myBiomodEM, group.by = c('algo', 'algo'))

# # Represent variables importance
p <-bm_PlotVarImpBoxplot(bm.out = myBiomodEM1, group.by = c('expl.var', 'algo', 'algo'))
# Assuming you can extract the variable importance data from myBiomodEM1
library(ggplot2)

library(ggplot2)
library(ggplot2)

# Create the boxplot with customizations
library(ggplot2)

# Create the boxplot with customizations
library(ggplot2)

# Create the boxplot with customizations
library(ggplot2)
library(RColorBrewer)

library(ggplot2)
library(RColorBrewer)

# Create the boxplot with customizations
ggplot(variable_importance, aes(x = expl.var, y = var.imp, fill = expl.var)) + 
  geom_boxplot(color = "black") +  # Set border color
  scale_x_discrete(labels = c("indiabio13" = "bio_13",
                              "indiabio14" = "bio_14",
                              "indiabio15" = "bio_15",
                              "indiabio18" = "bio_18",
                              "indiabio19" = "bio_19",
                              "indiabio2" = "bio_2",
                              "indiabio3" = "bio_3",
                              "indiabio8" = "bio_8",
                              "indiabio9" = "bio_9")) +
  labs(x = "Bioclimatic Variables", y = "Variable Importance", fill = "Bioclimatic Variables") +  # Change axis labels and legend title
  ggtitle("Variable Importance by Bioclimatic Variables") +  # Update title
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5, margin = margin(b = 10)),  # Center title, size 10, and margin
    axis.title.x = element_text(size = 8, margin = margin(t = 10)),  # Increase space above x-axis title
    axis.title.y = element_text(size = 8, margin = margin(r = 10)),  # Increase space to the right of y-axis title
    axis.text.x = element_text(size = 6),  # X-axis tick label size
    axis.text.y = element_text(size = 6),  # Y-axis tick label size
    legend.title = element_text(size = 8),  # Legend title size
    legend.text = element_text(size = 6),   # Legend text size
    plot.margin = margin(10, 10, 10, 10)   # Increase overall plot margins
  ) +
  scale_fill_brewer(palette = "Set3", labels = c("bio_13", "bio_14", "bio_15", "bio_18", "bio_19", "bio_2", "bio_3", "bio_8", "bio_9"))  # Match legend labels with x ticks



bm_PlotVarImpBoxplot(bm.out = myBiomodEM1, group.by = c('expl.var', 'algo', 'merged.by.PA'))
bm_PlotVarImpBoxplot(bm.out = myBiomodEM1, group.by = c('algo', 'expl.var', 'merged.by.PA'))


# # Represent response curves
 bm_PlotResponseCurves(bm.out = myBiomodEM, 
                       models.chosen = get_built_models(myBiomodEM),
                       fixed.var = 'median')
 
 bm_PlotResponseCurves(bm.out = myBiomodEM, 
                       models.chosen = get_built_models(myBiomodEM),
                       fixed.var = 'min')

 
 # Projecting the current data using the existing model
 current_projection <- BIOMOD_Projection(
   bm.mod = myBiomodModel00,
   new.env = env_stack,  # Use the same environmental variables
   proj.name = "Current_Climate",
   selected.models = "all"
 )

 # Project ensemble models (from single projections)
 myBiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM1, 
                                              bm.proj = current_projection,
                                              models.chosen = 'all',
                                              metric.binary = 'all',
                                              metric.filter = 'all')
 library(tidyterra)
 myBiomodEMProj
 plot(myBiomodEMProj)
 
 library(raster)
 
 # Extract the ensemble projection from your result (assuming it's a raster)
 ensemble_rasterCURRENT <- get_predictions(myBiomodEMProj)
 library(ggplot2)
 library(rasterVis)  # Helps plot raster objects with ggplot2
 library(terra)
 
 # Assuming ensemble_rasterCURRENT is a SpatRaster object
 raster_df <- as.data.frame(ensemble_rasterCURRENT, xy = TRUE)
 
 str(raster_df)  # Check what the column name is for the raster values
 
 ggplot(raster_df) +
   geom_tile(aes(x = x, y = y, fill = Ageratum.conyzoides.L._EMmeanByTSS_mergedData_mergedRun_mergedAlgo)) + 
   scale_fill_viridis_c(guide = guide_colorbar(title = NULL)) +  # Remove legend heading
   ggtitle("Current Distribution of Ageratum conyzoides") +  # Update title
   theme_minimal() +
   theme(plot.title = element_text(size = 10, hjust = 0.5),  # Center title and set size
         axis.title.x = element_text(size = 8),
         axis.title.y = element_text(size = 8),
         axis.text.x = element_text(size = 6),
         axis.text.y = element_text(size = 6),
         legend.title = element_blank()) +  # Ensure no title for the legend
   labs(x = "Longitude", y = "Latitude")  # Update axis labels
 
 
 # Set the path to the folder containing future climate data
 fut_clim_1 <- "D:/INDIA MIROC6 2.5 FUT/ssp126F21-40"
 
 # Load all raster files from the folder
 fut_clim_1 <- list.files(fut_clim_1, pattern = ".tif$", full.names = TRUE)
 
 # Create a raster stack from the future climate files (using underscores instead of hyphens in object names)
 env_126f21_40 <- stack(fut_clim_1)
 
 # Check the stack
 print(env_126f21_40)
 
 
 
 # Step 2: Project the future climate data using the existing model
 future_126_21_40 <- BIOMOD_Projection(
   bm.mod = myBiomodModel00,              # Your existing model
   new.env = env_126f21_40,                   # Future environmental raster stack
   proj.name = "Future_bio_126(21-40)",           # Name for the projection
   selected.models = "all"                 # Choose all models
 )
 
 # Step 3: Ensemble forecasting based on the future projections
 myBiomodEMProj_Future <- BIOMOD_EnsembleForecasting(
   bm.em = myBiomodEM1,                   # Your ensemble model
   bm.proj = future_126_21_40,            # Use the future projection
   models.chosen = 'all',                  # Include all models in the ensemble
   metric.binary = 'all',                  # Use all binary metrics
   metric.filter = 'all'                   # Use all filter metrics
 )
 
 # Check the results of the future ensemble projections
 print(myBiomodEMProj_Future)
 
 # Optional: Plot the results to visualize the future ensemble predictions
 plot(myBiomodEMProj_Future)  # Default plot
 myBiomodEMProj_Future
# Extract the ensemble projection from your result (assuming it's a raster)
ensemfu126_21_40 <- get_predictions(myBiomodEMProj_Future)
 library(ggplot2)
 library(rasterVis)  # Helps plot raster objects with ggplot2
 library(terra)
 
 # Assuming ensemble_rasterCURRENT is a SpatRaster object
 raster_df1 <- as.data.frame(ensemfu126_21_40, xy = TRUE)
 
 str(raster_df1)  # Check what the column name is for the raster values
 
 ggplot(raster_df1) +
   geom_tile(aes(x = x, y = y, fill = Ageratum.conyzoides.L._EMmeanByTSS_mergedData_mergedRun_mergedAlgo)) + 
   scale_fill_viridis_c(guide = guide_colorbar(title = NULL)) +  # Remove legend heading
   ggtitle("Distribution of Ageratum conyzoides (SSP126(2021-2040)") +  # Update title
   theme_minimal() +
   theme(plot.title = element_text(size = 10, hjust = 0.5),  # Center title and set size
         axis.title.x = element_text(size = 8),
         axis.title.y = element_text(size = 8),
         axis.text.x = element_text(size = 6),
         axis.text.y = element_text(size = 6),
         legend.title = element_blank()) +  # Ensure no title for the legend
   labs(x = "Longitude", y = "Latitude")  # Update axis labels
 
 
 ####################################
 # Set the path to the folder containing future climate data
 fut_clim_2 <- "D:/INDIA MIROC6 2.5 FUT/SSP126F41-60"
 
 # Load all raster files from the folder
 fut_clim_2 <- list.files(fut_clim_2, pattern = ".tif$", full.names = TRUE)
 
 # Create a raster stack from the future climate files (using underscores instead of hyphens in object names)
 env_126f41_60 <- stack(fut_clim_2)
 
 # Check the stack
 print(env_126f41_60)
 
 
 
 # Step 2: Project the future climate data using the existing model
 future_126_41_60 <- BIOMOD_Projection(
   bm.mod = myBiomodModel00,              # Your existing model
   new.env = env_126f41_60,                   # Future environmental raster stack
   proj.name = "Future_bio_126(41-60)",           # Name for the projection
   selected.models = "all"                 # Choose all models
 )
 
 # Step 3: Ensemble forecasting based on the future projections
 myBiomodEMProj_Future1 <- BIOMOD_EnsembleForecasting(
   bm.em = myBiomodEM1,                   # Your ensemble model
   bm.proj = future_126_41_60,            # Use the future projection
   models.chosen = 'all',                  # Include all models in the ensemble
   metric.binary = 'all',                  # Use all binary metrics
   metric.filter = 'all'                   # Use all filter metrics
 )
 
 # Check the results of the future ensemble projections
 print(myBiomodEMProj_Future1)
 
 # Optional: Plot the results to visualize the future ensemble predictions
 plot(myBiomodEMProj_Future1)  # Default plot
 myBiomodEMProj_Future1
 # Extract the ensemble projection from your result (assuming it's a raster)
 ensemfu126_41_60 <- get_predictions(myBiomodEMProj_Future1)
 library(ggplot2)
 library(rasterVis)  # Helps plot raster objects with ggplot2
 library(terra)
 
 # Assuming ensemble_rasterCURRENT is a SpatRaster object
 raster_df2 <- as.data.frame(ensemfu126_41_60, xy = TRUE)
 
 str(raster_df2)  # Check what the column name is for the raster values
 
 ggplot(raster_df2) +
   geom_tile(aes(x = x, y = y, fill = Ageratum.conyzoides.L._EMmeanByTSS_mergedData_mergedRun_mergedAlgo)) + 
   scale_fill_viridis_c(guide = guide_colorbar(title = NULL)) +  # Remove legend heading
   ggtitle("Distribution of Ageratum conyzoides (SSP126(2041-2060)") +  # Update title
   theme_minimal() +
   theme(plot.title = element_text(size = 10, hjust = 0.5),  # Center title and set size
         axis.title.x = element_text(size = 8),
         axis.title.y = element_text(size = 8),
         axis.text.x = element_text(size = 6),
         axis.text.y = element_text(size = 6),
         legend.title = element_blank()) +  # Ensure no title for the legend
   labs(x = "Longitude", y = "Latitude")  # Update axis labels
 
 
 #######################################################
 # Set the path to the folder containing future climate data
 fut_clim_3 <- "D:/INDIA MIROC6 2.5 FUT/ssp126F61-80"
 
 # Load all raster files from the folder
 fut_clim_3 <- list.files(fut_clim_3, pattern = ".tif$", full.names = TRUE)
 
 # Create a raster stack from the future climate files (using underscores instead of hyphens in object names)
 env_126f61_80 <- stack(fut_clim_3)
 
 # Check the stack
 print(env_126f61_80)
 
 
 
 # Step 2: Project the future climate data using the existing model
 future_126_61_80 <- BIOMOD_Projection(
   bm.mod = myBiomodModel00,              # Your existing model
   new.env = env_126f61_80,                   # Future environmental raster stack
   proj.name = "Future_bio_126(61-80)",           # Name for the projection
   selected.models = "all"                 # Choose all models
 )
 
 # Step 3: Ensemble forecasting based on the future projections
 myBiomodEMProj_Future2 <- BIOMOD_EnsembleForecasting(
   bm.em = myBiomodEM1,                   # Your ensemble model
   bm.proj = future_126_61_80,            # Use the future projection
   models.chosen = 'all',                  # Include all models in the ensemble
   metric.binary = 'all',                  # Use all binary metrics
   metric.filter = 'all'                   # Use all filter metrics
 )
 
 # Check the results of the future ensemble projections
 print(myBiomodEMProj_Future2)
 
 # Optional: Plot the results to visualize the future ensemble predictions
 plot(myBiomodEMProj_Future2)  # Default plot
 myBiomodEMProj_Future2
 # Extract the ensemble projection from your result (assuming it's a raster)
 ensemfu126_61_80 <- get_predictions(myBiomodEMProj_Future2)
 library(ggplot2)
 library(rasterVis)  # Helps plot raster objects with ggplot2
 library(terra)
 
 # Assuming ensemble_rasterCURRENT is a SpatRaster object
 raster_df3 <- as.data.frame(ensemfu126_61_80, xy = TRUE)
 
 str(raster_df3)  # Check what the column name is for the raster values
 
 ggplot(raster_df3) +
   geom_tile(aes(x = x, y = y, fill = Ageratum.conyzoides.L._EMmeanByTSS_mergedData_mergedRun_mergedAlgo)) + 
   scale_fill_viridis_c(guide = guide_colorbar(title = NULL)) +  # Remove legend heading
   ggtitle("Distribution of Ageratum conyzoides (SSP126(2061-2080)") +  # Update title
   theme_minimal() +
   theme(plot.title = element_text(size = 10, hjust = 0.5),  # Center title and set size
         axis.title.x = element_text(size = 8),
         axis.title.y = element_text(size = 8),
         axis.text.x = element_text(size = 6),
         axis.text.y = element_text(size = 6),
         legend.title = element_blank()) +  # Ensure no title for the legend
   labs(x = "Longitude", y = "Latitude")  # Update axis labels
 
 ###########################################################
 #######################################################
 # Set the path to the folder containing future climate data
 fut_clim_4 <- "D:/INDIA MIROC6 2.5 FUT/SSP126F81-100"
 
 # Load all raster files from the folder
 fut_clim_4 <- list.files(fut_clim_4, pattern = ".tif$", full.names = TRUE)
 
 # Create a raster stack from the future climate files (using underscores instead of hyphens in object names)
 env_126f81_100 <- stack(fut_clim_4)
 
 # Check the stack
 print(env_126f81_100)
 
 
 
 # Step 2: Project the future climate data using the existing model
 future_126_81_100 <- BIOMOD_Projection(
   bm.mod = myBiomodModel00,              # Your existing model
   new.env = env_126f81_100,                   # Future environmental raster stack
   proj.name = "Future_bio_126(81-100)",           # Name for the projection
   selected.models = "all"                 # Choose all models
 )
 
 # Step 3: Ensemble forecasting based on the future projections
 myBiomodEMProj_Future3 <- BIOMOD_EnsembleForecasting(
   bm.em = myBiomodEM1,                   # Your ensemble model
   bm.proj = future_126_81_100,            # Use the future projection
   models.chosen = 'all',                  # Include all models in the ensemble
   metric.binary = 'all',                  # Use all binary metrics
   metric.filter = 'all'                   # Use all filter metrics
 )
 
 # Check the results of the future ensemble projections
 print(myBiomodEMProj_Future3)
 
 # Optional: Plot the results to visualize the future ensemble predictions
 plot(myBiomodEMProj_Future3)  # Default plot
 myBiomodEMProj_Future3
 # Extract the ensemble projection from your result (assuming it's a raster)
 ensemfu126_81_100 <- get_predictions(myBiomodEMProj_Future3)
 library(ggplot2)
 library(rasterVis)  # Helps plot raster objects with ggplot2
 library(terra)
 
 # Assuming ensemble_rasterCURRENT is a SpatRaster object
 raster_df4 <- as.data.frame(ensemfu126_81_100, xy = TRUE)
 
 str(raster_df4)  # Check what the column name is for the raster values
 
 ggplot(raster_df4) +
   geom_tile(aes(x = x, y = y, fill = Ageratum.conyzoides.L._EMmeanByTSS_mergedData_mergedRun_mergedAlgo)) + 
   scale_fill_viridis_c(guide = guide_colorbar(title = NULL)) +  # Remove legend heading
   ggtitle("Distribution of Ageratum conyzoides (SSP126(2081-2100)") +  # Update title
   theme_minimal() +
   theme(plot.title = element_text(size = 10, hjust = 0.5),  # Center title and set size
         axis.title.x = element_text(size = 8),
         axis.title.y = element_text(size = 8),
         axis.text.x = element_text(size = 6),
         axis.text.y = element_text(size = 6),
         legend.title = element_blank()) +  # Ensure no title for the legend
   labs(x = "Longitude", y = "Latitude")  # Update axis labels
 
 
 #########################################
 
 
 
# SSP585(a1-d1)
 # Set the path to the folder containing future climate data
 fut_clim_a1 <- "D:/INDIA MIROC6 2.5 FUT/ssp585F21-40"
 
 # Load all raster files from the folder
 fut_clim_a1 <- list.files(fut_clim_a1, pattern = ".tif$", full.names = TRUE)
 
 # Create a raster stack from the future climate files (using underscores instead of hyphens in object names)
 env_585f21_40 <- stack(fut_clim_a1)
 
 # Check the stack
 print(env_585f21_40)
 
 
 
 # Step 2: Project the future climate data using the existing model
 future_585_21_40 <- BIOMOD_Projection(
   bm.mod = myBiomodModel00,              # Your existing model
   new.env = env_585f21_40,                   # Future environmental raster stack
   proj.name = "Future_bio_585(21-40)",           # Name for the projection
   selected.models = "all"                 # Choose all models
 )
 
 # Step 3: Ensemble forecasting based on the future projections
 myBiomodEMProj_Futurea1 <- BIOMOD_EnsembleForecasting(
   bm.em = myBiomodEM1,                   # Your ensemble model
   bm.proj = future_585_21_40 ,            # Use the future projection
   models.chosen = 'all',                  # Include all models in the ensemble
   metric.binary = 'all',                  # Use all binary metrics
   metric.filter = 'all'                   # Use all filter metrics
 )
 
 # Check the results of the future ensemble projections
 print(myBiomodEMProj_Futurea1)
 
 # Optional: Plot the results to visualize the future ensemble predictions
 plot(myBiomodEMProj_Futurea1)  # Default plot
 myBiomodEMProj_Futurea1
 # Extract the ensemble projection from your result (assuming it's a raster)
 ensemfu585_21_40 <- get_predictions(myBiomodEMProj_Futurea1)
 library(ggplot2)
 library(rasterVis)  # Helps plot raster objects with ggplot2
 library(terra)
 
 
 # Assuming ensemble_rasterCURRENT is a SpatRaster object
 raster_dfa1 <- as.data.frame(ensemfu585_21_40, xy = TRUE)
 
 str(raster_dfa1)# Check what the column name is for the raster values
 
 ggplot(raster_dfa1) +
   geom_tile(aes(x = x, y = y, fill = Ageratum.conyzoides.L._EMmeanByTSS_mergedData_mergedRun_mergedAlgo)) + 
   scale_fill_viridis_c(guide = guide_colorbar(title = NULL)) +  # Remove legend heading
   ggtitle("Distribution of Ageratum conyzoides (SSP585(2021-2040)") +  # Update title
   theme_minimal() +
   theme(plot.title = element_text(size = 10, hjust = 0.5),  # Center title and set size
         axis.title.x = element_text(size = 8),
         axis.title.y = element_text(size = 8),
         axis.text.x = element_text(size = 6),
         axis.text.y = element_text(size = 6),
         legend.title = element_blank()) +  # Ensure no title for the legend
   labs(x = "Longitude", y = "Latitude")  # Update axis labels
 
 ###############################################################
 # SSP585(a1-d1)
 # Set the path to the folder containing future climate data
 fut_clim_b1 <-  "D:/INDIA MIROC6 2.5 FUT/ssp585F41-60"
 
 # Load all raster files from the folder
 fut_clim_b1 <- list.files(fut_clim_b1, pattern = ".tif$", full.names = TRUE)
 
 # Create a raster stack from the future climate files (using underscores instead of hyphens in object names)
 env_585f41_60 <- stack(fut_clim_b1)
 
 # Check the stack
 print(env_585f41_60)
 
 
 
 # Step 2: Project the future climate data using the existing model
 future_585_41_60 <- BIOMOD_Projection(
   bm.mod = myBiomodModel00,              # Your existing model
   new.env = env_585f41_60,                   # Future environmental raster stack
   proj.name = "Future_bio_585(41_60)N", # here the above result may have over written correct
   selected.models = "all",               # Choose all models
 )
 
 # Step 3: Ensemble forecasting based on the future projections
 myBiomodEMProj_Futureb1 <- BIOMOD_EnsembleForecasting(
   bm.em = myBiomodEM1,                   # Your ensemble model
   bm.proj = future_585_41_60 ,            # Use the future projection
   models.chosen = 'all',                  # Include all models in the ensemble
   metric.binary = 'all',                  # Use all binary metrics
   metric.filter = 'all'                   # Use all filter metrics
 )
 
 # Check the results of the future ensemble projections
 print(myBiomodEMProj_Futureb1)
 
 # Optional: Plot the results to visualize the future ensemble predictions
 plot(myBiomodEMProj_Futureb1)  # Default plot
 myBiomodEMProj_Futureb1
 # Extract the ensemble projection from your result (assuming it's a raster)
 ensemfu585_41_60 <- get_predictions(myBiomodEMProj_Futureb1)
 library(ggplot2)
 library(rasterVis)  # Helps plot raster objects with ggplot2
 library(terra)
 
 
 # Assuming ensemble_rasterCURRENT is a SpatRaster object
 raster_dfb1 <- as.data.frame(ensemfu585_41_60, xy = TRUE)
 
 str(raster_dfb1)# Check what the column name is for the raster values
 
 ggplot(raster_dfb1) +
   geom_tile(aes(x = x, y = y, fill = Ageratum.conyzoides.L._EMmeanByTSS_mergedData_mergedRun_mergedAlgo)) + 
   scale_fill_viridis_c(guide = guide_colorbar(title = NULL)) +  # Remove legend heading
   ggtitle("Distribution of Ageratum conyzoides (SSP585(2041-2060)") +  # Update title
   theme_minimal() +
   theme(plot.title = element_text(size = 10, hjust = 0.5),  # Center title and set size
         axis.title.x = element_text(size = 8),
         axis.title.y = element_text(size = 8),
         axis.text.x = element_text(size = 6),
         axis.text.y = element_text(size = 6),
         legend.title = element_blank()) +  # Ensure no title for the legend
   labs(x = "Longitude", y = "Latitude")  # Update axis labels
 
 ###############################################################
 # SSP585(a1-d1)
 # Set the path to the folder containing future climate data
 fut_clim_c1 <-  "D:/INDIA MIROC6 2.5 FUT/ssp585F61-80"
 
 # Load all raster files from the folder
 fut_clim_c1 <- list.files(fut_clim_c1, pattern = ".tif$", full.names = TRUE)
 
 # Create a raster stack from the future climate files (using underscores instead of hyphens in object names)
 env_585f61_80 <- stack(fut_clim_c1)
 
 # Check the stack
 print(env_585f61_80)
 
 
 
 # Step 2: Project the future climate data using the existing model
 future_585_61_80 <- BIOMOD_Projection(
   bm.mod = myBiomodModel00,              # Your existing model
   new.env = env_585f61_80,                   # Future environmental raster stack
   proj.name = "Future_bio_585(61-80)",           # Name for the projection
   selected.models = "all"                 # Choose all models
 )
 
 # Step 3: Ensemble forecasting based on the future projections
 myBiomodEMProj_Futurec1 <- BIOMOD_EnsembleForecasting(
   bm.em = myBiomodEM1,                   # Your ensemble model
   bm.proj = future_585_61_80 ,            # Use the future projection
   models.chosen = 'all',                  # Include all models in the ensemble
   metric.binary = 'all',                  # Use all binary metrics
   metric.filter = 'all'                   # Use all filter metrics
 )
 
 # Check the results of the future ensemble projections
 print(myBiomodEMProj_Futurec1)
 
 # Optional: Plot the results to visualize the future ensemble predictions
 plot(myBiomodEMProj_Futurec1)  # Default plot
 myBiomodEMProj_Futurec1
 # Extract the ensemble projection from your result (assuming it's a raster)
 ensemfu585_61_80 <- get_predictions(myBiomodEMProj_Futurec1)
 library(ggplot2)
 library(rasterVis)  # Helps plot raster objects with ggplot2
 library(terra)
 
 
 # Assuming ensemble_rasterCURRENT is a SpatRaster object
 raster_dfc1 <- as.data.frame(ensemfu585_61_80, xy = TRUE)
 
 str(raster_dfc1)# Check what the column name is for the raster values
 
 ggplot(raster_dfc1) +
   geom_tile(aes(x = x, y = y, fill = Ageratum.conyzoides.L._EMmeanByTSS_mergedData_mergedRun_mergedAlgo)) + 
   scale_fill_viridis_c(guide = guide_colorbar(title = NULL)) +  # Remove legend heading
   ggtitle("Distribution of Ageratum conyzoides (SSP585(2061-2080)") +  # Update title
   theme_minimal() +
   theme(plot.title = element_text(size = 10, hjust = 0.5),  # Center title and set size
         axis.title.x = element_text(size = 8),
         axis.title.y = element_text(size = 8),
         axis.text.x = element_text(size = 6),
         axis.text.y = element_text(size = 6),
         legend.title = element_blank()) +  # Ensure no title for the legend
   labs(x = "Longitude", y = "Latitude")  # Update axis labels
 
 
 ###############################################################
 # SSP585(a1-d1)
 # Set the path to the folder containing future climate data
 fut_clim_d1 <-  "D:/INDIA MIROC6 2.5 FUT/ssp585F20181-100"
 
 # Load all raster files from the folder
 fut_clim_d1 <- list.files(fut_clim_d1, pattern = ".tif$", full.names = TRUE)
 
 # Create a raster stack from the future climate files (using underscores instead of hyphens in object names)
 env_585f81_100 <- stack(fut_clim_d1)
 
 # Check the stack
 print(env_585f81_100)
 
 
 
 # Step 2: Project the future climate data using the existing model
 future_585_81_100 <- BIOMOD_Projection(
   bm.mod = myBiomodModel00,              # Your existing model
   new.env = env_585f81_100,                   # Future environmental raster stack
   proj.name = "Future_bio_585(81-100)",         # Name for the projection
   selected.models = "all"                 # Choose all models
 )
 
 # Step 3: Ensemble forecasting based on the future projections
 myBiomodEMProj_Futured1 <- BIOMOD_EnsembleForecasting(
   bm.em = myBiomodEM1,                   # Your ensemble model
   bm.proj = future_585_81_100,            # Use the future projection
   models.chosen = 'all',                  # Include all models in the ensemble
   metric.binary = 'all',                  # Use all binary metrics
   metric.filter = 'all'                   # Use all filter metrics
 )
 
 # Check the results of the future ensemble projections
 print(myBiomodEMProj_Futured1)
 
 # Optional: Plot the results to visualize the future ensemble predictions
 plot(myBiomodEMProj_Futured1)  # Default plot
 myBiomodEMProj_Futured1
 # Extract the ensemble projection from your result (assuming it's a raster)
 ensemfu585_81_100 <- get_predictions(myBiomodEMProj_Futured1)
 library(ggplot2)
 library(rasterVis)  # Helps plot raster objects with ggplot2
 library(terra)
 
 
 # Assuming ensemble_rasterCURRENT is a SpatRaster object
 raster_dfd1 <- as.data.frame(ensemfu585_81_100, xy = TRUE)
 
 str(raster_dfd1)# Check what the column name is for the raster values
 
 ggplot(raster_dfd1) +
   geom_tile(aes(x = x, y = y, fill = Ageratum.conyzoides.L._EMmeanByTSS_mergedData_mergedRun_mergedAlgo)) + 
   scale_fill_viridis_c(guide = guide_colorbar(title = NULL)) +  # Remove legend heading
   ggtitle("Distribution of Ageratum conyzoides (SSP585(2081-2100)") +  # Update title
   theme_minimal() +
   theme(plot.title = element_text(size = 10, hjust = 0.5),  # Center title and set size
         axis.title.x = element_text(size = 8),
         axis.title.y = element_text(size = 8),
         axis.text.x = element_text(size = 6),
         axis.text.y = element_text(size = 6),
         legend.title = element_blank()) +  # Ensure no title for the legend
   labs(x = "Longitude", y = "Latitude")  # Update axis labels
   
   
 #############################################################
 # SSP245(1A-4A)
 # Set the path to the folder containing future climate data
 fut_clim_1A <-  "D:/INDIA MIROC6 2.5 FUT/SSP245F21-40"
 
 # Load all raster files from the folder
 fut_clim_1A <- list.files(fut_clim_1A, pattern = ".tif$", full.names = TRUE)
 
 # Create a raster stack from the future climate files (using underscores instead of hyphens in object names)
 env_245f21_40 <- stack(fut_clim_1A)
 
 # Check the stack
 print( env_245f21_40)
 
 
 
 # Step 2: Project the future climate data using the existing model
 future_245_21_40 <- BIOMOD_Projection(
   bm.mod = myBiomodModel00,              # Your existing model
   new.env = env_245f21_40,                  # Future environmental raster stack
   proj.name = "Future_bio_245(21-40)",         # Name for the projection
   selected.models = "all"                 # Choose all models
 )
 
 # Step 3: Ensemble forecasting based on the future projections
 myBiomodEMProj_Future1A <- BIOMOD_EnsembleForecasting(
   bm.em = myBiomodEM1,                   # Your ensemble model
   bm.proj = future_245_21_40,            # Use the future projection
   models.chosen = 'all',                  # Include all models in the ensemble
   metric.binary = 'all',                  # Use all binary metrics
   metric.filter = 'all'                   # Use all filter metrics
 )
 
 # Check the results of the future ensemble projections
 print(myBiomodEMProj_Future1A)
 
 # Optional: Plot the results to visualize the future ensemble predictions
 plot(myBiomodEMProj_Future1A)  # Default plot
 myBiomodEMProj_Future1A
 # Extract the ensemble projection from your result (assuming it's a raster)
 ensemfu245_21_40 <- get_predictions(myBiomodEMProj_Future1A)
 library(ggplot2)
 library(rasterVis)  # Helps plot raster objects with ggplot2
 library(terra)
 
 
 # Assuming ensemble_rasterCURRENT is a SpatRaster object
 raster_df1A <- as.data.frame(ensemfu245_21_40, xy = TRUE)
 
 str(raster_df1A)# Check what the column name is for the raster values
 
 ggplot(raster_df1A) +
   geom_tile(aes(x = x, y = y, fill = Ageratum.conyzoides.L._EMmeanByTSS_mergedData_mergedRun_mergedAlgo)) + 
   scale_fill_viridis_c(guide = guide_colorbar(title = NULL)) +  # Remove legend heading
   ggtitle("Distribution of Ageratum conyzoides (SSP245(2021-2040)") +  # Update title
   theme_minimal() +
   theme(plot.title = element_text(size = 10, hjust = 0.5),  # Center title and set size
         axis.title.x = element_text(size = 8),
         axis.title.y = element_text(size = 8),
         axis.text.x = element_text(size = 6),
         axis.text.y = element_text(size = 6),
         legend.title = element_blank()) +  # Ensure no title for the legend
   labs(x = "Longitude", y = "Latitude")  # Update axis labels
 #############################################################
 
 
 # SSP245(1A-4A)
 # Set the path to the folder containing future climate data
 fut_clim_2A <-  "D:/INDIA MIROC6 2.5 FUT/ssp245F41-60"
 
 # Load all raster files from the folder
 fut_clim_2A <- list.files(fut_clim_2A, pattern = ".tif$", full.names = TRUE)
 
 # Create a raster stack from the future climate files (using underscores instead of hyphens in object names)
 env_245f41_60 <- stack(fut_clim_2A)
 
 # Check the stack
 print( env_245f41_60)
 
 
 
 # Step 2: Project the future climate data using the existing model
 future_245_41_60 <- BIOMOD_Projection(
   bm.mod = myBiomodModel00,              # Your existing model
   new.env = env_245f41_60,                  # Future environmental raster stack
   proj.name = "Future_bio_245(41-60)",         # Name for the projection
   selected.models = "all"                 # Choose all models
 )
 
 # Step 3: Ensemble forecasting based on the future projections
 myBiomodEMProj_Future2A <- BIOMOD_EnsembleForecasting(
   bm.em = myBiomodEM1,                   # Your ensemble model
   bm.proj = future_245_41_60,            # Use the future projection
   models.chosen = 'all',                  # Include all models in the ensemble
   metric.binary = 'all',                  # Use all binary metrics
   metric.filter = 'all'                   # Use all filter metrics
 )
 
 # Check the results of the future ensemble projections
 print(myBiomodEMProj_Future2A)
 
 # Optional: Plot the results to visualize the future ensemble predictions
 plot(myBiomodEMProj_Future2A)  # Default plot
 myBiomodEMProj_Future2A
 # Extract the ensemble projection from your result (assuming it's a raster)
 ensemfu245_41_60 <- get_predictions(myBiomodEMProj_Future2A)
 library(ggplot2)
 library(rasterVis)  # Helps plot raster objects with ggplot2
 library(terra)
 
 
 # Assuming ensemble_rasterCURRENT is a SpatRaster object
 raster_df2A <- as.data.frame(ensemfu245_41_60, xy = TRUE)
 
 str(raster_df2A)# Check what the column name is for the raster values
 
 ggplot(raster_df2A) +
   geom_tile(aes(x = x, y = y, fill = Ageratum.conyzoides.L._EMmeanByTSS_mergedData_mergedRun_mergedAlgo)) + 
   scale_fill_viridis_c(guide = guide_colorbar(title = NULL)) +  # Remove legend heading
   ggtitle("Distribution of Ageratum conyzoides (SSP245(2041-2060)") +  # Update title
   theme_minimal() +
   theme(plot.title = element_text(size = 10, hjust = 0.5),  # Center title and set size
         axis.title.x = element_text(size = 8),
         axis.title.y = element_text(size = 8),
         axis.text.x = element_text(size = 6),
         axis.text.y = element_text(size = 6),
         legend.title = element_blank()) +  # Ensure no title for the legend
   labs(x = "Longitude", y = "Latitude")  # Update axis labels
 
 #############################################################
 
 
 # SSP245(1A-4A)
 # Set the path to the folder containing future climate data
 fut_clim_3A <-  "D:/INDIA MIROC6 2.5 FUT/ssp245F61-80"
 
 # Load all raster files from the folder
 fut_clim_3A <- list.files(fut_clim_3A, pattern = ".tif$", full.names = TRUE)
 
 # Create a raster stack from the future climate files (using underscores instead of hyphens in object names)
 env_245f61_80 <- stack(fut_clim_3A)
 
 # Check the stack
 print( env_245f61_80)
 
 
 
 # Step 2: Project the future climate data using the existing model
 future_245_61_80 <- BIOMOD_Projection(
   bm.mod = myBiomodModel00,              # Your existing model
   new.env = env_245f61_80,                  # Future environmental raster stack
   proj.name = "Future_bio_245(61-80)",         # Name for the projection
   selected.models = "all"                 # Choose all models
 )
 
 # Step 3: Ensemble forecasting based on the future projections
 myBiomodEMProj_Future3A <- BIOMOD_EnsembleForecasting(
   bm.em = myBiomodEM1,                   # Your ensemble model
   bm.proj = future_245_61_80,            # Use the future projection
   models.chosen = 'all',                  # Include all models in the ensemble
   metric.binary = 'all',                  # Use all binary metrics
   metric.filter = 'all'                   # Use all filter metrics
 )
 
 # Check the results of the future ensemble projections
 print(myBiomodEMProj_Future3A)
 
 # Optional: Plot the results to visualize the future ensemble predictions
 plot(myBiomodEMProj_Future3A)  # Default plot
 myBiomodEMProj_Future3A
 # Extract the ensemble projection from your result (assuming it's a raster)
 ensemfu245_61_80 <- get_predictions(myBiomodEMProj_Future3A)
 library(ggplot2)
 library(rasterVis)  # Helps plot raster objects with ggplot2
 library(terra)
 
 
 # Assuming ensemble_rasterCURRENT is a SpatRaster object
 raster_df3A <- as.data.frame(ensemfu245_61_80, xy = TRUE)
 
 str(raster_df3A)# Check what the column name is for the raster values
 
 ggplot(raster_df3A) +
   geom_tile(aes(x = x, y = y, fill = Ageratum.conyzoides.L._EMmeanByTSS_mergedData_mergedRun_mergedAlgo)) + 
   scale_fill_viridis_c(guide = guide_colorbar(title = NULL)) +  # Remove legend heading
   ggtitle("Distribution of Ageratum conyzoides (SSP245(2061-2080)") +  # Update title
   theme_minimal() +
   theme(plot.title = element_text(size = 10, hjust = 0.5),  # Center title and set size
         axis.title.x = element_text(size = 8),
         axis.title.y = element_text(size = 8),
         axis.text.x = element_text(size = 6),
         axis.text.y = element_text(size = 6),
         legend.title = element_blank()) +  # Ensure no title for the legend
   labs(x = "Longitude", y = "Latitude")  # Update axis labels
 
 #############################################################
 
 
 # SSP245(1A-4A)
 # Set the path to the folder containing future climate data
 fut_clim_4A <-  "D:/INDIA MIROC6 2.5 FUT/ssp245F81-100"
 
 # Load all raster files from the folder
 fut_clim_4A <- list.files(fut_clim_4A, pattern = ".tif$", full.names = TRUE)
 
 # Create a raster stack from the future climate files (using underscores instead of hyphens in object names)
 env_245f81_100<- stack(fut_clim_4A)
 
 # Check the stack
 print( env_245f81_100)
 
 
 
 # Step 2: Project the future climate data using the existing model
 future_245_81_100 <- BIOMOD_Projection(
   bm.mod = myBiomodModel00,              # Your existing model
   new.env = env_245f81_100,                # Future environmental raster stack
   proj.name = "Future_bio_245(81-100)",     # Name for the projection
   selected.models = "all"                 # Choose all models
 )
 
 # Step 3: Ensemble forecasting based on the future projections
 myBiomodEMProj_Future4A <- BIOMOD_EnsembleForecasting(
   bm.em = myBiomodEM1,                   # Your ensemble model
   bm.proj = future_245_81_100,           # Use the future projection
   models.chosen = 'all',                  # Include all models in the ensemble
   metric.binary = 'all',                  # Use all binary metrics
   metric.filter = 'all'                   # Use all filter metrics
 )
 
 # Check the results of the future ensemble projections
 print(myBiomodEMProj_Future4A)
 
 # Optional: Plot the results to visualize the future ensemble predictions
 plot(myBiomodEMProj_Future4A)  # Default plot
 myBiomodEMProj_Future4A
 # Extract the ensemble projection from your result (assuming it's a raster)
 ensemfu245_81_100 <- get_predictions(myBiomodEMProj_Future4A)
 library(ggplot2)
 library(rasterVis)  # Helps plot raster objects with ggplot2
 library(terra)
 
 
 # Assuming ensemble_rasterCURRENT is a SpatRaster object
 raster_df4A <- as.data.frame(ensemfu245_81_100, xy = TRUE)
 
 str(raster_df4A)# Check what the column name is for the raster values
 
 ggplot(raster_df4A) +
   geom_tile(aes(x = x, y = y, fill = Ageratum.conyzoides.L._EMmeanByTSS_mergedData_mergedRun_mergedAlgo)) + 
   scale_fill_viridis_c(guide = guide_colorbar(title = NULL)) +  # Remove legend heading
   ggtitle("Distribution of Ageratum conyzoides (SSP245(2081-2100)") +  # Update title
   theme_minimal() +
   theme(plot.title = element_text(size = 10, hjust = 0.5),  # Center title and set size
         axis.title.x = element_text(size = 8),
         axis.title.y = element_text(size = 8),
         axis.text.x = element_text(size = 6),
         axis.text.y = element_text(size = 6),
         legend.title = element_blank()) +  # Ensure no title for the legend
   labs(x = "Longitude", y = "Latitude")  # Update axis labels
 ###################################################################
 
 #ssp3704B-1B
 
 # Set the path to the folder containing future climate data
 fut_clim_4B <-  "D:/INDIA MIROC6 2.5 FUT/ssp370F21-40"
 
 # Load all raster files from the folder
 fut_clim_4B <- list.files(fut_clim_4B, pattern = ".tif$", full.names = TRUE)
 
 # Create a raster stack from the future climate files (using underscores instead of hyphens in object names)
 env_370f21_40<- stack(fut_clim_4B)
 
 # Check the stack
 print( env_370f21_40)
 
 
 
 # Step 2: Project the future climate data using the existing model
 future_370_21_40 <- BIOMOD_Projection(
   bm.mod = myBiomodModel00,              # Your existing model
   new.env = env_370f21_40,                # Future environmental raster stack
   proj.name = "Future_bio_370(21-40)",     # Name for the projection
   selected.models = "all"                 # Choose all models
 )
 
 # Step 3: Ensemble forecasting based on the future projections
 myBiomodEMProj_Future4B <- BIOMOD_EnsembleForecasting(
   bm.em = myBiomodEM1,                   # Your ensemble model
   bm.proj =  future_370_21_40,           # Use the future projection
   models.chosen = 'all',                  # Include all models in the ensemble
   metric.binary = 'all',                  # Use all binary metrics
   metric.filter = 'all'                   # Use all filter metrics
 )
 
 # Check the results of the future ensemble projections
 print(myBiomodEMProj_Future4B)
 
 # Optional: Plot the results to visualize the future ensemble predictions
 plot(myBiomodEMProj_Future4B)  # Default plot
 myBiomodEMProj_Future4B
 # Extract the ensemble projection from your result (assuming it's a raster)
 ensemfu370_21_40 <- get_predictions(myBiomodEMProj_Future4B)
 library(ggplot2)
 library(rasterVis)  # Helps plot raster objects with ggplot2
 library(terra)
 
 
 # Assuming ensemble_rasterCURRENT is a SpatRaster object
 raster_df4B <- as.data.frame( ensemfu370_21_40, xy = TRUE)
 
 str(raster_df4B)# Check what the column name is for the raster values
 
 ggplot(raster_df4B) +
   geom_tile(aes(x = x, y = y, fill = Ageratum.conyzoides.L._EMmeanByTSS_mergedData_mergedRun_mergedAlgo)) + 
   scale_fill_viridis_c(guide = guide_colorbar(title = NULL)) +  # Remove legend heading
   ggtitle("Distribution of Ageratum conyzoides (SSP370(2021-2040)") +  # Update title
   theme_minimal() +
   theme(plot.title = element_text(size = 10, hjust = 0.5),  # Center title and set size
         axis.title.x = element_text(size = 8),
         axis.title.y = element_text(size = 8),
         axis.text.x = element_text(size = 6),
         axis.text.y = element_text(size = 6),
         legend.title = element_blank()) +  # Ensure no title for the legend
   labs(x = "Longitude", y = "Latitude")  # Update axis labels
 ###################################################################
 
 #ssp3704B-1B
 
 # Set the path to the folder containing future climate data
 fut_clim_3B <-  "D:/INDIA MIROC6 2.5 FUT/SSP370F41-60"
 
 # Load all raster files from the folder
 fut_clim_3B <- list.files(fut_clim_3B, pattern = ".tif$", full.names = TRUE)
 
 # Create a raster stack from the future climate files (using underscores instead of hyphens in object names)
 env_370f41_60<- stack(fut_clim_3B)
 
 # Check the stack
 print( env_370f41_60)
 
 
 
 # Step 2: Project the future climate data using the existing model
 future_370_41_60 <- BIOMOD_Projection(
   bm.mod = myBiomodModel00,              # Your existing model
   new.env = env_370f41_60,                # Future environmental raster stack
   proj.name = "Future_bio_370(41-60)",     # Name for the projection
   selected.models = "all"                 # Choose all models
 )
 
 # Step 3: Ensemble forecasting based on the future projections
 myBiomodEMProj_Future3B <- BIOMOD_EnsembleForecasting(
   bm.em = myBiomodEM1,                   # Your ensemble model
   bm.proj =  future_370_41_60,           # Use the future projection
   models.chosen = 'all',                  # Include all models in the ensemble
   metric.binary = 'all',                  # Use all binary metrics
   metric.filter = 'all'                   # Use all filter metrics
 )
 
 # Check the results of the future ensemble projections
 print(myBiomodEMProj_Future3B)
 
 # Optional: Plot the results to visualize the future ensemble predictions
 plot(myBiomodEMProj_Future3B)  # Default plot
 myBiomodEMProj_Future3B
 # Extract the ensemble projection from your result (assuming it's a raster)
 ensemfu370_41_60 <- get_predictions(myBiomodEMProj_Future3B)
 library(ggplot2)
 library(rasterVis)  # Helps plot raster objects with ggplot2
 library(terra)
 
 
 # Assuming ensemble_rasterCURRENT is a SpatRaster object
 raster_df3B <- as.data.frame( ensemfu370_41_60, xy = TRUE)
 
 str(raster_df3B)# Check what the column name is for the raster values
 
 ggplot(raster_df3B) +
   geom_tile(aes(x = x, y = y, fill = Ageratum.conyzoides.L._EMmeanByTSS_mergedData_mergedRun_mergedAlgo)) + 
   scale_fill_viridis_c(guide = guide_colorbar(title = NULL)) +  # Remove legend heading
   ggtitle("Distribution of Ageratum conyzoides (SSP370(2041-2060)") +  # Update title
   theme_minimal() +
   theme(plot.title = element_text(size = 10, hjust = 0.5),  # Center title and set size
         axis.title.x = element_text(size = 8),
         axis.title.y = element_text(size = 8),
         axis.text.x = element_text(size = 6),
         axis.text.y = element_text(size = 6),
         legend.title = element_blank()) +  # Ensure no title for the legend
   labs(x = "Longitude", y = "Latitude")  # Update axis labels
 
 ###################################################################
 
 #ssp3704B-1B
 
 # Set the path to the folder containing future climate data
 fut_clim_2B <-  "D:/INDIA MIROC6 2.5 FUT/ssp370F61-80"
 
 # Load all raster files from the folder
 fut_clim_2B <- list.files(fut_clim_2B, pattern = ".tif$", full.names = TRUE)
 
 # Create a raster stack from the future climate files (using underscores instead of hyphens in object names)
 env_370f61_80<- stack(fut_clim_2B)
 
 # Check the stack
 print( env_370f61_80)
 
 
 
 # Step 2: Project the future climate data using the existing model
 future_370_61_80 <- BIOMOD_Projection(
   bm.mod = myBiomodModel00,              # Your existing model
   new.env = env_370f61_80,                # Future environmental raster stack
   proj.name = "Future_bio_370(61-80)",     # Name for the projection
   selected.models = "all"                 # Choose all models
 )
 
 # Step 3: Ensemble forecasting based on the future projections
 myBiomodEMProj_Future2B <- BIOMOD_EnsembleForecasting(
   bm.em = myBiomodEM1,                   # Your ensemble model
   bm.proj =  future_370_61_80,           # Use the future projection
   models.chosen = 'all',                  # Include all models in the ensemble
   metric.binary = 'all',                  # Use all binary metrics
   metric.filter = 'all'                   # Use all filter metrics
 )
 
 # Check the results of the future ensemble projections
 print(myBiomodEMProj_Future2B)
 
 # Optional: Plot the results to visualize the future ensemble predictions
 plot(myBiomodEMProj_Future2B)  # Default plot
 myBiomodEMProj_Future2B
 # Extract the ensemble projection from your result (assuming it's a raster)
 ensemfu370_61_80 <- get_predictions(myBiomodEMProj_Future2B)
 library(ggplot2)
 library(rasterVis)  # Helps plot raster objects with ggplot2
 library(terra)
 
 
 # Assuming ensemble_raster is a SpatRaster object
 raster_df2B <- as.data.frame( ensemfu370_61_80, xy = TRUE)
 
 str(raster_df2B)# Check what the column name is for the raster values
 
 ggplot(raster_df2B) +
   geom_tile(aes(x = x, y = y, fill = Ageratum.conyzoides.L._EMmeanByTSS_mergedData_mergedRun_mergedAlgo)) + 
   scale_fill_viridis_c(guide = guide_colorbar(title = NULL)) +  # Remove legend heading
   ggtitle("Distribution of Ageratum conyzoides (SSP370(2061-2080)") +  # Update title
   theme_minimal() +
   theme(plot.title = element_text(size = 10, hjust = 0.5),  # Center title and set size
         axis.title.x = element_text(size = 8),
         axis.title.y = element_text(size = 8),
         axis.text.x = element_text(size = 6),
         axis.text.y = element_text(size = 6),
         legend.title = element_blank()) +  # Ensure no title for the legend
   labs(x = "Longitude", y = "Latitude")  # Update axis labels

 
 #########################################################################
 #ssp3704B-1B
 
 # Set the path to the folder containing future climate data
 fut_clim_1B <-  "D:/INDIA MIROC6 2.5 FUT/SSP370F81-100"
 
 # Load all raster files from the folder
 fut_clim_1B <- list.files(fut_clim_1B, pattern = ".tif$", full.names = TRUE)
 
 # Create a raster stack from the future climate files (using underscores instead of hyphens in object names)
 env_370f81_100 <- stack(fut_clim_1B)
 
 # Check the stack
 print( env_370f81_100)
 
 
 
 # Step 2: Project the future climate data using the existing model
 future_370_81_100 <- BIOMOD_Projection(
   bm.mod = myBiomodModel00,              # Your existing model
   new.env = env_370f81_100,                # Future environmental raster stack
   proj.name = "Future_bio_370(81-100)", # Name for the projection
   selected.models = "all"                 # Choose all models
 )
 
 # Step 3: Ensemble forecasting based on the future projections
 myBiomodEMProj_Future1B <- BIOMOD_EnsembleForecasting(
   bm.em = myBiomodEM1,                   # Your ensemble model
   bm.proj =   future_370_81_100,           # Use the future projection
   models.chosen = 'all',                  # Include all models in the ensemble
   metric.binary = 'all',                  # Use all binary metrics
   metric.filter = 'all'                   # Use all filter metrics
 )
 
 # Check the results of the future ensemble projections
 print(myBiomodEMProj_Future1B)
 
 # Optional: Plot the results to visualize the future ensemble predictions
 plot(myBiomodEMProj_Future1B)  # Default plot
 myBiomodEMProj_Future1B
 # Extract the ensemble projection from your result (assuming it's a raster)
 ensemfu370_81_100 <- get_predictions(myBiomodEMProj_Future1B)
 library(ggplot2)
 library(rasterVis)  # Helps plot raster objects with ggplot2
 library(terra)
 
 
 # Assuming ensemble_rasterCURRENT is a SpatRaster object
 raster_df1B <- as.data.frame( ensemfu370_81_100, xy = TRUE)
 
 str(raster_df1B)# Check what the column name is for the raster values
 
 ggplot(raster_df1B) +
   geom_tile(aes(x = x, y = y, fill = Ageratum.conyzoides.L._EMmeanByTSS_mergedData_mergedRun_mergedAlgo)) + 
   scale_fill_viridis_c(guide = guide_colorbar(title = NULL)) +  # Remove legend heading
   ggtitle("Distribution of Ageratum conyzoides (SSP370(2081-2100)") +  # Update title
   theme_minimal() +
   theme(plot.title = element_text(size = 10, hjust = 0.5),  # Center title and set size
         axis.title.x = element_text(size = 8),
         axis.title.y = element_text(size = 8),
         axis.text.x = element_text(size = 6),
         axis.text.y = element_text(size = 6),
         legend.title = element_blank()) +  # Ensure no title for the legend
   labs(x = "Longitude", y = "Latitude")  # Update axis labels
 