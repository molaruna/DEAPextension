#! /usr/bin/env Rscript

# DEAPext is a command-line executable tool that incorporates DEAP code to automatically run multiple multivariate linear regressions. 
# It can be used to carry out fixed or mixed regressions, with modifiable features such as: the dependent variables queried, 
# independent variable, covariates, z-scoring, base parameter option, ability to add an additional variable, 
# filtering for fmri and QC scores. This tool allows contains the ability to create 2D or 3D cortical projection heatmaps 
# with the effects of the regressions, with the option of masking the effects for p-val<0.05. 

# Usage Example: 
# ./DEAPext.R --mat3D --masked --qc --mrif snoring_matched smri_thick_cort.desikan sleep_15_p 
# --fixed race.ethnicity.5level,sex,high.educ.bl,married.bl,age,household.income.bl,hisp
# --rand rel_family_id,abcd_site,src_subject_id

# Current version: DEAPext v2.5

# Author: maria.olaru@ucsf.edu

r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

if("argparser" %in% rownames(installed.packages()) == FALSE) {install.packages("argparser")}
library(argparser)

p <- arg_parser("DEAP extension toolbox\n 
                  Here's an example of a command you might run:\n 
                  ./DEAPext.R --mat3D --masked --qc --mrif --fixed race.4level,sex,high.educ.bl,married.bl,age,household.income.bl 
                  --rand rel_family_id,abcd_site,src_subject_id snoring_matched smri_thick_cort.desikan sleep_15_p")


#Add positional arguments
p = add_argument(p, "outdir",  help = "User-specified analysis folder")
p = add_argument(p, "depvar",  help = "Dependent variable string for partial matching of variables in data")
p = add_argument(p, "indvar",  help = "Independent variable")

#Add optional arguments
p = add_argument(p, "--fixed",      help = "Fixed effect variables", flag = FALSE, default= NULL)
p = add_argument(p, "--rand",       help = "Random effect variables", flag = FALSE, default = NULL)
p = add_argument(p, "--addvar",     help = "filepath of additional variable in 3-column CSV: src_subj_id, eventname, variable", flag = FALSE, default = NULL)
p = add_argument(p, "--baseparam",  help = "Baseline parameter for lm", flag = FALSE, default = NULL)
p = add_argument(p, "--baseline",   help = "Filter subject data from baseline visit", default = TRUE)

#Add flags
p = add_argument(p, "--zsc",      help = "zscore numerical fields of data", flag = TRUE)
p = add_argument(p, "--qc",       help = "Filter subject scans with recommended imaging guidelines", flag = TRUE)
p = add_argument(p, "--mrif",     help = "Filter subject scans with mrif score of 1 or 2", flag = TRUE)
p = add_argument(p, "--mat2D",    help = "Create 2D matrix and plot for correlation data", flag = TRUE)
p = add_argument(p, "--mat3D",    help = "FS command for 3D cortical projection", flag = TRUE)
p = add_argument(p, "--masked",   help = "mat3D option: Display effect sizes with p<0.05", flag = TRUE)
p = add_argument(p, "--ss",       help = "mat3D option: Take screenshots", flag = TRUE)
p = add_argument(p, "--ordered",  help = "Run orthogonal regressions if independent variable is an ordered factor", flag = TRUE)

argv <- parse_args(p)

####Initialize Environment
if("gamm4" %in% rownames(installed.packages()) == FALSE) {install.packages("gamm4")}
if("rjson" %in% rownames(installed.packages()) == FALSE) {install.packages("rjson")}
if("stargazer" %in% rownames(installed.packages()) == FALSE) {install.packages("stargazer")}
if("knitr" %in% rownames(installed.packages()) == FALSE) {install.packages("knitr")}
if("R.matlab" %in% rownames(installed.packages()) == FALSE) {install.packages("R.matlab")}
if("tableone" %in% rownames(installed.packages()) == FALSE) {install.packages("tableone")}
if("tidyr" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyr")}
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}
if("stringi" %in% rownames(installed.packages()) == FALSE) {install.packages("stringi")}
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
if("gplots" %in% rownames(installed.packages()) == FALSE) {install.packages("gplots")}
if("RColorBrewer" %in% rownames(installed.packages()) == FALSE) {install.packages("RColorBrewer")}
if("methods" %in% rownames(installed.packages()) == FALSE) {install.packages("methods")}
if("base" %in% rownames(installed.packages()) == FALSE) {install.packages("base")}
if("rstudioapi" %in% rownames(installed.packages()) == FALSE) {install.packages("rstudioapi")}

library(gamm4)
library(rjson)
library(stargazer)
library(knitr)
library(R.matlab)
library(tableone)
library(tidyr)
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(gplots)
library(RColorBrewer)
library(methods)
library(base)
library(rstudioapi)

#### Output version
cat("Running DEAPext v2.5\n\n")
#### Show user settings
cat ("User-specified Settings: \n", 
     "Out dir:\t", argv$outdir, "\n", 
     "Dep var:\t", argv$depvar, "\n",
     "Ind var:\t", argv$indvar, "\n", 
     "Fixed vars:\t", argv$fixed, "\n", 
     "Rand vars:\t", argv$rand, "\n", 
     "Add var:\t", argv$addvar, "\n", 
     "Zsc opt:\t", argv$zsc, "\n", 
     "QC opt:\t", argv$qc, "\n",
     "Mrif opt:\t", argv$mrif, "\n", 
     "Baseparam opt:\t", argv$baseparam, "\n",
     "Baseline opt:\t", argv$baseline, "\n", 
     "2D opt:\t", argv$mat2D, "\n", 
     "3D opt:\t", argv$mat3D, "\n", 
     "Masked opt:\t", argv$masked, "\n",
     "Screenshot opt:", argv$ss, "\n", 
     "Ordered opt:\t", argv$ordered, "\n\n")


####Function: get script directory path
get_directory <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file <- "--file="
  rstudio <- "RStudio"
  
  match <- grep(rstudio, args)
  if (length(match) > 0) {
    return(dirname(rstudioapi::getSourceEditorContext()$path))
  } else {
    match <- grep(file, args)
    if (length(match) > 0) {
      return(dirname(normalizePath(sub(file, "", args[match]))))
    } else {
      return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
    }
  }
}

#### Import data
user.dir_path = paste0(getwd(), "/")
user.script_path = get_directory()
user.data_path = paste0(user.script_path, "/nda2.0.2.Rds")

#### Inputs
#Store inputs as list object
inputs = rep(list(list()), 3) 
names(inputs) <- c("fields", "boolean_opts", "addl_specs")
reg_rand.opts = c("mri_info_deviceserialnumber", "abcd_site", "src_subject_id", "rel_family_id")

out_filenames = c()

############################################################
#BEGIN ENTERING IN ANALYSIS PREFERENCES
############################################################
inputs$addl_specs [[ 'anal_dir' ]]    = argv$outdir
inputs$addl_specs [['dep_var.query' ]]= argv$depvar
inputs$fields [[ 'ind_var' ]]         = argv$indvar
inputs$addl_specs [[ 'base_param' ]]  = argv$baseparam
inputs$fields [[ 'cov.fixed' ]]       = if(!is.na(argv$fixed)) unlist(strsplit(argv$fixed, split=","))
inputs$fields [[ 'reg_rand' ]]        = if(!is.na(argv$rand)) unlist(strsplit(argv$rand, split=","))
data.addl_path                        = argv$addvar

#Unfinished code
#inputs.fields [[ 'gr_var' ]] = gr_var
#inputs.fields [[ 'user_covar' ]] = user_covar
#inputs.fields [[ 'ws.var' ]] = ws.var
#inputs.fields [[ 'smo.var' ]] = smo.var
#inputs.fields [[ 'log.var' ]] = log.var
#inputs.fields [[ 'int.var' ]] = int.var
#inputs.fields [[ 'sq.var' ]] = sq.var
#inputs.fields [[ 'fl.var' ]] = fl.var

inputs$boolean_opts [[ 'zsc' ]]         = argv$zsc
qc_opt                                  = argv$qc
mrif_opt                                = argv$mrif
baseline_only                           = argv$baseline
inputs$boolean_opts [[ 'mat_heatmap' ]] = argv$mat2D
inputs$boolean_opts [[ 'cort_heatmap' ]]= argv$mat3D
inputs$boolean_opts [[ 'cort.masked' ]] = argv$masked
inputs$boolean_opts [[ 'cort.ss' ]]     = argv$ss
inputs$boolean_opts [[ 'ordered' ]]     = argv$ordered



############################################################
#FINISH ENTERING IN ANALYSIS PREFERENCES
############################################################

#Change NULL values in list to NA
if (is.null(inputs$fields$cov.fixed)) inputs$fields$cov.fixed = NA
if (is.null(inputs$fields$reg_rand)) inputs$fields$reg_rand = NA
if (is.null(inputs$addl_specs$base_param)) inputs$addl_specs$base_param = NA


#Check dependent variable input is present
depvarqueryVar = unlist(inputs$addl_specs [[ 'dep_var.query' ]])

#### Store data (from DEAPext script)
if (!exists('data.orig')) {
  cat("Loading DEAP data...")
  data.orig = readRDS(user.data_path)
}

#### Derive additional variables
#First derived value: field names of the dependent variable queried
inputs$fields [[ 'dep_var.vals' ]] = grep(depvarqueryVar, names(data.orig), value = TRUE)

#Second derived value: QC option
if (qc_opt == T) {
  if(grepl("rsfmri", depvarqueryVar)) {
    inputs$fields [[ 'qc' ]] = c('fsqc_qc', 'rsfmri_cor_network.gordon_ntpoints') 
  }else if(grepl("tfmri_mid", depvarqueryVar)) {
    inputs$fields [[ 'qc' ]] = c('fsqc_qc', 'tfmri_mid_beh_perform.flag', 'tfmri_mid_all_beta_dof', 'tfmri_mid_all_sem_dof')
  }else if(grepl("tfmri_sst", depvarqueryVar)) {
    inputs$fields [[ 'qc' ]] = c('fsqc_qc', 'tfmri_sst_beh_perform.flag', 'tfmri_sst_all_beta_dof', 'tfmri_sst_all_sem_dof')
  }else if(grepl("smri", depvarqueryVar) | grepl("dti", depvarqueryVar)) {
    inputs$fields [[ 'qc' ]] = 'fsqc_qc'
  }else {
    inputs$fields [[ 'qc' ]] = NA
    warning(paste0("Your dependent variable type (", depvarqueryVar, ") does not contain a QC option. No QC will be completed. View -help option for more information"))
  }
} else {
  inputs$fields [[ 'qc' ]] = NA
}

#Third derived value: mrif option
if (mrif_opt == T) {
  inputs$fields [[ 'mrif' ]] = "mrif_score"
} else {
  inputs$fields [[ 'mrif' ]] = NA
}


#### Create dictionaries for cortical desikan & gordon atlases
#Cheat command: paste(temp_vec, collapse = "', '")
#Dictionary for Desikan cortical atlas
desikan_cort_dic = c('_cort.desikan_bankssts.lh', '_cort.desikan_bankssts.rh', '_cort.desikan_caudalanteriorcingulate.lh', '_cort.desikan_caudalanteriorcingulate.rh', '_cort.desikan_caudalmiddlefrontal.lh', '_cort.desikan_caudalmiddlefrontal.rh', '_cort.desikan_cuneus.lh', '_cort.desikan_cuneus.rh', '_cort.desikan_entorhinal.lh', '_cort.desikan_entorhinal.rh', '_cort.desikan_frontalpole.lh', '_cort.desikan_frontalpole.rh', '_cort.desikan_fusiform.lh', '_cort.desikan_fusiform.rh', '_cort.desikan_inferiorparietal.lh', '_cort.desikan_inferiorparietal.rh', '_cort.desikan_inferiortemporal.lh', '_cort.desikan_inferiortemporal.rh', '_cort.desikan_insula.lh', '_cort.desikan_insula.rh', '_cort.desikan_isthmuscingulate.lh', '_cort.desikan_isthmuscingulate.rh', '_cort.desikan_lateraloccipital.lh', '_cort.desikan_lateraloccipital.rh', '_cort.desikan_lateralorbitofrontal.lh', '_cort.desikan_lateralorbitofrontal.rh', '_cort.desikan_lingual.lh', '_cort.desikan_lingual.rh', '_cort.desikan_medialorbitofrontal.lh', '_cort.desikan_medialorbitofrontal.rh', '_cort.desikan_middletemporal.lh', '_cort.desikan_middletemporal.rh', '_cort.desikan_paracentral.lh', '_cort.desikan_paracentral.rh', '_cort.desikan_parahippocampal.lh', '_cort.desikan_parahippocampal.rh', '_cort.desikan_parsopercularis.lh', '_cort.desikan_parsopercularis.rh', '_cort.desikan_parsorbitalis.lh', '_cort.desikan_parsorbitalis.rh', '_cort.desikan_parstriangularis.lh', '_cort.desikan_parstriangularis.rh', '_cort.desikan_pericalcarine.lh', '_cort.desikan_pericalcarine.rh', '_cort.desikan_postcentral.lh', '_cort.desikan_postcentral.rh', '_cort.desikan_posteriorcingulate.lh', '_cort.desikan_posteriorcingulate.rh', '_cort.desikan_precentral.lh', '_cort.desikan_precentral.rh', '_cort.desikan_precuneus.lh', '_cort.desikan_precuneus.rh', '_cort.desikan_rostralanteriorcingulate.lh', '_cort.desikan_rostralanteriorcingulate.rh', '_cort.desikan_rostralmiddlefrontal.lh', '_cort.desikan_rostralmiddlefrontal.rh', '_cort.desikan_superiorfrontal.lh', '_cort.desikan_superiorfrontal.rh', '_cort.desikan_superiorparietal.lh', '_cort.desikan_superiorparietal.rh', '_cort.desikan_superiortemporal.lh', '_cort.desikan_superiortemporal.rh', '_cort.desikan_supramarginal.lh', '_cort.desikan_supramarginal.rh', '_cort.desikan_temporalpole.lh', '_cort.desikan_temporalpole.rh', '_cort.desikan_transversetemporal.lh', '_cort.desikan_transversetemporal.rh')

#Dictionary for gordon cortical atlas
gordon_cort_dic1 = c('_cor_network.gordon_auditory_network.gordon_auditory', '_cor_network.gordon_auditory_network.gordon_cingulooperc', '_cor_network.gordon_auditory_network.gordon_cinguloparietal', '_cor_network.gordon_auditory_network.gordon_default', '_cor_network.gordon_auditory_network.gordon_dorsalattn', '_cor_network.gordon_auditory_network.gordon_frontoparietal', '_cor_network.gordon_auditory_network.gordon_none', '_cor_network.gordon_auditory_network.gordon_retrosplenialtemporal', '_cor_network.gordon_auditory_network.gordon_salience', '_cor_network.gordon_auditory_network.gordon_smhand', '_cor_network.gordon_auditory_network.gordon_smmouth', '_cor_network.gordon_auditory_network.gordon_ventralattn', '_cor_network.gordon_auditory_network.gordon_visual', '_cor_network.gordon_cingulooperc_network.gordon_auditory', '_cor_network.gordon_cingulooperc_network.gordon_cingulooperc', '_cor_network.gordon_cingulooperc_network.gordon_cinguloparietal', '_cor_network.gordon_cingulooperc_network.gordon_default', '_cor_network.gordon_cingulooperc_network.gordon_dorsalattn', '_cor_network.gordon_cingulooperc_network.gordon_frontoparietal', '_cor_network.gordon_cingulooperc_network.gordon_none', '_cor_network.gordon_cingulooperc_network.gordon_retrosplenialtemporal', '_cor_network.gordon_cingulooperc_network.gordon_salience', '_cor_network.gordon_cingulooperc_network.gordon_smhand', '_cor_network.gordon_cingulooperc_network.gordon_smmouth', '_cor_network.gordon_cingulooperc_network.gordon_ventralattn', '_cor_network.gordon_cingulooperc_network.gordon_visual', '_cor_network.gordon_cinguloparietal_network.gordon_auditory', '_cor_network.gordon_cinguloparietal_network.gordon_cingulooperc', '_cor_network.gordon_cinguloparietal_network.gordon_cinguloparietal', '_cor_network.gordon_cinguloparietal_network.gordon_default', '_cor_network.gordon_cinguloparietal_network.gordon_dorsalattn', '_cor_network.gordon_cinguloparietal_network.gordon_frontoparietal', '_cor_network.gordon_cinguloparietal_network.gordon_none', '_cor_network.gordon_cinguloparietal_network.gordon_retrosplenialtemporal', '_cor_network.gordon_cinguloparietal_network.gordon_salience', '_cor_network.gordon_cinguloparietal_network.gordon_smhand', '_cor_network.gordon_cinguloparietal_network.gordon_smmouth', '_cor_network.gordon_cinguloparietal_network.gordon_ventralattn', '_cor_network.gordon_cinguloparietal_network.gordon_visual', '_cor_network.gordon_default_network.gordon_auditory', '_cor_network.gordon_default_network.gordon_cingulooperc', '_cor_network.gordon_default_network.gordon_cinguloparietal', '_cor_network.gordon_default_network.gordon_default', '_cor_network.gordon_default_network.gordon_dorsalattn', '_cor_network.gordon_default_network.gordon_frontoparietal', '_cor_network.gordon_default_network.gordon_none')
gordon_cort_dic2 = c('_cor_network.gordon_default_network.gordon_retrosplenialtemporal', '_cor_network.gordon_default_network.gordon_salience', '_cor_network.gordon_default_network.gordon_smhand', '_cor_network.gordon_default_network.gordon_smmouth', '_cor_network.gordon_default_network.gordon_ventralattn', '_cor_network.gordon_default_network.gordon_visual', '_cor_network.gordon_dorsalattn_network.gordon_auditory', '_cor_network.gordon_dorsalattn_network.gordon_cingulooperc', '_cor_network.gordon_dorsalattn_network.gordon_cinguloparietal', '_cor_network.gordon_dorsalattn_network.gordon_default', '_cor_network.gordon_dorsalattn_network.gordon_dorsalattn', '_cor_network.gordon_dorsalattn_network.gordon_frontoparietal', '_cor_network.gordon_dorsalattn_network.gordon_none', '_cor_network.gordon_dorsalattn_network.gordon_retrosplenialtemporal', '_cor_network.gordon_dorsalattn_network.gordon_salience', '_cor_network.gordon_dorsalattn_network.gordon_smhand', '_cor_network.gordon_dorsalattn_network.gordon_smmouth', '_cor_network.gordon_dorsalattn_network.gordon_ventralattn', '_cor_network.gordon_dorsalattn_network.gordon_visual', '_cor_network.gordon_frontoparietal_network.gordon_auditory', '_cor_network.gordon_frontoparietal_network.gordon_cingulooperc', '_cor_network.gordon_frontoparietal_network.gordon_cinguloparietal', '_cor_network.gordon_frontoparietal_network.gordon_default', '_cor_network.gordon_frontoparietal_network.gordon_dorsalattn', '_cor_network.gordon_frontoparietal_network.gordon_frontoparietal', '_cor_network.gordon_frontoparietal_network.gordon_none', '_cor_network.gordon_frontoparietal_network.gordon_retrosplenialtemporal', '_cor_network.gordon_frontoparietal_network.gordon_salience', '_cor_network.gordon_frontoparietal_network.gordon_smhand', '_cor_network.gordon_frontoparietal_network.gordon_smmouth', '_cor_network.gordon_frontoparietal_network.gordon_ventralattn', '_cor_network.gordon_frontoparietal_network.gordon_visual', '_cor_network.gordon_none_network.gordon_auditory', '_cor_network.gordon_none_network.gordon_cingulooperc', '_cor_network.gordon_none_network.gordon_cinguloparietal', '_cor_network.gordon_none_network.gordon_default', '_cor_network.gordon_none_network.gordon_dorsalattn', '_cor_network.gordon_none_network.gordon_frontoparietal', '_cor_network.gordon_none_network.gordon_none', '_cor_network.gordon_none_network.gordon_retrosplenialtemporal', '_cor_network.gordon_none_network.gordon_salience', '_cor_network.gordon_none_network.gordon_smhand', '_cor_network.gordon_none_network.gordon_smmouth', '_cor_network.gordon_none_network.gordon_ventralattn', '_cor_network.gordon_none_network.gordon_visual', '_cor_network.gordon_retrosplenialtemporal_network.gordon_auditory', '_cor_network.gordon_retrosplenialtemporal_network.gordon_cingulooperc', '_cor_network.gordon_retrosplenialtemporal_network.gordon_cinguloparietal', '_cor_network.gordon_retrosplenialtemporal_network.gordon_default', '_cor_network.gordon_retrosplenialtemporal_network.gordon_dorsalattn', '_cor_network.gordon_retrosplenialtemporal_network.gordon_frontoparietal', '_cor_network.gordon_retrosplenialtemporal_network.gordon_none', '_cor_network.gordon_retrosplenialtemporal_network.gordon_retrosplenialtemporal', '_cor_network.gordon_retrosplenialtemporal_network.gordon_salience', '_cor_network.gordon_retrosplenialtemporal_network.gordon_smhand', '_cor_network.gordon_retrosplenialtemporal_network.gordon_smmouth', '_cor_network.gordon_retrosplenialtemporal_network.gordon_ventralattn', '_cor_network.gordon_retrosplenialtemporal_network.gordon_visual', '_cor_network.gordon_salience_network.gordon_auditory', '_cor_network.gordon_salience_network.gordon_cingulooperc', '_cor_network.gordon_salience_network.gordon_cinguloparietal', '_cor_network.gordon_salience_network.gordon_default')
gordon_cort_dic3 = c('_cor_network.gordon_salience_network.gordon_dorsalattn', '_cor_network.gordon_salience_network.gordon_frontoparietal', '_cor_network.gordon_salience_network.gordon_none', '_cor_network.gordon_salience_network.gordon_retrosplenialtemporal', '_cor_network.gordon_salience_network.gordon_salience', '_cor_network.gordon_salience_network.gordon_smhand', '_cor_network.gordon_salience_network.gordon_smmouth', '_cor_network.gordon_salience_network.gordon_ventralattn', '_cor_network.gordon_salience_network.gordon_visual', '_cor_network.gordon_smhand_network.gordon_auditory', '_cor_network.gordon_smhand_network.gordon_cingulooperc', '_cor_network.gordon_smhand_network.gordon_cinguloparietal', '_cor_network.gordon_smhand_network.gordon_default', '_cor_network.gordon_smhand_network.gordon_dorsalattn', '_cor_network.gordon_smhand_network.gordon_frontoparietal', '_cor_network.gordon_smhand_network.gordon_none', '_cor_network.gordon_smhand_network.gordon_retrosplenialtemporal', '_cor_network.gordon_smhand_network.gordon_salience', '_cor_network.gordon_smhand_network.gordon_smhand', '_cor_network.gordon_smhand_network.gordon_smmouth', '_cor_network.gordon_smhand_network.gordon_ventralattn', '_cor_network.gordon_smhand_network.gordon_visual', '_cor_network.gordon_smmouth_network.gordon_auditory', '_cor_network.gordon_smmouth_network.gordon_cingulooperc', '_cor_network.gordon_smmouth_network.gordon_cinguloparietal', '_cor_network.gordon_smmouth_network.gordon_default', '_cor_network.gordon_smmouth_network.gordon_dorsalattn', '_cor_network.gordon_smmouth_network.gordon_frontoparietal', '_cor_network.gordon_smmouth_network.gordon_none', '_cor_network.gordon_smmouth_network.gordon_retrosplenialtemporal', '_cor_network.gordon_smmouth_network.gordon_salience', '_cor_network.gordon_smmouth_network.gordon_smhand', '_cor_network.gordon_smmouth_network.gordon_smmouth', '_cor_network.gordon_smmouth_network.gordon_ventralattn', '_cor_network.gordon_smmouth_network.gordon_visual', '_cor_network.gordon_ventralattn_network.gordon_auditory', '_cor_network.gordon_ventralattn_network.gordon_cingulooperc', '_cor_network.gordon_ventralattn_network.gordon_cinguloparietal', '_cor_network.gordon_ventralattn_network.gordon_default', '_cor_network.gordon_ventralattn_network.gordon_dorsalattn', '_cor_network.gordon_ventralattn_network.gordon_frontoparietal', '_cor_network.gordon_ventralattn_network.gordon_none', '_cor_network.gordon_ventralattn_network.gordon_retrosplenialtemporal', '_cor_network.gordon_ventralattn_network.gordon_salience', '_cor_network.gordon_ventralattn_network.gordon_smhand', '_cor_network.gordon_ventralattn_network.gordon_smmouth', '_cor_network.gordon_ventralattn_network.gordon_ventralattn', '_cor_network.gordon_ventralattn_network.gordon_visual', '_cor_network.gordon_visual_network.gordon_auditory', '_cor_network.gordon_visual_network.gordon_cingulooperc', '_cor_network.gordon_visual_network.gordon_cinguloparietal', '_cor_network.gordon_visual_network.gordon_default', '_cor_network.gordon_visual_network.gordon_dorsalattn', '_cor_network.gordon_visual_network.gordon_frontoparietal', '_cor_network.gordon_visual_network.gordon_none', '_cor_network.gordon_visual_network.gordon_retrosplenialtemporal', '_cor_network.gordon_visual_network.gordon_salience', '_cor_network.gordon_visual_network.gordon_smhand', '_cor_network.gordon_visual_network.gordon_smmouth', '_cor_network.gordon_visual_network.gordon_ventralattn', '_cor_network.gordon_visual_network.gordon_visual')

gordon_cort_dic = c(gordon_cort_dic1, gordon_cort_dic2, gordon_cort_dic3)

##### Dictionary checking function
check_dict <- function(dictionary, vector, dictionary_name) {
  if (length(dictionary) > length(vector)) {
    stop("Your analysis does not contain all the areas listed in the ", dictionary_name)
  } else {
    indx_dic = sort(as.vector(sapply(dictionary, grep, vector)))
    if (length(indx_dic) < length(vector)) {
      warning("Removing dependent variables not in listed in the ", dictionary_name, ":\n", toString(vector[-c(indx_dic)]), "\n")
      cat("\n\n")
      return(vector[indx_dic])
    } else {
      return(vector)
    }
  }
}

#### Basic error handling
#2D heatmap error handling
if (inputs$boolean_opts[['mat_heatmap']] == T & !grepl("cor_network", depvarqueryVar)) {
  inputs$boolean_opts[[ 'mat_heatmap' ]] = F
  warning(paste0("Your dependent variable type (", depvarqueryVar, ") cannot be used for a 2D heatmap. No 2D heatmap will be made. View -help option for more information\n"))
}

if (inputs$boolean_opts[['mat_heatmap']] == T) {
  #Remove dependent variables not in Gordon cortical atlas
  print("working before")
  inputs$fields [[ 'dep_var.vals' ]] = check_dict(gordon_cort_dic, inputs$fields [[ 'dep_var.vals' ]], "Gordon cortical atlas")
  print("working after")
}

#3D cortical heatmap error handling
if (inputs$boolean_opts[['cort_heatmap']] == T & !grepl("_cort.desikan", depvarqueryVar)) {
  inputs$boolean_opts[[ 'cort_heatmap' ]] = F
  warning(paste0("Your dependent variable type (", depvarqueryVar, ") cannot be used for a 3D cortical heatmap. No 3D cortical heatmap will be made. View -help option for more information\n"))
}

if (inputs$boolean_opts[['cort_heatmap']] == T) {
  #Remove dependent variables not in Desikan atlas
  inputs$fields [[ 'dep_var.vals' ]] = check_dict(desikan_cort_dic, inputs$fields [[ 'dep_var.vals' ]], "Desikan cortical atlas")
}

#Check appropriate random effects are chosen 
if(any(!is.na(inputs$fields$reg_rand))) {
  if(any(is.na(match(inputs$fields$reg_rand, reg_rand.opts)))) {
    bad_vars = inputs$fields$reg_rand[which(is.na(match(inputs$fields$reg_rand,reg_rand.opts)))]
    stop(paste0("This model only runs with these random effects: ", toString(reg_rand.opts), "\n It cannot run with your input: ", bad_vars, "\n Try again using the random effects above."))
  }
}

if (any(!is.na(inputs$fields$reg_rand))) {
  if (any(is.na(match(inputs$fields$reg_rand, reg_rand.opts)))) {
    bad_vars = inputs$fields$reg_rand[which(is.na(match(inputs$fields$reg_rand,reg_rand.opts)))]
    stop(paste0("This model only runs with these random effects: ", toString(reg_rand.opts), "\n It cannot run with your input: ", bad_vars, "\n Try again using the random effects above."))
  }
}

#Remove subject random effect if only first timepoint of data is being analyzed
if (baseline_only == T) {
  if ("src_subject_id" %in% unlist(inputs$fields$reg_rand)) {
    inputs$fields$reg_rand = inputs$fields$reg_rand[inputs$fields$reg_rand != "src_subject_id"]
    warning(paste0("subject random effect removed because there is only one timepoint of subject data analyzed\n"))
  }
}

#Remove device random effect if site random effect is present
if ("abcd_site" %in% unlist(inputs$fields$reg_rand) & "mri_info_deviceserialnumber" %in% unlist(inputs$fields$reg_rand)) {
  stop(paste0("You cannot include both site and scanner in your model, choose one and try again"))
}

#Sanity check for input existence
if (is.na(inputs$addl_specs[[ 'anal_dir' ]]) | inputs$addl_specs[[ 'anal_dir' ]] == "") {
  stop(paste0("You need to include an analysis directory"))
}

if (is.na(inputs$fields [[ 'ind_var' ]]) | inputs$fields [[ 'ind_var' ]] == "") {
  stop(paste0("You need to include an independent variable"))
}

cat("\nYour analysis inputs: \n\n")
print(inputs)


#### Exit script if no inputs for variables in dataset are entered
#Original DEAP code -- not applicable for our code b/c it is possible for fields to be empty

# #exit script silently if @inputs(fields) is empty
# empty = T;
# for (key in names(inputs$fields)){
#   if(length(inputs$fields[[key]]) != 0 && inputs$fields[[key]] != ""){
#     empty = F;
#   }
# }
# 
# if(empty){
#   options(warn=-1)
#   opt <- options(show.error.messages=FALSE) 
#   on.exit(options(opt)) 
#   stop()
# }

#### Unlist variables
independendVar     = unlist(inputs$fields[['ind_var']])
covfixedVar        = unlist(inputs$fields[['cov.fixed']])
regrandVar         = unlist(inputs$fields[['reg_rand']])
dependendVar.vals  = unlist(inputs$fields[['dep_var.vals']])
qcVar              = unlist(inputs$fields[['qc']])
mrifVar            = unlist(inputs$fields[['mrif']])
zscVar             = unlist(inputs$boolean_opts[['zsc']])
matheatmapVar      = unlist(inputs$boolean_opts[['mat_heatmap']])
cortheatmapVar     = unlist(inputs$boolean_opts[['cort_heatmap']])
cortmaskedVar      = unlist(inputs$boolean_opts[['cort.masked']])
cortssVar          = unlist(inputs$boolean_opts[['cort.ss']])
orderedVar         = unlist(inputs$boolean_opts[['ordered']])
analdirVar         = unlist(inputs$addl_specs[['anal_dir']])
baseparamVar       = unlist(inputs$addl_specs[['base_param']])


# Unfinished options
# groupVar = unlist(inputs[['gr.var']])
# usercovVar     = paste(unlist(inputs[['user_covar']]),  sep='+')
# wsVar = unlist(inputs[['ws.var']])
# smoothVar.all = unlist(inputs[['smo.var']])
# logVar = unlist(inputs[['log.var']])
# interactionVar = unlist(inputs[['int.var']])
# sqVar = unlist(inputs[['sq.var']])
# subsetVar = unlist(inputs[['fl.var']]);

#### Function that extracts inputs from list object of user input
extract.variables = function(a){
  rt = c()
  for (l in 1:length(a) ){
    #if(length(a[[l]]) > 0){
    if(!is.na(a[[l]][1])) {
      for(e in 1:length(a[[l]])){
        #if(unlist(a[[l]][[e]]) != "")
        if(!is.na(unlist(a[[l]][[e]]))) {
          rt = c(rt, unlist(a[[l]][[e]]))
        }
      }
    }
  }
  rt_inster = c()
  for( item in 1:length(rt)){
    if(!is.character(rt[item])){
      next
    }
    if(length(unlist(strsplit(rt[item],"[*]"))) > 1){
      rt_inster = c(rt_inster, unlist(strsplit(rt[item],"[*]")))
    }
    else if(length(unlist(strsplit(rt[item],"[+]"))) > 1){
      rt_inster = c(rt_inster, unlist(strsplit(rt[item],"[+]")))
    }
    
    else if(length(unlist(strsplit(rt[item],"^2", fixed=TRUE))) > 1){
      rt_inster = c(rt_inster, unlist(strsplit(rt[item],"^2", fixed=TRUE)))
    }
    else if( length(regmatches(rt[item], gregexpr("(?<=\\().*?(?=\\))", rt[item], perl=T))[[1]] ) > 0 ){
      
      rt_inster = c(rt_inster, regmatches(rt[item], gregexpr("(?<=\\().*?(?=\\))", rt[item], perl=T))[[1]])
    }else{
      rt_inster = c(rt_inster, rt[item])
    }
  }
  rt = rt_inster
  return(rt);
}

#### Create dataframe with variables of interest
varList.initial = extract.variables(inputs$fields)
vars.in.data = varList.initial[varList.initial %in% names(data.orig)]

#Add add'l vars: subj ID & default random effect variables
#Create full list of input fields 
vars.keep = c("src_subject_id","eventname", vars.in.data)
vars.keep = vars.keep[!duplicated(vars.keep)] #there are copies b/c derived keys contain same vars as user-inputted keys 

#Create a dataframe only with fields of interest
data.int = data.orig[,vars.keep]

if (baseline_only == T) {
  data.int = data.int[data.int$eventname == "baseline_year_1_arm_1", ]
}

if (!is.na(data.addl_path)) {
  data.addl = read.csv(data.addl_path)
  #data.addl = apply(data.addl,2,function(x)gsub('\\s+', '',x))
  data.int = merge(x = data.addl, y = data.int, by = c("src_subject_id", "eventname"), all.x = T)
  if(anyDuplicated(data.int$rel_family_id) == 0) {
    if ("rel_family_id" %in% unlist(inputs$fields$reg_rand)) {
      inputs$fields$reg_rand = inputs$fields$reg_rand[inputs$fields$reg_rand != "rel_family_id"]
      regrandVar= unlist(inputs$fields[['reg_rand']])
      warning(paste0("family random effect removed because there are no duplicate family ids\n"))
    }
  }
}

if (any(is.na(match(varList.initial, names(data.int))))) {
  bad_vars = varList.initial[which(is.na(match(varList.initial, names(data.orig))))]
  stop(paste0("These fields do not exist: ", bad_vars, "\nTry again using fields from the dataset"))
}


#### Some basic functions
##################
##  functions   ##
##################

# Original DEAP code - I don't see this function anywhere in the code
# Mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }

# Original DEAP code - not currently implemented
# sep.vars = function(x){
#   x = gsub(" ", "", x, fixed = TRUE)
#   x = unlist(strsplit(x,"+",fixed=T))
#   return(x)
# }

# Original DEAP code - not currently implemented
# If the index value is outside the 0.25%-99.75% range of values, value is recoded as NA
# censor =  function(x, fraction=.005){
#   if(length(fraction) != 1 || fraction < 0 || fraction > 1){
#     stop("bad value for 'fraction'")
#   }
#   lim <- quantile(x, probs=c(fraction/2, 1-fraction/2), na.rm = T)
#   x[ x < lim[1] ] <- NA
#   x[ x > lim[2] ] <- NA
#   x
# }

#By using Rserve gamm4 is already loaded
##### Censors the data if it is numeric
### censor/windsorize first
# wsVar = sep.vars(wsVar) #Gets rid of empty indices
# 
# #Censor the dataframe for all fields that are numeric
# if(length(wsVar)>0){
#   for(ii in 1:length(wsVar)){
#     if(class(data[,wsVar[ii]]) == "numeric"){
#       data[,wsVar[ii]] = censor(data[,wsVar[ii]])
#     }
#   }
# }

#### Original DEAP code -- Log-transforms dependent variable
##if y is log-transformed...
# for (i in 1:length(dependendVar)) {
# if(substring(dependendVar[i],1,4) == "log("){
#   dependendVar.name[i] = substring(dependendVar[i],5,nchar(dependendVar[i])-1)
#   if(sum(data.int[[dependendVar.name[i]]] <= 0, na.rm=T) > 0){
#     data.int[[dependendVar.name[i]]][data.int[[dependendVar.name[i]]] <= 0] = NA
#     trigger.warning = T
#     warning.logging.0 = paste0("1 or more log transformed variable contains values <=0. All <=0 values replaced with NA.")
#   }
#   data.int$Y.log = log(data.int[[dependendVar.name[i]]])
#   new.name = paste0("log.",dependendVar.name[i])
#   names(data.int)[names(data.int) == "Y.log"] = new.name
#   dependendVar[i] = new.name
# }
# }

#### Stops script if dependent variable is a factor w/ 3+ levels or doesn't exist in database
for (i in 1:length(dependendVar.vals)) {
  if(dependendVar.vals[i] %in% names(data.int)){
    if(is.factor(data.int[[dependendVar.vals[i]]])){
      if(nlevels(data.int[[dependendVar.vals[i]]]) > 2){
        stop("Categorical variables with more than 2 levels are not supported as dependent variables. \nConsider converting your categorical variable into a continuous variable.")
      }
    }
  } else {
    stop(paste("Dependent variable <", dependendVar.vals[i] ,">does not exist in the database"));
  }
}

#### Original DEAP code -- Format add'l user-inputted and derived key variable values 
# smoothVar.all = sep.vars(smoothVar.all)
# 
# logVar = unlist(inputs[['log.var']])
# logVar = sep.vars(logVar)

#check if 0's in logged vars. if so, add 0.0001
# strip.log = substring(logVar,5,nchar(logVar)-1)

#### Original DEAP code -- Ensure all log-formatted values are greater than 0, if not... recode to NA
# if(length(strip.log)>0){
#   if(sum(data[,strip.log]<=0 , na.rm=T) > 0){
#     trigger.warning = T
#     warning.logging.0 = paste0("1 or more log transformed variable contains values <=0. All <=0 values replaced with NA.")
#     for(ii in 1:length(strip.log)){
#       log.var_i = strip.log[ii]
#       if(sum(data[,log.var_i]<=0 , na.rm=T) > 0){
#         data[[log.var_i]][data[[log.var_i]] <= 0] = NA
#       }
#     }
#   }
# }

#### Original DEAP code -- Format variables that need to be squared 
# sqVar = sep.vars(sqVar)
# sqVar = substring(sqVar,1,nchar(sqVar)-2)
# 
# sqVar_SQUARED = NULL
# if(length(sqVar)>0){
#   for(ii in 1:length(sqVar)){
#     sqVar_SQUARED[ii] = paste0(sqVar[ii],"_SQUARED")
#     data[,sqVar_SQUARED[ii]] = data[,sqVar[ii]]^2
#   }
# }

#### Original DEAP code -- Format group & subset variables
# if(length(groupVar) > 0){
#   if(is.character(groupVar) & nchar(groupVar) ==0){
#     groupVar = NULL 
#   } 
# }

# if(length(subsetVar) > 0){
#   if(is.character(subsetVar) & nchar(subsetVar) ==0){
#     subsetVar = NULL 
#   } 
# }

#### Original DEAP code -- Transform group variable appropriately (smooth, log, square) for interaction w/ independent variable
#Interacting grouping variable with independent variable
#if(length(groupVar)>0){
#may need to change it instead of searching for the string, it strips string first and looks for exact match
#  is.smooth = grepl(independendVar, smoothVar.all)
#  is.log =    independendVar %in% substring(logVar,5,nchar(logVar)-1)
#  is.square = independendVar %in% sqVar
## If independent var is smooth
#  if( sum(is.smooth) > 0 ){
#replace s(independendVar) with s(independendVar, by = groupVar)
#    smoothVar.all[is.smooth] = paste0("s(", independendVar,",by=",groupVar, ")")
#  } else if(is.square){
#else if squared, add var*groupvar and var_SQUARED*groupvar
#    interactionVar = c(interactionVar, paste0(independendVar,"*",groupVar), paste0(independendVar,"_SQUARED*",groupVar) )
#  } else if(is.log){    
# else if log, interact with log(var)
#    log.independent = logVar[independendVar == substring(logVar,5,nchar(logVar)-1)]
#    interactionVar = c(interactionVar, paste0(log.independent,"*",groupVar) )
#  } else {
# else make normal interaction
#    interactionVar = c(interactionVar, paste0(independendVar,"*",groupVar) )
#  }
#}  #may need to do another if with ^2 independent variables

##### Original DEAP code -- Find smooth variables for interaction, independent variable, and add'l variables... 
# smoothVarInt.ind = grepl(",by=", smoothVar.all)
# smoothVarInt = smoothVar.all[smoothVarInt.ind]
# smoothVar =    smoothVar.all[!smoothVarInt.ind]
# 
# if(0 %in% nchar(sqVar)) sqVar = character()
# 
# print(sqVar)
# print(paste("length sqVar",length(sqVar)))
#nestVar = c("Site", "FamilyID")
#usercovVar =  usercovVar[!(usercovVar %in% nestVar)]

#TODO: seperate Site and Familiy to another catagory of random effect

#if(include.random.site){
#  inputs[['cov.fixed']][[which(unlist(inputs[['cov.fixed']]) == "abcd_site")]] = NULL
#}
#inputs[['cov.fixed']][[which(unlist(inputs[['cov.fixed']]) == "rel_family_id")]] = NULL

# smoothVarInt.stripped.term1 = ""
# smoothVarInt.stripped.term2 = ""

#### Original DEAP code -- Remove duplicate user-defined covariates (add'l ind. vars), if they are coded w/in transformed variable keys
# if(length(smoothVarInt)>0){
#   smoothVarInt.stripped.term1 = unlist(lapply( strsplit(smoothVarInt,","), function(x)x[[1]]))
#   smoothVarInt.stripped.term1 = substring(smoothVarInt.stripped.term1,3,nchar(smoothVarInt.stripped.term1))
#   smoothVarInt.stripped.term2 = unlist(lapply( strsplit(smoothVarInt,"by="), function(x)x[[2]]))
#   smoothVarInt.stripped.term2 = substring(smoothVarInt.stripped.term2,1,nchar(smoothVarInt.stripped.term2)-1)
# }

### if usercovVar's have been transformed, then they are stored in usercovVar as well as the 
### transformed vars (smoothVar, logVar, etc.), in which they will need to be removed from
### usercovVar before putting into formula
# print(paste("before remove",usercovVar))
# print(c( substring(smoothVar,3,nchar(smoothVar)-1),
#          smoothVarInt.stripped.term1,
#          smoothVarInt.stripped.term2,
#          substring(logVar,5,nchar(logVar)-1),
#          sqVar ))
# cov.ind.remove = usercovVar %in% c( substring(smoothVar,3,nchar(smoothVar)-1),
#                                     smoothVarInt.stripped.term1,
#                                     smoothVarInt.stripped.term2,
#                                     substring(logVar,5,nchar(logVar)-1),
#                                     sqVar )
# if(sum(cov.ind.remove)>0 ){
#   usercovVar = usercovVar[!cov.ind.remove]
# }
# print(paste("after remove",usercovVar))
# 
# #remove covfixedVar when it is a smooth, smooth interaction (first variable), or log -- should usually only be age of covfixedVar
# #ok to keep it in as interaction, squared, or smooth interaction term
# covfixedVar.ind.remove = covfixedVar %in% c( substring(smoothVar,3,nchar(smoothVar)-1),
#                                              smoothVarInt.stripped.term1,
#                                              substring(logVar,5,nchar(logVar)-1))
# if(sum(covfixedVar.ind.remove)>0 ){
#   covfixedVar = covfixedVar[!covfixedVar.ind.remove]
# }

#### Original DEAP code -- Define independent variable as categorical or numeric
#if independent variable has 5 or less unique values change it to character/factor variable
#categorical.independent = FALSE

#DEAP has this section commented out
#if( length(table(data[[independendVar]])) < 6 ){
#  data[[independendVar]] = as.character(data[[independendVar]])
#  categorical.independent = TRUE
#} else{
#  data[[independendVar]] = as.numeric(as.character(data[[independendVar]]))
#}

#if(class(data.int[[independendVar]]) != "numeric"){
#  categorical.independent = TRUE
#}

#user defined covariates
#for(ucov in unlist(inputs[['usercov.']])){
#  data[[ucov]] = as.numeric(as.character(data[[ucov]]))
#}

#print(summary(data.int[[independendVar]]))

#### Subset data based on boolean input options
data.subset = data.int

print(paste0("Overall n (data of interest): ", nrow(data.subset)), quote=F)
#Subset for QC
if (!is.na(qcVar[1])) {
  indx_pass = which(data.subset[[qcVar[1]]] == "accept") #first variable is always fsqc_qc
  if (length(qcVar) > 1) {
    for (i in 2:length(qcVar)) {
      if(qcVar[i] == "rsfmri_cor_network.gordon_ntpoints") {
        new_indx = which(data.subset[[qcVar[i]]] > 375)
        indx_pass = as.numeric(na.omit(indx_pass[match(new_indx, indx_pass)]))
        
      } else if(grepl("tfmri_.*_beh_perform.flag", qcVar[i])) {
        new_indx = which(data.subset[[qcVar[i]]] == "acceptable") #old version was 1
        indx_pass = as.numeric(na.omit(indx_pass[match(new_indx, indx_pass)]))
        
      } else if(grepl("tfmri_.*_all_.*_dof", qcVar[i])) {
        new_indx = which(data.subset[[qcVar[i]]] > 200)
        indx_pass = as.numeric(na.omit(indx_pass[match(new_indx, indx_pass)]))
      }
    }
  }
  data.subset = data.subset[indx_pass, ]
  print(paste0("Overall n (after QC): ", nrow(data.subset)), quote=F)
}


##Subset for normal mri score
if(!is.na(mrifVar)) {
  indx_pass = which(data.subset[[mrifVar]] == "No abnormal findings" | data.subset[[mrifVar]] == "Normal anatomical variant of no clinical significance")
  data.subset = data.subset[indx_pass, ]
  print(paste0("Overall n (after mrif): ", nrow(data.subset)), quote=F)
}

##Label independent variable to ordered factor
if (orderedVar == TRUE) {
  if (class(data.subset[[independendVar]]) == "factor") {
    data.subset[[independendVar]] = ordered(data.subset[[independendVar]])
    print(paste(c("Independent Variable:", independendVar, "is now:", class(data.subset[[independendVar]])), collapse = " "))
  }
  else {
    print(paste0("Independent Variable: ", independendVar, "is not a factor, cannot convert to an ordered factor..."))
  }
}

##Remove incomplete entries
data.subset = data.subset[complete.cases(data.subset),]

print(paste0("Overall n: ", nrow(data.subset)), quote=F)

#### Adjust levels 

if(!is.na(baseparamVar) & !is.factor(data.subset[[independendVar]])) {
  warning("There are not multiple parameters to reassign. Reverting to default baseline.")
  baseparamVar = NA
}

if (is.factor(data.subset[[independendVar]])) {
  indVar.factor = T
  indVar.factor.num = length(table(data.subset[[independendVar]]))
  if(!is.na(baseparamVar)) {
    if(is.na(match(baseparamVar, levels(data.subset[[independendVar]])))) {
      stop("Your baseline parameter must be one of these levels: ", toString(levels(data.subset[[independendVar]])))
    } else {
      data.subset[[independendVar]] = relevel(data.subset[[independendVar]], baseparamVar)
    }
  } else {
    baseparamVar = levels(data.subset[[independendVar]])[1]
  }
} else {
  indVar.factor = F
  indVar.factor.num = 1
}


#### zscoring
#Check that all dependent variables are same class
indx <- match(dependendVar.vals, names(data.subset))

if (length(dependendVar.vals) == 1) {
  depVar.class_type = class(data.subset[[dependendVar.vals]])
} else {
  depVar.class_type = names(table(sapply(data.subset[ ,indx], class)))
}

if (length(depVar.class_type) > 1) {
  stop(paste0("Dependent variables contain multiple class types: ", depVar.class_type, "\n Use a dependent variable expression that will only match with variables of the same class"))
}

# Change dependent variable class to numeric 
if(is.factor(depVar.class_type)){
  stop("Your dependent variable is a factor, need to add a logistic regression option")
} else{
  if (length(dependendVar.vals) == 1) {
    data.subset[ ,indx] = as.numeric(as.character(data.subset[ ,indx]))
  } else {
    data.subset[ ,indx] = lapply(data.subset[ ,indx], function(x) as.numeric(as.character(x)))
  }
}

#zscore option
if (zscVar == T) {
  data.subset[ ,c(indx)] = data.frame(lapply(data.subset[ ,c(indx)], function(x) scale(x, center = TRUE)))
}

#### Functions for filing system
create_dir = function(dir_name) {
  if (!dir.exists(dir_name)) {
    dir.create(dir_name)
  }
}

append_filename <- function(filename) {
  if(file.exists(filename)) {
    stri_sub(filename, -4, -5) <- 1
    append_filename(filename)
  } else {
    return(filename)
  }
}

#### Export meta dataframe
#Create directory structure
user.anal_path = paste0(user.dir_path, analdirVar, "/")
out_dir = paste0(user.anal_path, independendVar, '/tables/table_inputs/')

create_dir(user.anal_path)
create_dir(paste0(user.anal_path ,independendVar))
create_dir(paste0(user.anal_path, independendVar, "/tables"))
create_dir(paste0(user.anal_path, independendVar, "/tables/table_inputs/"))

data.input_name = paste(depvarqueryVar, ".", independendVar, sep = "")
data.input_fp = paste0(out_dir, data.input_name, ".csv")
data.input_fp = append_filename(data.input_fp)

write.csv(data.subset, data.input_fp)
cat("Input data:", data.input_fp, "\n")

out_filenames = c(out_filenames, data.input_fp)

#### Linear model Functions
#Function Input requirements: the dataframe and column indices of independent variable and regressors

make_formula_random = function(randVar) {
  
  #Create variables for random effects
  include.random.scanner = "mri_info_deviceserialnumber" %in% randVar
  include.random.site    = "abcd_site" %in% randVar
  include.random.subject = "src_subject_id" %in% randVar
  include.family.id      = "rel_family_id" %in% randVar
  
  formula.random.str = c()
  
  if(include.random.site & include.family.id & !include.random.subject){
    formula.random.str = "(1|abcd_site/rel_family_id)"
  } else if(include.random.scanner & include.family.id & !include.random.subject){
    formula.random.str = "(1|mri_info_deviceserialnumber/rel_family_id)"
  } else {
    if(include.random.site) {
      formula.random.str = "(1|abcd_site)"
    }
    if(include.random.scanner) {
      formula.random.str = c(formula.random.str, "(1|mri_info_deviceserialnumber)")
    }
    if(include.family.id) {
      formula.random.str = c(formula.random.str, "(1|rel_family_id)")
    }
    if(include.random.subject) {
      formula.random.str = c(formula.random.str, "(1|src_subject_id)")
    }
  }
  
  formula.random.str = formula.random.str[!is.na(formula.random.str)]
  formula.random.str = paste0("~",paste(formula.random.str, collapse = "+"))
  formula.random = formula.random.str
  
  return(formula.random)
}
make_formula_string = function(dv, covVar, indVar) {
  if (any(is.na(covVar))) {
    formula_string = paste(dv, " ~ ", paste(indVar, collapse='+'))
  } else {
    formula_string = paste(dv, " ~ ", paste(c(indVar, covVar), collapse='+'))
  }
  return(formula_string)
}

mixed_model = function(dv, df, indVar, covVar, randVar, dv_vec, lrange) {
  
  #track regression completion
  dv_indx = grep(paste0("^",dv,"$"), dependendVar.vals)
  
  #Run lm
  formula_string = make_formula_string(dv, covVar, indVar)
  formula_random = make_formula_random(randVar)
  reg_model = gamm4(as.formula(formula_string), random = as.formula(formula_random), data = df)
  
  ###Output first formula for reference, and dep var tracking
  if (dv_indx == 1) {
    cat(" Mixed Model: \n <dependendVar> ~", toString(paste0(reg_model$gam$pred.formula)[2]), "\n", paste0(formula_random), "\n\n")
    out_filenames = c(out_filenames, reg_model$gam$pred.formula[2], formula_random)
  }
  
  cat('current dependent variable is: ', dv , " (",dv_indx ,"/", length(dv_vec), ")\n", sep = "")
  
  #Extract coefficient data
  coef_summary = summary(reg_model$gam)$p.table[lrange, 1:4]
  
  if(length(lrange) == 1) {
    coef_summary = as.data.frame(t(as.matrix(c(parameter_comp = row.names(summary(reg_model$gam)$p.table)[lrange], dep_var=dv, coef_summary))))
  } else {
    coef_summary = cbind.data.frame(parameter_comp = row.names(coef_summary), dep_var=dv, coef_summary)
    rownames(coef_summary) = c()
  }
  
  #Extract anova data
  anova_summary = summary(reg_model$gam)$pTerms.table[1, 1:3]
  anova_summary = cbind.data.frame(ind_var = indVar, dep_var = dv, t(anova_summary))
  
  summary_list = list(coef_summary, anova_summary)
  
  return(summary_list)
}
fixed_model = function(dv, df, indVar, covVar, dv_vec, lrange) {
  
  #track regression completion
  dv_indx = grep(paste0("^",dv,"$"), dependendVar.vals)
  
  #Run lm
  formula_string = make_formula_string(dv, covVar, indVar)
  reg_model = lm(formula = formula_string, data = df)
  
  ###Output first formula for reference
  if (dv_indx == 1) {
    cat(" Fixed Model: \n <dependendVar> ~", trimws(str_split(formula_string, "~")[[1]][2]), "\n\n")  
    out_filenames = c(out_filenames, trimws(str_split(formula_string, "~")[[1]][2]))
  }
  
  cat('current dependent variable is: ', dv , " (",dv_indx ,"/", length(dv_vec), ")\n", sep = "")
  
  #Extract coefficient data
  coef_summary = summary(reg_model)$coefficients[lrange, 1:4]
  
  if(length(lrange) == 1) {
    coef_summary = as.data.frame(t(as.matrix(c(parameter_comp = row.names(summary(reg_model)$coefficients)[lrange], dep_var=dv, coef_summary))))
  } else {
    coef_summary = cbind.data.frame(parameter_comp = row.names(coef_summary), dep_var=dv, coef_summary)
    rownames(coef_summary) = c()
  }
  
  #Extract anova data
  anova_summary = anova(reg_model)[1, 1:5]
  anova_summary = cbind.data.frame(ind_var = indVar, dep_var = dv, anova_summary)
  rownames(anova_summary) = c()
  
  summary_list = list(coef_summary, anova_summary)
  
  return(summary_list)
}

#### Running multivariate linear regression 
#Determine how many rows to pull from summary lm data
if (indVar.factor == F) {
  lrange = 2
} else {
  lrange = 2:indVar.factor.num
}

if(any(is.na(regrandVar))) {
  list_dfs = lapply(dependendVar.vals, fixed_model, df = data.subset, indVar = independendVar, covVar = covfixedVar, dv_vec = dependendVar.vals, lrange = lrange) 
} else {
  list_dfs = lapply(dependendVar.vals, mixed_model, df = data.subset, indVar = independendVar, covVar = covfixedVar, randVar = regrandVar, dv_vec = dependendVar.vals, lrange = lrange)
}

#Separate coefficient and anova data
list_coefs = lapply(list_dfs, function(l) l[[1]])
list_anova = lapply(list_dfs, function(l) l[[2]])

#Transform list into dataframe
data.coefs = do.call(rbind.data.frame, list_coefs)
data.anova = do.call(rbind.data.frame, list_anova)

#Transform type to numeric
data.coefs[ ,3:6] = lapply(data.coefs[ ,3:6], function(x) as.numeric(as.character(x)))

#### Export dataframes
#Export coefficient table
out_dir.coefs = paste0(user.anal_path, independendVar, "/tables/table_coefs/")
create_dir(out_dir.coefs)

data.coefs_name = paste(data.input_name, ".coefs", sep = "") 
data.coefs_fp = paste0(out_dir.coefs, data.coefs_name, '.csv')
data.coefs_fp = append_filename(data.coefs_fp)

write.csv(data.coefs, data.coefs_fp)
cat("\ncoefficient table:", data.coefs_fp, "\n")
out_filenames = c(out_filenames, data.coefs_fp)

#Export ANOVA table
out_dir.anova = paste0(user.anal_path, independendVar, "/tables/table_anova/")
create_dir(out_dir.anova)

data.anova_name = paste(data.input_name, ".anova", sep = "") 
data.anova_fp = paste0(out_dir.anova, data.anova_name, '.csv')
data.anova_fp = append_filename(data.anova_fp)
out_filenames = c(out_filenames, data.anova_fp)

write.csv(data.anova, data.anova_fp)
cat("ANOVA table: ", data.anova_fp, "\n\n")

#### Preparing to format for heatmaps & FS
#Get all parameter names

if (length(table(data.coefs$parameter_comp)) == 1) { #reassign ind var if not a factor/does not contain levels
  param_comps = data.coefs$parameter_comp[1]
} else {
  param_comps = row.names(table(data.coefs$parameter_comp))
  param_comps_concat = gsub(" ", "", param_comps, fixed=TRUE)
  param_comps_concat = str_split(param_comps_concat, independendVar, simplify=TRUE)[,2]
  
  base_param_concat = gsub(" ", "", baseparamVar, fixed=TRUE)
}

#### Create heatmaps 
if (matheatmapVar == T) {
  print("running 2D matrix portion of script")
  for (i in 1:length(param_comps)) {  
    cat("creating heatmaps for: ", param_comps[i], "\n")
    data.mat <- subset(data.coefs, parameter_comp == param_comps[i])
    
    #Create df for all & sig effects
    data.mat_all = cbind.data.frame(dep_var = data.mat$dep_var, Estimate = data.mat$Estimate, p_val = data.mat$`Pr(>|t|)`)
    data.mat_sig = data.mat_all
    data.mat_sig[data.mat_sig$p_val>0.05, c("Estimate", "p_val")] <- NA
    
    #### Create matrices for heatmap
    mat.size = length(data.mat_all[ ,1])
    
    mat_all = data.mat_all$Estimate
    mat_sig = data.mat_sig$Estimate
    
    mat.names = (data.mat_all$dep_var)[1:sqrt(mat.size)]
    mat.names = sub(".*_", "", mat.names) #Assumption that network is last word before underscore
    
    dim(mat_all) = c(sqrt(mat.size),sqrt(mat.size))
    dim(mat_sig) = c(sqrt(mat.size),sqrt(mat.size))
    
    colnames(mat_all) = mat.names
    rownames(mat_all) = mat.names
    colnames(mat_sig) = mat.names
    rownames(mat_sig) = mat.names
    
    ####Output matrices
    out_dir = paste0(user.anal_path, independendVar, "/tables/heatmap_matrix/")
    create_dir(out_dir)
    
    if (indVar.factor.num > 1) {
      mat.filename <- paste0(depvarqueryVar, ".", independendVar, ".", base_param_concat,  ".", param_comps_concat[i])
    } else {
      mat.filename <- paste0(depvarqueryVar, ".", independendVar)
    }
    
    mat.filename <- gsub(" ", "", mat.filename, fixed=TRUE)
    mat.filename <- gsub("\\(", "", mat.filename)
    mat.filename <- gsub("\\)", "", mat.filename)
    
    
    mat_all.fp = paste0(out_dir, mat.filename, "matrix_all.csv")
    mat_sig.fp = paste0(out_dir, mat.filename, "matrix_sig.csv")
    
    mat_all.fp = append_filename(mat_all.fp)
    mat_sig.fp = append_filename(mat_sig.fp)
    
    write.csv(mat_all, mat_all.fp)
    out_filenames = c(out_filenames, mat_all.fp)
    write.csv(mat_sig, mat_sig.fp)
    out_filenames = c(out_filenames, mat_sig.fp)
    
    cat("2D heatmap matrices: \n", mat_all.fp, "\n", mat_sig.fp, "\n") 
    
    #####Create graph
    ######Apply color key 
    
    val_vec = c(abs(min(mat_all)), max(mat_all))
    max_val = sort(val_vec)[2]
    min_val = (max_val * -1)
    
    col_palette <- colorRampPalette(c("blue", "white", "red"))(n = 299)
    shades = c(seq(min_val,-0.01,length=100), seq(-0.0099, 0.0099, length = 100), seq(0.01,max_val,length=100))
    
    ####Export plots
    out_dir = paste0(user.anal_path, independendVar, "/heatmap_plot/")
    create_dir(out_dir)
    
    plot_all.fp = paste0(out_dir, mat.filename, ".heatmap.all.png")
    plot_sig.fp = paste0(out_dir, mat.filename, ".heatmap.sig.png")
    
    plot_all.fp = append_filename(plot_all.fp)
    out_filenames = c(out_filenames, plot_all.fp)
    plot_sig.fp = append_filename(plot_sig.fp)
    out_filenames = c(out_filenames, plot_sig.fp)
    
    #Output Plot for all vals
    png(plot_all.fp,
        width = 5*300,        # 5 x 300 pixels
        height = 5*300,
        res = 300,            # 300 pixels per incht
        pointsize = 8)
    
    heatmap.2(mat_all, Rowv = F, Colv = "Rowv", dendrogram = 'none', 
              col=col_palette, trace = 'none', srtRow = 45, srtCol = 45, 
              density.info = 'none', breaks = shades, na.color = "White", 
              key=T, cexRow = 0.75, cexCol=0.75, keysize=0.75, key.par = list(cex=0.5),
              main = paste(param_comps[i], "all"), symkey=F)
    
    dev.off() 
    
    #Output Plot for Sig vals
    png(plot_sig.fp,
        width = 5*300,        # 5 x 300 pixels
        height = 5*300,
        res = 300,            # 300 pixels per incht
        pointsize = 8)
    
    heatmap.2(mat_sig, Rowv = F, Colv = "Rowv", dendrogram = 'none', 
              col=col_palette, trace = 'none', srtRow = 45, srtCol = 45, 
              density.info = 'none', breaks = shades, na.color = "White", 
              key=T, cexRow = 0.75, cexCol=0.75, keysize=0.75, key.par = list(cex=0.5),
              main = paste(param_comps[i], "sig"), symkey=F)
    
    dev.off() 
    
    cat("\n2D heatmap plots: \n", plot_all.fp, "\n", plot_sig.fp, "\n\n")
  }
}

#### Create FS formatting for tables
if (cortheatmapVar == T) {
  
  #Create cortical overlay tables for each parameter
  for (i in 1:length(param_comps)) {
    data.FS = subset(data.coefs, parameter_comp == param_comps[i])
    data.FS = cbind(data.FS, p_val_neglog = -log10(data.FS$`Pr(>|t|)`))
    
    #Find maximum value of magnitude
    beta_vec = sort(abs(data.FS$Estimate))
    max_val = beta_vec[length(beta_vec)]
    print(paste0("max val is: ", max_val))
    
    names_rows_fs <- str_sub(data.FS$dep_var, end = -4)
    names_rows_fs <- names_rows_fs[1:(length(names_rows_fs)/2)]
    names_rows_fs <- gsub("^.*_","", names_rows_fs) #Assumes name is last word before underscore
    
    #Create fs tables
    df_lh_Beta = as.data.frame(data.FS[grep(".lh", data.FS$dep_var), which("Estimate" == colnames(data.FS)), drop = FALSE])
    row.names(df_lh_Beta) = names_rows_fs
    
    df_rh_Beta = as.data.frame(data.FS[grep(".rh$", data.FS$dep_var), which("Estimate" == colnames(data.FS)), drop = FALSE])
    row.names(df_rh_Beta) = names_rows_fs
    
    df_lh_pval = as.data.frame(data.FS[grep(".lh", data.FS$dep_var), which("p_val_neglog" == colnames(data.FS)), drop = FALSE])
    row.names(df_lh_pval) = names_rows_fs
    
    df_rh_pval = as.data.frame(data.FS[grep(".rh$", data.FS$dep_var), which("p_val_neglog" == colnames(data.FS)), drop = FALSE])
    row.names(df_rh_pval) = names_rows_fs
    
    out_dir = paste0(user.anal_path, independendVar, "/tables/table_fs/")
    create_dir(out_dir)
    
    #Add additional information in filename if  multiple parameter types
    if (indVar.factor.num > 1) {
      cat("Exposure parameter: ", param_comps_concat[i], "\n")
      data.fs_name <- paste0(depvarqueryVar, ".", independendVar, ".", base_param_concat,  ".", param_comps_concat[i])
    } else {
      data.fs_name <- paste0(depvarqueryVar, ".", independendVar)
    }
    
    data.fs_name = gsub(" ", "", data.fs_name, fixed=TRUE)
    data.fs_name = gsub("\\(", "", data.fs_name)
    data.fs_name = gsub("\\)", "", data.fs_name)
    
    #Export tables
    df_lh_Beta_name = paste(out_dir, data.fs_name, ".lh.Beta", '.txt', sep = "")
    df_lh_Beta_name = append_filename(df_lh_Beta_name)
    
    df_rh_Beta_name = paste(out_dir, data.fs_name, ".rh.Beta", '.txt', sep = "")
    df_rh_Beta_name = append_filename(df_rh_Beta_name)
    
    df_lh_pval_name = paste(out_dir, data.fs_name, ".lh.pval", '.txt', sep = "") 
    df_lh_pval_name = append_filename(df_lh_pval_name)
    
    df_rh_pval_name = paste(out_dir, data.fs_name, ".rh.pval", '.txt', sep = "")
    df_rh_pval_name = append_filename(df_rh_pval_name)
    
    write.table(df_lh_Beta, df_lh_Beta_name, sep="\t", quote=FALSE)
    out_filenames = c(out_filenames, df_lh_Beta_name)
    write.table(df_rh_Beta, df_rh_Beta_name, sep="\t", quote=FALSE)
    out_filenames = c(out_filenames, df_rh_Beta_name)
    write.table(df_lh_pval, df_lh_pval_name, sep="\t", quote=FALSE)
    out_filenames = c(out_filenames, df_lh_pval_name)
    write.table(df_rh_pval, df_rh_pval_name, sep="\t", quote=FALSE)
    out_filenames = c(out_filenames, df_rh_pval_name)
    
    #cat("FS formatted tables: \n", df_lh_Beta_name, "\n", df_rh_Beta_name, "\n", df_lh_pval_name, "\n", df_rh_pval_name, "\n\n")
    
    #Print out freeview command
    fs_command = paste0(user.script_path, "/fs_mat3D.sh ", df_lh_Beta_name, " ", max_val, " ", cortmaskedVar, " ", cortssVar)
    out_filenames = c(out_filenames, fs_command)
    cat("FREEVIEW COMMAND: \n", fs_command, "\n\n")
  }
}

#### Create log of all analyses
out_dir = paste0(user.anal_path, independendVar, "/logs/")
create_dir(out_dir)

log.fp = paste(out_dir, depvarqueryVar, ".", independendVar, ".txt", sep = "")
log.fp = append_filename(log.fp)

cat(capture.output(print(c(inputs, out_filenames)), file=log.fp))

cat("log file: ", log.fp, "\n\n")

