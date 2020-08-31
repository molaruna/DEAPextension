To use the DEAPext.R tool, you'll need to make several downloads, installations and changes to your environment. 

Requirements: Step 1 - Download DEAPext repo
Directory: Google Drive/LPN/Projects/ABCD/code/DEAPext_package
Download the most recent .RDS file
Place this file in the same directory as your github repository download
Give yourself permission to execute the scripts:
- In terminal, navigate into your parent directory
- Type in this command: chmod 777 *

Requirements: Step 2 - Installation Software
Install R: https://www.r-project.org/
Install Freesurfer: https://surfer.nmr.mgh.harvard.edu/fswiki/DownloadAndInstall

Requirements: Step 3 - Set your FREESURFER_HOME path variable

Example: 
   vim ~/.bashrc #open your bashrc file

#Freesurfer path
   export FREESURFER_HOME=/Applications/freesurfer
   export PATH=$FREESURFER_HOME/bin:$PATH
   :qc #save and exit 

   source ~/.bashrc #update settings


More information about DEAPext: 
DEAPext is a command-line executable tool that incorporates DEAP code to automatically run multiple multivariate linear regressions. It can be used to carry out fixed or mixed regressions, with modifiable features such as: the dependent variables queried, independent variable, covariates, z-scoring, baseline regression parameter option, ability to add an additional variable, filtering for mrif and QC scores. This tool allows contains the ability to create 2D or 3D cortical projection heatmaps with the effects of the regressions, with the option of masking the effects for p-val<0.05. 2D heatmaps are created using the Gordon rsfmri network atlas, and 3D heatmaps are created using the Desikan cortical atlas. 

Outputs include: 
(1) input data for regression
(2) coefficient table
(3) anova table
(4) 2D matrices and heatmaps
(5) FS-formatted text files
(5) Command to run FS for 3D cortical heatmap
(6) log of all analysis inputs and filepaths 

Organizational structure:
Files are located in _your_ working directory, within the analysis directory that you name
The script will append a 1 to your filename instead of overwriting the file

For more information, use the help function: 
./DEAPext.R -h

Examples for certain partially matched dependent variable strings:
CT = "smri_thick_cort.desikan"
CA = "smri_area_cort.desikan"
CV = "smri_vol_cort.desikan"
nih toolbox scores = "corrected"
rsfmri_desikan = "rsfmri_var_cort.desikan"
rsfmri_gordon = "^rsfmri_cor_network\\.gordon.*\\.gordon.*"
dti_desikan = "dmri_dti.fa.wm_cort.desikan"
dti_fiber = "dmri_dti.fa_fiber"
tfmri_mid_rewardfeedback_desikan = "tfmri_mid_all_reward.pos.vs.neg.feedback_beta_cort.desikan"
tfmri_sst_stopgo_desikan = "tfmri_sst_all_correct.stop.vs.correct.go_beta_cort.desikan"

Random effect options that this tool allows:
mri_info_deviceserialnumber 
abcd_site
src_subject_id
rel_family_id

Fixed effect options that this tool allows: Any variable within the DEAP dataset (or additional variables that are manually added by you) 

# Author: maria.olaru@ucsf.edu





