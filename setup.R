
# This script sets up a few parameters needed for the pipeline

# Setting up the years of analysis
years_analysis_setup <- seq(1971, 2020, 1)

# Setting up the filepath to datasets
path_to_datasets_setup <- "PATH_TO_DATASETS/"

# Setting up the filepath to inst
# In principle this one does not need to be changed
path_to_inst_setup <- "inst/"

# IEA WEEB version
iea_weeb_file_setup <- "IEA/WEEBs/IEA Extended Energy Balances 2022 (TJ).csv" #  2022 version in TJ
# iea_weeb_file_setup <- "Extended-Energy-Balances-2021-full-ktoe.csv" # 2021 version in ktoe

# IEA WEEB units
iea_weeb_units_setup <- "TJ"
# iea_weeb_units_setup <- "ktoe"

# Exiobase v3.2
path_to_exiobase_setup <- "Exiobase/Exiobase_3_8_2/"

# GPFU database psut mats pin name
gpfu_psut_file_setup <- "GPFU_database/OutputData/PipelineReleases/psut/20231207T124854Z-73744" # Version 1.2
