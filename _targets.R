library(targets)

# Sourcing scripts containing functions
source("Functions/Global_EROIs/global_funcs.R")
source("Functions/Global_EROIs/aggregate_eu_fds.R")
source("Functions/Global_EROIs/idE_calcs.R")
source("Functions/Global_EROIs/dE_idE_breakdown.R")
source("Functions/National_EROIs/national_erois.R")
source("Functions/Efficiencies/eta_funcs.R")
source("Functions/Efficiencies/efficiencies_by_group.R")
source("Functions/Efficiencies/electrification.R")
source("Functions/EROI_equivalents/eroi_equivalents.R")

# Sourcing scripts containing pipeline parameters
source("setup.R")


# Specific options for workflow 
# 1) Packages we want to load
# 2) Method for errors (target will return NULL if errored)
tar_option_set(packages = c("magrittr", "ECCTools", "dplyr", "Matrix"),
               error = "stop")


# Building list of targets
list(
  
  #### (0) Setting up analysis ####
  # Values read from setup.R script
  
  # 1) Years of analysis
  tar_target(
    years_analysis,
    return(years_analysis_setup)
  ),
  # 2) Path to datasets
  tar_target(
    path_to_datasets,
    return(path_to_datasets_setup)
  ),
  # 3) Path to light data
  tar_target(
    path_to_inst,
    return(path_to_inst_setup)
  ),
  # 4) IEA WEEBs file
  tar_target(
    iea_weeb_file,
    return(iea_weeb_file_setup)
  ),
  # 5) IEA WEEBs units
  tar_target(
    iea_weeb_units,
    return(iea_weeb_units_setup)
  ),
  # 6) GPFU PSUT file
  tar_target(
    gpfu_psut_file,
    return(gpfu_psut_file_setup)
  ),
  # 7) Exiobase directory
  tar_target(
    path_to_exiobase,
    return(path_to_exiobase_setup)
  ),
  
  
  #### (1) DATA TARGETS ####
  
  # 1.1: IEA data file
  tar_target(
    iea_weeb_data_file,
    paste0(path_to_datasets, iea_weeb_file),
    format = "file"
  ),
  # 1.2: Amending data file for World level analysis (fixing China and USSR)
  tar_target(
    iea_global_amending_data_file,
    select_iea_global_fixing_file(path_to_inst = path_to_inst,
                                  iea_weeb_energy_units = iea_weeb_units_setup),
    format = "file"
  ),
  # 1.3: PSUT workflow outcome data
  tar_target(
    psut_workflow_outcome_data_file,
    paste0(path_to_datasets, gpfu_psut_file, "/psut.rds"),
    format = "file"
  ),
  # 1.4: Final-to-useful conversion machine (industry) to end-use category data
  tar_target(
    machine_to_end_use_category_file,
    paste0(path_to_inst, "EU_FDS_Aggregation/Machine_to_EU_concordance_EA.csv"),
    format = "file"
  ),
  # 1.5: Final demand sector to final demand sector category concordance data
  tar_target(
    fds_to_fds_category_file,
    paste0(path_to_inst, "EU_FDS_Aggregation/Fds_concordance.csv"),
    format = "file"
  ),
  # 1.6: Regional aggregation table data
  tar_target(
    regional_aggregation_table_file,
    paste0(path_to_inst, "Country_Aggregation/regional_aggregation_table.csv"),
    format = "file"
  ),
  # 1.7: Path to Exiobase tables
  tar_target(
    path_to_exiobase_dataset,
    paste0(path_to_datasets, path_to_exiobase)
  ),
  # 1.8: Machine electrification concordance table
  tar_target(
    path_to_machine_electrification_concordance,
    paste0(path_to_inst, "Substitution_Efficiencies/Machine_electrification_concordance.csv"),
    format = "file"
  ),
  # 1.9: Machine electrification backstop efficiencies
  tar_target(
    path_to_machine_elec_backstop_efficiencies,
    paste0(path_to_inst, "Substitution_Efficiencies/Electrification_backstop_efficiencies.csv"),
    format = "file"
  ),
  # 1.10: Machine electrification concordance table, heat pump scenario
  tar_target(
    path_to_machine_HP_scenario_concordance,
    paste0(path_to_inst, "Substitution_Efficiencies/Machine_HP_scenario.csv"),
    format = "file"
  ),

  #### (2) LOADING INPUT DATA ####
  
  # 2.1: Loading raw iea data. No changes made here.
  tar_target(
    iea_weeb_raw_data,
    load_raw_iea_data(iea_weeb_data_file,
                      years = years_analysis,
                      path_to_inst = path_to_inst,
                      energy_units = iea_weeb_units)
  ),
  # 2.2: Loading amending data file
  tar_target(
    amending_iea_df,
    load_amending_data(iea_global_amending_data_file,
                       years = years_analysis)
  ),
  # 2.3: Loading PSUT output data file
  # Keep only energy type data, and Year >= 1971
  tar_target(
    PFU_output_data,
    load_PFU_output_data(psut_workflow_outcome_data_file,
                         years = years_analysis)
  ),
  # 2.4: Expand PSUT output data with more needed matrices
  tar_target(
    PFU_output_data_expanded,
    expand_PFU_output(PFU_output_data,
                      years = years_analysis)
  ),
  # 2.5: Loading machine to end-use concordance table
  # Rename machine column so that joins by Industry can be done easily
  tar_target(
    machine_to_end_use_category_data,
    load_machine_to_end_use_category_data(machine_to_end_use_category_file)
  ),
  # 2.6: Loading final demand sector to final demand sector category concordance table
  tar_target(
    fds_to_fds_category_data,
    load_fds_to_fds_category_data(fds_to_fds_category_file)
  ),
  # 2.7: Reading regional aggregation table
  tar_target(
    regional_aggregation_table_data,
    prepare_regional_aggregation_table(regional_aggregation_table_file,
                                       path_to_inst = path_to_inst)
  ),
  # 2.8: Loading machine electrification concordance
  tar_target(
    machine_electrification_concordance_data,
    load_machine_electrification_concordance(path_to_machine_electrification_concordance)
  ),
  # 2.9: Loading machine backstop efficiencies (for electrification)
  tar_target(
    machine_backstop_efficiencies_data,
    load_machine_backstop_efficiencies(path_to_machine_elec_backstop_efficiencies,
                                       years = years_analysis)
  ),
  # 2.10: Loading machine electrification concordance
  tar_target(
    machine_HP_scenario_data,
    load_machine_electrification_concordance(path_to_machine_HP_scenario_concordance)
  ),
  
  #### (3) SHARES OF PRODUCT USE BY COUNTRY, FD SECTOR, END USE ####
  
  # 3.1: Specifying regional IEA data, excluding world, and excluding any double accounting region
  tar_target(
    specified_regional_iea_data,
    specify_regional_iea_data(iea_weeb_raw_data)
  ),
  # 3.2: Routing Stock Changes and Statistical Differences to balancing matrix
  # Likewise for losses, exports and non-energy uses. Losses only works is flow is kept as "Losses".
  tar_target(
    specified_regional_iea_data_adapted_Y_for_shares,
    adapt_Y_to_calc_shares(specified_regional_iea_data)
  ),
  # 3.3: Building a data frame with the shares of product use by location
  # So, when summing across countries, the sum should give unity.
  tar_target(
    share_use_product_by_country,
    calc_share_product_use_by_country(specified_regional_iea_data_adapted_Y_for_shares)
  ),
  # 3.4: Building a data frame with the share of each product use within each end-use by country
  # So, when summing across countries for a single product and single end-use, shares must add up to unity.
  tar_target(
    share_use_by_end_use_by_country,
    calc_share_product_use_by_end_use_by_country(
      PSUT_mats_df = PFU_output_data,
      machine_to_end_use_df = machine_to_end_use_category_data
    )
  ),
  # 3.5: Building a data frame with the share of each product use within each final demand sector by country
  # So, when summing across countries for a single product and single final demand sector, shares must add up to unity.
  tar_target(
    share_use_by_fds_by_country,
    calc_share_product_use_by_fds_by_country(
      tidy_specified_iea_data = specified_regional_iea_data_adapted_Y_for_shares,
      fds_to_fds_category_df = fds_to_fds_category_data
    )
  ),
  # 3.6: Building a data frame with the shares of each fossil fuel GROUP use by location
  # So, when summing across countries, the sum should give unity.
  tar_target(
    share_ff_group_use_by_country,
    calc_share_ff_group_use_by_country(specified_regional_iea_data_adapted_Y_for_shares)
  ),
  # 3.7: Building a data frame with the share of each fossil fuel GROUP use within each end-use by country
  # So, when summing across countries for a single fossil fuel group and single end-use, shares must add up to unity.
  tar_target(
    share_ff_group_use_by_end_use_by_country,
    calc_share_ff_group_use_by_end_use_by_country(
      PSUT_mats_df = PFU_output_data,
      tidy_iea_df = specified_regional_iea_data_adapted_Y_for_shares,
      machine_to_end_use_df = machine_to_end_use_category_data
    )
  ),
  # 3.8: Building a data frame with the share of each fossil fuel group use within each final demand sector by country
  # So, when summing across countries for a single fossil fuel group and single final demand sector, shares must add up to unity.
  tar_target(
    share_ff_group_use_by_fds_by_country,
    calc_share_ff_group_use_by_fds_by_country(
      tidy_specified_iea_data = specified_regional_iea_data_adapted_Y_for_shares,
      fds_to_fds_category_df = fds_to_fds_category_data
    )
  ),
  
  
  #### (4) EFFICIENCIES CALCULATIONS ####
  
  # 4.1: Pulls out tidy FU efficiencies by industry, end-use, and country:
  tar_target(
    tidy_fu_efficiencies_by_machine_country,
    calc_tidy_efficiencies(PFU_output_data_expanded)
  ),
  # 4.2: Pulls out tidy shares of use (D_rev) by product, end-use, and country:
  tar_target(
    tidy_D_rev,
    extract_tidy_D_rev(PFU_output_data_expanded)
  ),
  # 4.3: Pulls out tidy shares of product p inputs in industry i inputs, by country:
  tar_target(
    tidy_C_rev,
    extract_tidy_C_rev(PFU_output_data_expanded)
  ),
  
  ### GLOBAL AVERAGE WITHOUT BREAKDOWN ###
  # 4.4 : Finds average national final-to-useful efficiencies for each country (and product):
  tar_target(
    FU_efficiencies_avg_by_country,
    calc_avg_efficiencies_by_country(tidy_fu_efficiencies_by_machine_country,
                                     tidy_D_rev)
  ),
  # 4.5: Calculates global average final-to-useful efficiency for each product
  # Use average national final-to-useful efficiencies and applies the share of product use by country
  tar_target(
    FU_efficiencies_avg_global,
    calc_avg_global_FU_efficiencies(FU_efficiencies_avg_by_country,
                                    share_use_product_by_country)
  ),
  
  ### GLOBAL AVERAGE BREAKDOWN BY END-USE ###
  # 4.6: Calculates average national final-to-useful efficiencies within each end-use (for each product)
  tar_target(
    FU_efficiencies_avg_by_end_use_by_country,
    calc_avg_efficiencies_by_end_use_by_country(tidy_fu_efficiencies_by_machine_country,
                                                tidy_D_rev,
                                                machine_to_end_use_category_data)
  ),
  # 4.7: Calculates average global final-to-useful efficiencies within each end-use (for each product)
  tar_target(
    global_avg_FU_efficiencies_by_end_use,
    calc_global_avg_efficiencies_by_end_use(FU_efficiencies_avg_by_end_use_by_country,
                                            share_use_by_end_use_by_country)
  ),
  
  ### GLOBAL AVERAGE BREAKDOWN BY FINAL DEMAND SECTOR ###
  
  # 4.8: Calculates the share of use of each product by each machine, grouped by final demand sector category, at the national level
  tar_target(
    share_national_use_product_by_machine_by_fds,
    calc_share_national_use_product_by_machine_by_fds(PFU_output_data_expanded,
                                                      tidy_C_rev,
                                                      fds_to_fds_category_data)
  ),
  # 4.9: Calculates average national final-to-useful efficiencies within each final demand sector (for each product)
  tar_target(
    FU_efficiencies_avg_by_fds_by_country,
    calc_avg_efficiencies_by_fds_by_country(share_national_use_product_by_machine_by_fds,
                                            tidy_fu_efficiencies_by_machine_country)
  ),
  # 4.10: Calculates average global final-to-useful efficiencies within each final demand sector (for each product)
  tar_target(
    global_avg_FU_efficiencies_by_fds,
    calc_global_avg_efficiencies_by_fds(share_use_by_fds_by_country,
                                        FU_efficiencies_avg_by_fds_by_country)
  ),
  
  #### (5) Specifying electricity and heat in efficiency data frames ####
  # So that the names are compatible with EROI calculations
  # For instance Electricity will become "Electricity [from Coal products]", etc.
  
  # 5.1: Specifies electricity and heat in the global efficiencies df, without breakdown
  tar_target(
    FU_efficiencies_avg_global_elect_heat_specified,
    add_elect_heat_to_efficiencies(FU_efficiencies_avg_global)
  ),
  # 5.2: Specifies electricity and heat in the global efficiencies df, breakdown by end-use
  tar_target(
    global_avg_FU_efficiencies_by_end_use_elect_heat_specified,
    add_elect_heat_to_efficiencies(global_avg_FU_efficiencies_by_end_use)
  ),
  # 5.3: Specifies electricity and heat in the global efficiencies df, breakdown by final demand sector
  tar_target(
    global_avg_FU_efficiencies_by_fds_elect_heat_specified,
    add_elect_heat_to_efficiencies(global_avg_FU_efficiencies_by_fds)
  ),
  # 5.4: Specifies electricity and heat in the national efficiencies data frame, without breakdown
  tar_target(
    FU_efficiencies_avg_by_country_elect_heat_specified,
    add_elect_heat_to_efficiencies(FU_efficiencies_avg_by_country)
  ),
  # 5.5: Specified electricity and heat in the national efficiencies data frame, breakdown by end-use category
  tar_target(
    FU_efficiencies_avg_by_end_use_by_country_elect_heat_specified,
    add_elect_heat_to_efficiencies(FU_efficiencies_avg_by_end_use_by_country)
  ),
  # 5.6: Specified electricity and heat in the national efficiencies data frame, breakdown by final demand sector category
  tar_target(
    FU_efficiencies_avg_by_fds_by_country_elect_heat_specified,
    add_elect_heat_to_efficiencies(FU_efficiencies_avg_by_fds_by_country)
  ),


  #### (6) Workflow for global EROIs ####
  
  # 6.1: Specifying all global level data
  tar_target(
    specified_world_iea_data,
    specify_and_amend_world_data(iea_weeb_raw_data, amending_iea_df)
  ),
  # 6.2: Route stocks changes and statistical differences to balancing matrix
  tar_target(
    specified_world_iea_data_removed_SC_SD,
    route_SC_SD_to_balancing(specified_world_iea_data)
  ),
  # 6.3: Transform to Domestic Technology Assumption
  tar_target(
    specified_world_iea_data_DTA,
    transform_world_to_dta(specified_world_iea_data_removed_SC_SD)
  ),
  # 6.4: Build IO matrices
  tar_target(
    world_io_matrices_DTA,
    build_io_matrices(specified_world_iea_data_DTA)
  ),
  # 6.5: Calculates and extracts EROIs from IO calcs (so, both primary and final stage)
  # The EROIs that the Recca::calc_erois() function returns are product-level,
  # Meaning primary stage for primary energy products and final stage for final energy products
  tar_target(
    tidy_global_io_erois,
    calc_and_extract_erois(world_io_matrices_DTA,
                           eroi_method = "DTA")
  ),
  # 6.6: Push to global average useful stage EROIs without breakdown
  tar_target(
    tidy_global_useful_erois,
    push_to_global_useful_erois(tidy_global_io_erois,
                                FU_efficiencies_avg_global_elect_heat_specified,
                                Average_Efficiency_Col = "Average_Efficiency_Global")
  ),
  # 6.7: Aggregates EROIs without any end-use or final demand sector breakdown
  tar_target(
    aggregated_global_erois_without_breakdown,
    aggregate_global_erois_whout_breakdown(tidy_io_erois_df = tidy_global_io_erois,
                                           tidy_useful_erois_df = tidy_global_useful_erois,
                                           ecc_dta_df = specified_world_iea_data_DTA)
  ),

  ### PUSHING TO AND AGGREGATING EROIS WITH END-USE CATEGORY AND FINAL DEMAND SECTOR BREAKDOWN ###
  
  ### First, by end-use category ###
  # 6.8: Push to global average useful stage EROIs by end-use category
  tar_target(
    tidy_global_useful_erois_by_eu,
    push_to_global_useful_erois(tidy_global_io_erois,
                                global_avg_FU_efficiencies_by_end_use_elect_heat_specified,
                                Average_Efficiency_Col = "Global_Avg_Efficiency_By_End_Use")
  ),
  # 6.9: Determining the share of each energy product use, 
  # within each fossil fuel group, at each energy stage, and for each end-use category
  tar_target(
    tidy_global_shares_by_end_use,
    calc_global_shares_by_end_use(PFU_output_df = PFU_output_data,
                                  specified_world_iea_data_DTA_df = specified_world_iea_data_DTA,
                                  machine_to_end_use_category_df = machine_to_end_use_category_data,
                                  tidy_global_useful_erois_by_eu_df = tidy_global_useful_erois_by_eu,
                                  energy_units = iea_weeb_units)
  ),
  # 6.10: Aggregates useful stage EROIs with a breakdown by end-use category
  tar_target(
    aggregated_global_useful_erois_by_eu,
    aggregate_global_useful_erois_by_eu(tidy_global_useful_erois_by_eu_df = tidy_global_useful_erois_by_eu,
                                        tidy_global_shares_by_end_use_df = tidy_global_shares_by_end_use)
  ),
  
  ### Second, by final demand sector ###
  # 6.11: Push to global average useful stage EROIs by final demand sector
  tar_target(
    tidy_global_useful_erois_by_fds,
    push_to_global_useful_erois(tidy_global_io_erois,
                                global_avg_FU_efficiencies_by_fds_elect_heat_specified,
                                Average_Efficiency_Col = "Global_Average_Efficiency_By_FDSC")
  ),
  # 6.12: Determining the share of each energy product use, 
  # within each fossil fuel group, at each energy stage, and for final demand sector
  tar_target(
    tidy_global_shares_by_fds,
    calc_global_shares_by_fds(PFU_output_df = PFU_output_data,
                              specified_world_iea_data_DTA_df = specified_world_iea_data_DTA,
                              fds_to_fds_category_df = fds_to_fds_category_data,
                              tidy_global_useful_erois_by_fds_df = tidy_global_useful_erois_by_fds,
                              energy_units = iea_weeb_units)
  ),
  # 6.13: Aggregates useful stage EROIs with a breakdown by final demand sector
  tar_target(
    aggregated_global_useful_erois_by_fds,
    aggregate_global_useful_erois_by_fds(tidy_global_useful_erois_by_fds_df = tidy_global_useful_erois_by_fds,
                                         tidy_global_shares_by_fds_df = tidy_global_shares_by_fds)
  ),
  
  
  #### (7) Workflow for national EROIs using the GMA ####
  
  # 7.1: Aggregates the specified regional IEA data frame according to the regional aggregation table provided
  # Includes sending statistical differences and stock changes to balancing matrix.
  # Also gets rid of fuel gasoline by converting it into motor gasoline (issue is that it is imported and used by some countries whereas no country produces it in the period 2013-2019!)
  tar_target(
    specified_aggregated_regional_iea_data,
    aggregate_specified_regional_iea_data(specified_regional_iea_data,
                                          regional_aggregation_table_data,
                                          aggregate = FALSE)
  ),
  # 7.2: Transforms to Global Market Assumption
  tar_target(
    specified_aggregated_iea_data_gma,
    transforms_regional_data_to_gma(specified_aggregated_regional_iea_data)
  ),
  # 7.3: Builds PSUT matrices
  tar_target(
    PSUT_mats_gma,
    prepare_gma_psut_mats(specified_aggregated_iea_data_gma)
  ),
  
  # 7.4: Skipped; all code that calculated and stored IO mats.
  # Takes a lot of memory up while not needed. Now merges in 7.5.
  # If we want to run this one better to split it else expect a crash
  # tar_target(
  #   IO_mats_gma,
  #   calc_IO_mats_gma(PSUT_mats_gma,
  #                    from = 1971,
  #                    until = 2019)
  # ),
  
  # 7.5: Calculates from IO methods, tidy final stage EROIs for each country, so production perspective
  # So gives the EROI for a given product when PRODUCED in a given country
  tar_target(
    tidy_national_io_erois,
    calc_IO_mats_tidy_national_io_erois(PSUT_mats_gma,
                                        eroi_method = "GMA",
                                        from = min(years_analysis_setup),
                                        until = max(years_analysis_setup))
  ),
  
  
  
  # 7.6.a: Calculates tidy useful stage EROIs for each country
  # So gives the useful stage EROI when a given product (Product column;
  # location of production attached to product name, so each product is location-specified)
  # is used in a given country (Country column)
  tar_target(
    tidy_national_useful_erois,
    push_to_national_useful_erois(tidy_io_national_erois_df = tidy_national_io_erois,
                                  FU_national_efficiencies_df = FU_efficiencies_avg_by_country_elect_heat_specified,
                                  Average_Efficiency_Col = "Average_Efficiency_By_Country",
                                  years_analysis = years_analysis)
  ),
  # 7.7.a: Aggregates country level EROIs without breakdown
  # Primary stage
  tar_target(
    aggregated_national_primary_erois_without_breakdown,
    aggregate_national_primary_erois_whout_breakdown(tidy_io_erois_df = tidy_national_io_erois,
                                                     ecc_gma_df = specified_aggregated_iea_data_gma)
  ),
  # 7.7.b: Aggregates country level EROIs without breakdown
  # Final stage
  tar_target(
    aggregated_national_final_erois_without_breakdown,
    aggregate_national_final_erois_whout_breakdown(tidy_io_erois_df = tidy_national_io_erois,
                                                   ecc_gma_df = specified_aggregated_iea_data_gma)
  ),
  # 7.7.c: Aggregates country level EROIs without breakdown
  # Useful stage
  tar_target(
    aggregated_national_useful_erois_without_breakdown,
    aggregate_national_useful_erois_whout_breakdown(tidy_useful_erois_df = tidy_national_useful_erois,
                                                   ecc_gma_df = specified_aggregated_iea_data_gma)
  ),
  
  # 7.7.d: Aggregates country level EROIs without breakdown
  # Bind together primary, final, and useful stages
  tar_target(
    aggregated_national_erois_without_breakdown,
    aggregate_national_erois_whout_breakdown(aggregated_national_primary_erois_df = aggregated_national_primary_erois_without_breakdown,
                                             aggregated_national_final_erois_df = aggregated_national_final_erois_without_breakdown,
                                             aggregated_national_useful_erois_df = aggregated_national_useful_erois_without_breakdown)
  ),
  
  ### Calculations by end-use category ###
  # 7.8: Calculating tidy national useful stage EROIs by end-use category
  tar_target(
    tidy_national_useful_erois_by_eu,
    push_to_national_useful_erois(tidy_io_national_erois_df = tidy_national_io_erois,
                                  FU_national_efficiencies_df = FU_efficiencies_avg_by_end_use_by_country_elect_heat_specified,
                                  Average_Efficiency_Col = "Average_Efficiency_By_End_Use",
                                  years_analysis = years_analysis)
  ),
  # 7.9: Calculating tidy national shares of each product within each product group and end-use category, in each country
  tar_target(
    tidy_national_shares_by_end_use,
    calc_national_shares_by_end_use(PFU_output_df = PFU_output_data,
                                    ecc_gma_df = specified_aggregated_iea_data_gma,
                                    machine_to_end_use_category_df = machine_to_end_use_category_data,
                                    tidy_national_useful_erois_by_eu_df = tidy_national_useful_erois_by_eu,
                                    energy_units = iea_weeb_units)
  ),
  # 7.10: Aggregates country level EROIs by end-use category
  tar_target(
    aggregated_national_useful_erois_by_eu,
    aggregate_national_useful_erois_by_eu(tidy_national_useful_erois_by_eu_df = tidy_national_useful_erois_by_eu,
                                          tidy_national_shares_by_end_use_df = tidy_national_shares_by_end_use)
  ),
  
  
  ### Calculations by final demand sector category ###
  # 7.11: Calculating tidy national useful stage EROIs by final demand sector
  tar_target(
    tidy_national_useful_erois_by_fds,
    push_to_national_useful_erois(tidy_io_national_erois_df = tidy_national_io_erois,
                                  FU_national_efficiencies_df = FU_efficiencies_avg_by_fds_by_country_elect_heat_specified,
                                  Average_Efficiency_Col = "Average_Efficiency_By_FDSC_By_Country",
                                  years_analysis = years_analysis)
  ),
  # 7.12: Calculating tidy national shares of each product within each product group and final demand sector, in each country
  tar_target(
    tidy_national_shares_by_fds,
    calc_national_shares_by_fds(PFU_output_df = PFU_output_data,
                                ecc_gma_df = specified_aggregated_iea_data_gma,
                                fds_to_fds_category_df = fds_to_fds_category_data,
                                tidy_national_useful_erois_by_fds_df = tidy_national_useful_erois_by_fds,
                                years_analysis = years_analysis,
                                energy_units = iea_weeb_units)
  ),
  # 7.13: Aggregates country level EROIs by final demand sector category
  tar_target(
    aggregated_national_useful_erois_by_fds,
    aggregate_national_useful_erois_by_fds(tidy_national_useful_erois_by_fds_df = tidy_national_useful_erois_by_fds,
                                           tidy_national_shares_by_fds)
  ),
  
  #### (8) Workflow to determine aggregated fossil fuel efficiencies ####
  
  # 8.1: Globally, without breakdown:
  # Here we directly use adapted efficiencies, excluding some uses of electricity, because they are only used for the calculation of EROI equivalents
  tar_target(
    FF_global_aggregated_efficiencies,
    calc_FF_global_aggregated_efficiencies(FU_efficiencies_avg_global_elect_heat_specified,
                                           specified_world_iea_data_DTA,
                                           Average_Efficiency_Col = "Average_Efficiency_Global")
  ),
  # 8.2: Globally, breakdown by end-use
  tar_target(
    FF_global_aggregated_efficiencies_by_end_use,
    calc_FF_global_aggregated_efficiencies_by_end_use(global_avg_FU_efficiencies_by_end_use_elect_heat_specified,
                                                      tidy_global_shares_by_end_use)
  ),
  # 8.3: Globally, breakdown by final demand sector
  tar_target(
    FF_global_aggregated_efficiencies_by_fds,
    calc_FF_global_aggregated_efficiencies_by_fds(global_avg_FU_efficiencies_by_fds_elect_heat_specified,
                                                  tidy_global_shares_by_fds)
  ),
  # 8.4: Nationally, without breakdown
  # Here we directly use adapted efficiencies, excluding some uses of electricity, because they are only used for the calculation of EROI equivalents
  tar_target(
    FF_national_aggregated_efficiencies,
    calc_FF_national_aggregated_efficiencies(FU_efficiencies_avg_by_country_elect_heat_specified,
                                             specified_aggregated_iea_data_gma)
  ),
  # 8.5: Nationally, breakdown by end-use
  tar_target(
    FF_national_aggregated_efficiencies_by_end_use,
    calc_FF_national_aggregated_efficiencies_by_end_use(FU_efficiencies_avg_by_end_use_by_country_elect_heat_specified,
                                                        tidy_national_shares_by_end_use)
  ),
  # 8.6: Nationally, breakdown by final demand sector
  tar_target(
    FF_national_aggregated_efficiencies_by_fds,
    calc_FF_national_aggregated_efficiencies_by_fds(FU_efficiencies_avg_by_fds_by_country_elect_heat_specified,
                                                    tidy_national_shares_by_fds)
  ),
  
  #### (9) Indirect energy calculations ####
  
  # 9.1: Generating Exiobase final energy extension vectors from Exiobase dataset
  # a) For industries
  tar_target(
    exiobase_energy_extension_vectors,
    generate_energy_extension_vectors(path_to_exiobase_dataset)
  ),
  # b) For final demand sectors
  tar_target(
    exiobase_fds_energy_extension_vectors,
    generate_fds_energy_extension_vectors(path_to_exiobase_dataset)
  ),
  
  # 9.2.a:: Calculating indirect energy by industry (IO), this takes AGES
  # Both underestimation and overestimation method
  tar_target(
    idE_by_industry,
    calc_idE_by_industry(path_to_exiobase_dataset,
                         exiobase_energy_extension_vectors),
    # Careful, here we kill the recalculation if upstream arguments are modified.
    # I do so to avoid recalculation when updating the path to the dataset as calculation time is long.
    cue = tar_cue(
      depend = FALSE
    )
  ),
  # 9.2.b: Adding average as a method in the calculation of idE by industry
  tar_target(
    idE_by_industry_avg_added,
    add_avg_to_idE_by_industry(idE_by_industry)
  ),
  # 9.3.a: Adding indirect energy to global EROIs, without breakdown
  tar_target(
    aggregated_global_erois_without_breakdown_idE,
    add_idE_to_global_erois(aggregated_global_erois_df = aggregated_global_erois_without_breakdown,
                            idE_by_industry_df = idE_by_industry_avg_added,
                            tidy_world_iea_df = specified_world_iea_data_DTA,
                            energy_units = iea_weeb_units_setup)
  ),
  # 9.4: Adding indirect energy to global EROIs, breakdown by end-use category
  tar_target(
    aggregated_global_erois_by_eu_idE,
    add_idE_to_global_erois_by(aggregated_global_erois_by_df = aggregated_global_useful_erois_by_eu,
                               aggregated_global_erois_no_breakdown_df = aggregated_global_erois_without_breakdown,
                               idE_by_industry_df = idE_by_industry_avg_added,
                               tidy_world_iea_df = specified_world_iea_data_DTA,
                               by = "EU_category",
                               energy_units = iea_weeb_units_setup)
  ),
  # 9.5: Adding indirect energy to global EROIs, breakdown by final demand sector
  tar_target(
    aggregated_global_erois_by_fds_idE,
    add_idE_to_global_erois_by(aggregated_global_erois_by_df = aggregated_global_useful_erois_by_fds,
                               aggregated_global_erois_no_breakdown_df = aggregated_global_erois_without_breakdown,
                               idE_by_industry_df = idE_by_industry_avg_added,
                               tidy_world_iea_df = specified_world_iea_data_DTA,
                               by = "Final_Demand_Sector_Category",
                               energy_units = iea_weeb_units_setup)
  ),
  # 9.6.a: Adding indirect energy to national EROIs, without breakdown
  tar_target(
    aggregated_national_erois_without_breakdown_idE,
    add_idE_to_national_erois(aggregated_national_erois_df = aggregated_national_erois_without_breakdown,
                              idE_by_industry_df = idE_by_industry_avg_added,
                              tidy_world_iea_df = specified_world_iea_data_DTA,
                              energy_units = iea_weeb_units_setup)
  ),
  # 9.7: Adding indirect energy to national EROIs, breakdown by end-use category
  tar_target(
    aggregated_national_useful_erois_by_eu_idE,
    add_idE_to_national_erois_by(aggregated_national_erois_by_df = aggregated_national_useful_erois_by_eu,
                                 aggregated_national_erois_no_breakdown_df = aggregated_national_erois_without_breakdown,
                                 idE_by_industry_df = idE_by_industry_avg_added,
                                 tidy_world_iea_df = specified_world_iea_data_DTA,
                                 by = "EU_category",
                                 energy_units = iea_weeb_units_setup)
  ),
  # 9.8: Adding indirect energy to national EROIs, breakdown by final demand sector
  tar_target(
    aggregated_national_useful_erois_by_fds_idE,
    add_idE_to_national_erois_by(aggregated_national_erois_by_df = aggregated_national_useful_erois_by_fds,
                                 aggregated_national_erois_no_breakdown_df = aggregated_national_erois_without_breakdown,
                                 idE_by_industry_df = idE_by_industry_avg_added,
                                 tidy_world_iea_df = specified_world_iea_data_DTA,
                                 by = "Final_Demand_Sector_Category",
                                 energy_units = iea_weeb_units_setup)
  ),
  
  
  #### (10) Determining electricity efficiencies to substitute fossil fuels ####

  # 10.1: Gives the elec-machine that would replace each machine for each (Country, Year), as well as its efficiency
  tar_target(
    machine_substitution_efficiencies,
    collate_machine_substitution_efficiencies(tidy_fu_efficiencies_by_machine_country_df = tidy_fu_efficiencies_by_machine_country,
                                              machine_electrification_concordance_df = machine_electrification_concordance_data,
                                              machine_backstop_efficiencies_df = machine_backstop_efficiencies_data)
  ),
  # 10.2: Gives the elec-machine that would replace each machine for each (Country, Year), as well as its efficiency
  tar_target(
    machine_substitution_efficiencies_HP_scenario,
    collate_machine_substitution_efficiencies(tidy_fu_efficiencies_by_machine_country_df = tidy_fu_efficiencies_by_machine_country,
                                              machine_electrification_concordance_df = machine_HP_scenario_data,
                                              machine_backstop_efficiencies_df = machine_backstop_efficiencies_data)
  ),
  
  ### ECONOMY-WIDE LEVEL ###
  
  # 10.3: Determines the share of use of each fossil fuel group by machine, at the national level
  tar_target(
    share_use_ff_by_machine_national,
    calc_share_ff_by_machine_national(specified_regional_iea_data_adapted_Y_for_shares,
                                      tidy_D_rev)
  ),
  # 10.4: Determines the share of use of each fossil fuel group by machine, at the global level
  # This function seems to work once we debug 10.2.
  tar_target(
    share_use_ff_by_machine_global,
    calc_share_ff_by_machine_global(share_ff_group_use_by_country,
                                    share_use_ff_by_machine_national)
  ),
  
  # 10.5.a: Determining national electricity efficiencies of replacing fossil fuels at the economy-wide level
  tar_target(
    efficiency_substitution_national,
    calc_efficiency_substitution(share_use_ff_by_machine_national,
                                 machine_substitution_efficiencies,
                                 level = "national")
  ),
  # 10.5.b: Determining national electricity efficiencies of replacing fossil fuels at the economy-wide level, HEAT PUMP SCENARIO
  tar_target(
    efficiency_substitution_national_HPS,
    calc_efficiency_substitution(share_use_ff_by_machine_national,
                                 machine_substitution_efficiencies_HP_scenario,
                                 level = "national")
  ),
  
  # 10.6.a: Determining global electricity efficiency of replacing fossil fuels at the economy-wide level
  tar_target(
    efficiency_substitution_global,
    calc_efficiency_substitution(share_use_ff_by_machine_global,
                                 machine_substitution_efficiencies,
                                 level = "global")
  ),
  # 10.6.b: Determining global electricity efficiency of replacing fossil fuels at the economy-wide level, HEAT PUMP SCENARIO
  tar_target(
    efficiency_substitution_global_HPS,
    calc_efficiency_substitution(share_use_ff_by_machine_global,
                                 machine_substitution_efficiencies_HP_scenario,
                                 level = "global")
  ),
  
  ### BY END-USE LEVEL ###
  
  # 10.7: Determines the share of use of each fossil fuel group by machine within each end-use category, at the national level
  # THIS FUNCTION NEEDS TO BE DOUBLED CHECKED.
  tar_target(
    share_use_ff_by_machine_national_by_eu,
    calc_share_ff_by_machine_national_by_eu(tidy_national_shares_by_end_use,
                                            tidy_D_rev,
                                            machine_to_end_use_category_data)
  ),
  
  # 10.8: Determines the share of use of each fossil fuel group by machine, by end-use category, at the global level
  tar_target(
    share_use_ff_by_machine_global_by_eu,
    calc_share_ff_by_machine_global_by_eu(share_ff_group_use_by_end_use_by_country,
                                          share_use_ff_by_machine_national_by_eu)
  ),
  
  # 10.9.a: Determining national electricity efficiencies of replacing fossil fuels, by end-use
  tar_target(
    efficiency_substitution_national_by_eu,
    calc_efficiency_substitution_by(share_use_ff_by_machine_national_by_eu,
                                    machine_substitution_efficiencies,
                                    level = "national",
                                    by = "EU_category")
  ),
  # 10.9.b: Determining national electricity efficiencies of replacing fossil fuels, by end-use, HEAT PUMP SCENARIO
  tar_target(
    efficiency_substitution_national_by_eu_HPS,
    calc_efficiency_substitution_by(share_use_ff_by_machine_national_by_eu,
                                    machine_substitution_efficiencies_HP_scenario,
                                    level = "national",
                                    by = "EU_category")
  ),
  
  # 10.10: Determining global electricity efficiency of replacing fossil fuels, by end-use
  tar_target(
    efficiency_substitution_global_by_eu,
    calc_efficiency_substitution_by(share_use_ff_by_machine_global_by_eu,
                                    machine_substitution_efficiencies,
                                    level = "global",
                                    by = "EU_category")
  ),
  # 10.10: Determining global electricity efficiency of replacing fossil fuels, by end-use, HEAT PUMP SCENARIO
  tar_target(
    efficiency_substitution_global_by_eu_HPS,
    calc_efficiency_substitution_by(share_use_ff_by_machine_global_by_eu,
                                    machine_substitution_efficiencies_HP_scenario,
                                    level = "global",
                                    by = "EU_category")
  ),
  
  
  ### BY FINAL DEMAND SECTOR ###
  
  # 10.11: Determines the share of use of each fossil fuel group by machine, by final demand sector category, at the national level
  tar_target(
    share_use_ff_by_machine_national_by_fds,
    calc_share_ff_by_machine_national_by_fds(tidy_national_shares_by_fds,
                                             share_national_use_product_by_machine_by_fds)
  ),
  # 10.12: Determines the share of use of each fossil fuel group by machine, by final demand sector category, at the global level
  tar_target(
    share_use_ff_by_machine_global_by_fds,
    calc_share_ff_by_machine_global_by_fds(share_ff_group_use_by_fds_by_country,
                                           share_use_ff_by_machine_national_by_fds)
  ),
  # 10.13.a: Determining national electricity efficiencies of replacing fossil fuels, by final demand sector
  tar_target(
    efficiency_substitution_national_by_fds,
    calc_efficiency_substitution_by(share_use_ff_by_machine_national_by_fds,
                                    machine_substitution_efficiencies,
                                    level = "national",
                                    by = "Final_Demand_Sector_Category")
  ),
  # 10.13.b: Determining national electricity efficiencies of replacing fossil fuels, by final demand sector, HEAT PUMP SCENARIO
  tar_target(
    efficiency_substitution_national_by_fds_HPS,
    calc_efficiency_substitution_by(share_use_ff_by_machine_national_by_fds,
                                    machine_substitution_efficiencies_HP_scenario,
                                    level = "national",
                                    by = "Final_Demand_Sector_Category")
  ),
  
  # 10.14.a: Determining global electricity efficiency of replacing fossil fuels, by final demand sector
  tar_target(
    efficiency_substitution_global_by_fds,
    calc_efficiency_substitution_by(share_use_ff_by_machine_global_by_fds,
                                    machine_substitution_efficiencies,
                                    level = "global",
                                    by = "Final_Demand_Sector_Category")
    ),
  # 10.14.b: Determining global electricity efficiency of replacing fossil fuels, by final demand sector, HEAT PUMP SCENARIO
  tar_target(
    efficiency_substitution_global_by_fds_HPS,
    calc_efficiency_substitution_by(share_use_ff_by_machine_global_by_fds,
                                    machine_substitution_efficiencies_HP_scenario,
                                    level = "global",
                                    by = "Final_Demand_Sector_Category")
  ),

  
  #### (11) EROI equivalent calculations ####
  # Note that the term "required EROI" refers everywhere to the terminology "EROI equivalent" in the published paper;
  # The terminology in the code is obsolete.
  
  # 11.1: Calculating globally EROI equivalents for each year, with no breakdown
  tar_target(
    required_global_erois,
    calc_global_required_erois(
      substitution_efficiencies_df = efficiency_substitution_global,
      substitution_efficiencies_energy_sector_df = efficiency_substitution_global,
      aggregated_FF_FU_efficiencies_df = FF_global_aggregated_efficiencies,
      aggregated_global_erois_idE_df = aggregated_global_erois_without_breakdown_idE
    )
  ),
  # 11.2: Calculating globally EROI equivalents for each year, breakdown by end-use
  tar_target(
    required_global_erois_by_eu,
    calc_global_required_erois_by(
      substitution_efficiencies_by_df = efficiency_substitution_global_by_eu,
      substitution_efficiencies_energy_sector_df = efficiency_substitution_global,
      aggregated_FF_FU_efficiencies_df = FF_global_aggregated_efficiencies,
      aggregated_global_erois_idE_by_df = aggregated_global_erois_by_eu_idE,
      by = "EU_category"
    )
  ),
  # 11:3: Calculating globally EROI equivalents for each year, breakdown by final demand sector
  tar_target(
    required_global_erois_by_fds,
    calc_global_required_erois_by(
      substitution_efficiencies_by_df = efficiency_substitution_global_by_fds,
      substitution_efficiencies_energy_sector_df = efficiency_substitution_global,
      aggregated_FF_FU_efficiencies_df = FF_global_aggregated_efficiencies,
      aggregated_global_erois_idE_by_df = aggregated_global_erois_by_fds_idE,
      by = "Final_Demand_Sector_Category"
    )
  ),
  # 11.4: Calculating national EROI equivalents for each year, with no breakdown
  tar_target(
    required_national_erois,
    calc_national_required_erois(
      substitution_efficiencies_df = efficiency_substitution_national,
      substitution_efficiencies_energy_sector_df = efficiency_substitution_national,
      aggregated_national_FF_FU_efficiencies_df = FF_national_aggregated_efficiencies,
      aggregated_national_erois_idE_df = aggregated_national_erois_without_breakdown_idE
    )
  ),
  # 11.5: Calculating national EROI equivalents for each year, breakdown by end-use
  tar_target(
    required_national_erois_by_eu,
    calc_national_required_erois_by(
      substitution_efficiencies_by_df = efficiency_substitution_national_by_eu,
      substitution_efficiencies_energy_sector_df = efficiency_substitution_national,
      aggregated_national_FF_FU_efficiencies_df = FF_national_aggregated_efficiencies,
      aggregated_national_erois_idE_by_df = aggregated_national_useful_erois_by_eu_idE,
      by = "EU_category"
    )
  ),
  # 11.6: Calculating national EROI equivalents for each year, breakdown by final demand sector
  tar_target(
    required_national_erois_by_fds,
    calc_national_required_erois_by(
      substitution_efficiencies_by_df = efficiency_substitution_national_by_fds,
      substitution_efficiencies_energy_sector_df = efficiency_substitution_national,
      aggregated_national_FF_FU_efficiencies_df = FF_national_aggregated_efficiencies,
      aggregated_national_erois_idE_by_df = aggregated_national_useful_erois_by_fds_idE,
      by = "Final_Demand_Sector_Category"
    )
  ),
  
  
  ### HEAT PUMP SCENARIOS ###
  # 11.7: Calculating globally EROI equivalents for each year, with no breakdown
  tar_target(
    required_global_erois_HPS,
    calc_global_required_erois(
      substitution_efficiencies_df = efficiency_substitution_global_HPS,
      substitution_efficiencies_energy_sector_df = efficiency_substitution_global,
      aggregated_FF_FU_efficiencies_df = FF_global_aggregated_efficiencies,
      aggregated_global_erois_idE_df = aggregated_global_erois_without_breakdown_idE
    )
  ),
  # 11.8: Calculating globally EROI equivalents for each year, breakdown by end-use
  tar_target(
    required_global_erois_by_eu_HPS,
    calc_global_required_erois_by(
      substitution_efficiencies_by_df = efficiency_substitution_global_by_eu_HPS,
      substitution_efficiencies_energy_sector_df = efficiency_substitution_global,
      aggregated_FF_FU_efficiencies_df = FF_global_aggregated_efficiencies,
      aggregated_global_erois_idE_by_df = aggregated_global_erois_by_eu_idE,
      by = "EU_category"
    )
  ),
  # 11:9: Calculating globally EROI equivalents for each year, breakdown by final demand sector
  tar_target(
    required_global_erois_by_fds_HPS,
    calc_global_required_erois_by(
      substitution_efficiencies_by_df = efficiency_substitution_global_by_fds_HPS,
      substitution_efficiencies_energy_sector_df = efficiency_substitution_global,
      aggregated_FF_FU_efficiencies_df = FF_global_aggregated_efficiencies,
      aggregated_global_erois_idE_by_df = aggregated_global_erois_by_fds_idE,
      by = "Final_Demand_Sector_Category"
    )
  ),
  # 11.10: Calculating national EROI equivalents for each year, with no breakdown
  tar_target(
    required_national_erois_HPS,
    calc_national_required_erois(
      substitution_efficiencies_df = efficiency_substitution_national_HPS,
      substitution_efficiencies_energy_sector_df = efficiency_substitution_national,
      aggregated_national_FF_FU_efficiencies_df = FF_national_aggregated_efficiencies,
      aggregated_national_erois_idE_df = aggregated_national_erois_without_breakdown_idE
    )
  ),
  # 11.11: Calculating national EROI equivalents for each year, breakdown by end-use
  tar_target(
    required_national_erois_by_eu_HPS,
    calc_national_required_erois_by(
      substitution_efficiencies_by_df = efficiency_substitution_national_by_eu_HPS,
      substitution_efficiencies_energy_sector_df = efficiency_substitution_national,
      aggregated_national_FF_FU_efficiencies_df = FF_national_aggregated_efficiencies,
      aggregated_national_erois_idE_by_df = aggregated_national_useful_erois_by_eu_idE,
      by = "EU_category"
    )
  ),
  # 11.12: Calculating national EROI equivalents for each year, breakdown by final demand sector
  tar_target(
    required_national_erois_by_fds_HPS,
    calc_national_required_erois_by(
      substitution_efficiencies_by_df = efficiency_substitution_national_by_fds_HPS,
      substitution_efficiencies_energy_sector_df = efficiency_substitution_national,
      aggregated_national_FF_FU_efficiencies_df = FF_national_aggregated_efficiencies,
      aggregated_national_erois_idE_by_df = aggregated_national_useful_erois_by_fds_idE,
      by = "Final_Demand_Sector_Category"
    )
  ),
  
  #### (12) Breakdown direct versus indirect energy requirements ####
  
  # 12.1: Breaking down at the global level, without breakdown
  tar_target(
    dE_vs_idE_global,
    calc_energy_breakdown(
      dE_erois_df = aggregated_global_erois_without_breakdown,
      idE_erois_df = aggregated_global_erois_without_breakdown_idE
    )
  ),
  # 12.2: Breaking down at the global level, breakdown by EU
  tar_target(
    dE_vs_idE_global_by_eu,
    calc_energy_breakdown_by(
      dE_erois_df = aggregated_global_useful_erois_by_eu,
      idE_erois_df = aggregated_global_erois_by_eu_idE,
      by = "EU_category"
    )
  ),
  # 12.3: Breaking down at the global level, breakdown by final demand sector
  tar_target(
    dE_vs_idE_global_by_fds,
    calc_energy_breakdown_by(
      dE_erois_df = aggregated_global_useful_erois_by_fds,
      idE_erois_df = aggregated_global_erois_by_fds_idE,
      by = "Final_Demand_Sector_Category"
    )
  ),
  # 12.4: Breaking down at the national levels, without breakdown
  tar_target(
    dE_vs_idE_national,
    calc_energy_breakdown(
      dE_erois_df = aggregated_national_erois_without_breakdown,
      idE_erois_df = aggregated_national_erois_without_breakdown_idE
    )
  ),
  # 12.5: Breaking down at the national levels, breakdown by EU
  tar_target(
    dE_vs_idE_national_by_eu,
    calc_energy_breakdown_by(
      dE_erois_df = aggregated_national_useful_erois_by_eu,
      idE_erois_df = aggregated_national_useful_erois_by_eu_idE,
      by = "EU_category"
    )
  ),
  # 12.6: Breaking down at the national levels, breakdown by final demand sector
  tar_target(
    dE_vs_idE_national_by_fds,
    calc_energy_breakdown_by(
      dE_erois_df = aggregated_national_useful_erois_by_fds,
      idE_erois_df = aggregated_national_useful_erois_by_fds_idE,
      by = "Final_Demand_Sector_Category"
    )
  ),
  
  #### (13) Other accessory calculations #### 
  # 13.1: Share of fossil fuel consumption to TFC
  tar_target(
    share_ff_to_tfc,
    calc_share_ff_to_tfc(
      .tidy_iea_df = specified_regional_iea_data_adapted_Y_for_shares
    )
  ),
  # 13.2: Determine ratio of indirect energy to output
  # Here function that should be moved to EROITools
  tar_target(
    idE_to_output_ratio,
    calc_idE_to_output_ratio(
      aggregated_global_erois_df = aggregated_global_erois_without_breakdown,
      idE_by_industry_df = idE_by_industry_avg_added,
      tidy_world_iea_df = specified_world_iea_data_DTA,
      energy_units = iea_weeb_units_setup
    )
  )
  
  #####
)

