
# This function prepares the regional aggregation table
prepare_regional_aggregation_table <- function(regional_aggregation_table_file,
                                               path_to_inst){
  
  # List of countries (PFU.code format) of the PFU workflow:
  list_countries_pfu <- setdiff(PFUWorkflow::canonical_countries, c("WRLD")) %>% as.character()
  
  # Loading the IEA regions to PFU.code concordance table
  SEAPFU_workflow_countries_concordance <- PFUWorkflow::load_country_concordance_table(
    country_concordance_path = paste0(path_to_inst, "Country_Concordance_Full.xlsx")
  ) %>% 
    dplyr::filter(PFU.code %in% list_countries_pfu)
  
  # Reading the aggregation table, joining each IEA_region and Destination_region to PFU.code of the IEA_region
  # and excluding those regions for which PFU.code is NA (because then they are excluded from the workflow!)
  regional_aggregation_table_data <- readr::read_csv(regional_aggregation_table_file) %>% 
    dplyr::left_join(SEAPFU_workflow_countries_concordance, by = c("IEA_regions" = "IEA.name")) %>% 
    dplyr::select(IEA_regions, PFU.code, Destination_regions) %>% 
    dplyr::mutate(
      PFU.code = dplyr::case_when(
        stringr::str_detect(IEA_regions, "Ivoire") ~ "CIV",
        TRUE ~ PFU.code
      )
    ) %>% 
    dplyr::filter(! is.na(PFU.code)) %>% 
    dplyr::rename(
      Country = PFU.code
    )
}


# This function aggregates regionally the specified regional IEA data 
# with a user-provided regional concordance table
aggregate_specified_regional_iea_data <- function(specified_regional_iea_df,
                                                  regional_aggregation_table_df,
                                                  aggregate = TRUE){
  
  if (aggregate == TRUE){
    
    # First step, checking that all countries are routed somewhere
    
    # List of countries present in specified regional data frame
    list_specified_regional <- specified_regional_iea_df %>% 
      dplyr::ungroup() %>% 
      dplyr::select(Country) %>% 
      dplyr::distinct()
    
    # Determining those that are not routed to any destination region
    not_routed_regions <- list_specified_regional %>% 
      dplyr::left_join(regional_aggregation_table_df, by = "Country") %>% 
      dplyr::filter(is.na(Destination_regions)) %>% 
      dplyr::select(IEA_regions) %>% 
      dplyr::distinct() %>% 
      dplyr::pull()
    
    # Checking that only WAB and WMB are not routed anywhere
    if ((length(not_routed_regions) > 0) && (not_routed_regions == c("WABK", "WMBK"))){
      "Some regions are not routed anywhere, careful!"#, but these are only WAB and WMB."
    } else if (length(not_routed_regions) > 0){
      "Careful, some regions are not routed anywhere!"
    }
    
    # Second step, aggregate regions
    aggregated_regional_iea_df <- specified_regional_iea_df %>% 
      dplyr::select(-matnames) %>% 
      IEATools::aggregate_regions(aggregation_table = regional_aggregation_table_df, 
                                  net_trade = FALSE) %>% 
      # Removing regions routed to NA. Formerly WABK and WMBK.
      dplyr::filter(!is.na(Country))
    
    # Third step, compute net trade flows for the aggregation. Need to unspecify imports/exports first:
    net_trade_flows <- aggregated_regional_iea_df %>% 
      # The \\[ trick gets rid of Exports to WAB/WMB.
      dplyr::filter(stringr::str_detect(Flow, "Imports \\[") | stringr::str_detect(Flow, "Exports \\[")) %>% 
      IEATools::despecify_col(col = "Flow", despecified_col = "Flow") %>% 
      # The convert_to_net_trade function specifies back imports and exports flows
      ECCTools::convert_to_net_trade()
    
    # Fourth step, filter out imports and exports flows of the aggregated data frame, bind net trade flows, add back the matnames column
    # and send statistical differences and stock changes to balancing.
    specified_aggregated_regional_iea_df <- aggregated_regional_iea_df %>% 
      # This also filters out exports to WMB and WAB, which is perfect.
      dplyr::filter(! (stringr::str_detect(Flow, "Imports") | stringr::str_detect(Flow, "Exports"))) %>% 
      dplyr::bind_rows(net_trade_flows) %>% 
      IEATools::add_psut_matnames() %>% 
      # Send stat diffs and stock changes to balancing
      ECCTools::stat_diffs_to_balancing() %>% 
      ECCTools::stock_changes_to_balancing() %>% 
      ECCTools::convert_fuel_gasoline_into_motor_gasoline() %>% 
      # Fixing Australia data
      # The issue is that Blast furnaces supply AND consume the same quantity of blast furnace gas in Australia!
      # Which is an issue for inversing the matrix
      # This tweak only adds a minor issue in the data.
      dplyr::mutate(
        matnames = dplyr::case_when(
          (Year %in% seq(2013, 2020)) & (Country == "AUS") & (Product == "Blast furnace gas") & (Flow == "Blast furnaces") & (E.dot < 0) ~ "Epsilon",
          TRUE ~ matnames
        )
      )
    
  } else {
    
    specified_aggregated_regional_iea_df <- specified_regional_iea_df %>% 
      dplyr::filter(! Country %in% c("WABK", "WMBK")) %>% 
      # Send stat diffs and stock changes to balancing
      ECCTools::stat_diffs_to_balancing() %>%
      ECCTools::stock_changes_to_balancing() %>%
      ECCTools::convert_fuel_gasoline_into_motor_gasoline() %>% 
      # Fixing Australia data
      # The issue is that Blast furnaces supply AND consume the same quantity of blast furnace gas in Australia!
      # Which is an issue for inversing the matrix
      # This tweak only adds a minor issue in the data.
      dplyr::mutate(
        matnames = dplyr::case_when(
          (Year %in% seq(2013, 2020)) & (Country == "AUS") & (Product == "Blast furnace gas") & (Flow == "Blast furnaces") & (E.dot < 0) ~ "Epsilon",
          TRUE ~ matnames
        )
      )
    
  }
  
  return(specified_aggregated_regional_iea_df)
}


# Transforms specified regional data to Global Market Assumption
transforms_regional_data_to_gma <- function(specified_regional_iea_df){
  ECCTools::transform_to_gma(specified_regional_iea_df)
}


# Prepare PSUT matrices for a specified regional Global Market Assumption data frame
prepare_gma_psut_mats <- function(specified_regional_iea_gma_df) {
  IEATools::prep_psut(specified_regional_iea_gma_df)
}


# Calculates IO matrices for the Global Market Assumption, World
calc_IO_mats_gma <- function(PSUT_mats_gma_df,
                             from = 1971,
                             until = 2020){

  for(i in seq(from, until, 1)){

    to_add <-   PSUT_mats_gma_df %>%
      dplyr::filter(Year == i) %>%
      Recca::calc_io_mats()

    if (i == from){
      cumulative_years_mats <- to_add
    } else if (i > from){
      cumulative_years_mats <- dplyr::bind_rows(
        cumulative_years_mats,
        to_add
      )
    }
    print(i)
  }
  
  return(cumulative_years_mats)
}


# This function should allow to directly calculate the tidy_national_erois without storing
# previously the IO mats, which should gain a lot of storing space and memory
calc_IO_mats_tidy_national_io_erois <- function(PSUT_mats_gma_df,
                                        eroi_method = "GMA",
                                        from = 1971,
                                        until = 2020){
  
  for(i in seq(from, until, 1)){
    
    IO_mats <- PSUT_mats_gma_df %>%
      dplyr::filter(Year == i) %>%
      Recca::calc_io_mats()
    
    erois_to_add <- IO_mats %>% 
      Recca::calc_E_EIOU() %>% 
      Recca::calc_erois() %>% 
      EROITools::extract_tidy_product_erois() %>% 
      dplyr::mutate(
        Eroi.method = eroi_method
      ) %>% 
      dplyr::relocate(.data[["Eroi.method"]], .after = Year)
    
    if (i == from){
      tidy_national_io_erois <- erois_to_add
    } else if (i > from){
      tidy_national_io_erois <- dplyr::bind_rows(
        tidy_national_io_erois,
        erois_to_add
      )
    }
    print(i)
  }
  return(tidy_national_io_erois)
}



# Binds IO mats data frames
bind_IO_mats <- function(IO_mats_df_1, 
                         IO_mats_df_2){
  IO_mats_df <- dplyr::bind_rows(IO_mats_df_1, IO_mats_df_2)
  return(IO_mats_df)
}

# Binds targets
bind_targets <- function(target_1, 
                         target_2){
  IO_mats_df <- dplyr::bind_rows(target_1, target_2)
  return(IO_mats_df)
}


# Calculates and extracts national level, final stage EROIs
calc_and_extract_national_erois <- function(IO_mats,
                                            eroi_method = "GMA"){
  IO_mats %>% 
    Recca::calc_E_EIOU() %>% 
    Recca::calc_erois() %>% 
    EROITools::extract_tidy_product_erois() %>% 
    dplyr::mutate(
      Eroi.method = eroi_method
    ) %>% 
    dplyr::relocate(.data[["Eroi.method"]], .after = Year) %>% 
    dplyr::mutate(
      Country = stringr::str_extract(Product, "\\{.*\\}") %>% 
        stringr::str_remove("\\{") %>% 
        stringr::str_remove("\\}"),
      Product = stringr::str_remove(Product, "\\{.*\\}_")
    )
}


# This function bind the nationally aggregated primary, final and useful stage EROIs without end-use or final demand sector breakdown.
aggregate_national_erois_whout_breakdown <- function(aggregated_national_primary_erois_df,
                                                     aggregated_national_final_erois_df,
                                                     aggregated_national_useful_erois_df){
  
  # Binding all these together and returning results
  aggregated_national_erois <- dplyr::bind_rows(
    aggregated_national_primary_erois_df,
    aggregated_national_final_erois_df,
    aggregated_national_useful_erois_df
  )
  
  return(aggregated_national_erois)
}


# This function aggregates EROIs at the primary stage, using the production mix of each product, in each country, within each fossil fuel group
aggregate_national_primary_erois_whout_breakdown <- function(tidy_io_erois_df,
                                                             ecc_gma_df){
  
  # First, adapting the ECC GMA data frame
  ecc_gma_adapted <- EROITools::prepare_gma_for_shares(ecc_gma_df)

  # Second, aggregating at the primary energy stage:
  aggregated_national_primary_erois <- EROITools::aggregate_primary_stage_erois(
    .tidy_erois_df = tidy_io_erois_df,
    .tidy_iea_df = ecc_gma_adapted,
    eroi_calc_method = "gma"
  )
  
  # And returning aggregated primary EROIs
  return(aggregated_national_primary_erois)
}


# This function aggregates EROIs at the final stage, using the consumption mix (including EIOU flows, excluding feedstock flows)
# of each product, in each country, within each fossil fuel group
aggregate_national_final_erois_whout_breakdown <- function(tidy_io_erois_df,
                                                           ecc_gma_df){
  
  # First, adapting the ECC GMA data frame
  ecc_gma_adapted <- EROITools::prepare_gma_for_shares(ecc_gma_df)

  # Second, aggregating at the final energy stage:
  aggregated_national_final_erois <- EROITools::aggregate_final_stage_erois(
    .tidy_erois_df = tidy_io_erois_df,
    .tidy_iea_df = ecc_gma_adapted,
    include_non_energy_uses = FALSE,
    eroi_calc_method = "gma"
  )
  
  return(aggregated_national_final_erois)
}

# This function aggregates EROIs at the useful stage, using the consumption mix (including EIOU flows, excluding feedstock flows)
# of each product, in each country, within each fossil fuel group
aggregate_national_useful_erois_whout_breakdown <- function(tidy_useful_erois_df,
                                                            ecc_gma_df){
  
  # First, adapting the ECC GMA data frame
  ecc_gma_adapted <- EROITools::prepare_gma_for_shares(ecc_gma_df)
  
  rm(ecc_gma_df)

  aggregated_national_useful_erois <- tibble::tibble(
    Country = as.character(),
    Method = as.character(),
    Energy.type = as.character(),
    Last.stage = as.character(),
    Year = as.numeric(),
    Eroi.method = as.character(),
    Type = as.character(),
    Boundary = as.character(),
    Non_Energy_Uses = as.character(),
    Product.Group = as.character(),
    Energy.stage = as.character(),
    Group.eroi = as.double()
  )
    
  # For loop for each year:
  for (i in seq(1971, 2020)){
    
    print(i)
    
    ecc_gma_adapted_current_year <- ecc_gma_adapted %>% 
      dplyr::filter(Year == i)
    
    tidy_useful_erois_current_year <- tidy_useful_erois_df %>% 
      dplyr::filter(Year == i)
    
    to_add <- EROITools::aggregate_useful_stage_erois(
      .tidy_erois_df = tidy_useful_erois_current_year,
      .tidy_iea_df = ecc_gma_adapted_current_year,
      include_non_energy_uses = FALSE,
      eroi_calc_method = "gma"
    )
    
    aggregated_national_useful_erois <- dplyr::bind_rows(
      aggregated_national_useful_erois,
      to_add
    )
  }
  
  return(aggregated_national_useful_erois)
}


# This function calculates national useful stage EROIs from national IO erois and average national efficiencies by product
push_to_national_useful_erois <- function(tidy_io_national_erois_df,
                                          FU_national_efficiencies_df,
                                          Average_Efficiency_Col = "Average_Efficiency_By_Country",
                                          years_analysis){
  
  for (i in years_analysis){
    
    print(i)
    
    erois_to_push <- tidy_io_national_erois_df %>% 
      dplyr::filter(Year == i)
    
    efficiencies_to_use <- FU_national_efficiencies_df %>% 
      dplyr::filter(Year == i)
    
    if (i == min(years_analysis)){
      tidy_national_useful_stage_erois <- EROITools::push_to_useful_erois(
        .tidy_io_erois = erois_to_push,
        tidy_FU_efficiencies = efficiencies_to_use,
        average_efficiency = Average_Efficiency_Col,
        eroi_calc_method = "gma"
      )
    } else {
      to_add <- EROITools::push_to_useful_erois(
        .tidy_io_erois = erois_to_push,
        tidy_FU_efficiencies = efficiencies_to_use,
        average_efficiency = Average_Efficiency_Col,
        eroi_calc_method = "gma"
      )
      
      tidy_national_useful_stage_erois <- tidy_national_useful_stage_erois %>% 
        dplyr::bind_rows(to_add)
    }
  }
  
  return(tidy_national_useful_stage_erois)
}



# This function calculates the tidy national useful erois by end-use sector data frame
calc_tidy_national_useful_erois_by_eu <- function(tidy_national_io_erois_df,
                                                  FU_efficiencies_national_by_eu_df){
  
  tidy_national_io_erois_adapted <- tidy_national_io_erois_df %>% 
    dplyr::mutate(
      product_without_origin = stringr::str_remove(Product, "\\{.*\\}_")
    ) %>% 
    dplyr::select(-Country)
  
  tidy_national_useful_erois_by_eu <- FU_efficiencies_national_by_eu_df %>% 
    dplyr::rename(product_without_origin = Product) %>% 
    dplyr::inner_join(
      tidy_national_io_erois_adapted,
      by = c("Method", "Energy.type", "Year", "product_without_origin")
    ) %>% 
    dplyr::mutate(
      Useful_Stage_EROI_by_eu = Average_Efficiency_By_End_Use * EROI
    )
  
  return(tidy_national_useful_erois_by_eu)
}



# This function calculates the tidy national useful erois by final demand sector data frame
calc_tidy_national_useful_erois_by_fds <- function(tidy_national_io_erois_df,
                                                   FU_efficiencies_national_by_fds_df){
  
  tidy_national_io_erois_adapted <- tidy_national_io_erois_df %>% 
    dplyr::mutate(
      product_without_origin = stringr::str_remove(Product, "\\{.*\\}_")
    ) %>% 
    dplyr::select(-Country)
  
  tidy_national_useful_erois_by_fds <- FU_efficiencies_national_by_fds_df %>% 
    dplyr::rename(product_without_origin = Product) %>% 
    dplyr::inner_join(
      tidy_national_io_erois_adapted,
      by = c("Method", "Energy.type", "Year", "product_without_origin")
    ) %>% 
    dplyr::mutate(
      Useful_Stage_EROI_by_fds = Average_Efficiency_By_FDSC_By_Country * EROI
    )
  
  return(tidy_national_useful_erois_by_fds)
}


# This function calculates, for each country, the share of fossil fuel (including fossil fuels
# used as heat and electricity) consumption to total final consumption and so gives an indication 
# of the scope remaining in each country to substitute fossil fuels by renewable energy
calc_share_ff_to_tfc <- function(.tidy_iea_df){
  
  # Calculating final energy of fossil fuel origin in each country
  final_energy_from_ff_df <- EROITools::calc_fec_from_ff_by_group(.tidy_iea_df = .tidy_iea_df)
  
  # Calculating final energy consumption by country
  tfc_df <- .tidy_iea_df %>% 
    dplyr::filter(matnames %in% c("Y", "U_EIOU")) %>% 
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, Unit) %>% 
    dplyr::summarise(tfc = sum(abs(E.dot)))
  
  # Binding and calculating the ratio
  share_ff_to_tfc_df <- final_energy_from_ff_df %>% 
    dplyr::left_join(
      tfc_df,
      by = c("Country", "Method", "Energy.type", "Last.stage", "Year", "Unit")
    ) %>% 
    dplyr::mutate(
      ff_to_tfc = E.dot / tfc
    )
  
  return(share_ff_to_tfc_df)
}

