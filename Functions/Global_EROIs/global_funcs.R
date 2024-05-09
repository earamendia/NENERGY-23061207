
# Selects the file to use to fix IEA data based on the IEA WEEB file units
select_iea_global_fixing_file <- function(path_to_inst,
                                          iea_weeb_energy_units){
  
  if(iea_weeb_energy_units == "ktoe"){
    iea_global_fixing_file <- paste0(path_to_inst, "World_fixes/World_fixes_ktoe.csv")
  } else if (iea_weeb_energy_units == "TJ"){
    iea_global_fixing_file <- paste0(path_to_inst, "World_fixes/World_fixes_TJ.csv")
  }
  return(iea_global_fixing_file)
}

# Loads raw iea data
load_raw_iea_data <- function(data,
                              years,
                              path_to_inst,
                              energy_units){
  data %>% 
    IEATools::load_tidy_iea_df(
      override_df = PFUWorkflow::load_country_concordance_table(
        country_concordance_path =  paste0(path_to_inst, "Country_Concordance_Full.xlsx")
      ),
      unit_val = energy_units
    ) %>% 
    dplyr::filter(Year %in% years) |> 
    IEATools::fix_COL_WRLD_electricity()
}

# Loads raw amending data for global IEA WEEB
load_amending_data <- function(data,
                               years){
  readr::read_csv(data) %>% 
    tidyr::pivot_longer(cols = where(is.double), names_to = "Year", values_to = "E.dot") %>% 
    dplyr::mutate(
      Year = as.double(Year)
    ) %>% 
    dplyr::mutate(Country = "WRLD") %>% 
    dplyr::filter(Year %in% years)
}

# Amends and specifies global IEA data
specify_and_amend_world_data <- function(main_data, amending_data){
  main_data %>% 
    dplyr::filter(Year >= 1971) %>%
    dplyr::filter(Country == "WRLD") %>% 
    IEATools::fix_tidy_iea_df_balances(max_fix = 6) %>% 
    dplyr::bind_rows(amending_data) %>% 
    matsindf::group_by_everything_except("E.dot") %>% 
    dplyr::summarise(
      E.dot = sum(E.dot)
    ) %>% 
    IEATools::specify_all() %>% 
    ECCTools::specify_elect_heat_renewables() %>% 
    ECCTools::specify_elect_heat_fossil_fuels() %>% 
    ECCTools::specify_elect_heat_nuclear() %>% 
    ECCTools::specify_other_elec_heat_production() %>% 
    ECCTools::specify_elect_heat_markets() %>% 
    IEATools::add_psut_matnames() %>% 
    # this line here is needed to assign a matrix to the "fake" flows added to the amending data
    # could be improved upstream so that this line is not needed, see later on.
    dplyr::mutate(
      matnames = dplyr::case_when(
        is.na(matnames) ~ "Y",
        TRUE ~ matnames
      )
    ) %>% 
    ECCTools::specify_losses_as_industry()
}


# Routes stock changes and statistical differences to balancing matrix
route_SC_SD_to_balancing <- function(tidy_iea_data){
  tidy_iea_data %>% 
    ECCTools::stat_diffs_to_balancing() %>% 
    ECCTools::stock_changes_to_balancing()
}

# Routes stock changes, statistical differences, exports,
# losses, and non-energy uses, to balancing matrix. Losses will only work if flow is kept as "Losses".
adapt_Y_to_calc_shares <- function(tidy_iea_data){
  tidy_iea_data %>% 
    ECCTools::stat_diffs_to_balancing() %>% 
    ECCTools::stock_changes_to_balancing() %>% 
    ECCTools::exports_to_balancing() %>% 
    ECCTools::losses_to_balancing() %>% 
    ECCTools::non_energy_uses_to_balancing()
}

# Builds Input-Output matrices
build_io_matrices <- function(PSUT_mats){
  PSUT_mats %>% 
    IEATools::prep_psut() %>% 
    Recca::calc_io_mats(method_q_calculation = "sum_R_V_cols")
}


# Transforms world data to Domestic Technology Assumption
transform_world_to_dta <- function(tidy_world_data_to_transform){
  # Keep only U_feed for matrix requirements
  ECCTools::transform_to_dta(tidy_world_data_to_transform,
                             requirement_matrices_list = c("U_feed"))
}


# Calculates and extracts global level, final stage EROIs
calc_and_extract_erois <- function(IO_mats,
                                   eroi_method = "DTA"){
  IO_mats %>% 
    Recca::calc_E_EIOU() %>% 
    Recca::calc_erois() %>% 
    EROITools::extract_tidy_product_erois() %>% 
    dplyr::mutate(
      Eroi.method = eroi_method
    ) %>% 
    dplyr::relocate(.data[["Eroi.method"]], .after = Year)
}


# Pushing EROIs to useful stage without particular breakdown
push_to_global_useful_erois <- function(tidy_io_global_erois_df,
                                        FU_global_efficiencies_df,
                                        Average_Efficiency_Col = "Average_Efficiency_Col"){
  
  useful_erois_df <- EROITools::push_to_useful_erois(
    .tidy_io_erois = tidy_io_global_erois_df,
    tidy_FU_efficiencies = FU_global_efficiencies_df,
    average_efficiency = Average_Efficiency_Col,
    eroi_calc_method = "dta"
  )
  
  return(useful_erois_df)
}


# Aggregating final and useful stage EROIs separately, then binding them together
aggregate_global_erois_whout_breakdown <- function(tidy_io_erois_df,
                                                   tidy_useful_erois_df,
                                                   ecc_dta_df){
  
  # Aggregating primary stage erois
  aggregated_primary_global_erois <- EROITools::aggregate_primary_stage_erois(
    .tidy_erois_df = tidy_io_erois_df,
    .tidy_iea_df = ecc_dta_df,
    eroi_calc_method = "dta"
  )
  
  # Aggregating final stage erois
  aggregated_final_global_erois <- EROITools::aggregate_final_stage_erois(
    .tidy_erois_df = tidy_io_erois_df,
    .tidy_iea_df = ecc_dta_df,
    include_non_energy_uses = FALSE,
    eroi_calc_method = "dta"
  )
  
  # Aggregating useful stage erois
  aggregated_useful_global_erois <- EROITools::aggregate_useful_stage_erois(
    .tidy_erois_df = tidy_useful_erois_df,
    .tidy_iea_df = ecc_dta_df,
    include_non_energy_uses = FALSE,
    eroi_calc_method = "dta"
  )
  
  # Binding aggregated final and useful erois
  aggregated_global_erois <- dplyr::bind_rows(
    aggregated_primary_global_erois,
    aggregated_final_global_erois,
    aggregated_useful_global_erois
  )
  
  return(aggregated_global_erois)
}


# Loads idE raw data
load_idE_raw_data <- function(idE_calcs_outcome_data_file,
                              years){
  
  readr::read_rds(idE_calcs_outcome_data_file) %>% 
    dplyr::mutate(Country = "WRLD") %>% 
    dplyr::filter(Year %in% years)
}


# Specifies regional IEA data making sure that World is removed and that double accounting regions are removed too
specify_regional_iea_data <- function(tidy_raw_iea_df){

  list_countries <- setdiff(PFUPipelineTools::canonical_countries, c("WRLD")) %>% as.character()
  
  tidy_raw_iea_df %>% 
    dplyr::filter(Country %in% list_countries) %>% 
    dplyr::filter(Year >= 1971) %>% 
    IEATools::fix_tidy_iea_df_balances(max_fix = 6) %>%
    IEATools::specify_all() %>% 
    ECCTools::specify_elect_heat_renewables() %>% 
    ECCTools::specify_elect_heat_fossil_fuels() %>% 
    ECCTools::specify_elect_heat_nuclear() %>% 
    ECCTools::specify_other_elec_heat_production() %>% 
    ECCTools::specify_elect_heat_markets() %>% 
    IEATools::add_psut_matnames()
}


# Calculates the global share of each product used in each country
# So, when summing for a given product across countries, we obtain 1.
calc_share_product_use_by_country <- function(tidy_specified_iea_data){

  share_product_use_by_country <- tidy_specified_iea_data %>% 
    dplyr::filter(matnames == "Y") %>% 
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, Product, Unit) %>% 
    dplyr::summarise(
      Product_Use_By_Country = sum(E.dot)
    ) %>%
    dplyr::group_by(Method, Energy.type, Last.stage, Year, Product, Unit) %>% 
    dplyr::mutate(
      Share_Product_Use_By_Country = Product_Use_By_Country / sum(Product_Use_By_Country)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Product_Use_By_Country, -Unit, -Last.stage)
  
  # Test
  share_product_use_by_country %>% 
    group_by(Method, Energy.type, Year, Product) %>% 
    dplyr::summarise(
      sum_shares = sum(Share_Product_Use_By_Country)
    ) %>% 
    dplyr::filter(abs(sum_shares - 1) > 1e-4) %>%
    nrow() %>%
    testthat::expect_equal(0)
  
  return(share_product_use_by_country)
}


# Calculates the global share of each fossil fuel group used in each country
# So, when summing for a given fossil fuel group across countries, we obtain 1.
calc_share_ff_group_use_by_country <- function(tidy_specified_iea_data){

  # Here I collect specified electricity final energy consumption flows by picking up the flows going in the "Electricity market" activity
  specified_elec_flows <- tidy_specified_iea_data %>% 
    dplyr::filter(Flow == "Electricity market", matnames == "U_feed") %>% 
    dplyr::mutate(
      E.dot = abs(E.dot),
      Energy.stage = "Final (fuel+elec+heat)"
    )
  # Here I collect specified heat final energy consumption flows by picking up the flows going in the "Electricity market" activity
  specified_heat_flows <- tidy_specified_iea_data %>% 
    dplyr::filter(Flow == "Heat market", matnames == "U_feed") %>% 
    dplyr::mutate(
      E.dot = abs(E.dot),
      Energy.stage = "Final (fuel+elec+heat)"
    )
  
  # Shares by group
  share_ff_group_use_by_country_1 <- tidy_specified_iea_data %>% 
    dplyr::filter(matnames == "Y") %>% 
    tidyr::expand_grid(Energy.stage = c("Final (fuel)", "Final (fuel+elec+heat)")) %>% 
    dplyr::bind_rows(
      specified_elec_flows,
      specified_heat_flows
    ) %>% 
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% IEATools::coal_and_coal_products ~ "Coal products",
        Product %in% IEATools::oil_and_oil_products ~ "Oil products",
        Product %in% IEATools::primary_gas_products ~ "Natural gas",
        stringr::str_detect(Product, "from Natural gas") ~ "Natural gas",
        stringr::str_detect(Product, "from Oil products") ~ "Oil products",
        stringr::str_detect(Product, "from Coal products") ~ "Coal products",
        TRUE ~ "Others"
      )
    ) %>% 
    dplyr::filter(Product.Group != "Others") %>% 
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, Product.Group, Energy.stage, Unit) %>% 
    dplyr::summarise(
      FF_Use_By_Country = sum(E.dot)
    ) %>%
    dplyr::group_by(Method, Energy.type, Last.stage, Year, Product.Group, Energy.stage, Unit) %>% 
    dplyr::mutate(
      Share_FF_Use_By_Country = FF_Use_By_Country / sum(FF_Use_By_Country)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-FF_Use_By_Country, -Unit, -Last.stage)
  
  # Shares for oil and gas products
  share_ff_group_use_by_country_2 <- tidy_specified_iea_data %>% 
    dplyr::filter(matnames == "Y") %>% 
    tidyr::expand_grid(Energy.stage = c("Final (fuel)", "Final (fuel+elec+heat)")) %>% 
    dplyr::bind_rows(
      specified_elec_flows,
      specified_heat_flows
    ) %>% 
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        stringr::str_detect(Product, "from Natural gas") ~ "Oil and gas products",
        stringr::str_detect(Product, "from Oil products") ~ "Oil and gas products",
        TRUE ~ "Others"
      )
    ) %>% 
    dplyr::filter(Product.Group != "Others") %>% 
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, Product.Group, Energy.stage, Unit) %>% 
    dplyr::summarise(
      FF_Use_By_Country = sum(E.dot)
    ) %>%
    dplyr::group_by(Method, Energy.type, Last.stage, Year, Product.Group, Energy.stage, Unit) %>% 
    dplyr::mutate(
      Share_FF_Use_By_Country = FF_Use_By_Country / sum(FF_Use_By_Country)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-FF_Use_By_Country, -Unit, -Last.stage)
  
  # Shares for all fossil fuels
  share_ff_group_use_by_country_3 <- tidy_specified_iea_data %>% 
    dplyr::filter(matnames == "Y") %>% 
    tidyr::expand_grid(Energy.stage = c("Final (fuel)", "Final (fuel+elec+heat)")) %>% 
    dplyr::bind_rows(
      specified_elec_flows,
      specified_heat_flows
    ) %>% 
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% c(IEATools::coal_and_coal_products, IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "All fossil fuels",
        stringr::str_detect(Product, "from Natural gas") ~ "All fossil fuels",
        stringr::str_detect(Product, "from Oil products") ~ "All fossil fuels",
        stringr::str_detect(Product, "from Coal products") ~ "All fossil fuels",
        TRUE ~ "Others"
      )
    ) %>% 
    dplyr::filter(Product.Group != "Others") %>% 
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, Product.Group, Energy.stage, Unit) %>% 
    dplyr::summarise(
      FF_Use_By_Country = sum(E.dot)
    ) %>%
    dplyr::group_by(Method, Energy.type, Last.stage, Year, Product.Group, Energy.stage, Unit) %>% 
    dplyr::mutate(
      Share_FF_Use_By_Country = FF_Use_By_Country / sum(FF_Use_By_Country)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-FF_Use_By_Country, -Unit, -Last.stage)
  
  # Binding all shares
  share_ff_group_use_by_country <- dplyr::bind_rows(
    share_ff_group_use_by_country_1,
    share_ff_group_use_by_country_2,
    share_ff_group_use_by_country_3
  )
  
  # Test
  share_ff_group_use_by_country %>% 
    dplyr::group_by(Method, Energy.type, Year, Product.Group, Energy.stage) %>% 
    dplyr::summarise(sum_shares = sum(Share_FF_Use_By_Country)) %>% dplyr::filter(abs(sum_shares - 1) > 1e-4) %>% nrow() %>% testthat::expect_equal(0)
    
  return(share_ff_group_use_by_country)
}




# Calculates average global FU efficiencies
calc_avg_global_FU_efficiencies <- function(avg_FU_efficiencies_by_country,
                                            share_global_product_use_by_country){
  
  share_global_product_use_by_country %>% 
    dplyr::left_join(avg_FU_efficiencies_by_country,
                     by = c("Country", "Method", "Energy.type", "Year", "Product")) %>% 
    dplyr::filter(!is.na(Average_Efficiency_By_Country)) %>% 
    dplyr::group_by(Method, Energy.type, Year, Product) %>% 
    dplyr::summarise(
      Average_Efficiency_Global = sum(Share_Product_Use_By_Country * Average_Efficiency_By_Country) / sum(Share_Product_Use_By_Country)
    ) %>% 
    dplyr::mutate(
      Country = "WRLD"
    ) %>% 
    dplyr::relocate(Country, .before = Method)
}


# Calculates the share of each product use within each end-use by country
# So, when summing across countries for a single product and single end-use, shares must add up to unity.
# This function filters out EU categories with no correspondent category
calc_share_product_use_by_end_use_by_country <- function(PSUT_mats_df,
                                                         machine_to_end_use_df){
  
  share_product_use_by_end_use_by_country <- PSUT_mats_df %>% 
    dplyr::filter(Last.stage == "Useful") %>% 
    dplyr::select(Country, Method, Energy.type, Last.stage, Year, U) %>% 
    tidyr::pivot_longer(cols = "U", names_to = "matnames", values_to = "matvals") %>% 
    matsindf::expand_to_tidy() %>% 
    dplyr::filter(matvals != 0) %>% 
    dplyr::select(-rowtypes, -coltypes) %>% 
    dplyr::rename(
      Product = rownames,
      Industry = colnames
    ) %>% 
    dplyr::filter(stringr::str_detect(Industry, "->")) %>% 
    dplyr::filter(Industry != "Non-energy consumption -> NEU") %>% 
    dplyr::left_join(machine_to_end_use_df, by = "Industry") %>%
    # Filtering out EU categories with no correspondent category
    dplyr::filter(!is.na(EU_category)) %>% 
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, Product, EU_category) %>% 
    dplyr::summarise(
      fec_by_end_use_by_country = sum(matvals)
    ) %>% 
    dplyr::group_by(Method, Energy.type, Last.stage, Year, Product, EU_category) %>% 
    dplyr::mutate(
      Share_fec_by_end_use_by_country = fec_by_end_use_by_country / sum(fec_by_end_use_by_country)
    ) %>% 
    dplyr::select(-fec_by_end_use_by_country)
  
  # Test
  share_product_use_by_end_use_by_country %>% 
    dplyr::group_by(Method, Energy.type, Last.stage, Year, Product, EU_category) %>% 
    dplyr::summarise(
      sum_shares = sum(Share_fec_by_end_use_by_country)
    ) %>% 
    dplyr::filter(abs(sum_shares - 1) > 1e-4) %>% nrow() %>% testthat::expect_equal(0)
  
  return(share_product_use_by_end_use_by_country)
}


# Calculates the share of each fossil fuel group use within each end-use by country
# So, when summing across countries for a single fossil fuel group and single end-use, shares must add up to unity.
# This function filters out EU categories with no correspondent category
calc_share_ff_group_use_by_end_use_by_country <- function(PSUT_mats_df,
                                                          tidy_iea_df,
                                                          machine_to_end_use_df){
  
  # First determining fossil fuels products products use
  product_use_by_machine_end_use <- PSUT_mats_df %>% 
    dplyr::filter(Last.stage == "Useful") %>% 
    dplyr::select(Country, Method, Energy.type, Last.stage, Year, U) %>% 
    tidyr::pivot_longer(cols = "U", names_to = "matnames", values_to = "matvals") %>% 
    matsindf::expand_to_tidy() %>% 
    dplyr::filter(matvals != 0) %>% 
    dplyr::select(-rowtypes, -coltypes) %>% 
    dplyr::rename(
      Product = rownames,
      Industry = colnames
    ) %>% 
    dplyr::filter(stringr::str_detect(Industry, "->")) %>% 
    dplyr::filter(Industry != "Non-energy consumption -> NEU") %>% 
    # Remove WABK and WMBK as they are a pain for subsequent targets and don't need them
    dplyr::filter(! Country %in% c("WABK", "WMBK")) %>% 
    dplyr::left_join(machine_to_end_use_df, by = "Industry") %>%
    # Filtering out EU categories with no correspondent category
    dplyr::filter(!is.na(EU_category))
  
  
  # Second determining electricity use by fossil fuel group of generation
  # Calling helper functions from EROITools and storing results
  a <- EROITools::calc_share_elec_supply_by_ff_group(tidy_iea_df) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-tidyselect::any_of(c("Last.stage", "Unit", "Energy.stage")))
  b <- EROITools::calc_shares_elec_by_ff_group(tidy_iea_df) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-tidyselect::any_of(c("Last.stage", "Unit", "Energy.stage")))
  
  # Determining actual electricity use by fossil fuel group of generation
  elec_use <- product_use_by_machine_end_use %>% 
    dplyr::filter(Product == "Electricity") %>% 
    dplyr::left_join(
      a, 
      by = c("Country", "Method", "Energy.type", "Year")
    ) %>% 
    dplyr::filter(!is.na(Share)) %>% 
    dplyr::mutate(
      Elec_from_FFs = matvals * Share
    ) %>% 
    dplyr::select(-Share, -matvals, -Product) %>% 
    dplyr::left_join(
      b,
      by = c("Country", "Method", "Energy.type", "Year", "Product.Group")
    ) %>% 
    dplyr::mutate(
      matvals = Elec_from_FFs * Share,
      Energy.stage = "Useful (fuel+elec+heat)"
    ) %>% 
    dplyr::select(-Elec_from_FFs, -Share, -Non_Energy_Uses, -Product.Group)
  
  
  # Third determining heat use by fossil fuel group of generation
  # Calling helper functions from EROITools and storing results
  c <- EROITools::calc_share_heat_supply_by_ff_group(tidy_iea_df) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-tidyselect::any_of(c("Last.stage", "Unit", "Energy.stage")))
  d <- EROITools::calc_shares_heat_by_ff_group(tidy_iea_df) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-tidyselect::any_of(c("Last.stage", "Unit", "Energy.stage")))
  
  # Determining actual heat use by fossil fuel group of generation
  heat_use <- product_use_by_machine_end_use %>% 
    dplyr::filter(Product == "Heat") %>% 
    dplyr::left_join(
      c,
      by = c("Country", "Method", "Energy.type", "Year")
    ) %>% 
    dplyr::filter(!is.na(Share)) %>% 
    dplyr::mutate(
      Heat_from_FFs = matvals * Share
    ) %>% 
    dplyr::select(-Share, -matvals, -Product) %>% 
    dplyr::left_join(
      d,
      by = c("Country", "Method", "Energy.type", "Year", "Product.Group")
    ) %>% 
    dplyr::mutate(
      matvals = Heat_from_FFs * Share,
      Energy.stage = "Useful (fuel+elec+heat)"
    ) %>% 
    dplyr::select(-Heat_from_FFs, -Share, -Non_Energy_Uses, -Product.Group)
  
  # Shares for Coal products, Oil products, and Natural gas
  share_ff_group_use_by_end_use_by_country_1 <- product_use_by_machine_end_use %>% 
    dplyr::filter(Product %in% c(IEATools::coal_and_coal_products, IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>% 
    tidyr::expand_grid(Energy.stage = c("Useful (fuel)", "Useful (fuel+elec+heat)")) %>% 
    dplyr::bind_rows(
      elec_use,
      heat_use
    ) %>% 
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% IEATools::coal_and_coal_products ~ "Coal products",
        Product %in% IEATools::oil_and_oil_products ~ "Oil products",
        Product %in% IEATools::primary_gas_products ~ "Natural gas",
        stringr::str_detect(Product, "from Natural gas") ~ "Natural gas",
        stringr::str_detect(Product, "from Oil products") ~ "Oil products",
        stringr::str_detect(Product, "from Coal products") ~ "Coal products",
        TRUE ~ "Others"
      )
    ) %>% 
    dplyr::filter(Product.Group != "Others") %>% 
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, Product.Group, Energy.stage, EU_category) %>% 
    dplyr::summarise(
      fec_by_ff_group_by_eu = sum(matvals)
    ) %>% 
    dplyr::group_by(Method, Energy.type, Last.stage, Year, Product.Group, Energy.stage, EU_category) %>% 
    dplyr::mutate(
      Share_fec_by_ff_group_by_eu = fec_by_ff_group_by_eu / sum(fec_by_ff_group_by_eu)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-fec_by_ff_group_by_eu, -Last.stage)
  
  # Shares for Oil and gas products
  share_ff_group_use_by_end_use_by_country_2 <- product_use_by_machine_end_use %>% 
    dplyr::filter(Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>% 
    tidyr::expand_grid(Energy.stage = c("Useful (fuel)", "Useful (fuel+elec+heat)")) %>% 
    dplyr::bind_rows(
      elec_use,
      heat_use
    ) %>% 
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        stringr::str_detect(Product, "from Natural gas") ~ "Oil and gas products",
        stringr::str_detect(Product, "from Oil products") ~ "Oil and gas products",
        TRUE ~ "Others"
      )
    ) %>%
    dplyr::filter(Product.Group != "Others") %>% 
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, Product.Group, Energy.stage, EU_category) %>% 
    dplyr::summarise(
      fec_by_ff_group_by_eu = sum(matvals)
    ) %>% 
    dplyr::group_by(Method, Energy.type, Last.stage, Year, Product.Group, Energy.stage, EU_category) %>% 
    dplyr::mutate(
      Share_fec_by_ff_group_by_eu = fec_by_ff_group_by_eu / sum(fec_by_ff_group_by_eu)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-fec_by_ff_group_by_eu, -Last.stage)
  
  # Shares for All fossil fuels
  share_ff_group_use_by_end_use_by_country_3 <- product_use_by_machine_end_use %>% 
    dplyr::filter(Product %in% c(IEATools::coal_and_coal_products, IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>% 
    tidyr::expand_grid(Energy.stage = c("Useful (fuel)", "Useful (fuel+elec+heat)")) %>% 
    dplyr::bind_rows(
      elec_use,
      heat_use
    ) %>% 
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% c(IEATools::coal_and_coal_products, IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "All fossil fuels",
        stringr::str_detect(Product, "from Natural gas") ~ "All fossil fuels",
        stringr::str_detect(Product, "from Oil products") ~ "All fossil fuels",
        stringr::str_detect(Product, "from Coal products") ~ "All fossil fuels",
        TRUE ~ "Others"
      )
    ) %>% 
    dplyr::filter(Product.Group != "Others") %>% 
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, Product.Group, Energy.stage, EU_category) %>% 
    dplyr::summarise(
      fec_by_ff_group_by_eu = sum(matvals)
    ) %>% 
    dplyr::group_by(Method, Energy.type, Last.stage, Year, Product.Group, Energy.stage, EU_category) %>% 
    dplyr::mutate(
      Share_fec_by_ff_group_by_eu = fec_by_ff_group_by_eu / sum(fec_by_ff_group_by_eu)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-fec_by_ff_group_by_eu, -Last.stage)
  
  # Binding all together
  share_ff_group_use_by_end_use_by_country <- dplyr::bind_rows(
    share_ff_group_use_by_end_use_by_country_1,
    share_ff_group_use_by_end_use_by_country_2,
    share_ff_group_use_by_end_use_by_country_3
  )
  
  # Test
  share_ff_group_use_by_end_use_by_country %>% 
    dplyr::group_by(Method, Energy.type, Year, Product.Group, Energy.stage, EU_category) %>% 
    dplyr::summarise(sum_shares = sum(Share_fec_by_ff_group_by_eu)) %>% dplyr::filter(abs(sum_shares - 1) > 1e-4) %>% nrow() %>% testthat::expect_equal(0)
  
  return(share_ff_group_use_by_end_use_by_country)
}


# Calculates the share of each product use within each final demand sector by country
# So, when summing across countries for a single product and single final demand sector, shares must add up to unity.
# Function filters out final demand sectors that end up as NAs (i.e. no correspondence in concordance table)
calc_share_product_use_by_fds_by_country <- function(tidy_specified_iea_data,
                                                     fds_to_fds_category_df){
  
  share_product_use_by_fds_by_country <- tidy_specified_iea_data %>% 
    dplyr::filter(matnames == "Y") %>% 
    dplyr::left_join(
      fds_to_fds_category_df %>% 
        dplyr::rename(Flow = Final_Demand_Sector),
      by = "Flow"
    ) %>% 
    # Taking out final demand sectors classified as NAs (i.e. with no correspondence)
    dplyr::filter(!is.na(Final_Demand_Sector_Category)) %>% 
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, Ledger.side, Unit, Product, Final_Demand_Sector_Category) %>% 
    dplyr::summarise(
      Fec_by_fdsc_by_country = sum(E.dot)
    ) %>% 
    dplyr::group_by(Method, Energy.type, Last.stage, Year, Ledger.side, Unit, Product, Final_Demand_Sector_Category) %>% 
    dplyr::mutate(
      Share_fec_fdsc_by_country = Fec_by_fdsc_by_country / sum(Fec_by_fdsc_by_country)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Fec_by_fdsc_by_country, -Ledger.side, -Unit)
  
  share_product_use_by_fds_by_country %>% 
    dplyr::group_by(Method, Energy.type, Last.stage, Year, Product, Final_Demand_Sector_Category) %>% 
    dplyr::summarise(
      sum_shares = sum(Share_fec_fdsc_by_country)
    ) %>% dplyr::filter(abs(sum_shares - 1) > 1e-4) %>% nrow() %>% testthat::expect_equal(0)
  
  return(share_product_use_by_fds_by_country)
}


# Calculates the share of each fossil fuel group use within each final demand sector by country
# So, when summing across countries for a single fossil fuel group and single final demand sector, shares must add up to unity.
# Function filters out final demand sectors that end up as NAs (i.e. no correspondence in concordance table)
calc_share_ff_group_use_by_fds_by_country <- function(tidy_specified_iea_data,
                                                      fds_to_fds_category_df){
  
  # List of product use by final demand sector
  product_by_fds_df <- tidy_specified_iea_data %>% 
    dplyr::filter(matnames == "Y") %>% 
    dplyr::left_join(
      fds_to_fds_category_df %>% 
        dplyr::rename(Flow = Final_Demand_Sector),
      by = "Flow"
    ) %>% 
    # Taking out final demand sectors classified as NAs (i.e. with no correspondence)
    dplyr::filter(!is.na(Final_Demand_Sector_Category))
  
  # Electricity use by final demand sector
  # Calling helper functions from EROITools and storing results
  a <- EROITools::calc_share_elec_supply_by_ff_group(tidy_specified_iea_data) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-tidyselect::any_of(c("Last.stage", "Unit", "Energy.stage")))
  b <- EROITools::calc_shares_elec_by_ff_group(tidy_specified_iea_data) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-tidyselect::any_of(c("Last.stage", "Unit", "Energy.stage")))
  
  specified_elec_use_by_fds <- product_by_fds_df %>% 
    dplyr::filter(Product == "Electricity") %>% 
    dplyr::left_join(
      a, 
      by = c("Country", "Method", "Energy.type", "Year")
    ) %>% 
    dplyr::filter(!is.na(Share)) %>% 
    dplyr::mutate(
      Elec_from_FFs = E.dot * Share
    ) %>% 
    dplyr::select(-Share, -E.dot, -Product) %>% 
    dplyr::left_join(
      b,
      by = c("Country", "Method", "Energy.type", "Year", "Product.Group")
    ) %>% 
    dplyr::mutate(
      E.dot = Elec_from_FFs * Share,
      Energy.stage = "Useful (fuel+elec+heat)"
    ) %>% 
    dplyr::select(-Elec_from_FFs, -Share, -Non_Energy_Uses, -Product.Group)
  

  # Heat use by final demand sector
  # Calling helper functions from EROITools and storing results
  c <- EROITools::calc_share_heat_supply_by_ff_group(tidy_specified_iea_data) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-tidyselect::any_of(c("Last.stage", "Unit", "Energy.stage")))
  d <- EROITools::calc_shares_heat_by_ff_group(tidy_specified_iea_data) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-tidyselect::any_of(c("Last.stage", "Unit", "Energy.stage")))
  
  specified_heat_use_by_fds <- product_by_fds_df %>% 
    dplyr::filter(Product == "Heat") %>% 
    dplyr::left_join(
      c,
      by = c("Country", "Method", "Energy.type", "Year")
    ) %>% 
    dplyr::filter(!is.na(Share)) %>% 
    dplyr::mutate(
      Heat_from_FFs = E.dot * Share
    ) %>% 
    dplyr::select(-Share, -E.dot, -Product) %>% 
    dplyr::left_join(
      d,
      by = c("Country", "Method", "Energy.type", "Year", "Product.Group")
    ) %>% 
    dplyr::mutate(
      E.dot = Heat_from_FFs * Share,
      Energy.stage = "Useful (fuel+elec+heat)"
    ) %>% 
    dplyr::select(-Heat_from_FFs, -Share, -Non_Energy_Uses, -Product.Group)
  
  
  # Determining shares of product use within each fossil fuel product group
  # Coal products, Oil products, Natural gas
  share_ff_group_use_by_fds_by_country_1 <- product_by_fds_df %>% 
    dplyr::filter(Product %in% c(IEATools::coal_and_coal_products, IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>% 
    tidyr::expand_grid(Energy.stage = c("Useful (fuel)", "Useful (fuel+elec+heat)")) %>% 
    dplyr::bind_rows(
      specified_elec_use_by_fds,
      specified_heat_use_by_fds
    ) %>% 
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% IEATools::coal_and_coal_products ~ "Coal products",
        Product %in% IEATools::oil_and_oil_products ~ "Oil products",
        Product %in% IEATools::primary_gas_products ~ "Natural gas",
        stringr::str_detect(Product, "from Natural gas") ~ "Natural gas",
        stringr::str_detect(Product, "from Oil products") ~ "Oil products",
        stringr::str_detect(Product, "from Coal products") ~ "Coal products",
        TRUE ~ "Others"
      )
    ) %>% 
    dplyr::filter(Product.Group != "Others") %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Product.Group, Energy.stage, Final_Demand_Sector_Category) %>% 
    dplyr::summarise(
      Fec_by_ff_group_by_fdsc = sum(E.dot)
    ) %>% 
    dplyr::group_by(Method, Energy.type, Year, Product.Group, Energy.stage, Final_Demand_Sector_Category) %>% 
    dplyr::mutate(
      Share_fec_by_ff_group_by_fdsc = Fec_by_ff_group_by_fdsc / sum(Fec_by_ff_group_by_fdsc)
    ) %>% 
    dplyr::select(-Fec_by_ff_group_by_fdsc)
    
  # Oil and gas products
  share_ff_group_use_by_fds_by_country_2 <- product_by_fds_df %>% 
    dplyr::filter(Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>% 
    dplyr::bind_rows(
      specified_elec_use_by_fds,
      specified_heat_use_by_fds
    ) %>% 
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        stringr::str_detect(Product, "from Natural gas") ~ "Oil and gas products",
        stringr::str_detect(Product, "from Oil products") ~ "Oil and gas products",
        TRUE ~ "Others"
      )
    ) %>%
    dplyr::filter(Product.Group != "Others") %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Product.Group, Energy.stage, Final_Demand_Sector_Category) %>% 
    dplyr::summarise(
      Fec_by_ff_group_by_fdsc = sum(E.dot)
    ) %>% 
    dplyr::group_by(Method, Energy.type, Year, Product.Group, Energy.stage, Final_Demand_Sector_Category) %>% 
    dplyr::mutate(
      Share_fec_by_ff_group_by_fdsc = Fec_by_ff_group_by_fdsc / sum(Fec_by_ff_group_by_fdsc)
    ) %>% 
    dplyr::select(-Fec_by_ff_group_by_fdsc)
  
  # All fossil fuels
  share_ff_group_use_by_fds_by_country_3 <- product_by_fds_df %>% 
    dplyr::filter(Product %in% c(IEATools::coal_and_coal_products, IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>% 
    dplyr::bind_rows(
      specified_elec_use_by_fds,
      specified_heat_use_by_fds
    ) %>% 
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% c(IEATools::coal_and_coal_products, IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "All fossil fuels",
        stringr::str_detect(Product, "from Natural gas") ~ "All fossil fuels",
        stringr::str_detect(Product, "from Oil products") ~ "All fossil fuels",
        stringr::str_detect(Product, "from Coal products") ~ "All fossil fuels",
        TRUE ~ "Others"
      )
    ) %>% 
    dplyr::filter(Product.Group != "Others") %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Product.Group, Energy.stage, Final_Demand_Sector_Category) %>% 
    dplyr::summarise(
      Fec_by_ff_group_by_fdsc = sum(E.dot)
    ) %>% 
    dplyr::group_by(Method, Energy.type, Year, Product.Group, Energy.stage, Final_Demand_Sector_Category) %>% 
    dplyr::mutate(
      Share_fec_by_ff_group_by_fdsc = Fec_by_ff_group_by_fdsc / sum(Fec_by_ff_group_by_fdsc)
    ) %>% 
    dplyr::select(-Fec_by_ff_group_by_fdsc)
  
  # Binding all these
  share_ff_group_use_by_fds_by_country <- dplyr::bind_rows(
    share_ff_group_use_by_fds_by_country_1,
    share_ff_group_use_by_fds_by_country_2,
    share_ff_group_use_by_fds_by_country_3
  )
  
  # Test
  share_ff_group_use_by_fds_by_country %>% 
    dplyr::group_by(Method, Energy.type, Year, Product.Group, Energy.stage, Final_Demand_Sector_Category) %>% 
    dplyr::summarise(sum_shares = sum(Share_fec_by_ff_group_by_fdsc)) %>% dplyr::filter(abs(sum_shares - 1) > 1e-4) %>% nrow() %>% testthat::expect_equal(0)
  
  return(share_ff_group_use_by_fds_by_country)
}


# Calculates the average efficiency, for each product, in each country, by end-use category
calc_avg_efficiencies_by_end_use_by_country <- function(tidy_fu_efficiencies_df,
                                                        tidy_D_rev_df,
                                                        machine_to_end_use_df){
  
  # First, we calculate the share of each product p, in each country c, used in machine m
  # within a given end-use category eu.
  calc_shares_by_machine_eu <- tidy_D_rev_df %>% 
    #dplyr::filter(Energy.type == "E") %>% 
    dplyr::left_join(machine_to_end_use_df, by = c("Industry")) %>% 
    # Adds up the shares of all product p that are used in machine m and end-use category euc
    dplyr::group_by(Country, Method, Energy.type, Year, Product, Industry, EU_category) %>% 
    dplyr::summarise(
      Total_Share_by_Industry_Within_euc = sum(Share)
    ) %>%
    # Calculates the share of product p used in machine m within a given end-use category
    dplyr::group_by(Country, Method, Energy.type, Year, Product, EU_category) %>% 
    dplyr::mutate(
      Share_Industry_By_EU_Category = Total_Share_by_Industry_Within_euc / sum(Total_Share_by_Industry_Within_euc)
    )
  
  # Second, we write a test so that we are sure that for each product and each end-use category euc,
  # the sum of shares across machines/industries equals unity.
  calc_shares_by_machine_eu %>% 
    dplyr::group_by(Country, Method, Year, Product, EU_category) %>% 
    dplyr::summarise(sum_shares = sum(Share_Industry_By_EU_Category)) %>% 
    dplyr::filter(abs(sum_shares - 1) > 1e-4) %>% 
    nrow() %>% 
    testthat::expect_equal(0)
  
  # Third, we use those shares to determine, for each product p, and each country c,
  # the average efficiency with which product p is used within a given end-use category eu.
  avg_FU_efficiency_by_end_use <- calc_shares_by_machine_eu %>% 
    dplyr::left_join(tidy_fu_efficiencies_df,
                     by = c("Country", "Method", "Energy.type", "Year", "Industry")) %>% 
    #dplyr::filter(Energy.type == "E") %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Product, EU_category) %>%
    dplyr::summarise(
      Average_Efficiency_By_End_Use = sum(Share_Industry_By_EU_Category * Efficiency) / sum(Share_Industry_By_EU_Category)
    )
}


# Calculates the average global efficiency for each product, by end-use category.
calc_global_avg_efficiencies_by_end_use <- function(FU_efficiencies_avg_by_end_use_by_country_df,
                                                    share_use_by_end_use_by_country_df){

  # Test no NAs in join
  share_use_by_end_use_by_country_df %>% 
    dplyr::anti_join(FU_efficiencies_avg_by_end_use_by_country_df, by = c("Country", "Method", "Year", "Product", "EU_category")) %>% 
    nrow() %>% 
    testthat::expect_equal(0)
    
  # Actual calculation of the average global efficiency for each product, by end-use category
  share_use_by_end_use_by_country_df %>% 
    dplyr::left_join(FU_efficiencies_avg_by_end_use_by_country_df, by = c("Country", "Method", "Energy.type", "Year", "Product", "EU_category")) %>% 
    dplyr::group_by(Method, Year, Energy.type, Product, EU_category) %>% 
    dplyr::summarise(
      Global_Avg_Efficiency_By_End_Use = sum(Share_fec_by_end_use_by_country * Average_Efficiency_By_End_Use) / sum(Share_fec_by_end_use_by_country)
    ) %>% 
    dplyr::mutate(
      Country = "WRLD"
    ) %>% 
    dplyr::relocate(Country, .before = Method)
}


# Pulls out values of C_rev, which indicate the share of product p in the input of industry i, for each country
extract_tidy_C_rev <- function(PSUT_mat_expanded_df){
  
  PSUT_mat_expanded_df %>% 
    dplyr::filter(Last.stage == "Useful") %>% 
    tidyr::pivot_longer(cols = -c("Country", "Method", "Energy.type", "Last.stage", "Year"), names_to = "matnames", values_to = "matvals") %>% 
    dplyr::filter(matnames == "C_rev") %>% 
    matsindf::expand_to_tidy() %>% 
    dplyr::filter(matvals != 0) %>% 
    dplyr::select(-rowtypes, -coltypes, -matnames) %>% 
    dplyr::rename(
      Industry = colnames,
      Product = rownames,
      Product_Input_Share = matvals
    )
}




calc_share_national_use_product_by_machine_by_fds <- function(PSUT_mat_df,
                                                              tidy_C_rev_df,
                                                              fds_to_fds_category_df){
  
  # Calculates input requirements for each machine to fulfil final demand for final demand sector s
  machine_inputs_requirements_by_fds <- PSUT_mat_df %>% 
    dplyr::filter(Last.stage == "Useful") %>%
    tidyr::pivot_longer(cols = -c("Country", "Method", "Energy.type", "Last.stage", "Year"), names_to = "matnames", values_to = "matvals") %>% 
    dplyr::filter(matnames == "eta_D_Y") %>% 
    matsindf::expand_to_tidy() %>% 
    dplyr::filter(matvals != 0) %>% 
    dplyr::select(-rowtypes, -coltypes, -matnames) %>% 
    dplyr::rename(
      Final_Demand_Sector = colnames,
      Industry = rownames,
      Energy_Input_Requirements = matvals
    ) %>% 
    dplyr::left_join(fds_to_fds_category_df, by = "Final_Demand_Sector") %>% 
    dplyr::filter(!is.na(Final_Demand_Sector_Category)) %>% 
    dplyr::group_by(Country, Method, Year, Energy.type, Last.stage, Final_Demand_Sector_Category, Industry) %>% 
    dplyr::summarise(
      Energy_Input_Requirements_By_FDSC = sum(Energy_Input_Requirements)
    ) %>% 
    # Getting rid of non-energy uses:
    dplyr::filter(Industry != "Non-energy consumption -> NEU")
  
  # Calculates the energy inputs to fulfil each final demand sector broken down by energy product and FU conversion machine
  fec_by_product_machine_fds <- machine_inputs_requirements_by_fds %>% 
    dplyr::left_join(tidy_C_rev_df, by = c("Country", "Method", "Year", "Industry", "Energy.type", "Last.stage")) %>% 
    dplyr::mutate(
      Product_Use_By_Machine_FDSC = Energy_Input_Requirements_By_FDSC * Product_Input_Share
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Energy_Input_Requirements_By_FDSC, -Product_Input_Share, -Energy.type, -Last.stage) 
  
  # Calculates, for each product, the share used in machine m within final demand sector s
  share_fec_by_product_machine_fds <- fec_by_product_machine_fds %>% 
    dplyr::group_by(Country, Method, Year, Final_Demand_Sector_Category, Product) %>% 
    dplyr::mutate(
      Share_Product_Use_By_Machine_FDSC = Product_Use_By_Machine_FDSC / sum(Product_Use_By_Machine_FDSC)
    ) %>% 
    dplyr::select(-Product_Use_By_Machine_FDSC)
  
  # Quality check: sum of shares = 1
  share_fec_by_product_machine_fds %>% 
    dplyr::group_by(Country, Method, Year, Final_Demand_Sector_Category, Product) %>% 
    dplyr::summarise(sum_shares = sum(Share_Product_Use_By_Machine_FDSC)) %>% 
    dplyr::filter(abs(sum_shares - 1) > 1e-4) %>% 
    nrow() %>% 
    testthat::expect_equal(0)
  
  return(share_fec_by_product_machine_fds)
}


# This function calculates, for each product p and in each country, the average efficiency
# with which product p is used in a given final demand sector category s.
calc_avg_efficiencies_by_fds_by_country <- function(share_fec_by_product_machine_fds_df,
                                                    tidy_fu_efficiencies_df){
  
  # Calculate average efficiencies by final demand sector by country
  FU_efficiencies_by_fds_by_country_df <- share_fec_by_product_machine_fds_df %>% 
    dplyr::left_join(tidy_fu_efficiencies_df, by = c("Country", "Method", "Year", "Industry")) %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product) %>% 
    dplyr::summarise(
      Average_Efficiency_By_FDSC_By_Country = sum(Share_Product_Use_By_Machine_FDSC * Efficiency) / sum(Share_Product_Use_By_Machine_FDSC)
    )
  
  # Check that there are no NAs in the join
  share_fec_by_product_machine_fds_df %>% 
    dplyr::anti_join(tidy_fu_efficiencies_df, by = c("Country", "Method", "Year", "Industry")) %>% 
    nrow() %>% 
    testthat::expect_equal(0)
  
  return(FU_efficiencies_by_fds_by_country_df)
}



# This function calculates, for each product p, the average global efficiency
# with which product p is used in a given final demand sector category s.
calc_global_avg_efficiencies_by_fds <- function(share_use_by_fds_by_country_df,
                                                FU_efficiencies_avg_by_fds_by_country){
  
  # There are some NAs in the join. These NAs correspond to flows that are NOT non-energy uses flows in the IEA's WEEB,
  # But that are routed to non-energy consumption in the PFU workflow.
  share_use_by_fds_by_country_df %>% 
    dplyr::left_join(FU_efficiencies_avg_by_fds_by_country, by = c("Country", "Method", "Energy.type", "Year", "Final_Demand_Sector_Category", "Product")) %>%
    dplyr::group_by(Method, Year, Energy.type, Product, Final_Demand_Sector_Category) %>% 
    dplyr::summarise(
      Global_Average_Efficiency_By_FDSC = sum(Share_fec_fdsc_by_country * Average_Efficiency_By_FDSC_By_Country) / sum(Share_fec_fdsc_by_country)
    ) %>% 
    dplyr::mutate(
      Country = "WRLD"
    ) %>% 
    dplyr::relocate(Country, .before = Method)
}
