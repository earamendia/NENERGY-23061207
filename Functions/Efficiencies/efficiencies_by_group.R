# This function calculates the average final-to-useful efficiencies of each fossil fuel group globally,
# without any breakdown.
calc_FF_global_aggregated_efficiencies <- function(FU_efficiencies_avg_global_specified_df,
                                                   specified_world_iea_DTA_df,
                                                   Average_Efficiency_Col){
  
  average_global_FU_ff_efficiencies <- EROITools::calc_avg_efficiency_by_ff_group(
    .tidy_efficiencies_df = FU_efficiencies_avg_global_specified_df,
    .tidy_iea_df = specified_world_iea_DTA_df,
    average_efficiency = Average_Efficiency_Col,
    calc_method = "dta"
  )
  
  return(average_global_FU_ff_efficiencies)
}


# This function calculates the average final-to-useful efficiencies of each fossil fuel group globally,
# with a breakdown by end-use category.
calc_FF_global_aggregated_efficiencies_by_end_use <- function(FU_efficiencies_avg_global_by_end_use_specified_df,
                                                              shares_global_ff_use_by_end_use_df){
  
  # Calculating and returning average efficiencies
  FF_global_aggregated_efficiencies_by_end_use <- shares_global_ff_use_by_end_use_df %>%
    dplyr::left_join(FU_efficiencies_avg_global_by_end_use_specified_df, by = c("Country", "Method", "Energy.type", "Year", "Product", "EU_category")) %>%
    dplyr::filter(! is.na(Global_Avg_Efficiency_By_End_Use)) %>%
    dplyr::group_by(Country, Method, Energy.type, Year, EU_category, Product.Group, Energy.stage) %>%
    dplyr::summarise(
      Average_Efficiency_by_end_use = sum(Share_Group_Input * Global_Avg_Efficiency_By_End_Use) / sum(Share_Group_Input)
    )
  
  return(FF_global_aggregated_efficiencies_by_end_use)
}



# This function calculates the average final-to-useful efficiencies of each fossil fuel group globally,
# with a breakdown by final demand sector
calc_FF_global_aggregated_efficiencies_by_fds <- function(FU_efficiencies_avg_global_by_fds_specified_df,
                                                          shares_global_ff_use_by_fds_df){
  
  # Calculating and returning average efficiencies
  FF_global_aggregated_efficiencies_by_fds <- shares_global_ff_use_by_fds_df %>%
    dplyr::left_join(FU_efficiencies_avg_global_by_fds_specified_df, by = c("Country", "Method", "Energy.type", "Year", "Product", "Final_Demand_Sector_Category")) %>%
    dplyr::filter(! is.na(Global_Average_Efficiency_By_FDSC)) %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Energy.stage) %>%
    dplyr::summarise(
      Average_Efficiency_by_fds = sum(Share_Group_Demand * Global_Average_Efficiency_By_FDSC) / sum(Share_Group_Demand)
    )
  
  return(FF_global_aggregated_efficiencies_by_fds)
}



# This function calculates the average final-to-useful efficiencies of each fossil fuel group nationally,
# without any breakdown.
calc_FF_national_aggregated_efficiencies <- function(FU_efficiencies_avg_by_country_df,
                                                     ecc_gma_df){
  
  # First, adapting the ECC GMA data frame
  ecc_gma_adapted <- EROITools::prepare_gma_for_shares(ecc_gma_df)
  
  # Second, calculating the average efficiency
  average_national_FU_ff_efficiencies <- EROITools::calc_avg_efficiency_by_ff_group(
    .tidy_efficiencies_df = FU_efficiencies_avg_by_country_df,
    .tidy_iea_df = ecc_gma_adapted,
    average_efficiency = "Average_Efficiency_By_Country",
    calc_method = "gma"
  )
  
  return(average_national_FU_ff_efficiencies)
}



# This function calculates the average final-to-useful efficiencies of each fossil fuel group nationally,
# with a breakdown by end-use category.
calc_FF_national_aggregated_efficiencies_by_end_use <- function(FU_efficiencies_avg_by_end_use_by_country_df,
                                                                tidy_national_shares_by_end_use_df){
  
  
  # 1) Unspecify the origin of each product in the shares, because the efficiency doesn't depend on the origin
  tidy_national_shares_by_end_use_unspecified_df <- tidy_national_shares_by_end_use_df %>% 
    dplyr::select(-Product) %>% 
    dplyr::rename(Product = product_without_origin) %>% 
    dplyr::filter(! is.na(Share_Group_Input))
    
  # 2) Calculating and returning useful stage EROIs by end-use category
  national_FF_efficiencies_by_eu <- tidy_national_shares_by_end_use_unspecified_df %>%
    dplyr::left_join(FU_efficiencies_avg_by_end_use_by_country_df, by = c("Country", "Method", "Energy.type", "Year", "Product", "EU_category")) %>%
    dplyr::filter(! is.na(Average_Efficiency_By_End_Use)) %>% # this should be empty ideally....!
    dplyr::group_by(Country, Method, Energy.type, Year, EU_category, Product.Group, Energy.stage) %>%
    dplyr::summarise(
      Average_Efficiency_By_End_Use = sum(Share_Group_Input * Average_Efficiency_By_End_Use) / sum(Share_Group_Input)
    )
  
  return(national_FF_efficiencies_by_eu)
}


# This function calculates the average final-to-useful efficiencies of each fossil fuel group nationally,
# with a breakdown by final demand sector
calc_FF_national_aggregated_efficiencies_by_fds <- function(FU_efficiencies_avg_by_fds_by_country_df,
                                                            tidy_national_shares_by_fds_df){
  
  
  # 1) Unspecify the origin of each product in the shares, because the efficiency doesn't depend on the origin
  tidy_national_shares_by_fds_unspecified_df <- tidy_national_shares_by_fds_df %>% 
    dplyr::select(-Product) %>% 
    dplyr::rename(Product = product_without_origin) %>% 
    dplyr::filter(! is.na(Share_Group_Demand))

  # 2) Calculating and returning useful stage EROIs by end-use category
  national_FF_efficiencies_by_fds <- tidy_national_shares_by_fds_unspecified_df %>%
    dplyr::left_join(FU_efficiencies_avg_by_fds_by_country_df, by = c("Country", "Method", "Energy.type", "Year", "Product", "Final_Demand_Sector_Category")) %>%
    dplyr::filter(! is.na(Average_Efficiency_By_FDSC_By_Country)) %>% # this should be empty ideally....!
    dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Energy.stage) %>%
    dplyr::summarise(
      Average_Efficiency_By_FDSC = sum(Share_Group_Demand * Average_Efficiency_By_FDSC_By_Country) / sum(Share_Group_Demand)
    )
  
  return(national_FF_efficiencies_by_fds)
}

