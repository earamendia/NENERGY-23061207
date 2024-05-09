

# Loading data ------------------------------------------------------------

# This function reads the machine electrification concordance table
load_machine_electrification_concordance <- function(path_to_machine_electrification_concordance){
  concordance_df <- readr::read_csv(path_to_machine_electrification_concordance)
  return(concordance_df)
}


# This function reads the backstop efficiencies for the electrification machines
load_machine_backstop_efficiencies <- function(path_to_machine_elec_backstop_efficiencies,
                                               Efficiency_Col_Name = "Backstop_Efficiency",
                                               Year_Col_Name = "Year",
                                               years){
  
  backstop_efficiencies_df <- readr::read_csv(path_to_machine_elec_backstop_efficiencies) %>% 
    tidyr::pivot_longer(cols = -tidyselect::all_of("Substitution_machine"), values_to = Efficiency_Col_Name, names_to = Year_Col_Name) %>% 
    dplyr::mutate(dplyr::across(Year:Efficiency_Col_Name, as.numeric)) %>% 
    dplyr::filter(.data[[Year_Col_Name]] %in% years) %>% 
    glimpse()
 
  return(backstop_efficiencies_df) 
}



# Preparing substitution efficiencies for each machine --------------------

# This function collates the machine substitution (electricity) efficiencies to be used
collate_machine_substitution_efficiencies <- function(tidy_fu_efficiencies_by_machine_country_df,
                                                      machine_electrification_concordance_df,
                                                      machine_backstop_efficiencies_df){
  
  # List of elec machines
  list_elec_machines <- machine_electrification_concordance_df %>% 
    dplyr::select(tidyselect::all_of(c("Substitution_machine"))) %>% 
    dplyr::pull()
  
  # Contains also non-elec machines, but these will be 
  tidy_elec_machine_df <- tidy_fu_efficiencies_by_machine_country_df %>% 
    dplyr::rename(
      Substitution_machine = Industry,
      Substitution_efficiency = Efficiency
    ) %>% 
    dplyr::filter(Substitution_machine %in% list_elec_machines)
  
  # Collates substitution machines efficiencies
  collated_machine_substitution_efficiencies <- tidy_fu_efficiencies_by_machine_country_df %>% 
    dplyr::left_join(machine_electrification_concordance_df %>% dplyr::rename(Industry = PFU_machine), by = "Industry") %>% 
    dplyr::left_join(
      tidy_elec_machine_df,
      by = c("Country", "Method", "Energy.type", "Last.stage", "Year", "Substitution_machine")
    ) %>% 
    dplyr::left_join(
      machine_backstop_efficiencies_df,
      by = c("Substitution_machine", "Year")
    ) %>% 
    dplyr::mutate(
      Substitution_efficiency = dplyr::coalesce(Substitution_efficiency, Backstop_Efficiency)
    ) %>% 
    dplyr::select(-Last.stage)
  
  return(collated_machine_substitution_efficiencies)
}




# Determining substituting efficiencies without breakdown -----------------

# This function calculates the shares of each fossil fuel group use by each machine at the national level
# Summing across machines for a given fossil fuel group should give unity
calc_share_ff_by_machine_national <- function(.tidy_iea_df,
                                              tidy_D_rev_df){
  
  # Building shares of use of each product by each machine
  # Need to normalise as some are used in other industries in D_rev, not only final-to-useful devices
  tidy_shares_product_machine <- tidy_D_rev_df %>% 
    group_by(Country, Method, Energy.type, Last.stage, Year, Product) %>% 
    dplyr::mutate(
      Share_Product_Machine = Share / sum(Share)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-tidyselect::any_of(c("Last.stage", "Share"))) 
  
  # Building shares of use of each product within each product group
  tidy_shares_product_in_each_group_excl_elec <- EROITools::calc_share_ff_use_by_product_by_group(.tidy_iea_df, share = "Share_Product_Group") %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-tidyselect::any_of(c("Unit", "Total_Group_Use", "product_without_origin", "Total_Product_Use", "Non_Energy_Uses", "Last.stage")))
  
  tidy_shares_product_in_each_group_incl_elec <- EROITools::calc_shares_ff_by_group_inc_elec_heat(.tidy_iea_df) %>% 
    rename(Share_Product_Group = Share) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-tidyselect::any_of(c("Unit", "Total_Group_Use", "product_without_origin", "Total_Product_Use", "Non_Energy_Uses", "Last.stage"))) %>% 
    dplyr::mutate(
      Product = stringr::str_remove(Product, " \\[.*\\]")
    )
  
  tidy_shares_product_in_each_group <- dplyr::bind_rows(
    tidy_shares_product_in_each_group_excl_elec,
    tidy_shares_product_in_each_group_incl_elec
  )
  
  # Building shares of use of each product group by each machine
  tidy_shares_ff_by_machine_national <- tidy_shares_product_in_each_group %>% 
    dplyr::left_join(
      tidy_shares_product_machine,
      by = c("Country", "Method", "Energy.type", "Year", "Product")
    ) %>% 
    dplyr::filter(!is.na(Share_Product_Machine)) %>%
    # Adjusting so that the sum of shares is 1
    dplyr::mutate(
      Share_Product_Group_Machine = Share_Product_Group * Share_Product_Machine
    ) %>%
    dplyr::group_by(Country, Method, Energy.type, Year, Product.Group, Energy.stage) %>%
    dplyr::mutate(
      Share_Product_Group_Machine = Share_Product_Group_Machine / sum(Share_Product_Group_Machine)
    ) %>%
    dplyr::group_by(Country, Method, Energy.type, Year, Product.Group, Energy.stage, Industry) %>%
    dplyr::summarise(
      Share_Product_Group_Machine = sum(Share_Product_Group_Machine)
    )
  
  # Check shares
  tidy_shares_ff_by_machine_national %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Product.Group, Energy.stage) %>%
    dplyr::summarise(sum_shares = sum(Share_Product_Group_Machine)) %>% dplyr::filter(abs(sum_shares - 1) > 1e-4) %>% nrow() %>% testthat::expect_equal(0)
  
  return(tidy_shares_ff_by_machine_national)
}



# This function calculates the shares of each fossil fuel group use by each machine at the global level
# Summing across machines for a given fossil fuel group should give unity
calc_share_ff_by_machine_global <- function(share_use_ff_group_by_country_df,
                                            share_use_ff_by_machine_national_df){

  tidy_share_ff_by_machine_global <- share_use_ff_group_by_country_df %>% 
    dplyr::left_join(
      share_use_ff_by_machine_national_df,
      by = c("Country", "Method", "Energy.type", "Year", "Product.Group", "Energy.stage")
    ) %>% 
    dplyr::mutate(
      Share_FF_Country_Machine = Share_FF_Use_By_Country * Share_Product_Group_Machine
    ) %>% 
    dplyr::select(-Share_FF_Use_By_Country, -Share_Product_Group_Machine)

  # Check shares
  tidy_share_ff_by_machine_global %>% 
    dplyr::group_by(Method, Energy.type, Year, Product.Group, Energy.stage) %>%
    dplyr::summarise(sum_shares = sum(Share_FF_Country_Machine)) %>% dplyr::filter(abs(sum_shares - 1) > 1e-4) %>% nrow() %>% testthat::expect_equal(0)
  
  return(tidy_share_ff_by_machine_global)
}



# Determining substituting efficiencies by end-use ------------------------

# Determines, for each country, the share of each product group that ends-up being used by each machine, within each end-use
calc_share_ff_by_machine_national_by_eu <- function(tidy_national_shares_by_end_use_df,
                                                    tidy_D_rev_df,
                                                    machine_to_end_use_df){
  
  # For each product group, shares of product use in each end-use
  shares_product_by_eu <- tidy_national_shares_by_end_use_df %>% 
    dplyr::filter(Energy.stage %in% c("Useful (fuel)", "Useful (fuel+elec+heat)")) %>% 
    mutate(
      Product = stringr::str_remove(product_without_origin, " \\[.*\\]")
    ) %>% 
    select(-product_without_origin) %>% 
    group_by(Country, Method, Energy.type, Year, EU_category, Product, Product.Group, Energy.stage) %>% 
    summarise(
      Share_Group_Input = sum(Share_Group_Input)
    )
  
  # Shares of product use by each machine within each end-use
  shares_product_by_machine <- tidy_D_rev_df %>% 
    dplyr::left_join(machine_to_end_use_df, by = c("Industry")) %>% 
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, Product, EU_category) %>% 
    dplyr::mutate(
      Share_Product_Use_Machine = Share / sum(Share)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-tidyselect::all_of(c("Share", "Last.stage")))

  # Determining, for each product group, its share use by each machine, within each end-use
  share_ff_by_machine_national_by_eu <-  shares_product_by_eu %>% 
    dplyr::left_join(
      shares_product_by_machine,
      by = c("Country", "Method", "Energy.type", "Year", "Product", "EU_category")
    ) %>% 
    dplyr::mutate(
      Share_Product_Group_Machine_By = Share_Group_Input * Share_Product_Use_Machine
    ) %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Product.Group, EU_category, Energy.stage, Industry) %>% 
    dplyr::summarise(
      Share_Product_Group_Machine_By = sum(Share_Product_Group_Machine_By)
    )
  
  # Check shares
  share_ff_by_machine_national_by_eu %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Product.Group, EU_category, Energy.stage) %>%
    dplyr::summarise(sum_shares = sum(Share_Product_Group_Machine_By)) %>% dplyr::filter(abs(sum_shares - 1) > 1e-4) %>% nrow() %>% testthat::expect_equal(0)
    
  return(share_ff_by_machine_national_by_eu)
}



# Determines, at the global level, the share of each product group that ends-up being used by each machine, within each end-use
calc_share_ff_by_machine_global_by_eu <- function(share_ff_group_use_by_end_use_by_country_df,
                                                  share_use_ff_by_machine_national_by_eu_df){

  # Exclude elec and heat on their own
  share_use_ff_by_machine_national_by_eu_adapt <- share_use_ff_by_machine_national_by_eu_df %>% 
    dplyr::filter(Energy.stage %in% c("Useful (fuel)", "Useful (fuel+elec+heat)"))
  
  share_ff_by_machine_global_by_eu <- share_ff_group_use_by_end_use_by_country_df %>%
    dplyr::left_join(
      share_use_ff_by_machine_national_by_eu_adapt,
      by = c("Country", "Method", "Energy.type", "Year", "Product.Group", "EU_category", "Energy.stage")
    ) %>%
    dplyr::filter(
      !is.na(Share_Product_Group_Machine_By)
    ) %>%
    dplyr::mutate(
      Global_share_Product_Group_Machine_by = Share_fec_by_ff_group_by_eu * Share_Product_Group_Machine_By
    ) %>%
    dplyr::group_by(Method, Energy.type, Year, Product.Group, EU_category, Energy.stage) %>% 
    dplyr::mutate(
      Global_share_Product_Group_Machine_by = Global_share_Product_Group_Machine_by / sum(Global_share_Product_Group_Machine_by)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-tidyselect::any_of(c("Share_fec_by_ff_group_by_eu", "Share_Product_Group_Machine_By")))

  # Check shares
  share_ff_by_machine_global_by_eu %>% 
    dplyr::group_by(Method, Energy.type, Year, Product.Group, EU_category, Energy.stage) %>%
    dplyr::summarise(sum_shares = sum(Global_share_Product_Group_Machine_by)) %>% dplyr::filter(abs(sum_shares - 1) > 1e-4) %>% nrow() %>% testthat::expect_equal(0)
  
  return(share_ff_by_machine_global_by_eu)
}




# Determining substituting efficiencies by final demand sector ------------

# Determines, for each country, the share of each product group that ends-up being used by each machine, within each final demand sector
calc_share_ff_by_machine_national_by_fds <- function(tidy_national_shares_by_fds_df,
                                                     share_national_use_product_by_machine_by_fds_df){

  share_ff_by_machine_national_by_fds <- tidy_national_shares_by_fds_df %>% 
    dplyr::mutate(
      Product = stringr::str_remove(product_without_origin, " \\[.*\\]")
    ) %>% 
    dplyr::select(-product_without_origin) %>% 
    dplyr::left_join(
      share_national_use_product_by_machine_by_fds_df,
      by = c("Country", "Method", "Year", "Product", "Final_Demand_Sector_Category")
    ) %>% 
    dplyr::mutate(
      Share_Product_Group_Machine_By = Share_Group_Demand * Share_Product_Use_By_Machine_FDSC
    ) %>% 
    dplyr::select(-tidyselect::any_of(c("Share_Group_Demand", "Share_Product_Use_By_Machine_FDSC"))) %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Energy.stage, Industry) %>% 
    dplyr::summarise(
      Share_Product_Group_Machine_By = sum(Share_Product_Group_Machine_By)
    )

  # Check shares
  share_ff_by_machine_national_by_fds %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Product.Group, Energy.stage, Final_Demand_Sector_Category) %>%
    dplyr::summarise(sum_shares = sum(Share_Product_Group_Machine_By)) %>% dplyr::filter(abs(sum_shares - 1) > 1e-4) %>% nrow() %>% testthat::expect_equal(0)
  
  return(share_ff_by_machine_national_by_fds)
}


# Determines, at the global level, the share of each product group that ends-up being used by each machine, within each final demand sector
calc_share_ff_by_machine_global_by_fds <- function(share_ff_group_use_by_fds_by_country_df,
                                                   share_use_ff_by_machine_national_by_fds_df){

  share_ff_by_machine_global_by_fds <- share_ff_group_use_by_fds_by_country_df %>% 
    dplyr::left_join(
      share_use_ff_by_machine_national_by_fds_df,
      by = c("Country", "Method", "Energy.type", "Year", "Product.Group", "Energy.stage", "Final_Demand_Sector_Category")
    ) %>% 
    dplyr::filter(!is.na(Share_Product_Group_Machine_By)) %>% 
    dplyr::mutate(
      Global_share_Product_Group_Machine_by = Share_fec_by_ff_group_by_fdsc * Share_Product_Group_Machine_By,
    ) %>% 
    dplyr::group_by(Method, Energy.type, Year, Product.Group, Final_Demand_Sector_Category, Energy.stage) %>%
    # Adjusting so that the sum of shares equals 1
    dplyr::mutate(
      Global_share_Product_Group_Machine_by = Global_share_Product_Group_Machine_by / sum(Global_share_Product_Group_Machine_by)
    ) %>% 
    dplyr::select(-Share_Product_Group_Machine_By, -Share_fec_by_ff_group_by_fdsc)
  
  # Check shares
  share_ff_by_machine_global_by_fds %>% 
    dplyr::group_by(Method, Energy.type, Year, Product.Group, Energy.stage, Final_Demand_Sector_Category) %>%
    dplyr::summarise(sum_shares = sum(Global_share_Product_Group_Machine_by)) %>% dplyr::filter(abs(sum_shares - 1) > 1e-4) %>% nrow() %>% testthat::expect_equal(0)

  return(share_ff_by_machine_global_by_fds)
}


# Determines the average efficiency of substituting fossil fuels, without breakdown
calc_efficiency_substitution <- function(share_use_ff_by_machine_df,
                                         machine_efficiencies_df,
                                         level = c("national", "global")){
  
  level <- match.arg(level)
  
  # Binding the share of use of fossil fuels by machine with the efficiencies of substitution
  share_use_ff_with_efficiencies <- share_use_ff_by_machine_df %>% 
    dplyr::left_join(
      machine_efficiencies_df,
      by = c("Country", "Method", "Energy.type", "Year", "Industry")
    ) %>% 
    dplyr::mutate(
      Substitution_efficiency = dplyr::coalesce(Substitution_efficiency, Backstop_Efficiency)
    )
  
  # Checking that all shares have an efficiency of substitution
  share_use_ff_with_efficiencies %>% filter(is.na(Substitution_efficiency)) %>% nrow() %>% as.numeric() |> testthat::expect_equal(0)
  
  # Determining the weighted efficiency of substitution
  if (level == "national"){
  efficiency_substitution <- share_use_ff_with_efficiencies %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Product.Group, Energy.stage) %>% 
    dplyr::summarise(
      Efficiency_FF_Substitution = sum(Share_Product_Group_Machine * Substitution_efficiency)
    )
  } else if (level == "global"){
    efficiency_substitution <- share_use_ff_with_efficiencies %>% 
      dplyr::group_by(Method, Energy.type, Year, Product.Group, Energy.stage) %>% 
      dplyr::summarise(
        Efficiency_FF_Substitution = sum(Share_FF_Country_Machine * Substitution_efficiency)
      ) %>% 
      dplyr::mutate(Country = "WRLD") %>% 
      dplyr::relocate(Country, .before = Method)
  }
  
  return(efficiency_substitution)
}



# Determines the average efficiency of substituting fossil fuels, with a breakdown by "BY"
calc_efficiency_substitution_by <- function(share_use_ff_by_machine_by_df,
                                            machine_efficiencies_df,
                                            level = c("national", "global"),
                                            by = "EU_category"){
  
  level <- match.arg(level)
  
  # Binding the share of use of fossil fuels by machine with the efficiencies of substitution
  share_use_ff_with_efficiencies <- share_use_ff_by_machine_by_df %>% 
    dplyr::left_join(
      machine_efficiencies_df,
      by = c("Country", "Method", "Energy.type", "Year", "Industry")
    ) %>% 
    dplyr::mutate(
      Substitution_efficiency = dplyr::coalesce(Substitution_efficiency, Backstop_Efficiency)
    )
  
  # Checking that all shares have an efficiency of substitution
  share_use_ff_with_efficiencies %>% filter(is.na(Substitution_efficiency)) %>% nrow() %>% testthat::expect_equal(0)
  
  # Determining the weighted efficiency of substitution
  if (level == "national"){
    efficiency_substitution <- share_use_ff_with_efficiencies %>% 
      dplyr::group_by(Country, Method, Energy.type, Year, Product.Group, Energy.stage, .data[[by]]) %>% 
      dplyr::summarise(
        Efficiency_FF_Substitution_by = sum(Share_Product_Group_Machine_By * Substitution_efficiency)
      )
  } else if (level == "global"){
    efficiency_substitution <- share_use_ff_with_efficiencies %>% 
      dplyr::group_by(Method, Energy.type, Year, Product.Group, Energy.stage, .data[[by]]) %>% 
      dplyr::summarise(
        Efficiency_FF_Substitution_by = sum(Global_share_Product_Group_Machine_by * Substitution_efficiency)
      ) %>% 
      dplyr::mutate(Country = "WRLD") %>% 
      dplyr::relocate(Country, .before = Method)
  }
  
  return(efficiency_substitution)
}
