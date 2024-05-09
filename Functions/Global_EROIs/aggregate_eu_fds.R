
# This function calculates the global shares of each product use within each fossil fuel group,
# at each different final energy stage, for each different end-use category
calc_global_shares_by_end_use <- function(PFU_output_df,
                                          specified_world_iea_data_DTA_df,
                                          machine_to_end_use_category_df,
                                          # Using this last argument allows to pick up the combinations of end use categories and years
                                          # for which electricity and heat actually are used in the data, and avoids NAs in joins after.
                                          tidy_global_useful_erois_by_eu_df,
                                          energy_units){
  
  # 1) Calculates fossil fuel inputs by product and EU category
  
  # All inputs by product and EU category
  inputs_by_product_eu_df <- PFU_output_df %>% 
    dplyr::filter(Last.stage == "Useful") %>% 
    tidyr::pivot_longer(cols = -c("Country", "Method", "Energy.type", "Last.stage", "Year"), names_to = "matnames", values_to = "matvals") %>%
    dplyr::filter(matnames == "U") %>%
    matsindf::expand_to_tidy() %>%
    dplyr::filter(matvals != 0) %>%
    dplyr::select(-coltypes, -rowtypes, -matnames) %>%
    dplyr::rename(
      Product = rownames,
      Industry = colnames,
      Energy_Input = matvals
    ) %>%
    dplyr::filter(stringr::str_detect(Industry, "->")) %>%
    dplyr::filter(Industry != "Non-energy consumption -> NEU") %>%
    dplyr::left_join(machine_to_end_use_category_df, by = "Industry") %>%
    # Exclude NAs in EU_category, which correspond to end-uses that are routed to no category
    dplyr::filter(!is.na(EU_category)) %>% 
    dplyr::group_by(Method, Energy.type, Year, Product, EU_category) %>%
    dplyr::summarise(
      Energy_Inputs = sum(Energy_Input)
    ) %>%
    dplyr::mutate(
      Country = "WRLD",
      Unit = energy_units
    ) %>%
    dplyr::relocate(
      Country, .before = Method
    )
  
  # Fossil fuel inputs only, by product and EU category
  ff_inputs_by_product_eu_df <- inputs_by_product_eu_df %>%
    dplyr::filter(
      Product %in% c(
        IEATools::oil_and_oil_products,
        IEATools::coal_and_coal_products,
        IEATools::primary_gas_products
      )
    )
  
  # 2) Determines shares of specified electricity and heat products
  # at the Useful (electrcity) and Useful (heat) energy stages
  
  # First, Useful (electricity) energy stage
  list_elec_EU_categories_year <- tidy_global_useful_erois_by_eu_df %>% 
    dplyr::filter(Product %in% c("Electricity [from Oil products]", "Electricity [from Coal products]", "Electricity [from Natural gas]")) %>% 
    dplyr::ungroup() %>%
    dplyr::select(Year, EU_category) %>% 
    dplyr::distinct()
  
  shares_elect <- EROITools::calc_shares_elec_by_ff_group(specified_world_iea_data_DTA_df,
                                                          share = "Share_Group_Input") %>% 
    dplyr::mutate(
      Energy.stage = "Useful (electricity)"
    ) %>% 
    dplyr::select(-Last.stage) %>% 
    dplyr::inner_join(list_elec_EU_categories_year, by = "Year")
  
  
  # Second, Useful (heat) energy stage
  list_heat_EU_categories_year <- tidy_global_useful_erois_by_eu_df %>% 
    dplyr::filter(Product %in% c("Heat [from Oil products]", "Heat [from Coal products]", "Heat [from Natural gas]")) %>% 
    dplyr::ungroup() %>%
    dplyr::select(Year, EU_category) %>% 
    dplyr::distinct()
  
  shares_heat <- EROITools::calc_shares_heat_by_ff_group(specified_world_iea_data_DTA_df,
                                                         share = "Share_Group_Input") %>%
    dplyr::mutate(
      Energy.stage = "Useful (heat)"
    ) %>% 
    dplyr::select(-Last.stage) %>% 
    dplyr::inner_join(list_heat_EU_categories_year, by = "Year")
  
  
  # 3) Determining specified electricity and heat inputs to each end-use category
  share_elec_heat_supply_by_ff_group <- dplyr::bind_rows(
    EROITools::calc_share_elec_supply_by_ff_group(specified_world_iea_data_DTA_df) %>% 
      dplyr::mutate(Product = "Electricity"),
    EROITools::calc_share_heat_supply_by_ff_group(specified_world_iea_data_DTA_df) %>% 
      dplyr::mutate(Product = "Heat")
  ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Unit)
  
  # First, find electricity (from fossil fuels) inputs to each EU category
  elec_inputs_eu_df <- inputs_by_product_eu_df %>% 
    dplyr::filter(
      Product == "Electricity"
    ) %>%
    dplyr::left_join(
      share_elec_heat_supply_by_ff_group, by = c("Country", "Method", "Energy.type", "Year", "Product")
    ) %>%
    dplyr::mutate(
      Energy_Inputs_FF_Elec = Share * Energy_Inputs
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Energy_Inputs, -Share, -Product, -Energy.stage, -Last.stage) %>% 
    dplyr::left_join(
      shares_elect %>% 
        dplyr::select(-Energy.stage),
      by = c("Country", "Method", "Energy.type", "Year", "Product.Group", "EU_category")
    ) %>%
    dplyr::mutate(
      Energy_Inputs = Energy_Inputs_FF_Elec * Share_Group_Input
    ) %>% 
    dplyr::select(-Share_Group_Input, -Energy_Inputs_FF_Elec)
  
  # Second, find heat (from fossil fuels) inputs to each EU category
  heat_inputs_eu_df <- inputs_by_product_eu_df %>% 
    dplyr::filter(
      Product == "Heat"
    ) %>%
    dplyr::left_join(
      share_elec_heat_supply_by_ff_group, by = c("Country", "Method", "Energy.type", "Year", "Product")
    ) %>%
    dplyr::mutate(
      Energy_Inputs_FF_Heat = Share * Energy_Inputs
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Energy_Inputs, -Share, -Product, -Energy.stage, -Last.stage) %>% 
    dplyr::left_join(
      shares_heat %>% 
        dplyr::select(-Energy.stage),
      by = c("Country", "Method", "Energy.type", "Year", "Product.Group", "EU_category")
    ) %>%
    dplyr::mutate(
      Energy_Inputs = Energy_Inputs_FF_Heat * Share_Group_Input
    ) %>% 
    dplyr::select(-Share_Group_Input, -Energy_Inputs_FF_Heat)
  
  # Third, binding these together:
  elec_heat_inputs_eu_df <- dplyr::bind_rows(
    elec_inputs_eu_df,
    heat_inputs_eu_df
  )
  
  # 4) Calculates shares of each product use, within each EU category, for each product group, 
  # for the "Useful (fuel)" energy stage
  
  # All fossil fuels together, excluding electricity and heat
  total_ff_inputs_excl_elect_heat_1 <- ff_inputs_by_product_eu_df %>%
    dplyr::mutate(Product.Group = "All fossil fuels") %>%
    dplyr::group_by(Country, Method, Energy.type, Year, EU_category, Product.Group, Unit) %>%
    dplyr::summarise(
      Sum_Energy_Inputs_By_Group = sum(Energy_Inputs)
    )
  
  # "Oil and gas" and "Coal products" groups, excluding electricity and heat
  total_ff_inputs_excl_elect_heat_2 <- ff_inputs_by_product_eu_df %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        Product %in% IEATools::coal_and_coal_products ~ "Coal products"
      )
    ) %>%
    dplyr::group_by(Country, Method, Energy.type, Year, EU_category, Product.Group, Unit) %>%
    dplyr::summarise(
      Sum_Energy_Inputs_By_Group = sum(Energy_Inputs)
    )
  
  # "Oil products" and "Natural gas", excluding electricity and heat
  total_ff_inputs_excl_elect_heat_3 <- ff_inputs_by_product_eu_df %>%
    dplyr::filter(Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% IEATools::oil_and_oil_products ~ "Oil products",
        Product %in% IEATools::primary_gas_products ~ "Natural gas"
      )
    ) %>%
    dplyr::group_by(Country, Method, Energy.type, Year, EU_category, Product.Group, Unit) %>%
    dplyr::summarise(
      Sum_Energy_Inputs_By_Group = sum(Energy_Inputs)
    )
  
  # Shares all fossil fuels together, excluding electricity and heat
  share_excl_elect_heat_1 <- ff_inputs_by_product_eu_df %>%
    dplyr::mutate(
      Product.Group = "All fossil fuels"
    ) %>%
    dplyr::left_join(
      total_ff_inputs_excl_elect_heat_1, by = c("Country", "Method", "Energy.type", "Year", "EU_category", "Product.Group", "Unit")
    ) %>%
    dplyr::mutate(
      Share_Group_Input = Energy_Inputs / Sum_Energy_Inputs_By_Group
    ) %>%
    dplyr::mutate(
      Energy.stage = "Useful (fuel)"
    )
  
  # Shares "Oil and gas" and "Coal products" groups, excluding electricity and heat
  share_excl_elect_heat_2 <- ff_inputs_by_product_eu_df %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        Product %in% IEATools::coal_and_coal_products ~ "Coal products"
      )
    ) %>%
    dplyr::left_join(
      total_ff_inputs_excl_elect_heat_2, by = c("Country", "Method", "Energy.type", "Year", "EU_category", "Product.Group", "Unit")
    ) %>%
    dplyr::mutate(
      Share_Group_Input = Energy_Inputs / Sum_Energy_Inputs_By_Group
    ) %>%
    dplyr::mutate(
      Energy.stage = "Useful (fuel)"
    )
  
  # Shares "Oil products" and "Natural gas", excluding electricity and heat
  share_excl_elect_heat_3 <- ff_inputs_by_product_eu_df %>%
    dplyr::filter(Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% IEATools::oil_and_oil_products ~ "Oil products",
        Product %in% IEATools::primary_gas_products ~ "Natural gas"
      )
    ) %>%
    dplyr::left_join(
      total_ff_inputs_excl_elect_heat_3, by = c("Country", "Method", "Energy.type", "Year", "EU_category", "Product.Group", "Unit")
    ) %>%
    dplyr::mutate(
      Share_Group_Input = Energy_Inputs / Sum_Energy_Inputs_By_Group
    ) %>%
    dplyr::mutate(
      Energy.stage = "Useful (fuel)"
    )
  
  # Gathering all shares together for the "Useful (fuel)" energy stage.
  all_shares_excl_elect_heat <- dplyr::bind_rows(
    share_excl_elect_heat_1, share_excl_elect_heat_2, share_excl_elect_heat_3
  ) %>%
    dplyr::select(-Energy_Inputs, -Sum_Energy_Inputs_By_Group, -Unit)
  
  
  # 5) Calculates shares of each product use, within each EU category, for each product group, 
  # for the "Useful (fuel+elec+heat)" energy stage
  
  # All fossil fuels together, including electricity and heat
  total_ff_inputs_inc_elec_heat_1 <- ff_inputs_by_product_eu_df %>%
    dplyr::bind_rows(elec_heat_inputs_eu_df %>% dplyr::filter(Product.Group == "All fossil fuels")) %>%
    dplyr::mutate(Product.Group = "All fossil fuels") %>%
    dplyr::group_by(Country, Method, Energy.type, Year, EU_category, Product.Group, Unit) %>%
    dplyr::summarise(
      Sum_Energy_Inputs_By_Group = sum(Energy_Inputs)
    )
  
  # "Oil and gas products" and "Coal products" product groups, including electricity and heat
  total_ff_inputs_inc_elec_heat_2 <- ff_inputs_by_product_eu_df %>%
    dplyr::bind_rows(elec_heat_inputs_eu_df %>% dplyr::filter(Product.Group %in% c("Oil and gas products", "Coal products"))) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        Product %in% IEATools::coal_and_coal_products ~ "Coal products",
        TRUE ~ Product.Group
      )
    ) %>%
    dplyr::group_by(Country, Method, Energy.type, Year, EU_category, Product.Group, Unit) %>%
    dplyr::summarise(
      Sum_Energy_Inputs_By_Group = sum(Energy_Inputs)
    )
  
  # "Oil products" and "Natural gas" product groups, including electricity and heat
  total_ff_inputs_inc_elec_heat_3 <- ff_inputs_by_product_eu_df %>%
    dplyr::filter(Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>%
    dplyr::bind_rows(elec_heat_inputs_eu_df %>% dplyr::filter(Product.Group %in% c("Oil products", "Natural gas"))) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% IEATools::oil_and_oil_products ~ "Oil products",
        Product %in% IEATools::primary_gas_products ~ "Natural gas",
        TRUE ~ Product.Group
      )
    ) %>%
    dplyr::group_by(Country, Method, Energy.type, Year, EU_category, Product.Group, Unit) %>%
    dplyr::summarise(
      Sum_Energy_Inputs_By_Group = sum(Energy_Inputs)
    )
  
  # Shares all fossil fuels together, including electricity and heat
  share_inc_elec_heat_1 <- ff_inputs_by_product_eu_df %>%
    dplyr::bind_rows(elec_heat_inputs_eu_df %>% dplyr::filter(Product.Group == "All fossil fuels")) %>%
    dplyr::mutate(
      Product.Group = "All fossil fuels"
    ) %>%
    dplyr::left_join(
      total_ff_inputs_inc_elec_heat_1, by = c("Country", "Method", "Energy.type", "Year", "EU_category", "Product.Group", "Unit")
    ) %>%
    dplyr::mutate(
      Share_Group_Input = Energy_Inputs / Sum_Energy_Inputs_By_Group
    ) %>%
    dplyr::mutate(
      Energy.stage = "Useful (fuel+elec+heat)"
    ) %>%
    dplyr::select(-Unit)
  
  # Shares "Oil and gas products" and "Coal products" product groups, including electricity and heat
  share_inc_elec_heat_2 <- ff_inputs_by_product_eu_df %>%
    dplyr::bind_rows(elec_heat_inputs_eu_df %>% dplyr::filter(Product.Group %in% c("Oil and gas products", "Coal products"))) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        Product %in% IEATools::coal_and_coal_products ~ "Coal products",
        TRUE ~ Product.Group
      )
    ) %>%
    dplyr::left_join(
      total_ff_inputs_inc_elec_heat_2, by = c("Country", "Method", "Energy.type", "Year", "EU_category", "Product.Group", "Unit")
    ) %>%
    dplyr::mutate(
      Share_Group_Input = Energy_Inputs / Sum_Energy_Inputs_By_Group
    ) %>%
    dplyr::mutate(
      Energy.stage = "Useful (fuel+elec+heat)"
    ) %>%
    dplyr::select(-Unit)
  
  # Shares "Oil products" and "Natural gas" product groups, including electricity and heat
  share_inc_elec_heat_3 <- ff_inputs_by_product_eu_df %>%
    dplyr::filter(Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>%
    dplyr::bind_rows(elec_heat_inputs_eu_df %>% dplyr::filter(Product.Group %in% c("Oil products", "Natural gas"))) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% IEATools::oil_and_oil_products ~ "Oil products",
        Product %in% IEATools::primary_gas_products ~ "Natural gas",
        TRUE ~ Product.Group
      )
    ) %>%
    dplyr::left_join(
      total_ff_inputs_inc_elec_heat_3, by = c("Country", "Method", "Energy.type", "Year", "EU_category", "Product.Group", "Unit")
    ) %>%
    dplyr::mutate(
      Share_Group_Input = Energy_Inputs / Sum_Energy_Inputs_By_Group
    ) %>%
    dplyr::mutate(
      Energy.stage = "Useful (fuel+elec+heat)"
    ) %>%
    dplyr::select(-Unit)
  
  # Binding all shares for Useful (fuel+elec+heat) energy stage
  all_shares_inc_elec_heat <- dplyr::bind_rows(
    share_inc_elec_heat_1, share_inc_elec_heat_2, share_inc_elec_heat_3
  ) %>%
    dplyr::select(-Energy_Inputs, -Sum_Energy_Inputs_By_Group)
  
  
  # 6) Binding all shares, checking them, and calculating EROIs
  all_shares <- dplyr::bind_rows(
    all_shares_excl_elect_heat,
    all_shares_inc_elec_heat,
    shares_heat,
    shares_elect
  )
  
  # 7) Checking and returning shares
  all_shares %>% dplyr::group_by(Country, Method, Energy.type, Year, EU_category, Product.Group, Energy.stage) %>%
    dplyr::summarise(sum_shares = sum(Share_Group_Input)) %>%
    dplyr::filter(abs(sum_shares - 1) > 1e-4) %>%
    nrow() %>%
    testthat::expect_equal(0)
  
  return(all_shares)
}


# Aggregates global useful stage EROIs with a breakdown by end-use category
aggregate_global_useful_erois_by_eu <- function(tidy_global_useful_erois_by_eu_df,
                                                tidy_global_shares_by_end_use_df){
  
  # 1) Calculating and returning EROIs
  global_useful_erois_by_eu <- tidy_global_shares_by_end_use_df %>%
    dplyr::left_join(tidy_global_useful_erois_by_eu_df %>%
                       dplyr::select(-Last.stage), by = c("Country", "Method", "Energy.type", "Year", "Product", "EU_category")) %>%
    dplyr::filter(! is.na(Useful_Stage_EROI)) %>% # this should be empty ideally....!
    dplyr::group_by(Country, Method, Energy.type, Year, EU_category, Product.Group, Energy.stage, Eroi.method, Type, Boundary) %>%
    dplyr::summarise(
      Group.eroi.inverse = sum(Share_Group_Input * (1 / Useful_Stage_EROI)) / sum(Share_Group_Input),
      Group.eroi = 1 / Group.eroi.inverse
    ) %>%
    dplyr::select(-Group.eroi.inverse)
  
  return(global_useful_erois_by_eu)
}


# This function calculates the global shares of each product use within each fossil fuel group,
# at each different final energy stage, for each different final demand sector
calc_global_shares_by_fds <- function(PFU_output_df,
                                      specified_world_iea_data_DTA_df,
                                      fds_to_fds_category_df,
                                      # Using this last argument allows to pick up the combinations of final demand sector and years
                                      # for which electricity and heat actually are used in the data, and avoids NAs in joins after.
                                      tidy_global_useful_erois_by_fds_df,
                                      energy_units){
  
  # 1) Calculates fossil fuel inputs by product and final demand sector category
  
  # All products by final demand sector category
  demand_by_product_fds_df <- PFU_output_df %>% 
    dplyr::filter(Last.stage == "Final") %>% 
    tidyr::pivot_longer(cols = -c("Country", "Method", "Energy.type", "Last.stage", "Year"), names_to = "matnames", values_to = "matvals") %>% 
    dplyr::filter(matnames == "Y") %>% 
    matsindf::expand_to_tidy() %>% 
    dplyr::filter(matvals != 0) %>% 
    dplyr::select(-coltypes, -rowtypes, -matnames) %>% 
    dplyr::rename(
      Product = rownames,
      Final_Demand_Sector = colnames,
      Energy_Demand = matvals
    ) %>% 
    # Taking out non-energy use flows
    dplyr::filter(! stringr::str_detect(Final_Demand_Sector, "Non-energy use")) %>% 
    # Taking out Exports
    dplyr::filter(! stringr::str_detect(Final_Demand_Sector, "Exports")) %>% 
    # Taking out Stock changes
    dplyr::filter(! stringr::str_detect(Final_Demand_Sector, "Stock changes")) %>% 
    # Taking out statistical differences
    dplyr::filter(! stringr::str_detect(Final_Demand_Sector, "Statistical differences")) %>% 
    # Taking out Losses
    dplyr::filter(! stringr::str_detect(Final_Demand_Sector, "Losses")) %>% 
    # Taking out non-energy products
    dplyr::filter(! Product %in% IEATools::nonenergy_products) %>% 
    # Summing across final demand sector categories
    dplyr::left_join(fds_to_fds_category_df, by = "Final_Demand_Sector") %>%
    # Exclude NAs in Final Demand Sector Category, which correspond to Final Demand Sectors that are routed nowhere in the input csv file
    dplyr::filter(!is.na(Final_Demand_Sector_Category)) %>% 
    dplyr::group_by(Method, Energy.type, Year, Product, Final_Demand_Sector_Category) %>% 
    dplyr::summarise(
      Energy_Demand = sum(Energy_Demand)
    ) %>% 
    dplyr::mutate(
      Country = "WRLD",
      Unit = energy_units
    ) %>% 
    dplyr::relocate(
      Country, .before = Method
    )
  
  # Fossil fuel inputs only, by product and final demand sector category
  ff_demand_by_product_fds_df <- demand_by_product_fds_df %>% 
    dplyr::filter(
      Product %in% c(
        IEATools::oil_and_oil_products,
        IEATools::coal_and_coal_products,
        IEATools::primary_gas_products
      )
    )
  
  
  # 2) Determines shares of specified electricity and heat products
  # at the Useful (electrcity) and Useful (heat) energy stages
  
  # First, Useful (electricity) energy stage
  list_elec_fds_categories_year <- tidy_global_useful_erois_by_fds_df %>% 
    dplyr::filter(Product %in% c("Electricity [from Oil products]", "Electricity [from Coal products]", "Electricity [from Natural gas]")) %>% 
    dplyr::ungroup() %>%
    dplyr::select(Year, Final_Demand_Sector_Category) %>% 
    dplyr::distinct()
  
  shares_elect <- EROITools::calc_shares_elec_by_ff_group(specified_world_iea_data_DTA_df,
                                                          share = "Share_Group_Input") %>% 
    dplyr::mutate(
      Energy.stage = "Useful (electricity)"
    ) %>% 
    dplyr::rename(
      Share_Group_Demand = Share_Group_Input
    ) %>% 
    dplyr::select(-Last.stage) %>% 
    dplyr::inner_join(list_elec_fds_categories_year, by = "Year")
  
  
  # Second, Useful (heat) energy stage
  list_heat_fds_categories_year <- tidy_global_useful_erois_by_fds_df %>% 
    dplyr::filter(Product %in% c("Heat [from Oil products]", "Heat [from Coal products]", "Heat [from Natural gas]")) %>% 
    dplyr::ungroup() %>%
    dplyr::select(Year, Final_Demand_Sector_Category) %>% 
    dplyr::distinct()
  
  shares_heat <- EROITools::calc_shares_heat_by_ff_group(specified_world_iea_data_DTA_df,
                                                         share = "Share_Group_Input") %>%
    dplyr::mutate(
      Energy.stage = "Useful (heat)"
    ) %>% 
    dplyr::rename(
      Share_Group_Demand = Share_Group_Input
    ) %>% 
    dplyr::select(-Last.stage) %>% 
    dplyr::inner_join(list_heat_fds_categories_year, by = "Year")
  
  
  # 3) Determining specified electricity and heat inputs to each final demand sector category
  
  share_elec_heat_supply_by_ff_group <- dplyr::bind_rows(
    EROITools::calc_share_elec_supply_by_ff_group(specified_world_iea_data_DTA_df) %>% 
      dplyr::mutate(Product = "Electricity"),
    EROITools::calc_share_heat_supply_by_ff_group(specified_world_iea_data_DTA_df) %>% 
      dplyr::mutate(Product = "Heat")
  ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Unit)
  
  # First, find electricity (from fossil fuels) inputs to each final demand sector category
  elec_demand_fds_df <- demand_by_product_fds_df %>% 
    dplyr::filter(
      Product == "Electricity"
    ) %>%
    dplyr::left_join(
      share_elec_heat_supply_by_ff_group, by = c("Country", "Method", "Energy.type", "Year", "Product")
    ) %>%
    dplyr::mutate(
      Energy_Demand_FF_Elec = Share * Energy_Demand
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Energy_Demand, -Share, -Product, -Energy.stage, -Last.stage) %>% 
    dplyr::left_join(
      shares_elect %>% 
        dplyr::select(-Energy.stage),
      by = c("Country", "Method", "Energy.type", "Year", "Product.Group", "Final_Demand_Sector_Category")
    ) %>%
    dplyr::mutate(
      Energy_Demand = Energy_Demand_FF_Elec * Share_Group_Demand
    ) %>% 
    dplyr::select(-Share_Group_Demand, -Energy_Demand_FF_Elec)
  
  
  # Second, find heat (from fossil fuels) in each final demand sector
  heat_demand_fds_df  <- demand_by_product_fds_df %>% 
    dplyr::filter(
      Product == "Heat"
    ) %>%
    dplyr::left_join(
      share_elec_heat_supply_by_ff_group, by = c("Country", "Method", "Energy.type", "Year", "Product")
    ) %>%
    dplyr::mutate(
      Energy_Demand_FF_Heat = Share * Energy_Demand
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Energy_Demand, -Share, -Product, -Energy.stage, -Last.stage) %>% 
    dplyr::left_join(
      shares_heat %>% 
        dplyr::select(-Energy.stage),
      by = c("Country", "Method", "Energy.type", "Year", "Product.Group", "Final_Demand_Sector_Category")
    ) %>%
    dplyr::mutate(
      Energy_Demand = Energy_Demand_FF_Heat * Share_Group_Demand
    ) %>% 
    dplyr::select(-Share_Group_Demand, -Energy_Demand_FF_Heat)
  
  
  # Third, binding these together:
  elec_heat_demand_fds_df <- dplyr::bind_rows(
    elec_demand_fds_df,
    heat_demand_fds_df
  )
  
  
  # 4) Calculates shares of each product use, within each final demand sector category, for each product group, 
  # for the "Useful (fuel)" energy stage
  
  # All fossil fuels together, excluding electricity and heat
  total_ff_demand_excl_elect_heat_1 <- ff_demand_by_product_fds_df %>%
    dplyr::mutate(Product.Group = "All fossil fuels") %>%
    dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Unit) %>% 
    dplyr::summarise(
      Sum_Energy_Demand_By_Group = sum(Energy_Demand)
    )
  
  # "Oil and gas" and "Coal products" groups, excluding electricity and heat
  total_ff_demand_excl_elect_heat_2 <- ff_demand_by_product_fds_df %>% 
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        Product %in% IEATools::coal_and_coal_products ~ "Coal products"
      )
    ) %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Unit) %>% 
    dplyr::summarise(
      Sum_Energy_Demand_By_Group = sum(Energy_Demand)
    )
  
  # "Oil products" and "Natural gas", excluding electricity and heat
  total_ff_demand_excl_elect_heat_3 <- ff_demand_by_product_fds_df %>% 
    dplyr::filter(Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>% 
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% IEATools::oil_and_oil_products ~ "Oil products",
        Product %in% IEATools::primary_gas_products ~ "Natural gas"
      )
    ) %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Unit) %>% 
    dplyr::summarise(
      Sum_Energy_Demand_By_Group = sum(Energy_Demand)
    )
  
  
  # Shares all fossil fuels together, excluding electricity and heat
  share_excl_elect_heat_1 <- ff_demand_by_product_fds_df %>% 
    dplyr::mutate(
      Product.Group = "All fossil fuels"
    ) %>% 
    dplyr::left_join(
      total_ff_demand_excl_elect_heat_1, by = c("Country", "Method", "Energy.type", "Year", "Final_Demand_Sector_Category", "Product.Group", "Unit")
    ) %>% 
    dplyr::mutate(
      Share_Group_Demand = Energy_Demand / Sum_Energy_Demand_By_Group
    ) %>% 
    dplyr::mutate(
      Energy.stage = "Useful (fuel)"
    )
  
  
  # Shares "Oil and gas" and "Coal products" groups, excluding electricity and heat
  share_excl_elect_heat_2 <- ff_demand_by_product_fds_df %>% 
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        Product %in% IEATools::coal_and_coal_products ~ "Coal products"
      )
    ) %>% 
    dplyr::left_join(
      total_ff_demand_excl_elect_heat_2, by = c("Country", "Method", "Energy.type", "Year", "Final_Demand_Sector_Category", "Product.Group", "Unit")
    ) %>% 
    dplyr::mutate(
      Share_Group_Demand = Energy_Demand / Sum_Energy_Demand_By_Group
    ) %>% 
    dplyr::mutate(
      Energy.stage = "Useful (fuel)"
    )
  
  
  # Shares "Oil products" and "Natural gas", excluding electricity and heat
  share_excl_elect_heat_3 <- ff_demand_by_product_fds_df %>% 
    dplyr::filter(Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>% 
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% IEATools::oil_and_oil_products ~ "Oil products",
        Product %in% IEATools::primary_gas_products ~ "Natural gas"
      )
    ) %>% 
    dplyr::left_join(
      total_ff_demand_excl_elect_heat_3, by = c("Country", "Method", "Energy.type", "Year", "Final_Demand_Sector_Category", "Product.Group", "Unit")
    ) %>% 
    dplyr::mutate(
      Share_Group_Demand = Energy_Demand / Sum_Energy_Demand_By_Group
    ) %>% 
    dplyr::mutate(
      Energy.stage = "Useful (fuel)"
    )
  
  # Gathering all shares together for the "Useful (fuel)" energy stage.
  all_shares_excl_elect_heat <- dplyr::bind_rows(
    share_excl_elect_heat_1, share_excl_elect_heat_2, share_excl_elect_heat_3
  ) %>% 
    dplyr::select(-Energy_Demand, -Sum_Energy_Demand_By_Group, -Unit)
  
  
  
  # 5) Calculates shares of each product use, within each final demand sector category, for each product group, 
  # for the "Useful (fuel+elec+heat)" energy stage
  
  # All fossil fuels together, including electricity and heat
  total_ff_demand_inc_elec_heat_1 <- ff_demand_by_product_fds_df %>%
    dplyr::bind_rows(elec_heat_demand_fds_df %>% dplyr::filter(Product.Group == "All fossil fuels")) %>% 
    # Figure out cols concordance...!
    dplyr::mutate(Product.Group = "All fossil fuels") %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Unit) %>% 
    dplyr::summarise(
      Sum_Energy_Demand_By_Group = sum(Energy_Demand)
    )
  
  # "Oil and gas products" and "Coal products" product groups, including electricity and heat
  total_ff_demand_inc_elec_heat_2 <- ff_demand_by_product_fds_df %>% 
    dplyr::bind_rows(elec_heat_demand_fds_df %>% dplyr::filter(Product.Group %in% c("Oil and gas products", "Coal products"))) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        Product %in% IEATools::coal_and_coal_products ~ "Coal products",
        TRUE ~ Product.Group
      )
    ) %>%
    dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Unit) %>% 
    dplyr::summarise(
      Sum_Energy_Demand_By_Group = sum(Energy_Demand)
    )
  
  # "Oil products" and "Natural gas" product groups, including electricity and heat
  total_ff_demand_inc_elec_heat_3 <- ff_demand_by_product_fds_df %>% 
    dplyr::filter(Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>% 
    dplyr::bind_rows(elec_heat_demand_fds_df %>% dplyr::filter(Product.Group %in% c("Oil products", "Natural gas"))) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% IEATools::oil_and_oil_products ~ "Oil products",
        Product %in% IEATools::primary_gas_products ~ "Natural gas",
        TRUE ~ Product.Group
      )
    ) %>%
    dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Unit) %>% 
    dplyr::summarise(
      Sum_Energy_Demand_By_Group = sum(Energy_Demand)
    )
  
  
  # Shares all fossil fuels together, including electricity and heat
  share_inc_elec_heat_1 <- ff_demand_by_product_fds_df %>% 
    dplyr::bind_rows(elec_heat_demand_fds_df %>% dplyr::filter(Product.Group == "All fossil fuels")) %>% 
    dplyr::mutate(
      Product.Group = "All fossil fuels"
    ) %>% 
    dplyr::left_join(
      total_ff_demand_inc_elec_heat_1, by = c("Country", "Method", "Energy.type", "Year", "Final_Demand_Sector_Category", "Product.Group", "Unit")
    ) %>% 
    dplyr::mutate(
      Share_Group_Demand = Energy_Demand / Sum_Energy_Demand_By_Group
    ) %>% 
    dplyr::mutate(
      Energy.stage = "Useful (fuel+elec+heat)"
    ) %>% 
    dplyr::select(-Unit)
  
  
  # Shares "Oil and gas products" and "Coal products" product groups, including electricity and heat
  share_inc_elec_heat_2 <- ff_demand_by_product_fds_df %>% 
    dplyr::bind_rows(elec_heat_demand_fds_df %>% dplyr::filter(Product.Group %in% c("Oil and gas products", "Coal products"))) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        Product %in% IEATools::coal_and_coal_products ~ "Coal products",
        TRUE ~ Product.Group
      )
    ) %>% 
    dplyr::left_join(
      total_ff_demand_inc_elec_heat_2, by = c("Country", "Method", "Energy.type", "Year", "Final_Demand_Sector_Category", "Product.Group", "Unit")
    ) %>% 
    dplyr::mutate(
      Share_Group_Demand = Energy_Demand / Sum_Energy_Demand_By_Group
    ) %>% 
    dplyr::mutate(
      Energy.stage = "Useful (fuel+elec+heat)"
    ) %>% 
    dplyr::select(-Unit)
  
  
  # Shares "Oil products" and "Natural gas" product groups, including electricity and heat
  share_inc_elec_heat_3 <- ff_demand_by_product_fds_df %>% 
    dplyr::filter(Product %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>% 
    dplyr::bind_rows(elec_heat_demand_fds_df %>% dplyr::filter(Product.Group %in% c("Oil products", "Natural gas"))) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        Product %in% IEATools::oil_and_oil_products ~ "Oil products",
        Product %in% IEATools::primary_gas_products ~ "Natural gas",
        TRUE ~ Product.Group
      )
    ) %>% 
    dplyr::left_join(
      total_ff_demand_inc_elec_heat_3, by = c("Country", "Method", "Energy.type", "Year", "Final_Demand_Sector_Category", "Product.Group", "Unit")
    ) %>% 
    dplyr::mutate(
      Share_Group_Demand = Energy_Demand / Sum_Energy_Demand_By_Group
    ) %>% 
    dplyr::mutate(
      Energy.stage = "Useful (fuel+elec+heat)"
    ) %>% 
    dplyr::select(-Unit)
  
  # Binding all shares for Useful (fuel+elec+heat) energy stage
  all_shares_inc_elec_heat <- dplyr::bind_rows(
    share_inc_elec_heat_1, share_inc_elec_heat_2, share_inc_elec_heat_3
  ) %>% 
    dplyr::select(-Energy_Demand, -Sum_Energy_Demand_By_Group)
  
  
  # 6) Binding all shares, checking them, and calculating EROIs
  all_shares <- dplyr::bind_rows(
    all_shares_excl_elect_heat,
    all_shares_inc_elec_heat,
    shares_heat,
    shares_elect
  )
  
  # Checking shares and returning values
  all_shares %>% dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Energy.stage) %>%
    dplyr::summarise(sum_shares = sum(Share_Group_Demand)) %>%
    dplyr::filter(abs(sum_shares - 1) > 1e-4) %>%
    nrow() %>%
    testthat::expect_equal(0)
  
  return(all_shares)
}


# Aggregates global useful stage EROIs with a breakdown by final demand sector category
aggregate_global_useful_erois_by_fds <- function(tidy_global_useful_erois_by_fds_df,
                                                 tidy_global_shares_by_fds_df){

  # Calculating and returning EROIs
  global_useful_erois_by_fds <- tidy_global_shares_by_fds_df %>%
    dplyr::left_join(tidy_global_useful_erois_by_fds_df %>%
                       dplyr::select(-Last.stage), by = c("Country", "Method", "Energy.type", "Year", "Product", "Final_Demand_Sector_Category")) %>%
    dplyr::filter(! is.na(Useful_Stage_EROI)) %>%
    dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Energy.stage, Eroi.method, Type, Boundary) %>%
    dplyr::summarise(
      Group.eroi.inverse = sum(Share_Group_Demand * (1 / Useful_Stage_EROI)) / sum(Share_Group_Demand),
      Group.eroi = 1 / Group.eroi.inverse
    ) %>%
    dplyr::select(-Group.eroi.inverse)
  
  return(global_useful_erois_by_fds)
}




# This function calculates the national shares of each product use within each fossil fuel group,
# at each different final energy stage, for each different end-use category
calc_national_shares_by_end_use <- function(PFU_output_df,
                                            ecc_gma_df,
                                            machine_to_end_use_category_df,
                                            # Ideally this argument should be removed but convenient for now
                                            # as it allows to capture the relevant combinations of EU and elec/heat use, and limits subsequent NAs in join
                                            tidy_national_useful_erois_by_eu_df,
                                            energy_units){
  
  ### 0) Working on the set up ###
  
  # 0.a) Adapting the ECC - GMA data frame
  ecc_gma_adapted <- ecc_gma_df %>% 
    dplyr::mutate(
      Country = stringr::str_extract(Flow, "\\{.*\\}") %>% 
        stringr::str_remove("\\{") %>% 
        stringr::str_remove("\\}"),
      Flow = stringr::str_remove(Flow, "\\{.*\\}_"),
      product_without_origin = stringr::str_remove(Product, "\\{.*\\}_"),
    )
  
  # 0.b) Pulling out country list of the regions covered in the GMA data
  list_eroi_countries <- ecc_gma_adapted %>% 
    dplyr::ungroup() %>% 
    dplyr::select(Country) %>% 
    dplyr::distinct() %>% 
    dplyr::pull()
  
  # 0.c) Calculate the share of origin for each product, within each country
  shares_by_product_origin_df <- ecc_gma_adapted %>% 
    dplyr::filter(matnames %in% c("Y", "U_EIOU")) %>% 
    dplyr::mutate(
      product_origin = stringr::str_extract(Product, "\\{.*\\}") %>%
        stringr::str_remove("\\{") %>% 
        stringr::str_remove("\\}")
    ) %>% 
    # Adding up all consumption flows in one unique flow
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, Product, product_origin, product_without_origin, Unit) %>% 
    dplyr::summarise(
      FEN_by_product_origin = sum(abs(E.dot))
    ) %>% 
    # Now calculating share by supplying region
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, product_without_origin, Unit) %>% 
    dplyr::mutate(
      share_by_product_origin = FEN_by_product_origin / sum(FEN_by_product_origin)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-FEN_by_product_origin, -Unit)
  
  # 0.d) Checking sum of shares
  shares_by_product_origin_df %>% 
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, product_without_origin) %>% 
    dplyr::summarise(
      sum_shares = sum(share_by_product_origin)
    ) %>% 
    dplyr::filter(abs(sum_shares - 1) > 1e-4) %>% 
    nrow() %>% 
    testthat::expect_equal(0)
  
  # 0.e) Adding product without origin column
  tidy_national_useful_erois_by_eu_df <- tidy_national_useful_erois_by_eu_df %>% 
    dplyr::mutate(
      product_without_origin = stringr::str_remove(Product, "\\{.*\\}_")
    )
  
  
  # 1) Calculates fossil fuel inputs by product and EU category
  
  # All inputs by product and EU category
  inputs_by_product_eu_df <- PFU_output_df %>% 
    dplyr::filter(Country %in% list_eroi_countries) %>% 
    dplyr::filter(Last.stage == "Useful") %>% 
    tidyr::pivot_longer(cols = -c("Country", "Method", "Energy.type", "Last.stage", "Year"), names_to = "matnames", values_to = "matvals") %>%
    dplyr::filter(matnames == "U") %>%
    matsindf::expand_to_tidy() %>%
    dplyr::filter(matvals != 0) %>%
    dplyr::select(-coltypes, -rowtypes, -matnames) %>%
    dplyr::rename(
      Product = rownames,
      Industry = colnames,
      Energy_Input = matvals
    ) %>%
    dplyr::filter(stringr::str_detect(Industry, "->")) %>%
    dplyr::filter(Industry != "Non-energy consumption -> NEU") %>%
    dplyr::left_join(machine_to_end_use_category_df, by = "Industry") %>%
    # Exclude NAs in EU_category, which correspond to end-uses that are routed to no category
    dplyr::filter(!is.na(EU_category)) %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Product, EU_category) %>%
    dplyr::summarise(
      Energy_Inputs = sum(Energy_Input)
    ) %>%
    dplyr::mutate(
      Unit = energy_units
    ) %>%
    dplyr::relocate(
      Country, .before = Method
    )
  
  # Fossil fuel inputs only, by product and EU category
  ff_inputs_by_product_eu_df <- inputs_by_product_eu_df %>%
    dplyr::filter(
      Product %in% c(
        IEATools::oil_and_oil_products,
        IEATools::coal_and_coal_products,
        IEATools::primary_gas_products
      )
    ) %>% 
    # Adding origins to each product
    dplyr::rename(product_without_origin = Product) %>%
    dplyr::inner_join(
      shares_by_product_origin_df,
      by = c("Country", "Method", "Energy.type", "Year", "product_without_origin")
    ) %>% 
    dplyr::mutate(
      Energy_Inputs = Energy_Inputs * share_by_product_origin
    ) %>% 
    dplyr::ungroup() %>%
    dplyr::select(-product_origin, -Last.stage)
  
  
  # 2) Determines shares of specified electricity and heat products
  # at the Useful (electrcity) and Useful (heat) energy stages
  
  # First, Useful (electricity) energy stage
  list_elec_EU_categories_year_country <- tidy_national_useful_erois_by_eu_df %>% 
    dplyr::filter(product_without_origin %in% c("Electricity [from Oil products]", "Electricity [from Coal products]", "Electricity [from Natural gas]")) %>% 
    dplyr::ungroup() %>%
    dplyr::select(Country, Year, EU_category) %>% 
    dplyr::distinct()
  
  shares_elect <- EROITools::calc_shares_elec_by_ff_group(ecc_gma_adapted,
                                                          share = "Share_Group_Input") %>% 
    dplyr::mutate(
      Energy.stage = "Useful (electricity)"
    ) %>% 
    dplyr::select(-Last.stage) %>% 
    dplyr::inner_join(list_elec_EU_categories_year_country, by = c("Country", "Year")) %>% 
    dplyr::mutate(
      product_without_origin = stringr::str_remove(Product, "\\{.*\\}_")
    )
  
  
  # Second, Useful (heat) energy stage
  list_heat_EU_categories_year_country <- tidy_national_useful_erois_by_eu_df %>% 
    dplyr::filter(product_without_origin %in% c("Heat [from Oil products]", "Heat [from Coal products]", "Heat [from Natural gas]")) %>% 
    dplyr::ungroup() %>%
    dplyr::select(Country, Year, EU_category) %>% 
    dplyr::distinct()
  
  shares_heat <- EROITools::calc_shares_heat_by_ff_group(ecc_gma_adapted,
                                                         share = "Share_Group_Input") %>%
    dplyr::mutate(
      Energy.stage = "Useful (heat)"
    ) %>% 
    dplyr::select(-Last.stage) %>% 
    dplyr::inner_join(list_heat_EU_categories_year_country, by = c("Country", "Year")) %>% 
    dplyr::mutate(
      product_without_origin = stringr::str_remove(Product, "\\{.*\\}_")
    )
  
  
  
  # 3) Determining specified electricity and heat inputs to each end-use category
  
  share_elec_heat_supply_by_ff_group <- dplyr::bind_rows(
    EROITools::calc_share_elec_supply_by_ff_group(ecc_gma_adapted) %>% 
      dplyr::mutate(Product = "Electricity"),
    EROITools::calc_share_heat_supply_by_ff_group(ecc_gma_adapted) %>% 
      dplyr::mutate(Product = "Heat")
  ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Unit)
  
  # First, find electricity (from fossil fuels) inputs to each EU category
  elec_inputs_eu_df <- inputs_by_product_eu_df %>% 
    dplyr::filter(
      Product == "Electricity"
    ) %>%
    dplyr::left_join(
      share_elec_heat_supply_by_ff_group, by = c("Country", "Method", "Energy.type", "Year", "Product")
    ) %>% 
    dplyr::mutate(
      Energy_Inputs_FF_Elec = Share * Energy_Inputs
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Energy_Inputs, -Share, -Product, -Energy.stage, -Last.stage) %>% 
    dplyr::left_join(
      shares_elect %>% 
        dplyr::select(-Energy.stage),
      by = c("Country", "Method", "Energy.type", "Year", "Product.Group", "EU_category")
    ) %>%
    dplyr::mutate(
      Energy_Inputs = Energy_Inputs_FF_Elec * Share_Group_Input,
      product_without_origin = stringr::str_remove(Product, "\\{.*\\}_")
    ) %>% 
    dplyr::select(-Share_Group_Input, -Energy_Inputs_FF_Elec)
  
  # Second, find heat (from fossil fuels) inputs to each EU category
  heat_inputs_eu_df <- inputs_by_product_eu_df %>% 
    dplyr::filter(
      Product == "Heat"
    ) %>%
    dplyr::left_join(
      share_elec_heat_supply_by_ff_group, by = c("Country", "Method", "Energy.type", "Year", "Product")
    ) %>%
    dplyr::mutate(
      Energy_Inputs_FF_Heat = Share * Energy_Inputs
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Energy_Inputs, -Share, -Product, -Energy.stage, -Last.stage) %>% 
    dplyr::left_join(
      shares_heat %>% 
        dplyr::select(-Energy.stage),
      by = c("Country", "Method", "Energy.type", "Year", "Product.Group", "EU_category")
    ) %>%
    dplyr::mutate(
      Energy_Inputs = Energy_Inputs_FF_Heat * Share_Group_Input,
      product_without_origin = stringr::str_remove(Product, "\\{.*\\}_")
    ) %>% 
    dplyr::select(-Share_Group_Input, -Energy_Inputs_FF_Heat)
  
  # Third, binding these together:
  elec_heat_inputs_eu_df <- dplyr::bind_rows(
    elec_inputs_eu_df,
    heat_inputs_eu_df
  )
  
  
  # 4) Calculates shares of each product use, within each EU category, for each product group, 
  # for the "Useful (fuel)" energy stage
  
  # All fossil fuels together, excluding electricity and heat
  total_ff_inputs_excl_elect_heat_1 <- ff_inputs_by_product_eu_df %>%
    dplyr::mutate(Product.Group = "All fossil fuels") %>%
    dplyr::group_by(Country, Method, Energy.type, Year, EU_category, Product.Group, Unit) %>%
    dplyr::summarise(
      Sum_Energy_Inputs_By_Group = sum(Energy_Inputs)
    )
  
  # "Oil and gas" and "Coal products" groups, excluding electricity and heat
  total_ff_inputs_excl_elect_heat_2 <- ff_inputs_by_product_eu_df %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        product_without_origin %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        product_without_origin %in% IEATools::coal_and_coal_products ~ "Coal products"
      )
    ) %>%
    dplyr::group_by(Country, Method, Energy.type, Year, EU_category, Product.Group, Unit) %>%
    dplyr::summarise(
      Sum_Energy_Inputs_By_Group = sum(Energy_Inputs)
    )
  
  # "Oil products" and "Natural gas", excluding electricity and heat
  total_ff_inputs_excl_elect_heat_3 <- ff_inputs_by_product_eu_df %>%
    dplyr::filter(product_without_origin %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        product_without_origin %in% IEATools::oil_and_oil_products ~ "Oil products",
        product_without_origin %in% IEATools::primary_gas_products ~ "Natural gas"
      )
    ) %>%
    dplyr::group_by(Country, Method, Energy.type, Year, EU_category, Product.Group, Unit) %>%
    dplyr::summarise(
      Sum_Energy_Inputs_By_Group = sum(Energy_Inputs)
    )
  
  # Shares all fossil fuels together, excluding electricity and heat
  share_excl_elect_heat_1 <- ff_inputs_by_product_eu_df %>%
    dplyr::mutate(
      Product.Group = "All fossil fuels"
    ) %>%
    dplyr::left_join(
      total_ff_inputs_excl_elect_heat_1, by = c("Country", "Method", "Energy.type", "Year", "EU_category", "Product.Group", "Unit")
    ) %>%
    dplyr::mutate(
      Share_Group_Input = Energy_Inputs / Sum_Energy_Inputs_By_Group
    ) %>%
    dplyr::mutate(
      Energy.stage = "Useful (fuel)"
    )
  
  # Shares "Oil and gas" and "Coal products" groups, excluding electricity and heat
  share_excl_elect_heat_2 <- ff_inputs_by_product_eu_df %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        product_without_origin %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        product_without_origin %in% IEATools::coal_and_coal_products ~ "Coal products"
      )
    ) %>%
    dplyr::left_join(
      total_ff_inputs_excl_elect_heat_2, by = c("Country", "Method", "Energy.type", "Year", "EU_category", "Product.Group", "Unit")
    ) %>%
    dplyr::mutate(
      Share_Group_Input = Energy_Inputs / Sum_Energy_Inputs_By_Group
    ) %>%
    dplyr::mutate(
      Energy.stage = "Useful (fuel)"
    )
  
  # Shares "Oil products" and "Natural gas", excluding electricity and heat
  share_excl_elect_heat_3 <- ff_inputs_by_product_eu_df %>%
    dplyr::filter(product_without_origin %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        product_without_origin %in% IEATools::oil_and_oil_products ~ "Oil products",
        product_without_origin %in% IEATools::primary_gas_products ~ "Natural gas"
      )
    ) %>%
    dplyr::left_join(
      total_ff_inputs_excl_elect_heat_3, by = c("Country", "Method", "Energy.type", "Year", "EU_category", "Product.Group", "Unit")
    ) %>%
    dplyr::mutate(
      Share_Group_Input = Energy_Inputs / Sum_Energy_Inputs_By_Group
    ) %>%
    dplyr::mutate(
      Energy.stage = "Useful (fuel)"
    )
  
  
  
  # Gathering all shares together for the "Useful (fuel)" energy stage.
  all_shares_excl_elect_heat <- dplyr::bind_rows(
    share_excl_elect_heat_1, share_excl_elect_heat_2, share_excl_elect_heat_3
  ) %>%
    dplyr::select(-Energy_Inputs, -Sum_Energy_Inputs_By_Group, -Unit, -share_by_product_origin)
  
  
  # 5) Calculates shares of each product use, within each EU category, for each product group, 
  # for the "Useful (fuel+elec+heat)" energy stage
  
  # All fossil fuels together, including electricity and heat
  total_ff_inputs_inc_elec_heat_1 <- ff_inputs_by_product_eu_df %>%
    dplyr::bind_rows(elec_heat_inputs_eu_df %>% dplyr::filter(Product.Group == "All fossil fuels")) %>%
    dplyr::mutate(Product.Group = "All fossil fuels") %>%
    dplyr::group_by(Country, Method, Energy.type, Year, EU_category, Product.Group, Unit) %>%
    dplyr::summarise(
      Sum_Energy_Inputs_By_Group = sum(Energy_Inputs)
    )
  
  # "Oil and gas products" and "Coal products" product groups, including electricity and heat
  total_ff_inputs_inc_elec_heat_2 <- ff_inputs_by_product_eu_df %>%
    dplyr::bind_rows(elec_heat_inputs_eu_df %>% dplyr::filter(Product.Group %in% c("Oil and gas products", "Coal products"))) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        product_without_origin %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        product_without_origin %in% IEATools::coal_and_coal_products ~ "Coal products",
        TRUE ~ Product.Group
      )
    ) %>%
    dplyr::group_by(Country, Method, Energy.type, Year, EU_category, Product.Group, Unit) %>%
    dplyr::summarise(
      Sum_Energy_Inputs_By_Group = sum(Energy_Inputs)
    )
  
  # "Oil products" and "Natural gas" product groups, including electricity and heat
  total_ff_inputs_inc_elec_heat_3 <- ff_inputs_by_product_eu_df %>%
    dplyr::filter(product_without_origin %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>%
    dplyr::bind_rows(elec_heat_inputs_eu_df %>% dplyr::filter(Product.Group %in% c("Oil products", "Natural gas"))) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        product_without_origin %in% IEATools::oil_and_oil_products ~ "Oil products",
        product_without_origin %in% IEATools::primary_gas_products ~ "Natural gas",
        TRUE ~ Product.Group
      )
    ) %>%
    dplyr::group_by(Country, Method, Energy.type, Year, EU_category, Product.Group, Unit) %>%
    dplyr::summarise(
      Sum_Energy_Inputs_By_Group = sum(Energy_Inputs)
    )
  
  
  # Shares all fossil fuels together, including electricity and heat
  share_inc_elec_heat_1 <- ff_inputs_by_product_eu_df %>%
    dplyr::bind_rows(elec_heat_inputs_eu_df %>% dplyr::filter(Product.Group == "All fossil fuels")) %>%
    dplyr::mutate(
      Product.Group = "All fossil fuels"
    ) %>%
    dplyr::left_join(
      total_ff_inputs_inc_elec_heat_1, by = c("Country", "Method", "Energy.type", "Year", "EU_category", "Product.Group", "Unit")
    ) %>%
    dplyr::mutate(
      Share_Group_Input = Energy_Inputs / Sum_Energy_Inputs_By_Group
    ) %>%
    dplyr::mutate(
      Energy.stage = "Useful (fuel+elec+heat)"
    )
  
  # Shares "Oil and gas products" and "Coal products" product groups, including electricity and heat
  share_inc_elec_heat_2 <- ff_inputs_by_product_eu_df %>%
    dplyr::bind_rows(elec_heat_inputs_eu_df %>% dplyr::filter(Product.Group %in% c("Oil and gas products", "Coal products"))) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        product_without_origin %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        product_without_origin %in% IEATools::coal_and_coal_products ~ "Coal products",
        TRUE ~ Product.Group
      )
    ) %>%
    dplyr::left_join(
      total_ff_inputs_inc_elec_heat_2, by = c("Country", "Method", "Energy.type", "Year", "EU_category", "Product.Group", "Unit")
    ) %>%
    dplyr::mutate(
      Share_Group_Input = Energy_Inputs / Sum_Energy_Inputs_By_Group
    ) %>%
    dplyr::mutate(
      Energy.stage = "Useful (fuel+elec+heat)"
    )
  
  # Shares "Oil products" and "Natural gas" product groups, including electricity and heat
  share_inc_elec_heat_3 <- ff_inputs_by_product_eu_df %>%
    dplyr::filter(product_without_origin %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>%
    dplyr::bind_rows(elec_heat_inputs_eu_df %>% dplyr::filter(Product.Group %in% c("Oil products", "Natural gas"))) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        product_without_origin %in% IEATools::oil_and_oil_products ~ "Oil products",
        product_without_origin %in% IEATools::primary_gas_products ~ "Natural gas",
        TRUE ~ Product.Group
      )
    ) %>%
    dplyr::left_join(
      total_ff_inputs_inc_elec_heat_3, by = c("Country", "Method", "Energy.type", "Year", "EU_category", "Product.Group", "Unit")
    ) %>%
    dplyr::mutate(
      Share_Group_Input = Energy_Inputs / Sum_Energy_Inputs_By_Group
    ) %>%
    dplyr::mutate(
      Energy.stage = "Useful (fuel+elec+heat)"
    )
  
  
  # Binding all shares for Useful (fuel+elec+heat) energy stage
  all_shares_inc_elec_heat <- dplyr::bind_rows(
    share_inc_elec_heat_1, share_inc_elec_heat_2, share_inc_elec_heat_3
  ) %>%
    dplyr::select(-Energy_Inputs, -Sum_Energy_Inputs_By_Group, -Unit, -share_by_product_origin)
  
  
  # 6) Binding all shares, checking them, and calculating EROIs
  all_shares <- dplyr::bind_rows(
    all_shares_excl_elect_heat,
    all_shares_inc_elec_heat,
    shares_heat,
    shares_elect
  )
  
  # Check shares
  all_shares %>% dplyr::group_by(Country, Method, Energy.type, Year, EU_category, Product.Group, Energy.stage) %>%
    dplyr::summarise(sum_shares = sum(Share_Group_Input)) %>%
    dplyr::filter(abs(sum_shares - 1) > 1e-4) %>%
    nrow() %>%
    testthat::expect_equal(0)
  
  return(all_shares)
}




# Aggregates national useful stage EROIs with a breakdown by end-use category
aggregate_national_useful_erois_by_eu <- function(tidy_national_useful_erois_by_eu_df,
                                                  tidy_national_shares_by_end_use_df,
                                                  from = 1971,
                                                  until = 2020){
  
  for(i in seq(from, until, 1)){
    
    print(i)
    
    tidy_national_shares_by_end_use_current_year <- tidy_national_shares_by_end_use_df %>% 
      dplyr::filter(Year == i)
    
    tidy_national_useful_erois_by_eu_current_year <- tidy_national_useful_erois_by_eu_df %>% 
      dplyr::filter(Year == i)
    
    # Calculating and returning useful stage EROIs by end-use category
    to_add <- tidy_national_shares_by_end_use_current_year %>%
      dplyr::left_join(tidy_national_useful_erois_by_eu_current_year %>%
                         dplyr::select(-Last.stage), by = c("Country", "Method", "Energy.type", "Year", "Product", "EU_category")) %>%
      dplyr::filter(! is.na(Useful_Stage_EROI)) %>% # this should be empty ideally....!
      dplyr::group_by(Country, Method, Energy.type, Year, EU_category, Product.Group, Energy.stage, Eroi.method, Type, Boundary) %>%
      dplyr::summarise(
        Group.eroi.inverse = sum(Share_Group_Input * (1 / Useful_Stage_EROI)) / sum(Share_Group_Input),
        Group.eroi = 1 / Group.eroi.inverse
      ) %>%
      dplyr::select(-Group.eroi.inverse)
    
    if (i == from){
      national_useful_erois_by_eu <- to_add
    } else if (i > from){
      national_useful_erois_by_eu <- dplyr::bind_rows(
        national_useful_erois_by_eu,
        to_add
      )
    }
  }
  return(national_useful_erois_by_eu)
}


# This function calculates the national shares of each product use within each fossil fuel group,
# at each different final energy stage, for each different final demand sector
calc_national_shares_by_fds <- function(PFU_output_df,
                                        ecc_gma_df,
                                        fds_to_fds_category_df,
                                        # Ideally this argument should be removed but convenient for now
                                        # as it allows to capture the relevant combinations of FDS and elec/heat use, and limits subsequent NAs in join
                                        tidy_national_useful_erois_by_fds_df,
                                        years_analysis,
                                        energy_units){
  
  
  for (i in years_analysis){
    
    print(i)
  
  
  ### 0) Working on the set up ###
    
    ecc_gma_i_df <- ecc_gma_df %>% 
      dplyr::filter(Year == i)
    
    tidy_national_useful_erois_by_fds_i_df <- tidy_national_useful_erois_by_fds_df %>% 
      dplyr::filter(Year == i)
  
  # 0.a) Adapting the ECC - GMA data frame
  ecc_gma_adapted <- ecc_gma_i_df %>% 
    dplyr::mutate(
      Country = stringr::str_extract(Flow, "\\{.*\\}") %>% 
        stringr::str_remove("\\{") %>% 
        stringr::str_remove("\\}"),
      Flow = stringr::str_remove(Flow, "\\{.*\\}_"),
      product_without_origin = stringr::str_remove(Product, "\\{.*\\}_"),
    )
  
  # 0.b) Pulling out country list of the regions covered in the GMA data
  list_eroi_countries <- ecc_gma_adapted %>% 
    dplyr::ungroup() %>% 
    dplyr::select(Country) %>% 
    dplyr::distinct() %>% 
    dplyr::pull()
  
  # 0.c) Calculate the share of origin for each product, within each country
  shares_by_product_origin_df <- ecc_gma_adapted %>% 
    dplyr::filter(matnames %in% c("Y", "U_EIOU")) %>% 
    dplyr::mutate(
      product_origin = stringr::str_extract(Product, "\\{.*\\}") %>%
        stringr::str_remove("\\{") %>% 
        stringr::str_remove("\\}")
    ) %>% 
    # Adding up all consumption flows in one unique flow
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, Product, product_origin, product_without_origin, Unit) %>% 
    dplyr::summarise(
      FEN_by_product_origin = sum(abs(E.dot))
    ) %>% 
    # Now calculating share by supplying region
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, product_without_origin, Unit) %>% 
    dplyr::mutate(
      share_by_product_origin = FEN_by_product_origin / sum(FEN_by_product_origin)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-FEN_by_product_origin, -Unit)
  
  # 0.d) Checking sum of shares
  shares_by_product_origin_df %>% 
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Year, product_without_origin) %>% 
    dplyr::summarise(
      sum_shares = sum(share_by_product_origin)
    ) %>% 
    dplyr::filter(abs(sum_shares - 1) > 1e-4) %>% 
    nrow() %>% 
    testthat::expect_equal(0)
  
  # 0.e) Adding product_without_origin column
  tidy_national_useful_erois_by_fds_i_df <- tidy_national_useful_erois_by_fds_i_df %>% 
    dplyr::mutate(
      product_without_origin = stringr::str_remove(Product, "\\{.*\\}_")
    )
  
  
  # 1) Calculates fossil fuel inputs by product and final demand sector category
  
  # All products by final demand sector category
  demand_by_product_fds_df <- PFU_output_df %>% 
    dplyr::filter(Year == i) %>% 
    dplyr::filter(Last.stage == "Final") %>% 
    tidyr::pivot_longer(cols = -c("Country", "Method", "Energy.type", "Last.stage", "Year"), names_to = "matnames", values_to = "matvals") %>% 
    dplyr::filter(matnames == "Y") %>% 
    matsindf::expand_to_tidy() %>% 
    dplyr::filter(matvals != 0) %>% 
    dplyr::select(-coltypes, -rowtypes, -matnames) %>% 
    dplyr::rename(
      Product = rownames,
      Final_Demand_Sector = colnames,
      Energy_Demand = matvals
    ) %>% 
    # Taking out non-energy use flows
    dplyr::filter(! stringr::str_detect(Final_Demand_Sector, "Non-energy use")) %>% 
    # Taking out Exports
    dplyr::filter(! stringr::str_detect(Final_Demand_Sector, "Exports")) %>% 
    # Taking out Stock changes
    dplyr::filter(! stringr::str_detect(Final_Demand_Sector, "Stock changes")) %>% 
    # Taking out statistical differences
    dplyr::filter(! stringr::str_detect(Final_Demand_Sector, "Statistical differences")) %>% 
    # Taking out Losses
    dplyr::filter(Final_Demand_Sector != "Losses") %>% 
    # Taking out non-energy products
    dplyr::filter(! Product %in% IEATools::nonenergy_products) %>% 
    # Summing across final demand sector categories
    dplyr::left_join(fds_to_fds_category_df, by = "Final_Demand_Sector") %>%
    # Exclude NAs in Final Demand Sector Category, which correspond to Final Demand Sectors that are routed nowhere in the input csv file
    dplyr::filter(!is.na(Final_Demand_Sector_Category)) %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Product, Final_Demand_Sector_Category) %>% 
    dplyr::summarise(
      Energy_Demand = sum(Energy_Demand)
    ) %>% 
    dplyr::mutate(
      Unit = energy_units
    ) %>% 
    dplyr::relocate(
      Country, .before = Method
    )
  
  # Fossil fuel final demand by product and final demand sector category
  ff_demand_by_product_fds_df <- demand_by_product_fds_df %>% 
    dplyr::filter(Country %in% list_eroi_countries) %>% 
    dplyr::filter(
      Product %in% c(
        IEATools::oil_and_oil_products,
        IEATools::coal_and_coal_products,
        IEATools::primary_gas_products
      )
    ) %>% 
    # Adding origins to each product
    dplyr::rename(product_without_origin = Product) %>%
    # Inner_join will get rid of a few fossil fuel flows (like blast furnace gas in Australia 2013-2019)
    # and gasoline type jet fuel for some years - it really doesn't matter.
    dplyr::inner_join(
      shares_by_product_origin_df,
      by = c("Country", "Method", "Energy.type", "Year", "product_without_origin")
    ) %>% 
    dplyr::mutate(
      Energy_Demand = Energy_Demand * share_by_product_origin
    ) %>% 
    dplyr::ungroup() %>%
    dplyr::select(-product_origin, -Last.stage)
  
  
  # 2) Determines shares of specified electricity and heat products
  # at the Useful (electrcity) and Useful (heat) energy stages
  
  # First, Useful (electricity) energy stage
  list_elec_fds_categories_year <- tidy_national_useful_erois_by_fds_i_df %>% 
    dplyr::filter(product_without_origin %in% c("Electricity [from Oil products]", "Electricity [from Coal products]", "Electricity [from Natural gas]")) %>% 
    dplyr::ungroup() %>%
    dplyr::select(Country, Year, Final_Demand_Sector_Category) %>% 
    dplyr::distinct()
  
  
  shares_elect <- EROITools::calc_shares_elec_by_ff_group(ecc_gma_adapted,
                                                          share = "Share_Group_Input") %>% 
    dplyr::mutate(
      Energy.stage = "Useful (electricity)"
    ) %>% 
    dplyr::rename(
      Share_Group_Demand = Share_Group_Input
    ) %>% 
    dplyr::select(-Last.stage) %>% 
    dplyr::inner_join(list_elec_fds_categories_year, by = c("Country", "Year")) %>% 
    dplyr::mutate(
      product_without_origin = stringr::str_remove(Product, "\\{.*\\}_")
    )
  
  
  # Second, Useful (heat) energy stage
  list_heat_fds_categories_year <- tidy_national_useful_erois_by_fds_i_df %>% 
    dplyr::filter(product_without_origin %in% c("Heat [from Oil products]", "Heat [from Coal products]", "Heat [from Natural gas]")) %>% 
    dplyr::ungroup() %>%
    dplyr::select(Country, Year, Final_Demand_Sector_Category) %>% 
    dplyr::distinct()
  
  
  shares_heat <- EROITools::calc_shares_heat_by_ff_group(ecc_gma_adapted,
                                                         share = "Share_Group_Input") %>%
    dplyr::mutate(
      Energy.stage = "Useful (heat)"
    ) %>% 
    dplyr::rename(
      Share_Group_Demand = Share_Group_Input
    ) %>% 
    dplyr::select(-Last.stage) %>% 
    dplyr::inner_join(list_heat_fds_categories_year, by = c("Country", "Year")) %>% 
    dplyr::mutate(
      product_without_origin = stringr::str_remove(Product, "\\{.*\\}_")
    )
  
  
  # 3) Determining specified electricity and heat inputs to each end-use category
  
  share_elec_heat_supply_by_ff_group <- dplyr::bind_rows(
    EROITools::calc_share_elec_supply_by_ff_group(ecc_gma_adapted) %>% 
      dplyr::mutate(Product = "Electricity"),
    EROITools::calc_share_heat_supply_by_ff_group(ecc_gma_adapted) %>% 
      dplyr::mutate(Product = "Heat")
  ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Unit)
  
  
  # First, find electricity (from fossil fuels) inputs to each final demand sector category
  elec_demand_fds_df <- demand_by_product_fds_df %>% 
    dplyr::filter(Country %in% list_eroi_countries) %>% 
    dplyr::filter(
      Product == "Electricity"
    ) %>%
    dplyr::left_join(
      share_elec_heat_supply_by_ff_group, by = c("Country", "Method", "Energy.type", "Year", "Product")
    ) %>%
    dplyr::mutate(
      Energy_Demand_FF_Elec = Share * Energy_Demand
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Energy_Demand, -Share, -Product, -Energy.stage, -Last.stage) %>% 
    dplyr::left_join(
      shares_elect %>% 
        dplyr::select(-Energy.stage),
      by = c("Country", "Method", "Energy.type", "Year", "Product.Group", "Final_Demand_Sector_Category")
    ) %>%
    dplyr::mutate(
      Energy_Demand = Energy_Demand_FF_Elec * Share_Group_Demand
    ) %>% 
    dplyr::select(-Share_Group_Demand, -Energy_Demand_FF_Elec)
  
  
  # Second, find heat (from fossil fuels) inputs to each final demand sector category
  heat_demand_fds_df  <- demand_by_product_fds_df %>% 
    dplyr::filter(Country %in% list_eroi_countries) %>% 
    dplyr::filter(
      Product == "Heat"
    ) %>%
    dplyr::left_join(
      share_elec_heat_supply_by_ff_group, by = c("Country", "Method", "Energy.type", "Year", "Product")
    ) %>%
    dplyr::mutate(
      Energy_Demand_FF_Heat = Share * Energy_Demand
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Energy_Demand, -Share, -Product, -Energy.stage, -Last.stage) %>% 
    dplyr::left_join(
      shares_heat %>% 
        dplyr::select(-Energy.stage),
      by = c("Country", "Method", "Energy.type", "Year", "Product.Group", "Final_Demand_Sector_Category")
    ) %>%
    dplyr::mutate(
      Energy_Demand = Energy_Demand_FF_Heat * Share_Group_Demand
    ) %>% 
    dplyr::select(-Share_Group_Demand, -Energy_Demand_FF_Heat)
  
  
  # Third, binding these together:
  elec_heat_demand_fds_df <- dplyr::bind_rows(
    elec_demand_fds_df,
    heat_demand_fds_df
  )
  
  # Cleaning up space
  rm(elec_demand_fds_df, heat_demand_fds_df)
  
  
  # 4) Calculates shares of each product use, within each EU category, for each product group, 
  # for the "Useful (fuel)" energy stage
  
  # All fossil fuels together, excluding electricity and heat
  total_ff_demand_excl_elect_heat_1 <- ff_demand_by_product_fds_df %>%
    dplyr::mutate(Product.Group = "All fossil fuels") %>%
    dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Unit) %>% 
    dplyr::summarise(
      Sum_Energy_Demand_By_Group = sum(Energy_Demand)
    )
  
  # "Oil and gas" and "Coal products" groups, excluding electricity and heat
  total_ff_demand_excl_elect_heat_2 <- ff_demand_by_product_fds_df %>% 
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        product_without_origin %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        product_without_origin %in% IEATools::coal_and_coal_products ~ "Coal products"
      )
    ) %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Unit) %>% 
    dplyr::summarise(
      Sum_Energy_Demand_By_Group = sum(Energy_Demand)
    )
  
  # "Oil products" and "Natural gas", excluding electricity and heat
  total_ff_demand_excl_elect_heat_3 <- ff_demand_by_product_fds_df %>% 
    dplyr::filter(product_without_origin %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>% 
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        product_without_origin %in% IEATools::oil_and_oil_products ~ "Oil products",
        product_without_origin %in% IEATools::primary_gas_products ~ "Natural gas"
      )
    ) %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Unit) %>% 
    dplyr::summarise(
      Sum_Energy_Demand_By_Group = sum(Energy_Demand)
    )
  
  
  # Shares all fossil fuels together, excluding electricity and heat
  share_excl_elect_heat_1 <- ff_demand_by_product_fds_df %>% 
    dplyr::mutate(
      Product.Group = "All fossil fuels"
    ) %>% 
    dplyr::left_join(
      total_ff_demand_excl_elect_heat_1, by = c("Country", "Method", "Energy.type", "Year", "Final_Demand_Sector_Category", "Product.Group", "Unit")
    ) %>% 
    dplyr::mutate(
      Share_Group_Demand = Energy_Demand / Sum_Energy_Demand_By_Group
    ) %>% 
    dplyr::mutate(
      Energy.stage = "Useful (fuel)"
    )
  
  
  # Shares "Oil and gas" and "Coal products" groups, excluding electricity and heat
  share_excl_elect_heat_2 <- ff_demand_by_product_fds_df %>% 
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        product_without_origin %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        product_without_origin %in% IEATools::coal_and_coal_products ~ "Coal products"
      )
    ) %>% 
    dplyr::left_join(
      total_ff_demand_excl_elect_heat_2, by = c("Country", "Method", "Energy.type", "Year", "Final_Demand_Sector_Category", "Product.Group", "Unit")
    ) %>% 
    dplyr::mutate(
      Share_Group_Demand = Energy_Demand / Sum_Energy_Demand_By_Group
    ) %>% 
    dplyr::mutate(
      Energy.stage = "Useful (fuel)"
    )
  
  
  # Shares "Oil products" and "Natural gas", excluding electricity and heat
  share_excl_elect_heat_3 <- ff_demand_by_product_fds_df %>% 
    dplyr::filter(product_without_origin %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>% 
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        product_without_origin %in% IEATools::oil_and_oil_products ~ "Oil products",
        product_without_origin %in% IEATools::primary_gas_products ~ "Natural gas"
      )
    ) %>% 
    dplyr::left_join(
      total_ff_demand_excl_elect_heat_3, by = c("Country", "Method", "Energy.type", "Year", "Final_Demand_Sector_Category", "Product.Group", "Unit")
    ) %>% 
    dplyr::mutate(
      Share_Group_Demand = Energy_Demand / Sum_Energy_Demand_By_Group
    ) %>% 
    dplyr::mutate(
      Energy.stage = "Useful (fuel)"
    )
  
  # Gathering all shares together for the "Useful (fuel)" energy stage.
  all_shares_excl_elect_heat <- dplyr::bind_rows(
    share_excl_elect_heat_1, share_excl_elect_heat_2, share_excl_elect_heat_3
  ) %>% 
    dplyr::select(-Energy_Demand, -Sum_Energy_Demand_By_Group, -Unit, -share_by_product_origin)
  
  # Cleaning up space
  rm(share_excl_elect_heat_1, share_excl_elect_heat_2, share_excl_elect_heat_3)
  
  # 5) Calculates shares of each product use, within each EU category, for each product group, 
  # for the "Useful (fuel+elec+heat)" energy stage
  
  # All fossil fuels together, including electricity and heat
  total_ff_demand_inc_elec_heat_1 <- ff_demand_by_product_fds_df %>%
    dplyr::bind_rows(elec_heat_demand_fds_df %>% dplyr::filter(Product.Group == "All fossil fuels")) %>% 
    dplyr::mutate(Product.Group = "All fossil fuels") %>% 
    dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Unit) %>% 
    dplyr::summarise(
      Sum_Energy_Demand_By_Group = sum(Energy_Demand)
    )
  
  
  # "Oil and gas products" and "Coal products" product groups, including electricity and heat
  total_ff_demand_inc_elec_heat_2 <- ff_demand_by_product_fds_df %>% 
    dplyr::bind_rows(elec_heat_demand_fds_df %>% dplyr::filter(Product.Group %in% c("Oil and gas products", "Coal products"))) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        product_without_origin %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        product_without_origin %in% IEATools::coal_and_coal_products ~ "Coal products",
        TRUE ~ Product.Group
      )
    ) %>%
    dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Unit) %>% 
    dplyr::summarise(
      Sum_Energy_Demand_By_Group = sum(Energy_Demand)
    )
  
  
  # "Oil products" and "Natural gas" product groups, including electricity and heat
  total_ff_demand_inc_elec_heat_3 <- ff_demand_by_product_fds_df %>% 
    dplyr::filter(product_without_origin %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>% 
    dplyr::bind_rows(elec_heat_demand_fds_df %>% dplyr::filter(Product.Group %in% c("Oil products", "Natural gas"))) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        product_without_origin %in% IEATools::oil_and_oil_products ~ "Oil products",
        product_without_origin %in% IEATools::primary_gas_products ~ "Natural gas",
        TRUE ~ Product.Group
      )
    ) %>%
    dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Unit) %>% 
    dplyr::summarise(
      Sum_Energy_Demand_By_Group = sum(Energy_Demand)
    )
  
  # Shares all fossil fuels together, including electricity and heat
  share_inc_elec_heat_1 <- ff_demand_by_product_fds_df %>% 
    dplyr::bind_rows(elec_heat_demand_fds_df %>% dplyr::filter(Product.Group == "All fossil fuels")) %>% 
    dplyr::mutate(
      Product.Group = "All fossil fuels"
    ) %>% 
    dplyr::left_join(
      total_ff_demand_inc_elec_heat_1, by = c("Country", "Method", "Energy.type", "Year", "Final_Demand_Sector_Category", "Product.Group", "Unit")
    ) %>% 
    dplyr::mutate(
      Share_Group_Demand = Energy_Demand / Sum_Energy_Demand_By_Group
    ) %>% 
    dplyr::mutate(
      Energy.stage = "Useful (fuel+elec+heat)"
    )
  
  # Shares "Oil and gas products" and "Coal products" product groups, including electricity and heat
  share_inc_elec_heat_2 <- ff_demand_by_product_fds_df %>% 
    dplyr::bind_rows(elec_heat_demand_fds_df %>% dplyr::filter(Product.Group %in% c("Oil and gas products", "Coal products"))) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        product_without_origin %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products) ~ "Oil and gas products",
        product_without_origin %in% IEATools::coal_and_coal_products ~ "Coal products",
        TRUE ~ Product.Group
      )
    ) %>% 
    dplyr::left_join(
      total_ff_demand_inc_elec_heat_2, by = c("Country", "Method", "Energy.type", "Year", "Final_Demand_Sector_Category", "Product.Group", "Unit")
    ) %>% 
    dplyr::mutate(
      Share_Group_Demand = Energy_Demand / Sum_Energy_Demand_By_Group
    ) %>% 
    dplyr::mutate(
      Energy.stage = "Useful (fuel+elec+heat)"
    )
  
  # Shares "Oil products" and "Natural gas" product groups, including electricity and heat
  share_inc_elec_heat_3 <- ff_demand_by_product_fds_df %>% 
    dplyr::filter(product_without_origin %in% c(IEATools::oil_and_oil_products, IEATools::primary_gas_products)) %>% 
    dplyr::bind_rows(elec_heat_demand_fds_df %>% dplyr::filter(Product.Group %in% c("Oil products", "Natural gas"))) %>%
    dplyr::mutate(
      Product.Group = dplyr::case_when(
        product_without_origin %in% IEATools::oil_and_oil_products ~ "Oil products",
        product_without_origin %in% IEATools::primary_gas_products ~ "Natural gas",
        TRUE ~ Product.Group
      )
    ) %>% 
    dplyr::left_join(
      total_ff_demand_inc_elec_heat_3, by = c("Country", "Method", "Energy.type", "Year", "Final_Demand_Sector_Category", "Product.Group", "Unit")
    ) %>% 
    dplyr::mutate(
      Share_Group_Demand = Energy_Demand / Sum_Energy_Demand_By_Group
    ) %>% 
    dplyr::mutate(
      Energy.stage = "Useful (fuel+elec+heat)"
    )
  
  # Binding all shares for Useful (fuel+elec+heat) energy stage
  all_shares_inc_elec_heat <- dplyr::bind_rows(
    share_inc_elec_heat_1, share_inc_elec_heat_2, share_inc_elec_heat_3
  ) %>% 
    dplyr::select(-Energy_Demand, -Sum_Energy_Demand_By_Group, -Unit, -share_by_product_origin)
  
  # Cleaning up space
  rm(share_inc_elec_heat_1, share_inc_elec_heat_2, share_inc_elec_heat_3)
  
  # 6) Binding all shares, checking them, and calculating EROIs
  to_add <- dplyr::bind_rows(
    all_shares_excl_elect_heat,
    all_shares_inc_elec_heat,
    shares_heat,
    shares_elect
  )
  
  # Cleaning up space
  rm(all_shares_excl_elect_heat, all_shares_inc_elec_heat, shares_heat)
  
  # Check shares
  to_add %>% dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Energy.stage) %>%
    dplyr::summarise(sum_shares = sum(Share_Group_Demand)) %>%
    dplyr::filter(abs(sum_shares - 1) > 1e-4) %>%
    nrow() %>%
    testthat::expect_equal(0)
  
  
  if (i == min(years_analysis)){
    all_shares <- to_add
  } else {
    all_shares <- all_shares %>% 
      dplyr::bind_rows(
        to_add
      )
  }
  
  }
  
  return(all_shares)
}


bind_national_shares <- function(to_bind_1,
                                 to_bind_2){
  a <- dplyr::bind_rows(to_bind_1, to_bind_2)
  return(a)
}


# Aggregates national useful stage EROIs with a breakdown final demand sector category
aggregate_national_useful_erois_by_fds <- function(tidy_national_useful_erois_by_fds_df,
                                                   tidy_national_shares_by_fds_df,
                                                   from = 1971,
                                                   until = 2020){
  
  
  for(i in seq(from, until, 1)){
    
    print(i)
    
    tidy_national_shares_by_fds_current_year <- tidy_national_shares_by_fds_df %>% 
      dplyr::filter(Year == i)
    
    tidy_national_useful_erois_by_fds_current_year <- tidy_national_useful_erois_by_fds_df %>% 
      dplyr::filter(Year == i)
      
    # Calculating and returning useful stage EROIs by final demand sector
    to_add <- tidy_national_shares_by_fds_current_year %>%
      dplyr::left_join(tidy_national_useful_erois_by_fds_current_year %>%
                         dplyr::select(-Last.stage), by = c("Country", "Method", "Energy.type", "Year", "Product", "Final_Demand_Sector_Category")) %>% 
      #dplyr::filter(is.na(Useful_Stage_EROI)) %>% View()
      dplyr::filter(! is.na(Useful_Stage_EROI)) %>% # this should be empty ideally....!
      dplyr::group_by(Country, Method, Energy.type, Year, Final_Demand_Sector_Category, Product.Group, Energy.stage, Eroi.method, Type, Boundary) %>%
      dplyr::summarise(
        Group.eroi.inverse = sum(Share_Group_Demand * (1 / Useful_Stage_EROI)) / sum(Share_Group_Demand),
        Group.eroi = 1 / Group.eroi.inverse
      ) %>%
      dplyr::select(-Group.eroi.inverse)
    
    if (i == from){
      national_useful_erois_by_fds <- to_add
    } else if (i > from){
      national_useful_erois_by_fds <- dplyr::bind_rows(
        national_useful_erois_by_fds,
        to_add
      )
    }
  }
  return(national_useful_erois_by_fds)
}

