
# Loads PFU workflow output data
load_PFU_output_data <- function(PFU_output_file,
                                 years){
  readr::read_rds(PFU_output_file) %>% 
    dplyr::filter(Energy.type == "E") %>% 
    dplyr::filter(Year %in% years) %>% 
    dplyr::filter(IEAMW == "IEA") %>% 
    dplyr::select(-IEAMW)
}


# Loads machine to end use category data
load_machine_to_end_use_category_data <- function(machine_to_end_use_category_file){
  readr::read_csv(machine_to_end_use_category_file) %>% 
    dplyr::rename(Industry = PFU_machine)
}



# Loads final demand sector to final demand sector category data
load_fds_to_fds_category_data <- function(fds_to_fds_category_file){
  readr::read_csv(fds_to_fds_category_file) 
}


# Expands PFU output data with more matrices that are needed later in the workflow
expand_PFU_output <- function(PFU_output_df,
                              years) {
  PFU_output_df %>%
    dplyr::filter(Year %in% years) %>% 
    dplyr::mutate(
      g = matsbyname::rowsums_byname(V),
      q = matsbyname::colsums_byname(matsbyname::sum_byname(R, V)) %>%
        matsbyname::transpose_byname(),
      f = matsbyname::colsums_byname(U) %>%
        matsbyname::transpose_byname(),
      D = matsbyname::matrixproduct_byname(V, matsbyname::hatinv_byname(q, keep = "rownames")),
      eta_fu = matsbyname::matrixproduct_byname(
        matsbyname::transpose_byname(g),
        matsbyname::hatinv_byname(matsbyname::colsums_byname(U_feed), keep = "colnames")
      ) %>%
        matsbyname::transpose_byname(),
      D_rev = matsbyname::matrixproduct_byname(
        matsbyname::transpose_byname(U),
        matsbyname::hatinv_byname(q, keep = "rownames")
      ),
      C_rev = matsbyname::matrixproduct_byname(U, matsbyname::hatinv_byname(f, keep = "rownames")),
      eta_D_Y = matsbyname::matrixproduct_byname(matsbyname::hatinv_byname(eta_fu, keep = "rownames"), D) %>%
        matsbyname::matrixproduct_byname(Y),
      C_rev_eta_D_Y = matsbyname::matrixproduct_byname(C_rev, eta_D_Y)
    )
}




# Pulls out tidy FU efficiencies by product, end-use, and country
calc_tidy_efficiencies <- function(PSUT_mat_expanded_df){
  
  # Calculating and pulling out eta_fu
  PSUT_mat_expanded_df %>% 
    dplyr::filter(Last.stage == "Useful") %>% 
    tidyr::pivot_longer(cols = -c("Country", "Method", "Energy.type", "Last.stage", "Year"), names_to = "matnames", values_to = "matvals") %>% 
    dplyr::filter(matnames == "eta_fu") %>% 
    matsindf::expand_to_tidy() %>% 
    dplyr::filter(matvals != 0) %>% 
    dplyr::select(-matnames, -rowtypes, -coltypes, -colnames) %>% 
    dplyr::rename(
      Industry = rownames,
      Efficiency = matvals
    ) %>% 
    dplyr::filter(stringr::str_detect(Industry, "->")) %>% 
    dplyr::filter(Industry != "Non-energy consumption -> NEU")
}

# Pulls out tidy shares of use (D_rev) by product, end-use, and country:
extract_tidy_D_rev <- function(PSUT_mat_expanded_df){
  
  # Calculating and pulling out D_rev
  PSUT_mat_expanded_df %>% 
    dplyr::filter(Last.stage == "Useful") %>% 
    tidyr::pivot_longer(cols = -c("Country", "Method", "Energy.type", "Last.stage", "Year"), names_to = "matnames", values_to = "matvals") %>% 
    dplyr::filter(matnames == "D_rev") %>% 
    matsindf::expand_to_tidy() %>% 
    dplyr::filter(matvals != 0) %>% 
    dplyr::select(-matnames, -rowtypes, -coltypes) %>% 
    dplyr::rename(
      Industry = rownames,
      Product = colnames,
      Share = matvals
    ) %>% 
    dplyr::filter(stringr::str_detect(Industry, "->")) %>% 
    dplyr::filter(Industry != "Non-energy consumption -> NEU")
}


# Pulls out tidy shares of use (D_rev) by product, end-use, and country, and does so BY END-USE CATEGORY
extract_tidy_D_rev_by_eu <- function(tidy_D_rev_df,
                                     machine_to_end_use_category_df){
  
  tidy_D_rev_by_eu <- tidy_D_rev_df %>% 
    dplyr::left_join(machine_to_end_use_category_df, by = "Industry")
  
  return(tidy_D_rev_by_eu)
}


# Calculates average final-to-useful efficiencies for each country
calc_avg_efficiencies_by_country <- function(
  tidy_fu_efficiencies_product_end_use,
  tidy_D_rev,
  machines_to_exclude = c("")
){
  
  # Plugging together D_rev and efficiencies to determine average efficiencies
  avg_FU_efficiencies_by_country <- tidy_D_rev %>% 
    dplyr::filter(! Industry %in% machines_to_exclude) %>% 
    dplyr::left_join(tidy_fu_efficiencies_product_end_use, 
                     by = c("Country", "Method", "Energy.type", "Last.stage", "Year", "Industry")) %>% 
    dplyr::mutate(
      Useful_Energy_By_Final_Energy = Share * Efficiency
    ) %>% 
    dplyr::group_by(
      Country, Method, Energy.type, Last.stage, Year, Product
    ) %>% 
    dplyr::summarise(
      Average_Efficiency_By_Country = sum(Useful_Energy_By_Final_Energy) / sum(Share)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Last.stage)
}


# Add FU efficiencies for each specified electricity and heat product
add_elect_heat_to_efficiencies <- function(FU_efficiencies_df){
  
  # List electricity products
  list_electricity_products <- c("Electricity [from Coal products]", 
                                 "Electricity [from Oil products]", 
                                 "Electricity [from Natural gas]",
                                 "Electricity [from Other products]",
                                 "Electricity [from Renewables]",
                                 "Electricity [from Nuclear]",
                                 "Electricity [from Other processes]",
                                 "Electricity")
  
  # Expanding electricity efficiencies for each elec product
  # This ascribes to all sub-electricity "products" the efficiency of electricity
  elect_fu_efficiencies_df <- FU_efficiencies_df %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(Product == "Electricity") %>% 
    dplyr::select(-Product) %>% 
    tidyr::expand_grid(Product = list_electricity_products)
  
  # List heat products
  list_heat_products <- c("Heat [from Coal products]", 
                          "Heat [from Oil products]", 
                          "Heat [from Natural gas]",
                          "Heat [from Other products]",
                          "Heat [from Renewables]",
                          "Heat [from Nuclear]",
                          "Heat [from Other processes]",
                          "Heat")
  
  # Expanding heat efficiencies for each heat product
  # This ascribes to all sub-heat "products" the efficiency of heat
  heat_fu_efficiencies_df <- FU_efficiencies_df %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(Product == "Heat") %>% 
    dplyr::select(-Product) %>% 
    tidyr::expand_grid(Product = list_heat_products)
  
  # Binding and returning
  final_to_useful_efficiencies_with_elec_heat_df <- FU_efficiencies_df %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(Product != "Electricity" & Product != "Heat") %>% 
    dplyr::bind_rows(
      elect_fu_efficiencies_df,
      heat_fu_efficiencies_df
    )
  
  return(final_to_useful_efficiencies_with_elec_heat_df)
}
