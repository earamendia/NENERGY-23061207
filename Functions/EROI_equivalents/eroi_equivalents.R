
# This function calculates the required value for global EROIs, without breakdown
calc_global_required_erois <- function(substitution_efficiencies_df,
                                       substitution_efficiencies_energy_sector_df,
                                       aggregated_FF_FU_efficiencies_df,
                                       aggregated_global_erois_idE_df){
  
  # Pulling out and adapting the substitution efficiencies
  substitution_efficiencies_global <- substitution_efficiencies_df %>% 
    dplyr::rename(Energy.Carrier = Energy.stage) %>% 
    dplyr::mutate(
      Energy.Carrier = dplyr::case_when(
        Energy.Carrier == "Final (fuel)" ~ "Fuel",
        Energy.Carrier == "Final (fuel+elec+heat)" ~ "All carriers"
      )
    )
  
  # Pulling out and adapting the substitution efficiencies in the energy sector
  substitution_efficiencies_energy_sector <- substitution_efficiencies_energy_sector_df %>% 
    dplyr::filter(Energy.stage == "Final (fuel+elec+heat)", Product.Group == "All fossil fuels") %>% 
    dplyr::ungroup() %>% dplyr::select(-Energy.stage, -Product.Group) %>% 
    dplyr::rename(Efficiency_Substitution_Energy_Sector = Efficiency_FF_Substitution)
    
  # Pulling out fossil fuel FU efficiencies
  FF_avg_efficiencies_global <- aggregated_FF_FU_efficiencies_df %>% 
    dplyr::filter(Energy.stage == "Useful (fuel+elec+heat)") %>%
    dplyr::filter(Product.Group == "All fossil fuels") %>% 
    dplyr::group_by(Country, Method, Energy.type, Energy.stage, Product.Group) %>% 
    dplyr::rename(
      Average_FF_Efficiency = Aggregated_Efficiency
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Energy.stage, -Product.Group)
  
  # Pulling out fossil fuels useful stage EROIs with idE
  ff_useful_stage_erois_global_idE <- aggregated_global_erois_idE_df %>% 
    dplyr::filter(Type == "Gross") %>%
    dplyr::filter(Non_Energy_Uses == "Excluded") %>%
    dplyr::filter(Energy.stage %in% c("Useful (fuel)", "Useful (fuel+elec+heat)")) %>%
    dplyr::select(-Last.stage) %>%
    dplyr::rename(
      Average_FF_Useful_Stage_EROI = Group.eroi
    ) %>% 
    dplyr::rename(Energy.Carrier = Energy.stage) %>%
    dplyr::mutate(
      Energy.Carrier = dplyr::case_when(
        Energy.Carrier == "Useful (fuel)" ~ "Fuel",
        Energy.Carrier == "Useful (fuel+elec+heat)" ~ "All carriers",
        Energy.Carrier == "Useful (heat)" ~ "Heat",
        Energy.Carrier == "Useful (electricity)" ~ "Electricity",
      )
    )
  

  # Calculating the required EROI for useful energy variation to be null
  # First manufacture option, with renewqble electricity
  required_global_erois_1_df <- ff_useful_stage_erois_global_idE %>% 
    dplyr::left_join(substitution_efficiencies_global, by = c("Country", "Method", "Energy.type", "Year", "Product.Group", "Energy.Carrier")) %>% 
    dplyr::left_join(FF_avg_efficiencies_global, by = c("Country", "Method", "Energy.type", "Year")) %>% 
    dplyr::left_join(substitution_efficiencies_energy_sector, by = c("Country", "Method", "Energy.type", "Year")) %>% 
    dplyr::mutate(
      Required_EROI = (Average_FF_Useful_Stage_EROI - Average_FF_Efficiency + Efficiency_Substitution_Energy_Sector) / Efficiency_FF_Substitution,
      Manufacture = "Renewable"
    )
  
  # Second manufacture option, with fossil fuel mix
  required_global_erois_2_df <- ff_useful_stage_erois_global_idE %>% 
    dplyr::left_join(substitution_efficiencies_global, by = c("Country", "Method", "Energy.type", "Year", "Product.Group", "Energy.Carrier")) %>% 
    dplyr::mutate(
      Required_EROI = Average_FF_Useful_Stage_EROI / Efficiency_FF_Substitution,
      Manufacture = "Fossil fuel"
    )

  # Binding both
  required_global_erois_df <- dplyr::bind_rows(required_global_erois_1_df, 
                                               required_global_erois_2_df) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Average_FF_Efficiency, -Average_FF_Useful_Stage_EROI, -Efficiency_FF_Substitution, -Efficiency_Substitution_Energy_Sector) %>% 
    dplyr::relocate(Manufacture, .after = Product.Group)
  
  return(required_global_erois_df)
}



# This function calculates the global value for global EROIs, with a breakdown by user-filled category
calc_global_required_erois_by <- function(substitution_efficiencies_by_df,
                                          substitution_efficiencies_energy_sector_df,
                                          aggregated_FF_FU_efficiencies_df,
                                          aggregated_global_erois_idE_by_df,
                                          by = "EU_category"){

  # Pulling out substitution efficiencies by category
  substitution_efficiencies_global_by <- substitution_efficiencies_by_df %>% 
    dplyr::rename(Energy.Carrier = Energy.stage) %>% 
    dplyr::mutate(
      Energy.Carrier = dplyr::case_when(
        Energy.Carrier == "Useful (fuel)" ~ "Fuel",
        Energy.Carrier == "Useful (fuel+elec+heat)" ~ "All carriers"
      )
    )
  
  # Pulling out and adapting the substitution efficiencies in the energy sector
  substitution_efficiencies_energy_sector <- substitution_efficiencies_energy_sector_df %>% 
    dplyr::filter(Energy.stage == "Final (fuel+elec+heat)", Product.Group == "All fossil fuels") %>% 
    dplyr::ungroup() %>% dplyr::select(-Energy.stage, -Product.Group) %>% 
    dplyr::rename(Efficiency_Substitution_Energy_Sector = Efficiency_FF_Substitution)
  
  # Pulling out fossil fuel FU efficiencies by category
  ff_aggregated_efficiencies_global <- aggregated_FF_FU_efficiencies_df %>% 
    dplyr::filter(Energy.stage == "Useful (fuel+elec+heat)") %>%
    dplyr::filter(Product.Group == "All fossil fuels") %>% 
    dplyr::group_by(Country, Method, Energy.type, Energy.stage, Product.Group) %>% 
    dplyr::rename(
      Average_FF_Efficiency = Aggregated_Efficiency
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Energy.stage, -Product.Group)

  # Pulling out fossil fuel useful stage EROIs by category with idE
  ff_useful_stage_erois_global_idE_by <- aggregated_global_erois_idE_by_df %>% 
    dplyr::filter(Type == "Gross") %>%
    dplyr::filter(Energy.stage %in% c("Useful (fuel)", "Useful (fuel+elec+heat)")) %>%
    dplyr::rename(
      Average_FF_Useful_Stage_EROI_By = Group.eroi
    ) %>% 
    dplyr::rename(Energy.Carrier = Energy.stage) %>%
    dplyr::mutate(
      Energy.Carrier = dplyr::case_when(
        Energy.Carrier == "Useful (fuel)" ~ "Fuel",
        Energy.Carrier == "Useful (fuel+elec+heat)" ~ "All carriers",
        Energy.Carrier == "Useful (heat)" ~ "Heat",
        Energy.Carrier == "Useful (electricity)" ~ "Electricity"
      )
    )

  
  # Calculating the required EROI for useful energy variation to be null
  # First manufacture option, with renewqble electricity
  required_global_erois_by_1_df <- ff_useful_stage_erois_global_idE_by %>% 
    dplyr::left_join(substitution_efficiencies_global_by, by = c("Country", "Method", "Energy.type", "Year", "Product.Group", "Energy.Carrier", {by})) %>% 
    dplyr::left_join(ff_aggregated_efficiencies_global, by = c("Country", "Method", "Energy.type", "Year")) %>% 
    dplyr::left_join(substitution_efficiencies_energy_sector, by = c("Country", "Method", "Energy.type", "Year")) %>% 
    dplyr::mutate(
      Required_EROI = (Average_FF_Useful_Stage_EROI_By - Average_FF_Efficiency + Efficiency_Substitution_Energy_Sector) / (Efficiency_FF_Substitution_by),
      Manufacture = "Renewable"
    )
  
  # Second manufacture option, with fossil fuel mix
  required_global_erois_by_2_df <- ff_useful_stage_erois_global_idE_by %>% 
    dplyr::left_join(substitution_efficiencies_global_by, by = c("Country", "Method", "Energy.type", "Year", "Product.Group", "Energy.Carrier", {by})) %>% 
    dplyr::mutate(
      Required_EROI = Average_FF_Useful_Stage_EROI_By / Efficiency_FF_Substitution_by,
      Manufacture = "Fossil fuel"
    )
  
  # Binding both
  required_global_erois_by_df <- dplyr::bind_rows(required_global_erois_by_1_df, 
                                                  required_global_erois_by_2_df) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Average_FF_Efficiency, -Average_FF_Useful_Stage_EROI_By, -Efficiency_FF_Substitution_by, -Efficiency_Substitution_Energy_Sector) %>% 
    dplyr::relocate(Manufacture, .after = Product.Group)
  
  return(required_global_erois_by_df)
}




# This function calculates the required value for national EROIs, without breakdown
calc_national_required_erois <- function(substitution_efficiencies_df,
                                         substitution_efficiencies_energy_sector_df,
                                         aggregated_national_FF_FU_efficiencies_df,
                                         aggregated_national_erois_idE_df){

  # Pulling out and adapting the substitution efficiencies
  substitution_efficiencies_national <- substitution_efficiencies_df %>% 
    dplyr::rename(Energy.Carrier = Energy.stage) %>% 
    dplyr::mutate(
      Energy.Carrier = dplyr::case_when(
        Energy.Carrier == "Final (fuel)" ~ "Fuel",
        Energy.Carrier == "Final (fuel+elec+heat)" ~ "All carriers"
      )
    )
  
  # Pulling out and adapting the substitution efficiencies in the energy sector
  substitution_efficiencies_energy_sector <- substitution_efficiencies_energy_sector_df %>% 
    dplyr::filter(Energy.stage == "Final (fuel+elec+heat)", Product.Group == "All fossil fuels") %>% 
    dplyr::ungroup() %>% dplyr::select(-Energy.stage, -Product.Group) %>% 
    dplyr::rename(Efficiency_Substitution_Energy_Sector = Efficiency_FF_Substitution)

  # Pulling out fossil fuel FU efficiencies
  FF_avg_efficiencies_national <- aggregated_national_FF_FU_efficiencies_df %>% 
    dplyr::filter(Energy.stage == "Useful (fuel+elec+heat)") %>%
    dplyr::filter(Product.Group == "All fossil fuels") %>% 
    dplyr::group_by(Country, Method, Energy.type, Energy.stage, Product.Group) %>% 
    dplyr::rename(
      Average_FF_Efficiency = Aggregated_Efficiency
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Energy.stage, -Product.Group)
  
  # Pulling out fossil fuels useful stage EROIs with idE
  ff_useful_stage_erois_national_idE <- aggregated_national_erois_idE_df %>% 
    dplyr::filter(Type == "Gross") %>%
    dplyr::filter(Non_Energy_Uses == "Excluded") %>%
    dplyr::filter(Energy.stage %in% c("Useful (fuel)", "Useful (fuel+elec+heat)")) %>%
    dplyr::select(-Last.stage) %>%
    dplyr::rename(
      Average_FF_Useful_Stage_EROI = Group.eroi
    ) %>% 
    dplyr::rename(Energy.Carrier = Energy.stage) %>%
    dplyr::mutate(
      Energy.Carrier = dplyr::case_when(
        Energy.Carrier == "Useful (fuel)" ~ "Fuel",
        Energy.Carrier == "Useful (fuel+elec+heat)" ~ "All carriers",
        Energy.Carrier == "Useful (heat)" ~ "Heat",
        Energy.Carrier == "Useful (electricity)" ~ "Electricity"
      )
    )
  
  # Calculating the required EROI for useful energy variation to be null
  # First manufacture option, with renewqble electricity
  required_national_erois_1_df <- ff_useful_stage_erois_national_idE %>% 
    dplyr::left_join(substitution_efficiencies_national, by = c("Country", "Method", "Energy.type", "Year", "Product.Group", "Energy.Carrier")) %>% 
    dplyr::left_join(FF_avg_efficiencies_national, by = c("Country", "Method", "Energy.type", "Year")) %>% 
    dplyr::left_join(substitution_efficiencies_energy_sector, by = c("Country", "Method", "Energy.type", "Year")) %>% 
    dplyr::mutate(
      Required_EROI = (Average_FF_Useful_Stage_EROI - Average_FF_Efficiency + Efficiency_Substitution_Energy_Sector) / Efficiency_FF_Substitution,
      Manufacture = "Renewable"
    )
  
  # Second manufacture option, with fossil fuel mix
  required_national_erois_2_df <- ff_useful_stage_erois_national_idE %>% 
    dplyr::left_join(substitution_efficiencies_national, by = c("Country", "Method", "Energy.type", "Year", "Product.Group", "Energy.Carrier")) %>% 
    dplyr::mutate(
      Required_EROI = Average_FF_Useful_Stage_EROI / Efficiency_FF_Substitution,
      Manufacture = "Fossil fuel"
    )
  
  # Binding both
  required_national_erois_df <- dplyr::bind_rows(required_national_erois_1_df, 
                                                 required_national_erois_2_df) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Average_FF_Efficiency, -Average_FF_Useful_Stage_EROI, -Efficiency_FF_Substitution, -Efficiency_Substitution_Energy_Sector) %>% 
    dplyr::relocate(Manufacture, .after = Product.Group)
  
  return(required_national_erois_df)
}



# This function calculates the global value for global EROIs, with a breakdown by user-filled category
calc_national_required_erois_by <- function(substitution_efficiencies_by_df,
                                            substitution_efficiencies_energy_sector_df,
                                            aggregated_national_FF_FU_efficiencies_df,
                                            #aggregated_national_erois_by_df,
                                            aggregated_national_erois_idE_by_df,
                                            by = "EU_category"){

  # Pulling out and adapting the substitution efficiencies
  substitution_efficiencies_national <- substitution_efficiencies_by_df %>% 
    dplyr::rename(Energy.Carrier = Energy.stage) %>% 
    dplyr::mutate(
      Energy.Carrier = dplyr::case_when(
        Energy.Carrier == "Useful (fuel)" ~ "Fuel",
        Energy.Carrier == "Useful (fuel+elec+heat)" ~ "All carriers"
      )
    )
  
  # Pulling out and adapting the substitution efficiencies in the energy sector
  substitution_efficiencies_energy_sector <- substitution_efficiencies_energy_sector_df %>% 
    dplyr::filter(Energy.stage == "Final (fuel+elec+heat)", Product.Group == "All fossil fuels") %>% 
    dplyr::ungroup() %>% dplyr::select(-Energy.stage, -Product.Group) %>% 
    dplyr::rename(Efficiency_Substitution_Energy_Sector = Efficiency_FF_Substitution)
  
  # Pulling out fossil fuel FU efficiencies by category
  ff_aggregated_efficiencies_national <- aggregated_national_FF_FU_efficiencies_df %>% 
    dplyr::filter(Energy.stage == "Useful (fuel+elec+heat)") %>%
    dplyr::filter(Product.Group == "All fossil fuels") %>% 
    dplyr::group_by(Country, Method, Energy.type, Energy.stage, Product.Group) %>% 
    dplyr::rename(
      Average_FF_Efficiency = Aggregated_Efficiency
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Energy.stage, -Product.Group)
  
  
  # Pulling out fossil fuel useful stage EROIs by category with idE
  ff_useful_stage_erois_national_idE_by <- aggregated_national_erois_idE_by_df %>% 
    #dplyr::filter(Boundary == "Feedstock") %>%
    dplyr::filter(Type == "Gross") %>%
    dplyr::filter(Energy.stage %in% c("Useful (fuel)", "Useful (fuel+elec+heat)")) %>%
    dplyr::rename(
      Average_FF_Useful_Stage_EROI_By = Group.eroi
    ) %>% 
    dplyr::rename(Energy.Carrier = Energy.stage) %>%
    dplyr::mutate(
      Energy.Carrier = dplyr::case_when(
        Energy.Carrier == "Useful (fuel)" ~ "Fuel",
        Energy.Carrier == "Useful (heat)" ~ "Heat",
        Energy.Carrier == "Useful (electricity)" ~ "Electricity",
        Energy.Carrier == "Useful (fuel+elec+heat)" ~ "All carriers"
      )
    )
  
  
  # Calculating the required EROI for useful energy variation to be null
  # First manufacture option, with renewqble electricity
  required_national_erois_by_1_df <- ff_useful_stage_erois_national_idE_by %>% 
    dplyr::left_join(substitution_efficiencies_national, by = c("Country", "Method", "Energy.type", "Product.Group", "Year", {by}, "Energy.Carrier")) %>% 
    dplyr::left_join(ff_aggregated_efficiencies_national, by = c("Country", "Method", "Energy.type", "Year")) %>% 
    dplyr::left_join(substitution_efficiencies_energy_sector, by = c("Country", "Method", "Energy.type", "Year")) %>% 
    dplyr::mutate(
      Required_EROI = (Average_FF_Useful_Stage_EROI_By - Average_FF_Efficiency + Efficiency_Substitution_Energy_Sector) / (Efficiency_FF_Substitution_by),
      Manufacture = "Renewable"
    ) %>% 
    # If there are NAs in Required EROIs, it means that the calculation could not be conducted as there was
    # no useful stage fossil fuel EROI for this particular (by category, fossil fuel group).
    # So meaningless to return a required EROI, and filtering out makes sense.
    # Seems to be only happening for a few small regions.
    dplyr::filter(!is.na(Required_EROI))
  
  # Second manufacture option, with fossil fuel mix
  required_national_erois_by_2_df <- ff_useful_stage_erois_national_idE_by %>% 
    dplyr::left_join(substitution_efficiencies_national, by = c("Country", "Method", "Energy.type", "Product.Group", "Year", {by}, "Energy.Carrier")) %>% 
    dplyr::mutate(
      Required_EROI = Average_FF_Useful_Stage_EROI_By / Efficiency_FF_Substitution_by,
      Manufacture = "Fossil fuel"
    ) %>% 
    # If there are NAs in Required EROIs, it means that the calculation could not be conducted as there was
    # either no FU efficiency for electricity or no useful stage fossil fuel EROI for this particular by category.
    # So meaningless to return a required EROI, and filtering out makes sense.
    # Seems to be only happening for a few small regions.
    dplyr::filter(!is.na(Required_EROI))
  
  # Binding both
  required_national_erois_by_df <- dplyr::bind_rows(required_national_erois_by_1_df, 
                                                    required_national_erois_by_2_df) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Average_FF_Efficiency, -Average_FF_Useful_Stage_EROI_By, -Efficiency_FF_Substitution_by, -Efficiency_Substitution_Energy_Sector) %>% 
    dplyr::relocate(Manufacture, .after = Product.Group)
  
  return(required_national_erois_by_df)
}

