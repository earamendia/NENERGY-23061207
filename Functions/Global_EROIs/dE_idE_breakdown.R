
# This function calculates the breakdown energy requirements in terms of 
# direct and indirect energy requirements, without breakdown
calc_energy_breakdown <- function(dE_erois_df,
                                  idE_erois_df){
  
  # Renaming each EROI column
  dE_erois_df <- dE_erois_df %>% 
    dplyr::rename(dE_EROI = Group.eroi)
  
  idE_erois_df <- idE_erois_df %>% 
    dplyr::rename(idE_EROI = Group.eroi)
  
  # Computing the breakdown
  breakdown_df <- idE_erois_df %>% 
    dplyr::filter(Indirect_Energy == "Included") %>% 
    dplyr::left_join(
      dE_erois_df,
      by = c("Country", "Method", "Energy.type", "Last.stage", "Year", "Eroi.method", "Type", "Boundary", "Product.Group", "Energy.stage", "Non_Energy_Uses")
    ) %>% 
    dplyr::mutate(
      idE_to_dE_ratio = (1/idE_EROI - 1/dE_EROI) * dE_EROI
    ) %>% 
    tidyr::expand_grid(
      Scope = c("Direct", "Indirect")
    ) %>% 
    dplyr::mutate(
      Energy_Input = dplyr::case_when(
        Scope == "Direct" ~ 1 / (1 + idE_to_dE_ratio),
        Scope == "Indirect" ~ 1 / (1 + 1/idE_to_dE_ratio)
      )
    ) %>% 
    dplyr::relocate(Scope, .after = Method_idE) %>% 
    dplyr::select(-idE_EROI, -dE_EROI)
  
  return(breakdown_df)
}


# This function calculates the breakdown energy requirements in terms of 
# direct and indirect energy requirements, without breakdown by category
calc_energy_breakdown_by <- function(dE_erois_df,
                                     idE_erois_df,
                                     by = "EU_category"){

  # Renaming each EROI column
  dE_erois_df <- dE_erois_df %>%
    dplyr::rename(dE_EROI = Group.eroi)

  idE_erois_df <- idE_erois_df %>%
    dplyr::rename(idE_EROI = Group.eroi)

  # Computing the breakdown
  breakdown_df <- idE_erois_df %>% 
    dplyr::filter(Indirect_Energy == "Included") %>% 
    dplyr::left_join(
      dE_erois_df,
      by = c("Country", "Method", "Energy.type", "Year", "Eroi.method", "Type", "Boundary", "Product.Group", "Energy.stage", {by})
    ) %>% 
    dplyr::mutate(
      idE_to_dE_ratio = (1/idE_EROI - 1/dE_EROI) * dE_EROI
    ) %>% 
    tidyr::expand_grid(
      Scope = c("Direct", "Indirect")
    ) %>% 
    dplyr::mutate(
      Energy_Input = dplyr::case_when(
        Scope == "Direct" ~ 1 / (1 + idE_to_dE_ratio),
        Scope == "Indirect" ~ 1 / (1 + 1/idE_to_dE_ratio)
      )
    ) %>% 
    dplyr::relocate(Scope, .after = Method_idE) %>% 
    dplyr::select(-idE_EROI, -dE_EROI)

  return(breakdown_df)
}

