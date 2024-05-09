
# This function generates exiobase final energy extension vectors in a tidy data frame format,
# so, not in a vector format. When needing to use a vector, one will have to filter by year, and then to use
# the pivot_wider function to put the data in a vector format
# NOTE: this is NOT the vector of stressors (impact/output) but the vector of total final energy consumption.
# We can't use directly exiobase stressor vector because it is differentiated by TFC, LOSS, and NENE, 
# while we want to subtract LOSS and NENE from the total
generate_energy_extension_vectors <- function(path_to_exiobase){
  
  # Creating empty tidy data frame
  tidy_F_df <- tibble::tibble(
    Country = character(),
    Industry = numeric(),
    Sector = character(),
    FEC = numeric(),
    Year = numeric()
  )
  
  for (i in seq(1995, 2015)){
    
    print(i)
    
    # Unzipping file for year i
    zipF <- paste0(path_to_exiobase, "IOT_", i, "_ixi.zip")
    outDir <- paste0(path_to_exiobase, "IOT_", i, "_ixi/")
    unzip(zipF, exdir = path_to_exiobase)
    
    # Reading file
    F_df_raw <- read.delim(paste0(path_to_exiobase, "IOT_", i, "_ixi/satellite/F.txt"))
    
    # Transposing
    F_df <- data.table::transpose(F_df_raw[c(1107, 1108, 1115),], keep.names = "Country_Industry") %>% 
      tibble::as_tibble()
    
    # Changing column names and removing row with column names
    colnames(F_df) <- c("Country_Industry", F_df[1, 2:4])
    F_df <- F_df[-1, ]
    
    # Building the data frame to be added to tidy_extension_vector
    to_add <- F_df %>% 
      # Column names
      dplyr::rename(
        Total = `Energy Carrier Net Total`,
        Non_energy = `Energy Carrier Net NENE`,
        Eiou_losses = `Energy Carrier Net LOSS`
      ) %>% 
      # Separating Country and industry and adding 1 to industry number
      tidyr::separate(col = Country_Industry, into = c("Country", "Industry"), sep = "([.])", fill = "right") %>% 
      dplyr::mutate(
        Industry = as.numeric(Industry)
      ) %>% 
      tidyr::replace_na(list(Industry = 0)) %>% 
      dplyr::mutate(
        Industry = Industry + 1,
        Sector = stringr::str_c(Country, Industry, sep = "_"),
        Total = as.numeric(Total),
        Non_energy = as.numeric(Non_energy),
        Eiou_losses = as.numeric(Eiou_losses),
        # Calculating final energy excl. EIOU, losses and non-energy uses
        FEC = Total - Eiou_losses - Non_energy,
        Year = i
      ) %>% 
      # Removing unneeded columns
      dplyr::select(-Total, -Non_energy, -Eiou_losses)
    
    tidy_F_df <- dplyr::bind_rows(
      tidy_F_df,
      to_add
    )
    
    # Removing unzipped file
    unlink(outDir, recursive = TRUE)
  }
  return(tidy_F_df)
}


# This function generates exiobase final energy extension vectors in a tidy data frame format,
# so, not in a vector format. When needing to use a vector, one will have to filter by year, and then to use
# the pivot_wider function to put the data in a vector format
# NOTE: this is NOT the vector of stressors (impact/output) but the vector of total final energy consumption.
# We can't use directly exiobase stressor vector because it is differentiated by TFC, LOSS, and NENE, 
# while we want to subtract LOSS and NENE from the total
generate_fds_energy_extension_vectors <- function(path_to_exiobase){
  
  # Creating empty tidy data frame
  tidy_F_df <- tibble::tibble(
    Country = character(),
    Sector = numeric(),
    FEC = numeric(),
    Year = numeric()
  )
  
  for (i in seq(1995, 2015)){
    
    print(i)
    
    # Unzipping file for year i
    zipF <- paste0(path_to_exiobase, "IOT_", i, "_ixi.zip")
    outDir <- paste0(path_to_exiobase, "IOT_", i, "_ixi/")
    unzip(zipF, exdir = path_to_exiobase)
    
    # Reading file
    F_Y_raw_df <- read.delim(paste0(path_to_exiobase, "IOT_", i, "_ixi/satellite/F_Y.txt"))
    
    # Transposing
    F_Y_df <- data.table::transpose(F_Y_raw_df[c(1107, 1108, 1115),], keep.names = "Country_Sector") %>% 
      tibble::as_tibble()
    
    # Changing column names and removing row with column names
    colnames(F_Y_df) <- c("Country_Sector", F_Y_df[1, 2:4])
    F_Y_df <- F_Y_df[-1, ]
    
    # Building the data frame to be added to tidy_extension_vector
    to_add <- F_Y_df %>% 
      # Column names
      dplyr::rename(
        Total = `Energy Carrier Net Total`,
        Non_energy = `Energy Carrier Net NENE`,
        Eiou_losses = `Energy Carrier Net LOSS`
      ) %>% 
      # Separating Country and industry and adding 1 to industry number
      tidyr::separate(col = Country_Sector, into = c("Country", "Sector"), sep = "([.])", fill = "right") %>% 
      dplyr::mutate(
        Sector = as.numeric(Sector)
      ) %>% 
      tidyr::replace_na(list(Sector = 0)) %>% 
      dplyr::mutate(
        Sector = Sector + 1,
        Total = as.numeric(Total),
        Non_energy = as.numeric(Non_energy),
        Eiou_losses = as.numeric(Eiou_losses),
        # Calculating final energy excl. EIOU, losses and non-energy uses
        FEC = Total - Eiou_losses - Non_energy,
        Year = i
      ) %>% 
      # Removing unneeded columns
      dplyr::select(-Total, -Non_energy, -Eiou_losses)
    
    tidy_F_df <- dplyr::bind_rows(
      tidy_F_df,
      to_add
    )
    
    # Removing unzipped file
    unlink(outDir, recursive = TRUE)
  }
  
  return(tidy_F_df)
}




# This function calculates the indirect final energy consumption for each energy industry in a tidy format
calc_idE_by_industry <- function(path_to_exiobase,
                                 energy_extension_vectors_df){
  
  # Setting up empty data frame to fill in
  f_iE <- tibble(
    Year = integer(),
    Country = character(),
    Industry = integer(),
    Method_idE = character(),
    Energy = numeric()
  )
  
  # List of energy industries
  energy_industries <- c("20", "21", "22", "23", "56", "57", "96", "97", "101", "110")
  
  for (i in seq(1995, 2015, 1)){
    
    print(i)
    
    # Unzipping file for year i
    zipF <- paste0(path_to_exiobase, "IOT_", i, "_ixi.zip")
    outDir <- paste0(path_to_exiobase, "IOT_", i, "_ixi/")
    unzip(zipF, exdir = path_to_exiobase)
    
    # Will be added at the end of each cycle
    f_iE_to_add <- tibble(
      Year = integer(),
      Country = character(),
      Industry = integer(),
      Method_idE = character(),
      Energy = numeric()
    )
    
    # loading the vector of final energy consumption by industry,
    # and turning it into a vector format, ready for calculations
    F_ext <- energy_extension_vectors_df %>% 
      dplyr::filter(Year == i) %>% 
      dplyr::select(-Country,-Industry,-Year) %>% 
      tidyr::pivot_wider(names_from = Sector, values_from = FEC) %>% 
      as.matrix()
    
    industry_names <- colnames(F_ext)
    
    # Loading Z and Y matrices
    Z_raw <- readr::read_tsv(paste0(path_to_exiobase, "IOT_", i, "_ixi/Z.txt"),
                             col_names = FALSE, 
                             col_types = readr::cols(.default = readr::col_double()))
    Z <- as.matrix(Z_raw[4:7990, 3:7989])
    
    Y_raw <- readr::read_tsv(paste0(path_to_exiobase, "IOT_", i, "_ixi/Y.txt"),
                             col_names = FALSE, 
                             col_types = readr::cols(.default = readr::col_double()))
    Y <- as.matrix(Y_raw[4:7990, 3:345])
    
    # Giving cols and rows appropriate names
    colnames(Z) <- industry_names
    rownames(Z) <- industry_names
    rownames(Y) <- industry_names
    
    
    ## CALCULATIONS OF Q AND Q0 - NOT SPECIFIC TO THE ENERGY INDUSTRY ##
    
    # Creating X matrix of total output
    x <- as.matrix(rowSums(Y) + rowSums(Z))
    
    # Testing that there is not any 1 in the X matrix
    testthat::expect_equal(
      length(x[x == 1]), 0
    )
    
    # Creating a matrix inv(diag(X))
    x[x == 0] <- 1
    x_inv <- 1/x
    x[x == 1] <- 0
    x_inv[x_inv == 1] <- 0
    
    # Creating total final demand matrix y
    y <- as.matrix(rowSums(Y))
    rm(Y)
    gc()
    
    # Creating e from this matrix
    e <- F_ext %*% diag(x_inv[,1])
    rm(F_ext)
    
    colnames(e) <- industry_names
    
    # Putting zeros in energy industries in the extension vector
    e[, stringr::str_sub(colnames(e), start = 4) %in% energy_industries] <- 0
    
    # Putting zeros in negative values
    e[e < 0] <- 0
    
    # Checking there's no negative value in e
    testthat::expect_true(
      all(e >= 0)
    )
    
    # Calculating A and default footprint Q
    A <- Z %*% diag(x_inv[,1])
    rownames(A) <- industry_names
    colnames(A) <- industry_names
    
    L <- (diag(7987) - A) %>%
      solve()
    
    Q <- e %*% L %*% y
    rm(L)
    gc()
    
    
    # Calculating A0 excluding all energy industries
    A0 <- A
    A0[, stringr::str_sub(colnames(A0), start = 4) %in% energy_industries] <- 0
    
    # Now, inverting 
    L0 <- (diag(7987) - A0) %>%
      solve()
    # Seems to be working.
    rm(A0)
    gc()
    
    Q0 <- e %*% L0 %*% y
    rm(L0)
    gc()
    
    
    for (j in energy_industries){
      
      print(j)
      
      # Overestimation method
      A_j <- A
      A_j[, (stringr::str_sub(colnames(A_j), start = 4) == j)] <- 0

      L_j <- (diag(7987) - A_j) %>%
        solve()
      
      rm(A_j)
      gc()
      
      Q_j <- e %*% L_j %*% y
      
      rm(L_j)
      
      # idE calc
      indirect_energy <- (Q - Q_j)
      
      # Adding to observations in year i
      f_iE_to_add <- f_iE_to_add %>%
        bind_rows(tibble(
          Year = i,
          Country = "WRLD",
          Industry = as.integer(j),
          Method_idE = "Overestimation",
          Energy = as.numeric(indirect_energy)
        ))
      
      
      # Underestimation method
      A_j_prime <- A
      A_j_prime[, (stringr::str_sub(colnames(A_j_prime), start = 4) %in% energy_industries) & 
                  (stringr::str_sub(colnames(A_j_prime), start = 4) != j)] <- 0
      
      L_j_prime <- (diag(7987) - A_j_prime) %>%
        solve()
      
      rm(A_j_prime)
      gc()
      
      Q_j_prime <- e %*% L_j_prime %*% y
      
      rm(L_j_prime)
      gc()
      
      # idE
      indirect_energy_prime <- (Q_j_prime - Q0)
      
      # Adding to observations in year i
      f_iE_to_add <- f_iE_to_add %>%
        bind_rows(tibble(
          Year = i,
          Country = "WRLD",
          Industry = as.integer(j),
          Method_idE = "Underestimation",
          Energy = as.numeric(indirect_energy_prime)
        ))
    }
    
    f_iE <- f_iE %>%
      bind_rows(f_iE_to_add)
    
    # Removing unzipped file
    unlink(outDir, recursive = TRUE)
  }
  
  f_iE <- f_iE %>% 
    dplyr::mutate(Unit = "TJ") %>% 
    dplyr::relocate(Unit, .after = Method_idE)
  
  return(f_iE)
}


# This function adds (final) indirect energy requirements to the global EROIs
# previously calculated
add_idE_to_global_erois <- function(aggregated_global_erois_df,
                                    idE_by_industry_df,
                                    tidy_world_iea_df,
                                    energy_units){
  
  # First, filtering only years for which idE is calculated
  aggregated_global_erois_selected_years <- aggregated_global_erois_df %>% 
    dplyr::filter(Year %in% seq(1995, 2015, 1))
  
  # Second, work on tidy indirect energy data frame
  conversion_TJ_to_ktoe <- 0.0238845897 # TJ to ktoe
  
  # Figuring out conversion to IEA WEEBs units, dpeending on these units
  if (energy_units == "ktoe"){
    conversion_to_IEA_WEEBs_units <- conversion_TJ_to_ktoe
  } else if (energy_units == "TJ"){
    conversion_to_IEA_WEEBs_units <- 1
  }
  
  # Corresponding industries to product groups and stages
  product_group_stage_industries_df <- dplyr::bind_rows(
    # Primary stage
    tidyr::expand_grid(
      Product.Group = "Oil and gas products",
      Energy.stage = "Primary",
      Industry = c(21, 22, 23)
    ),
    tidyr::expand_grid(
      Product.Group = "Coal products",
      Energy.stage = "Primary",
      Industry = c(20)
    ),
    tidyr::expand_grid(
      Product.Group = "All fossil fuels",
      Energy.stage = "Primary",
      Industry = c(20, 21, 22, 23)
    ),
    # Final stage, here we don't precise fuel/elec/heat to do the indirect energy calcs.
    # We assume the same (idE / output) ratio for all the three categories of final energy (fuel, elect, heat).
    tidyr::expand_grid(
      Product.Group = "Oil and gas products",
      Energy.stage = "Final",
      Industry = c(21, 22, 23, 57, 97, 101, 110)
    ),
    tidyr::expand_grid(
      Product.Group = "Oil products",
      Energy.stage = "Final",
      Industry = c(21, 23, 57, 101)
    ),
    tidyr::expand_grid(
      Product.Group = "Natural gas",
      Energy.stage = "Final",
      Industry = c(22, 97, 110)
    ),
    tidyr::expand_grid(
      Product.Group = "Coal products",
      Energy.stage = "Final",
      Industry = c(20, 56, 96)
    ),
    tidyr::expand_grid(
      Product.Group = "All fossil fuels",
      Energy.stage = "Final",
      Industry = c(20, 21, 22, 23, 56, 57, 96, 97, 101, 110)
    )
  )
  
  # Aggregate all indirect energy flows accordingly
  tidy_global_indirect_energy <- product_group_stage_industries_df %>% 
    dplyr::left_join(idE_by_industry_df,
                     by = "Industry") %>%
    dplyr::group_by(Country, Year, Method_idE, Product.Group, Energy.stage) %>% 
    dplyr::summarise(
      Indirect_Energy_TJ = sum(Energy),
      Indirect_Energy_ktoe = Indirect_Energy_TJ * conversion_to_IEA_WEEBs_units
    ) %>% 
    dplyr::mutate(
      Indirect_Energy = "Included"
    ) %>% 
    dplyr::select(-Indirect_Energy_TJ)
  
  # Third, add indirect energy to global EROIs
  aggregated_global_erois_idE_years <- EROITools::add_indirect_energy_to_erois(
    .tidy_summarised_erois_df = aggregated_global_erois_selected_years,
    .tidy_indirect_energy = tidy_global_indirect_energy,
    .tidy_iea_df = tidy_world_iea_df
  )
  
  # Finally add average idE for years outside 1995-2015
  aggregated_global_erois_idE <- add_avg_idE_to_erois(
    aggregated_erois_df = aggregated_global_erois_df,
    aggregated_erois_idE_df = aggregated_global_erois_idE_years
  )
  
  return(aggregated_global_erois_idE)
}


# This function adds (final) indirect energy requirements to EROIs previously calculated.
# For the time period 1995-2015, it uses values calculated previously
# For remaining years, it adds the average idE/output ratio over the time period 1995-2015
# The average ratio is first computed using the aggregated_global_erois_idE_df data frame
add_avg_idE_to_erois <- function(aggregated_erois_df,
                                 aggregated_erois_idE_df){
  
  # Pulling out EROIs with only direct energy included (1995-2015)
  dE_EROIs <- aggregated_erois_idE_df %>% 
    dplyr::filter(Indirect_Energy == "Excluded") %>% 
    dplyr::select(-Indirect_Energy, -Method_idE) %>% 
    dplyr::rename(Group.eroi.dE = Group.eroi)
  
  # Pulling out EROIs with indirect energy included as well (1995-2015)
  idE_EROIs <- aggregated_erois_idE_df %>% 
    dplyr::filter(Indirect_Energy == "Included") %>% 
    dplyr::rename(Group.eroi.idE = Group.eroi)
  
  # Constructing ratio of indirect energy to output (for period 1995-2015)
  # and computing the average ratio
  avg_ratio_idE_output_excl_SUN_YUG <- idE_EROIs %>% 
    dplyr::left_join(
      dE_EROIs,
      by = c("Country", "Method", "Energy.type", "Last.stage", "Year", "Eroi.method", "Type", "Boundary", 
             "Product.Group", "Energy.stage", "Non_Energy_Uses")
    ) %>% 
    dplyr::mutate(
      ratio_idE_to_output = (1/Group.eroi.idE) - (1/Group.eroi.dE)
    ) %>% 
    dplyr::group_by(Country, Method, Energy.type, Last.stage, Product.Group, Energy.stage, Non_Energy_Uses, Indirect_Energy, Method_idE) %>% 
    dplyr::summarise(
      avg_ratio = mean(ratio_idE_to_output)
    )
  
  # Building observations for SUN and YUG based on RUS and SRB
  avg_ratio_idE_output_SUN_YUG <- avg_ratio_idE_output_excl_SUN_YUG |> 
    dplyr::filter(Country %in% c("RUS", "SRB")) |> 
    dplyr::mutate(
      Country = dplyr::case_match(
        Country,
        "RUS" ~ "SUN",
        "SRB" ~ "YUG"
      )
    )
  
  # Binding new observations for SUN and YUG
  avg_ratio_idE_output_df <- dplyr::bind_rows(
    avg_ratio_idE_output_excl_SUN_YUG,
    avg_ratio_idE_output_SUN_YUG
  )
  
  # Using this average ratio to extrapolate EROIs for years excluded from the 1995-2015 time period
  aggregated_erois_idE_remaining_years <- aggregated_erois_df %>% 
    dplyr::filter(! Year %in% seq(1995, 2015)) %>% 
    tidyr::expand_grid(Indirect_Energy = c("Included", "Excluded")) %>% 
    dplyr::left_join(
      avg_ratio_idE_output_df,
      by = c("Country", "Method", "Energy.type", "Last.stage", "Product.Group", "Energy.stage", "Non_Energy_Uses", "Indirect_Energy")
    ) %>%
    dplyr::mutate(
      Group.eroi = dplyr::case_when(
        Indirect_Energy == "Included" ~ 1/(1/Group.eroi + avg_ratio),
        Indirect_Energy == "Excluded" ~ Group.eroi
      )
    ) %>% 
    dplyr::select(-avg_ratio)
  
  # Finally, bind original idE dataframe with the one just constructed for missing years
  aggregated_erois_avg_idE <- dplyr::bind_rows(aggregated_erois_idE_df,
                                               aggregated_erois_idE_remaining_years)
  
  return(aggregated_erois_avg_idE)
}


# This function adds (final) indirect energy requirements to the global EROIs previously calculated with a given breakdown
# For the time period 1995-2015, it uses values calculated previously
# For remaining years, it adds the average idE/output ratio over the time period 1995-2015
# The average ratio is first computed using the aggregated_global_erois_idE_df data frame
add_avg_idE_to_erois_by <- function(aggregated_erois_by_df,
                                    aggregated_erois_by_idE_df,
                                    by = "EU_category"){
  
  # Pulling out EROIs with only direct energy included (1995-2015)
  dE_EROIs <- aggregated_erois_by_idE_df %>% 
    dplyr::filter(Indirect_Energy == "Excluded") %>% 
    dplyr::select(-Indirect_Energy, -Method_idE) %>% 
    dplyr::rename(Group.eroi.dE = Group.eroi)
  
  # Pulling out EROIs with indirect energy included as well (1995-2015)
  idE_EROIs <- aggregated_erois_by_idE_df %>% 
    dplyr::filter(Indirect_Energy == "Included") %>% 
    dplyr::rename(Group.eroi.idE = Group.eroi)
  
  # Constructing ratio of indirect energy to output (for period 1995-2015)
  # and computing the average ratio
  avg_ratio_idE_output_excl_SUN_YUG <- idE_EROIs %>% 
    dplyr::left_join(
      dE_EROIs,
      by = c("Country", "Method", "Energy.type", "Year", "Eroi.method", "Type", "Boundary", "Product.Group", "Energy.stage", {by})
    ) %>% 
    dplyr::mutate(
      ratio_idE_to_output = (1/Group.eroi.idE) - (1/Group.eroi.dE)
    ) %>% 
    dplyr::group_by(Country, Method, Energy.type, Product.Group, Energy.stage, Indirect_Energy, Method_idE, .data[[by]]) %>% 
    dplyr::summarise(
      avg_ratio = mean(ratio_idE_to_output)
    )
  
  # Building observations for SUN and YUG based on RUS and SRB
  avg_ratio_idE_output_SUN_YUG <- avg_ratio_idE_output_excl_SUN_YUG |> 
    dplyr::filter(Country %in% c("RUS", "SRB")) |> 
    dplyr::mutate(
      Country = dplyr::case_match(
        Country,
        "RUS" ~ "SUN",
        "SRB" ~ "YUG"
      )
    )
  
  # Binding new observations for SUN and YUG
  avg_ratio_idE_output_df <- dplyr::bind_rows(
    avg_ratio_idE_output_excl_SUN_YUG,
    avg_ratio_idE_output_SUN_YUG
  )
  
  # Using this average ratio to extrapolate EROIs for years excluded from the 1995-2015 time period
  aggregated_erois_by_idE_remaining_years <- aggregated_erois_by_df %>% 
    dplyr::filter(! Year %in% seq(1995, 2015)) %>% 
    tidyr::expand_grid(Indirect_Energy = c("Included", "Excluded")) %>% 
    dplyr::left_join(
      avg_ratio_idE_output_df,
      by = c("Country", "Method", "Energy.type", "Product.Group", "Energy.stage", "Indirect_Energy", {by})
    ) %>%
    dplyr::mutate(
      Group.eroi = dplyr::case_when(
        Indirect_Energy == "Included" ~ 1/(1/Group.eroi + avg_ratio),
        Indirect_Energy == "Excluded" ~ Group.eroi
      )
    ) %>% 
    dplyr::select(-avg_ratio)
  
  # Finally, bind original idE dataframe with the one just constructed for missing years
  aggregated_erois_by_avg_idE <- dplyr::bind_rows(aggregated_erois_by_idE_df,
                                                  aggregated_erois_by_idE_remaining_years)
  
  return(aggregated_erois_by_avg_idE)
}


# This function adds (final) indirect energy requirements to the global EROIs broken down by
# a given category (so, at the useful stage only), using the global ratio of idE/output,
# but applying the category and fossil fuel group FU efficiency - so that ratio idE/output
# varies by category and fossil fuel group.
add_idE_to_global_erois_by <- function(aggregated_global_erois_by_df,
                                       aggregated_global_erois_no_breakdown_df,
                                       idE_by_industry_df,
                                       tidy_world_iea_df,
                                       by = "EU_category",
                                       energy_units){
  
  # First, filtering only years for which idE is calculated
  aggregated_global_erois_selected_years <- aggregated_global_erois_no_breakdown_df %>% 
    dplyr::filter(Year %in% seq(1995, 2015, 1))
  
  aggregated_global_erois_by_selected_years <- aggregated_global_erois_by_df %>% 
    dplyr::filter(Year %in% seq(1995, 2015, 1))
  
  # Second, work on tidy indirect energy data frame
  conversion_TJ_to_ktoe <- 0.0238845897 # TJ to ktoe
  
  # Figuring out conversion to IEA WEEBs units, dpeending on these units
  if (energy_units == "ktoe"){
    conversion_to_IEA_WEEBs_units <- conversion_TJ_to_ktoe
  } else if (energy_units == "TJ"){
    conversion_to_IEA_WEEBs_units <- 1
  }
  
  # Corresponding industries to product groups and stages
  product_group_stage_industries_df <- dplyr::bind_rows(
    # Primary stage
    tidyr::expand_grid(
      Product.Group = "Oil and gas products",
      Energy.stage = "Primary",
      Industry = c(21, 22, 23)
    ),
    tidyr::expand_grid(
      Product.Group = "Coal products",
      Energy.stage = "Primary",
      Industry = c(20)
    ),
    tidyr::expand_grid(
      Product.Group = "All fossil fuels",
      Energy.stage = "Primary",
      Industry = c(20, 21, 22, 23)
    ),
    # Final stage, here we don't precise fuel/elec/heat to do the indirect energy calcs.
    # We assume the same (idE / output) ratio for all the three categories of final energy (fuel, elect, heat).
    tidyr::expand_grid(
      Product.Group = "Oil and gas products",
      Energy.stage = "Final",
      Industry = c(21, 22, 23, 57, 97, 101, 110)
    ),
    tidyr::expand_grid(
      Product.Group = "Oil products",
      Energy.stage = "Final",
      Industry = c(21, 23, 57, 101)
    ),
    tidyr::expand_grid(
      Product.Group = "Natural gas",
      Energy.stage = "Final",
      Industry = c(22, 97, 110)
    ),
    tidyr::expand_grid(
      Product.Group = "Coal products",
      Energy.stage = "Final",
      Industry = c(20, 56, 96)
    ),
    tidyr::expand_grid(
      Product.Group = "All fossil fuels",
      Energy.stage = "Final",
      Industry = c(20, 21, 22, 23, 56, 57, 96, 97, 101, 110)
    )
  )
  
  # Aggregate all indirect energy flows accordingly
  tidy_global_indirect_energy <- product_group_stage_industries_df %>% 
    dplyr::left_join(idE_by_industry_df,
                     by = "Industry") %>%
    dplyr::group_by(Country, Year, Method_idE, Product.Group, Energy.stage) %>% 
    dplyr::summarise(
      Indirect_Energy_TJ = sum(Energy),
      Indirect_Energy_ktoe = Indirect_Energy_TJ * conversion_to_IEA_WEEBs_units
    ) %>% 
    dplyr::mutate(
      Indirect_Energy = "Included"
    ) %>% 
    dplyr::select(-Indirect_Energy_TJ)
  
  
  # Joining final and useful stage EROIs
  
  list_by_breakdown <- aggregated_global_erois_by_selected_years %>% 
    dplyr::ungroup() %>% 
    dplyr::select("Country", "Year", "Product.Group", "Energy.stage", .data[[by]]) %>% 
    dplyr::mutate(
      Energy.stage = stringr::str_extract(Energy.stage, "\\(.*\\)")
    ) %>% 
    dplyr::distinct()

  final_stage_erois_with_breakdown <- aggregated_global_erois_selected_years %>% 
    dplyr::filter(stringr::str_detect(Energy.stage, "Final")) %>% 
    dplyr::select(-Non_Energy_Uses) %>% 
    dplyr::mutate(
      Energy.stage = stringr::str_extract(Energy.stage, "\\(.*\\)")
    ) %>% 
    dplyr::left_join(list_by_breakdown, by = c("Country", "Year", "Product.Group", "Energy.stage")) %>% 
    dplyr::mutate(
      Energy.stage = stringr::str_c("Final ", Energy.stage)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Last.stage)
  
  aggregated_merged_global_erois_df <- aggregated_global_erois_by_selected_years %>% 
    dplyr::bind_rows(final_stage_erois_with_breakdown)
  
  
  # Third, add indirect energy to global EROIs
  aggregated_global_erois_idE_by_selected_years <- EROITools::add_indirect_energy_useful_erois_by(
    .tidy_aggregated_erois_by_df = aggregated_merged_global_erois_df,
    .tidy_indirect_energy = tidy_global_indirect_energy,
    .tidy_iea_df = tidy_world_iea_df,
    aggregation_category = by
  )
  
  # Finally add average idE for years outside 1995-2015
  aggregated_global_erois_idE <- add_avg_idE_to_erois_by(
    aggregated_erois_by_df = aggregated_global_erois_by_df,
    aggregated_erois_by_idE_df = aggregated_global_erois_idE_by_selected_years,
    by = by
  )
  
  return(aggregated_global_erois_idE)
}


# This function adds (final) indirect energy requirements to the national EROIs
# previously calculated, using the global ratio of idE/output - no country specific idE requirements
add_idE_to_national_erois <- function(aggregated_national_erois_df,
                                      idE_by_industry_df,
                                      tidy_world_iea_df,
                                      energy_units){
  
  
  # First, filtering only years for which idE is calculated
  aggregated_national_erois_selected_years <- aggregated_national_erois_df %>% 
    dplyr::filter(Year %in% seq(1995, 2015, 1))
  
  # Second, work on tidy indirect energy data frame
  conversion_TJ_to_ktoe <- 0.0238845897 # TJ to ktoe
  
  # Figuring out conversion to IEA WEEBs units, dpeending on these units
  if (energy_units == "ktoe"){
    conversion_to_IEA_WEEBs_units <- conversion_TJ_to_ktoe
  } else if (energy_units == "TJ"){
    conversion_to_IEA_WEEBs_units <- 1
  }
  
  # Corresponding industries to product groups and stages
  product_group_stage_industries_df <- dplyr::bind_rows(
    # Primary stage
    tidyr::expand_grid(
      Product.Group = "Oil and gas products",
      Energy.stage = "Primary",
      Industry = c(21, 22, 23)
    ),
    tidyr::expand_grid(
      Product.Group = "Coal products",
      Energy.stage = "Primary",
      Industry = c(20)
    ),
    tidyr::expand_grid(
      Product.Group = "All fossil fuels",
      Energy.stage = "Primary",
      Industry = c(20, 21, 22, 23)
    ),
    # Final stage, here we don't precise fuel/elec/heat to do the indirect energy calcs.
    # We assume the same (idE / output) ratio for all the three categories of final energy (fuel, elect, heat).
    tidyr::expand_grid(
      Product.Group = "Oil and gas products",
      Energy.stage = "Final",
      Industry = c(21, 22, 23, 57, 97, 101, 110)
    ),
    tidyr::expand_grid(
      Product.Group = "Oil products",
      Energy.stage = "Final",
      Industry = c(21, 23, 57, 101)
    ),
    tidyr::expand_grid(
      Product.Group = "Natural gas",
      Energy.stage = "Final",
      Industry = c(22, 97, 110)
    ),
    tidyr::expand_grid(
      Product.Group = "Coal products",
      Energy.stage = "Final",
      Industry = c(20, 56, 96)
    ),
    tidyr::expand_grid(
      Product.Group = "All fossil fuels",
      Energy.stage = "Final",
      Industry = c(20, 21, 22, 23, 56, 57, 96, 97, 101, 110)
    )
  )
  
  # Aggregate all indirect energy flows accordingly
  tidy_global_indirect_energy <- product_group_stage_industries_df %>% 
    dplyr::left_join(idE_by_industry_df,
                     by = "Industry") %>%
    dplyr::group_by(Country, Year, Method_idE, Product.Group, Energy.stage) %>% 
    dplyr::summarise(
      Indirect_Energy_TJ = sum(Energy),
      Indirect_Energy_ktoe = Indirect_Energy_TJ * conversion_to_IEA_WEEBs_units
    ) %>% 
    dplyr::mutate(
      Indirect_Energy = "Included"
    ) %>% 
    dplyr::select(-Indirect_Energy_TJ)
  
  # Third, add indirect energy to global EROIs
  aggregated_national_erois_idE_selected_years <- EROITools::add_indirect_energy_to_erois(
    .tidy_summarised_erois_df = aggregated_national_erois_selected_years,
    .tidy_indirect_energy = tidy_global_indirect_energy,
    .tidy_iea_df = tidy_world_iea_df
  )
  
  # Then adding the average idE for years outside 1995-2015
  # In this function here we should probably add something so that something is added too when the country has no values available 1995:2015.
  # An example is the Soviet Union.
  aggregated_national_erois_idE <- add_avg_idE_to_erois(
    aggregated_erois_df = aggregated_national_erois_df,
    aggregated_erois_idE_df = aggregated_national_erois_idE_selected_years
  )
  
  return(aggregated_national_erois_idE)
}



# This function adds (final) indirect energy requirements to the national EROIs broken down by
# a given category (so, at the useful stage only), using the global ratio of idE/output 
# (no country specific calculations), but applying the category and fossil fuel group FU efficiency,
# so that ratio idE/output varies by category and fossil fuel group.

add_idE_to_national_erois_by <- function(aggregated_national_erois_by_df,
                                         aggregated_national_erois_no_breakdown_df,
                                         idE_by_industry_df,
                                         tidy_world_iea_df,
                                         by = "EU_category",
                                         energy_units){

  # First, filtering only years for which idE is calculated
  aggregated_national_erois_no_breakdown_selected_years <- aggregated_national_erois_no_breakdown_df %>% 
    dplyr::filter(Year %in% seq(1995, 2015, 1))
  
  aggregated_national_erois_by_selected_years <- aggregated_national_erois_by_df %>% 
    dplyr::filter(Year %in% seq(1995, 2015, 1))
  
  # Second, work on tidy indirect energy data frame
  conversion_TJ_to_ktoe <- 0.0238845897 # TJ to ktoe
  
  # Figuring out conversion to IEA WEEBs units, dpeending on these units
  if (energy_units == "ktoe"){
    conversion_to_IEA_WEEBs_units <- conversion_TJ_to_ktoe
  } else if (energy_units == "TJ"){
    conversion_to_IEA_WEEBs_units <- 1
  }
  
  # Corresponding industries to product groups and stages
  product_group_stage_industries_df <- dplyr::bind_rows(
    # Primary stage
    tidyr::expand_grid(
      Product.Group = "Oil and gas products",
      Energy.stage = "Primary",
      Industry = c(21, 22, 23)
    ),
    tidyr::expand_grid(
      Product.Group = "Coal products",
      Energy.stage = "Primary",
      Industry = c(20)
    ),
    tidyr::expand_grid(
      Product.Group = "All fossil fuels",
      Energy.stage = "Primary",
      Industry = c(20, 21, 22, 23)
    ),
    # Final stage, here we don't precise fuel/elec/heat to do the indirect energy calcs.
    # We assume the same (idE / output) ratio for all the three categories of final energy (fuel, elect, heat).
    tidyr::expand_grid(
      Product.Group = "Oil and gas products",
      Energy.stage = "Final",
      Industry = c(21, 22, 23, 57, 97, 101, 110)
    ),
    tidyr::expand_grid(
      Product.Group = "Oil products",
      Energy.stage = "Final",
      Industry = c(21, 23, 57, 101)
    ),
    tidyr::expand_grid(
      Product.Group = "Natural gas",
      Energy.stage = "Final",
      Industry = c(22, 97, 110)
    ),
    tidyr::expand_grid(
      Product.Group = "Coal products",
      Energy.stage = "Final",
      Industry = c(20, 56, 96)
    ),
    tidyr::expand_grid(
      Product.Group = "All fossil fuels",
      Energy.stage = "Final",
      Industry = c(20, 21, 22, 23, 56, 57, 96, 97, 101, 110)
    )
  )
  
  # Aggregate all indirect energy flows accordingly
  tidy_global_indirect_energy <- product_group_stage_industries_df %>% 
    dplyr::left_join(idE_by_industry_df,
                     by = "Industry") %>%
    dplyr::group_by(Country, Year, Method_idE, Product.Group, Energy.stage) %>% 
    dplyr::summarise(
      Indirect_Energy_TJ = sum(Energy),
      Indirect_Energy_ktoe = Indirect_Energy_TJ * conversion_to_IEA_WEEBs_units
    ) %>% 
    dplyr::mutate(
      Indirect_Energy = "Included"
    ) %>% 
    dplyr::select(-Indirect_Energy_TJ)
  
  
  # Joining final and useful stage EROIs
  
  list_by_breakdown <- aggregated_national_erois_by_selected_years %>% 
    dplyr::ungroup() %>% 
    dplyr::select("Country", "Year", "Product.Group", "Energy.stage", .data[[by]]) %>% 
    dplyr::mutate(
      Energy.stage = stringr::str_extract(Energy.stage, "\\(.*\\)")
    ) %>% 
    dplyr::distinct()
  
  final_stage_erois_with_breakdown <- aggregated_national_erois_no_breakdown_selected_years %>% 
    dplyr::filter(stringr::str_detect(Energy.stage, "Final")) %>% 
    dplyr::select(-Non_Energy_Uses) %>% 
    dplyr::mutate(
      Energy.stage = stringr::str_extract(Energy.stage, "\\(.*\\)")
    ) %>% 
    dplyr::left_join(list_by_breakdown, by = c("Country", "Year", "Product.Group", "Energy.stage")) %>% 
    dplyr::mutate(
      Energy.stage = stringr::str_c("Final ", Energy.stage)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-Last.stage)
  
  aggregated_merged_national_erois_df <- aggregated_national_erois_by_selected_years %>% 
    dplyr::bind_rows(final_stage_erois_with_breakdown)
  
  
  # Third, add indirect energy to global EROIs
  aggregated_national_erois_idE_by_selected_years <- EROITools::add_indirect_energy_useful_erois_by(
    .tidy_aggregated_erois_by_df = aggregated_merged_national_erois_df,
    .tidy_indirect_energy = tidy_global_indirect_energy,
    .tidy_iea_df = tidy_world_iea_df,
    aggregation_category = by
  )
  
  # Then add indirect energy also to years outside 1995-2015 using the average
  aggregated_national_erois_idE_by <- add_avg_idE_to_erois_by(
    aggregated_erois_by_df = aggregated_national_erois_by_df,
    aggregated_erois_by_idE_df = aggregated_national_erois_idE_by_selected_years,
    by = by
  )

  return(aggregated_national_erois_idE_by)
}


# This function adds the average of all the methods for the idE calculations as an additional method
add_avg_to_idE_by_industry <- function(idE_by_industry_df){
  
  to_add <- idE_by_industry_df %>% 
    dplyr::group_by(Country, Year, Industry, Unit) %>% 
    dplyr::summarise(
      Energy = mean(Energy)
    ) %>% 
    dplyr::mutate(
      Method_idE = "Average"
    )
  
  to_return <- idE_by_industry_df %>% 
    dplyr::bind_rows(
      to_add
    )
  
  return(to_return)
}


# This function calculates the ratio of indirect energy use to output by product group,
# based on the add_idE_to_global_erois() function
# Ideally the second half of the function should be coded up as a EROITools function... maybe to do one of next days
calc_idE_to_output_ratio <- function(aggregated_global_erois_df,
                                     idE_by_industry_df,
                                     tidy_world_iea_df,
                                     country = IEATools::iea_cols$country,
                                     year = IEATools::iea_cols$year,
                                     method = IEATools::iea_cols$method,
                                     energy_type = IEATools::iea_cols$energy_type,
                                     last_stage = IEATools::iea_cols$last_stage,
                                     e_dot = IEATools::iea_cols$e_dot,
                                     unit = IEATools::iea_cols$unit,
                                     product.group = "Product.Group",
                                     energy.stage = "Energy.stage",
                                     group.eroi = "Group.eroi",
                                     total_group_output = "Total_Group_Output",
                                     indirect_energy_ktoe = "Indirect_Energy_ktoe",
                                     eroi.method = "Eroi.method",
                                     type = "Type",
                                     boundary = "Boundary",
                                     final_to_useful_eff = "Final_to_useful_efficiency",
                                     ratio_indirect_energy_per_output = "ratio_indirect_energy_per_output",
                                     include_non_energy_uses = TRUE,
                                     energy_units){
  
  # First, filtering only years for which idE is calculated
  aggregated_global_erois_df <- aggregated_global_erois_df %>% 
    dplyr::filter(Year %in% seq(1995, 2015, 1))
  
  # Second, work on tidy indirect energy data frame
  conversion_TJ_to_ktoe <- 0.0238845897 # TJ to ktoe
  
  # Figuring out conversion to IEA WEEBs units, dpeending on these units
  if (energy_units == "ktoe"){
    conversion_to_IEA_WEEBs_units <- conversion_TJ_to_ktoe
  } else if (energy_units == "TJ"){
    conversion_to_IEA_WEEBs_units <- 1
  }
  
  # Corresponding industries to product groups and stages
  product_group_stage_industries_df <- dplyr::bind_rows(
    # Primary stage
    tidyr::expand_grid(
      Product.Group = "Oil and gas products",
      Energy.stage = "Primary",
      Industry = c(21, 22, 23)
    ),
    tidyr::expand_grid(
      Product.Group = "Coal products",
      Energy.stage = "Primary",
      Industry = c(20)
    ),
    tidyr::expand_grid(
      Product.Group = "All fossil fuels",
      Energy.stage = "Primary",
      Industry = c(20, 21, 22, 23)
    ),
    # Final stage, here we don't precise fuel/elec/heat to do the indirect energy calcs.
    # We assume the same (idE / output) ratio for all the three categories of final energy (fuel, elect, heat).
    tidyr::expand_grid(
      Product.Group = "Oil and gas products",
      Energy.stage = "Final",
      Industry = c(21, 22, 23, 57, 97, 101, 110)
    ),
    tidyr::expand_grid(
      Product.Group = "Oil products",
      Energy.stage = "Final",
      Industry = c(21, 23, 57, 101)
    ),
    tidyr::expand_grid(
      Product.Group = "Natural gas",
      Energy.stage = "Final",
      Industry = c(22, 97, 110)
    ),
    tidyr::expand_grid(
      Product.Group = "Coal products",
      Energy.stage = "Final",
      Industry = c(20, 56, 96)
    ),
    tidyr::expand_grid(
      Product.Group = "All fossil fuels",
      Energy.stage = "Final",
      Industry = c(20, 21, 22, 23, 56, 57, 96, 97, 101, 110)
    )
  )
  
  # Aggregate all indirect energy flows accordingly
  tidy_global_indirect_energy <- product_group_stage_industries_df %>% 
    dplyr::left_join(idE_by_industry_df,
                     by = "Industry") %>%
    dplyr::group_by(Country, Year, Method_idE, Product.Group, Energy.stage) %>% 
    dplyr::summarise(
      Indirect_Energy_TJ = sum(Energy),
      Indirect_Energy_ktoe = Indirect_Energy_TJ * conversion_to_IEA_WEEBs_units
    ) %>% 
    dplyr::mutate(
      Indirect_Energy = "Included"
    ) %>% 
    dplyr::select(-Indirect_Energy_TJ)
  
  # Third, calculate the ratio
  # Code below based on the add_indirect_energy_to_erois() function of the EROITools package (copied-pasted actually)
  
  # Working out primary energy supply, and final energy consumption by fossil fuel group (including energy / heat coming grom fossil fuels)
  total_output_per_group <- dplyr::bind_rows(
    EROITools::calc_primary_products_supply_by_group(tidy_world_iea_df,
                                          total_group_supply = total_group_output),
    EROITools::calc_fec_from_ff_by_group(tidy_world_iea_df,
                              include_non_energy_uses = include_non_energy_uses) %>%
      dplyr::rename(
        "{total_group_output}" := .data[[e_dot]]
      )
  )
  
  # Now calculating the ratio of indirect energy by output, both at the primary and final stage, for each group
  indirect_energy_per_output_primary_final <- tidy_global_indirect_energy %>%
    dplyr::inner_join(
      total_output_per_group,
      by = c({country}, {year}, {product.group}, {energy.stage})
    ) %>%
    dplyr::mutate(
      "{ratio_indirect_energy_per_output}" := .data[[indirect_energy_ktoe]] / .data[[total_group_output]]
    ) %>%
    dplyr::ungroup() %>% 
    dplyr::select(-.data[[country]], -.data[[indirect_energy_ktoe]], -.data[[method]], -.data[[energy_type]],
                  -.data[[last_stage]], -.data[[unit]], -.data[[total_group_output]])
  
  
  # Calculating the ratio of indirect energy by output, now at the final stage, for each group
  # This is needed because we need the ratio for each of Final (fuel), Final (elec), Final (heat), and Final (fuel+elec+heat)
  indirect_energy_per_output_useful <- aggregated_global_erois_df %>%
    dplyr::filter(! .data[[energy.stage]] == "Primary") %>%
    dplyr::filter(.data[[type]] == "Gross") %>%
    dplyr::filter(.data[[boundary]] == "Feedstock") %>%
    dplyr::mutate(
      "{product.group}" := stringr::str_c(.data[[product.group]], stringr::str_extract(.data[[energy.stage]], " \\(.*\\)")),
      "{energy.stage}" := stringr::str_remove(.data[[energy.stage]], " \\(.*\\)")
    ) %>%
    tidyr::pivot_wider(names_from = .data[[energy.stage]], values_from = .data[[group.eroi]]) %>%
    dplyr::mutate(
      "{final_to_useful_eff}" := .data[["Useful"]] / .data[["Final"]]
    ) %>%
    dplyr::select(-.data[["Final"]], -.data[["Useful"]]) %>%
    dplyr::mutate(
      "{energy.stage}" := stringr::str_c("Useful", stringr::str_extract(.data[[product.group]], " \\(.*\\)")),
      "{product.group}" := stringr::str_remove(.data[[product.group]], " \\(.*\\)")
    ) %>%
    dplyr::inner_join(
      indirect_energy_per_output_primary_final %>% dplyr::filter(stringr::str_detect(.data[[energy.stage]], "Final")) %>% dplyr::select(-.data[[energy.stage]]),
      by = c({year}, {product.group})
    ) %>%
    dplyr::mutate(
      "{ratio_indirect_energy_per_output}" := .data[[ratio_indirect_energy_per_output]] / .data[[final_to_useful_eff]]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data[[method]], -.data[[energy_type]], -.data[[last_stage]], -.data[[eroi.method]], -.data[[type]], -.data[[boundary]], -.data[["Non_Energy_Uses"]], -.data[[final_to_useful_eff]])
  
  
  # Binding final and useful ratios in a single data frame
  idE_to_output_ratio <- dplyr::bind_rows(
    indirect_energy_per_output_primary_final,
    indirect_energy_per_output_useful
  )
  
  return(idE_to_output_ratio)
}

