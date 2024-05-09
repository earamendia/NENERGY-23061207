
# Code for Extended Data Figure 7

# Loading needed data
tar_load(FF_global_aggregated_efficiencies)
tar_load(efficiency_substitution_global)

# Average lifetime of renewable energy technologies
L <- 25


# Reading and cleaning up EROI values from Murphy et al.

# Reading external harmonised data
# Value used by de Castro and ICP in their Excel
# share_td_losses <- 0.092
# Value estimated from IEA's WEEB - see "exploring losses" file
share_td_losses <- 0.076

harmonised_erois <- readr::read_csv("inst/Harmonised_EROIs/murphy_et_all_2022_harmonisation_erois.csv") %>% 
  dplyr::select(PES, `Original EROI`, Ref) %>% 
  dplyr::rename(EROI_excl_TDL = `Original EROI`) %>% 
  dplyr::rename(Technology = PES) %>% 
  dplyr::mutate(
    EROI_incl_TDL = EROI_excl_TDL * (1 - share_td_losses),
    Technology = stringr::str_replace(Technology, "PV", "Solar PV")
  ) %>% 
  dplyr::mutate(
    Product.Group = "RET"
  ) %>% 
  dplyr::filter(!Ref %in% c("de Castro and Capellan-Perez 2020", "Celik et al., 2018"))

harmonised_erois_for_end_uses <- harmonised_erois %>% 
  dplyr::mutate(
    Product.Group = Technology
  )



# Preparing FF efficiencies in 2019
all_ff_efficiencies <- FF_global_aggregated_efficiencies |> 
  dplyr::filter(Energy.stage == "Useful (fuel+elec+heat)") |> 
  dplyr::filter(Product.Group == "All fossil fuels") |> 
  dplyr::filter(Year == 2020) |> 
  dplyr::ungroup() |> 
  dplyr::select(Country, Year, Aggregated_Efficiency) |> 
  print()


# Preparing efficiency of substitution in 2019
substitution_efficiencies <- efficiency_substitution_global |> 
  filter(Energy.stage == "Final (fuel+elec+heat)") |> 
  filter(Year == 2020) |> 
  dplyr::ungroup() |> 
  dplyr::select(Country, Year, Product.Group, Efficiency_FF_Substitution) |> 
  print()



# Calculating corresponding EPBT values, final stage
epbt_values_final <- harmonised_erois |> 
  dplyr::select(-Product.Group) |> 
  tidyr::expand_grid(all_ff_efficiencies) |> 
  dplyr::mutate(
    EPBT_final = L / EROI_excl_TDL,
    Product.Group = " Final"
  ) |> 
  dplyr::select(Technology, Product.Group, EPBT_final) |>
  print()
  

# Calculating corresponding EPBT values
epbt_values_useful <- harmonised_erois |> 
  dplyr::select(-Product.Group) |> 
  tidyr::expand_grid(all_ff_efficiencies) |> 
  dplyr::left_join(substitution_efficiencies, by = c("Country", "Year"), relationship = "many-to-many") |> 
  dplyr::mutate(
    Final = L / EROI_excl_TDL,
    Useful = Final * Aggregated_Efficiency / Efficiency_FF_Substitution,
    ratio = Useful/Final
  ) |> 
  dplyr::select(Technology, Product.Group, Useful) |> 
  tidyr::pivot_longer(cols = "Useful", values_to = "EPBT_useful", names_to = "Energy.stage") |> 
  dplyr::select(-Energy.stage) |> 
  print()


labels_for_x_axis <- tibble::tibble(
  Product.Group = c(" Final", "All fossil fuels", "Coal products", "Natural gas", "Oil and gas products", "Oil products"),
  Stage_Labels = c("EPT[f]", "EPT[u]", "EPT[u]", "EPT[u]", "EPT[u]", "EPT[u]"),
  Product_Labels = c("", ",avg", ",coal", ",gas", ",OG ", ",oil   ")
)

all_epbts <- dplyr::bind_rows(
  epbt_values_useful,
  epbt_values_final |> dplyr::rename(EPBT_useful = EPBT_final)
)


full_factor_levels <- c(" Final", "All fossil fuels", "Coal products", "Natural gas", "Oil and gas products", "Oil products")
product_group_factors <- c("All fossil fuels", "Coal products", "Natural gas", "Oil and gas products", "Oil products")

 # Extended Data Figure 7:
x11()
all_epbts |> 
  ggplot(aes(x = factor(Product.Group, levels = full_factor_levels), alpha = 0.4, fill = "black", y = EPBT_useful)) +
  geom_boxplot(aes(col = factor(Product.Group, levels = full_factor_levels), fill =  factor(Product.Group, levels = full_factor_levels)), width = 0.6, alpha = 0.4) +
  geom_jitter(aes(col = factor(Product.Group, levels = full_factor_levels)), width = 0.3, alpha = 0.4, size = 1) +
  geom_text(data = labels_for_x_axis, aes(x = Product.Group, y = -0.30, label = Stage_Labels), parse = TRUE) +
  geom_text(data = labels_for_x_axis, aes(x = Product.Group, y = -0.33, label = Product_Labels), nudge_x = 0.31, size = 2.70) +
  ylab("Energy Payback Time (years)") +
  xlab("") +
  coord_cartesian(ylim = c(0, 3.2), clip = "off") +
  scale_color_viridis_d(end = 0.7, na.translate = F) +
  scale_fill_viridis_d(end = 0.8, na.translate = F) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )
