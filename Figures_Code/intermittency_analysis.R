
# In this file we conduct the sensitivity analysis of required EROIs vs EROI values when including storage
# Code for Figure 6, and Figures SI.3, SI.4 and SI.5

# Libraries
library(targets)
library(ggplot2)
library(dplyr)
library(viridis)


# Static required EROIs, global level, sensitivity with ICP and dC values ---------------------

# Reading targets data
tar_load(required_global_erois)
method_idE_factor <- c("Excluded", "Underestimation", "Average", "Overestimation")

# Value estimated from IEA's WEEB - see "exploring losses" file
share_td_losses <- 0.076

# Storage efficiency from Sgouridis paper comparison renewables with CCS. Similar values found by NREL.
storage_efficiency_batteries <- 0.83
storage_efficiency_PHS <- 0.80
storage_efficiency_P2H <- 0.30 # This value comes from Pellow et al. 2015

# ESOI values
ESOI_batteries <- 11
ESOI_PHS <- 11
ESOI_P2H <- 11 # Assuming same as batteries for hydrogen, conservative assumption
ESOI_Pulido_lowest <- 1.1 # # ESOI BATTERIES FOR PULIDO STUDY for sensitivity analysis

# Reading harmonised EROIs
harmonised_erois <- readr::read_csv("inst/Harmonised_EROIs/murphy_et_all_2022_harmonisation_erois.csv") %>% 
  dplyr::select(PES, `Original EROI`, Ref) %>% 
  dplyr::rename(EROI_excl_TDL = `Original EROI`) %>% 
  dplyr::rename(Technology = PES) %>% 
  dplyr::mutate(
    EROI_incl_TDL = EROI_excl_TDL * (1 - share_td_losses),
    Technology = stringr::str_replace(Technology, "PV", "Solar PV"),
    Technology = stringr::str_replace(Technology, "Wind", "Wind power")
  ) %>% 
  dplyr::filter(!Ref %in% c("de Castro and Capellan-Perez 2020", "Celik et al., 2018")) %>% 
  dplyr::select(-EROI_excl_TDL, -Ref)


# Reading scenario intermittency data
scenario_intermittency_data <- readr::read_csv("inst/Scenario_Intermittency_data/scenario_intermittency.csv") |> 
  dplyr::mutate(
    Variable = stringr::str_replace(Variable, " ", "_"),
    Technology = dplyr::case_match(
      Technology,
      c("CAES", "LAES") ~ "Batteries",
      .default = Technology
    )
  ) |>
  dplyr::group_by(Country, Scenario, Year, Range, Variable, Technology, Unit) |> 
  dplyr::summarise(Value = sum(Value)) |> 
  tidyr::pivot_wider(names_from = c("Variable", "Technology"), values_from = Value, names_sep = "_", values_fill = 0) |> 
  dplyr::rename(Curtailment_fraction = Curtailment_fraction_NA) |> 
  dplyr::mutate(
    Country = dplyr::case_match(
      Country,
      "EU" ~ "European Union",
      "UK" ~ "United Kingdom",
      "US" ~ "United States",
      "FR" ~ "France",
      .default = Country
    ),
    Scenario = dplyr::case_match(
      Country,
      "France" ~ stringr::str_remove(Scenario, "– "),
      .default = Scenario
    )
  ) |> 
  dplyr::select(-Unit) |> 
  print()


list_scenarios_core_analysis <- c("M0 Reference", "M23 Reference",#"M0 Reindustrialisation",
                                  "Mid_Case_95by2050_NoNascent", "High_Demand_Growth_95by2050_NoNascent",
                                  "System Transformation", "Leading the Way",
                                  "Distributed Energy", "Global Ambition")

# Adding the effects of intermittency on the EROIs
erois_with_intermittency <- scenario_intermittency_data |> 
  tidyr::expand_grid(harmonised_erois) |> 
  dplyr::mutate(
    # First and default value: excluding P2H
    EROI_disp_Excl_P2H = (Storage_fraction_Batteries * storage_efficiency_batteries + Storage_fraction_PHS * storage_efficiency_PHS + (1 - Curtailment_fraction - Storage_fraction_Batteries - Storage_fraction_PHS))/
      (1/EROI_incl_TDL + Storage_fraction_Batteries*storage_efficiency_batteries/ESOI_batteries + Storage_fraction_PHS*storage_efficiency_PHS/ESOI_PHS),
    # Second value for sensitivity analysis, including P2H
    EROI_disp_Incl_P2H = (Storage_fraction_Batteries * storage_efficiency_batteries + Storage_fraction_PHS * storage_efficiency_PHS + Storage_fraction_P2H * storage_efficiency_P2H + (1 - Curtailment_fraction - Storage_fraction_Batteries - Storage_fraction_PHS - Storage_fraction_P2H))/
      (1/EROI_incl_TDL + Storage_fraction_Batteries*storage_efficiency_batteries/ESOI_batteries + Storage_fraction_PHS*storage_efficiency_PHS/ESOI_PHS + Storage_fraction_P2H * storage_efficiency_P2H / ESOI_P2H),
    # Third value for sensitivity analysis, using Pulido's low ESOI values
    EROI_disp_Pulido_vals = (Storage_fraction_Batteries * storage_efficiency_batteries + Storage_fraction_PHS * storage_efficiency_PHS + (1 - Curtailment_fraction - Storage_fraction_Batteries - Storage_fraction_PHS))/
      (1/EROI_incl_TDL + Storage_fraction_Batteries*storage_efficiency_batteries/ESOI_Pulido_lowest + Storage_fraction_PHS*storage_efficiency_PHS/ESOI_PHS),
  ) |> 
  dplyr::select(-Curtailment_fraction, - Storage_fraction_Batteries, -Storage_fraction_P2H, -Storage_fraction_PHS)


# Intermittency-adjusted (scenario-level data) EROIs to show on figure
erois_intermittency_for_fig <- erois_with_intermittency |> 
  dplyr::filter(Scenario %in% list_scenarios_core_analysis) |> 
  dplyr::mutate(
    Scenario = dplyr::case_match(
      Scenario,
      "Mid_Case_95by2050_NoNascent" ~ "Mid Case 95%",
      "High_Demand_Growth_95by2050_NoNascent" ~ "High Demand 95%",
      .default = Scenario
    ),
    Scenario = stringr::str_c(Country, Scenario, sep = "\n")
  ) |> 
  dplyr::rename(Product.Group = Scenario,
                `Intermittency\nadjusted EROIs` = Technology)


# Creating factors for subsequent graphs
short_group_name_factor_excl_RET <- c("Average fossil\nfuel mix", "Coal products", "Fossil gas", "Oil and gas", "Oil products")

y_axis_order <- c("Average fossil\nfuel mix", "Coal products", "Fossil gas", "Oil and gas", "Oil products",
                  "European Union\nDistributed Energy", "European Union\nGlobal Ambition", "France\nM0 Reference", "France\nM23 Reference",#"France\nM0 Reindustrialisation"
                  "United Kingdom\nLeading the Way", "United Kingdom\nSystem Transformation", "United States\nMid Case 95%", "United States\nHigh Demand 95%")


# Code for Figure 6

x11()
required_global_erois %>% 
  dplyr::filter(Year == 2020) %>% 
  dplyr::filter(Type == "Gross", Boundary == "All") %>% 
  dplyr::filter(Manufacture == "Renewable") %>% 
  dplyr::filter(Energy.Carrier == "All carriers") %>%
  tidyr::replace_na(list(Method_idE = "Excluded")) %>% 
  dplyr::filter(Method_idE %in% c("Excluded", "Average")) %>% 
  dplyr::select(-Indirect_Energy) %>% 
  tidyr::pivot_wider(
    names_from = Method_idE, values_from = Required_EROI
  ) %>%
  dplyr::mutate(
    Excluded = Excluded - Average
  ) %>%
  tidyr::pivot_longer(
    cols = c("Excluded", "Average"),
    names_to = "Method_idE",
    values_to = "Required_EROI"
  ) %>%
  dplyr::mutate(
    Method_idE = dplyr::case_when(
      Method_idE == "Average" ~ "Included",
      TRUE ~ Method_idE
    )
  ) %>%
  dplyr::mutate(
    Product.Group = dplyr::case_when(
      Product.Group == "All fossil fuels" ~ "Average fossil\nfuel mix",
      Product.Group == "Oil and gas products" ~ "Oil and gas",
      Product.Group == "Natural gas" ~ "Fossil gas",
      TRUE ~ Product.Group
    ),
    Product.Group = forcats::fct(Product.Group, levels = y_axis_order)
  ) %>% 
  tidyr::complete(
    Product.Group,
    fill = list(Required_EROI = NA, Method_idE = "Included")
  ) |> 
  ggplot(aes(y = factor(Product.Group, levels = rev(y_axis_order)), x =  Required_EROI)) +
  geom_blank() +
  geom_jitter(data = erois_intermittency_for_fig, 
              aes(x = EROI_disp_Excl_P2H, y = factor(Product.Group, levels = y_axis_order), col = `Intermittency\nadjusted EROIs`),
              height = 0.14, size = 1, alpha = 0.5) +
  scale_color_viridis_d(begin = 0, end = 0.6, option = "viridis") +
  ggnewscale::new_scale_colour() +
  geom_vline(data = . %>% dplyr::filter(Method_idE == "Included"),
             aes(xintercept = Required_EROI, col =  factor(Product.Group, levels = short_group_name_factor_excl_RET)), linetype = 2, show.legend = FALSE) +
  scale_color_viridis_d(begin = 0, end = 0.65, option = "viridis") +
  geom_bar(aes(fill = factor(Product.Group, levels = y_axis_order), alpha = Method_idE), stat = "identity", position = "stack", width = 0.5) +
  scale_fill_viridis_d(end = 0.7, na.translate = F) +
  scale_alpha_discrete(range = c(0.8, 1)) +
  coord_cartesian(xlim = c(0, 22)) +
  xlab("Final stage Energy Return On Investment") +
  ylab("Group or scenario") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 9),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 9),
    legend.position = "right",
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 8),
    panel.spacing = unit(1.5, "mm"),
    plot.margin = margin(t = 15, b = 8, r = 5.5, l = 5.5, unit = "pt"),
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold")
  ) +
  labs(
    fill = "Reneable energy\nEROI equivalent to",
    alpha = "Indirect Energy",
  ) +
  guides(
    fill = guide_legend(order = 1,
                        keyheight = 1.35),
    alpha = guide_legend(order = 2),
    col = guide_legend(order = 3)
  )


 

# Uncertainty analysis including P2H --------------------------------------
# Code for Figures SI.4 and SI.5

 y_axis_order_P2H <- c("Average fossil\nfuel mix", "Coal products", "Fossil gas", "Oil and gas", "Oil products",
                   "France\nM0 Reference", "France\nM23 Reference", #France\nM0 Reindustrialisation",
                   "United Kingdom\nLeading the Way", "United Kingdom\nSystem Transformation")
 
 x11()
 required_global_erois %>% 
   dplyr::filter(Year == 2020) %>% 
   dplyr::filter(Type == "Gross", Boundary == "All") %>% 
   dplyr::filter(Manufacture == "Renewable") %>% 
   dplyr::filter(Energy.Carrier == "All carriers") %>%
   tidyr::replace_na(list(Method_idE = "Excluded")) %>% 
   dplyr::filter(Method_idE %in% c("Excluded", "Average")) %>% 
   dplyr::select(-Indirect_Energy) %>% 
   tidyr::pivot_wider(
     names_from = Method_idE, values_from = Required_EROI
   ) %>%
   dplyr::mutate(
     Excluded = Excluded - Average
   ) %>%
   tidyr::pivot_longer(
     cols = c("Excluded", "Average"),
     names_to = "Method_idE",
     values_to = "Required_EROI"
   ) %>%
   dplyr::mutate(
     Method_idE = dplyr::case_when(
       Method_idE == "Average" ~ "Included",
       TRUE ~ Method_idE
     )
   ) %>%
   dplyr::mutate(
     Product.Group = dplyr::case_when(
       Product.Group == "All fossil fuels" ~ "Average fossil\nfuel mix",
       Product.Group == "Oil and gas products" ~ "Oil and gas",
       Product.Group == "Natural gas" ~ "Fossil gas",
       TRUE ~ Product.Group
     ),
     Product.Group = forcats::fct(Product.Group, levels = y_axis_order_P2H)
   ) %>% 
   tidyr::complete(
     Product.Group,
     fill = list(Required_EROI = NA, Method_idE = "Included")
   ) |> 
   ggplot(aes(y = factor(Product.Group, levels = rev(y_axis_order_P2H)), x =  Required_EROI)) +
   geom_blank() +
   geom_jitter(data = erois_intermittency_for_fig |> dplyr::filter(Product.Group %in% y_axis_order_P2H), 
               aes(x = EROI_disp_Incl_P2H, y = factor(Product.Group, levels = y_axis_order), col = `Intermittency\nadjusted EROIs`),
               height = 0.13, size = 1, alpha = 0.5) +
   scale_color_viridis_d(begin = 0, end = 0.7, option = "viridis") +
   ggnewscale::new_scale_colour() +
   geom_vline(data = . %>% dplyr::filter(Method_idE == "Included"),
              aes(xintercept = Required_EROI, col =  factor(Product.Group, levels = short_group_name_factor_excl_RET)), linetype = 2, show.legend = FALSE) +
   scale_color_viridis_d(begin = 0, end = 0.65, option = "viridis") +
   geom_bar(aes(fill = factor(Product.Group, levels = y_axis_order_P2H), alpha = Method_idE), stat = "identity", position = "stack", width = 0.5) +
   scale_fill_viridis_d(end = 0.7, na.translate = F) +
   scale_alpha_discrete(range = c(0.8, 1)) +
   coord_cartesian(xlim = c(0, 22)) +
   xlab("Final stage Energy Return On Investment") +
   ylab("Group or scenario") +
   theme_bw() +
   theme(
     strip.text = element_text(size = 9),
     legend.text = element_text(size = 7),
     legend.title = element_text(size = 8),
     axis.text.y = element_text(size = 8),
     axis.text.x = element_text(size = 9),
     legend.position = "right",
     axis.title.y = element_blank(),
     axis.title.x = element_text(size = 8),
     panel.spacing = unit(1.5, "mm"),
     plot.margin = margin(t = 15, b = 8, r = 5.5, l = 5.5, unit = "pt"),
     plot.title = element_text(hjust = 0.5, size = 11, face = "bold")
   ) +
   labs(
     fill = "Renewable energy\nEROI equivalent to",
     alpha = "Indirect Energy",
   ) +
   guides(
     fill = guide_legend(order = 1,
                         keyheight = 1.35),
     alpha = guide_legend(order = 2),
     col = guide_legend(order = 3)
   )
 

 

# Uncertainty analysis - Low ESOI -----------------------------------------

 x11()
 required_global_erois %>% 
   dplyr::filter(Year == 2020) %>% 
   dplyr::filter(Type == "Gross", Boundary == "All") %>% 
   dplyr::filter(Manufacture == "Renewable") %>% 
   dplyr::filter(Energy.Carrier == "All carriers") %>%
   tidyr::replace_na(list(Method_idE = "Excluded")) %>% 
   dplyr::filter(Method_idE %in% c("Excluded", "Average")) %>% 
   dplyr::select(-Indirect_Energy) %>% 
   tidyr::pivot_wider(
     names_from = Method_idE, values_from = Required_EROI
   ) %>%
   dplyr::mutate(
     Excluded = Excluded - Average
   ) %>%
   tidyr::pivot_longer(
     cols = c("Excluded", "Average"),
     names_to = "Method_idE",
     values_to = "Required_EROI"
   ) %>%
   dplyr::mutate(
     Method_idE = dplyr::case_when(
       Method_idE == "Average" ~ "Included",
       TRUE ~ Method_idE
     )
   ) %>%
   dplyr::mutate(
     Product.Group = dplyr::case_when(
       Product.Group == "All fossil fuels" ~ "Average fossil\nfuel mix",
       Product.Group == "Oil and gas products" ~ "Oil and gas",
       Product.Group == "Natural gas" ~ "Fossil gas",
       TRUE ~ Product.Group
     ),
     Product.Group = forcats::fct(Product.Group, levels = y_axis_order)
   ) %>% 
   tidyr::complete(
     Product.Group,
     fill = list(Required_EROI = NA, Method_idE = "Included")
   ) |> 
   ggplot(aes(y = factor(Product.Group, levels = rev(y_axis_order)), x =  Required_EROI)) +
   geom_blank() +
   geom_jitter(data = erois_intermittency_for_fig,
               aes(x = EROI_disp_Pulido_vals, y = factor(Product.Group, levels = y_axis_order), col = `Intermittency\nadjusted EROIs`),
               height = 0.13, size = 1, alpha = 0.5) +
   
   scale_color_viridis_d(begin = 0, end = 0.7, option = "viridis") +
   ggnewscale::new_scale_colour() +
   geom_vline(data = . %>% dplyr::filter(Method_idE == "Included"),
              aes(xintercept = Required_EROI, col =  factor(Product.Group, levels = short_group_name_factor_excl_RET)), linetype = 2, show.legend = FALSE) +
   scale_color_viridis_d(begin = 0, end = 0.65, option = "viridis") +
   geom_bar(aes(fill = factor(Product.Group, levels = y_axis_order), alpha = Method_idE), stat = "identity", position = "stack", width = 0.5) +
   scale_fill_viridis_d(end = 0.7, na.translate = F) +
   scale_alpha_discrete(range = c(0.8, 1)) +
   coord_cartesian(xlim = c(0, 17)) +
   xlab("Final stage Energy Return On Investment") +
   ylab("Group or scenario") +
   theme_bw() +
   theme(
     strip.text = element_text(size = 9),
     legend.text = element_text(size = 7),
     legend.title = element_text(size = 8),
     axis.text.y = element_text(size = 8),
     axis.text.x = element_text(size = 9),
     legend.position = "right",
     axis.title.y = element_blank(),
     axis.title.x = element_text(size = 8),
     panel.spacing = unit(1.5, "mm"),
     plot.margin = margin(t = 15, b = 8, r = 5.5, l = 5.5, unit = "pt"),
     plot.title = element_text(hjust = 0.5, size = 11, face = "bold")
   ) +
   labs(
     fill = "Renewable energy\nEROI equivalent to",
     alpha = "Indirect Energy",
   ) +
   guides(
     fill = guide_legend(order = 1,
                         keyheight = 1.35),
     alpha = guide_legend(order = 2),
     col = guide_legend(order = 3)
   )
 



# Scenario data figure ----------------------------------------------------
# Figure SI.3
 
storage_curtailment_fractions <- scenario_intermittency_data |> 
 dplyr::mutate(
   Storage_fraction = Storage_fraction_Batteries + Storage_fraction_PHS,
   Included_Main_Paper = dplyr::case_match(
     Scenario,
     list_scenarios_core_analysis ~ "Included",
     .default = "Excluded"
   )
 ) |> 
 dplyr::select(-Storage_fraction_Batteries, -Storage_fraction_PHS, -Storage_fraction_P2H)


x11()
storage_curtailment_fractions |> 
 ggplot(aes(x = Storage_fraction, y = Curtailment_fraction)) +
  geom_point(aes(col = Country, alpha = Included_Main_Paper)) +
  scale_color_viridis_d(end = 0.8, na.translate = F, option = "plasma") +
  scale_alpha_discrete(range = c(0.4, 1)) +
  xlab(expression("Storage fraction"~(varphi))) +
  ylab(expression("Curtailment fraction"~(nu))) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    legend.position = "right",
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    plot.margin = margin(t = 15, b = 8, r = 5.5, l = 5.5, unit = "pt"),
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold")
  ) +
  labs(
    col = "Region",
    alpha = "Main paper"
  ) +
  guides(
    alpha = guide_legend(order = 2),
    col = guide_legend(order = 1)
  )

  