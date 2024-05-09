
# Code for Extended Data Figures 1, 2, and 3
# and Figures SI.1, SI.6, and SI.9

# Useful stage EROIs (dynamic) at the global level, excluding idE, as FUEL only ------------------------
# Code for Extended Data Figure 1

tar_load(aggregated_global_erois_without_breakdown_idE)
tar_load(aggregated_global_erois_by_eu_idE)

list_relevant_end_uses <- c("HTH", "LTH", "MECH", "MTH", "RaP", "RoP")
end_uses_factor <- c("Low Temperature Heating", "Medium Temperature Heating", "High Temperature Heating", "Rail Propulsion", "Road Propulsion", "Mechanical Work")

# All fossil fuels together:
p1 <- aggregated_global_erois_without_breakdown_idE %>% 
  filter(Boundary == "All", Type == "Gross") %>% 
  dplyr::filter(Energy.stage %in% c("Final (fuel)", "Useful (fuel)")) %>%
  dplyr::mutate(
    Energy.stage = dplyr::case_when(
      Energy.stage == "Final (fuel)" ~ "Final",
      Energy.stage == "Useful (fuel)" ~ "Useful",
    )
  ) %>% 
  dplyr::filter(Product.Group == "All fossil fuels") %>% 
  filter(Indirect_Energy == "Included", Method_idE == "Average") %>% 
  mutate(
    Product.Group = stringr::str_replace(Product.Group, "Natural gas", "Fossil gas"),
    Product.Group = stringr::str_replace(Product.Group, "All fossil fuels", "Average mix")
  ) %>% 
  ggplot(aes(x = Year, y = Group.eroi)) +
  geom_line(aes(col = Energy.stage, linetype = Energy.stage)) +
  facet_wrap(vars(Product.Group)) +
  ylab("Energy Return On Investment") +
  scale_colour_viridis_d(end = 0.7, direction = -1) +
  theme_bw() +
  ylim(c(0, 15)) +
  labs(
    linetype = "Energy stage",
    col = "Energy stage"
  ) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    legend.position = "bottom",
    legend.margin = margin(t = -12, b = 2),
  )


# Breakdown by group:
p2 <- aggregated_global_erois_without_breakdown_idE %>% 
  filter(Boundary == "All", Type == "Gross") %>% 
  dplyr::filter(Energy.stage %in% c("Final (fuel)", "Useful (fuel)")) %>%
  dplyr::mutate(
    Energy.stage = dplyr::case_when(
      Energy.stage == "Final (fuel)" ~ "Final",
      Energy.stage == "Useful (fuel)" ~ "Useful",
    )
  ) %>% 
  dplyr::filter(Product.Group %in% c("Coal products", "Natural gas")) %>% 
  filter(Indirect_Energy == "Included", Method_idE == "Average") %>% 
  mutate(
    Product.Group = stringr::str_replace(Product.Group, "Natural gas", "Fossil gas"),
    Product.Group = stringr::str_replace(Product.Group, "All fossil fuels", "Average mix")
  ) %>% 
  ggplot(aes(x = Year, y = Group.eroi)) +
  geom_line(aes(col = Energy.stage, linetype = Energy.stage)) +
  facet_wrap(vars(Product.Group)) +
  scale_colour_viridis_d(end = 0.7, direction = -1) +
  theme_bw() +
  ylim(c(0, NA)) +
  labs(
    linetype = "Energy stage",
    col = "Energy stage"
  ) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none",
    legend.margin = margin(t = -12, b = 2),
    panel.spacing.x = unit(4, "mm"),
    axis.title.y = element_blank(),
    plot.margin = margin(t = 5.5, b = 1, r = 5.5, l = 0, unit = "pt")
  )

p2_prime <- aggregated_global_erois_without_breakdown_idE %>% 
  filter(Boundary == "All", Type == "Gross") %>% 
  dplyr::filter(Energy.stage %in% c("Final (fuel)", "Useful (fuel)")) %>%
  dplyr::mutate(
    Energy.stage = dplyr::case_when(
      Energy.stage == "Final (fuel)" ~ "Final",
      Energy.stage == "Useful (fuel)" ~ "Useful",
    )
  ) %>% 
  dplyr::filter(Product.Group %in% c("Oil and gas products", "Oil products")) %>% 
  filter(Indirect_Energy == "Included", Method_idE == "Average") %>% 
  mutate(
    Product.Group = stringr::str_replace(Product.Group, "Natural gas", "Fossil gas"),
    Product.Group = stringr::str_replace(Product.Group, "All fossil fuels", "Average mix")
  ) %>% 
  ggplot(aes(x = Year, y = Group.eroi)) +
  geom_line(aes(col = Energy.stage, linetype = Energy.stage)) +
  facet_wrap(vars(Product.Group)) +
  scale_colour_viridis_d(end = 0.7, direction = -1) +
  theme_bw() +
  scale_y_continuous(limits = c(0, NA), breaks = c(0, 2, 4, 6, 8, 10)) +
  labs(
    linetype = "Energy stage",
    col = "Energy stage"
  ) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 9),
    legend.position = "none",
    legend.margin = margin(t = -12, b = 2),
    panel.spacing.x = unit(4, "mm"),
    axis.title.y = element_blank(),
    plot.margin = margin(t = 1, b = 5.5, r = 5.5, l = 0, unit = "pt")
  )

scale_adjusting_df <- data.frame(
  x_coordinate = rep(1971, 6),
  y_coordinate = c(25, 25, 25, 6, 6, 6),
  EU_category = c("Low Temperature Heating", "Medium Temperature Heating", "High Temperature Heating", "Road Propulsion", "Rail Propulsion", "Mechanical Work")
)

# Fossil fuels by group
p3 <- aggregated_global_erois_by_eu_idE %>% 
  filter(Boundary == "All", Type == "Gross") %>% 
  dplyr::filter(Energy.stage == "Useful (fuel)") %>% 
  dplyr::filter(EU_category %in% list_relevant_end_uses) %>% 
  dplyr::mutate(
    EU_category = dplyr::case_when(
      EU_category == "LTH" ~ "Low Temperature Heating",
      EU_category == "MTH" ~ "Medium Temperature Heating",
      EU_category == "HTH" ~ "High Temperature Heating",
      EU_category == "RoP" ~ "Road Propulsion",
      EU_category == "RaP" ~ "Rail Propulsion",
      EU_category == "MECH" ~ "Mechanical Work",
    )
  ) %>% 
  filter(Indirect_Energy == "Included", Method_idE == "Average") %>% 
  mutate(
    Product.Group = stringr::str_replace(Product.Group, "Natural gas", "Fossil gas"),
    Product.Group = stringr::str_replace(Product.Group, "All fossil fuels", "Average mix")
  ) %>% 
  ggplot(aes(x = Year, y = Group.eroi)) +
  geom_line(aes(col = Product.Group, linetype = Product.Group)) +
  geom_point(data = scale_adjusting_df, aes(x = x_coordinate, y = y_coordinate), alpha = 0) +
  ylab("Energy Return On Investment") +
  scale_color_viridis_d(option = "viridis", end = 0.7) +
  facet_wrap(vars(factor(EU_category, levels = end_uses_factor)), scales = "free_y") +
  theme_bw() +
  ylim(c(0, NA)) +
  theme(
    strip.text = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    legend.position = "bottom",
    legend.margin = margin(t = -10, b = 0),
    panel.spacing.x = unit(0.5, "mm"),
    plot.margin = margin(t = 10, b = 5.5, r = 5.5, l = 5.5)
  ) +
  labs(
    linetype = "Product Group",
    col = "Product Group"
  ) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020))


p2_mixed <- ggpubr::ggarrange(
  p2,
  p2_prime,
  nrow = 2,
  heights = c(1, 1.15)
)

x11()
ggpubr::ggarrange(
  ggpubr::ggarrange(
    p1, p2_mixed, ncol = 2, common.legend = TRUE, legend = "bottom", labels = c("a", ""), widths = c(1, 1.3)),
  p3, 
  nrow = 2,
  labels = c("", "b"),
  heights = c(1, 1.1)
)



# Comparing effect of manufacturing assumptions ---------------------------
# Code for Extended Data Figure 3

manufacture_group_name_factor <- c("All", "Coal", "Gas", "O&G", "Oil")

tar_load(required_global_erois)

# Comparing manufacture options
x11()
required_global_erois %>% 
  dplyr::filter(Year == 2020) %>% 
  dplyr::filter(Type == "Gross", Boundary == "All") %>% 
  dplyr::filter(Energy.Carrier == "All carriers") %>% 
  dplyr::filter(Indirect_Energy == "Included", Method_idE == "Average") %>% 
  dplyr::mutate(
    Product.Group = dplyr::case_when(
      Product.Group == "All fossil fuels" ~ "All",
      Product.Group == "Coal products" ~ "Coal",
      Product.Group == "Oil products" ~ "Oil",
      Product.Group == "Oil and gas products" ~ "O&G",
      Product.Group == "Natural gas" ~ "Gas"
    )
  ) %>% 
  ggplot(aes(x = factor(Product.Group, levels = manufacture_group_name_factor), y = Required_EROI)) +
  geom_bar(aes(fill = Manufacture), stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_viridis_d(begin = 0.05, end = 0.50) +
  xlab("Product Group") +
  ylab("Energy Return On Investment Equivalent") +
  theme_bw() +
  theme(
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 7),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  ) +
  labs(fill = "Manufacturing Energy")





# Looking at final and useful stage EROIs different countries -------------
# Code for Figure SI.1

scoped_countries_concordance <- data.frame(
  Country = c("BRA", "CHNM", "DEU", "GBR", "IND", "JPN", "MEX", "NGA", "TUR", "USA", "RUS", "DZA"),
  Country_Full_Name = c("Brazil", "China", "Germany", "United Kingdom", "India", "Japan", "Mexico", "Nigeria", "Turkey", "United States", "Russia", "Algeria")
) %>% 
  tibble::as_tibble()


tar_load(aggregated_national_erois_without_breakdown_idE)

scoped_countries <- c("BRA", "CHNM", "DEU", "GBR", "IND", "JPN", "MEX", "NGA", "TUR", "USA", "RUS", "DZA")

data_text <- data.frame(
  label = c("Not applicable"),
  Country_Full_Name = c("Nigeria"),
  Product.Group = c("Coal products")
)

aggregated_national_erois_without_breakdown_idE %>% 
  dplyr::filter(Boundary == "All", Type == "Gross") %>% 
  dplyr::filter(Country %in% scoped_countries) %>% 
  dplyr::filter(Energy.stage == "Useful (fuel+elec+heat)") %>% 
  dplyr::filter(Year >= 2000) %>% 
  dplyr::filter(Method_idE == "Average") %>% 
  dplyr::left_join(scoped_countries_concordance, by = "Country") %>% 
  dplyr::group_by(Country, Method, Energy.type, Last.stage, Eroi.method, Type, Boundary, Product.Group, Energy.stage, Non_Energy_Uses, Indirect_Energy, Method_idE) %>% 
  dplyr::summarise(
    Group.eroi = mean(Group.eroi)
  ) %>% 
  dplyr::left_join(
    scoped_countries_concordance, by = "Country"
  ) %>%
  mutate(
    Group.eroi = case_when(
      Country == "NGA" & Product.Group == "Coal products" ~ 0,
      TRUE ~ Group.eroi
    )
  ) %>% 
  dplyr::mutate(
    Product.Group = stringr::str_replace(Product.Group, "Natural gas", "Fossil gas"),
    Product.Group = stringr::str_replace(Product.Group, "All fossil fuels", "Average mix")
  ) %>% 
  ggplot(aes(x = Product.Group, y = Group.eroi)) +
  geom_bar(aes(fill = Product.Group), stat = "identity") +
  geom_text(
    data    = data_text,
    mapping = aes(x = Product.Group, y = 0, label = label, angle = 90),
    hjust   = 0,
    color = "steelblue"
  ) +
  scale_fill_viridis_d(end = 0.7) +
  xlab("Product Group") +
  ylab("Useful stage Energy Return On Investment") +
  facet_wrap(~Country_Full_Name) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.title = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    panel.spacing.x = unit(2, "mm"),
    legend.position = "bottom",
    legend.margin = margin(t = -8, b = 2)
  )
  



# Static EROI equivalents, adding literature EROIs --------------------------
# COde for Extended Data Figure 2

tar_load(required_global_erois_by_eu)

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
    Technology = stringr::str_replace(Technology, "PV", "Solar PV"),
    Technology = stringr::str_replace(Technology, "Wind", "Wind power")
  ) %>% 
  dplyr::mutate(
    Product.Group = Technology
  ) %>% 
  dplyr::filter(!Ref %in% c("de Castro and Capellan-Perez 2020", "Celik et al., 2018"))

harmonised_erois_adapted <- harmonised_erois %>% 
  dplyr::mutate(
    Product.Group = dplyr::case_when(
      Product.Group == "Oil and gas products" ~ "Oil and gas",
      TRUE ~ Product.Group
    )
  )

harmonised_erois_for_end_uses <- harmonised_erois %>% 
  dplyr::mutate(
    Product.Group = Technology
  )

short_group_name_factor_excl_RET <- c("Average fossil\nfuel mix", "Coal products", "Fossil gas", "Oil and gas", "Oil products")

data_text <- data.frame(
  label = c("Not applicable", "Not applicable", "Not applicable", "Not applicable"),
  EU_category = c("Rail Propulsion", "Road Propulsion", "Mechanical Work", "Mechanical Work"),
  Product.Group = c("Fossil gas", "Coal products", "Coal products", "Fossil gas")
)

dark_blue_text <- data.frame(
  label = c("Up to 49 excl. indirect"),
  EU_category = c("Low Temperature Heating"),
  Product.Group = c("Coal products")
)

# First graph of EROI equivalents without breakdown and comparing with RET:
p1 <- required_global_erois %>% 
  dplyr::filter(Year == 2020) %>% 
  dplyr::filter(Type == "Gross", Boundary == "All") %>% 
  dplyr::filter(Manufacture == "Renewable") %>% 
  dplyr::filter(Energy.Carrier == "Fuel") %>%
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
    )
  ) %>% 
  ggplot(aes(y = Required_EROI, x =  factor(Product.Group, levels = short_group_name_factor_excl_RET))) +
  geom_bar(aes(fill = factor(Product.Group, levels = short_group_name_factor_excl_RET), alpha = Method_idE), stat = "identity", position = "stack", width = 0.5) +
  geom_jitter(data = harmonised_erois, aes(x = Product.Group, y = EROI_incl_TDL, col = Technology), width = 0.25, size = 1) +
  scale_fill_viridis_d(end = 0.7, na.translate = F) +
  scale_alpha_discrete(range = c(0.8, 1)) +
  scale_color_viridis_d(begin = 0, end = 0.7, option = "viridis") +
  coord_cartesian(ylim = c(0, 25)) +
  ylab("Final stage Energy Return On Investment") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right",
    axis.title.y = element_text(size = 9),
    axis.title.x = element_blank(),
    plot.margin = margin(t = 15, b = 8, r = 5.5, l = 5.5, unit = "pt"),
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold")
  ) +
  labs(
    fill = "Renewable energy\nEROI equivalent to",
    alpha = "Indirect Energy",
    col = "Reported EROIs",
  ) +
  guides(
    fill = guide_legend(order = 1,
                        keyheight = 1.35),
    alpha = guide_legend(order = 2),
    col = guide_legend(order = 3)
  )

# Breakdown by end-use
p2 <- required_global_erois_by_eu %>% 
  dplyr::filter(Year == 2020) %>% 
  dplyr::filter(Type == "Gross", Boundary == "All") %>%
  dplyr::filter(Manufacture == "Renewable") %>% 
  dplyr::filter(Energy.Carrier == "Fuel") %>% 
  dplyr::filter(EU_category %in% list_relevant_end_uses) %>% 
  dplyr::mutate(
    EU_category = dplyr::case_when(
      EU_category == "LTH" ~ "Low Temperature Heating",
      EU_category == "MTH" ~ "Medium Temperature Heating",
      EU_category == "HTH" ~ "High Temperature Heating",
      EU_category == "RoP" ~ "Road Propulsion",
      EU_category == "RaP" ~ "Rail Propulsion",
      EU_category == "MECH" ~ "Mechanical Work",
    )
  ) %>% 
  dplyr::mutate(
    Product.Group = dplyr::case_when(
      Product.Group == "All fossil fuels" ~ "Average fossil\nfuel mix",
      Product.Group == "Oil and gas products" ~ "Oil and gas",
      Product.Group == "Natural gas" ~ "Fossil gas",
      TRUE ~ Product.Group
    )
  ) %>% 
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
  ggplot(aes(x = factor(Product.Group, levels = short_group_name_factor_excl_RET), y = Required_EROI)) +
  geom_bar(aes(fill = factor(Product.Group, levels = short_group_name_factor_excl_RET), alpha = Method_idE), stat = "identity", width = 0.9) +
  geom_jitter(data = harmonised_erois_for_end_uses, aes(x = Product.Group, y = EROI_incl_TDL, col = Technology), width = 0.32, size = 1) +
  geom_text(
    data    = data_text,
    mapping = aes(x = Product.Group, y = 0, label = label, angle = 90),
    hjust   = 0,
    color = "steelblue"
  ) +
  scale_fill_viridis_d(end = 0.7) +
  scale_alpha_discrete(range = c(0.8, 1)) +
  scale_color_viridis_d(begin = 0, end = 0.8, option = "viridis") +
  xlab("Product Group") +
  ylab("Final stage Energy Return On Investment") +
  coord_cartesian(ylim = c(0, 35)) +
  facet_wrap(vars(factor(EU_category, end_uses_factor))) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 9),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    panel.spacing = unit(1, "mm"),
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold")
  ) +
  labs(
    fill = "Renewable energy\nEROI equivalent to",
    alpha = "Indirect Energy",
    col = "Reported EROIs",
  ) +
  guides(
    fill = guide_legend(order = 1,
                        keyheight = 1.35),
    alpha = guide_legend(order = 2),
    col = guide_legend(order = 3)
  )


x11()
ggpubr::ggarrange(
  p1, p2,
  nrow = 2,
  labels = c("a", "b"),
  heights = c(1, 1.5),
  common.legend = TRUE,
  legend = "right")



# Graph ratio idE to output
# Code for Figure SI.6

tar_load(idE_to_output_ratio)


x11()
idE_to_output_ratio %>% 
  filter(Method_idE == "Average") %>% 
  dplyr::mutate(
    Energy.stage = dplyr::case_when(
      Energy.stage == "Useful (fuel+elec+heat)" ~ "Useful",
      TRUE ~ Energy.stage
    )
  ) %>% 
  filter(Energy.stage %in% c("Final", "Useful")) %>% 
  dplyr::mutate(
    Energy.stage = stringr::str_c(Energy.stage, " stage"),
    Product.Group = stringr::str_replace(Product.Group, "Natural gas", "Fossil gas"),
    Product.Group = stringr::str_replace(Product.Group, "All fossil fuels", "Average mix")
  ) %>% 
  ggplot(aes(x = Year, y = ratio_indirect_energy_per_output)) +
  geom_line(aes(col = Product.Group, linetype = Product.Group)) +
  scale_color_viridis_d(option = "viridis", end = 0.7) +
  ylim(0, 0.106) +
  ylab("Ratio indirect requirements energy to ouput") +
  facet_wrap(~Energy.stage) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    legend.position = "bottom",
    legend.margin = margin(t = -10, b = 0),
    plot.margin = margin(t = 10, b = 5.5, r = 5.5, l = 5.5)
  ) +
  labs(
    linetype = "Product Group",
    col = "Product Group"
  )




# Showing example of graphs by final demand sector
# Code for FIgure SI.9

tar_load(aggregated_global_erois_by_fds_idE)

tar_load(required_global_erois_by_fds)

p_useful <- aggregated_global_erois_by_fds_idE %>% 
  filter(Type == "Gross", Boundary == "All") %>% 
  filter(Indirect_Energy == "Included", Method_idE == "Average") %>% 
  filter(Energy.stage == "Useful (fuel+elec+heat)") %>% 
  filter(Final_Demand_Sector_Category %in% c("Iron and steel", "Road transportation")) %>% 
  dplyr::mutate(
    Product.Group = stringr::str_replace(Product.Group, "Natural gas", "Fossil gas"),
    Product.Group = stringr::str_replace(Product.Group, "All fossil fuels", "Average mix")
  ) %>% 
  ggplot(aes(x = Year, y = Group.eroi)) +
  geom_line(aes(col = Product.Group, linetype = Product.Group)) +
  scale_color_viridis_d(option = "viridis", end = 0.7) +
  ylab("Useful stage Energy Return On Investment") +
  facet_wrap(~Final_Demand_Sector_Category) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 9),
    legend.position = "bottom",
    legend.margin = margin(t = -10, b = 0),
    panel.spacing.x = unit(3, "mm"),
    plot.margin = margin(t = 10, b = 5.5, r = 5.5, l = 5.5)
  ) +
  labs(
    linetype = "Product Group",
    col = "Product Group"
  )

p_threshold <- required_global_erois_by_fds %>% 
  filter(Type == "Gross", Boundary == "All") %>% 
  filter(Indirect_Energy == "Included", Method_idE == "Average") %>% 
  filter(Energy.Carrier == "All carriers") %>% 
  filter(Manufacture == "Renewable") %>% 
  filter(Final_Demand_Sector_Category %in% c("Iron and steel", "Road transportation")) %>% 
  dplyr::mutate(
    Product.Group = stringr::str_replace(Product.Group, "Natural gas", "Fossil gas"),
    Product.Group = stringr::str_replace(Product.Group, "All fossil fuels", "Average mix")
  ) %>% 
  ggplot(aes(x = Year, y = Required_EROI)) +
  geom_line(aes(col = Product.Group, linetype = Product.Group)) +
  scale_color_viridis_d(option = "viridis", end = 0.7) +
  ylab("Energy Return On Investment Equivalent") +
  facet_wrap(~Final_Demand_Sector_Category) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 9),
    legend.position = "bottom",
    legend.margin = margin(t = -10, b = 0),
    panel.spacing.x = unit(3, "mm"),
    plot.margin = margin(t = 10, b = 15, r = 5.5, l = 5.5)
  )

ggpubr::ggarrange(
  p_useful, 
  p_threshold,
  nrow = 2,
  labels = c("a", "b"),
  heights = c(1, 1.05),
  common.legend = TRUE,
  legend = "bottom",
  align = "v"
)


