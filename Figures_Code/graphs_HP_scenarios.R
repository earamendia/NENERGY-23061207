
# Code for Extended Data Figure 6

# Reading targets data
tar_load(required_global_erois_HPS)
method_idE_factor <- c("Excluded", "Underestimation", "Average", "Overestimation")

tar_load(required_global_erois_by_eu_HPS)
list_relevant_end_uses <- c("HTH", "LTH", "MECH", "MTH", "RaP", "RoP")
end_uses_factor <- c("Low Temperature Heating", "Medium Temperature Heating", "High Temperature Heating", "Rail Propulsion", "Road Propulsion", "Mechanical Work")

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

harmonised_erois_for_end_uses <- harmonised_erois %>% 
  dplyr::mutate(
    Product.Group = Technology
  )

short_group_name_factor_excl_RET <- c("Average fossil\nfuel mix", "Coal products", "Fossil gas", "Oil and gas", "Oil products")


# First graph of required EROIs without breakdown and comparing with RET:
p1 <- required_global_erois_HPS %>% 
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
p2 <- required_global_erois_by_eu_HPS %>% 
  dplyr::filter(Year == 2020) %>% 
  dplyr::filter(Type == "Gross", Boundary == "All") %>%
  dplyr::filter(Manufacture == "Renewable") %>% 
  dplyr::filter(Energy.Carrier == "All carriers") %>% 
  dplyr::filter(EU_category %in% c("LTH", "MTH")) %>% 
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
  scale_fill_viridis_d(end = 0.7) +
  scale_alpha_discrete(range = c(0.8, 1)) +
  scale_color_viridis_d(begin = 0, end = 0.8, option = "viridis") +
  coord_cartesian(ylim = c(0, 25)) +
  xlab("Product Group") +
  ylab("Final stage Energy Return On Investment") +
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
  heights = c(1, 1),
  common.legend = TRUE,
  legend = "right")

