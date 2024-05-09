
# In this file we do a copy of the figure showing the EROI equivalent value for different idE calc methods
# then we also do the useful stage EROI graphs showing the difference between both methods
# Code for Figures SI.7 and SI.8

# Libraries
library(targets)
library(ggplot2)
library(dplyr)
library(viridis)
library(ggpubr)


# Sensitivity idE - Static EROI equivalents, global level ---------------------

# Reading targets data
tar_load(required_global_erois)
method_idE_factor <- c("Excluded", "Underestimation", "Average", "Overestimation")

tar_load(required_global_erois_by_eu)
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


# Code Figure SI.8

# First graph of EROI equivalents without breakdown and comparing with RET:
p1 <- required_global_erois %>% 
  dplyr::filter(Year == 2020) %>% 
  dplyr::filter(Type == "Gross", Boundary == "All") %>% 
  dplyr::filter(Manufacture == "Renewable") %>% 
  dplyr::filter(Energy.Carrier == "All carriers") %>%
  tidyr::replace_na(list(Method_idE = "Excluded")) %>% 
  dplyr::filter(Method_idE %in% c("Underestimation", "Overestimation")) %>% 
  dplyr::select(-Indirect_Energy) %>% 
  tidyr::pivot_wider(
    names_from = Method_idE, values_from = Required_EROI
  ) %>%
  dplyr::mutate(
    Underestimation = Underestimation - Overestimation
  ) %>%
  tidyr::pivot_longer(
    cols = c("Underestimation", "Overestimation"),
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
  geom_bar(aes(fill = factor(Product.Group, levels = short_group_name_factor_excl_RET), alpha = factor(Method_idE, levels = c("Underestimation", "Overestimation"))),
           stat = "identity", position = "stack", width = 0.5) +
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
    axis.ticks.length=unit(-0.3 * 12,  "pt"),
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
  dplyr::filter(Energy.Carrier == "All carriers") %>% 
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
  dplyr::filter(Method_idE %in% c("Underestimation", "Overestimation")) %>% 
  dplyr::select(-Indirect_Energy) %>% 
  tidyr::pivot_wider(
    names_from = Method_idE, values_from = Required_EROI
  ) %>% 
  dplyr::mutate(
    Underestimation = Underestimation - Overestimation
  ) %>% 
  tidyr::pivot_longer(
    cols = c("Underestimation", "Overestimation"),
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
  geom_bar(aes(fill = factor(Product.Group, levels = short_group_name_factor_excl_RET), alpha = factor(Method_idE, levels = c("Underestimation", "Overestimation"))), 
           stat = "identity", width = 0.9) +
  geom_jitter(data = harmonised_erois_for_end_uses, aes(x = Product.Group, y = EROI_incl_TDL, col = Technology), width = 0.32, size = 1) +
  scale_fill_viridis_d(end = 0.7) +
  scale_alpha_discrete(range = c(0.8, 1)) +
  scale_color_viridis_d(begin = 0, end = 0.8, option = "viridis") +
  xlab("Product Group") +
  ylab("Final stage Energy Return On Investment") +
  coord_cartesian(ylim = c(0, 25)) +
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
    axis.ticks.length=unit(-0.3 * 12,  "pt"),
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
  heights = c(1, 1.45),
  common.legend = TRUE,
  legend = "right")




# Sensitivity idE - Useful stage EROIs, global level ---------------------
# Figure SI.7

tar_load(aggregated_global_erois_without_breakdown_idE)

tar_load(aggregated_global_erois_by_eu_idE)

list_relevant_end_uses <- c("HTH", "LTH", "MECH", "MTH", "RaP", "RoP")
end_uses_factor <- c("Low Temperature Heating", "Medium Temperature Heating", "High Temperature Heating", "Rail Propulsion", "Road Propulsion", "Mechanical Work")

# All fossil fuels together:
p1 <- aggregated_global_erois_without_breakdown_idE %>% 
  filter(Method_idE %in% c("Underestimation", "Overestimation")) %>% 
  filter(Boundary == "All", Type == "Gross") %>% 
  dplyr::filter(Energy.stage %in% c("Final (fuel+elec+heat)", "Useful (fuel+elec+heat)")) %>%
  dplyr::mutate(
    Energy.stage = dplyr::case_when(
      Energy.stage == "Final (fuel+elec+heat)" ~ "Final",
      Energy.stage == "Useful (fuel+elec+heat)" ~ "Useful",
    )
  ) %>% 
  dplyr::filter(Product.Group == "All fossil fuels") %>% 
  dplyr::mutate(
    Product.Group = stringr::str_replace(Product.Group, "Natural gas", "Fossil gas"),
    Product.Group = stringr::str_replace(Product.Group, "All fossil fuels", "Average mix")
  ) %>% 
  ggplot(aes(x = Year, y = Group.eroi)) +
  geom_line(aes(col = Energy.stage, linetype = Method_idE)) +
  facet_wrap(vars(Product.Group)) +
  ylab("Energy Return On Investment") +
  scale_colour_viridis_d(end = 0.7, direction = -1) +
  theme_bw() +
  scale_y_continuous(limits = c(0, NA), breaks = c(0, 2, 4, 6, 8, 10)) +
  labs(
    linetype = "Indirect Energy",
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
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 9),
    legend.position = "bottom",
  ) +
  guides(
    col = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  )


# Breakdown by group:
p2 <- aggregated_global_erois_without_breakdown_idE %>% 
  filter(Method_idE %in% c("Underestimation", "Overestimation")) %>% 
  filter(Boundary == "All", Type == "Gross") %>% 
  dplyr::filter(Energy.stage %in% c("Final (fuel+elec+heat)", "Useful (fuel+elec+heat)")) %>% 
  dplyr::mutate(
    Energy.stage = dplyr::case_when(
      Energy.stage == "Final (fuel+elec+heat)" ~ "Final",
      Energy.stage == "Useful (fuel+elec+heat)" ~ "Useful",
    )
  ) %>% 
  dplyr::filter(Product.Group %in% c("Coal products", "Natural gas")) %>% 
  dplyr::mutate(
    Product.Group = stringr::str_replace(Product.Group, "Natural gas", "Fossil gas"),
    Product.Group = stringr::str_replace(Product.Group, "All fossil fuels", "Average mix")
  ) %>% 
  ggplot(aes(x = Year, y = Group.eroi)) +
  geom_line(aes(col = Energy.stage, linetype = Method_idE)) +
  facet_wrap(vars(Product.Group)) +
  ylim(c(0, NA)) +
  scale_colour_viridis_d(end = 0.7, direction = -1) +
  theme_bw() +
  labs(
    linetype = "Indirect Energy",
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
  filter(Method_idE %in% c("Underestimation", "Overestimation")) %>% 
  filter(Boundary == "All", Type == "Gross") %>% 
  dplyr::filter(Energy.stage %in% c("Final (fuel+elec+heat)", "Useful (fuel+elec+heat)")) %>% 
  dplyr::mutate(
    Energy.stage = dplyr::case_when(
      Energy.stage == "Final (fuel+elec+heat)" ~ "Final",
      Energy.stage == "Useful (fuel+elec+heat)" ~ "Useful",
    )
  ) %>% 
  dplyr::filter(Product.Group %in% c("Oil and gas products", "Oil products")) %>% 
  dplyr::mutate(
    Product.Group = stringr::str_replace(Product.Group, "Natural gas", "Fossil gas"),
    Product.Group = stringr::str_replace(Product.Group, "All fossil fuels", "Average mix")
  ) %>% 
  ggplot(aes(x = Year, y = Group.eroi)) +
  geom_line(aes(col = Energy.stage, linetype = Method_idE)) +
  facet_wrap(vars(Product.Group)) +
  scale_colour_viridis_d(end = 0.7, direction = -1) +
  theme_bw() +
  labs(
    linetype = "Indirect Energy",
    col = "Energy stage"
  ) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
  scale_y_continuous(limits = c(0, NA), breaks = c(0, 2, 4, 6, 8)) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_blank(),
    legend.position = "none",
    legend.margin = margin(t = -12, b = 2),
    panel.spacing.x = unit(4, "mm"),
    axis.title.y = element_blank(),
    plot.margin = margin(t = 1, b = 5.5, r = 5.5, l = 0, unit = "pt")
  )

p2_mixed <- ggpubr::ggarrange(
  p2,
  p2_prime,
  nrow = 2,
  heights = c(1, 1.15),
  align = "v"
)

x11()
ggpubr::ggarrange(p1, p2_mixed, ncol = 2, common.legend = TRUE, legend = "bottom", widths = c(1, 1.3))

