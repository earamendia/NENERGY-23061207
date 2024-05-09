
# In this file we plot all useful stage EROI graphs required for the paper.
# Code for Figures 2 and 3

# Libraries
library(targets)
library(ggplot2)
library(dplyr)
library(viridis)



# Useful stage EROIs (dynamic) at the global level, including extrapolated idE ------------------------
# Code for Figure 2

tar_load(aggregated_global_erois_without_breakdown_idE)

tar_load(aggregated_global_erois_by_eu_idE)

list_relevant_end_uses <- c("HTH", "LTH", "MECH", "MTH", "RaP", "RoP")
end_uses_factor <- c("Low Temperature Heating", "Medium Temperature Heating", "High Temperature Heating", "Rail Propulsion", "Road Propulsion", "Mechanical Work")

# All fossil fuels together:
p1 <- aggregated_global_erois_without_breakdown_idE %>% 
  filter(Indirect_Energy == "Included", Method_idE == "Average") %>% 
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
  geom_line(aes(col = Energy.stage, linetype = Energy.stage)) +
  facet_wrap(vars(Product.Group)) +
  ylab("Energy Return On Investment") +
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
    axis.title.y = element_text(size = 9),
    legend.position = "bottom",
    legend.margin = margin(t = -12, b = 2),
  )


# Breakdown by group:
p2 <- aggregated_global_erois_without_breakdown_idE %>% 
  filter(Indirect_Energy == "Included", Method_idE == "Average") %>% 
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
  geom_line(aes(col = Energy.stage, linetype = Energy.stage)) +
  facet_wrap(vars(Product.Group)) +
  ylim(c(0, NA)) +
  scale_colour_viridis_d(end = 0.7, direction = -1) +
  theme_bw() +
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
  filter(Indirect_Energy == "Included", Method_idE == "Average") %>% 
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
  geom_line(aes(col = Energy.stage, linetype = Energy.stage)) +
  facet_wrap(vars(Product.Group)) +
  scale_colour_viridis_d(end = 0.7, direction = -1) +
  theme_bw() +
  labs(
    linetype = "Energy stage",
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
    axis.title.x = element_text(size = 9),
    legend.position = "none",
    legend.margin = margin(t = -12, b = 2),
    panel.spacing.x = unit(4, "mm"),
    axis.title.y = element_blank(),
    plot.margin = margin(t = 1, b = 5.5, r = 5.5, l = 0, unit = "pt")
  )


# Fossil fuels by group
scale_adjusting_df <- data.frame(
  x_coordinate = rep(1971, 6),
  y_coordinate = c(0, 17, 17, 6, 6, 6),
  EU_category = c("Low Temperature Heating", "Medium Temperature Heating", "High Temperature Heating", "Road Propulsion", "Rail Propulsion", "Mechanical Work")
)

p3 <- aggregated_global_erois_by_eu_idE %>% 
  filter(Indirect_Energy == "Included", Method_idE == "Average") %>% 
  filter(Boundary == "All", Type == "Gross") %>% 
  dplyr::filter(Energy.stage == "Useful (fuel+elec+heat)") %>% 
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
    Product.Group = stringr::str_replace(Product.Group, "Natural gas", "Fossil gas"),
    Product.Group = stringr::str_replace(Product.Group, "All fossil fuels", "Average mix")
  ) %>% 
  ggplot(aes(x = Year, y = Group.eroi)) +
  geom_line(aes(col = Product.Group, linetype = Product.Group)) +
  geom_point(data = scale_adjusting_df, aes(x = x_coordinate, y = y_coordinate), alpha = 0) +
  ylab("Energy Return On Investment") +
  ylim(c(0, NA)) +
  scale_color_viridis_d(option = "viridis", end = 0.7) +
  facet_wrap(vars(factor(EU_category, levels = end_uses_factor)), scales = "free_y") +
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
  heights = c(1, 1.15),
  align = "v"
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



# Breakdown of energy inputs in terms of direct and indirect energy -------

tar_load(dE_vs_idE_global)

scope_levels <- c("Direct", "Indirect")


a <- dE_vs_idE_global %>% 
  filter(Indirect_Energy == "Included", Method_idE == "Average") %>% 
  filter(Energy.stage == "Final (fuel)") %>% 
  filter(Type == "Gross", Boundary == "All") %>% 
  dplyr::group_by(Country, Method, Energy.type, Last.stage, Eroi.method, Type, Boundary, Product.Group,
                  Energy.stage, Non_Energy_Uses, Indirect_Energy, Method_idE) %>% 
  dplyr::summarise(
    idE_to_dE_avg_ratio = mean(idE_to_dE_ratio)
  ) %>% 
  tidyr::expand_grid(
    Scope = c("Direct", "Indirect")
  ) %>% 
  dplyr::mutate(
    Energy_Input = dplyr::case_when(
      Scope == "Direct" ~ 1 / (1 + idE_to_dE_avg_ratio),
      Scope == "Indirect" ~ 1 / (1 + 1/idE_to_dE_avg_ratio)
    )
  ) %>% 
  dplyr::mutate(
    Product.Group = stringr::str_replace(Product.Group, "Natural gas", "Fossil gas"),
    Product.Group = stringr::str_replace(Product.Group, "All fossil fuels", "Average mix")
  )


# Code for Figure 3

x11()
a %>% 
  ggplot(aes(x = Energy_Input, y = factor(Product.Group, levels = rev(c("Average mix", "Coal products", "Fossil gas", "Oil and gas products", "Oil products"))))) +
  geom_bar(aes(fill = factor(Scope, levels = scope_levels)), stat = "identity", width = 0.8) +
  scale_fill_viridis_d(option = "viridis", begin = 0.05, end = 0.5, direction = 1) +
  xlab("Breakdown of energy invested") +
  theme_bw() +
  theme(
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_blank(),
    legend.position = "right",
  ) +
  labs(fill = "Energy", alpha = "Indirect Energy") +
  guides(
    fill = guide_legend(reverse = TRUE),
    alpha = guide_legend(reverse = TRUE)
  )

ggsave("Figures/breakdown_dE_idE_avg.pdf",
       width = 180,
       height = 80,
       units = "mm")

