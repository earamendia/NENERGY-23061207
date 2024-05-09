
# EROI Equivalent, national level, for all fossil fuels, with dot sizes -------------------
# Code for Figure 5 and Extended Data Figure 5

tar_load(required_national_erois)

tar_load(share_ff_to_tfc)

share_ff_to_tfc_2019 <- share_ff_to_tfc %>% 
  dplyr::filter(Year == 2020)

tar_load(share_ff_group_use_by_country)

share_2019 <- share_ff_group_use_by_country %>% 
  dplyr::filter(Year == 2020) %>% 
  dplyr::filter(Energy.stage == "Final (fuel+elec+heat)") %>% 
  dplyr::mutate(
    Energy.Carrier = "All carriers"
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-Year, -Energy.type, -Method, -Energy.stage)


# EROI equivalents with share of fossil fuel consumption too
a_prelim <- required_national_erois %>% 
  dplyr::filter(Year >= 2000) %>% 
  dplyr::group_by(Country, Method, Energy.type, Product.Group, Eroi.method, Type, Boundary, Manufacture, Energy.Carrier, Non_Energy_Uses, Indirect_Energy, Method_idE) %>% 
  dplyr::summarise(
    Required_EROI = mean(Required_EROI)
  ) %>% 
  dplyr::filter(Type == "Gross") %>% 
  dplyr::filter(Boundary == "All") %>% 
  dplyr::filter(Manufacture == "Renewable") %>% 
  dplyr::filter(Method_idE == "Average") %>% 
  dplyr::filter(Energy.Carrier == "All carriers") %>% 
  dplyr::left_join(
    share_ff_to_tfc_2019,
    by = c("Country", "Method", "Energy.type", "Product.Group")
  ) %>% 
  dplyr::filter(!is.na(ff_to_tfc)) %>% 
  dplyr::left_join(
    share_2019,
    by = c("Country", "Product.Group", "Energy.Carrier")
  ) %>% 
  filter(!is.na(Share_FF_Use_By_Country)) %>% 
  dplyr::mutate(
    Product.Group = stringr::str_replace(Product.Group, "Natural gas", "Fossil gas")
  )


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
    Technology = stringr::str_replace(Technology, "Wind", "Wind power"),
  ) %>% 
  tidyr::expand_grid(
    Product.Group = c("All fossil fuels", "Coal products", "Oil and gas products", "Oil products", "Fossil gas")
  ) %>% 
  dplyr::filter(!Ref %in% c("de Castro and Capellan-Perez 2020", "Celik et al., 2018"))


low_erois <- harmonised_erois %>% 
  dplyr::group_by(Technology) %>% 
  dplyr::summarise(EROI = min(EROI_incl_TDL)) %>% 
  dplyr::mutate(
    Value = "Lowest"
  )

median_erois <- harmonised_erois %>% 
  dplyr::group_by(Technology) %>% 
  dplyr::summarise(EROI = median(EROI_incl_TDL)) %>% 
  dplyr::mutate(
    Value = "Median"
  )

erois_to_display <- dplyr::bind_rows(low_erois, median_erois) %>% 
  tidyr::expand_grid(
    ff_to_tfc = seq(0, 1, 0.01)
  )



list_eu_countries <- c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", "NLD",
                       "GRC", "POL", "PRT", "ROU", "SLV", "SVK", "SWE")
list_oecd_countries <- c("AUS", "CAN", "CHL", "COL", "ISL", "ISR", "KOR", "NZL", "NOR", "TUR", "GBR")

a_main <- a_prelim %>% 
  dplyr::mutate(
    alpha_value = "Normal"
  ) %>% 
  dplyr::mutate(
    Region = dplyr::case_when(
      Country == "USA" ~ "US",
      Country == "CHNM" ~ "China",
      Country == "RUS" ~ "Russia",
      Country == "IND" ~ "India",
      Country == "IDN" ~ "Indonesia",
      Country == "IRN" ~ "Iran",
      Country == "SAU" ~ "Saudi Arabia",
      Country == "JPN" ~ "Japan",
      Country == "BRA" ~ "Brazil",
      Country == "MEX" ~ "Mexico",
      Country %in% list_eu_countries ~ "EU27",
      Country %in% list_oecd_countries ~ "OECD",
    )
  ) %>% 
  dplyr::filter(Product.Group == "All fossil fuels")

# Adding text for low EROIs instead of legend
low_erois_vector <- dplyr::pull(low_erois, EROI)

text_to_add <- data.frame(
  ff_to_tfc = c(0, 0),
  Required_EROI = low_erois_vector,
  Technology = c("Solar PV", "Wind"),
  Text = c("Lowest EROI - Solar PV", "Lowest EROI - Wind power")
)


# For this graph here, we keep only "All fossil fuels" together
# Code for Figure 5
x11()
a_main %>% 
  filter(!is.na(Region)) %>% 
  filter(! Country %in% c("COG", "PRK", "UZB")) |> 
  dplyr::filter(Required_EROI <= 15) %>% 
  ggplot(aes(x = ff_to_tfc, y = Required_EROI)) +
  geom_point(aes(size = Share_FF_Use_By_Country, alpha = alpha_value, fill = Region), shape = 21) +
  geom_point(data = a_main %>% dplyr::filter(is.na(Region)) %>% dplyr::filter(Required_EROI <= 15) |> filter(! Country %in% c("COG", "PRK", "UZB")),
             aes(size = Share_FF_Use_By_Country, alpha = alpha_value, fill = Region), shape = 21) +
  geom_text(text_to_add, mapping = aes(x = ff_to_tfc, y = Required_EROI, label = Text, col = Technology), hjust = 0, nudge_y = 0.09, nudge_x = 0.01, size = 3) +
  geom_line(data = erois_to_display %>% 
              dplyr::filter(Value == "Lowest"), 
            aes(y = EROI, x = ff_to_tfc, col = Technology), linewidth = 0.3) +
  scale_size_continuous(range = c(1, 15), labels = c("<0.01", 0.01, 0.02, 0.05, "0.10", 0.15, "0.20"), breaks = c(0.0001, 0.01, 0.02, 0.05, 0.10, 0.15, 0.2)) +
  scale_color_viridis_d(begin = 0, end = 0.5, option = "viridis", breaks = c("Wind", "Solar PV")) +
  scale_fill_viridis_d(option = "turbo", na.translate = FALSE) +
  scale_alpha_manual(values = c(1, 0)) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0.02)) +
  scale_y_continuous(limits = c(1.8, 8)) +
  ylab("Renewable energy Energy Return On Investment Equivalent") +
  xlab("Share of fossil fuels in final energy consumption") +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.text = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.margin = margin(t = 5.5, b = 2, r = 4, l = 0, unit = "pt"),
  ) +
  guides(
    alpha = "none",
    size = guide_legend(order = 1, override.aes = list(alpha = 1)),
    fill = guide_legend(order = 2, override.aes = list(alpha = 1, size = 4)),
    col = "none"
  ) +
  labs(
    size = "Share global fossil\nfuel consumption",
    col = "Technology"
  )




# Code for Extende Data Figure 5

added_rows_si <- a_prelim %>% 
  dplyr::filter(Product.Group != "All fossil fuels") %>% 
  ungroup() %>% 
  arrange(desc(Share_FF_Use_By_Country)) %>% 
  slice(c(1, n())) %>% 
  select(-Product.Group) %>% 
  tidyr::expand_grid(
    Product.Group = c("Coal products", "Fossil gas", "Oil products", "Oil and gas products")
  ) %>% 
  mutate(
    alpha_value = "Transparent",
    Country = "Fake observation"
  )


a_si_graphs <- a_prelim %>% 
  mutate(
    alpha_value = "Normal"
  ) %>% 
  dplyr::bind_rows(added_rows_si) %>% 
  dplyr::mutate(
    Region = dplyr::case_when(
      Country == "USA" ~ "US",
      Country == "CHNM" ~ "China",
      Country == "RUS" ~ "Russia",
      Country == "IND" ~ "India",
      Country == "IDN" ~ "Indonesia",
      Country == "IRN" ~ "Iran",
      Country == "SAU" ~ "Saudi Arabia",
      Country == "JPN" ~ "Japan",
      Country == "BRA" ~ "Brazil",
      Country == "MEX" ~ "Mexico",
      Country %in% list_eu_countries ~ "EU27",
      Country %in% list_oecd_countries ~ "OECD",
    )
  ) %>% 
  dplyr::filter(Product.Group != "All fossil fuels")


x11()
p_coal_gas <- a_si_graphs %>% 
  dplyr::filter(Product.Group %in% c("Coal products", "Fossil gas")) %>% 
  filter(! Country %in% c("COG", "PRK", "UZB")) |> 
  dplyr::filter(Required_EROI <= 40) %>% 
  ggplot(aes(x = ff_to_tfc, y = Required_EROI)) +
  geom_point(aes(size = Share_FF_Use_By_Country, alpha = alpha_value, fill = Region), shape = 21) +
  geom_line(data = erois_to_display, 
            aes(y = EROI, x = ff_to_tfc, col = Technology, linetype = Value), linewidth = 0.3) +
  scale_size_continuous(range = c(1, 18), labels = c("<0.01", "0.01", "0.02", "0.05", "0.10", "0.20", "0.50"), breaks = c(0.0001, 0.01, 0.02, 0.05, 0.10, 0.2, 0.5)) +
  scale_color_viridis_d(begin = 0, end = 0.5, option = "viridis", breaks = c("Wind power", "Solar PV")) +
  scale_fill_viridis_d(option = "turbo", na.translate = FALSE) +
  scale_linetype_manual(values = c(2, 1), breaks = c("Median", "Lowest")) +
  scale_alpha_manual(values = c(0.8, 0)) +
  scale_y_continuous(limits = c(0, NA), expand = c(0.02, 0.02)) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0.03)) +
  ylab("Renewable energy Energy Return On Investment Equivalent") +
  xlab("Share of the specific fossil fuel in final energy consumption") +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.text = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.margin = margin(l = 3, unit = "pt"),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 8),
    plot.margin = margin(t = 5.5, b = 2, r = 4, l = 0, unit = "pt"),
  ) +
  guides(
    alpha = "none",
    size = guide_legend(order = 1, override.aes = list(alpha = 1)),
    fill = guide_legend(order = 2, override.aes = list(alpha = 0.8, size = 4)),
    col = guide_legend(order = 3),
    linetype = guide_legend(order = 4)
  ) +
  labs(
    size = "Share global fossil\nfuel consumption",
    col = "Technology"
  ) +
  facet_wrap(~Product.Group)


p_oil_gas <- a_si_graphs %>% 
  dplyr::filter(Product.Group %in% c("Oil products", "Oil and gas products")) %>% 
  filter(! Country %in% c("COG", "PRK", "UZB")) |> 
  dplyr::filter(Required_EROI <= 8.5) %>% 
  ggplot(aes(x = ff_to_tfc, y = Required_EROI)) +
  geom_point(aes(size = Share_FF_Use_By_Country, alpha = alpha_value, fill = Region), shape = 21) +
  geom_line(data = erois_to_display, 
            aes(y = EROI, x = ff_to_tfc, col = Technology, linetype = Value), linewidth = 0.3) +
  scale_size_continuous(range = c(1, 18), labels = c("<0.01", "0.01", "0.02", "0.05", "0.10", "0.20", "0.50"), breaks = c(0.0001, 0.01, 0.02, 0.05, 0.10, 0.2, 0.5)) +
  scale_color_viridis_d(begin = 0, end = 0.5, option = "viridis", breaks = c("Wind power", "Solar PV")) +
  scale_fill_viridis_d(option = "turbo", na.translate = FALSE) +
  scale_linetype_manual(values = c(2, 1), breaks = c("Median", "Lowest")) +
  scale_alpha_manual(values = c(0.8, 0)) +
  scale_y_continuous(limits = c(2, 8), expand = c(0.02, 0.02)) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0.03)) +
  ylab("Renewable energy Energy Return On Investment Equivalent") +
  xlab("Share of the specific fossil fuel in final energy consumption") +
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.text = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 8),
    plot.margin = margin(t = 5.5, b = 2, r = 4, l = 0, unit = "pt"),
  ) +
  guides(
    alpha = "none",
    size = guide_legend(order = 1, override.aes = list(alpha = 1)),
    fill = guide_legend(order = 2, override.aes = list(alpha = 0.8, size = 4)),
    col = guide_legend(order = 3),
    linetype = guide_legend(order = 4)
  ) +
  labs(
    size = "Share global fossil\nfuel consumption",
    col = "Technology"
  ) +
  facet_wrap(~Product.Group) +
  guides(
    alpha = "none",
    col = "none",
    linetype = "none",
    size = "none",
    fill = "none"
  )


p_main <- ggpubr::ggarrange(p_coal_gas,
                            p_oil_gas,
                            nrow = 2,
                            common.legend = TRUE,
                            legend = "right",
                            align = "v",
                            widths = c(1.05, 1))


ggpubr::annotate_figure(
  p_main,
  bottom = text_grob("Share of the specific fossil fuel in final energy consumption", size = 8)
)

