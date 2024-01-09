# Setup -------------------------------------------------------------------



library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(scico)
library(geofacet)
library(ggplot2)
library(ggblend)
library(ggfx)
library(showtext)
library(rnaturalearth)
library(ggtext)
library(klibiwinds)
library(paletteer)
library(patchwork)
library(showtext)
showtext_auto()
# font_add_google("Lilita One", "lilita")
# font_add_google("Ysabeau Office", "ysabeau")
# font_add_google("Chela One", "chela")
# font_add_google("Calistoga", "calistoga")
font_add_google("Oleo Script", "oleo")
# font_add_google("Quicksand", "quicksand")
# font_add_google("Josefin Sans", "josefin")
font_add_google("Dosis", "dosis")
# font_add_google("Jost", "jost")
font_title <- "oleo"
font_subtitle <- "dosis"
showtext_auto()
colour_panel <- "#040720"
colour_panel_light <- NA
colour_title <- "white"
colour_subtitle <- "grey70"
colour_title_light <- "grey15"
colour_subtitle_light <- "grey50"
colour_endpoint <- "white"
colour_endpoint_light <- colour_panel
colour_discharge <- "#b38435"
colour_recharge <- "#506ea7"
# colour_referencecomparison <- c("#FF3D7FFF", "#DAD8A7FF", "#3FB8AFFF")
# colour_referencecomparison <- c("#B200B2FF", "#FFFF66FF")
colour_referencecomparison <- c("#FF3D7FFF", "#3FB8AFFF")
panel_spacing <- 0
year_start <- 1991
year_end <- 2017
year_refstart_referencecomparison <- 1981
year_refend_referencecomparison <- 2010
year_focus_referencecomparison <- 2015
buda_low <- c("#B200B2FF", "#B2F2FDFF")
buda_high <- c("#FFFF66FF", "#8C0172FF")
size_title <- 320
size_subtitle <- 90
lineheight_subtitle <- 0.5
shadow_sigma <- 45
subtitle_polar <- stringr::str_glue(
  "Time series of measured groundwater levels across Germany in polar coordinates.<br>
The time series cover the period between <b><span style='color:{buda_low};'>{year_start}</span></b> and <b><span style='color:{buda_high};'>{year_end}</span></b> as much as available.<br>
Here, the groundwater levels are shown as normalized instead of absolute values<br>
in order to visually compare between different regions."
)
subtitle_referencecomparison_kartesian <- stringr::str_glue(
  "Groundwater level time series in Germany for the year {year_focus_referencecomparison}. For the exact locations of the related observation wells<br>
check out the mini-map. The backfilled area shows 90% of measured levels during the reference period ({year_refstart_referencecomparison}&#8211;{year_refend_referencecomparison}).<br>
The colored lines indicate levels <b><span style='color:{colour_referencecomparison[1]};'>below</span></b> or <b><span style='color:{colour_referencecomparison[2]};'>above</span></b> the median levels over the reference period.<br>
Groundwater levels are normalized for easy visual comparison of dynamics rather than absolute values."
)
subtitle_cartesian <- stringr::str_glue(
  "Time series of measured groundwater levels across Germany.<br>
The time series cover the period between <b><span style='color:{buda_low};'>{year_start}</span></b> and <b><span style='color:{buda_high};'>{year_end}</span></b> as much as available.<br>
Here, the groundwater levels are shown as normalized instead of absolute values<br>
in order to visually compare between different regions."
)
subtitle_joydivision <- stringr::str_glue(
  "Time series of measured groundwater levels across Germany.<br>
The time series cover the period between {buda_low} and {buda_high} as much as available.<br>
Here, the groundwater levels are shown as normalized instead of absolute values<br>
in order to visually compare between different regions."
)
caption <- "By Max Nölscher   |   Data from German Geological Surveys and Authorities   |   Made in R"
# key_well_meta <- read_rds("key_well_meta.rds")


# Data --------------------------------------------------------------------

## GWL Correctiv Data ------------------------------------------------------

caption <- "By Max Nölscher   |   Data from CORRECTIV.Lokal   |   Made in R"
year_start <- 1991
year_end <- 2020

key_well_meta_sf <- read_csv("https://raw.githubusercontent.com/correctiv/grundwasser-data/main/messstellen.csv") |>
  drop_na(all_of(c("longitude", "latitude"))) |>
  sf::st_as_sf(coords = c("longitude", "latitude")) |>
  sf::st_sf(crs = 4326) |>
  rename(well_id = ms_nr) |>
  sf::st_transform(25832)


admin_borders_germany <- ne_countries(country = "Germany", scale = "medium") |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 25832)

data_gwl <- c("BW", "BY", "BE", "BB", "HB", "HH", "HE", "MV", "NI", "NW", "RP", "SL", "SN", "ST", "SH", "TH") |>
  stringr::str_to_lower() %>%
  stringr::str_c("https://raw.githubusercontent.com/correctiv/grundwasser-data/main/monthly/", ., "_monthly.csv", sep = "") |>
  map_df(readr::read_csv, .progress = TRUE)  |>
  as_tibble() |>
  rename(well_id = ms_nr) |>
  mutate(date = paste(year, month, 15, sep = "-") |> lubridate::ymd(), .after = well_id)

data_gwl <-
  data_gwl |>
  filter(well_id %in% unique(key_well_meta_sf$well_id))

data_gwl_prep <- data_gwl |>
  mutate(
    climate_model_name = 1,
  )

data_gwl_prep <- data_gwl_prep |>
  mutate(reference_period = if_else(year |> between(year_start, year_end), "Z1", NA_character_)) |>
  drop_na(reference_period, contains("_gwl"))

data_gwl_prep <- data_gwl_prep |>
  group_by(well_id, climate_model_name, year) |>
  # tsibble::as_tsibble(key = well_id, index = date) |>
  # tsibble::group_by_key() |>
  # tsibble::fill_gaps() |>
  # use_water_year() |>
  filter(n() >= 10) |>
  # filter(n() >= 12 * 30 * 0.9) |>
  group_by(well_id)

indicators_summary <- data_gwl_prep |>
  ungroup() |>
  make_summary_table() |>
  add_indicator_1_7(data_gwl_prep |> rename(gwl = mean_gwl)) |>
  add_indicator_2_1(data_gwl_prep |> rename(gwl = min_gwl)) |>
  add_indicator_2_2(data_gwl_prep |> rename(gwl = max_gwl)) |>
  add_indicator_3_1(data_gwl_prep |> rename(gwl = min_gwl)) |>
  add_indicator_3_2(data_gwl_prep |> rename(gwl = max_gwl))

indicators_summary_sf <-
  key_well_meta_sf |>
  inner_join(indicators_summary, by = join_by(well_id))

grid_germany <- sf::st_make_grid(indicators_summary_sf, cellsize = 25000, square = FALSE) %>%
  sf::st_sf() %>%
  filter(sf::st_intersects(., admin_borders_germany, sparse = FALSE)[, 1]) |>
  mutate(hex_id = row_number())

indicators_summary_sf <- indicators_summary_sf |>
  sf::st_join(grid_germany)

indicators_summary_hex <- indicators_summary_sf |>
  sf::st_drop_geometry() |>
  group_by(hex_id) |>
  summarise(
    indicator_17 = mean(indicator_17),
    indicator_21 = mean(indicator_21),
    indicator_22 = mean(indicator_22),
    indicator_31 = psych::circadian.mean(indicator_31 * 2) / 2,
    indicator_32 = psych::circadian.mean(indicator_32 * 2) / 2,
    n_obs = n()
  ) |>
  mutate(duration_recharge = if_else(
    indicator_32 < indicator_31,
    indicator_32 + 12 - indicator_31,
    indicator_32 - indicator_31
  )) |>
  mutate(duration_discharge = 12 - duration_recharge)

data_plot <- grid_germany |>
  left_join(indicators_summary_hex, by = join_by(hex_id))
# mutate(n_obs = replace_na(n_obs, 0))

hull <- data_plot |>
  sf::st_union()


## Data BGR ----------------------------------------------------------------


key_well_meta <- read_csv("J:/NUTZER/Noelscher.M/Studierende/Daten/groundwater_levels/germany/mixed_daily_to_monthly/tabular/bgr/description/metadata_timeseries.csv")

key_well_meta_sf <- key_well_meta |>
  select(well_id = proj_id, contains("coord"), surface_elevation) |>
  sf::st_as_sf(coords = c("x_coord", "y_coord")) |>
  sf::st_sf(crs = 25832)

admin_borders_germany <- ne_countries(country = "Germany", scale = "medium") |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 25832)

data_gwl <- feather::read_feather("J:/NUTZER/Noelscher.M/Studierende/Daten/groundwater_levels/germany/mixed_daily_to_monthly/tabular/bgr/data/processed_timeseries_as_feather/groundwater_levels_as_feather")

data_gwl_prep <- data_gwl |>
  select(well_id = proj_id, date = datum, gwl = nn_messwert) |>
  mutate(
    date = as.Date(date),
    # reference_period = 1,
    climate_model_name = 1,
  )

data_gwl_prep <- data_gwl_prep |>
  # use_water_year() |>
  add_reference_period_column()

data_gwl_prep <- data_gwl_prep |>
  # use_water_year() |>
  filter(reference_period == "Z1")

data_gwl_prep <- data_gwl_prep |>
  group_by(well_id, climate_model_name) |>
  # tsibble::as_tsibble(key = well_id, index = date) |>
  # tsibble::group_by_key() |>
  # tsibble::fill_gaps() |>
  drop_na(gwl) |>
  # use_water_year() |>
  filter(n() >= 52 * 30 * 0.9) |>
  group_by(well_id)

indicators_summary <- data_gwl_prep |>
  ungroup() |>
  make_summary_table() |>
  add_indicator_1_7(data_gwl_prep) |>
  add_indicator_2_1(data_gwl_prep) |>
  add_indicator_2_2(data_gwl_prep) |>
  add_indicator_3_1(data_gwl_prep) |>
  add_indicator_3_2(data_gwl_prep)

indicators_summary_sf <-
  key_well_meta_sf |>
  inner_join(indicators_summary, by = join_by(well_id))

grid_germany <- sf::st_make_grid(indicators_summary_sf, cellsize = 25000, square = FALSE) %>%
  sf::st_sf() %>%
  filter(sf::st_intersects(., admin_borders_germany, sparse = FALSE)[, 1]) |>
  mutate(hex_id = row_number())

indicators_summary_sf <- indicators_summary_sf |>
  sf::st_join(grid_germany)

indicators_summary_hex <- indicators_summary_sf |>
  sf::st_drop_geometry() |>
  group_by(hex_id) |>
  summarise(
    indicator_17 = mean(indicator_17),
    indicator_21 = mean(indicator_21),
    indicator_22 = mean(indicator_22),
    indicator_31 = psych::circadian.mean(indicator_31 * 2) / 2,
    indicator_32 = psych::circadian.mean(indicator_32 * 2) / 2,
    n_obs = n()
  ) |>
  mutate(duration_recharge = if_else(
    indicator_32 < indicator_31,
    indicator_32 + 12 - indicator_31,
    indicator_32 - indicator_31
  )) |>
  mutate(duration_discharge = 12 - duration_recharge)

data_plot <- grid_germany |>
  left_join(indicators_summary_hex, by = join_by(hex_id))
  # mutate(n_obs = replace_na(n_obs, 0))

hull <- data_plot |>
  sf::st_union()




# Plot -------------------------------------------------------------------

make_plot_annual_high_low_month <- function(data, variable_plot, title, palette, sigma) {
  variable_plot <- enquo(variable_plot)
  data_plot |>
    ggplot() +
    ggfx::with_shadow(
      geom_sf(data = hull, colour = NA, fill = "white"
      ),
      sigma = sigma, colour = "grey15", x_offset = 5, y_offset = 5
    ) +
    geom_sf(
      aes(fill = !!variable_plot),
      colour = "white",
      show.legend = FALSE,
      lwd = 0
    ) +
    # paletteer::scale_fill_paletteer_c("pals::kovesi.cyclic_mygbm_30_95_c78_s25", na_val) +
    scico::scale_fill_scico(
      # limits = c(year_start, year_end),
      palette = palette,
      direction = -1,
      na.value = "grey90"
      # palette = "batlowK"
    ) +
    labs(title = title) +
    coord_sf(datum = NA) +
    theme(
      plot.title = element_markdown(family = font_subtitle, size = size_title * 0.3, face = "plain", hjust = 0.5, colour = "grey", lineheight = 0.4),
      # plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title_light, margin = margin(60, 0, 40, 0)),
      plot.subtitle = element_markdown(family = font_subtitle, size = size_subtitle, hjust = 0, face = "plain", colour = colour_subtitle_light, lineheight = lineheight_subtitle, margin = margin(0, 0, 80, 0)),
      plot.caption = element_text(family = font_subtitle, colour = colour_subtitle_light, size = 40, hjust = 1, vjust = -20),
      axis.title = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      # panel.grid = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text = element_blank(),
      strip.background = element_blank(),
      strip.text = element_blank(),
      axis.ticks = element_line(colour = colour_panel_light),
      plot.margin = margin(0, 0, 0, 0, "cm"),
      panel.spacing = unit(panel_spacing, "mm")
    )
}



plot_gwl_high <- data_plot |>
  make_plot_annual_high_low_month(
    indicator_32,
    stringr::str_glue(
      "When does the <b><span style='color:{colour_recharge};'>re</span></b>charge period end?<br>
(annual maximum of groundwater level)"
    ),
    # "Month of highest groundwater level",
    "romaO",
    shadow_sigma
  )

plot_gwl_high

plot_gwl_low <- data_plot |>
  make_plot_annual_high_low_month(
    indicator_31,
    stringr::str_glue(
      "When does the <b><span style='color:{colour_discharge};'>dis</span></b>charge period end?<br>
(annual minimum of groundwater level)"
    ),
    # "Month of lowest groundwater level",
    "romaO",
    shadow_sigma
  )





make_plot_re_dis_charge <- function(data, variable_plot, title, palette, sigma) {
  variable_plot <- enquo(variable_plot)
  data |>
    ggplot() +
    ggfx::with_shadow(
      geom_sf(data = hull, colour = NA, fill = "white"
                ),
      sigma = sigma, colour = "grey15", x_offset = 5, y_offset = 5
    ) +
    # ggfx::with_shadow(
    geom_sf(
      aes(fill = !!variable_plot),
      colour = "white",
      show.legend = TRUE,
      lwd = 0
    # ), sigma = 9, colour = "grey15", x_offset = 5, y_offset = 5
  ) +
    # paletteer::scale_fill_paletteer_c("pals::kovesi.cyclic_mygbm_30_95_c78_s25", na_val) +
    scico::scale_fill_scico(
      limits = c(0, 10),
      breaks = 0:10,
      palette = palette,
      direction = -1,
      na.value = "grey90",
      # labels = scales::label_number(accuracy = 3),
      oob = scales::squish
      # palette = "batlowK"
    ) +
    guides(
      fill = guide_colorsteps(
        title = NA,
        title.position = "top",
        title.hjust = 0.5,
        reverse = TRUE,
        # title.theme = element_text(family = font_title, size = 20, face = "bold", colour = "grey40"),
        barwidth = unit(3, "mm"),
        barheight = unit(200, "mm")
      )
    ) +
    labs(title = title) +
    coord_sf(datum = NA) +
    theme(
      plot.title = element_markdown(family = font_subtitle, size = size_title * 0.3, face = "plain", hjust = 1, colour = "grey"),
      # plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title_light, margin = margin(60, 0, 40, 0)),
      plot.subtitle = element_markdown(family = font_subtitle, size = size_subtitle, hjust = 0, face = "plain", colour = colour_subtitle_light, lineheight = lineheight_subtitle, margin = margin(0, 0, 80, 0)),
      plot.caption = element_text(family = font_subtitle, colour = colour_subtitle_light, size = 40, hjust = 1, vjust = -20),
      axis.title = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      # panel.grid = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text = element_blank(),
      strip.background = element_blank(),
      strip.text = element_blank(),
      axis.ticks = element_line(colour = colour_panel_light),
      plot.margin = margin(0, 0, 0, 0, "cm"),
      panel.spacing = unit(panel_spacing, "mm"),
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(family = font_subtitle, size = size_title * 0.2, face = "bold", colour = "grey", margin = margin(0, 0, 0, 3.5, "mm"))
    )
}

plot_gwl_discharge <- data_plot |>
  make_plot_re_dis_charge(
    duration_discharge,
    stringr::str_glue("How long is the annual <b><span style='color:{colour_discharge};'>dis</span></b>charge period in months?"),
    "lapaz",
    shadow_sigma
  ) +
  theme(
  )

plot_gwl_recharge <- data_plot |>
  make_plot_re_dis_charge(
    duration_recharge,
    stringr::str_glue("How long is the annual <b><span style='color:{colour_recharge};'>re</span></b>charge period in months?"),
    "lapaz",
    shadow_sigma
  )


make_plot_distribution <- function(data, variable_plot, variable_plot_second, title = NA, palette, breaks = 0:12, labels = breaks) {
  variable_plot <- enquo(variable_plot)
  variable_plot_second <- enquo(variable_plot_second)
  data |>
    drop_na(!!variable_plot) |>
    mutate(test = "sddds") |>
    ggplot() +
    # geom_histogram(binwidth = 0.5, boundary = 0) +
    # geom_density(aes(duration_discharge, fill = stat(x))) +
    ggridges::geom_density_ridges_gradient(
      aes(x = !!variable_plot_second, y = test),
      fill = "grey90",
      colour = NA,
      show.legend = FALSE
    ) +
    ggridges::geom_density_ridges_gradient(
      aes(x = !!variable_plot, y = test, fill = stat(x)),
      colour = NA,
      show.legend = FALSE
    ) +
    scico::scale_fill_scico(
      limits = c(0, 10),
      breaks = 0:10,
      palette = palette,
      direction = -1,
      na.value = "grey90",
      # labels = scales::label_number(accuracy = 3),
      oob = scales::squish
      # palette = "batlowK"
    ) +
    labs(title = title) +
    scale_y_discrete(expand = c(0, 0.01)) +
    scale_x_continuous(breaks = breaks, labels = labels) +
    # coord_polar() +
    theme(
      plot.title = element_markdown(family = font_subtitle, size = size_title * 0.16, face = "plain", hjust = 0.5, colour = "grey"),
      # plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title_light, margin = margin(60, 0, 40, 0)),
      plot.subtitle = element_markdown(family = font_subtitle, size = size_subtitle, hjust = 0, face = "plain", colour = colour_subtitle_light, lineheight = lineheight_subtitle, margin = margin(0, 0, 80, 0)),
      plot.caption = element_text(family = font_subtitle, colour = colour_subtitle_light, size = 40, hjust = 1, vjust = -20),
      axis.title = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      # panel.grid = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.y = element_blank(),
      strip.background = element_blank(),
      strip.text = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(colour = "grey", size = 1),
      axis.line.x = element_line(colour = "grey", size = .5),
      plot.margin = margin(0, 0, 0, 0, "cm"),
      panel.spacing = unit(panel_spacing, "mm"),
      legend.position = "right",
      legend.title = element_blank(),
      axis.text.x = element_text(family = font_subtitle, size = size_title * 0.1, face = "bold", colour = "grey", margin = margin(3, 0, 0, 10, "mm"))
    )
}

plot_distribution_discharge <- data_plot |>
  make_plot_distribution(
    duration_discharge,
    duration_recharge,
    # stringr::str_glue("Distribution across all hexagons"),
    palette = "lapaz"
  )

plot_distribution_recharge <- data_plot |>
  make_plot_distribution(
    duration_recharge,
    duration_discharge,
    # stringr::str_glue("Distribution across all hexagons"),
    palette = "lapaz"
  )

make_plot_distribution_circular <- function(data, variable_plot, title = NA, palette, breaks = 0:12, labels = breaks) {
  variable_plot <- enquo(variable_plot)
  data |>
    mutate(test = "sddds") |>
    ggplot() +
    # geom_histogram(binwidth = 0.5, boundary = 0) +
    # geom_density(aes(duration_discharge, fill = stat(x))) +
    ggridges::geom_ridgeline_gradient(
      aes(x = x, y = test, height = y, fill = stat(x)),
      colour = NA,
      show.legend = FALSE
    ) +
    scico::scale_fill_scico(
      limits = c(0, 10),
      breaks = 0:10,
      palette = palette,
      direction = -1,
      na.value = "grey90",
      # labels = scales::label_number(accuracy = 3),
      oob = scales::squish
      # palette = "batlowK"
    ) +
    labs(title = title) +
    scale_y_discrete(expand = c(0, 0.01)) +
    scale_x_continuous(breaks = breaks, labels = labels) +
    # coord_polar() +
    theme(
      plot.title = element_markdown(family = font_subtitle, size = size_title * 0.16, face = "plain", hjust = 0.5, colour = "grey"),
      # plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title_light, margin = margin(60, 0, 40, 0)),
      plot.subtitle = element_markdown(family = font_subtitle, size = size_subtitle, hjust = 0, face = "plain", colour = colour_subtitle_light, lineheight = lineheight_subtitle, margin = margin(0, 0, 80, 0)),
      plot.caption = element_text(family = font_subtitle, colour = colour_subtitle_light, size = 40, hjust = 1, vjust = -20),
      axis.title = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      # panel.grid = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.y = element_blank(),
      strip.background = element_blank(),
      strip.text = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(colour = "grey", size = 1),
      axis.line.x = element_line(colour = "grey", size = .5),
      plot.margin = margin(0, 0, 0, 0, "cm"),
      panel.spacing = unit(panel_spacing, "mm"),
      legend.position = "right",
      legend.title = element_blank(),
      axis.text.x = element_text(family = font_subtitle, size = size_title * 0.1, face = "bold", colour = "grey", margin = margin(3, 0, 0, 10, "mm"))
    )
}

plot_distribution_highmonth <-
  data_plot |>
  sf::st_drop_geometry() |>
  drop_na(indicator_32) |>
  as_tibble() |>
  pull(indicator_32) |>
  circular::as.circular(unit = "hours") |>
  list() |>
  map(~ {
    d <- density(.x, bw = circular::bw.nrd.circular(.x))
    tibble(x = unclass(d$x), y = d$y)
  }) |>
  chuck(1) |> # pull(x) |> range()
  mutate(x = x * 2) |> # pull(x) |> range()
  mutate(x = rep(0:11, each = 43)[-(44:47)]) |>
  group_by(x) |>
  mutate(x = x + 1 / n() * row_number()) |>
  ungroup() |>
  make_plot_distribution_circular(
    indicator_32,
    palette = "romaO",
    breaks = 0:11,
    labels = month.abb |> stringr::str_sub(end = 1)
  )

plot_distribution_lowmonth <- data_plot |>
  sf::st_drop_geometry() |>
  drop_na(indicator_31) |>
  as_tibble() |>
  pull(indicator_31) |>
  circular::as.circular(unit = "hours") |>
  list() |>
  map(~ {
    d <- density(.x, bw = circular::bw.nrd.circular(.x))
    tibble(x = unclass(d$x), y = d$y)
  }) |>
  chuck(1) |> # pull(x) |> range()
  mutate(x = x * 2) |> #ggplot(aes(x, y)) + geom_line()
  mutate(x = rep(0:11, each = 43)[-(44:47)]) |>
  group_by(x) |>
  mutate(x = x + 1 / n() * row_number()) |>
  ungroup() |>
  make_plot_distribution_circular(
    indicator_31,
    palette = "romaO",
    breaks = 0:11,
    labels = month.abb |> stringr::str_sub(end = 1)
  )


make_plot_distribution_hist <- function(data, variable_plot, title = NA, palette, labels = NA) {
  variable_plot <- enquo(variable_plot)
  data |>
    ggplot() +
    # geom_histogram(binwidth = 0.5, boundary = 0) +
    # geom_density(aes(duration_discharge, fill = stat(x))) +
    geom_col(aes(!!variable_plot, n, fill = !!variable_plot), show.legend = FALSE) +
    # scale_x_binned() +
    scico::scale_fill_scico_d(
      palette = "romaO",
      direction = -1,
      na.value = "grey90",
    ) +
    scale_x_discrete(labels = labels) +
    scico::scale_fill_scico_d(
      palette = palette,
      direction = -1,
      na.value = "grey90"
    ) +
    labs(title = title) +
    scale_y_discrete(expand = c(0, 0.01)) +
    theme(
      plot.title = element_markdown(family = font_subtitle, size = size_title * 0.16, face = "plain", hjust = 0.5, colour = "grey"),
      # plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title_light, margin = margin(60, 0, 40, 0)),
      plot.subtitle = element_markdown(family = font_subtitle, size = size_subtitle, hjust = 0, face = "plain", colour = colour_subtitle_light, lineheight = lineheight_subtitle, margin = margin(0, 0, 80, 0)),
      plot.caption = element_text(family = font_subtitle, colour = colour_subtitle_light, size = 40, hjust = 1, vjust = -20),
      axis.title = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      # panel.grid = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.y = element_blank(),
      strip.background = element_blank(),
      strip.text = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(colour = "grey", size = 1),
      axis.line.x = element_line(colour = "grey", size = .5),
      plot.margin = margin(0, 0, 0, 0, "cm"),
      panel.spacing = unit(panel_spacing, "mm"),
      legend.position = "right",
      legend.title = element_blank(),
      axis.text.x = element_text(family = font_subtitle, size = size_title * 0.1, face = "bold", colour = "grey", margin = margin(3, 0, 0, 10, "mm"))
    )
}


levels_month = c(
  "[1,2]",
  "(2,3]",
  "(3,4]",
  "(4,5]",
  "(5,6]",
  "(6,7]",
  "(7,8]",
  "(8,9]",
  "(9,10]",
  "(10,11]",
  "(11,12]",
  "(12,13]"
)

plot_distribution_lowmonth <-
  data_plot |>
  as_tibble() |>
  drop_na(indicator_31) |>
  mutate(indicator_31 = cut_width(indicator_31 + 1, 1, boundary = 0)) |>
  group_by(indicator_31) |>
  count() |>
  right_join(tibble(indicator_31 = levels_month)) |>
  mutate(indicator_31 = factor(indicator_31, levels = levels_month)) |>
  make_plot_distribution_hist(
    indicator_31,
    palette = "romaO",
    labels = month.abb |> stringr::str_sub(end = 1)
  )

plot_distribution_highmonth <-
  data_plot |>
  as_tibble() |>
  drop_na(indicator_32) |>
  mutate(indicator_32 = cut_width(indicator_32 + 1, 1, boundary = 0)) |>
  group_by(indicator_32) |>
  count() |>
  right_join(tibble(indicator_32 = levels_month)) |>
  mutate(indicator_32 = factor(indicator_32, levels = levels_month)) |>
  make_plot_distribution_hist(
    indicator_32,
    palette = "romaO",
    labels = month.abb |> stringr::str_sub(end = 1)
  )



# Legend ------------------------------------------------------------------


plot_legend <-
  tibble(
    y = rep(1, 12 * 100),
    City = as.character(rep(1:100, each = 12)),
    month = factor(rep(month.abb, 100), month.abb)
  ) |>
  arrange(month) |>
  mutate(Temperature = factor(1:1200, 1:1200)) |>
  ggplot(aes(month, y)) +
  geom_point(aes(colour = Temperature),
    position = position_dodge(width = 1), show.legend = FALSE, size = 8
  ) +
  geom_vline(xintercept = 1:13 - 0.5, color = "gray90", lwd = .3) +
  geom_hline(yintercept = 0:1 * 1.5, color = "white") +
  geom_point(aes(colour = Temperature),
    position = position_dodge(width = 1),
    show.legend = FALSE,
    size = 5
  ) +
  # scale_fill_manual(values = c("darkorange", "dodgerblue4")) +
  scico::scale_colour_scico_d(
    # limits = c(year_start, year_end),
    palette = "romaO",
    direction = -1
    # palette = "batlowK"
  ) +
  geomtextpath::coord_curvedpolar() +
  theme_void() +
  theme(
    axis.text.x = element_text(family = font_subtitle, size = size_title * 0.1, face = "bold", colour = colour_subtitle_light),
    plot.title = element_text(family = font_title, size = 20, hjust = 0.5, colour = "grey", margin = margin(10, 0, 30, 0)),
    plot.subtitle = element_text(family = font_subtitle, size = 15, hjust = 0.5, face = "plain", colour = colour_subtitle_light, lineheight = lineheight_subtitle, margin = margin(0, 0, 50, 0)),
    panel.background = element_rect(fill = colour_panel_light, colour = NA),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )


# Metaplot ----------------------------------------------------------------

scale_breaks <- c(1, seq(10, 50, 10))
# scale_breaks <- c(1, seq(50, 150, 10))

plot_meta <- data_plot |>
  ggplot() +
  geom_sf(aes(fill = n_obs), colour = NA) +
  geom_sf(data = data_plot |> filter(n_obs == 0),fill = "white", colour = NA) +
  geom_sf(data = data_plot |> filter(n_obs == 1) |> sf::st_centroid(), colour = "white", size = .5) +
  geom_sf(data = hull, fill = NA, colour = "grey90", linewidth = .8) +
  theme_void() +
  # scico::scale_fill_scico(palette = "grayC", na.value = "yellow")
  # scale_fill_binned(palette = greys) +
  scale_fill_gradient(
    breaks = scale_breaks,
    labels = scale_breaks |> map_at(length(scale_breaks), ~ paste0("> ", .x)) |> reduce(c),
    limits = c(1, 50),
    low = "grey90",
    high = "grey40",
    na.value = "white",
oob = scales::squish
) +
  guides(
    fill = guide_colorsteps(
      title = NA,
      title.position = "top",
      title.hjust = 0.5,
      reverse = TRUE,
      # title.theme = element_text(family = font_title, size = 20, face = "bold", colour = "grey40"),
      barwidth = unit(3, "mm"),
      barheight = unit(100, "mm")
    )
  ) +
  labs(title = "\\# of wells per hexagon") +
  theme(
    plot.title = element_markdown(family = font_subtitle, size = size_title * 0.2, face = "plain", hjust = 0.5, colour = "grey"),
    # plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title_light, margin = margin(60, 0, 40, 0)),
    plot.subtitle = element_markdown(family = font_subtitle, size = size_subtitle, hjust = 0, face = "plain", colour = colour_subtitle_light, lineheight = lineheight_subtitle, margin = margin(0, 0, 80, 0)),
    plot.caption = element_text(family = font_subtitle, colour = colour_subtitle_light, size = 40, hjust = 1, vjust = -20),
    axis.title = element_blank(),
    # panel.background = element_rect(fill = "white"),
    # plot.background = element_rect(fill = colour_panel_light),
    # panel.grid = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.spacing = unit(panel_spacing, "mm"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text.align = 0,
    legend.text = element_text(family = font_subtitle, size = size_title * 0.1, face = "bold", colour = "grey", margin = margin(0, 0, 0, 1.5, "mm"))
  )
plot_meta


# Patchwork ---------------------------------------------------------------


layout_grid_firstrow <- c(
  area(t = 1, l = 1, b = 12, r = 10),
  area(t = 10, l = 10, b = 11, r = 12),
  area(t = 3, l = 11, b = 10, r = 19),
  area(t = 1, l = 20, b = 12, r = 30),
  area(t = 10, l = 18, b = 11, r = 20)
)


plot_firstrow <- (plot_gwl_high + plot_distribution_highmonth + plot_legend + plot_gwl_low + plot_distribution_lowmonth) +
  plot_layout(design = layout_grid_firstrow)
# layout_grid_secondandthirdrow <- c(
#   area(t = 1, l = 1, b = 6, r = 3),
#   area(t = 1, l = 4, b = 6, r = 6),
#   area(t = 5, l = 8, b = 5, r = 8),
#   area(t = 2, l = 9, b = 2, r = 9)
# )
layout_grid_secondandthirdrow <- c(
  area(t = 1, l = 1, b = 12, r = 6),
  area(t = 1, l = 8, b = 12, r = 12),
  area(t = 10, l = 6, b = 11, r = 7),
  area(t = 1, l = 18, b = 12, r = 18),
  # area(t = 1, l = 16, b = 12, r = 17)
  area(t = 1, l = 14, b = 12, r = 17),


  area(t = 13, l = 1, b = 24, r = 6),
  area(t = 13, l = 8, b = 24, r = 12),
  area(t = 22, l = 6, b = 23, r = 7),
  area(t = 13, l = 18, b = 24, r = 18),
  # area(t = 1, l = 16, b = 12, r = 17)
  area(t = 16, l = 14, b = 24, r = 17)
)

# plot_secondrow <- plot_spacer() +
#   plot_gwl_recharge +
#   plot_distribution_recharge +
#   plot_spacer() +
#   plot_spacer() +
#   plot_layout(design = layout_grid_secondandthirdrow)
#
# plot_thirdrow <- plot_spacer() +
#   plot_gwl_discharge +
#   plot_distribution_discharge +
#   plot_spacer() +
#   # plot_spacer() +
#   plot_meta +
#   plot_layout(design = layout_grid_secondandthirdrow)


plot_secondandthirdrow <-
  plot_spacer() +
  plot_gwl_recharge +
  plot_distribution_recharge +
  plot_spacer() +
  plot_spacer() +
  plot_spacer() +
  plot_gwl_discharge +
  plot_distribution_discharge +
  plot_spacer() +
  # plot_spacer() +
  plot_meta +
  plot_layout(
    guides = "collect",
    design = layout_grid_secondandthirdrow
    )

plot_gwl_re_discharge_legend <- plot_gwl_discharge |>
  ggpubr::get_legend() |>
  ggpubr::as_ggplot()

plot_gwl_discharge <- plot_gwl_discharge +
  theme(legend.position = "none")

plot_gwl_recharge <- plot_gwl_recharge +
  theme(
    legend.position = "none"
    # plot.margin = margin(0, 0, 0, 30, "mm")
  )

plot_meta_legend <- plot_meta |>
  ggpubr::get_legend() |>
  ggpubr::as_ggplot() +
  theme(
    plot.margin = margin(0, 0, 0, 12, "mm")
  )

plot_meta <- plot_meta +
  theme(legend.position = "none")

# 30 * 36
test <- c(
  area(t = 1, l = 1, b = 12, r = 10),
  area(t = 10, l = 10, b = 11, r = 12),
  area(t = 3, l = 11, b = 10, r = 20),
  area(t = 1, l = 21, b = 12, r = 30),
  area(t = 10, l = 19, b = 11, r = 21),

  area(t = 13, l = 1, b = 24, r = 10),
  area(t = 13, l = 11, b = 24, r = 20),
  area(t = 22, l = 10, b = 23, r = 12),

  area(t = 25, l = 1, b = 36, r = 10),
  area(t = 25, l = 11, b = 36, r = 20),
  area(t = 34, l = 10, b = 35, r = 12),
  area(t = 13, l = 22, b = 36, r = 22),
  area(t = 28, l = 23, b = 36, r = 28),
  area(t = 29, l = 29, b = 36, r = 29)
)
plot(test)



plot_test <-
  plot_gwl_high +
  plot_distribution_highmonth +
  plot_legend +
  plot_gwl_low +
  plot_distribution_lowmonth +

  plot_spacer() +
  plot_gwl_recharge +
  plot_distribution_recharge +

  plot_spacer() +
  plot_gwl_discharge +
  plot_distribution_discharge +
  plot_gwl_re_discharge_legend +
  plot_meta +
  plot_meta_legend +
  plot_layout(
    guides = "collect",
    design = layout_grid_secondandthirdrow
  )



plot_combined <- plot_test +
  plot_layout(design = test) +
  plot_annotation(
    title = "Groundwater in Germany",
    subtitle = stringr::str_glue(
      "Annual <b><span style='color:{colour_recharge};'>re</span></b>charge and <b><span style='color:{colour_discharge};'>dis</span></b>charge
periods calculated as averages per hexagon for the reference period from <b>{year_start}</b> and <b>{year_end}</b>. <br>
The data is based on timeseries from in total {indicators_summary_sf |> nrow()} observation wells distributed heterogeneously across Germany."
    ),
    caption = caption,
    theme = theme(
      plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title_light, margin = margin(30, 0, 10, 0, "mm")),
      plot.subtitle = element_markdown(family = font_subtitle, size = size_title * 0.3, hjust = 0.5, face = "plain", colour = colour_subtitle_light, lineheight = lineheight_subtitle, margin = margin(0, 0, 40, 0, "mm")),
      plot.caption = element_text(family = font_subtitle, colour = colour_subtitle_light, size = 40, hjust = 0.99, vjust = -0.93, margin = margin(0, 0, 7, 0, "mm")),
      legend.text = element_text(margin = margin(0, 0, 0, 3, "mm")),
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank()
    )
  )

plot_combined |>
  ggsave(filename = "groundwarter_summary_per_hexagon_kartesian_light_a1_correctivdata.png", width = 59.4, height = 84.1, units = "cm")

# showtext::showtext_opts(dpi = 300)
# plot_combined |>
#   ggsave(filename = "groundwarter_summary_per_hexagon_kartesian_light_a1.pdf",
#          width = 59.4, height = 84.1, units = "cm",
#          device = "pdf",
#          dpi = 300)
# showtext::showtext_opts(dpi = 96)
#
#
# plot_combined |> ggview::ggview(width = 59.4, height = 84.1, units = "cm")
