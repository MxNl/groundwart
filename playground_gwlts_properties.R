# Setup -------------------------------------------------------------------



library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(scico)
library(geofacet)
library(facetwarp)
library(ggplot2)
library(ggblend)
library(ggfx)
library(showtext)
library(rnaturalearth)
library(ggtext)
library(klibiwinds)
library(tsibble)
library(feasts)
library(sf)
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
# colour_referencecomparison <- c("#FF3D7FFF", "#3FB8AFFF")
colour_referencecomparison <- c("#07F985", "#FF00FA")
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
# subtitle_polar <- stringr::str_glue(
#   "Time series of measured groundwater levels across Germany in polar coordinates.<br>
# The time series cover the period between <b><span style='color:{buda_low};'>{year_start}</span></b> and <b><span style='color:{buda_high};'>{year_end}</span></b> as much as available.<br>
# Here, the groundwater levels are shown as normalized instead of absolute values<br>
# in order to visually compare between different regions."
# )
subtitle_meancomparison_polar <- stringr::str_glue(
  "This map shows selected characteristics (e.g. spikeness, trend) of groundwater level time series in Germany<br>
  as a modified radar/\U+1F577 chart. These characteristics are calculated for the period from {year_start} to {year_end}.<br>
The colours indicate wether the values of the respective characteristic lies <b><span style='color:{colour_referencecomparison[1]};'>below</span></b> or <b><span style='color:{colour_referencecomparison[2]};'>above</span></b> the mean value calculated for all wells.<br>"
)
# subtitle_cartesian <- stringr::str_glue(
#   "Time series of measured groundwater levels across Germany.<br>
# The time series cover the period between <b><span style='color:{buda_low};'>{year_start}</span></b> and <b><span style='color:{buda_high};'>{year_end}</span></b> as much as available.<br>
# Here, the groundwater levels are shown as normalized instead of absolute values<br>
# in order to visually compare between different regions."
# )
# subtitle_joydivision <- stringr::str_glue(
#   "Time series of measured groundwater levels across Germany.<br>
# The time series cover the period between {buda_low} and {buda_high} as much as available.<br>
# Here, the groundwater levels are shown as normalized instead of absolute values<br>
# in order to visually compare between different regions."
# )
caption <- "By Max Nölscher   |   Data from German Geological Surveys and Authorities   |   Made in R"
# key_well_meta <- read_rds("key_well_meta.rds")


# Data --------------------------------------------------------------------

## GWL Correctiv Data ------------------------------------------------------

caption <- "By Max Nölscher   |   Data from CORRECTIV.Lokal   |   Made in R"
year_start <- 1991
year_end <- 2020
CELLSIZE <- 50E3

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
  map_df(readr::read_csv, .progress = TRUE) |>
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
  add_indicator_2_3(data_gwl_prep |> rename(gwl = min_gwl)) |>
  add_indicator_2_4(data_gwl_prep |> rename(gwl = max_gwl))


indicators_tsfeatures <- data_gwl_prep |>
  # group_split() |>
  # magrittr::extract(1:3) |>
  # reduce(bind_rows) |>
  mutate(yearmonth = yearmonth(date)) |>
  select(well_id, yearmonth, mean_gwl) |>
  as_tsibble(index = yearmonth, key = well_id) |>
  features(
    mean_gwl,
    features = feature_set(tags = c(
      "count",
      "decomposition",
      "lumpiness",
      "seasonal"
    ))
  )

indicators_summary <- indicators_summary |>
  left_join(indicators_tsfeatures, by = join_by(well_id)) |>
  select(well_id, contains("indicator_"), trend_strength, spikiness, linearity, longest_flat_spot)

indicators_summary_sf <-
  key_well_meta_sf |>
  inner_join(indicators_summary, by = join_by(well_id))

grid_germany <- sf::st_make_grid(indicators_summary_sf, cellsize = CELLSIZE, square = TRUE) %>%
  sf::st_sf() %>%
  filter(sf::st_intersects(., admin_borders_germany, sparse = FALSE)[, 1]) |>
  mutate(cell_id = row_number())
  # sf::st_join(
  #   key_well_meta_sf |> sf::st_drop_geometry()
  # ) |>
  # arrange(cell_id)
  # sf::st_transform(25832)

geofacet_grid_germany <- grid_germany |>
  # sf::st_transform(25832) |>
  mutate(
    x_coord = grid_germany |> sf::st_centroid() |> sf::st_coordinates() |> as_tibble() |> pull(X),
    y_coord = grid_germany |> sf::st_centroid() |> sf::st_coordinates() |> as_tibble() |> pull(Y)
  ) |>
  mutate(
    row = cut_width(-y_coord, CELLSIZE, labels = FALSE),
    col = cut_width(x_coord, CELLSIZE, labels = FALSE)
  ) |>
  select(row, col, name = cell_id, code = cell_id) |>
  mutate(name = paste0("tile_", code)) |>
  sf::st_drop_geometry() |>
  as_tibble() |>
  tidyr::drop_na(code) |>
  arrange(row, col)

indicators_summary_sf <- indicators_summary_sf |>
  sf::st_join(grid_germany)

indicators_summary_hex <- indicators_summary_sf |>
  sf::st_drop_geometry() |>
  group_by(cell_id) |>
  summarise(
    across(c(contains("indicator_"), trend_strength, spikiness, linearity, longest_flat_spot), mean, na.rm = TRUE),
    n_obs = n()
  )

data_plot <- grid_germany |>
  left_join(indicators_summary_hex, by = join_by(cell_id)) |>
  rename(code = cell_id) |>
  mutate(
    x_center = grid_germany |> st_centroid() |> st_coordinates() |> as_tibble() |> pull(X),
    y_center = grid_germany |> st_centroid() |> st_coordinates() |> as_tibble() |> pull(Y)
    )
  # sf::st_drop_geometry() |>
  # as_tibble()

hull <- data_plot |>
  sf::st_union()


geofacet_grid_germany |>
  grid_preview()

data_plot <- data_plot |>
  st_drop_geometry()

data_plot <- data_plot |>
  filter(code %in% 1:120)

geofacet_grid_germany <- geofacet_grid_germany |>
  filter(code %in% 1:120)

# Plot --------------------------------------------------------------------
## Geofacet ----------------------------------------------------------------
### radar ----------------------------------------------------------------

coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction),
          is_linear = function(coord) TRUE) }

cp <- coord_radar(start = 0)
cp$is_free <- function() TRUE


#### dark --------------------------------------------------------------------

features <- c(
  "indicator_17",
  "dummy_1",
    "indicator_23",
  "dummy_2",
    "indicator_24",
  "dummy_3",
    "trend_strength",
  "dummy_4",
    "spikiness",
  "dummy_5",
    "linearity",
  "dummy_6",
    "longest_flat_spot",
  "dummy_7"
)

# dev.off()
# dev.off()

data_plot <- data_plot |>
  as_tibble() |>
  mutate(
    dummy_1 = 0,
    dummy_2 = 0,
    dummy_3 = 0,
    dummy_4 = 0,
    dummy_5 = 0,
    dummy_6 = 0,
    dummy_7 = 0
  ) |>
  left_join(geofacet_grid_germany, by = join_by(code)) |>
  select(-n_obs, -name) |>
  mutate(zz_dummy = indicator_17) |>
  # mutate(gr_id = code) |>
  # ggradar::ggradar(mapping = aes(facet = code))
  pivot_longer(cols = all_of(c(features, "zz_dummy"))) |>
  mutate(
    value_min = min(value, na.rm = TRUE),
    value_max = max(value, na.rm = TRUE),
    .by = name
  ) |>
  mutate(value = scales::rescale(value), .by = name) |>
  mutate(value = if_else(stringr::str_detect(name, "dummy_"), 0, value)) |>
  mutate(name = factor(name, levels = c(features, "zz_dummy"))) |>
  group_by(code) |>
  arrange(name) |>
  mutate(name_num = row_number()) |>
  ungroup() |>
  arrange(code)
  # mutate(name = factor(name, levels = c(
  #   "indicator_17",
  #   "indicator_23",
  #   "indicator_24",
  #   "trend_strength",
  #   "spikiness",
  #   "linearity",
  #   "longest_flat_spot"
  # ))) |>

data_plot_mean <- data_plot |>
  mutate(value_mean = mean(value, na.rm = TRUE), .by = name)

code_ids <- data_plot$code |> unique()
code_ids_length <- code_ids |> length()

p <- data_plot |>
  ggplot() +
  # geom_rect(
  #   data = tibble(code = code_ids, xmin = 1, xmax = 15, ymin = -0.3, ymax = 0),
  #   aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "grey15", colour = NA
  # ) +
  geom_area(aes(name_num, value), colour = NA, fill = "grey20") +
  # geom_line(data = data_plot_mean, aes(name_num, value_mean), colour = "grey30", linewidth = 0.1) +
  # geom_line(colour = "grey30", alpha = .5) +
  ggh4x::stat_difference(data = data_plot_mean, aes(name_num, ymin = value_mean, ymax = value), show.legend = FALSE) +
  # geom_line(aes(name_num, value), colour = "white", linewidth = 0.3) +
  # geom_line(colour = buda_high[[1]]) +
  ylim(-0.3, 1) +
  # geom_point(data = data_plot |> filter(!stringr::str_detect(name, "dummy_")), colour = buda_high[[1]]) +
  # geom_point(colour = "black", shape = 21, fill = NA) +
  # ggradar::ggradar(fill = TRUE, group.line.width = 1, group.point.size = 0) +
  # facet_wrap(~ code)
  # cp
  # scale_x_continuous(breaks = 1:8) +
  # facetwarp::facet_warp(vars(code), macro_x = "x_center", macro_y = "y_center") +
  facet_geo(~code, grid = geofacet_grid_germany, scales = "free") +
  scale_fill_manual(values = c("#07F985", "#FF00FA")) +
  # coord_radar(start = 0) +
  cp +
  scale_x_continuous(
    breaks = seq(1, 14, 2)
    # labels = features |> discard(.p = ~ .x |> stringr::str_detect("dummy_"))
    ) +
  labs(
    title = "Groundwater in Germany",
    subtitle = subtitle_meancomparison_polar,
    caption = caption
  ) +
  theme(
    plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title, margin = margin(30, 0, 20, 0)),
    plot.subtitle = element_markdown(family = font_subtitle, size = size_subtitle, hjust = 0.5, face = "plain", colour = colour_subtitle, lineheight = lineheight_subtitle, margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(family = font_subtitle, colour = colour_subtitle, size = 40, hjust = 1, vjust = -20),
    axis.title = element_blank(),
    panel.background = element_rect(fill = colour_panel),
    plot.background = element_rect(fill = colour_panel),
    # panel.grid = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(colour = "grey15", linetype = "dashed"),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel),
    plot.margin = margin(2, 2, 3, 2, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  )

relabeller <- function(x) {
  case_when(
    x == "indicator_17" ~ "Annual amplitude",
    x == "indicator_23" ~ "No. of years with wet conditions",
    x == "indicator_24" ~ "No. of years with dry conditions",
    x == "trend_strength" ~ "Trend strength",
    x == "spikiness" ~ "Spikiness",
    x == "linearity" ~ "Linearity",
    x == "longest_flat_spot" ~ "Longest flat spot"
  )
}

# legend <-
  data_plot_mean |>
  filter(code == 1) |>
  ggplot() +
  # geom_rect(
  #   data = tibble(code = code_ids, xmin = 1, xmax = 15, ymin = -0.3, ymax = 0),
  #   aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "grey15", colour = NA
  # ) +
  geom_area(data = data_plot_mean |> filter(code == 1), aes(name_num, value_mean), colour = NA, fill = NA) +
  # geom_line(data = data_plot_mean, aes(name_num, value_mean), colour = "grey30", linewidth = 0.1) +
  # geom_line(colour = "grey30", alpha = .5) +
  # ggh4x::stat_difference(data = data_plot_mean, aes(name_num, ymin = value_mean, ymax = value), show.legend = FALSE) +
  # geom_line(aes(name_num, value), colour = "white", linewidth = 0.3) +
  # geom_line(colour = buda_high[[1]]) +
  ylim(-0.3, 1) +
  # geom_point(data = data_plot |> filter(!stringr::str_detect(name, "dummy_")), colour = buda_high[[1]]) +
  # geom_point(colour = "black", shape = 21, fill = NA) +
  # ggradar::ggradar(fill = TRUE, group.line.width = 1, group.point.size = 0) +
  # facet_wrap(~ code)
  # cp
  # scale_x_continuous(breaks = 1:8) +
  # facetwarp::facet_warp(vars(code), macro_x = "x_center", macro_y = "y_center") +
  scale_fill_manual(values = c("#07F985", "#FF00FA")) +
  # coord_radar(start = 0) +
  cp +
    geomtextpath::coord_curvedpolar() +
  scale_x_continuous(
    breaks = seq(1, 14, 2),
    labels = features |> discard(.p = ~ .x |> stringr::str_detect("dummy_")) |> relabeller()
  ) +
  # labs(
  #   title = "Groundwater in Germany",
  #   subtitle = subtitle_meancomparison_polar,
  #   caption = caption
  # ) +
  theme(
    plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title, margin = margin(30, 0, 20, 0)),
    plot.subtitle = element_markdown(family = font_subtitle, size = size_subtitle, hjust = 0.5, face = "plain", colour = colour_subtitle, lineheight = lineheight_subtitle, margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(family = font_subtitle, colour = colour_subtitle, size = 40, hjust = 1, vjust = -20),
    axis.title = element_blank(),
    panel.background = element_rect(fill = colour_panel),
    plot.background = element_rect(fill = colour_panel),
    # panel.grid = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(colour = "grey", linetype = "dashed"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = font_subtitle, size = font_subtitle, hjust = 0.5, vjust = -1, colour = colour_title, margin = margin(3, 3, 3, 3)),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  )

p |>
  ggsave(filename = "plots/test2.png", width = 59.4, height = 84.1, units = "cm")


  ### Column ------------------------------------------------------------------



features <- c(
  "indicator_17",
  "indicator_23",
  "indicator_24",
  "trend_strength",
  "spikiness",
  "linearity",
  "longest_flat_spot"
)

# dev.off()
# dev.off()

data_plot <- data_plot |>
  as_tibble() |>
  mutate_at(vars(-code), scales::rescale) |>
  left_join(geofacet_grid_germany, by = join_by(code)) |>
  select(-n_obs, -name) |>
  mutate(zz_dummy = indicator_17) |>
  # mutate(gr_id = code) |>
  # ggradar::ggradar(mapping = aes(facet = code))
  pivot_longer(cols = all_of(c(features, "zz_dummy"))) |>
  mutate(name = factor(name, levels = c(features, "zz_dummy"))) |>
  group_by(code) |>
  arrange(name) |>
  mutate(name_num = row_number()) |>
  ungroup() |>
  arrange(code)
# mutate(name = factor(name, levels = c(
#   "indicator_17",
#   "indicator_23",
#   "indicator_24",
#   "trend_strength",
#   "spikiness",
#   "linearity",
#   "longest_flat_spot"
# ))) |>



p <- data_plot |>
  ggplot(aes(name, value)) +
  geom_col(aes(fill = name), colour = NA) +
  ylim(-0.3, 1) +
  coord_polar(start = 0) +
  # facetwarp::facet_warp(vars(code), macro_x = "x_center", macro_y = "y_center") +
  facet_geo(~code, grid = geofacet_grid_germany) +
  # coord_radar(start = 0) +
  # scale_x_continuous(breaks = 1:7, labels = c(features)) +
  theme(
    plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title, margin = margin(30, 0, 20, 0)),
    plot.subtitle = element_markdown(family = font_subtitle, size = size_subtitle, hjust = 0.5, face = "plain", colour = colour_subtitle, lineheight = lineheight_subtitle, margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(family = font_subtitle, colour = colour_subtitle, size = 40, hjust = 1, vjust = -20),
    axis.title = element_blank(),
    panel.background = element_rect(fill = colour_panel),
    plot.background = element_rect(fill = colour_panel),
    # panel.grid = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(colour = "grey20"),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel),
    plot.margin = margin(2, 2, 3, 2, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  )

p |>
  ggsave(filename = "plots/test_col.png", width = 59.4, height = 84.1, units = "cm")



# radar trials -------------------------------------------------------------------

data_plot |>
  as_tibble() |>
  left_join(geofacet_grid_germany, by = join_by(code)) |>
  select(-n_obs, -name) |>
  mutate_at(vars(-code), scales::rescale) |>
  # mutate(gr_id = code) |>
  # ggradar::ggradar(mapping = aes(facet = code))
  # pivot_longer(cols = c("indicator_17",
  #                       "indicator_23",
  #                       "indicator_24",
  #                       "trend_strength",
  #                       "spikiness",
  #                       "linearity",
  #                       "longest_flat_spot")) |>
  # mutate(name = factor(name, levels = c(
  #   "indicator_17",
#   "indicator_23",
#   "indicator_24",
#   "trend_strength",
#   "spikiness",
#   "linearity",
#   "longest_flat_spot"
# ))) |>
# ggplot(aes(name, value)) +
# geom_area(fill = "black") +
ggradar::ggradar(fill = TRUE, group.line.width = 1, group.point.size = 0) +
  facet_wrap(~ code)
# cp +
facetwarp::facet_warp(vars(code), macro_x = "col", macro_y = "row")
# facet_geo(~code, grid = geofacet_grid_germany, scales = "free")
theme(
  plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title, margin = margin(30, 0, 20, 0)),
  plot.subtitle = element_markdown(family = font_subtitle, size = size_subtitle, hjust = 0.5, face = "plain", colour = colour_subtitle, lineheight = lineheight_subtitle, margin = margin(0, 0, 10, 0)),
  plot.caption = element_text(family = font_subtitle, colour = colour_subtitle, size = 40, hjust = 1, vjust = -20),
  axis.title = element_blank(),
  panel.background = element_rect(fill = colour_panel),
  plot.background = element_rect(fill = colour_panel),
  # panel.grid = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.x = element_line(colour = "grey20"),
  axis.text = element_blank(),
  strip.background = element_blank(),
  strip.text = element_blank(),
  axis.ticks = element_line(colour = colour_panel),
  plot.margin = margin(2, 2, 3, 2, "cm"),
  panel.spacing = unit(panel_spacing, "mm")
)
