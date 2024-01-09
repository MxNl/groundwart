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


make_gaps_explicit_and_impute_linear <- function(historic_observations) {
  historic_observations |>
    tsibble::as_tsibble(key = fk_well, index = date) |>
    tsibble::group_by_key() |>
    tsibble::fill_gaps() |>
    dplyr::as_tibble() |>
    dplyr::group_by(fk_well) |>
    dplyr::mutate(gwl = zoo::na.approx(gwl))
}
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
colour_discharge <- "#cebd67ff"
colour_recharge <- "#8ecdcfff"
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
caption <- "By Max Nölscher | Data from German Geological Surveys and Authorities | Made in R"

# Data --------------------------------------------------------------------

## GWL BGR data ------------------------------------------------------------


data_gwl <- read_rds("groundwart_germany_data.rds")
key_well_meta <- read_rds("key_well_meta.rds")
key_well_meta <- key_well_meta |>
  filter(fk_well %in% unique(data_gwl$fk_well))
grid_germany <- read_rds("groundwart_grid_germany_alt_subset.rds")

grid_germany <- grid_germany |>
  sf::st_join(
    key_well_meta
  ) |>
  arrange(cell_id) |>
  sf::st_transform(25832)


geofacet_grid_germany <- grid_germany |>
  mutate(
    x_coord = grid_germany |> sf::st_centroid() |> sf::st_coordinates() |> as_tibble() |> pull(X),
    y_coord = grid_germany |> sf::st_centroid() |> sf::st_coordinates() |> as_tibble() |> pull(Y)
  ) |>
  mutate(
    row = cut_width(-y_coord, 50000, labels = FALSE),
    col = cut_width(x_coord, 50000, labels = FALSE)
  ) |>
  select(row, col, name = well_id, code = fk_well) |>
  sf::st_drop_geometry() |>
  as_tibble() |>
  tidyr::drop_na(code) |>
  arrange(row, col)
# filter(
#   row == row |> median() |> ceiling() |
#   col == col |> median() |> ceiling()
# )
# slice_head(n = 10)

# grid_germany |>
# # arrange(x_coord) |>
#   ggplot() +
#   geom_sf() +
#   geom_sf_label(aes(label = paste(row, column, sep = ", ")))

grid_preview(geofacet_grid_germany)


#
# data_gwl |>
#   filter(fk_well == 1281) |> View()
#   mutate(
#     sd =  sd(gwl, na.rm = TRUE),
#     gwl_ub = mean(gwl) + (3 * sd),
#     gwl_lb = mean(gwl) - (3 * sd)
#     ) |>
#   mutate(gwl = if_else(gwl |> between(gwl_lb, gwl_ub), gwl, NA_real_)) |>
#   arrange(year, week) |> View()
#   filter(!(row_number() %in% n():(n()-10))) |>
#   ggplot(aes(paste0(year, ".", week) |> as.numeric(), gwl)) +
#   geom_line()



data_plot <- data_gwl |>
  mutate(date = lubridate::make_date(year, "01", "01") + lubridate::days(week * 7) - 4) |>
  group_by(fk_well) |>
  group_split() |>
  purrr::map_if(
    .p = ~ unique(.x$fk_well) == 1281,
    ~ .x |>
      anomalize::time_decompose(gwl, method = "stl", merge = TRUE, message = FALSE) |>
      anomalize::anomalize(remainder, method = "iqr", verbose = FALSE) |>
      anomalize::clean_anomalies() |>
      filter(anomaly == "No") |>
      filter(year != 2017, week != 9),
    .progress = TRUE
  ) |>
  reduce(bind_rows) |>
  make_gaps_explicit_and_impute_linear() |>
  rename(code = fk_well) |>
  mutate(code = as.character(code)) |>
  filter(year %in% year_start:year_end) |>
  filter(code %in% geofacet_grid_germany$code)

data_plot <- data_plot |>
  select(code, year, week, gwl)

data_plot <- data_plot |>
  arrange(code, week) |>
  group_by(code, week) |>
  summarise(gwl = mean(gwl, na.rm = TRUE)) |>
  arrange(gwl) |>
  mutate(
    week_min = first(week),
    week_max = last(week),
    gwl_min = min(gwl),
    gwl_max = max(gwl),
    weeks_recharge = if_else(week_max < week_min, list(c(week_min:53, 1:week_max)), list(c(week_min:week_max))),
    re_or_dis = if_else(week %in% (weeks_recharge |> unlist()), "re", "dis")

    # week_max = if_else(week_max < week_min, week_max + 53, week_max),
    # re_or_dis = if_else(between(week, week_min, week_max), "re", "dis")
    # re_or_dis = if_else(week_max < week_min & re_or_dis == "dis", "re", re_or_dis)
    ) |>
  ungroup() |>
  arrange(code, week) |>
  # select(-weeks_recharge) |>
  mutate(code = as.integer(code))

# Plot --------------------------------------------------------------------
## Geofacet ----------------------------------------------------------------
### polar ----------------------------------------------------------------


cp <- coord_polar(start = 0, clip = "off")
cp$is_free <- function() TRUE
cp <- ggiraphExtra:::coord_radar(start = 0)
cp$is_free <- function() TRUE
#### dark --------------------------------------------------------------------


plot <- data_plot |>
  ggplot(aes(week, gwl, group = year, colour = year)) +
  # ggplot(aes(week, gwl, group = year, colour = as.numeric(as.Date(paste(year, week, "01", "-"), "%Y-%U-%d")))) +
  # geom_smooth(method = "lm", se = FALSE) +
  # geom_line(aes(linewidth = year), show.legend = FALSE) +
  geom_line(aes(alpha = year), linewidth = 0.6, show.legend = FALSE) +
  # with_outer_glow(geom_point(data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE), colour = "white") +
  geom_point(data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE) +
  # geom_line(linewidth = .6, show.legend = FALSE) |> partition(vars(year)) |> blend("multiply") +
  scico::scale_colour_scico(
    limits = c(year_start, year_end),
    palette = "buda"
    # palette = "batlowK"
  ) +
  cp +
  scale_x_continuous(breaks = c(1, 14, 27, 40)) +
  scale_alpha(range = c(0.4, 1)) +
  scale_linewidth(range = c(0.4, 2), trans = "sqrt") +
  # ggdark::dark_theme_void() +
  facet_geo(~code, grid = geofacet_grid_germany, scales = "free") +
  # theme_void()
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
    panel.grid.major.x = element_line(colour = "grey50"),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel),
    plot.margin = margin(2, 2, 3, 2, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  ) +
  labs(
    title = "Groundwater in Germany",
    subtitle = subtitle_polar[1],
    caption = caption
  )

plot |>
  ggsave(filename = "groundwarter_singlewell_per_gridcell_polarcoord_dark_a2.png", width = 59.4, height = 84.1, units = "cm")


#### light -------------------------------------------------------------------

  test <- data_plot |>
  rowwise() |>
  mutate(weeks_recharge = length(unlist(weeks_recharge))) |>
  mutate(weeks_discharge = -(52 - weeks_recharge)) |>
  ungroup() |>
  mutate(weeks_recharge_min = min(c(weeks_recharge, abs(weeks_discharge)))) |>
  mutate(weeks_recharge_max = max(c(weeks_recharge, abs(weeks_discharge)))) |>
  mutate(weeks_recharge_norm = (weeks_recharge - weeks_recharge_min) / (weeks_recharge_max - weeks_recharge_min)) |>
  mutate(weeks_discharge_norm = -(weeks_discharge - -weeks_recharge_min) / (-weeks_recharge_max - -weeks_recharge_min))

test |> group_by(code) |>
  distinct(weeks_recharge, weeks_recharge_norm, weeks_discharge,weeks_discharge_norm) |>
  arrange(weeks_recharge) |>
  View()

# group_by(code) |>
  # filter(cur_group_id() %in% 1:30)

plot <-
  test |>
  ggplot() +
  geom_ribbon(
    aes(week, ymin = gwl_min, ymax = gwl_temp, fill = weeks_recharge_norm),
    data = test |>
      group_by(code) |>
      mutate(
        gwl_temp = if_else(re_or_dis == "dis", NA_real_, gwl),
        n_na = cumsum(is.na(gwl_temp)),
        gwl_temp = if_else(
          n_na == 1 | (n_na == max(n_na) & is.na(gwl_temp)), gwl, gwl_temp
        ),
        gwl_temp = if_else(
          row_number() == n(), first(gwl), gwl_temp
        ),
        .after = gwl
      ),
    # fill = colour_recharge,
    linewidth = 0.6,
    # alpha = 0.9
    show.legend = FALSE
    ) +
  # scale_fill_gradient(low = "#9fd5ccff", high = "#4f75acff") +
  geom_ribbon(
    aes(week, ymin = gwl_min, ymax = gwl, fill = weeks_discharge_norm),
    # data = test |>
    #   mutate(
    #     gwl_temp = if_else(re_or_dis == "re", NA_real_, gwl),
    #     n_na = cumsum(is.na(gwl_temp)),
    #     gwl_temp = if_else(
    #       n_na == 1 | (n_na == max(n_na) & !is.na(gwl_temp)), gwl, gwl_temp
    #       ),
    #     gwl_temp = if_else(
    #       row_number() == n(), first(gwl), gwl_temp
    #       ),
    #     .after = gwl),
    data = test |> mutate(gwl = if_else(re_or_dis == "re", NA_real_, gwl)),
    # fill = colour_discharge,
    # fill = "grey80",
    linewidth = 0.6,
    # alpha = 0.9,
    show.legend = FALSE
    ) +
  scico::scale_fill_scico(palette = "roma", direction = 1, midpoint = 0) +
  # scale_fill_gradient2("#bc9540ff", "white", "#64acc9ff", midpoint = 0) +
  # geom_segment(
  #   data = test |>
  #     group_by(code) |>
  #     filter(week == week_min) |>
  #     mutate(week_max = if_else(week_max == 53, 1, week_max + 1)),
  #   aes(x = week_max, xend = week_max, y = gwl_min, yend = gwl_max)
  #   ) +
  geom_point(
    data = test |>
      group_by(code) |>
      # filter(week == week_min) |>
      mutate(
        week_max = if_else(week_max == 53, 1, week_max + 1),
        gwl_temp = seq(first(gwl_min), first(gwl_max), length.out = n())
        ),
    aes(x = week_max, y = gwl_temp),
    size = 0.3,
    colour = "white"
    ) +
  geom_point(
    data = test |>
      group_by(code) |>
      # filter(week == week_min) |>
      mutate(
        week_max = if_else(week_max == 53, 1, week_max + 1),
        gwl_temp = seq(first(gwl_min), first(gwl_max), length.out = n())
      ) |>
      slice_max(order_by = gwl_temp),
    aes(x = week_max, y = gwl_temp),
    size = 3,
    shape = 21,
    stroke = 1.1,
    fill = "grey90",
    colour = "white"
  ) +
  # geom_line(
  #   data = test |>
  #     group_by(code) |>
  #     filter(week == week_min) |>
  #     mutate(week_max = if_else(week_max == 53, 1, week_max + 1)),
  #   aes(x = week_max, y = gwl_max)
  # ) +
  # with_outer_glow(geom_point(data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE), colour = "white") +
  # geom_line(linewidth = .6, show.legend = FALSE) |> partition(vars(year)) |> blend("multiply") +
  # paletteer::scale_color_paletteer_c(
  #   "viridis::plasma",
  #   limits = c(year_start, year_end)
  #   ) +
  cp +
  scale_x_continuous(breaks = c(1, 14, 27, 40)) +
  # facet_wrap(~code, scales = "free") +
  facet_geo(~code, grid = geofacet_grid_germany, scales = "free") +
  # theme_void()
  theme(
    plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title_light, margin = margin(30, 0, 20, 0)),
    plot.subtitle = element_markdown(family = font_subtitle, size = size_subtitle, hjust = 0.5, face = "plain", colour = colour_subtitle_light, lineheight = lineheight_subtitle, margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(family = font_subtitle, colour = colour_subtitle_light, size = 40, hjust = 1, vjust = -20),
    # axis.title = element_blank(),
    panel.background = element_rect(fill = colour_panel_light),
    plot.background = element_rect(fill = colour_panel_light),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(colour = colour_subtitle_light),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel_light),
    plot.margin = margin(2, 2, 3, 2, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  ) +
  labs(
    title = "Groundwater in Germany",
    subtitle = subtitle_polar[2],
    caption = caption,
    x = ""
  )




plot |>
  ggsave(filename = "groundwarter_singlewell_per_gridcell_polarcoord_redis_light_a1.png", width = 59.4, height = 84.1, units = "cm")

# plot |>
#   ggsave(filename = "groundwarter_singlewell_per_gridcell_polarcoord_light_a2.pdf", width = 59.4, height = 84.1, units = "cm")



#### anomaly -----------------------------------------------------------------

plot <- data_plot |>
  ggplot(aes(week, gwl_anomaly, group = year, colour = year)) +
  # ggplot(aes(week, gwl, group = year, colour = as.numeric(as.Date(paste(year, week, "01", "-"), "%Y-%U-%d")))) +
  # geom_smooth(method = "lm", se = FALSE) +
  # geom_line(aes(linewidth = year), show.legend = FALSE) +
  geom_line(aes(alpha = year), linewidth = 0.6, show.legend = FALSE) +
  # with_outer_glow(geom_point(data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE), colour = "white") +
  geom_point(data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE) +
  # geom_line(linewidth = .6, show.legend = FALSE) |> partition(vars(year)) |> blend("multiply") +
  scico::scale_colour_scico(
    # limits = c(1980, 2018),
    palette = "buda"
    # palette = "batlowK"
  ) +
  cp +
  scale_x_continuous(breaks = c(1, 14, 27, 40)) +
  scale_alpha(range = c(0.4, 1)) +
  scale_linewidth(range = c(0.4, 2), trans = "sqrt") +
  # ggdark::dark_theme_void() +
  facet_geo(~code, grid = geofacet_grid_germany) +
  # theme_void()
  theme(
    plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title, margin = margin(70, 0, 20, 0)),
    plot.subtitle = element_text(family = font_subtitle, size = size_subtitle, hjust = 0.5, face = "plain", colour = colour_subtitle, lineheight = lineheight_subtitle, margin = margin(0, 0, 50, 0)),
    plot.caption = element_text(family = font_subtitle, colour = colour_subtitle, size = 40, hjust = 1, vjust = -20),
    axis.title = element_blank(),
    panel.background = element_rect(fill = colour_panel),
    plot.background = element_rect(fill = colour_panel),
    # panel.grid = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(colour = "grey50"),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel),
    plot.margin = margin(2, 2, 3, 2, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  ) +
  labs(
    title = "Groundwater in Germany",
    subtitle = "Time series of measured groundwater levels anomalies across Germany in polar coordinates.\nThe time series cover the period between {year_start} and {year_end} as much as available.\nHere, the groundwater level anomalies are shown as normalized instead of absolute values\nin order to visually compare between different regions.",
    caption = caption
  )

plot |>
  ggsave(filename = "groundwarter_singlewell_per_gridcell_polarcoord_anomaly_dark_a2.png", width = 59.4, height = 84.1, units = "cm")


#### highlight 2022 ----------------------------------------------------------


plot <- data_plot |>
  filter(year <= 2022) |>
  mutate(highlight = if_else(year == 2022, TRUE, FALSE)) |>
  ggplot(aes(week, gwl, group = year, colour = highlight)) +
  # ggplot(aes(week, gwl, group = year, colour = as.numeric(as.Date(paste(year, week, "01", "-"), "%Y-%U-%d")))) +
  # geom_smooth(method = "lm", se = FALSE) +
  # geom_line(aes(linewidth = year), show.legend = FALSE) +
  geom_line(aes(alpha = highlight, linewidth = highlight), show.legend = FALSE) +
  # with_outer_glow(geom_point(data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE), colour = "white") +
  geom_point(data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE) +
  # geom_line(linewidth = .6, show.legend = FALSE) |> partition(vars(year)) |> blend("multiply") +
  # scale_colour_manual(values = c("grey", "#D1245C")) +
  scale_colour_manual(values = c("grey", "yellow")) +
  cp +
  scale_x_continuous(breaks = c(1, 14, 27, 40)) +
  scale_alpha_manual(values = c(0.4, 0.9)) +
  scale_linewidth_manual(values = c(0.5, 1)) +
  # ggdark::dark_theme_void() +
  facet_geo(~code, grid = geofacet_grid_germany, scales = "free") +
  # theme_void()
  theme(
    plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title, margin = margin(70, 0, 20, 0)),
    plot.subtitle = element_text(family = font_subtitle, size = size_subtitle, hjust = 0.5, face = "plain", colour = colour_subtitle, lineheight = lineheight_subtitle, margin = margin(0, 0, 50, 0)),
    plot.caption = element_text(family = font_subtitle, colour = colour_subtitle, size = 40, hjust = 1, vjust = -20),
    axis.title = element_blank(),
    panel.background = element_rect(fill = colour_panel),
    plot.background = element_rect(fill = colour_panel),
    # panel.grid = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(colour = "grey50"),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel),
    plot.margin = margin(2, 2, 3, 2, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  ) +
  labs(
    title = "Groundwater in Germany",
    subtitle = stringr::str_glue("Time series of measured groundwater levels across Germany in polar coordinates.\nThe time series cover the period between {year_start} and {year_end} as much as available.\nHere, the groundwater levels are shown as normalized instead of absolute values\nin order to visually compare between different regions."),
    caption = caption
  )

plot |>
  ggsave(filename = "groundwarter_singlewell_per_gridcell_polarcoord_highlight_dark_a2.png", width = 59.4, height = 84.1, units = "cm")


### kartesian ----------------------------------------------------------------

plot <- data_plot |>
  ggplot(aes(week, gwl, group = year, colour = year)) +
  # ggplot(aes(week, gwl, group = year, colour = as.numeric(as.Date(paste(year, week, "01", "-"), "%Y-%U-%d")))) +
  # geom_smooth(method = "lm", se = FALSE) +
  # geom_line(aes(linewidth = year), show.legend = FALSE) +
  geom_line(aes(alpha = year), linewidth = 0.6, show.legend = FALSE) +
  # with_outer_glow(geom_point(data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE), colour = "white") +
  geom_point(data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE) +
  # geom_line(linewidth = .6, show.legend = FALSE) |> partition(vars(year)) |> blend("multiply") +
  scico::scale_colour_scico(
    # limits = c(1980, 2018),
    palette = "buda"
    # palette = "batlowK"
  ) +
  scale_x_continuous(breaks = c(1, 14, 27, 40)) +
  scale_alpha(range = c(0.4, 1)) +
  scale_linewidth(range = c(0.4, 2), trans = "sqrt") +
  # ggdark::dark_theme_void() +
  facet_geo(~code, grid = geofacet_grid_germany, scales = "free") +
  # theme_void()
  theme(
    plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title, margin = margin(70, 0, 20, 0)),
    plot.subtitle = element_text(family = font_subtitle, size = size_subtitle, hjust = 0.5, face = "plain", colour = colour_subtitle, lineheight = lineheight_subtitle, margin = margin(0, 0, 50, 0)),
    plot.caption = element_text(family = font_subtitle, colour = colour_subtitle, size = 40, hjust = 1, vjust = -20),
    axis.title = element_blank(),
    panel.background = element_rect(fill = colour_panel),
    plot.background = element_rect(fill = colour_panel),
    panel.grid = element_blank(),
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_line(colour = "grey50"),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel),
    plot.margin = margin(2, 2, 3, 2, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  ) +
  labs(
    title = "Groundwater in Germany",
    subtitle = subtitle_cartesian,
    caption = caption
  )

plot |>
  ggsave(filename = "groundwarter_singlewell_per_gridcell_kartesiancoord_dark_a2.png", width = 59.4, height = 84.1, units = "cm")

#### anomaly -----------------------------------------------------------------


plot <- data_plot |>
  ggplot(aes(week, gwl_anomaly, group = year, colour = year)) +
  # ggplot(aes(week, gwl, group = year, colour = as.numeric(as.Date(paste(year, week, "01", "-"), "%Y-%U-%d")))) +
  # geom_smooth(method = "lm", se = FALSE) +
  # geom_line(aes(linewidth = year), show.legend = FALSE) +
  geom_line(aes(alpha = year), linewidth = 0.6, show.legend = FALSE) +
  # with_outer_glow(geom_point(data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE), colour = "white") +
  geom_point(data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE) +
  # geom_line(linewidth = .6, show.legend = FALSE) |> partition(vars(year)) |> blend("multiply") +
  scico::scale_colour_scico(
    # limits = c(1980, 2018),
    palette = "buda"
    # palette = "batlowK"
  ) +
  scale_x_continuous(breaks = c(1, 14, 27, 40)) +
  scale_y_continuous(trans = scales::pseudo_log_trans()) +
  scale_alpha(range = c(0.4, 1)) +
  scale_linewidth(range = c(0.4, 2), trans = "sqrt") +
  # ggdark::dark_theme_void() +
  facet_geo(~code, grid = geofacet_grid_germany) +
  # theme_void()
  theme(
    plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title, margin = margin(70, 0, 20, 0)),
    plot.subtitle = element_text(family = font_subtitle, size = size_subtitle, hjust = 0.5, face = "plain", colour = colour_subtitle, lineheight = lineheight_subtitle, margin = margin(0, 0, 50, 0)),
    plot.caption = element_text(family = font_subtitle, colour = colour_subtitle, size = 40, hjust = 1, vjust = -20),
    axis.title = element_blank(),
    panel.background = element_rect(fill = colour_panel),
    plot.background = element_rect(fill = colour_panel),
    panel.grid = element_blank(),
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_line(colour = "grey50"),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel),
    plot.margin = margin(2, 2, 3, 2, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  ) +
  labs(
    title = "Groundwater in Germany",
    subtitle = stringr::str_glue("Time series of measured groundwater levels anomalies across Germany.\nThe time series cover the period between {year_start} and {year_end} as much as available.\nHere, the groundwater level anomalies are shown as normalized instead of absolute values\nin order to visually compare between different regions."),
    caption = caption
  )


plot |>
  ggsave(filename = "groundwarter_singlewell_per_gridcell_kartesiancoord_anomaly_dark_a2.png", width = 59.4, height = 84.1, units = "cm")






## Joy Division ------------------------------------------------------------


data_plot <- data_gwl |>
  mutate(date = lubridate::make_date(year, "01", "01") + lubridate::days(week * 7) - 4) |>
  group_by(fk_well) |>
  group_split() |>
  purrr::map_if(
    .p = ~ unique(.x$fk_well) == 1281,
    ~ .x |>
      anomalize::time_decompose(gwl, method = "stl", merge = TRUE, message = FALSE) |>
      anomalize::anomalize(remainder, method = "iqr", verbose = FALSE) |>
      anomalize::clean_anomalies() |>
      filter(anomaly == "No") |>
      filter(year != 2017, week != 9),
    .progress = TRUE
  ) |>
  reduce(bind_rows) |>
  make_gaps_explicit_and_impute_linear() |>
  rename(code = fk_well) |>
  mutate(code = as.character(code)) |>
  filter(year %in% 2005:year_end) |>
  filter(code %in% geofacet_grid_germany$code)



plot <- data_plot |>
  ggplot(aes(week, y = year, group = year)) +
  # ggplot(aes(week, gwl, group = year, colour = as.numeric(as.Date(paste(year, week, "01", "-"), "%Y-%U-%d")))) +
  # geom_smooth(method = "lm", se = FALSE) +
  # geom_line(aes(linewidth = year), show.legend = FALSE) +
  ggridges::geom_ridgeline(
    aes(height = gwl),
    # min_height = min(data_plot$gwl),
    scale = 0.2,
    size = 1,
    fill = colour_panel,
    colour = "white"
  ) +
  # geom_line(aes(alpha = year), linewidth = 0.6, show.legend = FALSE) +
  # with_outer_glow(geom_point(data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE), colour = "white") +
  # geom_point(data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE) +
  # geom_line(linewidth = .6, show.legend = FALSE) |> partition(vars(year)) |> blend("multiply") +
  scico::scale_colour_scico(
    # limits = c(1980, 2018),
    palette = "buda"
    # palette = "batlowK"
  ) +
  scale_x_continuous(breaks = c(1, 14, 27, 40)) +
  scale_alpha(range = c(0.4, 1)) +
  scale_linewidth(range = c(0.4, 2), trans = "sqrt") +
  # ggdark::dark_theme_void() +
  facet_geo(~code, grid = geofacet_grid_germany, scales = "free") +
  # theme_void()
  theme(
    plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title, margin = margin(70, 0, 20, 0)),
    plot.subtitle = element_markdown(family = font_subtitle, size = size_subtitle, hjust = 0.5, face = "plain", colour = colour_subtitle, lineheight = lineheight_subtitle, margin = margin(0, 0, 50, 0)),
    plot.caption = element_text(family = font_subtitle, colour = colour_subtitle, size = 40, hjust = 1, vjust = -20),
    axis.title = element_blank(),
    panel.background = element_rect(fill = colour_panel),
    plot.background = element_rect(fill = colour_panel),
    panel.grid = element_blank(),
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_line(colour = "grey50"),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel),
    plot.margin = margin(2, 2, 3, 2, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  ) +
  labs(
    title = "Groundwater in Germany",
    subtitle = subtitle_joydivision,
    caption = caption
  )

plot


plot |>
  ggsave(filename = "groundwarter_singlewell_per_gridcell_joydivision_dark_a2.png", width = 59.4, height = 84.1, units = "cm")

## Referencecomparison ------------------------------------------------------------


data_plot <- data_gwl |>
  mutate(date = lubridate::make_date(year, "01", "01") + lubridate::days(week * 7) - 4) |>
  group_by(fk_well) |>
  group_split() |>
  purrr::map_if(
    .p = ~ unique(.x$fk_well) == 1281,
    ~ .x |>
      anomalize::time_decompose(gwl, method = "stl", merge = TRUE, message = FALSE) |>
      anomalize::anomalize(remainder, method = "iqr", verbose = FALSE) |>
      anomalize::clean_anomalies() |>
      filter(anomaly == "No") |>
      filter(year != 2017, week != 9),
    .progress = TRUE
  ) |>
  reduce(bind_rows) |>
  make_gaps_explicit_and_impute_linear() |>
  rename(code = fk_well) |>
  mutate(code = as.character(code)) |>
  filter(year %in% year_refstart_referencecomparison:year_focus_referencecomparison) |>
  filter(code %in% geofacet_grid_germany$code)

data_reference <- data_plot |>
  filter(year |> between(year_refstart_referencecomparison, year_refend_referencecomparison)) |>
  group_by(code, week) |>
  summarise(
    gwl_min = quantile(gwl, 0.05),
    gwl_median = quantile(gwl, 0.5),
    gwl_max = quantile(gwl, 0.95),
    .groups = "drop"
  )

data_plot <- data_plot |>
  left_join(data_reference, by = c("code", "week")) |>
  mutate(
    cor = cor(gwl, gwl_median),
    cor_class = cor > 0.7
  )

data_plot <- data_plot |>
  select(-season, -trend, -remainder, -remainder_l1, -remainder_l2, -anomaly, observed_cleaned)

data_plot <- data_plot |>
  group_by(code, year) |>
  filter(n() >= 52) |>
  group_by(code) |>
  arrange(year, week) |>
  filter(year == max(year)) |>
  # filter(year |> between(max(year) - 1, max(year))) |>
  ungroup()

data_plot <- data_plot |>
  left_join(expand(data_plot, code, year, week, decimals = 0:9), multiple = "all", by = join_by(code, year, week)) |>
  mutate(week = as.numeric(paste(week, decimals, sep = "."))) |>
  mutate(gwl = if_else(week %% 1 != 0, NA_real_, gwl)) |>
  group_by(code) |>
  filter(row_number() <= (n() - 9)) |>
  dplyr::mutate(gwl = zoo::na.approx(gwl)) |>
  ungroup()

data_plot <- data_plot |>
  mutate(class = case_when(
    gwl > gwl_max ~ "below_min",
    between(gwl, gwl_max, gwl_min) ~ "in_range",
    gwl < gwl_min ~ "above_max",
    .default = NA
  ))

# data_plot <- data_plot |>
#   mutate(
#     cor = cor(gwl, gwl_median),
#     cor_class = cor > 0.9
#     )

### polar -------------------------------------------------------------------
#### dark -------------------------------------------------------------------


plot <- data_plot |>
  filter(week %% 1 == 0) |>
  ggplot(aes(week, group = year)) +
  # ggplot(aes(week, gwl, group = year, colour = as.numeric(as.Date(paste(year, week, "01", "-"), "%Y-%U-%d")))) +
  # geom_smooth(method = "lm", se = FALSE) +
  # geom_line(aes(linewidth = year), show.legend = FALSE) +
  geom_ribbon(aes(ymin = gwl_min, ymax = gwl_max), fill = "white", alpha = .2) +
  geom_line(aes(y = gwl_median, linetype = cor_class), colour = colour_panel, linewidth = 0.4, show.legend = FALSE) +
  geom_line(
    data = data_plot,
    aes(y = gwl, colour = gwl > gwl_median), linewidth = 0.8, show.legend = FALSE
  ) +
  geom_text(
    data = data_plot |> group_by(code, year) |> summarise(gwl = mean(gwl, na.rm = TRUE)),
    aes(x = 40, y = gwl, label = year),
    colour = "white"
  ) +
  # with_outer_glow(geom_point(data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE), colour = "white") +
  geom_point(aes(y = gwl), data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE, size = 1.5) +
  # geom_line(linewidth = .6, show.legend = FALSE) |> partition(vars(year)) |> blend("multiply") +
  scale_colour_manual(values = colour_referencecomparison) +
  geom_line(aes(y = gwl_median, linetype = cor_class), colour = colour_panel, linewidth = 0.4, show.legend = FALSE) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  cp +
  scale_x_continuous(breaks = c(1, 14, 27, 40)) +
  scale_alpha(range = c(0.4, 1)) +
  scale_linewidth(range = c(0.4, 2), trans = "sqrt") +
  facet_geo(~code, grid = geofacet_grid_germany, scales = "free") +
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
    panel.grid.major.x = element_line(colour = "grey50"),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel),
    plot.margin = margin(2, 2, 3, 2, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  ) +
  labs(
    title = "Groundwater in Germany",
    subtitle = subtitle_referencecomparison,
    caption = caption
  )

plot |>
  ggsave(filename = "groundwarter_singlewell_per_gridcell_referencecomparison_polar_dark_a2.png", width = 59.4, height = 84.1, units = "cm")



### kartesian -------------------------------------------------------------------
#### dark --------------------------------------------------------------------


plot <- data_plot |>
  filter(week %% 1 == 0) |>
  ggplot(aes(week, group = year)) +
  # ggplot(aes(week, gwl, group = year, colour = as.numeric(as.Date(paste(year, week, "01", "-"), "%Y-%U-%d")))) +
  # geom_smooth(method = "lm", se = FALSE) +
  # geom_line(aes(linewidth = year), show.legend = FALSE) +
  geom_ribbon(aes(ymin = gwl_min, ymax = gwl_max), fill = "white", alpha = .2) +
  geom_line(aes(y = gwl_median, linetype = cor_class), colour = colour_panel, linewidth = 0.4, show.legend = FALSE) +
  geom_line(
    data = data_plot,
    aes(y = gwl, colour = gwl > gwl_median), linewidth = 0.8, show.legend = FALSE
  ) +
  geom_text(
    data = data_plot |> group_by(code, year) |> summarise(gwl = max(gwl_max, na.rm = TRUE)),
    aes(x = 45, y = gwl, label = year),
    size = 8,
    colour = "white"
  ) +
  # with_outer_glow(geom_point(data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE), colour = "white") +
  geom_point(aes(y = gwl), data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE, size = 1.5) +
  # geom_line(linewidth = .6, show.legend = FALSE) |> partition(vars(year)) |> blend("multiply") +
  scale_colour_manual(values = colour_referencecomparison) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  # cp +
  scale_x_continuous(breaks = c(1, 14, 27, 40)) +
  scale_alpha(range = c(0.4, 1)) +
  scale_linewidth(range = c(0.4, 2), trans = "sqrt") +
  facet_geo(~code, grid = geofacet_grid_germany, scales = "free") +
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
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel),
    plot.margin = margin(2, 2, 3, 2, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  ) +
  labs(
    title = "Groundwater in Germany",
    subtitle = subtitle_referencecomparison_kartesian,
    caption = caption
  )

plot |>
  ggsave(filename = "groundwarter_singlewell_per_gridcell_referencecomparison_kartesian_dark_a2.png", width = 59.4, height = 84.1, units = "cm")



#### light --------------------------------------------------------------------

plot <- data_plot |>
  filter(week %% 1 == 0) |>
  rowwise() |>
  ungroup() |>
  ggplot(aes(week, group = year)) +
  geom_ribbon(aes(ymin = gwl_min, ymax = gwl_max), fill = "grey") +
  ggh4x::stat_difference(aes(x = week, ymin = gwl, ymax = gwl_median), alpha = 0.3, show.legend = FALSE) +
  geom_line(
    data = data_plot,
    aes(
      y = gwl,
      colour = gwl > gwl_median
    ),
    linewidth = 0.6, alpha = 0.8, show.legend = FALSE
  ) +
  geom_line(
    aes(
      y = gwl_median
    ),
    colour = "grey25", linewidth = 0.6, show.legend = FALSE
  ) +
  # geom_line(aes(week, gwl_max), colour = "grey50", linetype = "dashed") +
  # geom_line(aes(week, gwl_min), colour = "grey50", linetype = "dashed") +
  geom_text(
    data = data_plot |> filter(year != year_focus_referencecomparison) |> group_by(code, year) |> summarise(gwl = max(gwl_max, na.rm = TRUE)),
    aes(x = -Inf, y = -Inf, label = year),
    hjust = -6.2,
    vjust = -18.6,
    size = 8,
    colour = "grey50"
  ) +
  # geom_point(
  #   data = data_plot |>
  #     group_by(code) |>
  #     filter(gwl_median == min(gwl_median) | gwl_median == max(gwl_median)) |>
  #     group_by(code, gwl_median) |>
  #     slice_sample(n = 1),
  #   aes(y = gwl_median),
  #   colour = "grey25",
  #   show.legend = FALSE
  # ) +
  # with_outer_glow(geom_point(data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE), colour = "white") +
  # geom_point(aes(y = gwl), data = data_plot |> group_by(code) |> slice_max(order_by = year + week), colour = "white", shape = 8, show.legend = FALSE, size = 1.5) +
  scale_colour_manual(values = colour_referencecomparison) +
  scale_fill_manual(values = colour_referencecomparison) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(breaks = c(1, 14, 27, 40), expand = c(0, 0)) +
  # scale_y_continuous(expand = c(0, 0.075)) +
  scale_alpha(range = c(0.4, 1)) +
  scale_linewidth(range = c(0.4, 2), trans = "sqrt") +
  facet_geo(~code, grid = geofacet_grid_germany, scales = "free") +
  theme(
    plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title_light, margin = margin(60, 0, 40, 0)),
    plot.subtitle = element_markdown(family = font_subtitle, size = size_subtitle, hjust = 0, face = "plain", colour = colour_subtitle_light, lineheight = lineheight_subtitle, margin = margin(0, 0, 80, 0)),
    plot.caption = element_text(family = font_subtitle, colour = colour_subtitle_light, size = 40, hjust = 1, vjust = -20),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "grey90"),
    plot.background = element_rect(fill = colour_panel_light),
    # panel.grid = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel_light),
    plot.margin = margin(2, 2, 3, 2, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  ) +
  labs(
    title = "Groundwater in Germany",
    subtitle = subtitle_referencecomparison_kartesian,
    caption = caption
  )

plot |>
  ggsave(filename = "groundwarter_singlewell_per_gridcell_referencecomparison_kartesian_light_a2.png", width = 59.4, height = 84.1, units = "cm")

## Legend ------------------------------------------------------------------
### Year --------------------------------------------------------------------
#### Dark --------------------------------------------------------------------


data_legend <- data_plot |>
  distinct(year)

dummy_plot <- data_legend |> # mutate(year = as.character(year)) |>
  ggplot(aes(1, year, colour = year)) +
  geom_point() +
  paletteer::scale_colour_paletteer_binned("scico::buda", breaks = c(year_start, seq(1995, 2015, 5), year_end)) +
  guides(
    colour = guide_coloursteps(
      barwidth = unit(2, "mm"),
      barheight = unit(100, "mm"),
    )
  ) +
  labs(colour = "Year") +
  theme_void() +
  theme(
    legend.title = element_text(family = font_subtitle, size = 20, hjust = 0.0, face = "plain", colour = colour_subtitle, margin = margin(0, 0, 20, 0)),
    legend.text = element_text(family = font_subtitle, size = 16, hjust = 0.5, face = "plain", colour = colour_subtitle, margin = margin(0, 0, 50, 0)),
    plot.caption = element_text(family = font_subtitle, colour = colour_subtitle, size = 40, hjust = 1, vjust = -20),
    axis.title = element_blank(),
    panel.background = element_rect(fill = colour_panel),
    plot.background = element_rect(fill = colour_panel),
    panel.grid = element_blank(),
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_line(colour = "grey50"),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel),
    plot.margin = margin(2, 2, 3, 2, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  )

legend_plot <- dummy_plot |> ggpubr::get_legend()

legend_plot |>
  ggsave(filename = "legend_year.png", width = 4, height = 15, units = "cm", bg = colour_panel)


#### Light --------------------------------------------------------------------


data_legend <- data_plot |>
  distinct(year)

dummy_plot <- data_legend |> # mutate(year = as.character(year)) |>
  ggplot(aes(1, year, colour = year)) +
  geom_point() +
  paletteer::scale_colour_paletteer_binned("scico::hawaii", direction = -1, breaks = c(year_start, seq(1995, 2015, 5), year_end)) +
  guides(
    colour = guide_coloursteps(
      barwidth = unit(2, "mm"),
      barheight = unit(100, "mm"),
    )
  ) +
  labs(colour = "Year") +
  theme_void() +
  theme(
    legend.title = element_text(family = font_subtitle, size = 20, hjust = 0.0, face = "plain", colour = colour_subtitle_light, margin = margin(0, 0, 20, 0)),
    legend.text = element_text(family = font_subtitle, size = 16, hjust = 0.5, face = "plain", colour = colour_subtitle_light, margin = margin(0, 0, 50, 0)),
    plot.caption = element_text(family = font_subtitle, colour = colour_subtitle_light, size = 40, hjust = 1, vjust = -20),
    axis.title = element_blank(),
    panel.background = element_rect(fill = colour_panel_light),
    plot.background = element_rect(fill = colour_panel_light),
    panel.grid = element_blank(),
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_line(colour = "grey50"),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel_light),
    plot.margin = margin(2, 2, 3, 2, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  )

legend_plot <- dummy_plot |> ggpubr::get_legend()

legend_plot |>
  ggsave(filename = "legend_year_light.png", width = 4, height = 15, units = "cm", bg = colour_panel_light)

# library(patchwork)
#
# plot +
#   inset_element(legend_plot)


### Minimap -----------------------------------------------------------------


#### Dark --------------------------------------------------------------------



admin_borders_germany <- ne_countries(country = "Germany", scale = "medium") |>
  sf::st_as_sf() |>
  sf::st_transform(sf::st_crs(grid_germany))

admin_borders_germany |>
  write_rds("admin_borders_germany.rds")


plot_data_legend <- admin_borders_germany |>
  sf::st_intersection(grid_germany) |>
  mutate(has_well = !is.na(fk_well))

legend_plot <- plot_data_legend |>
  ggplot() +
  # geom_sf_pattern(aes(pattern_type = has_well), show.legend = FALSE) +
  geom_sf(aes(fill = has_well), show.legend = FALSE, linewidth = .5, colour = colour_subtitle) +
  geom_sf(data = admin_borders_germany, colour = colour_subtitle, fill = NA, linewidth = .9) +
  # geom_sf(data = grid_germany, fill = NA) +
  geom_sf(data = key_well_meta, shape = 21, colour = "yellow", fill = colour_subtitle, size = 2) +
  # geom_sf_label(aes(label = fk_well), data = key_well_meta) +
  # scale_pattern(choices = c("stripes", NA)) +
  scale_fill_manual(values = c("grey40", colour_panel)) +
  theme(
    plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title, margin = margin(70, 0, 20, 0)),
    plot.subtitle = element_text(family = font_subtitle, size = size_subtitle, hjust = 0.5, face = "plain", colour = colour_subtitle, lineheight = lineheight_subtitle, margin = margin(0, 0, 50, 0)),
    plot.caption = element_text(family = font_subtitle, colour = colour_subtitle, size = 40, hjust = 1, vjust = -20),
    axis.title = element_blank(),
    panel.background = element_rect(fill = colour_panel, colour = NA),
    plot.background = element_rect(fill = colour_panel, colour = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  )

legend_plot |>
  ggsave(filename = "legend_minimap.png", scale = 2.5, width = 6, height = 8, units = "cm")


##### Referencecomparison -----------------------------------------------------


legend_plot <- plot_data_legend |>
  ggplot() +
  # geom_sf_pattern(aes(pattern_type = has_well), show.legend = FALSE) +
  geom_sf(aes(fill = has_well), show.legend = FALSE, linewidth = .5, colour = colour_subtitle) +
  geom_sf(data = admin_borders_germany, colour = colour_subtitle, fill = NA, linewidth = .9) +
  # geom_sf(data = grid_germany, fill = NA) +
  geom_sf(data = key_well_meta, shape = 21, colour = colour_referencecomparison[1], fill = colour_subtitle, size = 2) +
  # geom_sf_label(aes(label = fk_well), data = key_well_meta) +
  # scale_pattern(choices = c("stripes", NA)) +
  scale_fill_manual(values = c("grey40", colour_panel)) +
  theme(
    plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title, margin = margin(70, 0, 20, 0)),
    plot.subtitle = element_text(family = font_subtitle, size = size_subtitle, hjust = 0.5, face = "plain", colour = colour_subtitle, lineheight = lineheight_subtitle, margin = margin(0, 0, 50, 0)),
    plot.caption = element_text(family = font_subtitle, colour = colour_subtitle, size = 40, hjust = 1, vjust = -20),
    axis.title = element_blank(),
    panel.background = element_rect(fill = colour_panel, colour = NA),
    plot.background = element_rect(fill = colour_panel, colour = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  )

legend_plot |>
  ggsave(filename = "legend_minimap_referencecomparison.png", scale = 2.5, width = 6, height = 8, units = "cm")


#### Light --------------------------------------------------------------------



admin_borders_germany <- ne_countries(country = "Germany", scale = "medium") |>
  sf::st_as_sf() |>
  sf::st_transform(sf::st_crs(grid_germany))

admin_borders_germany |>
  write_rds("admin_borders_germany.rds")


plot_data_legend <- admin_borders_germany |>
  sf::st_intersection(grid_germany) |>
  mutate(has_well = !is.na(fk_well)) |>
  sf::st_as_sf()

breaks_y <- grid_germany |>
  sf::st_centroid() |>
  sf::st_coordinates() |>
  as_tibble() |>
  pull(Y) |>
  unique()

breaks_x <- grid_germany |>
  sf::st_centroid() |>
  sf::st_coordinates() |>
  as_tibble() |>
  pull(X) |>
  unique()

legend_plot <- plot_data_legend |>
  ggplot() +
  # geom_sf_pattern(aes(pattern_type = has_well), show.legend = FALSE) +
  geom_sf(aes(fill = has_well), show.legend = FALSE, linewidth = .5, colour = colour_subtitle_light) +
  geom_sf(data = admin_borders_germany, colour = colour_subtitle_light, fill = NA, linewidth = .9) +
  # geom_sf(data = grid_germany, fill = NA) +
  geom_sf(data = key_well_meta, shape = 21, colour = buda_high[2], fill = colour_subtitle_light, size = 2) +
  # geom_sf_label(aes(label = fk_well), data = key_well_meta) +
  # scale_pattern(choices = c("stripes", NA)) +
  scale_fill_manual(values = c("grey50", "white")) +
  scale_y_continuous(
    breaks = breaks_y,
    labels = LETTERS[length(breaks_y):1]
  ) +
  scale_x_continuous(
    breaks = breaks_x,
    labels = 1:length(breaks_x)
  ) +
  coord_sf(datum = sf::st_crs(plot_data_legend), label_graticule = "NE", label_axes = list(top = "N", left = "E")) +
  theme(
    plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title_light, margin = margin(70, 0, 20, 0)),
    plot.subtitle = element_text(family = font_subtitle, size = size_subtitle, hjust = 0.5, face = "plain", colour = colour_subtitle_light, lineheight = lineheight_subtitle, margin = margin(0, 0, 50, 0)),
    plot.caption = element_text(family = font_subtitle, colour = colour_subtitle_light, size = 40, hjust = 1, vjust = -20),
    axis.title = element_blank(),
    panel.background = element_rect(fill = colour_panel_light, colour = NA),
    plot.background = element_rect(fill = colour_panel_light, colour = NA),
    panel.grid.major = element_line(linetype = "dotted", colour = "grey90"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(family = font_subtitle, face = "bold", colour = colour_subtitle_light),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel_light),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  )
legend_plot

legend_plot |>
  ggsave(filename = "legend_minimap_light.png", scale = 2.5, width = 6, height = 8, units = "cm")



##### Referencecomparison -----------------------------------------------------

legend_plot <- plot_data_legend |>
  ggplot() +
  # geom_sf_pattern(aes(pattern_type = has_well), show.legend = FALSE) +
  geom_sf(aes(fill = has_well), show.legend = FALSE, linewidth = .5, colour = colour_subtitle_light) +
  geom_sf(data = admin_borders_germany, colour = colour_subtitle_light, fill = NA, linewidth = .9) +
  # geom_sf(data = grid_germany, fill = NA) +
  geom_sf(data = key_well_meta, shape = 21, colour = colour_referencecomparison[1], fill = colour_subtitle_light, size = 2) +
  # geom_sf_label(aes(label = fk_well), data = key_well_meta) +
  # scale_pattern(choices = c("stripes", NA)) +
  scale_y_continuous(
    breaks = breaks_y,
    labels = LETTERS[length(breaks_y):1]
  ) +
  scale_x_continuous(
    breaks = breaks_x,
    labels = 1:length(breaks_x)
  ) +
  coord_sf(datum = sf::st_crs(plot_data_legend), label_graticule = "NE", label_axes = list(top = "N", left = "E")) +
  scale_fill_manual(values = c(colour_subtitle_light, "white")) +
  theme(
    plot.title = element_text(family = font_title, size = size_title, hjust = 0.5, colour = colour_title_light, margin = margin(70, 0, 20, 0)),
    plot.subtitle = element_text(family = font_subtitle, size = size_subtitle, hjust = 0.5, face = "plain", colour = colour_subtitle_light, lineheight = lineheight_subtitle, margin = margin(0, 0, 50, 0)),
    plot.caption = element_text(family = font_subtitle, colour = colour_subtitle_light, size = 40, hjust = 1, vjust = -20),
    axis.title = element_blank(),
    panel.background = element_rect(fill = colour_panel_light, colour = NA),
    plot.background = element_rect(fill = colour_panel_light, colour = NA),
    panel.grid.major = element_line(linetype = "dotted", colour = "grey90"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(family = font_subtitle, face = "bold", colour = colour_subtitle_light),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_line(colour = colour_panel_light),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.spacing = unit(panel_spacing, "mm")
  )
legend_plot


legend_plot |>
  ggsave(filename = "legend_minimap_referencecomparison_light.png", scale = 2.5, width = 6, height = 8, units = "cm")





# plot --------------------------------------------------------------------




data_gwl |>
  rename(code = fk_well) |>
  group_by(code) |>
  filter(cur_group_id() %in% 1:5) |>
  ggplot(aes(week, gwl, group = year, colour = year)) +
  # ggplot(aes(week, gwl, group = year, colour = as.numeric(as.Date(paste(year, week, "01", "-"), "%Y-%U-%d")))) +
  # geom_smooth(method = "lm", se = FALSE) +
  geom_line(linewidth = .6, show.legend = FALSE) +
  # geom_line(linewidth = .5, show.legend = FALSE) |> partition(vars(year)) |> blend("multiply") +
  scico::scale_colour_scico(
    # limits = c(1980, 2018),
    palette = "buda"
  ) +
  coord_polar(start = 0, clip = "off") +
  scale_x_continuous(breaks = 1:53) +
  # ggdark::dark_theme_void() +
  theme_void() +
  facet_geo(~code, grid = geofacet_grid_germany) +
  theme(
    axis.title = element_blank(),
    panel.background = element_rect(fill = colour_panel)
  )


data_gwl |>
  rename(code = fk_well) |>
  mutate(code = as.character(code)) |>
  # group_by(code) |>
  # filter(cur_group_id() %in% 1:5, year == 2015) |>
  filter(code %in% geofacet_grid_germany$code) |>
  ggplot(aes(week, gwl, group = year, colour = year)) +
  # ggplot(aes(week, gwl, group = year, colour = as.numeric(as.Date(paste(year, week, "01", "-"), "%Y-%U-%d")))) +
  # geom_smooth(method = "lm", se = FALSE) +
  geom_line(linewidth = .6, show.legend = FALSE) +
  # geom_line(linewidth = .5, show.legend = FALSE) |> partition(vars(year)) |> blend("multiply") +
  scico::scale_colour_scico(
    # limits = c(1980, 2018),
    palette = "buda"
  ) +
  # cp +
  scale_x_continuous(breaks = 1:53) +
  # ggdark::dark_theme_void() +
  theme_void() +
  facet_geo(~code, grid = geofacet_grid_germany, scales = "free") +
  theme(
    axis.title = element_blank(),
    panel.background = element_rect(fill = colour_panel)
  )



## Patchwork ---------------------------------------------------------------


plot_list <- data_gwl |>
  group_by(fk_well) |>
  group_split() |>
  purrr::map(
    ~ .x |>
      ggplot(aes(week, gwl, group = year, colour = year)) +
      # ggplot(aes(week, gwl, group = year, colour = as.numeric(as.Date(paste(year, week, "01", "-"), "%Y-%U-%d")))) +
      # geom_smooth(method = "lm", se = FALSE) +
      geom_line(linewidth = .6, show.legend = FALSE) +
      # geom_line(linewidth = .5, show.legend = FALSE) |> partition(vars(year)) |> blend("multiply") +
      scico::scale_colour_scico(
        # limits = c(1980, 2018),
        palette = "buda"
      ) +
      coord_polar(start = 0, clip = "off") +
      scale_x_continuous(breaks = 1:53) +
      # ggdark::dark_theme_void() +
      theme_void() +
      theme(
        axis.title = element_blank(),
        panel.background = element_rect(fill = colour_panel)
      )
  )

plot_list |>
  magrittr::extract(1:5) |>
  patchwork::wrap_plots() &
  theme(
    panel.background = element_rect(fill = colour_panel, colour = NA)
  )
