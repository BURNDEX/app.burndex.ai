


library(tidyverse)
library(climateR)
library(AOI)
aoi <- aoi_get(state = "CA", county = "Santa Barbara")

cities <- USAboundaries::us_cities() %>%
  filter(!state_name %in% c("Alaska", "Hawaii", "Puerto Rico"))
mapview::mapview(cities$geometry)

maca_jan <- aggregate_maca(aoi,start_date = "2050-01-05", end_date = "2050-01-05")
maca_mar <- aggregate_maca(aoi,start_date = "2050-03-05", end_date = "2050-03-05")
maca_june <- aggregate_maca(aoi,start_date = "2050-06-05", end_date = "2050-06-05")
maca_sept <- aggregate_maca(aoi,start_date = "2050-09-05", end_date = "2050-09-05")
doParallel::stopImplicitCluster()

maca_jan <- dplyr::select(maca_jan,lat,lon,date, burn_index)
maca_mar <- dplyr::select(maca_mar,lat,lon,date, burn_index)
maca_june <- dplyr::select(maca_june,lat,lon,date, burn_index)
maca_sept <- dplyr::select(maca_sept,lat,lon,date, burn_index)
maca_jan2 <- maca_jan %>%
  rename(Latitude = lat, Longitude = lon,"Burn index" = burn_index) %>%
  pivot_longer(4, values_to = "value", names_to = "variable") %>%
  group_by(variable) %>%
  mutate(value = scales::rescale(value, to = 0:1),
         source = "MACA")
maca_mar2 <- maca_mar %>%
  rename(Latitude = lat, Longitude = lon,"Burn index" = burn_index) %>%
  pivot_longer(4, values_to = "value", names_to = "variable") %>%
  group_by(variable) %>%
  mutate(value = scales::rescale(value, to = 0:1),
         source = "MACA")
maca_june2 <- maca_june %>%
  rename(Latitude = lat, Longitude = lon,"Burn index" = burn_index) %>%
  pivot_longer(4, values_to = "value", names_to = "variable") %>%
  group_by(variable) %>%
  mutate(value = scales::rescale(value, to = 0:1),
         source = "MACA")
maca_sept2 <- maca_sept %>%
  rename(Latitude = lat, Longitude = lon,"Burn index" = burn_index) %>%
  pivot_longer(4, values_to = "value", names_to = "variable") %>%
  group_by(variable) %>%
  mutate(value = scales::rescale(value, to = 0:1),
         source = "MACA")

join <- inner_join(maca_jan, maca_june, by = c("lat", "lon"))
maca2 <- maca %>%
  rename(Latitude = lat, Longitude = lon,
         Precipitation = "prcp", "Burn index" = burn_index,
         "Rel. Humidity max" = rhmax, "Rel. Humidity min" = rhmin,
         "Specific humidity" = shum, "Downward solar radiation" = srad,
         "Temperature max" = tmax, "Temperature min" = tmin) %>%
  pivot_longer(4:11, values_to = "value", names_to = "variable") %>%
  group_by(variable) %>%
  mutate(value = scales::rescale(value, to = 0:1),
         source = "MACA")

gridmet <- aggregate_gridmet(aoi,start_date = "2018-01-08", end_date = "2018-01-08")
doParallel::stopImplicitCluster()

gridmet2 <- gridmet %>%
  rename(Latitude = lat, Longitude = lon,
         Precipitation = "prcp", "Burn index" = burn_index,
         "Rel. Humidity max" = rhmax, "Rel. Humidity min" = rhmin,
         "Specific humidity" = shum, "Downward solar radiation" = srad,
         "Temperature max" = tmax, "Temperature min" = tmin) %>%
  pivot_longer(4:11, values_to = "value", names_to = "variable") %>%
  group_by(variable) %>%
  mutate(value = scales::rescale(value, to = 0:1),
         source = "gridMET")


maca_jan_gg <- ggplot() +
  geom_tile(
    data = maca_jan2, aes(Longitude, Latitude, fill = value)) +
  facet_wrap(~variable)+
scale_fill_distiller(type = "div", palette = "Spectral") +
  coord_equal() +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
panel.grid.minor = element_blank(),
panel.grid.major = element_blank(),
plot.background = element_rect(fill = "transparent",colour = NA)
) +
  ggtitle("MACA January 2050 Burn Index") +
  labs(fill = " ",
       x = " ",
       y = " ")

maca_mar_gg <- ggplot() +
  geom_tile(
    data = maca_mar2, aes(Longitude, Latitude, fill = value)) +
  facet_wrap(~variable)+
  scale_fill_distiller(type = "div", palette = "Spectral") +
  coord_equal() +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  ggtitle("MACA March 2050 Burn Index") +
  labs(fill = " ",
       x = " ",
       y = " ")

maca_june_gg <- ggplot() +
  geom_tile(
    data = maca_june2, aes(Longitude, Latitude, fill = value)) +
  facet_wrap(~variable)+
  scale_fill_distiller(type = "div", palette = "Spectral") +
  coord_equal() +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  ggtitle("MACA June 2050 Burn Index") +
  labs(fill = " ",
       x = " ",
       y = " ")

maca_sept_gg <- ggplot() +
  geom_tile(
    data = maca_sept2, aes(Longitude, Latitude, fill = value)) +
  facet_wrap(~variable)+
  scale_fill_distiller(type = "div", palette = "Spectral") +
  coord_equal() +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  ggtitle("MACA September 2050 Burn Index") +
  labs(fill = " ",
       x = " ",
       y = " ")

gridmet_gg <- ggplot() +
  geom_tile(
    data = gridmet2, aes(Longitude, Latitude, fill = value)) +
  facet_wrap(~variable)+
  scale_fill_distiller(type = "div", palette = "Spectral") +
  coord_equal() +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  ggtitle("gridMET Climate rasters") +
  labs(fill = " ",
       x = " ",
       y = " ")


patch <- patchwork::plot_layout(gridmet_gg + maca_gg, guides = "collect")
class(patch)
ggplotGrob(patch)
install.packages("ggassemble")
library
ggplo
ggsave("maca_jan.png", maca_jan_gg, bg ="transparent")
ggsave("maca_mar.png", maca_mar_gg, bg ="transparent")
ggsave("maca_june.png", maca_june_gg, bg ="transparent")
ggsave("maca_sept.png", maca_sept_gg, bg ="transparent")

# library(raster)
# tmp1 <- rasterFromXYZ(xyz = dplyr::select(gridmet, lon,lat, prcp:burn_index))
# gridmet_st <- stack(tmp1)
# tmp2 <- rasterFromXYZ(xyz = dplyr::select(maca, lon,lat, prcp:burn_index))
# maca_st <- stack(tmp2)
# resamp <- resample(tmp2, tmp1, method = "bilinear")
# resamp <- stack(resamp)
custom_agg <- function(aoi, date) {
  aggregate_gridmet(
    aoi,
    start_date = date,
    end_date   = date
  ) %>%
    dplyr::mutate(
      AOI = aoi$name
    )
}
areas_of_interest <-
  list(
    AOI::aoi_get(county = "Santa Barbara", state = "California"),
    AOI::aoi_get(county = "King", state = "Washington"),
    AOI::aoi_get(county = "Lee", state = "Florida"),
    AOI::aoi_get(county = "El Paso", state = "Texas"),
    AOI::aoi_get(county = "York", state = "Maine"),
    AOI::aoi_get(county = "Ramsey", state = "Minnesota"),
    AOI::aoi_get(county = "Shawnee", state = "Kansas")
  )
progressr::with_progress({
  gridmet_data_jan <- lapply(
    X = areas_of_interest,
    FUN = custom_agg,
    date = years[1]
  )
})

