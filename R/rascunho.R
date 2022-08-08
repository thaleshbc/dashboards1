

library(ggplot2)
library(dplyr)

ubs <- readr::read_rds("ubs_completa.rds")

ubs |>
  #dplyr::filter(nome_uf == "Rio Grande do Norte") |>
  leaflet::leaflet() |>
  leaflet::addTiles() |>
  #leaflet::addCircles() |>
  leaflet::addCircleMarkers(lng = ~longitude, lat = ~latitude, clusterOptions = leaflet::markerClusterOptions())

municipios <- geobr::read_municipality()
mesorregiao <- geobr::read_meso_region()
estados <- geobr::read_state()

estados <- estados |>
  select(code_state, abbrev_state) |>
  tibble::as_tibble() |>
  rename(uf = code_state)

ubs <- left_join(ubs, estados)

ubs |>
  dplyr::select(cnes:abbrev_state) |>
  readr::write_rds("ubs_completa_final.rds")

# contagem municipios -----------------------------------------------------

tmp <- ubs |>
  dplyr::count(municipio_completo)|>
  dplyr::rename(code_muni = municipio_completo) |>
  dplyr::mutate(code_muni = as.numeric(code_muni))

municipios <- dplyr::left_join(municipios, tmp)

municipios <- municipios |>
  dplyr::mutate(
    n = tidyr::replace_na(n, 0)
  )

municipios |>
  filter(abbrev_state == "SP") |>
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_gradientn(colors = c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026"))


# contagem mesorregiao ----------------------------------------------------

tmp2 <- ubs |>
  dplyr::mutate(
    code_meso = paste0(uf, mesorregiao_geografica),
    code_meso = as.numeric(code_meso)
  ) |>
  dplyr::count(code_meso) |>
  dplyr::filter(code_meso != is.na(code_meso))

mesorregiao <- dplyr::left_join(mesorregiao, tmp2)

mesorregiao |>
  filter(abbrev_state == "SP") |>
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_gradientn(colors = c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026"))




RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")
RColorBrewer::display.brewer.all()
