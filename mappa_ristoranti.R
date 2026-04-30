#!/usr/bin/env Rscript

required_packages <- c("sf", "ggplot2", "dplyr", "readr", "tidyr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(readr)
})

# URL dei dataset forniti
geojson_url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson"
csv_url <- "https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/data_on_french_states.csv"

# Cartella locale dove salvare i file scaricati
data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

geojson_path <- file.path(data_dir, "communes.geojson")
csv_path <- file.path(data_dir, "ristoranti_francia.csv")

if (!file.exists(geojson_path)) {
  download.file(geojson_url, destfile = geojson_path, mode = "wb")
}

if (!file.exists(csv_path)) {
  download.file(csv_url, destfile = csv_path, mode = "wb")
}

# Lettura dati ristoranti.
# Questo CSV e' separato da ';' e non contiene lat/lon dirette:
# usiamo quindi il codice comunale (dciris -> primi 5 caratteri) per
# collegarlo ai comuni e ottenere coordinate dai centroidi.
ristoranti_raw <- read_delim(
  csv_path,
  delim = ";",
  skip = 1,
  col_names = c("id", "reg", "dep", "depcom", "dciris", "an", "typequ", "nb_equip"),
  show_col_types = FALSE
)

ristoranti <- ristoranti_raw %>%
  mutate(
    commune_code = substr(dciris, 1, 5),
    nb_equip = suppressWarnings(as.numeric(nb_equip))
  ) %>%
  filter(!is.na(commune_code), !is.na(nb_equip), nb_equip > 0)

# Nel dataset il codice A504 corrisponde ai ristoranti.
if ("A504" %in% ristoranti$typequ) {
  ristoranti <- filter(ristoranti, typequ == "A504")
}

# Lettura confini dei comuni francesi
comuni_fr <- st_read(geojson_path, quiet = TRUE)
comuni_fr$code <- as.character(comuni_fr$code)

# Selezione "sud della Francia" con bounding box approssimativa
# (metropolitana meridionale: sud-ovest + sud-est)
sud_bbox <- st_bbox(c(xmin = -1.8, ymin = 42.2, xmax = 7.8, ymax = 45.6), crs = st_crs(4326))
sud_geom <- st_as_sfc(sud_bbox)

comuni_sud <- comuni_fr[st_intersects(comuni_fr, sud_geom, sparse = FALSE), ]

ristoranti_geo <- ristoranti %>%
  group_by(commune_code) %>%
  summarise(nb_ristoranti = sum(nb_equip, na.rm = TRUE), .groups = "drop")

comuni_sud_rist <- comuni_sud %>%
  left_join(ristoranti_geo, by = c("code" = "commune_code")) %>%
  filter(!is.na(nb_ristoranti), nb_ristoranti > 0)

if (nrow(comuni_sud_rist) == 0) {
  stop("Nessun ristorante trovato nel sud con i filtri correnti.")
}

centroidi <- st_point_on_surface(comuni_sud_rist)
coords <- st_coordinates(centroidi)

punti <- comuni_sud_rist %>%
  st_drop_geometry() %>%
  mutate(
    lon = coords[, "X"],
    lat = coords[, "Y"],
    n = pmax(1L, as.integer(round(nb_ristoranti)))
  ) %>%
  tidyr::uncount(weights = n)

# Mappa di densita'
p <- ggplot() +
  geom_sf(data = comuni_sud, fill = "grey95", color = "white", linewidth = 0.1) +
  stat_density_2d(
    data = punti,
    aes(x = lon, y = lat, fill = after_stat(level), alpha = after_stat(level)),
    geom = "polygon",
    contour = TRUE
  ) +
  scale_fill_viridis_c(option = "magma", name = "Densita'") +
  scale_alpha(range = c(0.15, 0.75), guide = "none") +
  coord_sf(
    xlim = c(sud_bbox["xmin"], sud_bbox["xmax"]),
    ylim = c(sud_bbox["ymin"], sud_bbox["ymax"]),
    expand = FALSE
  ) +
  labs(
    title = "Densita' dei ristoranti nel sud della Francia",
    subtitle = "Dati: R Graph Gallery (CSV) + France GeoJSON (comuni)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12)

output_path <- "mappa_densita_ristoranti_sud_francia.png"
ggsave(output_path, p, width = 10, height = 8, dpi = 300)

message("Script completato.")
message("CSV locale: ", csv_path)
message("GeoJSON locale: ", geojson_path)
message("Mappa salvata in: ", output_path)

