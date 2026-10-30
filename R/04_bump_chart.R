# Genera el bump chart de LaLiga en formato SVG.

raiz_proyecto <- function() {
  actual <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (basename(actual) == "R") return(dirname(actual))
  if (dir.exists(file.path(actual, "R"))) return(actual)
  stop("Ejecuta el script desde la carpeta principal del proyecto o desde R/.", call. = FALSE)
}

raiz <- raiz_proyecto()
ruta_laliga <- file.path(raiz, "R", "laliga")
ruta_salida <- file.path(raiz, "assets", "bump-chart.svg")
dir.create(dirname(ruta_salida), recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(ruta_laliga)) {
  stop("No existe la carpeta de datos: ", ruta_laliga, call. = FALSE)
}
if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE) ||
    !requireNamespace("readr", quietly = TRUE) || !requireNamespace("ggbump", quietly = TRUE) ||
    !requireNamespace("svglite", quietly = TRUE)) {
  stop("Instala ggplot2, dplyr, readr, ggbump y svglite antes de ejecutar el script.", call. = FALSE)
}
suppressPackageStartupMessages(library(dplyr))

temporadas <- tibble::tribble(
  ~archivo, ~season, ~season_index,
  "2020-2021.csv", "2020/21", 1L,
  "2021-2022.csv", "2021/22", 2L,
  "2022-2023.csv", "2022/23", 3L,
  "2023-2024.csv", "2023/24", 4L,
  "2024-2025.csv", "2024/25", 5L
)

diccionario <- tibble::tribble(
  ~team_name, ~team,
  "Alaves", "Deportivo Alavés", "Almeria", "Almería",
  "Ath Bilbao", "Athletic Club", "Ath Madrid", "Atlético de Madrid",
  "Barcelona", "FC Barcelona", "Betis", "Real Betis",
  "Cadiz", "Cádiz", "Celta", "RC Celta", "Eibar", "SD Eibar",
  "Elche", "Elche CF", "Espanol", "Espanyol", "Getafe", "Getafe CF",
  "Girona", "Girona FC", "Granada", "Granada CF", "Huesca", "SD Huesca",
  "Las Palmas", "UD Las Palmas", "Leganes", "CD Leganés",
  "Levante", "Levante UD", "Mallorca", "RCD Mallorca",
  "Osasuna", "CA Osasuna", "Real Madrid", "Real Madrid",
  "Sevilla", "Sevilla FC", "Sociedad", "Real Sociedad",
  "Valencia", "Valencia CF", "Valladolid", "Real Valladolid",
  "Vallecano", "Rayo Vallecano", "Villarreal", "Villarreal CF"
)

stopifnot(
  all(file.exists(file.path(ruta_laliga, temporadas$archivo))),
  !anyDuplicated(diccionario$team_name),
  !anyDuplicated(diccionario$team)
)

clasificacion <- dplyr::bind_rows(lapply(seq_len(nrow(temporadas)), function(i) {
  readr::read_csv(file.path(ruta_laliga, temporadas$archivo[[i]]), show_col_types = FALSE) |>
    dplyr::mutate(season = temporadas$season[[i]], season_index = temporadas$season_index[[i]])
})) |>
  dplyr::select(season, season_index, position, team_name) |>
  dplyr::left_join(diccionario, by = "team_name") |>
  dplyr::arrange(season_index, position)

stopifnot(
  nrow(clasificacion) == 20L * nrow(temporadas),
  all(!is.na(clasificacion$team)),
  all(clasificacion$position %in% 1:20),
  !anyDuplicated(clasificacion[c("season", "position")]),
  !anyDuplicated(clasificacion[c("season", "team")])
)

datos_top10 <- clasificacion |>
  dplyr::group_by(team) |>
  dplyr::arrange(season_index, .by_group = TRUE) |>
  dplyr::mutate(
    en_top10 = position <= 10L,
    consecutiva = season_index == dplyr::lag(season_index) + 1L,
    en_top10_anterior = dplyr::lag(en_top10, default = FALSE) & consecutiva,
    inicia_top10 = en_top10 & !en_top10_anterior,
    sale_top10 = !en_top10 & en_top10_anterior,
    tramo = cumsum(inicia_top10),
    mostrar = en_top10 | sale_top10,
    posicion_mostrada = dplyr::if_else(en_top10, position, 11L),
    grupo_tramo = paste(team, tramo, sep = "__")
  ) |>
  dplyr::filter(mostrar) |>
  dplyr::ungroup()

stopifnot(
  nrow(datos_top10) > 0,
  all(datos_top10$posicion_mostrada %in% 1:11),
  all(datos_top10$posicion_mostrada[datos_top10$sale_top10] == 11L)
)

# Colores identificativos de los clubes para facilitar el seguimiento visual.
paleta <- c(
  "Deportivo Alavés" = "#005CA9", "Almería" = "#E30613",
  "Athletic Club" = "#E9002B", "Atlético de Madrid" = "#B5173B",
  "FC Barcelona" = "#A50044", "Real Betis" = "#00954C",
  "Cádiz" = "#D9A300", "RC Celta" = "#2389C9",
  "SD Eibar" = "#243E90", "Elche CF" = "#0A7D39",
  "Espanyol" = "#007AC2", "Getafe CF" = "#00539F",
  "Girona FC" = "#D42E3D", "Granada CF" = "#7D1536",
  "SD Huesca" = "#104B9B", "UD Las Palmas" = "#C99A00",
  "CD Leganés" = "#0062A4", "Levante UD" = "#004A99",
  "RCD Mallorca" = "#D40032", "CA Osasuna" = "#A71930",
  "Real Madrid" = "#5D8AA8", "Sevilla FC" = "#C8102E",
  "Real Sociedad" = "#0067B1", "Valencia CF" = "#E97817",
  "Real Valladolid" = "#5B2C83", "Rayo Vallecano" = "#E31B23",
  "Villarreal CF" = "#C9A000"
)
stopifnot(setequal(names(paleta), unique(clasificacion$team)))
paleta_mostrada <- paleta[sort(unique(datos_top10$team))]
salidas_top10 <- dplyr::filter(datos_top10, sale_top10)
etiquetas_finales <- dplyr::filter(
  datos_top10,
  season_index == max(season_index) & en_top10
)
equipos_resto <- setdiff(sort(unique(datos_top10$team)), sort(unique(etiquetas_finales$team)))
stopifnot(length(equipos_resto) > 0)

grafico <- ggplot2::ggplot(
  datos_top10,
  ggplot2::aes(season_index, posicion_mostrada, colour = team, group = grupo_tramo)
) +
  ggbump::geom_bump(linewidth = 1.25, smooth = 8) +
  ggplot2::geom_point(
    size = 2.35,
    show.legend = FALSE
  ) +
  ggplot2::geom_text(
    data = etiquetas_finales,
    ggplot2::aes(label = team),
    hjust = -0.08,
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  ggplot2::scale_colour_manual(
    values = paleta_mostrada,
    name = "Equipos fuera del Top 10 en 2024/25",
    breaks = equipos_resto
  ) +
  ggplot2::scale_x_continuous(
    breaks = temporadas$season_index,
    labels = temporadas$season
  ) +
  ggplot2::scale_y_reverse(
    breaks = 1:11,
    labels = c(as.character(1:10), "Resto de equipos"),
    limits = c(11.5, 0.5)
  ) +
  ggplot2::coord_cartesian(xlim = c(0.9, max(temporadas$season_index) + 2.25), clip = "off") +
  ggplot2::labs(
    title = "Evolución de los equipos que han estado en el Top 10 de LaLiga",
    x = "Temporada",
    y = "Posición"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(colour = "#D8E0E6", linewidth = 0.35),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(face = "bold", size = 9),
    legend.text = ggplot2::element_text(size = 8.5),
    plot.margin = ggplot2::margin(10, 175, 10, 10)
  )

ggplot2::ggsave(ruta_salida, grafico, width = 13.5, height = 9, device = svglite::svglite, bg = "white")
message("Generado: ", ruta_salida)

