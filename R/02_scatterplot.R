# Genera el scatterplot de Palmer Penguins en formato SVG.

raiz_proyecto <- function() {
  actual <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (basename(actual) == "R") return(dirname(actual))
  if (dir.exists(file.path(actual, "R"))) return(actual)
  stop("Ejecuta el script desde la carpeta principal del proyecto o desde R/.", call. = FALSE)
}

raiz <- raiz_proyecto()
ruta_datos <- file.path(raiz, "R", "penguins.csv")
ruta_salida <- file.path(raiz, "assets", "scatterplot.svg")
dir.create(dirname(ruta_salida), recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("readr", quietly = TRUE) ||
    !requireNamespace("dplyr", quietly = TRUE) || !requireNamespace("svglite", quietly = TRUE)) {
  stop("Instala ggplot2, readr, dplyr y svglite antes de ejecutar el script.", call. = FALSE)
}

pinguinos <- readr::read_csv(ruta_datos, show_col_types = FALSE) |>
  dplyr::filter(!is.na(flipper_length_mm), !is.na(body_mass_g)) |>
  dplyr::mutate(species = factor(species, levels = c("Adelie", "Chinstrap", "Gentoo")))

stopifnot(nrow(pinguinos) > 0, all(c("species", "flipper_length_mm", "body_mass_g") %in% names(pinguinos)))

grafico <- ggplot2::ggplot(
  pinguinos,
  ggplot2::aes(flipper_length_mm, body_mass_g, colour = species, shape = species)
) +
  ggplot2::geom_point(alpha = 0.7, size = 2.6) +
  ggplot2::scale_colour_manual(
    values = c(Adelie = "#0072B2", Chinstrap = "#D55E00", Gentoo = "#009E73"),
    name = "Especie"
  ) +
  ggplot2::scale_shape_manual(
    values = c(Adelie = 16, Chinstrap = 17, Gentoo = 15),
    name = "Especie"
  ) +
  ggplot2::labs(
    title = "Masa corporal y longitud de aleta en pingüinos Palmer",
    x = "Longitud de la aleta (mm)",
    y = "Masa corporal (g)"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(ruta_salida, grafico, width = 10, height = 6.5, device = svglite::svglite, bg = "white")
message("Generado: ", ruta_salida)

