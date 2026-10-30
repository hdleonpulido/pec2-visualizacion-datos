# Genera las coordenadas paralelas de datasets::mtcars en formato SVG.

raiz_proyecto <- function() {
  actual <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (basename(actual) == "R") return(dirname(actual))
  if (dir.exists(file.path(actual, "R"))) return(actual)
  stop("Ejecuta el script desde la carpeta principal del proyecto o desde R/.", call. = FALSE)
}

raiz <- raiz_proyecto()
ruta_salida <- file.path(raiz, "assets", "parallel-coordinates.svg")
dir.create(dirname(ruta_salida), recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE) || !requireNamespace("GGally", quietly = TRUE) ||
    !requireNamespace("svglite", quietly = TRUE)) {
  stop("Instala ggplot2, dplyr, GGally y svglite antes de ejecutar el script.", call. = FALSE)
}

coches <- datasets::mtcars |>
  dplyr::mutate(cilindros = factor(cyl, levels = c(4, 6, 8), labels = c("4 cilindros", "6 cilindros", "8 cilindros")))

ejes <- c("mpg", "disp", "hp", "wt")
stopifnot(nrow(coches) == 32L, all(ejes %in% names(coches)), all(coches$cyl %in% c(4, 6, 8)))
indices_ejes <- match(ejes, names(coches))
stopifnot(!anyNA(indices_ejes))

grafico <- GGally::ggparcoord(
  coches,
  columns = indices_ejes,
  groupColumn = "cilindros",
  scale = "uniminmax",
  order = indices_ejes,
  alphaLines = 0.78,
  showPoints = FALSE
) +
  ggplot2::scale_colour_manual(
    values = c("4 cilindros" = "#006BA4", "6 cilindros" = "#C2187A", "8 cilindros" = "#F28E4B"),
    name = "Cilindros"
  ) +
  ggplot2::scale_x_discrete(labels = c(
    mpg = "Consumo\n(mpg)",
    disp = "Cilindrada\n(pulg³)",
    hp = "Potencia\n(hp)",
    wt = "Peso\n(1.000 lb)"
  )) +
  ggplot2::labs(
    title = "Perfiles de automóviles de Motor Trend",
    x = NULL,
    y = "Valor normalizado (0–1)"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(ruta_salida, grafico, width = 10, height = 6.5, device = svglite::svglite, bg = "white")
message("Generado: ", ruta_salida)

