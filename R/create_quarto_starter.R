#' Createa a quarto document for that project
#'
#' @param dir The directory where the `quarto` folder should be created
#' @param title The Title in the quarto document
#' @param filename The filename
#'
#' @importFrom here here
#' @importFrom glue glue
#'
#' @return nothing. Creates just some files
#' @export
#'
#' @examples
#' create_quarto_starter(title = "Wiener Linien Störungen", filename="wienerLinienEda")
create_quarto_starter = function(dir = here(),
                                title = "test",
                                filename = "eda") {

  # quarto file -------------------------------------------------------------
  quarto_header =
    glue(
      "
      ---
      title: '{{title}}'
      format:
        html:
          toc: true
          code-fold: true
          css: style.css
      execute:
        warning: false
      ---

      ```{r}
      #| message: false
      #| warning: false

      #packages
      library(tidyverse)
      library(here)
      library(glue)
      library(sf)
      library(rajudas)
      library(jsonlite)
      ```
      ",
      .open = "{{"
    )

  d = here::here(dir, "quarto")
  quarto_filename = rajudas::makePath(here::here(d, glue("{filename}.qmd")))
  writeLines(quarto_header, con = quarto_filename)

  # css file -------------------------------------------------------------
  css_filename = rajudas::makePath(here::here(d, glue("style.css")))
  file.create(css_filename)

  cli::cli_h2(glue("Created quarto and css document in:\n{d}"))

}
