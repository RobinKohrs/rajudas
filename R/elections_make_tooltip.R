#' Make Tooltip for DW Chart
#' @description
#' Creates the tooltip-code for a vertical bar chart. Importantly!! The 'colname'-key in each list entry must match the column-name of the dataframe which holds the percentages.
#' The 'dw_name'-entry muss be how Datawrapper would write this column. Mostly lower case and connected with lowerstring (_).
#'
#' @param df The dataframe with the percetage (!importannt as the bar widths will be computed using this)
#' @param color_mapping A list where each entry is a list containing the information on each column you want in the tooltip. \code{colname} = The name in the \code{df}. \code{color} = The color of the bar
#' \code{dw_name} = Must match the naming-convention of Datawrapper
#' @param barHeight The bar height (a css string)
#' @param fontSize The fontsize of the party labels (a css string)
#' @param bg_color The bg color of each bar
#'
#' @return
#' @export
#'
#' @examples
election_make_tooltips = function(df,
                                  color_mapping = list(
                                    list(
                                      "colname" = "ÖVP",
                                      "color" = "#62c3d0",
                                      dw_name = "vp"
                                    ),
                                    list(
                                      "colname" = "SPÖ",
                                      "color" = "#ff6363",
                                      dw_name = "sp"
                                    ),
                                    list(
                                      "colname" = "FPÖ",
                                      "color" = "#0066b3",
                                      dw_name = "fp"
                                    ),
                                    list(
                                      "colname" = "GRÜNE",
                                      "color" = "#82ae47",
                                      dw_name = "grne"
                                    ),
                                    list(
                                      "colname" = "NEOS",
                                      "color" = "#e84188",
                                      dw_name = "neos"
                                    ),
                                    list(
                                      "colname" = "DNA",
                                      "color" = "#bf0000",
                                      dw_name = "dna"
                                    ),
                                    list(
                                      "colname" = "KPÖ",
                                      "color" = "#bf0000",
                                      dw_name = "kp"
                                    )
                                  ),
                                  barHeight = "10px",
                                  fontSize = ".7rem",
                                  bg_color = "#f0eded") {
  # open the grid
  grid_open = "<span style='display: grid; grid-template-columns: auto 3fr; gap: 0.15rem; min-width: 200px; align-items: center;'>"

  grid_inner = map(color_mapping, function(ent) {
    cell = glue(
      "<span style='font-size: {fontSize}; font-weight: bold; overflow: hidden; text-overflow: ellipsis;'>{ent$colname}</span>
     <span style='display: inline-block; width: 100%; height: {barHeight}; background: {bg_color}; position: relative;'>
      <span style='position: absolute; height: 100%; width: {{{{{ent$dw_name}}}}}%; background-color: {ent$color};'> </span>
      <span style='position: absolute; top: 50%; transform: translateY(-50%); left: calc({{{{{ent$dw_name}}}}}% + 2px);'>{{{{FORMAT({ent$dw_name}, '0.0%')}}}}</span>
    </span>"
    )

  }) %>% paste0(collapse = "")

  grid_close = "</span>"

  # tt
  tooltip = glue("{grid_open}{grid_inner}{grid_close}")

  return(tooltip)
}
