library(R6)

FT_StatPropsMgr <- R6Class(
  classname = "FT_StatPropsMgr",

  private = list(

    coi_pvt = NULL,
    population_pvt = NULL,
    stats_pvt = c("den", "num", "percent", "ci"),
    exclude_pvt = "^$",
    responses_pvt = ".*",
    rename_pvt = c(percent = "pct"),
    widths_pvt = c(subset = 3, ci = 1),
    align_pvt = c(ci_pvt = "center"),
    subset_placement_pvt = "left",
    subset_sep_pvt = flextable::fp_border_default(),
    subvar_sep_pvt = flextable::fp_border_default(),
    denom_sep_pvt = flextable::fp_border_default(),
    data_sep_pvt = flextable::fp_border_default(),
    response_sep_pvt = flextable::fp_border_default(),
    stats_sep_pvt = flextable::fp_border_default(),
    line_spacing_pvt = 1.0,
    title_spacing_pvt = 1.0,

    header_pvt = NULL,
    titles_pvt = NULL,
    title_max_char_pvt = 9999,
    highlights_pvt = NULL,
    footers_pvt = NULL,
    footnotes_pvt = TRUE,

    borders_pvt = ft_borders_list(),

    bgs_pvt = list(),

    fonts_pvt = list(),

    paddings_pvt = list(),

    box_pvt = NULL,
    grid_pvt = NULL
  ),

  public = list(

    initialize = function() {

    }

  ),

  active = list(

    borders = function(value) {

      if(missing(value)) return(private$borders_pvt)

      if(class(value) != "ft_borders_list") return(NULL)

      private$borders_pvt <- value
    }
  )
)
