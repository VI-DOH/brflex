library(R6)

#' @export
FT_StatsMgr <- R6Class(
  classname = "FT_StatsMgr",

  private = list(
    stats_mgr = NULL,
    props_mgr = NULL,
    multi_yr = FALSE,
    years = NULL,
    use_first_factor = FALSE
  ),

  public = list(

    initialize = function(props_mgr = NULL, stats_mgr = NULL,
                          use_first_factor = FALSE) {

      if(!is.null(props_mgr) ) {

        if("FT_StatPropsMgr" %in% class(props_mgr)) {
          private$props_mgr <- props_mgr
        }
      }
      if(!is.null(stats_mgr) ) {

        if("StatsMgr" %in% class(stats_mgr)) {
          private$stats_mgr <- stats_mgr
        }
      }
    },

    ft = function(coi = NULL) {

      df_stats <- private$stats_mgr$survey_stats(coi = coi, reduce = FALSE)

      props_mgr <- private$props_mgr

      if(private$use_first_factor) {

        resp <- stats_mgr$survey_stats() %>% pull(response) %>% {.[1]}
        private$props_mgr$responses <- paste0("^", resp, "$")

      }

      ft_stats(df_stats = df_stats,
               coi = NULL,
               population = props_mgr$population,
               stats = props_mgr$stats,
               exclude =  props_mgr$exclude,
               responses = props_mgr$responses,
               rename = c(percent = "pct"),
               widths = props_mgr$widths,
               aligns = props_mgr$aligns,
               subset_placement = props_mgr$subset_placement,
               line_spacing = 1.0,
               title_spacing = 1.0,

               titles = props_mgr$titles,
               title_max_char = 9999,
               highlights = props_mgr$highlights,
               footers = NULL,
               footnotes = props_mgr$footnotes,

               borders = props_mgr$borders,

               bgs = props_mgr$bgs,
               bgs_mgr = props_mgr$bgs_mgr,

               fonts = props_mgr$fonts,
               fonts_mgr = props_mgr$fonts_mgr,

               paddings = props_mgr$paddings,

               box = props_mgr$box,
               grid = props_mgr$grid
      )
    }

  ),

#   ft_multi_year = function(cois = NULL, years = NULL) {
#
#     {
#       df_stats <- private$stats_mgr$multi_year_stats(cois, years)
#
#       df_stats <- private$stats_mgr$survey_stats(coi = coi, reduce = FALSE)
#     }
#
#     props_mgr <- private$props_mgr
#
#     if(private$use_first_factor) {
#
#       resp <- stats_mgr$survey_stats() %>% pull(response) %>% {.[1]}
#       private$props_mgr$responses <- paste0("^", resp, "$")
#
#     }
#
#     ft_stats(df_stats = df_stats,
#              coi = NULL,
#              population = props_mgr$population,
#              stats = props_mgr$stats,
#              exclude =  props_mgr$exclude,
#              responses = props_mgr$responses,
#              rename = c(percent = "pct"),
#              widths = props_mgr$widths,
#              aligns = props_mgr$aligns,
#              subset_placement = props_mgr$subset_placement,
#              line_spacing = 1.0,
#              title_spacing = 1.0,
#
#              titles = props_mgr$titles,
#              title_max_char = 9999,
#              highlights = props_mgr$highlights,
#              footers = NULL,
#              footnotes = props_mgr$footnotes,
#
#              borders = props_mgr$borders,
#
#              bgs = props_mgr$bgs,
#              bgs_mgr = props_mgr$bgs_mgr,
#
#              fonts = props_mgr$fonts,
#              fonts_mgr = props_mgr$fonts_mgr,
#
#              paddings = props_mgr$paddings,
#
#              box = props_mgr$box,
#              grid = props_mgr$grid
#     )
#   }
#
# ),

  active = list(

    data = function(value) {

      if(missing(value)) return(private$df_stats)

      if("brfss_stats" %in% class(value)) {
        private$df_stats <- value
      }

    },

    first_response = function(value) {

      if(missing(value)) return(private$use_first_factor)

      if(!class(value) == "logical") return(NULL)

      private$use_first_factor <-value
    }
  )
)
