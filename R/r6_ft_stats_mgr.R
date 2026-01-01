library(R6)

#' @export
FT_StatsMgr <- R6Class(
  classname = "FT_StatsMgr",

  private = list(
    stats_mgr = NULL,
    props = NULL,
    use_first_factor = FALSE
  ),

  public = list(

    initialize = function(props = NULL, stats_mgr = NULL, use_first_factor = FALSE) {

      if(!is.null(props) ) {

        if("FT_StatProps" %in% class(props)) {
          private$props <- props
        }
      }
      if(!is.null(props) ) {

        if("StatsMgr" %in% class(stats_mgr)) {
          private$stats_mgr <- stats_mgr
        }
      }
    },

    ft = function(coi = NULL) {

      df_stats <- private$stats_mgr$survey_stats(coi = coi)

      props <- private$props

      if(private$use_first_factor) {

        resp <- stats_mgr$survey_stats() %>% pull(response) %>% {.[1]}
        private$props$responses <- paste0("^", resp, "$")

      }

      ft_stats(df_stats = df_stats,
               coi = NULL,
               population = props$population,
               stats = props$stats,
               exclude =  props$exclude,
               responses = props$responses,
               rename = c(percent = "pct"),
               widths = props$widths,
               aligns = props$aligns,
               subset_placement = props$subset_placement,
               line_spacing = 1.0,
               title_spacing = 1.0,

               titles = props$titles,
               title_max_char = 9999,
               highlights = props$highlights,
               footers = NULL,
               footnotes = props$footnotes,

               borders = props$borders,

               bgs = props$bgs,

               fonts = props$fonts,

               paddings = props$paddings,

               box = props$box,
               grid = props$grid
               )
    }

  ),

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
