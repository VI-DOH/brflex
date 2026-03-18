library(R6)

#' @export
FT_StatsMgr <- R6Class(
  classname = "FT_StatsMgr",

  private = list(
    stats_mgr_pvt = NULL,
    props_mgr_pvt = NULL,
    multi_yr = FALSE,
    years = NULL,
    use_first_factor = FALSE
  ),

  public = list(

    initialize = function(props_mgr = NULL, stats_mgr = NULL,
                          use_first_factor = FALSE) {

      if(!is.null(props_mgr) ) {

        if("FT_StatPropsMgr" %in% class(props_mgr)) {
          private$props_mgr_pvt <- props_mgr
        }
      } else {

        private$props_mgr_pvt <-FT_DefaultStatPropsMgr$new()

      }

      if(!is.null(stats_mgr) ) {

        if("StatsMgr" %in% class(stats_mgr)) {
          private$stats_mgr_pvt <- stats_mgr
        }
      } else {
        private$stats_mgr_pvt <- brfss::StatsMgr$new()
      }
    },

    ft = function(coi = NULL, df_stats = NULL) {

      props_mgr <- private$props_mgr_pvt
      stats_mgr <- private$stats_mgr_pvt

      if(is.null(df_stats)) df_stats <- stats_mgr$survey_stats(coi = coi, reduce = FALSE)

      if(is.null(df_stats)) return(NULL)

      if(private$use_first_factor) {

        resp <- stats_mgr$survey_stats() %>% pull(response) %>% {.[1]}
        props_mgr$responses <- paste0("^", resp, "$")

      }


      df_stats <- df_stats %>% mutate(suppress = FALSE)

      ft_stats(df_stats = df_stats,
               coi = NULL,
               population = props_mgr$population,
               stats = stats_mgr$stats,
               exclude =  props_mgr$exclude,
               responses = props_mgr$responses,
               rename = props_mgr$renames,
               widths = props_mgr$widths,
               aligns = props_mgr$aligns,
               digits = props_mgr$digits,
               subset_placement = props_mgr$subset_placement,
               line_spacing = 1.0,
               title_spacing = 1.0,

               titles = props_mgr$titles,
               title_max_char = 9999,
               highlights = props_mgr$highlights,
               highlights_mgr = props_mgr$highlights_mgr,
               footers = NULL,
               footnotes = props_mgr$footnotes,

               borders_mgr = props_mgr$borders_mgr,
               bgs_mgr = props_mgr$bgs_mgr,
               fonts_mgr = props_mgr$fonts_mgr,

               paddings = props_mgr$paddings,

               box = props_mgr$box,
               grid = props_mgr$grid
      )
    }

  ),

  active = list(

    year = function(value) {

      if(missing(value)) {
        return(self$stats_mgr$data_mgr$dataset_mgr$get(year))
      } else {
        if(is.numeric(value)) {
          self$stats_mgr$data_mgr$dataset_mgr$set(year = value)
        }
      }
    },

    data = function(value) {

      if(missing(value)) return(private$df_stats)

      if("brfss_stats" %in% class(value)) {
        private$df_stats <- value
      }

    },

    stats_mgr  = function(value) {

      if(missing(value)) return(private$stats_mgr_pvt)

      if(inherits(value, "StatsMgr")) {
        private$stats_mgr_pvt <- value
      }

    },

    props_mgr   = function(value) {

      if(missing(value)) return(private$props_mgr_pvt)

      if(inherits(value, "FT_StatPropsMgr")) {
        private$props_mgr_pvt <- value
      }

    },

    first_response = function(value) {

      if(missing(value)) return(private$use_first_factor)

      if(!class(value) == "logical") return(NULL)

      private$use_first_factor <-value
    }
  )
)


#' @export
FT_DefaultStatsMgr <- R6Class(
  classname = "FT_DefaultStatsMgr",
  inherit = FT_StatsMgr,

  public = list(

    initialize = function() {

      stats_mgr <- brfss::StatsMgr$new()
      super$initialize(props_mgr = FT_DefaultStatPropsMgr$new(),
                       stats_mgr = stats_mgr)

      stats_mgr$subvars <- c("Sex", "Race")

    }
  )

)

