library(R6)

#' @export
FT_StatsMgr <- R6Class(
  classname = "FT_StatsMgr",

  private = list(
    stats_mgr_pvt = NULL,
    props_mgr_pvt = NULL,
    multi_yr = FALSE,
    years = NULL,
    use_first_factor = FALSE,
    suppress_pvt = FALSE,
    suppress_if_pvt =  c(low_num = "num < 6", high_cv = "cv > 30"),

    add_suppression = function(df, include_why = FALSE) {

      rule_exprs <- purrr::map(private$suppress_if_pvt, rlang::parse_expr)

      df$suppress_reason <- purrr::pmap_chr(
        df,
        function(...) {
          row <- list(...)
          hits <- names(rule_exprs)[
            purrr::map_lgl(rule_exprs, ~ rlang::eval_tidy(.x, data = row))
          ]
          if (length(hits) == 0) NA_character_ else paste(hits, collapse = "; ")
        }
      )

      df$suppress <- !is.na(df$suppress_reason)

      if(!include_why) df <- df %>% select(-suppress_reason)

      df
    }
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
      }
    },

    ft = function(coi = NULL, df_stats = NULL, suppress = NULL) {

      props_mgr <- private$props_mgr_pvt
      stats_mgr <- private$stats_mgr_pvt

      if(is.null(suppress)) suppress <- private$suppress_pvt

      if(is.null(df_stats)) df_stats <- stats_mgr$survey_stats(coi = coi, reduce = FALSE)

      if(private$use_first_factor) {

        resp <- stats_mgr$survey_stats() %>% pull(response) %>% {.[1]}
        props_mgr$responses <- paste0("^", resp, "$")

      }

      if(suppress) {
        df_stats <- df_stats %>% private$add_suppression(include_why = FALSE)
      } else {
        df_stats <- df_stats %>% mutate(suppress = FALSE)
      }

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

    suppress_if = function(value) {

      if(missing(value)) return(private$suppress_if_pvt)

      if(!class(value) == "character") return(NULL)

      private$suppress_if_pvt <-value
    },

    suppress = function(value) {

      if(missing(value)) return(private$suppress_pvt)

      if(!class(value) == "logical") return(NULL)

      private$suppress_pvt <-value
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

