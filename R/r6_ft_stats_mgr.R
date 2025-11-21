library(R6)

FT_StatsMgr <- R6Class(
  classname = "FT_StatsMgr",

  private = list(
    df_stats = NULL,
    props = NULL
  ),

  public = list(

    initialize = function(props) {

      if(!is.null(props) ) {

        if(class(value) == "FT_StatPropsMgr") {
          private$props <- props
        }
      }
    },

    ft = function() {

      ft_stats(df_stats = df_stats,
               coi = props$coi,
               population = NULL,
               stats = c("den", "num", "percent", "ci"),
               exclude = "^$",
               responses = ".*",
               rename = c(percent = "pct"),
               widths = c(subset = 3, ci = 1),
               align = c(ci = "center"),
               subset_placement = "left",
               subset_sep = flextable::fp_border_default(),
               subvar_sep = flextable::fp_border_default(),
               denom_sep = flextable::fp_border_default(),
               data_sep = flextable::fp_border_default(),
               response_sep = flextable::fp_border_default(),
               stats_sep = flextable::fp_border_default(),
               line_spacing = 1.0,
               title_spacing = 1.0,

               header = NULL,
               titles = NULL,
               title_max_char = 9999,
               highlights = NULL,
               footers = NULL,
               footnotes = TRUE,

               borders = ft_borders_list(),

               bgs = list(),

               fonts = list(),

               paddings = list(),

               box = NULL,
               grid = NULL
               )
    }

  ),

  active = list(

    stats = function(value) {

      if(missing(value)) return(private$df_stats)

      if("brfss_stats" %in% class(value)) {
        private$df_stats <- value
      }

    },

    borders = function(value) {

      if(missing(value)) return(private$borders_pvt)

      if(class(value) != "ft_borders_list") return(NULL)

      private$borders_pvt <- value
    }
  )
)
