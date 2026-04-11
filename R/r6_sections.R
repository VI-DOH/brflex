

FlexSection <-
  R6::R6Class(
    classname = "FlexSection",

    private = list(

      bg_pvt = NULL,
      font_pvt = NULL,
      borders_pvt = NULL



    ),

    public = list(

      # This is the "Abstract" method
      row6s = function(ft) {
        stop("Subclasses MUST implement a 'rows' method.")
      },

      cols = function(ft) {
        stop("Subclasses MUST implement a 'cols' method.")
      },

      rowcols = function(ft) {
        stop("Subclasses MUST implement a 'rowcols' method.")
      }


    ),

    active = list(

      bg = function(value) {

        if(missing(value)) return(private$bg_pvt)

        if(!inherits(value, "FT_Bg")) {

          message("value must be class FT_Bg")

          return()

        }

        private$bg_pvt <- value

      },

      font = function(value) {

        if(missing(value)) return(private$font_pvt)

        if(!inherits(value, "FT_Font")) {

          message("value must be class FT_Font")

          return()

        }

        private$font_pvt <- value

      },

      borders = function(value) {

        if(missing(value)) return(private$borders_pvt)

        if(!inherits(value, "FT_Border")) {

          message("value must be class FT_Border")

          return()

        }

        private$borders_pvt <- value

      }

    )
  )

TableFlexSection <-
  R6::R6Class(
    classname = "TableFlexSection",
    inherit = FlexSection,

    private = list(

    ),

    public = list(

      rows = function(ft) {
        NULL

      },

      cols = function(ft) {

        NULL

      },

      # This is the "Abstract" method
      rowcols = function(ft) {

        list(rows = self$rows(ft),
             cols = self$cols(ft),
             part = "all"
        )
      }


    ),

    active = list(

    )
  )

HeaderFlexSection <-
  R6::R6Class(
    classname = "HeaderFlexSection",
    inherit = FlexSection,

    private = list(

    ),

    public = list(

      rows = function(ft) {

        NULL

      },

      cols = function(ft) {

        NULL

      },

      # This is the "Abstract" method
      rowcols = function(ft) {

        list(rows = self$rows(ft),
             cols = self$cols(ft),
             part = "header"
        )
      }


    ),

    active = list(




    )
  )

DataFlexSection <-
  R6::R6Class(
    classname = "DataFlexSection",
    inherit = FlexSection,

    private = list(

    ),

    public = list(

      rows = function(ft) {

        NULL

      },

      cols = function(ft) {

        NULL

      },

      # This is the "Abstract" method
      rowcols = function(ft) {

        list(rows = self$rows(ft),
             cols = self$cols(ft),
             part = "body"
        )
      }


    ),

    active = list(




    )
  )

FooterFlexSection <-
  R6::R6Class(
    classname = "FooterFlexSection",
    inherit = FlexSection,

    private = list(

    ),

    public = list(

      rows = function(ft) {

        NULL

      },

      cols = function(ft) {

        NULL

      },

      # This is the "Abstract" method
      rowcols = function(ft) {

        list(rows = self$rows(ft),
             cols = self$cols(ft),
             part = "footer"
        )
      }


    ),

    active = list(




    )
  )


TitlesFlexSection <-
  R6::R6Class(
    classname = "TitlesFlexSection",
    inherit = FlexSection,

    private = list(

    ),

    public = list(

      rows = function(ft) {

        ft$header$sections$nrow$titles

      },

      cols = function(ft) {

        1:length(ft$col_keys)

      },

      # This is the "Abstract" method
      rowcols = function(ft) {

        list(rows = self$rows(ft),
             cols = self$cols(ft),
             part = "header"
        )
      }


    ),

    active = list(




    )
  )

ResponsesFlexSection <-
  R6::R6Class(
    classname = "ResponsesFlexSection",
    inherit = FlexSection,

    private = list(

      response_cols = function(ft) {

        irows <- self$rows(ft)

        resp <- ft$header$dataset[irows,] %>% as.character()

        df <- data.frame(jcol =1:length(ft$header$col_keys), resp = resp)

        resps <-resp %>% unique()

        grp <- resp %>% purrr::map_int(\(r) {
          which(r == resps)
        })

        df <- df %>% mutate(grp = grp)

        df %>%
          summarise(min = min(jcol), max = max(jcol), .by = c(resp, grp)) %>%
          filter(resp != "") %>%
          arrange(min) %>% ungroup()


      }

    ),

    public = list(

      rows = function(ft) {

        ft$header$sections$nrow$titles + ft$header$sections$nrow$years + 1

      },

      cols = function(ft) {

        cols <- private$response_cols(ft)

        purrr::map2(cols$min, cols$max,\(min, max) min:max) %>% unlist()

      },

      # This is the "Abstract" method
      rowcols = function(ft) {

        list(rows = self$rows(ft),
             cols = self$cols(ft),
             part = "header"
        )
      }


    ),

    active = list(




    )
  )

StatsFlexSection <-
  R6::R6Class(
    classname = "StatsFlexSection",
    inherit = FlexSection,

    private = list(

    ),

    public = list(

      rows = function(ft) {

        ft$header$sections$nrow$stats

      },

      cols = function(ft) {

        1:length(ft$col_keys)

      },

      # This is the "Abstract" method
      rowcols = function(ft) {

        list(rows = self$rows(ft),
             cols = self$cols(ft),
             part = "header"
        )
      }


    ),

    active = list(




    )
  )

TitlesFlexSection <-
  R6::R6Class(
    classname = "TitlesFlexSection",
    inherit = FlexSection,

    private = list(

    ),

    public = list(

      rows = function(ft) {

        ft$header$sections$nrow$titles

      },

      cols = function(ft) {

        1:length(ft$col_keys)

      },

      # This is the "Abstract" method
      rowcols = function(ft) {

        list(rows = self$rows(ft),
             cols = self$cols(ft),
             part = "header"
        )
      }


    ),

    active = list(




    )
  )

SubvarsFlexSection <-
  R6::R6Class(
    classname = "SubvarsFlexSection",
    inherit = FlexSection,

    private = list(

    ),

    public = list(

      rows = function(ft) {

        ft$header$sections$nrow$titles

      },

      cols = function(ft) {

        1:length(ft$col_keys)

      },

      # This is the "Abstract" method
      rowcols = function(ft) {

        list(rows = self$rows(ft),
             cols = self$cols(ft),
             part = "body"
        )
      }


    ),

    active = list(




    )
  )

SubsetsFlexSection <-
  R6::R6Class(
    classname = "SubsetsFlexSection",
    inherit = FlexSection,

    private = list(

    ),

    public = list(

      rows = function(ft) {

        ft$header$sections$nrow$titles

      },

      cols = function(ft) {

        1:length(ft$col_keys)

      },

      # This is the "Abstract" method
      rowcols = function(ft) {

        list(rows = self$rows(ft),
             cols = self$cols(ft),
             part = "body"
        )
      }


    ),

    active = list(




    )
  )
