
#' @export
FT_SectionMgr <-
  R6::R6Class(
    classname = "FT_SectionMgr",

    private = list(

      ft = NULL

    ),

    public = list(

      initialize = function(ft = NULL) {

        private$ft <- ft
      }


    ),

    active = list(

      table_rc = function(value) {

        if(!missing(value)) return(NULL)

        parts <- c("header", "body", "footer")

        purrr::map(parts, \(part) {

          list(rows = 1:nrow_part(private$ft, part),
               cols =  1:length(private$ft$col_keys),
               part = part
          )
        })
      },

      header_rc = function(value) {

        if(!missing(value)) return(NULL)

        list(rows = 1:private$ft$header$content$nrow,
             cols = 1:length(private$ft$col_keys),
             part = "header"
        )
      },

      footer_rc = function(value) {

        if(!missing(value)) return(NULL)

        list(rows = 1:nrow_part(private$ft, "footer"),
             cols =  1:length(private$ft$col_keys),
             part = "footer"
        )
      },

      titles_rc = function(value) {

        if(!missing(value)) return(NULL)

        list(rows = titles_rows(private$ft),
             cols = 1:length(private$ft$col_keys),
             part = "header"
        )
      },


      responses_rc = function(value) {

        if(!missing(value)) return(NULL)

        cols <- response_cols(private$ft)

        cols <- purrr::map2(cols$min, cols$max,\(min, max) min:max) %>% unlist()

        list(rows = responses_rows(private$ft),
             cols = cols,
             part = "header")

      },


      stats_rc = function(value) {

        if(!missing(value)) return(NULL)

        list(
          rows = stats_rows(private$ft),
          cols = 1:length(private$ft$body$col_keys),
          part = "header"
        )

      },


      subsets_rc = function(value) {

        if(!missing(value)) return(NULL)

        rows <- subvar_rows(private$ft)

        rc <- purrr::map2(rows$min, rows$max,\(min, max) {
          list(rows = min:max,
               cols = 2 - (private$ft$properties$sub_placement == "top"),
               part = "body")

        })

        rc
      },

      subvars_rc = function(value) {

        if(!missing(value)) return(NULL)

        rows <- subvar_rows(private$ft)

        if(private$ft$properties$sub_placement == "top") {

          rows <- rows %>% tail(-1)

          rc <- purrr::map2(rows$min, rows$max,\(min, max) {
            list(rows = min-1,
                 cols = 1:length(private$ft$col_keys),
                 part = "body")

          })

        } else {

          rc <- purrr::map2(rows$min, rows$max,\(min, max) {
            list(rows = min:max,
                 cols = 1,
                 part = "body")

          })
        }


        rc
      },


      data_rc = function(value) {

        if(!missing(value)) return(NULL)

        rows <- subvar_rows(private$ft)

        rc <- purrr::map2(rows$min, rows$max,\(min, max) {

          offset <- (private$ft$properties$sub_placement == "top")
          list(rows = min:max,
               cols = (3 - offset):length(private$ft$col_keys),
               part = "body")

        })

        rc
      },

      titles_rows = function(value) {

        if(!missing(value)) return(NULL)

        1:(private$ft$header$sections$nrow$titles)

      },

      responses_rows = function(value) {

        if(!missing(value)) return(NULL)

        irow <- private$ft$header$sections$nrow$titles + 1
        irow:(irow + private$ft$header$sections$nrow$responses - 1)

      }
    )

  )


makeActiveBinding(
  "names",
  function() {
    c("table", "header", "titles", "years", "responses", "stats",
      "data", "subsets", "subvars", "footer", "footnotes")
  },
  env = FT_SectionMgr
)




