
#' @export
FT_BordersMgr <-
  R6::R6Class(
    classname = "FT_BordersMgr",

    private = list(

      border = list(
        table_pvt = NULL,
        data_pvt = NULL,
        header_pvt = NULL,
        responses_pvt = NULL,
        stats_pvt = NULL,
        subsets_pvt = NULL,
        subvars_pvt = NULL,
        titles_pvt = NULL,
        footnotes_pvt = NULL,
        footer_pvt = NULL
      ),

      set_active = function(value, area) {

        if(inherits(value, "FT_Color")) {
        } else if(inherits(value[[1]], "FT_Color")) {

          value <-value[[1]]

        } else if(inherits(value, "character") && FT_Color$is_valid(value)){
          value <- FT_Bg$new(value)
        } else  {
          message("value must be an FT_Color object or a character representation of a color")
          return(NULL)
        }

        private$border[[paste0(area, "_pvt")]] <- list(value)

      }

    ),

    public = list(

      initialize = function(table = NULL,
                            data = NULL,
                            header = NULL,
                            responses = NULL,
                            stats = NULL,
                            subsets = NULL,
                            subvars = NULL,
                            titles = NULL,
                            footnotes = NULL,
                            footer = NULL) {


        args <- as.list(match.call()) %>% tail(-1) %>% names()

        purrr::walk(args, \(arg) {


          value <- get(arg)

          if(inherits(value, FT_RangeBorders)) {
            value <- list(value)
          } else {
            value <- NULL
          }

          private$border[[paste0(arg, "_pvt")]] <- value

        })




      },

      as.data.frame = function() {

        priv_list <- as.list(private$border)

        df <- purrr::imap(priv_list, \(value, nm){
          if(!is.null(value)) {

            purrr::map(value,\(border) {

              border$as.data.frame()
            }) %>%
              bind_rows() %>%
              mutate(area = gsub("_pvt","",nm)) %>%
              mutate(index = row_number()) %>%
              relocate(area, index)
          }
        }
        ) %>% bind_rows()

        df
      },

      print = function() {


        print(self$as.data.frame())


      },

      n_borders = function(area) {

        length(private$border[[paste0(area, "_pvt")]])
      },

      add_border = function(border, area) {

        if (!inherits(border, "FT_Bg")) {

          if(!inherits(border, "FT_Bg")) {

            message("border must be of class FT_Bg")
            return(NULL)
          }
        }

        n <- self$n_borders(area)
        if(n == 0) private$border[[paste0(area, "_pvt")]] <- list()

        private$border[[paste0(area, "_pvt")]][[n + 1]] <- border
      },

      apply = function(ft) {

        has_subvar_col <- "subvar" %in% ft$col_keys

        if(has_subvar_col) {
          jcols_subvars <- 1
          irows_subvars <- NULL

          jcols_subsets <- 2
          irows_subsets <- NULL

        } else {

          rowspans <- ft$body$spans$rows

          if(!is.null(dim(rowspans))) {

            jcols_subvars <- 1
            irows_subvars <- which(ft$body$spans$rows[,1] > 1)

            jcols_subsets <- 1
            irows_subsets <- which(ft$body$spans$rows[,1] == 1)
          }else {
            jcols_subvars <- 1
            irows_subvars <- 1

            jcols_subsets <- 0
            irows_subsets <- 0
          }
        }

        nrow_header <- ft$header$content$nrow
        nrow_titles <- nrow_header - 2

        #handle_borders_header(borders)
        # handle_borders_response(borders)%>%
        #   handle_borders_stats(borders) %>%
        #self$apply_titles(ft)


        ft <- ft %>%
          self$apply_area("titles", i = 1:nrow_titles, part = "header") %>%
          self$apply_area("responses", i = nrow_header - 1, part = "header") %>%
          self$apply_area("stats", i = nrow_header, part = "header")  %>%
          self$apply_area("data", part = "body")%>%
          self$apply_area("footnotes",  part = "footer") %>%
          self$apply_area("subvars", i = irows_subvars, j = jcols_subvars, part = "body")

        if(irows_subsets[1] > 0) ft <- ft %>%
          self$apply_area("subsets", i = irows_subsets, j = jcols_subsets, part = "body")

        ft

      },

      apply_area = function(ft, area, i = NULL, j = NULL, part) {

        borders <- private$border[[paste0(area,"_pvt")]]

        if(is.null(borders)) return(ft)

        if(is.null(i)) i <- 1:(ft[[part]]$content$nrow)

        nrow <- length(i)

        nborders <- self$n_borders(area)

        if(nrow != nborders) {
          iborders <- rep(1,nrow)
        } else {
          iborders <- 1:nrow
        }


        purrr::walk2(i, iborders,\(irow, iborder) {
          #if(area == "responses") browser()

          border  <-  borders[[iborder]]$color

          ft <<- self$apply_border(ft, i = irow, j = j,
                                   border = border, part = part)
        })
        ft
      },

      apply_border = function(ft, i = NULL, j = NULL, border , part = "header") {

        ft %>%
          flextable::border(i = i, j = j, border = border , part = part)

      }
    ),

    active = list(

      table = function(value) {

        if(missing(value)) return(private$border$table_pvt)

        private$set_active(value, "table")

      },

      data = function(value) {

        if(missing(value)) return(private$border$data_pvt)

        private$set_active(value, "data")

      },

      header = function(value) {

        if(missing(value)) return(private$border$header_pvt)

        private$set_active(value, "header")

      },

      responses = function(value) {

        if(missing(value)) return(private$border$responses_pvt)

        private$set_active(value, "responses")

      },

      stats = function(value) {

        if(missing(value)) return(private$border$stats_pvt)

        private$set_active(value, "stats")

      },

      subsets = function(value) {

        if(missing(value)) return(private$border$subsets_pvt)

        private$set_active(value, "subsets")

      },

      subvars = function(value) {

        if(missing(value)) return(private$border$subvars_pvt)

        private$set_active(value, "subvars")

      },

      titles = function(value) {

        if(missing(value)) return(private$border$titles_pvt)

        private$set_active(value, "titles")

      },

      footer = function(value) {

        if(missing(value)) return(private$border$footer_pvt)

        private$set_active(value, "footer")

      }
    )
  )

#' @export
FT_RangeBorders <-
  R6::R6Class(
    classname = "FT_RangeBorders",

    private =       list(top_pvt = NULL,
                         bottom_pvt = NULL,
                         left_pvt = NULL,
                         right_pvt = NULL,
                         midv_pvt = NULL,
                         midh_pvt = NULL),

    public = list (


    ),

    active = list(

      top = function(value) {
        if(missing(value)) return(private$top_pvt)

        if(inherits(value, "FT_Border")) {
          private$top_pvt <- value
        }
      },

      bottom = function(value) {
        if(missing(value)) return(private$bottom_pvt)

        if(inherits(value, "FT_Border")) {
          private$bottom_pvt <- value
        }
      },
      left = function(value) {
        if(missing(value)) return(private$left_pvt)

        if(inherits(value, "FT_Border")) {
          private$left_pvt <- value
        }
      },
      right = function(value) {
        if(missing(value)) return(private$right_pvt)

        if(inherits(value, "FT_Border")) {
          private$right_pvt <- value
        }
      },
      midv = function(value) {
        if(missing(value)) return(private$midv_pvt)

        if(inherits(value, "FT_Border")) {
          private$midv_pvt <- value
        }
      },
      midh = function(value) {
        if(missing(value)) return(private$midh_pvt)

        if(inherits(value, "FT_Border")) {
          private$midh_pvt <- value
        }
      }
    )

  )


#' @export
FT_Border <-
  R6::R6Class(
    classname = "FT_Border",

    private = list(
      styles_pvt = c("solid",
                     "dashed",
                     "dotted",
                     "double",
                     "groove",
                     "ridge",
                     "inset",
                     "outset",
                     "none",
                     "hidden"),
      color_pvt = NULL,
      style_pvt = NULL ,
      width_pvt = NULL
    ),

    public = list(

      initialize = function( color = NULL,
                             style = NULL ,
                             width = NULL) {

        if(!is.null(color)) private$color_pvt = color
        if(!is.null(style)) private$style_pvt = style
        if(!is.null(width)) private$width_pvt = width

      }

    ),

    active = list(

      fp = function(value) {

        if(!missing(value)) {

          if(inherits(value, "fp_border")) {
            private$color_pvt = value$color
            private$style_pvt = value$style
            private$width_pvt = value$width

          }
          return()
        }

        officer::fp_border(style = self$style, color = self$color, width = self$width)

      },

      color = function(value) {

        if(missing(value)) return(private$color_pvt)

        if(inherits(value, "FT_Color")) {
          private$color_pvt <- value
        } else if(FT_Color$is_valid(value)) {
          private$color_pvt <- FT_Color$new(value)
        }
      },

      style = function(value) {

        if(missing(value)) return(private$style_pvt)

        if(inherits(value, "character") && value %in% private$styles_pvt) {
          private$style_pvt <- value
        } else {
          message("invalid border style")
        }
      },

      width = function(value) {

        if(missing(value)) return(private$width_pvt)

        if(inherits(value, "numeric")) {
          private$width_pvt <- value
        } else {
          message("invalid border style")
        }
      }
    )

  )
