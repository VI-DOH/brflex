
#' @export
FT_BordersMgr <-
  R6::R6Class(
    classname = "FT_BordersMgr",

    private = list(

      borders = list(
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

        if(inherits(value, "FT_RangeBorders")) {
        } else if(inherits(value[[1]], "FT_RangeBorders")) {

          value <-value[[1]]

        } else  {
          message("value must be an FT_RangeBorders object")
          return(NULL)
        }

        private$borders[[paste0(area, "_pvt")]] <- list(value)

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

          if(inherits(value, "FT_RangeBorders")) {
            value <- list(value)
          } else {
            value <- NULL
          }
          private$borders[[paste0(arg, "_pvt")]] <- value

        })

      },

      as.data.frame = function() {


        priv_list <- as.list(private$borders)

        keep <- !purrr::map_lgl(priv_list, is.null) %>% unname()
        priv_list <- priv_list[keep]

        nms <- names(private$borders)


        df <- purrr::map(nms, \(nm){


          brdr <- private$borders[[nm]][[1]]

          if(!is.null(brdr)) {

            x <- brdr$as.data.frame()

            x <- x %>%
              mutate(area = gsub("_pvt","",nm)) %>%
              relocate(area)

          } else {
            x <- NULL
          }

          x
        })

        df %>% bind_rows()
      },

      print = function() {


        print(self$as.data.frame(),  row.names = F)


      },

      n_borders = function(area) {

        length(private$borders[[paste0(area, "_pvt")]])
      },

      add_border = function(border, area) {

        if (!inherits(border, "FT_RangeBorders")) {

          message("border must be of class FT_RangeBorders")
          return(NULL)
        }

        n <- self$n_borders(area)
        if(n == 0) private$borders[[paste0(area, "_pvt")]] <- list()

        private$borders[[paste0(area, "_pvt")]][[n + 1]] <- border
      },

      apply = function(ft) {


        nms <- self$areas

        purrr::walk(nms, \(nm) {

 #         cat("trying ", nm, " ...\n")
          if(!is.null(self[[nm]])) {
            brdr_rng <- self[[nm]][[1]]

   #         cat("  ok ", nm, " is not NULL\n")
            f <- paste0(nm, "_rc")
            rc <- do.call(f, args = list(ft))

            ft <<- brdr_rng$apply(ft = ft, rc = rc)
          }

        })


        ft

      }

    ),

    active = list(

      areas = function(value) {

        if(!missing(value)) {

          message("this property is read-only")

          return()
        }

        priv_list <- names(as.list(private$borders)) %>% gsub("_pvt","",.)

      },

      table = function(value) {

        if(missing(value)) return(private$borders$table_pvt)

        private$set_active(value, "table")

      },

      data = function(value) {

        if(missing(value)) return(private$borders$data_pvt)

        private$set_active(value, "data")

      },

      header = function(value) {

        if(missing(value)) return(private$borders$header_pvt)

        private$set_active(value, "header")

      },

      responses = function(value) {

        if(missing(value)) return(private$borders$responses_pvt)

        private$set_active(value, "responses")

      },

      stats = function(value) {

        if(missing(value)) return(private$borders$stats_pvt)

        private$set_active(value, "stats")

      },

      subsets = function(value) {

        if(missing(value)) return(private$borders$subsets_pvt)

        private$set_active(value, "subsets")

      },

      subvars = function(value) {

        if(missing(value)) return(private$borders$subvars_pvt)

        private$set_active(value, "subvars")

      },

      titles = function(value) {

        if(missing(value)) return(private$borders$titles_pvt)

        private$set_active(value, "titles")

      },

      footer = function(value) {

        if(missing(value)) return(private$borders$footer_pvt)

        private$set_active(value, "footer")

      }
    )
  )

#' @export
FT_RangeBorders <-
  R6::R6Class(
    classname = "FT_RangeBorders",

    private = list(top_pvt = NULL,
                   bottom_pvt = NULL,
                   left_pvt = NULL,
                   right_pvt = NULL,
                   midv_pvt = NULL,
                   midh_pvt = NULL
    ),

    public = list (

      initialize = function(top = NULL,
                            bottom = NULL,
                            left = NULL,
                            right = NULL,
                            midv = NULL,
                            midh = NULL) {


        args <- as.list(match.call()) %>% tail(-1) %>% names()

        purrr::walk(args, \(arg) {

          value <- get(arg)

          if(inherits(value, "FT_Border")) {
            value <- value
          } else {
            value <- NULL
          }

          private[[paste0(arg, "_pvt")]] <- value

        })

      },

      addresses = function() {

        priv_list <-as.list(private)
        priv_list <- priv_list[!purrr::map_lgl(priv_list, is.null)]

        purrr::iwalk(priv_list, ~cat(stringr::str_pad(.y, width = 12,
                                                      side = "left", pad = " "),
                                     ":", lobstr::obj_addr(.x), "\n"))


      },

      as.data.frame = function() {

        priv_list <-as.list(private)
        priv_list <- priv_list[!purrr::map_lgl(priv_list, is.null)]

        # df <- list(
        #   priv_list$midv$as.data.frame(),
        #   priv_list$midh$as.data.frame(),
        #   priv_list$top$as.data.frame(),
        #   priv_list$bottom$as.data.frame(),
        #   priv_list$left$as.data.frame(),
        #   priv_list$right$as.data.frame()
        # )
        #
        # df <- df %>% bind_rows()%>%
        #   mutate(what = gsub("_pvt","",names(priv_list))) %>%
        #   relocate(what)

        df <- purrr::imap(priv_list, \(value, nm){
          if(!is.null(value)) {
            cat(nm, "\n")
            value$as.data.frame()%>%
              mutate(where = gsub("_pvt","",nm))
          }
        } ) %>%
          bind_rows() %>% relocate(where)

        df
      },

      print = function() {

        print(self$as.data.frame())

      },

      do_top = function(ft, rc) {


        if(!is.null(self$top)) {

          brdr <- self$top$fp

          if(!is.list(rc[[1]])) rc <- list(rc)

          purrr::walk(rc, \(rc0) {

            rows <- rc0$rows
            cols <- rc0$cols
            part <- rc0$part

            irow <-  rows[1]
            if(irow == 1) {
              ft <<- ft %>% flextable::hline_top( j = cols, border = brdr, part = part)
            } else {
              ft <<- ft %>% flextable::hline(i = irow - 1, j = cols, border = brdr, part = part)
            }
          })
        }

        ft

      },

      do_bottom = function(ft, rc) {

        if(!is.null(self$bottom)) {

          brdr <- self$bottom$fp

          if(!is.list(rc[[1]])) rc <- list(rc)

          purrr::walk(rc, \(rc0) {

            rows <- rc0$rows
            cols <- rc0$cols
            part <- rc0$part

            irow <-  tail(rows,1)

            ft <<- ft %>% flextable::hline(i = irow , j = cols, border = brdr, part = part)

          })
        }

        ft

      },

      do_right = function(ft, rc) {

        if(!is.null(self$right)) {

          brdr <- self$right$fp

          if(!is.list(rc[[1]])) rc <- list(rc)

          purrr::walk(rc, \(rc0) {

            rows <- rc0$rows
            cols <- rc0$cols
            part <- rc0$part

            icol <-  tail(cols,1)

            ft <<- ft %>% flextable::vline(i = rows , j = icol, border = brdr, part = part)

          })
        }

        ft

      },

      do_left = function(ft, rc) {

        if(!is.null(self$left)) {

          brdr <- self$left$fp

          if(!is.list(rc[[1]])) rc <- list(rc)

          purrr::walk(rc, \(rc0) {

            rows <- rc0$rows
            cols <- rc0$cols
            part <- rc0$part

            icol <-  cols[1]
            if(icol == 1) {
              ft <<- ft %>% flextable::vline_left( i = rows, border = brdr, part = part)

            } else {
              ft <<- ft %>% flextable::vline(i = rows, j = icol - 1, border = brdr, part = part)

            }

          })
        }

        ft

      },

      do_midv = function(ft, rc) {

        if(!is.null(self$midv)) {

          brdr <- self$midv$fp

          if(!is.list(rc[[1]])) rc <- list(rc)

          purrr::walk(rc, \(rc0) {

            rows <- rc0$rows
            cols <- rc0$cols
            part <- rc0$part


            ft <<- ft %>% flextable::vline(i = rows , j = head(cols, -1),
                                           border = brdr, part = part)

          })

        }

        ft

      },

      do_midh = function(ft,rc) {

        if(!is.null(self$midh)) {

          if(!is.list(rc[[1]])) rc <- list(rc)

          brdr <- private[["midh_pvt"]]

          purrr::walk(rc, \(rc0) {

            rows <- rc0$rows
            cols <- rc0$cols
            part <- rc0$part

            fp_brdr <- brdr$fp

            ft <<- ft %>% flextable::hline(i = head(rows, -1) , j = cols,
                                           border = fp_brdr, part = part)

          })

        }

        ft

      },

      apply = function(ft, rc) {

        ft <- ft %>%
          self$do_top(rc) %>%
          self$do_bottom(rc) %>%
          self$do_right(rc) %>%
          self$do_left(rc) %>%
          self$do_midv(rc) %>%
          self$do_midh(rc)

        ft
      }



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

      },

      print = function() {

        print(self$as.data.frame())

      },

      as.data.frame = function() {

        data.frame(color = self$color, style = self$style, width = self$width)
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
          private$color_pvt <- value$color
        } else if(FT_Color$is_valid(value)) {
          private$color_pvt <- value
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

#' @export
FT_DefaultBordersMgr <-
  R6::R6Class(
    classname = "FT_DefaultBordersMgr",
    inherit = FT_BordersMgr,

    public = list(

      initialize = function() {

        brdr_solid <- FT_Border$new(color = "grey33", style = "solid", width = 2)
        brdr_dashed <- FT_Border$new(color = "grey33", style = "dashed", width = 1)
        brdr_dotted <- FT_Border$new(color = "grey33", style = "dotted", width = 1)
        brdr_double <- FT_Border$new(color = "grey33", style = "double", width = 1)

        brdrs_solid_box_dotted_int <-
          FT_RangeBorders$new(top = brdr_solid, bottom = brdr_solid,
                              left = brdr_solid,right = brdr_solid,
                              midv = brdr_solid, midh = brdr_dotted)

        brdrs_solid_box_no_int <-
          FT_RangeBorders$new(top = brdr_solid, bottom = brdr_solid,
                              left = brdr_solid,right = brdr_solid,
                              midv = NULL, midh = NULL)

        brdrs_solid_box_solid_int <-
          FT_RangeBorders$new(top = brdr_solid, bottom = brdr_solid,
                              left = brdr_solid, right = brdr_solid,
                              midv = brdr_solid, midh = brdr_solid)


        super$titles <- brdrs_solid_box_no_int
        super$responses <- brdrs_solid_box_solid_int
        super$stats <- brdrs_solid_box_solid_int
        super$data <- brdrs_solid_box_dotted_int
        super$subsets <- brdrs_solid_box_dotted_int
        super$subvars <- brdrs_solid_box_dotted_int

      }
    )
  )
