

#' @export
FT_HighlightsMgr <-
  R6::R6Class(
    classname = "FT_HighlightsMgr",

    private = list(

      highlights_pvt = list(),
      h_highlights_pvt = 0
    ),

    public = list(

      add = function(hilt) {

        if (!inherits(hilt, "FT_Highlight")) {

          if(!inherits(bg, "FT_Bg")) {

            message("hilt must be of class FT_Highlight")
            return(NULL)
          }
        }

        n <- length(private$highlights_pvt)

        private$highlights_pvt[[n + 1]] <- hilt
      },

      set = function(hilt) {

        if (!inherits(hilt, "FT_Highlight")) {

          if(!inherits(bg, "FT_Bg")) {

            message("hilt must be of class FT_Highlight")
            return(NULL)
          }
        }

        private$highlights_pvt <- list(hilt)
      },

      get = function(index) {

        if(index > length(private$highlights_pvt)) {
          message("Not that many items")
          return(NULL)

        }

        private$highlights_pvt[[index]]
      },

      apply = function(ft) {

        hlts <- private$highlights_pvt
        purrr::walk(hlts,\(hlt) {

          ft <<- hlt$apply_highlight(ft)
        })

        ft
      }

    ),

    active = list(





    )
  )




#' @export
FT_Highlight <-
  R6::R6Class(
    classname = "FT_Highlight",

    private = list(

      type_pvt = NULL,
      row_nums_pvt = NULL,
      row_names_pvt = NULL,
      row_values_pvt = NULL,
      col_nums_pvt = NULL,
      col_names_pvt = NULL,
      col_values_pvt = NULL,
      font_pvt = NULL,
      bg_pvt = NULL,
      borders_pvt = NULL,
      part_pvt = "body",

      get_row_nums = function(ft) {

        df <-  ft$body$dataset

        if(!is.null(self$row_nums)) {

          row_nums <- self$row_nums

        } else {


          nrows = length(self$row_names)

          if("subvar" %in% ft$col_keys) {
            df2 <- df %>% select(self$col_nums) %>% mutate(rn = row_number())

            row_nums <- mapply(function(nm, val) {
              df2  %>% filter(subvar == nm & subset == val) %>%  pull(rn)

            }, self$row_names, self$row_values)

          } else {
            row_nums <- df %>% pull(subset) %>% {which(. %in% self$row_values)}

          }
        }

        return(row_nums)
      }
    ),

    public = list(

      print = function() {

        cat("type_pvt:", private$type_pvt, "\n")
        cat("row_nums_pvt:", private$row_nums_pvt, "\n")
        cat("row_names_pvt:", private$row_names_pvt, "\n")
        cat("row_values_pvt:", private$row_values_pvt, "\n")
        cat("col_nums_pvt:", private$col_nums_pvt, "\n")
        cat("col_names_pvt:", private$col_names_pvt, "\n")
        cat("col_values_pvt:", private$col_values_pvt, "\n")
        cat("font_pvt:", "obj", "\n")
        cat("bg_pvt:", "obj", "\n")
        cat("borders_pvt:", "obj", "\n")
        cat("part_pvt:", private$part_pvt, "\n")
      }
    ),

    active = list(

      type = function(value) {

        if(missing(value)) {
          return(private$type_pvt)
        }

        private$type_pvt <- value
      },

      row_nums = function(value) {
        if(missing(value)) {
          return(private$row_nums_pvt)
        }
        private$row_nums_pvt <- value
      },

      row_names = function(value) {
        if(missing(value)) {
          return(private$row_names_pvt)
        }
        private$row_names_pvt <- value
      },

      row_values = function(value) {
        if(missing(value)) {
          return(private$row_values_pvt)
        }
        private$row_values_pvt <- value
      },

      col_nums = function(value) {
        if(missing(value)) {
          return(private$col_nums_pvt)
        }
        private$col_nums_pvt <- value
      },

      col_names = function(value) {
        if(missing(value)) {
          return(private$col_names_pvt)
        }
        private$col_names_pvt <- value
      },

      col_values = function(value) {
        if(missing(value)) {
          return(private$col_values_pvt)
        }
        private$col_values_pvt <- value
      },

      font = function(value) {
        if(missing(value)) {
          return(private$font_pvt)
        }

        if (!inherits(value, "FT_Font")) {

          message("value must be of class FT_Font")
          return(NULL)
        }


        private$font_pvt <- value
      },

      bg = function(value) {
        if(missing(value)) {
          return(private$bg_pvt)
        }

        if(!FT_Color$is_valid(bg)) {
          message("value must be a valid color")
          return()
        }

        private$bg_pvt <- value
      },

      borders = function(value) {
        if(missing(value)) {
          return(private$borders_pvt)
        }
        private$borders_pvt <- value
      },

      part = function(value) {
        if(missing(value)) {
          return(private$part_pvt)
        }
        private$part_pvt <- value
      }



    )
  )

#' @export
FT_HighlightRow <-
  R6::R6Class(
    classname = "FT_HighlightRow",
    inherit = FT_Highlight,

    private = list(


    ),

    public = list(


      initialize  = function(where, col_nums = c(1,2),
                             font = ft_font_parts(),
                             bg = NULL,
                             borders = NULL,
                             part = "body") {

        p <- private

        nrows <- length(where)

        if(is.numeric(where)) {
          p$row_nums_pvt <- where
          p$row_names_pvt <- NULL
          p$row_values_pvt <- NULL
        } else {
          p$row_nums_pvt <- NULL
          p$row_names_pvt <- names(where)
          p$row_values_pvt <- unlist(where)
        }

        if(part %in% c("footer")) col_nums <- c(1,1)


        p$type_pvt <- "row"
        p$col_nums_pvt <- col_nums
        p$col_names_pvt <- NULL
        p$col_values_pvt <- NULL
        p$font_pvt <- font
        p$bg_pvt <- bg
        p$borders_pvt <- borders
        p$part_pvt <- part


      },


      apply_highlight = function(ft) {

        p <- private

        df <- ft$body$dataset

        row_nums <- p$get_row_nums(ft)

        font <- self$font
        if(!is.null(font)) {
          if(!is.list(font)) {
           font <- self$font$as.list() %>% replace(.,is.na(.),NULL)
          }
          elems <- font %>% names()
        } else {

          elems <- list()
        }

        bg <- self$bg
        borders <- self$borders

        part <- self$part
        cols <- p$col_nums

        if(is.null(cols)) {
          cols <- 2:ncol(df)
        }

        # =============================================================
        #   when you delete a column after ft is created,
        #     like when top is selected for subset placement,
        #     ft doesn't alter the dataset, so we track it with
        #      this attribute
        #

        # if(attr(ft,"sub_placement") == "top") cols <-  cols - 1
        if(ft$properties["sub_placement"] == "top") cols <-  cols - 1

        purrr::walk(row_nums, \(irow) {
          purrr::walk(cols,\(jcol) {

            purrr::walk(elems,\(elem) {

              #cat("trying elem: ", elem, "\n")
              if(elem %in% names(ft[[part]]$styles$text)) {
                ft[[part]]$styles$text[[elem]]$data[irow,jcol] <<- font[[elem]]
                #cat("   OK\n")
              }
            })

            if(!is.null(self$bg))
              ft[[part]]$styles$cells$background.color$data[irow, jcol] <<- self$bg$color

            if(!is.null(self$borders)){

              borders <- self$borders

              borders0 <- ft_borders()

              i0 <- row_nums[1]
              i1 <- tail(row_nums,1)

              j0 <- cols[1]
              j1 <- tail(cols,1)

              if(jcol > j0) {
                borders0$left <- borders$midv
              } else {
                borders0$left <- borders$left
              }

              if(jcol < j1) {
                borders0$right <- borders$midv
              } else {
                borders0$right <- borders$right
              }

              if(irow > i0) {
                borders0$top <- borders$midh
              } else {
                borders0$top <- borders$top
              }

              if(irow < i1) {
                borders0$bottom <- borders$midh
              } else {
                borders0$bottom <- borders$bottom
              }

              # cat("========================================\n",
              #     "[",irow,",",  jcol,"]\n", sep = "")
              # print(borders0)

              ft <<- ft %>%  brflex:::make_border(borders = borders0, i = irow, j = jcol, part = part)

            }
          })

        })

        ft
      }

    ),

    active = list(





    )
  )



#' @export
FT_HighlightIf <-
  R6::R6Class(
    classname = "FT_HighlightIf",
    inherit = FT_Highlight,

    private = list(

      quo_pvt = NULL

    ),

    public = list(

      initialize  = function(where , font = NULL,
                             bg = NULL,
                             borders = NULL) {

        p <- private

        p$type_pvt = "if"

        p$quo_pvt <- rlang::enquo(where)
        p$font_pvt = font
        p$bg_pvt = bg
        p$borders_pvt = borders
      },

      print = function() {

        cat("condition: ",  private$quo_pvt %>% as_label())
        super$print()
      }
    ),

    active = list(





    )
  )
