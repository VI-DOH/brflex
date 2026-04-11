
#' @export
#' @method + ft_font
#'
`+.FT_Font` <- function(fnt0, fnt1) {

  if(is.null(fnt0)) {
    return(fnt1)
  }

  if (!inherits(fnt0, "FT_Font") || !inherits(fnt1, "FT_Font")) {

    stop("Can only add two FT_Font objects")
  }
  fnt_new <- fnt0$clone()
  fnt_new$merge(fnt1)

  fnt_new
}

`%<<%` <- function(fnt0, fnt1) {

  if(is.null(fnt0)) {
    return(fnt1$clone())
  }

  if (!inherits(fnt0, "FT_Font") || !inherits(fnt1, "FT_Font")) {

    stop("Can only add two FT_Font objects")
  }
  fnt_new <- fnt0$clone()
  # if(fnt0$color == "green") browser()
  fnt_new$merge(fnt1)

  fnt_new
}

`%>>%` <- function(fnt0, fnt1) {

  if(is.null(fnt1)) {
    return(fnt0$clone())
  }

  if (!inherits(fnt0, "FT_Font") || !inherits(fnt1, "FT_Font")) {

    stop("Can only add two FT_Font objects")
  }
  fnt_new <- fnt1$clone()
  fnt_new$merge(fnt0)

  fnt_new
}


#' @export
FT_FontsMgr <-
  R6::R6Class(
    classname = "FT_FontsMgr",

    private = list(

      font = list(
        table_pvt = NULL,
        data_pvt = NULL,
        header_pvt = NULL,
        responses_pvt = NULL,
        years_pvt = NULL,
        stats_pvt = NULL,
        subsets_pvt = NULL,
        subvars_pvt = NULL,
        titles_pvt = NULL,
        footnotes_pvt = NULL,
        footer_pvt = NULL
      ),

      verbose_pvt = FALSE,

      merge_one = function(major, areas) {

        private$font[[paste0(major, "_pvt")]][[1]] <-
          private$font[[paste0(major, "_pvt")]][[1]]  %<<% private$font$table_pvt[[1]]

        purrr::walk(areas, \(area) {
          #area <- "titles"
          n_fnts <- self$n_fonts(area)
          if(n_fnts == 0) {
            private$font[[paste0(area, "_pvt")]][[1]] <-
              private$font[[paste0(major, "_pvt")]][[1]]$clone()
          } else {
            purrr::map(1:n_fnts, \(ifnt) {
              private$font[[paste0(area, "_pvt")]][[ifnt]] <-
                private$font[[paste0(area, "_pvt")]][[ifnt]] %<<%
                private$font[[paste0(major, "_pvt")]][[1]]
            })
          }

        })

      },

      set_active = function(value, area) {

        if(!inherits(value, "FT_Font")) {
          if(!inherits(value[[1]], "FT_Font")) {
            message("value must be an FT_Font object")
            return(NULL)
          }
          value <-value[[1]]
        }

        if(inherits(value, "FT_Font")) {
          value <- list(value)
        }

        private$font[[paste0(area, "_pvt")]] <- value

      }

    ),

    public = list(

      as.data.frame = function() {

        priv_list <- as.list(private$font)


        df <- purrr::imap(priv_list, \(value, nm){
          if(!is.null(value)) {

            purrr::map(value,\(fnt) {

              fnt$as.data.frame()
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

      merge_fonts = function() {

        default_font <- FT_DefaultFont$new()


        private$font$table_pvt[[1]] <- private$font$table_pvt[[1]] %<<% default_font

        private$merge_one(major = "data", areas = c("subsets", "subvars"))
        private$merge_one(major = "header", areas = c("titles", "years", "responses", "stats"))
        private$merge_one(major = "footer", areas = c("footnotes"))

      },

      n_fonts = function(area) {

        length(private$font[[paste0(area, "_pvt")]])
      },

      set_font = function(fnt, area) {

        if (!inherits(fnt, "FT_Font")) {

          message("fnt must be of class FT_Font")
          return(NULL)
        }

        private$font[[paste0(area, "_pvt")]] <- list()

        self$add_font(fnt, area)

      },

      add_font = function(fnt, area) {

        if (!inherits(fnt, "FT_Font")) {

          message("fnt must be of class FT_Font")
          return(NULL)
        }

        n <- self$n_fonts(area)
        if(n == 0) private$font[[paste0(area, "_pvt")]] <- list()

        private$font[[paste0(area, "_pvt")]][[n + 1]] <- fnt
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
          } else {
            jcols_subvars <- 1
            irows_subvars <- 1

            jcols_subsets <- 0
            irows_subsets <- 0

          }

        }

        nrow_header <- ft$header$content$nrow
        nrow_titles <- nrow_header - 2

        #handle_fonts_header(fonts)
        # handle_fonts_response(fonts)%>%
        #   handle_fonts_stats(fonts) %>%
        #self$apply_titles(ft)
        ft <- ft %>%
          self$apply_area("titles", i = 1:nrow_titles, part = "header") %>%
          self$apply_area("responses", i = nrow_header - 1, part = "header") %>%
          self$apply_area("stats", i = nrow_header, part = "header")  %>%
          self$apply_area("years", part = "header")%>%
          self$apply_area("data", part = "body")%>%
          self$apply_area("footnotes",  part = "footer") %>%
          self$apply_area("subvars", i = irows_subvars, j = jcols_subvars, part = "body")

        if(!is.null(irows_subsets) && irows_subsets[1] > 0) ft <- ft %>%
          self$apply_area("subsets", i = irows_subsets, j = jcols_subsets, part = "body")

        ft
      },

      apply_area = function(ft, area, i = NULL, j = NULL, part) {

        if(is.null(i)) i <- 1:(ft[[part]]$content$nrow)

        f <- paste0(area, "_rc")
        rc <- do.call(f, args = list(ft))

        i <- rc$rows
        j = rc$cols
        part <- rc$part

        nrow <- length(i)

        nfonts <- self$n_fonts(area)

        if(nrow != nfonts) {
          ifonts <- rep(1,nrow)
        } else {
          ifonts <- 1:nrow
        }

        purrr::walk2(i, ifonts,\(irow, ifnt) {
          #if(area == "responses") browser()
          fnt  <-  private$font[[paste0(area,"_pvt")]][[ifnt]]

          ft <<- self$apply_font(ft, i = irow, j = j,
                                 font = fnt, part = part)
        })
        ft
      },

      apply_font = function(ft, i = NULL, j = NULL, font , part = "header") {

        ft %>%
          flextable::fontsize(i = i, j = j, size = font$font.size, part = part) %>%
          flextable::color(i = i, j = j, color = font$color, part = part) %>%
          flextable::bold(i = i, j = j,bold = font$bold, part = part) %>%
          flextable::italic(i = i, j = j,italic = font$italic, part = part) %>%
          flextable::font(i = i, j = j ,fontname = font$font, part = part) %>%
          flextable::bg(i = i, j = j ,bg = font$shading.color, part = part)
      }
    ),

    active = list(

      verbose = function(value) {

        if(missing(value)) return(private$verbose_pvt)

        if(!inherits(value, "logical")) {

          message("value must be logical")
          return(NULL)
        }

        private$verbose_pvt <- value
      },

      table = function(value) {

        if(missing(value)) return(private$font$table_pvt[[1]])

        private$set_active(value, "table")

      },

      data = function(value) {

        if(missing(value)) return(private$font$data_pvt[[1]])

        private$set_active(value, "data")

      },

      header = function(value) {

        if(missing(value)) return(private$font$header_pvt[[1]])

        private$set_active(value, "header")

      },

      years = function(value) {

        if(missing(value)) return(private$font$years_pvt[[1]])

        private$set_active(value, "years")

      },

      responses = function(value) {

        if(missing(value)) return(private$font$responses_pvt[[1]])

        private$set_active(value, "responses")

      },

      stats = function(value) {

        if(missing(value)) return(private$font$stats_pvt[[1]])

        private$set_active(value, "stats")

      },

      subsets = function(value) {

        if(missing(value)) return(private$font$subsets_pvt[[1]])

        private$set_active(value, "subsets")

      },

      subvars = function(value) {

        if(missing(value)) return(private$font$subvars_pvt[[1]])

        private$set_active(value, "subvars")

      },

      titles = function(value) {

        if(missing(value)) return(private$font$titles_pvt[[1]])

        private$set_active(value, "titles")

      },

      footer = function(value) {

        if(missing(value)) return(private$font$footer_pvt[[1]])

        private$set_active(value, "footer")

      }
    )

  )

#' @export
FT_DefaultFontsMgr <-
  R6::R6Class(
    classname = "FT_DefaultFontsMgr",
    inherit = FT_FontsMgr,

    public = list(

      initialize = function(font = "Open Sans") {

        super$add_font(FT_Font$new(font = font, color = "grey22", font.size = 12), "table")

        super$add_font(FT_Font$new(font = font, color = "grey22", font.size = 13,
                                   bold = TRUE), "titles")

        super$add_font(FT_Font$new(font = font, color = "grey22", font.size = 12), "data")

        super$merge_fonts()

      }
    )
  )





#' @export
FT_Font <-
  R6::R6Class(
    classname = "FT_Font",

    private = list(
      color_pvt = NA,
      font_pvt = NA,
      font.size_pvt = NA,
      bold_pvt = NA,
      italic_pvt = NA,
      underlined_pvt = NA,
      vertical.align_pvt = NA,
      shading.color_pvt = NA

    ),

    public = list(

      initialize = function (color = NULL, font = NULL, font.size = NULL,
                             bold = NULL, italic = NULL,
                             underlined = NULL,
                             vertical.align = NULL,
                             shading.color = NULL) {

        private$color_pvt <- if(is.null(color)) NA else color
        private$font_pvt <- if(is.null(font)) NA else font
        private$font.size_pvt <- if(is.null(font.size)) NA else font.size
        private$bold_pvt <- if(is.null(bold)) NA else bold
        private$italic_pvt <- if(is.null(italic)) NA else italic
        private$underlined_pvt <- if(is.null(underlined)) NA else underlined
        private$vertical.align_pvt <- if(is.null(vertical.align)) NA else vertical.align
        private$shading.color_pvt <- if(is.null(shading.color)) NA else shading.color

      },

      print = function() {

        cat(
          "bold: ", self$bold, "\n",
          "color: ", self$color, "\n",
          "font.size: ", self$font.size, "\n",
          "font: ", self$font, "\n",
          "italic: ", self$italic, "\n",
          "shading.color: ", self$shading.color, "\n",
          "underlined: ", self$underlined, "\n",
          "vertical.align: ", self$vertical.align, "\n",
          sep = ""
        )

      },

      as.data.frame = function() {


        data.frame(color = private$color_pvt,
                   font.size = private$font.size_pvt,
                   bold = private$bold_pvt,
                   italic = private$italic_pvt,
                   underlined = private$underlined_pvt,
                   font.family = private$font_pvt,
                   vertical.align = private$vertical.align_pvt,
                   shading.color = private$shading.color_pvt
        )

      },

      merge = function(fnt1) {

        if(inherits(fnt1, "FT_Font")) {
          fnt1 <- fnt1$as.list()
        }

        fnt0 <- self$as.list()
        purrr::iwalk(fnt1,\(elem, nm) {
          if(is.na(private[[paste0(nm, "_pvt")]])) private[[paste0(nm, "_pvt")]] <<- elem
        })
      },

      fp_text = function() {

        x <-  officer::fp_text(color = private$color_pvt,
                               font.size = private$font.size_pvt,
                               bold = private$bold_pvt,
                               italic = private$italic_pvt,
                               underlined = private$underlined_pvt,
                               font.family = private$font_pvt,
                               vertical.align = private$vertical.align_pvt,
                               shading.color = private$shading.color_pvt)

        structure(x, class = c("ft_text", "ft_font"))
      },

      as.list = function() {

        list(
          color  = self$color,
          font  = self$font,
          font.size = self$font.size,
          bold  = self$bold,
          italic  = self$italic,
          underlined  = self$underlined,
          vertical.align  = self$vertical.align,
          shading.color  = self$shading.color
        )
      }

    ),

    active = list(
      color = function(value) {

        if(missing(value)) return (private$color_pvt)

        private$color_pvt <- value

        return(self)

      },

      font = function(value) {

        if(missing(value)) return (private$font_pvt)

        private$font_pvt <- value

      },

      font.size = function(value) {

        if(missing(value)) return (private$font.size_pvt)

        private$font.size_pvt <- value

      },

      bold = function(value) {

        if(missing(value)) return (private$bold_pvt)

        private$bold_pvt <- value

      },

      italic = function(value) {

        if(missing(value)) return (private$italic_pvt)

        private$italic_pvt <- value

      },

      underlined = function(value) {

        if(missing(value)) return (private$underlined_pvt)

        private$underlined_pvt <- value

      },

      vertical.align = function(value) {

        if(missing(value)) return (private$vertical.align_pvt)

        private$vertical.align_pvt <- value

      },

      shading.color = function(value) {

        if(missing(value)) return (private$shading.color_pvt)

        private$shading.color_pvt <- value

      }
    )

  )

#' @export
FT_DefaultFont <-
  R6::R6Class(
    classname = "FT_DefaultFont",
    inherit = FT_Font,

    private = list(

    ),

    public = list(
      initialize = function (color = NULL, font = NULL, font.size = NULL,
                             bold = NULL, italic = NULL,
                             underlined = NULL,
                             vertical.align = NULL,
                             shading.color = NULL) {


        super$color <- "black"
        super$font <- "Open Sans"
        super$font.size <- 10
        super$bold <- FALSE
        super$italic <- FALSE
        super$underlined <- FALSE
        super$vertical.align <- "baseline"
        super$shading.color <- "transparent"

      }
    ),

    active = list(
    )

  )

#' @export
FT_Font$is_valid_list <- function(lst) {

  val_fields <- FT_Font$private_fields %>% names() %>% purrr::map_chr( ~gsub("_pvt", "", .x))
  lst_fields <- lst %>% names()

  not_ok <- setdiff(x = lst_fields, y = val_fields)

  return(length(not_ok) == 0)

}
