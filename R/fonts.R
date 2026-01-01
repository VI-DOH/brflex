
#' Flextable Fonts Helper
#'
#'   Helper function to build list of font elements for sections to the
#'   stats table to be use in call to ft_add_font()
#'
#' @param table - ft_font
#' @param data  - ft_font
#' @param header  - ft_font
#' @param responses  - ft_font
#' @param stats  - ft_font
#' @param titles  - ft_font
#' @param footer  - ft_font
#'
#' @returns list to pass as argument to ft_stats(fonts = list)
#' @export
#'
#' @examples
#' ft_font_list(data = ft_font(color = "darkblue", font.size = 12))
#'
ft_font_list <- function(table = NULL,
                         data = NULL,
                         header = NULL,
                         responses = NULL,
                         stats = NULL,
                         subsets = NULL,
                         subvars = NULL,
                         titles = NULL,
                         footer = NULL){

  structure(
    list(table_fonts = table,
       data_fonts = data,
       header_fonts = header,
       stats_fonts = stats,
       subsets_fonts = subsets,
       subvars_fonts = subvars,
       titles_fonts = titles,
       footer_fonts = footer,
       responses_fonts = responses),
    class = "ft_fonts_list")

}
#' Font Builder for BRFSS Flextables
#'
#' @param color character (#RRGGBB)
#' @param font character
#' @param font.size integer
#' @param bold logical
#' @param italic logical
#' @param underlined logical
#' @param vertical.align character
#' @param shading.color character
#'
#' @returns the ft_font object
#' @export
#'
ft_font <- function (color = "black", font = "Arial", font.size = 10,
                     bold = FALSE, italic = FALSE,
                     underlined = FALSE,
                     vertical.align = "baseline",
                     shading.color = "transparent", as.list = FALSE) {


  x <- officer::fp_text(color = color, font.size = font.size, bold = bold, italic = italic,
                        underlined = underlined, font.family = font,
                        vertical.align = vertical.align,
                        shading.color = shading.color)

  if(as.list) {
    x <- list(x) %>%
      structure(class = c("list", "ft_text", "ft_font"))
  } else {

    x <-  structure(x, class = c("ft_text", "ft_font"))
  }

  x

}


#' Font Parts Builder for BRFSS Flextables
#'
#'   Make a list of font elements to be added to (merged with) another set of element
#'
#' @param color character (#RRGGBB)
#' @param font character
#' @param font.size integer
#' @param bold logical
#' @param italic logical
#' @param underlined logical
#' @param vertical.align character
#' @param shading.color character
#'
#' @returns the ft_font object
#' @export
#'
ft_font_parts <- function (color = NA, font = NA, font.size = NA,
                           bold = NA, italic = NA,
                           underlined = NA,
                           vertical.align = NA,
                           shading.color = NA) {

  x <- officer::fp_text(color = color, font.size = font.size, bold = bold, italic = italic,
                        underlined = underlined, font.family = font,
                        vertical.align = vertical.align,
                        shading.color = shading.color)


  x <-  structure(x, class = c("ft_font_parts"))

  x

}

as.ft_font <- function(x) {
  structure(x, class = c(class(x) ,"ft_font"))
}

#' @export
#' @method + ft_font
`+.ft_font` <- function(fnt, fnt_prts) {

  if (!inherits(fnt_prts, "list")) {
    if (!inherits(fnt_prts, "ft_font_parts"))
      stop("Can only add ft_font to ft_font_parts")

    fnt_prts <- replace(fnt_prts, is.na(fnt_prts), NULL)

    # combine elements, keeping non-NULL values from e2 overriding e1
    merged <- modifyList(fnt, fnt_prts, keep.null = TRUE )
  } else {
    mrgd <- purrr::map(fnt_prts, \(fnt_prt) {
      fnt_prt <- replace(fnt_prt, is.na(fnt_prt), NULL)
      modifyList(fnt, fnt_prt, keep.null = TRUE )
    })

    merged <- mrgd
  }

  class(merged) <- "ft_font"
  merged
}

ft_add_font <- function(ft, i = NULL, j = NULL, ft_text,
                        part = c('all', 'body', 'header', 'footer')) {

  part <- match.arg(part, c('all', 'body', 'header', 'footer'))

  ft %>%
    flextable::fontsize(i = i, j = j, size = ft_text$font.size, part = part) %>%
    flextable::color(i = i, j = j, color = ft_text$color, part = part) %>%
    flextable::bold(i = i, j = j,bold = ft_text$bold, part = part) %>%
    flextable::italic(i = i, j = j,italic = ft_text$italic, part = part) %>%
    flextable::font(i = i, j = j ,fontname = ft_text$font.family, part = part) %>%
    flextable::bg(i = i, j = j ,bg = ft_text$shading.color, part = part)
}

# if a font is null, use its parent otherwise add it to its parent
#   fnt is the font to check, fnt0 is its parent
#     by paent we mean font from encompassing section
#     for example if there is no specified font for the stats row (fonts$stats_fonts)
#     then we just use the header font (fonts$header font)

merge_font <- function(fnt,fnt0) {

  if(is.null(fnt)) fnt0 else fnt0  + fnt

}

merge_fonts <- function(fonts) {

  default_font <- ft_font()

  fonts$table_fonts <- merge_font(fonts$table_fonts, default_font)
  fonts$header_fonts <- merge_font(fonts$header_fonts, fonts$table_fonts)
  fonts$data_fonts <- merge_font(fonts$data_fonts, fonts$table_fonts)
  fonts$footer_fonts <- merge_font(fonts$footer_fonts, fonts$table_fonts)

  # sub data fonts

  fonts$subsets_fonts <- merge_font(fonts$subsets_fonts, fonts$data_fonts)
  fonts$subvars_fonts <- merge_font(fonts$subvars_fonts, fonts$data_fonts)

  # sub header fonts

  fonts$response_fonts <- merge_font(fonts$response_fonts, fonts$header_fonts)
  fonts$stats_fonts <- merge_font(fonts$stats_fonts, fonts$header_fonts)
  fonts$titles_fonts <- merge_font(fonts$titles_fonts, fonts$header_fonts)

  # sub footer fonts

  fonts$footnotes_fonts <- merge_font(fonts$footnotes_fonts, fonts$footer_fonts)

  # sub footer fonts

  fonts

}

handle_fonts <- function(ft, fonts) {

  if(!is.null(fonts)) {

    fonts <- fonts %>% merge_fonts()

    ft <- ft %>% handle_fonts_table(fonts) %>%
      handle_fonts_header(fonts) %>%
      handle_fonts_body(fonts) %>%
      handle_fonts_footer(fonts)  %>%
      handle_fonts_footnotes(fonts) %>%
      {.}

  }
  ft
}

handle_fonts_table <- function(ft, fonts) {

  font <- fonts$table_fonts

  if(!is.null(font) && "ft_font" %in% class(font)) {

    ft <- ft %>% ft_add_font(ft_text = font, part = "all")
  }

  ft

}

handle_fonts_header <- function(ft, fonts) {

  font <- fonts$header_fonts

  if(!is.null(font) && "ft_font" %in% class(font)) {

    ft <- ft %>% ft_add_font(ft_text = font, part = "header")
  }

  ft <-   ft <- ft %>%
    handle_fonts_response(fonts)%>%
    handle_fonts_stats(fonts) %>%
#    handle_fonts_response(fonts)%>%
    handle_fonts_titles(fonts)


  ft
}

handle_fonts_titles <- function(ft, fonts) {


  tit_fonts <- fonts$titles_fonts

  if(!is.null(tit_fonts) && ft$header$sections$nrow$titles > 0) {

    irows <- titles_rows(ft)
    nrows <- length(irows)

    if(! "list" %in% class(tit_fonts)) {
      #      if(class(tit_fonts) != "list") {

      ft <- ft_add_font(ft, i = irows, ft_text = tit_fonts, part = "header")

    } else {

      tit_fonts <- reused(tit_fonts, nrows)

      purrr::map2(irows, tit_fonts, \(irow, fnt) {

        ft <<- ft_add_font(ft, i = irow, ft_text = fnt, part = "header")


      })
    }
  }


  ft
}



handle_fonts_response <- function(ft, fonts) {

  if(!is.null(fonts$response_fonts) && ft$header$sections$nrow$responses > 0) {

    irows <- responses_rows(ft)

    ft <- ft %>%
      ft_add_font(i = irows, ft_text = fonts$response_fonts , part = "header")

  }

  ft
}

handle_fonts_body <- function(ft, fonts) {

  if(!is.null(fonts$data_fonts)) {

    ft <- ft %>%
      ft_add_font( ft_text = fonts$data_fonts , part = "body")

    ft <- ft %>%
      handle_fonts_subvars(fonts)

  }

  ft
}

handle_fonts_subvars <- function(ft, fonts) {


  if(!is.null(fonts$subvars_fonts)) {
    irows <- subvar_rows(ft)

    if(irows[2,"min"] == 3) {
      irows <- irows$min - 1
      irows <- irows[irows>1]

    } else {
      irows <- irows$min
      irows <- irows[irows>1]
    }

    ft <- ft %>%
      ft_add_font(i = irows, j = 1, ft_text = fonts$subvars_fonts , part = "body")

  }

  ft
}

handle_fonts_stats <- function(ft, fonts) {

  if(!is.null(fonts$stats_fonts) && ft$header$sections$nrow$stats > 0) {
    irows <- stats_rows(ft)

    ft <- ft %>%
      ft_add_font(i = irows, ft_text = fonts$stats_fonts , part = "header")

  }

  ft
}

handle_fonts_footer <- function(ft, fonts) {

  ftr_fonts <- fonts$footer_fonts

  if(!is.null(ftr_fonts)) {
    nrows <- ft$header$content$nrow - 2
    nfonts <- length(ftr_fonts)

    if(nrows > 0){
      if(! "list" %in% class(ftr_fonts)) {
        ft <<- ft_add_font(ft, i = 1:nrows, ft_text = ftr_fonts, part = "footer")
      } else {
        purrr::walk(1:nrows, \(irow) {
          ft <<- ft_add_font(ft, i = irow, ft_text = ftr_fonts[[irow]], part = "footer")

        })
      }

    }
  }

  ft
}

handle_fonts_footnotes <- function(ft, fonts) {

  ftr_fonts <- fonts$footer_fonts

  if(!is.null(ftr_fonts)) {

    nrows <- ft$footer$content$nrow
    #nfonts <- length(ftr_fonts)

    if(nrows > 0){
      if(! "list" %in% class(ftr_fonts)) {

        purrr::walk(1:nrows,\(irow) {
          ft <<- ft_handle_footnote(ft, irow, ftr_fonts)
        })
        ft <<- ft_add_font(ft, i = 1:nrows, ft_text = ftr_fonts, part = "footer")
      } else {
        purrr::walk(1:nrows, \(irow) {
          ft <<- ft_add_font(ft, i = irow, ft_text = ftr_fonts[[irow]], part = "footer")

        })
      }

    }
  }

  ft
}

ft_handle_footnote <- function(ft, iftnt, font) {

  x <- ft$footer$content$data[[iftnt]][2,]

  purrr::walk(names(font), \(nm) {

    if(nm %in% names(x)) x[nm] <<- font[nm]
  })


  ft$footer$content$data[[iftnt]][2,] <- x

  ft
}
