#' Flextable cell (or range) borders
#'
#' @param top fp_border
#' @param bottom fp_border
#' @param left fp_border
#' @param right fp_border
#'
#' @returns list
#' @export
#'
ft_borders <- function(top = NA, bottom = NA, left = NA, right = NA) {

  structure(list(top = top,
                 bottom = bottom,
                 left = left,
                 right = right
  ), class = "ft_borders"
  )

}

#' List of Borders
#'
#' @param table - ft_borders
#' @param data - ft_borders
#' @param header - ft_borders
#' @param responses - ft_borders
#' @param stats - ft_borders
#' @param titles - ft_borders
#' @param footer - ft_borders
#' @param subvars - ft_borders
#' @param subsets - ft_borders
#'
#' @returns
#' @export
#'
#' @examples
ft_borders_list <- function(table = NULL,
                            data = NULL,
                            header = NULL,
                            responses = NULL,
                            stats = NULL,
                            titles = NULL,
                            footer = NULL,
                            subvars = NULL,
                            subsets = NULL){

  structure(
    list(table_borders = table,
       data_borders = data,
       header_borders = header,
       stats_borders = stats,
       titles_borders = titles,
       footer_borders = footer,
       responses_borders = responses,
       subvars_borders = subvars,
       subsets_borders = subsets),
    class = "ft_borders_list")

}

borders_valid <- function(borders, brdr) {

  x <- paste0(brdr,"_borders")

  ok <- x %in% names(borders)
  if(ok) ok <-  !is.null(borders[[x]])

  ok
}

make_border <- function(ft, borders, i = NULL,j = NULL, part = "body") {

  if("ft_borders" %in% class(borders)) {

    borders <- borders %>% replace(., is.na(.),NULL)
    brdrs <- names(borders)
    if(is.null(i)) {
      irows <- 1:nrow_part(ft,part)
    } else {
      irows <- i
    }
    if(is.null(j)) {
      jcols <- 1:length(ft$col_keys)
    } else {
      j
    }

    purrr::walk(brdrs, \(nm) {
      brdr_data <- borders[nm]

      purrr::walk(c("color","width","style"), \(elem) {

        purrr::map(irows, \(i) {
          purrr::map(jcols, \(j) {

            trbl <- paste0("border.", elem,".", nm)
            cat(" setting ... ",
                "ft[[",part, "]]$styles$cells[[", trbl, "]]$data[", i , ", ", j, "] <<-",
                " brdr_data[[", nm, "]][[", elem, "]]", "\n")

            ft[[part]]$styles$cells[[trbl]]$data[i,j] <<- brdr_data[[nm]][[elem]]
          })
        })
      })
    })
  }


  ft
}

handle_borders <- function(ft, borders) {

  ft <- ft %>%
    handle_borders_table(borders) %>%
    handle_borders_header(borders)%>%
    handle_borders_data(borders) %>%
    # handle_borders_footer(borders) %>%
    {.}

  ft
}

handle_borders_table <- function(ft, borders) {

  if(borders_valid(borders, "table")) {
    p <- borders$table_border

    if(!is.null(p)) {

      ft <- ft %>%
        flextable::border(border.top = p["top"],  border.bottom = p["bottom"],
                          border.left = p["left"], border.right = p["right"],
                          part = "all")
    }

  }

  ft
}

handle_borders_data <- function(ft, borders) {

  if(borders_valid(borders, "data")) {
    brdr <- borders$data_borders

    if(!is.null(brdr)) {
      ft <- ft %>%
        make_border(borders = brdr, part = "bodt")

    }

  }

  ft
}


handle_borders_header <- function(ft, borders) {

  if(borders_valid(borders, "header")) {
    p <- borders$header_border

    if(!is.null(p)) {

      ft <- ft %>%
        flextable::border(border.top = p["top"],
                          border.bottom = p["bottom"],
                          border.left = p["left"],
                          border.right = p["right"],
                          part = "header")

    }

  }

  ft <- ft %>%
    handle_borders_stats(borders) %>%
    handle_borders_response(borders) %>%
    handle_borders_titles(borders)

  ft
}

handle_borders_titles <- function(ft, borders) {

  if(borders_valid(borders, "titles") && ft$header$sections$nrow$titles > 0) {
    p <- borders$titles_border

    if(!is.null(p)) {

      irows <- title_rows(ft)

      if(nrows > 0){
        if(length(p) == 1) {
          ft <- ft %>%
            flextable::border(i = irows,
                              border.top = p["top"],
                              border.bottom = p["bottom"],
                              border.left = p["left"],
                              border.right = p["right"],
                              part = "header")
        } else {

          purrr::walk(irows, \(irow) {

            ft <- ft %>%
              flextable::border(i = irow,
                                border.top = p[irow]["top"],
                                border.bottom = p[irow]["bottom"],
                                border.left = p[irow]["left"],
                                border.right = p[irow]["right"],
                                part = "header")

          })
        }
      }
    }
  }
  ft
}

handle_borders_response <- function(ft, borders) {

  if(borders_valid(borders, "responses") && ft$header$sections$nrow$responses > 0) {
    p <- borders$responses_border

    if(!is.null(p)) {

      irows <- responses_rows(ft)

      ft <- ft %>%
        flextable::border(i = irows,
                          border.top = p["top"],
                          border.bottom = p["bottom"],
                          border.left = p["left"],
                          border.right = p["right"],
                          part = "header")

    }

  }

  ft
}


handle_borders_stats <- function(ft, borders) {


  if(borders_valid(borders, "stats") && ft$header$sections$nrow$stats > 0) {

    p <- borders$stats_border

    if(!is.null(p)) {

      irows <- stats_rows(ft)

      ft <- ft %>%
        flextable::border(i = irows,
                          border.top = p["top"],
                          border.bottom = p["bottom"],
                          border.left = p["left"],
                          border.right = p["right"],
                          part = "header")

    }

  }

  ft
}

handle_borders_footer <- function(ft, borders) {

  if(borders_valid(borders, "footer")) {

    nrows <- ft$footer$content$nrow

    if(nrows > 0){
      if(length(borders$footer_border) == 1)
        borders$footer_border <- rep(borders$footer_border, nrows)

      purrr::walk(1:nrows, \(irow) {

        ft <<- ft %>%
          flextable::border(i = irow, border = borders$footer_border[irow] , part = "footer")

      })
    }

  }


  ft
}
