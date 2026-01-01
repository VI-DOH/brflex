
#' Flextable Backgrounds Helper
#'
#'   Helper function to build list of background elements for sections to the
#'   stats table to be use in call to padding()
#'
#' @param table - character (color)
#' @param data  - character (color)
#' @param header  - character (color)
#' @param responses  - character (color)
#' @param stats  - character (color)
#' @param titles  - character (color)
#' @param footer  - character (color)
#' @param subsets  - character (color)
#'
#' @returns list to pass as argument to ft_stats(paddings = list)
#' @export
#'
#' @examples
#' ft_paddings_list(subvars = ft_padding(top = 1))
#'
ft_paddings_list <- function(table = NULL,
                       data = NULL,
                       header = NULL,
                       responses = NULL,
                       stats = NULL,
                       titles = NULL,
                       footer = NULL,
                       subvars = NULL,
                       subsets = NULL){

  structure(
    list(table_padding = table,
         data_padding = data,
         header_padding = header,
         stats_padding = stats,
         titles_padding = titles,
         footer_padding = footer,
         responses_padding = responses,
         subvars_padding = subvars,
         subsets_padding = subsets),
    class = "ft_paddings_list")

}

#' Flextable Padding
#'
#' @param top integer
#' @param bottom integer
#' @param left integer
#' @param right integer
#'
#' @returns list
#' @export
#'
ft_padding <- function(top = NA, bottom = NA, left = NA, right = NA) {

  structure(
    c(top = top,
    bottom = bottom,
    left = left,
    right = right),
    class = "ft_padding"
  )

}

paddings_valid <- function(paddings, pdg) {

  x <- paste0(pdg,"_padding")

  ok <- x %in% names(paddings)
  if(ok) ok <-  !is.null(paddings[[x]])

  ok
}
handle_paddings <- function(ft, paddings) {

  ft <- ft %>%
    handle_padding_table(paddings) %>%
    handle_padding_header(paddings)%>%
    handle_padding_data(paddings) %>%
    # handle_padding_footer(paddings) %>%
    {.}

  ft
}

handle_padding_table <- function(ft, paddings) {

  if(paddings_valid(paddings, "table")) {
    p <- paddings$table_padding

    if(!is.null(p)) {

      ft <- ft %>%
        flextable::padding(padding.top = p["top"],  padding.bottom = p["bottom"],
                           padding.left = p["left"], padding.right = p["right"],
                           part = "all")
    }

  }

  ft
}

handle_padding_data <- function(ft, paddings) {

  if(paddings_valid(paddings, "data")) {
    p <- paddings$data_padding

    if(!is.null(p)) {

      ft <- ft %>%
        flextable::padding(padding.top = p["top"],
                           padding.bottom = p["bottom"],
                           padding.left = p["left"],
                           padding.right = p["right"],
                           part = "body")

    }

  }

  ft <- ft %>%
    handle_padding_subsets(paddings) %>%
    handle_padding_subvars(paddings)

  ft
}


handle_padding_header <- function(ft, paddings) {

  if(paddings_valid(paddings, "header")) {
    p <- paddings$header_padding

    if(!is.null(p)) {

      ft <- ft %>%
        flextable::padding(padding.top = p["top"],
                           padding.bottom = p["bottom"],
                           padding.left = p["left"],
                           padding.right = p["right"],
                           part = "header")

    }

  }

  ft <- ft %>%
    handle_padding_stats(paddings) %>%
    handle_padding_response(paddings) %>%
    handle_padding_titles(paddings)

  ft
}

handle_padding_titles <- function(ft, paddings) {

  if(paddings_valid(paddings, "titles") && ft$header$sections$nrow$titles > 0) {

    p <- paddings$titles_padding

    if(!is.null(p)) {

      irows <- titles_rows(ft)
      nrows <- length(irows)

      if(nrows > 0){
        if(length(p) == 1) {
          ft <- ft %>%
            flextable::padding(i = irows,
                               padding.top = p["top"],
                               padding.bottom = p["bottom"],
                               padding.left = p["left"],
                               padding.right = p["right"],
                               part = "header")
        } else {

          purrr::walk(irows, \(irow) {

            ft <- ft %>%
              flextable::padding(i = irow,
                                 padding.top = p[irow]["top"],
                                 padding.bottom = p[irow]["bottom"],
                                 padding.left = p[irow]["left"],
                                 padding.right = p[irow]["right"],
                                 part = "header")

          })
        }
      }
    }
  }
  ft
}

handle_padding_response <- function(ft, paddings) {

  if(paddings_valid(paddings, "responses") && ft$header$sections$nrow$responses > 0) {
    p <- paddings$responses_padding

    if(!is.null(p)) {

      irows <- responses_rows(ft)

      ft <- ft %>%
        flextable::padding(i = irows,
                           padding.top = p["top"],
                           padding.bottom = p["bottom"],
                           padding.left = p["left"],
                           padding.right = p["right"],
                           part = "header")

    }

  }

  ft
}


handle_padding_stats <- function(ft, paddings) {


  if(paddings_valid(paddings, "stats") && ft$header$sections$nrow$stats > 0) {

    p <- paddings$stats_padding

    if(!is.null(p)) {

      irows <- stats_rows(ft)

      ft <- ft %>%
        flextable::padding(i = irows,
                           padding.top = p["top"],
                           padding.bottom = p["bottom"],
                           padding.left = p["left"],
                           padding.right = p["right"],
                           part = "header")

    }

  }

  ft
}

handle_padding_subsets <- function(ft, paddings) {

  if(paddings_valid(paddings,"subsets") ) {

    p <- paddings$subsets_padding

    if("subset" %in% ft$col_keys) {

      if("subvar" %in% ft$col_keys) {

        irows <- NULL

      } else {

        irows <- subset_rows(ft)

      }

      ft <- ft %>%
        flextable::padding(i = irows, j = "subset",
                           padding.top = p["top"],
                           padding.bottom = p["bottom"],
                           padding.left = p["left"],
                           padding.right = p["right"],
                           part = "body")

    }
  }

  ft
}


handle_padding_subvars <- function(ft, paddings) {

  if(paddings_valid(paddings,"subvars") ) {

        p <- paddings$subvars_padding

    if("subvar" %in% ft$col_keys) {

      irows <- NULL
      jcol <- "subvar"

    } else {

      irows <- subvar_rows(ft)
      jcol <- NULL

    }

    ft <-  ft %>%
      flextable::padding(i = irows, j = jcol,
                         padding.top = p["top"],
                         padding.bottom = p["bottom"],
                         padding.left = p["left"],
                         padding.right = p["right"],
                         part = "body")

  }

  ft
}

handle_padding_footer <- function(ft, paddings) {

  if(paddings_valid(paddings, "footer")) {

    nrows <- ft$footer$content$nrow

    if(nrows > 0){
      if(length(paddings$footer_padding) == 1)
        paddings$footer_padding <- rep(paddings$footer_padding, nrows)

      purrr::walk(1:nrows, \(irow) {

        ft <<- ft %>%
          flextable::padding(i = irow, padding = paddings$footer_padding[irow] , part = "footer")

      })
    }

  }


  ft
}
