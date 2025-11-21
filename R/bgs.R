
#' Flextable Backgrounds Helper
#'
#'   Helper function to build list of background elements for sections to the
#'   stats table to be use in call to bg()
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
#' @returns list to pass as argument to ft_stats(bgs = list)
#' @export
#'
#' @examples
ft_bg_list <- function(table = NULL,
                       data = NULL,
                       header = NULL,
                       responses = NULL,
                       stats = NULL,
                       titles = NULL,
                       footer = NULL,
                       subvars = NULL,
                       subsets = NULL){

  structure(
    list(table_bg = table,
       data_bg = data,
       header_bg = header,
       stats_bg = stats,
       titles_bg = titles,
       footer_bg = footer,
       responses_bg = responses,
       subvars_bg = subvars,
       subsets_bg = subsets),
    class = "ft_bgs_list")

}

handle_bgs <- function(ft, bgs) {

  ft <- ft %>%
    handle_bg_table(bgs) %>%
    handle_bg_header(bgs)%>%
    handle_bg_data(bgs)%>%
    handle_bg_footer(bgs)

  ft
}

bg_valid <- function(bgs, bg) {

  x <- paste0(bg,"_bg")

  ok <- x %in% names(bgs)
  if(ok) ok <-  !is.null(bgs[[x]])

  ok
}

handle_bg_table <- function(ft, bgs) {

  if(bg_valid(bgs,"table")) {

    ft <- ft %>%
      flextable::bg(bg = bgs$table_bg, part = "all")

  }

  ft
}

handle_bg_header <- function(ft, bgs) {

  if(bg_valid(bgs,"header")) {

    ft <- ft %>%
      flextable::bg(bg = bgs$header_bg, part = "header")
  }

  ft <- ft %>%
    handle_bg_titles(bgs = bgs) %>%
    handle_bg_response(bgs = bgs) %>%
    handle_bg_stats(bgs = bgs)

  ft
}

handle_bg_titles <- function(ft, bgs) {

  if(bg_valid(bgs,"titles") && ft$header$sections$nrow$titles > 0) {

    irows <- titles_rows(ft)
    nrows <- ft$header$sections$nrow$titles

    if(nrows > 0){
      if(length(bgs$titles_bg) == 1) bgs$titles_bg <- rep(bgs$titles_bg, nrows)

      purrr::walk(irows, \(irow) {

        ft <<- ft %>%
          flextable::bg(i = irow, bg = bgs$titles_bg[irow] , part = "header")

      })
    }
  }

  ft
}

handle_bg_response <- function(ft, bgs) {

  if(bg_valid(bgs,"responses")  && ft$header$sections$nrow$responses > 0) {

    irows <- responses_rows(ft)

    ft <- ft %>%
      flextable::bg(i = irows, bg = bgs$response_bg , part = "header")

  }

  ft
}

handle_bg_data <- function(ft, bgs) {

  if(bg_valid(bgs,"data")) {

    ft <- ft %>%
      flextable::bg( bg = bgs$data_bg , part = "body")
  }


  ft <- ft %>%
    handle_bg_subsets(bgs) %>%
    handle_bg_subvars(bgs)

  ft
}

handle_bg_subsets <- function(ft, bgs) {

    if(bg_valid(bgs,"subsets") ) {

      if("subset" %in% ft$col_keys) {

        if("subvar" %in% ft$col_keys) {

          irows <- NULL

        } else {

          irows <- subset_rows(ft)

        }

        ft <- ft %>%
          flextable::bg(i = irows, j = "subset", bg = bgs$subsets_bg , part = "body")
      }
    }

    ft
  }


  handle_bg_subvars <- function(ft, bgs) {

    if(bg_valid(bgs,"subvars") ) {
      if("subvar" %in% ft$col_keys) {

        irows <- NULL
        jcol <- "subvar"

      } else {

        irows <- subvar_rows(ft)
        jcol <- NULL

      }

      ft <- ft %>%
        flextable::bg(i = irows, j = jcol, bg = bgs$subvars_bg , part = "body")
    }

    ft
  }


  handle_bg_stats <- function(ft, bgs) {

    if(bg_valid(bgs,"stats") && ft$header$sections$nrow$stats > 0) {

      irows <- stats_rows(ft)

      ft <- ft %>%
        flextable::bg(i = irows, bg = bgs$stats_bg , part = "header")

    }

    ft
  }

  handle_bg_footer <- function(ft, bgs) {

    if(bg_valid(bgs,"footer")) {

      nrows <- ft$footer$content$nrow

      if(nrows > 0){
        if(length(bgs$footer_bg) == 1) bgs$footer_bg <- rep(bgs$footer_bg, nrows)

        purrr::walk(1:nrows, \(irow) {

          ft <<- ft %>%
            flextable::bg(i = irow, bg = bgs$footer_bg[irow] , part = "footer")

        })
      }

    }

    ft
  }
