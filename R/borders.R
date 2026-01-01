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
ft_borders <- function(top = NA, bottom = NA, midv = NA,
                       left = NA, right = NA, midh = NA) {

  structure(
    list(top = top,
         bottom = bottom,
         left = left,
         right = right,
         midv = midv,
         midh = midh),
    class = "ft_borders"
  )

}

#' @export
print.ft_borders <- function(x, ...) {

  x <- x %>% replace(.,is.na(.), NULL)

  purrr::map(names(x), \(nm) {
    data.frame(what = nm,
               color = x[[nm]][["color"]],
               style = x[[nm]][["style"]],
               width = x[[nm]][["width"]]
               )
    }) %>% bind_rows() %>%
    print()

  invisible()

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
#' @returns ft_borders_list
#' @export
#'
#' @examples
#'
#' solid_brdr <- officer::fp_border(style = "solid", color = "grey11", width = 1)
#' dotted_brdr <- officer::fp_border(style = "dotted", color = "grey11", width = 1)

#' ft_borders_list(
#' responses = ft_borders(left = solid_brdr,
#'                        right = solid_brdr,
#'                        midv = dotted_brdr)
#'
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

make_border <- function(ft, borders, i = NULL,j = NULL, part = "body", verbose = FALSE) {

  if("ft_borders" %in% class(borders)) {

    borders <- borders %>% replace(., is.na(.),NULL)

    brdrs <- names(borders)

    brdrs <- brdrs[grepl("^[trbl]", brdrs)]

    if(is.null(i)) {
      irows <- 1:nrow_part(ft,part)
    } else {
      irows <- i
    }
    if(is.null(j)) {
      jcols <- 1:length(ft$col_keys)
    } else {
      jcols <- j
    }

    purrr::walk(brdrs, \(nm) {
      brdr_data <- borders[nm]

      purrr::map(irows, \(i) {
        purrr::map(jcols, \(j) {
          purrr::walk(c("color","width","style"), \(elem) {


            trbl <- paste0("border.", elem,".", nm)

            if(verbose)  cat(" setting ... ",
                             "ft[[",part, "]]$styles$cells[[", trbl, "]]$data[", i , ", ", j, "] <<-",
                             brdr_data[[nm]][[elem]], "\n")

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

    brdr <- borders$table_borders

    if(!is.null(brdr)) {
      ft <- ft %>%
        make_border(borders = brdr, part = "all")

    }

  }

  ft
}

handle_borders_data <- function(ft, borders) {

  if(borders_valid(borders, "data")) {
    brdr <- borders$data_borders

    if(!is.null(brdr)) {
      ft <- ft %>%
        make_border(borders = brdr, part = "body")

    }

  }

  ft <- ft %>%
    handle_borders_subsets(borders) %>%
    handle_borders_subvars(borders)

  ft
}


handle_borders_header <- function(ft, borders) {

  if(borders_valid(borders, "header")) {

    brdr <- borders$header_borders

    if(!is.null(brdr)) {
      ft <- ft %>%
        make_border(borders = brdr, part = "header")

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

    #   if(!is.null(p)) {
    #
    #     irows <- titles_rows(ft)
    #     nrows <- length(irows)
    #
    #
    #     if(nrows > 0){
    #       if(class(p) == "ft_borders") {
    #         ft <- ft %>%
    #           flextable::border(i = irows,
    #                             border.top = p["top"],
    #                             border.bottom = p["bottom"],
    #                             border.left = p["left"],
    #                             border.right = p["right"],
    #                             part = "header")
    #       } else {
    #
    #         purrr::walk(irows, \(irow) {
    #           browser()
    #           ft <- ft %>%
    #             flextable::border(i = irow,
    #                               border.top = p[[irow]]["top"],
    #                               border.bottom = p[[irow]]["bottom"],
    #                               border.left = p[[irow]]["left"],
    #                               border.right = p[[irow]]["right"],
    #                               part = "header")
    #
    #         })
    #       }
    #     }
    #   }
  }
  ft
}

handle_borders_response_OLD <- function(ft, borders) {

  if(borders_valid(borders, "responses") && ft$header$sections$nrow$responses > 0) {

    brdr <- borders$responses_borders

    if(!is.null(brdr)) {

      irows <- responses_rows(ft)

      ft <- ft %>%
        make_border(i = irows, borders = brdr, part = "header")

    }

  }

  ft

}

handle_borders_response <- function(ft, borders) {

  if(borders_valid(borders, "responses") && ft$header$sections$nrow$responses > 0) {

    brdr <- borders$responses_borders

    if(!is.null(brdr)) {

      irow_stats <- stats_rows(ft)
      irows <- NULL

      df_types <- response_col_types(ft)

      # ------------ handle the top rows -------------------

      if(!all(is.na(brdr$left))) {

        jcols <- df_types %>% filter(left) %>% pull(jcol)

        brdtmp <- brdr
        brdtmp$right <- brdtmp$midv

        ft <- ft %>%
          make_border(j = jcols, i = irows, borders = brdtmp, part = "body")

        ft <- ft %>%
          make_border(j = jcols, i = irow_stats, borders = brdtmp, part = "header")

      }

      # ------------ handle the right rows -------------------

      if(!all(is.na(brdr$right))) {

        jcols <- df_types %>% filter(right) %>% pull(jcol)

        brdtmp <- brdr
        brdtmp$left <- brdtmp$midv

        ft <- ft %>%
          make_border(j = jcols, i = irows, borders = brdtmp, part = "body")

        ft <- ft %>%
          make_border(j = jcols, i = irow_stats, borders = brdtmp, part = "header")
      }

      # ------------ handle the middle rows -------------------

      if(!all(is.na(brdr$midv))) {
        jcols <- df_types %>% filter(mid) %>% pull(jcol)

        brdtmp <- brdr
        brdtmp$left <- brdtmp$midv
        brdtmp$right <- brdtmp$midv

        ft <- ft %>%
          make_border(j = jcols, i = irows, borders = brdtmp, part = "body")

        ft <- ft %>%
          make_border(j = jcols, i = irow_stats, borders = brdtmp, part = "header")
      }

      # ------------ handle the middle rows -------------------

      brdtmp <- brdr
      brdtmp$left <- NA
      brdtmp$right <- NA

      ft <- ft %>%
        make_border(j = jcols, borders = brdtmp, part = "body")

    }

  }

  ft
}

handle_borders_subvars <- function(ft, borders) {


  if(borders_valid(borders, "subvars")) {
    brdr <- borders$subvars_borders

    if(!is.null(brdr)) {

      jcols <- which(ft$col_keys == "subvars")

      ft <- ft %>%
        make_border(j = jcols, borders = brdr, part = "body")

    }

  }

  ft
}

handle_borders_subsets <- function(ft, borders) {

  #
  #     subsets are a little different ...
  #     we often want to have a different border between the top and the bottom
  #     this is done using the midh part of the borders, as in middle rows horizontal borders

  if(borders_valid(borders, "subsets")) {
    brdr <- borders$subsets_borders

    if(!is.null(brdr)) {

      jcols <- NULL #which(ft$col_keys == "subset")

      df_types <- subvar_row_types(ft)

      # ------------ handle the top rows -------------------

      if(!all(is.na(brdr$top))) {

        irows <- df_types %>% filter(top) %>% pull(irow)

        brdtmp <- brdr
        brdtmp$bottom <- brdtmp$midh
        brdtmp$left <- NA
        brdtmp$right <- NA

        ft <- ft %>%
          make_border(i = irows, j = jcols, borders = brdtmp, part = "body")

      }

      # ------------ handle the bottom rows -------------------

      if(!all(is.na(brdr$bottom))) {

        irows <- df_types %>% filter(bottom) %>% pull(irow)

        brdtmp <- brdr
        brdtmp$top <- NA
        brdtmp$left <- NA
        brdtmp$right <- NA

        ft <- ft %>%
          make_border(i = irows, j = jcols, borders = brdtmp, part = "body")

      }

      # ------------ handle the middle rows -------------------

      if(!all(is.na(brdr$midh))) {
        irows <- df_types %>% filter(mid) %>% pull(irow)

        brdtmp <- brdr
        brdtmp$top <- brdtmp$midh
        brdtmp$bottom <- brdtmp$midh
        brdtmp$left <- NA
        brdtmp$right <- NA

        ft <- ft %>%
          make_border(i = irows, j = jcols, borders = brdtmp, part = "body")
      }

      # ------------ handle the middle rows -------------------

      brdtmp <- brdr
      brdtmp$top <- NA
      brdtmp$bottom <- NA

      ft <- ft %>%
        make_border(j = "subset", borders = brdtmp, part = "body")

    }

  }

  ft
}

handle_borders_stats <- function(ft, borders) {


  if(borders_valid(borders, "stats")) {
    brdr <- borders$stats_borders

    if(!is.null(brdr)) {

      irows <- stats_rows(ft)

      ft <- ft %>%
        make_border(i = irows, borders = brdr, part = "header")

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

ft_blank_border <-  function() { fp_border(color = "white", style = "none", width = 0) }


ft_box_table <- function(ft, box) {

  if(box$style == "none") box$width <- 0

  box_border <-  fp_border(color = box$color,
                           style = box$style,
                           width = box$width )

  sides <- strsplit(box$what, "")[[1]]

  top <- if("t" %in% sides) top <- box_border else top <- ft_blank_border()
  bottom <- if("b" %in% sides) bottom <- box_border else bottom <- ft_blank_border()
  left <- if("l" %in% sides) left <- box_border else left <- ft_blank_border()
  right <- if("r" %in% sides) right <- box_border else right <- ft_blank_border()

  ft %>%
    flextable::vline_left(border = left) %>%
    flextable::vline_right(border = right) %>%
    flextable::hline_top(border = top, part = "header") %>%
    flextable::hline_bottom(border = bottom, part = "footer")

}

ft_grid_table <- function(ft, grid) {

  nrow_body <- ft %>%  nrow_part("body")

  if(grid$style == "none") grid$width  <-  0

  ft %>% border(i = 1:nrow_body, j = 1:ncol_keys(ft),  border=grid, part = "body")

}

#' Build Box Info Object
#'
#' @param what - character
#' @param color - character
#' @param] width - integer
#' @param style  - character
#'
#' @returns ft_box
#' @export
#'
#' @examples
#'
#' box = ft_box(what = "trl", color = "black", style = "dotted")
#'
ft_box <- function(what = "lrtb", color = "black",  width = 1, style = "solid"){


  structure(list(
    what = what,
    color = color,
    width = width,
    style = style
  ), class = "ft_box")
}

