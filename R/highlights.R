
ft_add_highlights <- function(ft, highlights) {

  if(!is.list(highlights[[1]])) {
    highlights <- list(highlights)
  }

  lapply(highlights, function(highlight){

    if(class(highlight) == "ft_highlight") ft <<- ft %>% ft_add_highlight(highlight)
    if(class(highlight) == "ft_highlight_if") ft <<- ft %>% ft_add_highlight_if(highlight)

  })

  ft
}

ft_add_highlight_if <- function(ft, highlight) {

  if(class(highlight) != "ft_highlight_if") return(ft)

  pred <- rewrite_quosure(highlight$quo)
  var <- rlang::quo_get_expr(highlight$quo)[[2]] %>% as.character()

  df <- ft$body$dataset %>% mutate(rn = row_number()) %>%
    mutate(across(.cols = starts_with(paste0(var,"^")),.fns =  ~as.integer(.x))) %>%
    filter(if_any(matches(paste0("^", var, "\\^")), pred))

  row_nums <- df %>% pull(rn)

  df <- df %>% select(-rn) %>%
    select(all_of(ft$col_keys))

  col1 <- which(ft$col_keys == "subset")

  cols <- col1:ncol(df)

  font <- highlight$font %>% replace(.,is.na(.),NULL)
  elems <- font %>% names()

  bg <- highlight$bg
  borders <- highlight$borders

  part <- "body"

  purrr::walk(row_nums, \(irow) {
    purrr::walk(cols,\(jcol) {

      # cat("========================================\n",
      #     "[",irow,",",  jcol,"]\n", sep = "")
      #
      purrr::walk(elems,\(elem) {

 #       cat("trying elem: ", elem, "\n")
        if(elem %in% names(ft[[part]]$styles$text)) {
          ft[[part]]$styles$text[[elem]]$data[irow,jcol] <<- font[[elem]]
#          cat("   OK\n")
        } else {
#          cat("   NOPE\n")
        }
      })

      if(!is.null(highlight$bg)) ft[[part]]$styles$cells$background.color$data[irow, jcol] <<- bg

      if(!is.null(highlight$borders)){

        borders <- highlight$borders

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

        print(borders0)

        ft <<- ft %>%  brflex:::make_border(borders = borders0, i = irow, j = jcol, part = part)

      }
    })

  })

  ft

}

ft_add_highlight <- function(ft, highlight) {

  if(class(highlight) != "ft_highlight") return(ft)

  df <- ft$body$dataset


  if(highlight$type == "row") {

    if(!is.null(highlight$row_nums)) {

      row_nums <- highlight$row_nums

    } else {


      nrows = length(highlight$row_names)

      if("subvar" %in% ft$col_keys) {
        df2 <- df %>% select(highlight$col_nums) %>% mutate(rn = row_number())

        row_nums <- mapply(function(nm, val) {
          df2  %>% filter(subvar == nm & subset == val) %>%  pull(rn)

        }, highlight$row_names, highlight$row_values)

      } else {
        row_nums <- df %>% pull(subset) %>% {which(. %in% highlight$row_values)}

      }
    }

    font <- highlight$font %>% replace(.,is.na(.),NULL)
    elems <- font %>% names()

    bg <- highlight$bg
    borders <- highlight$borders

    part <- highlight$part

    cols <- 2:ncol(df)

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

        if(!is.null(highlight$bg)) ft[[part]]$styles$cells$background.color$data[irow, jcol] <<- bg

        if(!is.null(highlight$borders)){

          borders <- highlight$borders

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
  }
  ft
}

#' Conditional Highlighter
#'
#' Format a row with highlighting formats if a condition is met
#'
#' @param where - vector of rows, either integer row numbers or character row names
#' @param font - ft_font object
#' @param bg - ft_bg object
#' @param borders - ft_borders object
#'
#' @returns ft_highlight_if object
#' @export
ft_highlight_if <- function(where , font = NULL,
                            bg = NULL,
                            borders = NULL) {


  quo <- rlang::enquo(where)

  structure(list(
    quo = quo,
    font = font,
    bg = bg,
    borders = borders
  ),
  class = "ft_highlight_if")

}

#' Row Highlighter
#'
#' Format a row with highlighting formats
#'
#' @param where - vector of rows, either integer row numbers or character row names
#' @param font - ft_font object
#' @param bg - ft_bg object
#' @param borders - ft_borders object
#' @param part - character - ft part
#'
#' @returns ft_highlight object
#' @export
#'
ft_highlight_row <- function(where, col_nums = c(1,2),
                             font = ft_font_parts(),
                             bg = NULL,
                             borders = NULL,
                             part = "body") {

  nrows <- length(where)

  if(is.numeric(where)) {
    row_nums = where
    row_names = NULL
    row_values = NULL
  } else {
    row_nums = NULL
    row_names = names(where)
    row_values = unlist(where)
  }

  if(part %in% c("footer")) col_nums = c(1,1)

  x <- list(
    type = "row",
    row_nums = row_nums,
    row_names = row_names,
    row_values = row_values,
    col_nums = col_nums,
    col_names = NULL,
    col_values = NULL,
    font = font,
    bg = bg,
    borders = borders,
    part = part
  )

  class(x) <- "ft_highlight"

  x
}
