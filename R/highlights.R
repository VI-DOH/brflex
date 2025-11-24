
ft_add_highlights <- function(ft, highlights) {

  if(!is.list(highlights[[1]])) {
    highlights <- list(highlights)
  }

  lapply(highlights, function(highlight){

    ft <<- ft %>% ft_add_highlight(highlight)
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

      df2 <- df %>% select(highlight$col_nums) %>% rename(V1 = 1, V2 = 2)

      row_nums <- mapply(function(nm, val) {

        df2 %>% mutate(rn = row_number()) %>% filter(V1 == nm & V2 == val) %>%  pull(rn)

      }, highlight$row_names, highlight$row_values)

    }

    fonts <- highlight$fonts
    part <- highlight$part

    cols <- 2:ncol(df)

    # =============================================================
    #   when you delete a column after ft is created,
    #     like when top is selected for subset placement,
    #     ft doesn't alter the dataset, so we track it with
    #      this attribute
    #

    if(attr(ft,"sub_placement") == "top") cols <-  cols - 1

    mapply(function(row_num, fnt) {

      ft <<- ft_add_font(ft, i = row_num, j = cols,
                         ft_text = fnt, part = part )

      ft <<- ft %>%
        bg(i = row_num, j = cols, bg = fnt$shading, part = part)

    },row_nums,fonts)
  }
  ft
}

ft_highlight_row <- function(where, fonts = ft_font(shading.color = "yellow", as.list = TRUE), col_nums = c(1,2), part = "body", as.list = TRUE) {

  nrows <- length(where)
  nfonts <- length(fonts)

  if(nfonts == 1) {
    fonts <- rep(fonts, nrows )
  }

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
    fonts = fonts,
    part = part
  )

  class(x) <- "ft_highlight"

  x
}
