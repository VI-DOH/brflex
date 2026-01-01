
#' @importFrom magrittr %>%
#'

# handle_fonts <- function(ft, fonts) {
#
#   #  ==== split header font into one for responses and one for stats
#   #            if there's only one font object (nheader_fonts == 1) ,
#   #                the first and last will be the same ...
#   #             header_font[[1]] == header_font[[nhdrfnts]]
#
#     if(length(header_font) > 6) header_font <- list(header_font)
#
#   nhdrfnts <- length(header_font)
#   resp_hdrfont <- header_font[[1]]
#   stats_hdrfont <- header_font[[nhdrfnts]]
#
#   if(is.null(fonts$table)) {
#     table_font  <-   default_font
#   } #else {
#   table_font <- table_font[[1]]
#   # }
#
#   if(is.null(footer_fonts)) {
#     footer_fonts <- default_font
#   } #else {
#   footer_fonts <- footer_fonts[[1]]
#   # }
#
#   if(is.null(data_font)) {
#     data_font  <-   table_font
#   } else {
#     data_font <- data_font[[1]]
#   }
#
#   if(is.null(subset_font)) {
#     subset_font  <-   table_font
#   } else {
#     subset_font <- subset_font[[1]]
#   }
#
#   if(is.null(header_font)) {
#     header_font  <-   table_font
#   }
#
#   if(is.null(title_fonts)) {
#     title_fonts  <-   list(table_font)
#   }
#   ft
# }


ft_stats_html <- function(df = NULL, coi, subsets = NULL, digits = 2, responses = ".*", ...) {


  if(is.null(responses)) responses <- ".*"

  if(is.null(df)) df <- prepped_data()

  df_stats <-  brfss::survey_stats(df, coi = coi, pct = TRUE, digits = digits, subsets = subsets)

  df_stats <- df_stats %>%
    filter(grepl(responses, response))

  if(is.null(df_stats)) return(NULL)

  if(length(subsets) == 0) df_stats <- df_stats %>% select(-matches("subvar"))

  ft_stats(df_stats, ...)

}

#
# ft_border <- function(color = "black", style = "solid", width = 1) {
#
#   if(style == "none") width <-  0
#
#   fp_border(color = color,style = style, width = width)
#
# }
#

ft_merge_font <- function(x = NULL, color = NULL, font = NULL, font.size = NULL,
                          bold = NULL, italic = NULL,
                          underlined = NULL,
                          vertical.align = NULL,
                          shading.color = NULL) {

  if(!is.null(color)) x[[1]]["color"] <- color
  if(!is.null(font.size)) x[[1]]["font.size"] <- font.size
  if(!is.null(bold)) x[[1]]["bold"] <- bold
  if(!is.null(italic)) x[[1]]["italic"] <- italic
  if(!is.null(underlined)) x[[1]]["underlined"] <- underlined
  if(!is.null(font)) x[[1]]["font.family"] <- font
  if(!is.null(vertical.align)) x[[1]]["vertical.align"] <-vertical.align
  if(!is.null(shading.color)) x[[1]]["shading.color"] <- shading.color

  x

}


gsub_attr <- function(x, attr, delim = "{}") {

  delim1 <- substr(delim,1,1)
  delim2 <- substr(delim,nchar(delim),nchar(delim))

  if(grepl('[[:punct:]]', delim1)) delim1 <- paste0("\\",delim1)
  if(grepl('[[:punct:]]', delim2)) delim2 <- paste0("\\",delim2)

  pttrn <- paste0(".*", delim1, "(*.*)", delim2, ".*")

  ret <-   sapply(x, function(x0){

    if(grepl(pttrn, x0)) {

      #  ====  figure out which attribute and assign that to the title   =====

      attrib <- gsub(pttrn,"\\1",x0)
      attrib_val <- attr[attrib]

      x0 <- gsub(paste0("{", attrib, "}"), attrib_val, x0, fixed = TRUE)

      #%>% split_before(title_max_char,collapse = "\n")

    }
    x0
  }) %>%  unname()

  ret
}

my_na <- function(x) {

  my_class <- class(x)
  browser()
  na_value <- switch(
    my_class,
    "integer" = NA_integer_,
    "numeric" = NA_real_,
    "logical" = NA,
    "character" = NA_character_,
    "factor" = factor(NA, levels = levels(x)),
    NA  # fallback
  )
  na_value
}

ft_add_data <- function(ft, df) {

  ncols <- ncol(df)

  last_sub <- ""
  num_cols <- grep("num^", colnames(df))

  df <- df %>% mutate(across(starts_with("num"), as.integer))

  ft$body$dataset <-ft$body$dataset %>% mutate(across(starts_with("num"), as.integer))
  new_row <- df[0, ] %>% add_row(subvar = "", subset = "")

  frow <- 0  # the actual flextable row

  sapply(1:nrow(df), function(irow) {   # the data table row

    #  ----  x will be the row of data   ------------

    x <- df[irow,]

    x_lst <- x %>% as.list()

    #  -----  get this subvar ... if it has changed we need to insert a subset name row

    subv <- x_lst[["subvar"]]

    if(irow>1 && !is.null(subv) && subv != last_sub) {

      frow <<- frow + 1

      new_row[1,"subvar"] <<- ""
      new_row[1,"subset"] <<- subv

      nr_list <- as.list(new_row)

      #ft$body$dataset <<-  ft$body$dataset %>% add_row(new_row)

      ft <<- ft %>%
        add_body(top = FALSE, values = nr_list) %>%
        merge_h_range(i = frow, j1 = 2, j2 = ncols)

    }

    frow <<- frow + 1

    #ft$body$dataset <<-  ft$body$dataset %>% add_row(x)

    ft <<- ft %>%
      add_body(top = FALSE, values = x_lst)%>%
      align(i = frow,j = 3:ncols, align = "right") %>%
      hline(i = frow, border = ft_blank_border())

    last_sub <<- subv
  })

  rspans <- ft$body$spans$rows[,-1]

  ft <- ft %>% delete_columns(j=1)

  ft$body$spans$rows <- rspans

  ft


}

which_subvar <- function(subvar, subvars) {

  x <- sapply(subvar, function(subv) {
    y <- which(subvars %in% subv)

    if(length(y) == 0) y <- 0

    y
  })


  x
}

ft_add_titles <- function(ft, titles, title_spacing = 1.0) {

  ntitles <- length(titles)

  title_rows <- 1:ntitles

  if(ntitles > 0) {

    df <- ft$body$dataset

    titles <-   sapply(titles, function(title){
      if(grepl("\\{(.*)\\}", title)) {

        #  ====  figure out which attribute and assign that to the title   =====

        attrib <- gsub(".*\\{(.*)\\}.*","\\1",title)
        attrib_val <- attr(df_stats, attrib)

        title <- gsub(paste0("{", attrib, "}"), attrib_val, title, fixed = TRUE)

      }
      title
    }) %>%  unname()

    irow <- 0

    sapply(titles, function(title) {
      irow <<- irow + 1
      ft <<- ft %>%
        flextable::add_header_lines (top = FALSE, values = title) %>%
        flextable::align(i = irow,align = "center", part = "header")
    })

    ft <- ft %>%
      flextable::padding(i = title_rows, padding.top = 2, padding.bottom = 2,
                         part = "header") %>%
      flextable::line_spacing(i = title_rows, space = title_spacing, part = "header")
  }

  ft

}

ft_add_resp_hdr <- function(ft, values) {

  resp_row <-  nrow_part(ft, part = "header") + 1
  vline_cols <- which(values != "")  - 1

  ft <- ft %>%
    flextable::add_header_row (top = FALSE, values = values) %>%
    flextable::merge_h(i = resp_row, part = "header")  %>%
    flextable::align(i = resp_row, align = "center", part = "header") #%>%
  # flextable::vline(i = resp_row, j = vline_cols, border = fp_border(),
  #                  part = "header")

  ft

}

ft_add_stats_hdr <- function(ft, values) {

  stat_n <- tail(values,1)

  ## vline_cols <- which(values %in% c("Population", "den", stat_n)) -1
  #vline_cols <- which(values %in% c( "den", stat_n)) -1

  stats_row <-  nrow_part(ft, part = "header") + 1

  ft <- ft %>%
    flextable::add_header_row (top = FALSE, values = values)  %>%

    # flextable::vline(i = stats_row,  border = ft_blank_border(),
    #                  part = "header") %>%
    # flextable::vline(i = stats_row, j = vline_cols, border = fp_border(),
    #                  part = "header") %>%
    # flextable::hline(i = stats_row, border = fp_border(),
    #                  part = "header") %>%
    flextable::align(i = stats_row,
                     align = "center", part = "header")
  ft

}


ft_strata <- function(df = NULL) {

  extent <- brfss.param("extent")
  monthly <- (extent == "monthly")

  if(is.null(df)) {
    if(monthly)
      df <- monthly_data() else
        df <- brfss_data()
  }

  year <- brfss.param(year)
  monthly_text <- ifelse(monthly," Monthly", "")

  df %>%
    count(FMONTH, `_GEOSTR` ) %>%
    tidyr::pivot_wider(names_from = `_GEOSTR`, values_from = n) %>%
    replace(is.na(.),0) %>%
    as.data.frame() %>%
    flextable() %>%
    flextable::colformat_int(j = 2:5, big.mark = "") %>%
    add_header_row(values = c("","Landline", "Cell"), colwidths = c(1,2,2)) %>%
    add_header_row(values = c("","Geostrata"), colwidths = c(1,4)) %>%
    add_header_lines(values = paste0(year, monthly_text, " BRFSS Data")) %>%
    align(align = "center", part = "header")

}

ft_dispo_codes <- function(df_brfss, type , data_col = NULL){

  df <- df_brfss %>% disp_categ(type = type)

  if(is.null(data_col)) data_col <- "Strata"
  cats <- df %>% pull(Code) %>% substring(1,1)
  nxt <- tail(cats,-1)
  cats <- head(cats, -1)
  hl <- which(cats!=nxt)

  data_cols <- grep("^ST",colnames(df))
  tot_col <- which(colnames(df) %in% "Total")
  pct_col <- which(colnames(df) %in% c("Percent", "Pct"))

  flextable::flextable(data = df) %>%
    width(j = 1, width = 0.3) %>%
    width(j = 2, width = 5) %>%
    width(j = data_cols,  width = 0.33) %>%
    width(j = tot_col,  width = 0.55) %>%
    width(j =  pct_col,  width = 0.65) %>%
    padding(padding.top = 2,padding.bottom = 1, part = "body") %>%
    hline(i = hl)
}


ft_questions <- function(x, use_label = FALSE) {

  lo_mgr <- brfss::Layout_Mgr$new()
  lo_mgr$layout_from_data()
  df_lo <- lo_mgr$data_layout()

  df_cols <- lo_mgr$section_col_names(x)

  df_cols <-  df_cols %>% as.data.frame() %>%
    rename(col_name = 1)

  xcol <- ifelse(use_label, "label", "question")

  df_lo %>% inner_join(df_cols) %>%
    select(col_name, {{xcol}}) %>%
    flextable() %>%
    width(j = xcol, width = 5)


}

split_before <- function(x, before = 80, collapse = NULL) {

  sp2 <- rawToChar(as.raw(c(0xc2,0xa0)))

  ret <- sapply(x, function(str) {
    str_rem <- str

    parts <- character(0)

    while(nchar(str_rem)>= before) {
      words <- str_rem %>% stringr::str_split(pattern = sp2) %>% unlist() %>%
        stringr::str_split(pattern = " ") %>% unlist()

      split <- as.data.frame(words) %>%
        mutate(nspace = row_number())%>%
        mutate(len = nchar(words)) %>%
        mutate(running = cumsum(len) + nspace) %>%
        filter(running <= before) %>%
        tail(1) %>%
        pull(running)

      part <- substring(str_rem,1,split-1)

      parts <- c(parts,part)

      str_rem <- substring(str_rem, split+1)
    }

    parts <- c(parts,str_rem)

    if(!is.null(collapse)) {

      parts <- paste0(parts,collapse = collapse)
    }
    parts
  })

  ret
}

handle_widths <- function(ft, widths) {

  invisible(
    mapply(function(col, width) {

      key_names <-  gsub("^.*", "",ft$col_keys)

      cols <- which(key_names == col)

      if(length(cols) == 0) {
        key_names <- ft$header$content$data %>% tail(1) %>%
          purrr::map(~.x["txt"][[1]]) %>%
          purrr::map(~.x[1]) %>% as.character()
        cols <- which(key_names == col)
      }

      if(length(cols) > 0) ft <<- ft %>%  flextable::width(j = cols, width = width)
    }, names(widths), widths)

  )

  ft
}

handle_aligns <- function(ft, aligns) {

  key_names <-  gsub("\\^.*", "",ft$col_keys)

  key_names2 <- ft$header$content$data %>% tail(1) %>%
    purrr::map(~.x["txt"][[1]]) %>%
    purrr::map(~.x[1]) %>% as.character()

  invisible(
    mapply(function(col, align) {

      if(col == "subset") {

        cols <- which(key_names == "subset")

        if(ft$properties["sub_placement"] == "top") {

          irows <- which(ft$body$spans$rows[,cols] == 1)
        } else {
          irows = NULL
        }
      } else {


        cols <- which(key_names == col)

        if(length(cols) == 0) {
          cols <- which(key_names2 == col)
        }

        irows = NULL
      }

      if(length(cols) > 0) {
        ft <<- ft %>%
          flextable::align(i = irows, j = cols, align = align)
      }

    }, names(aligns), aligns)

  )

  ft
}
