
#' BRFSS Stats to Flextable
#'
#' Takes the results of a call to survey_stats() and turns it into a flextable object.
#'   This function gets some information from the attributes of the stats table (df_stats)
#'
#' @param df_stats a brfss_stats classed data frame
#' @param ... other args
#' @param coi character column of interest
#' @param population character
#' @param stats character
#' @param exclude character
#' @param rename named character
#' @param widths integer
#' @param align character
#' @param subset_placement character
#' @param col_padding integer
#' @param default_font fp_font object
#' @param line_spacing integer
#' @param title_spacing integer
#' @param response character
#' @param titles character
#' @param title_max_char integer
#' @param highlights highlights object
#' @param footers footers
#' @param footnotes footnotes object
#' @param box box
#' @param grid grid
#'
#' @return flextable object
#' @export
#'
ft_stats <- function(df_stats, ...,
                     coi = NULL,
                     population = NULL,
                     stats = c("den", "num", "percent", "ci"),
                     exclude = "^$",
                     responses = ".*",
                     rename = c(percent = "pct"),
                     widths = c(subset = 3, ci = 1),
                     digits = 1,
                     aligns = c(ci = "center"),
                     subset_placement = "left",
                     col_padding = NULL, #c(percent = 0.05, den = 0.1),
                     line_spacing = 1.0,
                     title_spacing = 1.0,
                     titles = NULL,
                     title_max_char = 9999,
                     highlights = NULL,
                     highlights_mgr = NULL,
                     footers = NULL,
                     footnotes = TRUE,
                     borders_mgr = NULL,
                     bgs_mgr = NULL,
                     fonts_mgr = NULL,
                     paddings = list(),
                     box = NULL,
                     grid = NULL) {



  #  ==== if the stats table is NULL or empty, return NULL

  if(any(c("brfss_data", "brfss_prepped") %in% class(df_stats))) {
    if(missing(..1)) {
      return(NULL)
    }
    coi <- rlang::as_name(enquos(...)[[1]])
    df_stats <- survey_stats(df_stats, coi = coi, digits = 1,
                             pct = TRUE)

  } else if(! "brfss_stats" %in% class(df_stats)){

    message(paste0("=======================================\n",
                   "Invalid BRFSS stats data\n",
                   "========================================"))
    return(NULL)
  }

  df_stats <- df_stats %>%
    filter(!grepl(exclude, subset)) %>%
    filter(grepl(responses, response))

  if(nrow(df_stats) == 0) {

    message(paste0("===================================================\n",
                   "Invalid responses filter ... no matching responses\n",
                   "==================================================="))
    return(NULL)
  }

  #  ====  get the attributes from the stats df to use later  ============
  #  =========    right now for use in title substitutions  =========

  stats_attrs <- attributes(df_stats)

  # =======  apply default fonts if necessary  ==============

  flextable::set_flextable_defaults()


  # ==========================================================================
  #
  #                we're good to go

  coi <- attr(df_stats, "coi")

  df_stats <- df_stats %>% set_population(population)


  #  character for separating stats and vals

  sep_char <- "^"


  # add ci text column (CI_Lower-CI_upper)

  df_stats_rpt <- df_stats

  if("ci" %in% stats && "CI_lower" %in% colnames(df_stats_rpt)) {
    df_stats_rpt <- df_stats_rpt %>%
      mutate(ci = paste0(CI_lower, "-", CI_upper))
  }
  # get the set of valid values

  vals <- df_stats_rpt %>% pull(response) %>% unique()
  nvals <- length(vals)

  stats_in <- df_stats_rpt %>%
    select(where(is.numeric), matches("^ci$")) %>%
    colnames() %>%
    {.[. != "year"]}

  stats_rm <- stats_in %>% {.[!. %in% stats]}

  df_stats_rpt <- df_stats_rpt %>% select(-all_of(stats_rm))

  stats <- stats %>% {.[!. %in% "den"]} %>% {.[.%in% stats_in]}

  cnames <- df_stats_rpt %>% colnames()
  nstats <- length(stats)

  static_cols <- cnames %>% {.[!. %in% c(stats, "response", "suppress", "year")]}

  nstatic <- length(static_cols)

  has_subvar <- "subvar" %in% static_cols

  if(has_subvar) subvars <- df_stats %>%
    pull(subvar) %>%
    unique()
  else subvars  <-  character(0)

  df_tbl <- widen(df_stats_rpt)

  df_suppress <- df_tbl %>%
    select(subvar, subset, starts_with("suppress"))

  df_tbl <- df_tbl %>% select(-starts_with("suppress"))

  df_hdr_rows <- wide_header_rows(df_tbl)

  if(has_subvar) {

    df_tbl <- df_tbl %>%
      group_by(subvar) %>%
      mutate(rn = row_number()) %>%
      ungroup() %>%
      mutate(my_sub = which_subvar(subvar = subvar, subvars = subvars) ) %>%
      arrange(my_sub, rn) %>%
      select(-my_sub, -rn) %>%
#      mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, .x))) %>%
      mutate(across(where(is.character), ~if_else(is.na(.x), "-", .x)))
  }


  val_hdr <- df_tbl %>% colnames() %>% grep(sep_char,., fixed = T, value = T) %>%
    gsub("(.*)\\^(.*)","\\2",.)

  hdr_2_stats <- c(static_cols, rep(stats, nvals))

  denom_col <- static_cols %>% {. == "den"} %>% which()

  #  figure out merging for val header line

  # ====================================================================
  # ====================================================================
  #  === create the table, remove the header ... we will create it, and
  #  ============       set any table defaults    ===========


  ft <- df_tbl %>% flextable()

  sections <- list(nrow = list(responses = 0, stats = 0, titles = 0))

  if(subset_placement == "top" ) {

    has_subvar <- FALSE

    df_hdr_rows <- df_hdr_rows %>% select(-1)

    ft <- ft %>%
      delete_part(part = "body") %>%
      ft_add_data(df_tbl)

  }


  hdr_2_stats<- as.character(df_hdr_rows %>% tail(1))

  ## ====   set placement info for use later   ======

  #  attr(ft,"sub_placement") <- subset_placement
  ft$properties["sub_placement"] <- subset_placement

  ft$properties["multi_year"] <- "year" %in% colnames(df_stats)
  sections$nrow$years <- as.integer(ft$properties["multi_year"])

  ft <- ft %>%
    flextable::delete_rows(i=1, part = "header")


  # ==========================================================
  # ==========   handle the header lines  =====================

  # ========  do the titles   =====

  if(!is.null(titles)) {

    ntitles <- length(titles)
    sections$nrow$titles <- ntitles

    if(ntitles > 0) {

      titles <- gsub_attr(titles, stats_attrs)%>%
        split_before(title_max_char,collapse = "\n")

      ft <- ft %>%
        ft_add_titles(titles = titles, title_spacing = title_spacing)
    }
  }

  # ========  do the stats header and the responses header (and maybe year header)   =====


  hdr_lines <- purrr::map(1:nrow(df_hdr_rows), ~df_hdr_rows[.x,] %>% as.character())

  stats_line <- hdr_lines %>% tail(1)

  nresponses <- df_stats %>% pull(response) %>% unique() %>% length()
  nyears <- df_stats %>% pull(year) %>% unique() %>% length()

  if(nyears == 1) {
    hdr_lines <- tail(hdr_lines,-1)
    sections$nrow$years <- 0
  } else {
    sections$nrow$years <- 1
  }

  hdr_lines <- hdr_lines %>% head(-1) # get rid of stats

  hdr_lines <- purrr::map(hdr_lines, \(hdr_line) {

    ncats <- unique(hdr_line) %>% length() - 1

    if(ncats == 1) {
      hdr_line <- NULL
    }
    hdr_line

  }) %>% purrr::compact()

  sections$nrow$responses <- length(hdr_lines)

  purrr::walk(hdr_lines,\(hdrs) {

    ft <<- ft %>%
      ft_add_resp_hdr(values = hdrs)
  })


  if(!is.null(stats)) {

    # ====================================================================
    # ==========   rename the headers if requested    ==============


    rename <- as.list(rename)

    invisible(
      mapply(function(stat_in, stat_out) {
        stats_line[[1]][stats_line[[1]] == stat_in] <<- stat_out
      }, names(rename), rename)
    )


    sections$nrow$stats <- 1

    ft <- ft %>%
      ft_add_stats_hdr(values = stats_line[[1]])
  }

  if(has_subvar) ft <- ft %>%
    flextable::merge_v(j = 1, part = "body")

  ##############################################################
  ##
  ##      ===========   handle footnotes    ===============

  weighted <- df_stats %>% attr("weighted")
  ftnote_stats <- hdr_2_stats
  if(!is.null(weighted) && !weighted) hdr_2_stats <- hdr_2_stats[!hdr_2_stats %in% "pct"]

  if(footnotes) ft <- ft %>% add_footnotes(ftnote_stats, population)

  ft$header$sections <- sections

  # =========  do the fonts  ================


  if(!is.null(fonts_mgr)) {
    if(inherits(fonts_mgr, "FT_FontsMgr")) {
      ft <- ft %>% fonts_mgr$apply()
    } else {
      message("Invalid fonts_mgr ... does not inherit class <FT_FontsMgr>")
    }
  }

  # =========  do the backgrounds  ================

  if(!is.null(bgs_mgr)) {
    if(inherits(bgs_mgr, "FT_BGsMgr")) {
      ft <- ft %>% bgs_mgr$apply()
    } else {
      message("Invalid bgs_mgr ... does not inherit class <FT_BGsMgr>")
    }
  }

  # =========  do the paddings  ================

  ft <- ft %>% handle_paddings(paddings = paddings)

  # =========  do the borders  ================

  if(!is.null(borders_mgr)) {
    if(inherits(borders_mgr, "FT_BordersMgr")) {
      ft <- ft %>% borders_mgr$apply()
    } else {

      message("Invalid borders_mgr ... does not inherit class <FT_BordersMgr>")
    }
  }

  # ======= handle line spacing for body  ===========

  ft <- ft %>%flextable::line_spacing(j = 1:length(hdr_2_stats), space = line_spacing, part = "body")

  #  ===========   handle columns widths    ===============

  ft <- ft %>% handle_widths(widths = widths)



  #  ===========   handle column alignment    ===============

  ft <- ft %>% handle_aligns(aligns = aligns)


  #  ===========   handle column padding    ===============

  invisible(
    mapply(function(col, padding) {
      cols <- which(hdr_2_stats == col)

      if(length(cols)>0) {
        ft <<- ft %>%
          flextable::padding(j = cols, padding.right=padding*72)
      }
    }, names(col_padding), col_padding)
  )

  #  ======  if there are highlight_rows   ====================

  if(!is.null(highlights)) ft <- ft %>% ft_add_highlights(highlights)

  if(!is.null(highlights_mgr)) ft <- ft %>% highlights_mgr$apply()

  df_suppress <- df_suppress %>%
    rowwise() %>%
    mutate(
      suppress = any(c_across(where(is.logical)))
    ) %>%
    ungroup() %>%
    filter(suppress)

  sup_rows <- ft$body$data %>%
    left_join(df_suppress, by = join_by(subvar, subset)) %>%
    pull(suppress) %>%
    which()

  if(length(sup_rows) > 0) {
    ft <- ft %>%
      flextable::color(i = sup_rows, j = 2:length(ft$col_keys),
                       color = "#aaaaaa", part = "body")
  }

  #  ======  do the grid around the data if required

  if(!is.null(grid)) ft <- ft %>% ft_grid_table(grid = grid)

  #  ======  do the box around the table if requird

  if(!is.null(box)) ft <- ft %>% ft_box_table(box = box)

  #  ==== there are some border issues so this their fix  ========

  ft <- ft %>%
    flextable::fix_border_issues() %>%
    colformat_double(digits = digits)

  #  ====  caption   ===========

  # ft <- ft %>%  set_caption(
  #   "My table!", fp_p = fp_par(border = fp_border())
  # )

  #  ====  reset the defaults   ===========

  flextable::init_flextable_defaults()

  #  ====  return the finished table   ===========

  ft
}


widen <- function(df_stats) {

  stats <- StatsMgr$stats_names()

  stats <- c(stats, "suppress") %>%
    paste0(., collapse = "$|^") %>%
    paste0("^", ., "$")

  if("year" %in% colnames(df_stats)) {
    df_stats <- df_stats %>% relocate(year, .after = response)
  }

  df_wide <- df_stats %>%
    tidyr::pivot_wider(names_from = c(matches("year|response")),
                       id_cols = c(subvar, subset),
                       values_from =  c(matches(stats)), names_vary = "slowest",
                       names_sep = "^")

  df_wide
}

wide_header_rows <- function(df_wide) {


  df_cols <- data.frame(col = colnames(df_wide)) %>%
    mutate(nrows = gsub("[^\\^]","", col)) %>%
    mutate(nrows = nchar(nrows) + 1)

  max_rows <- max(df_cols %>% pull(nrows))


  df_header <- df_cols %>% purrr::pmap(\(col, nrows) {

    n_missing <- max_rows - nrows

    vals <- stringr::str_split_1(col, "\\^")

    c(vals,rep("", n_missing))

  })   %>% bind_cols(.name_repair = "universal_quiet") %>%
    slice(n():1) %>%
    as.data.frame()

  df_header
}

set_population <- function(df_stats, population) {

  if(is.null(population) ) {
    if("population" %in%  names(attributes(df_stats))) {

      population <- attr(df_stats, "population")

      df_stats <- df_stats %>%
        mutate(subset = ifelse(subset == "All Respondents", population, subset))
    }
  } else {

    df_stats <- df_stats %>%
      mutate(subset = ifelse(subset == "All Respondents", population, subset))
  }

  df_stats

}
