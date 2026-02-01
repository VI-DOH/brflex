
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
                     borders = list(),
                     borders_mgr = NULL,
                     bgs = list(),
                     bgs_mgr = NULL,
                     fonts = list(),
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

  #  character for separating stats and vals

  sep_char <- "^"


  # add ci text column (CI_Lower-CI_upper)

  df_stats_rpt <- df_stats

  if("ci" %in% stats && "CI_lower" %in% colnames(df_stats_rpt)) {
    df_stats_rpt <- df_stats_rpt %>%
      mutate(ci = paste0(CI_lower, "-", CI_upper))
  }
  # get the set of valid values

  # vals <- values() %>% filter(col_name == coi) %>% pull(text)

  vals <- df_stats_rpt %>% pull(response) %>% unique()
  nvals <- length(vals)

  stats_in <- df_stats_rpt %>% select( where(is.numeric), matches("^ci$")) %>% colnames()
  stats_rm <- stats_in %>% {.[!. %in% stats]}

  df_stats_rpt <- df_stats_rpt %>% select(-all_of(stats_rm))

  stats <- stats %>% {.[!. %in% "den"]} %>% {.[.%in% stats_in]}

  cnames <- df_stats_rpt %>% colnames()
  nstats <- length(stats)

  static_cols <- cnames %>% {.[!. %in% c(stats, "response", "suppress")]}

  nstatic <- length(static_cols)

  has_subvar <- "subvar" %in% static_cols

  fin_cols <- c(static_cols,
                expand.grid(stats,vals) %>%
                  mutate(col = paste0(Var1,sep_char,Var2)) %>%
                  pull(col))

  if(length(stats) == 1) fin_cols <- gsub(".*\\^","", fin_cols)

  if(has_subvar) subvars <- df_stats %>%
    pull(subvar) %>%
    unique()
  else subvars  <-  character(0)


  df_tbl <- df_stats_rpt %>%
    tidyr::pivot_wider(names_from = response,
                       values_from = c(all_of(stats), "suppress"),
                       names_sep = sep_char) %>%
    as.data.frame()

  df_suppress <- df_tbl %>%
    select(subvar, subset, starts_with("suppress"))

  df_tbl <- df_tbl %>%
    select(all_of(fin_cols))

  if(has_subvar) {

    df_tbl <- df_tbl %>%
      group_by(subvar) %>%
      mutate(rn = row_number()) %>%
      ungroup() %>%
      mutate(my_sub = which_subvar(subvar = subvar, subvars = subvars) ) %>%
      arrange(my_sub, rn) %>%
      select(-my_sub, -rn) %>%
      mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, .x))) %>%
      mutate(across(where(is.character), ~if_else(is.na(.x), "-", .x)))
  }
  #subsets = subsets, subvar))


  val_hdr <- df_tbl %>% colnames() %>% grep(sep_char,., fixed = T, value = T) %>%
    gsub("(.*)\\^(.*)","\\2",.)

  hdr_2_stats <- c(static_cols, rep(stats, nvals))

  denom_col <- static_cols %>% {. == "den"} %>% which()

  #  figure out merging for val header line
  val_hdr <- c("",vals)
  hdr_1_vals <- rep("", nstatic)


  sapply(vals, function(val){
    hdr_1_vals <<- c(hdr_1_vals, rep(val,nstats))
  })


  right_bords <- which(hdr_2_stats == stats[1]) - 1


  # ====================================================================
  # ==========   rename the headers if requested    ==============


  rename <- as.list(rename)

  invisible(
    mapply(function(stat_in, stat_out) {
      hdr_2_stats[hdr_2_stats == stat_in] <<- stat_out
    }, names(rename), rename)
  )


  # ====================================================================
  #  === calculate tops of each new subset for a border    ==============

  if(has_subvar) {
    top_bords <- df_tbl %>% select(subvar) %>% distinct() %>%

      left_join(df_tbl %>%
                  count(subvar),by = join_by(subvar)) %>%
      mutate(x=cumsum(n)+1) %>%
      mutate(start = x-n) %>%
      pull(start)

    bottom_bords <- (top_bords - 1) %>%   {.[.>0]}

  } else bottom_bords<-NULL

  stat_cols <- (nstatic + 1) : ncol(df_tbl)

  # if(has_subvar) df_tbl <- df_tbl %>%
  #   mutate(subvar = ifelse(subvar == "", subset, subvar))

  # ====================================================================
  # ====================================================================
  #  === create the table, remove the header ... we will create it, and
  #  ============       set any table defaults    ===========


  ft <- df_tbl %>% flextable()

  sections <- list(nrow = list(responses = 0, stats = 0, titles = 0))

  if(subset_placement == "top" ) {

    has_subvar <- FALSE
    hdr_1_vals <- tail(hdr_1_vals,-1)
    hdr_2_stats <- tail(hdr_2_stats,-1)
    right_bords <- right_bords - 1

    ft <- ft %>%
      delete_part(part = "body") %>%
      ft_add_data(df_tbl)

  }

  ## ====   set placement info for use later   ======

  #  attr(ft,"sub_placement") <- subset_placement
  ft$properties["sub_placement"] <- subset_placement

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

  # ========  do the stats header and the responses header   =====


  if(!is.null(responses)) {

    sections$nrow$responses <- 1

    ft <- ft %>%
      ft_add_resp_hdr(values = hdr_1_vals)
  }

  if(!is.null(stats)) {

    sections$nrow$stats <- 1

    ft <- ft %>%
      ft_add_stats_hdr(values = hdr_2_stats)
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
  } else {
    ft <- ft %>% handle_fonts(fonts = fonts)
  }
  # =========  do the backgrounds  ================

  if(!is.null(bgs_mgr)) {
    if(inherits(bgs_mgr, "FT_BGsMgr")) {
      ft <- ft %>% bgs_mgr$apply()
    } else {
      message("Invalid bgs_mgr ... does not inherit class <FT_BGsMgr>")
    }
  } else {
    ft <- ft %>% handle_bgs(bgs = bgs)
  }

  # =========  do the paddings  ================

  ft <- ft %>% handle_paddings(paddings = paddings)

  # =========  do the borders  ================

  if(!is.null(borders_mgr)) {
    if(inherits(borders_mgr, "FT_BordersMgr")) {
      ft <- ft %>% borders_mgr$apply()
    } else {
      browser()
      message("Invalid borders_mgr ... does not inherit class <FT_BordersMgr>")
    }
  } else {

    ft <- ft %>% handle_borders(borders = borders)

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
    flextable::fix_border_issues()

  #  ====  caption   ===========

  # ft <- ft %>%  set_caption(
  #   "My table!", fp_p = fp_par(border = fp_border())
  # )

  #  ====  reset the defaults   ===========

  flextable::init_flextable_defaults()

  #  ====  return the finished table   ===========

  ft
}


