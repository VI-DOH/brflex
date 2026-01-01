
responses_rows <- function(ft) {

  irow <- ft$header$sections$nrow$titles + 1
  irow:(irow + ft$header$sections$nrow$responses - 1)

}

titles_rows <- function(ft) {

  1:(ft$header$sections$nrow$titles)

}

stats_rows <- function(ft) {

  irow <- ft$header$sections$nrow$titles + ft$header$sections$nrow$responses + 1
  irow:(irow + ft$header$sections$nrow$stats - 1)

}

subvar_rows <- function(ft) {

  x <- ft$body$dataset$subvar %>% as.data.frame() %>% rename(subvar = 1) %>%
    group_by(subvar)

  subvar <- group_keys(x)
  min <- group_rows(x) %>% purrr::map_int(~min(.x))
  max <- group_rows(x) %>% purrr::map_int(~max(.x))

  data.frame(subvar, min, max) %>%
    arrange(min) %>% as.data.frame()

}

subvar_row_types <- function(ft) {

  df <- data.frame(irow =1:ft$body$content$nrow)
  df_vars <- subvar_rows(ft)

  df %>%
    left_join(df_vars %>% select(min, max), by = join_by(irow == min)) %>%
    mutate(top = !is.na(max)) %>%
    select(irow,top) %>%
    left_join(df_vars %>% select(max, min), by = join_by(irow == max)) %>%
    mutate(bottom  = !is.na(min)) %>%
    select(irow, top, bottom) %>%
    mutate(mid = !(top | bottom))

}

subset_rows <- function(ft) {

  subvars <- ft$body$dataset$subvar %>% unique() %>% {.[. != ""]}
  which(!ft$body$dataset$subset %in% subvars)

}

subset_row <- function(ft, subset) {

  which(ft$body$dataset$subset == {{subset}})

}


response_cols <- function(ft) {

  irows <- responses_rows(ft)

  resp <- ft$header$dataset[irows,] %>% as.character()

  df <- data.frame(jcol =1:length(ft$header$col_keys), resp = resp)

  resps <-resp %>% unique()

  grp <- resp %>% purrr::map_int(\(r) {
    which(r == resps)
  })

  df <- df %>% mutate(grp = grp)

  df %>% group_by(resp ,grp) %>%
    summarise(min = min(jcol), max = max(jcol)) %>%
    filter(resp != "") %>%
    arrange(min) %>% ungroup()


}
response_col_types <- function(ft) {

  irows <- responses_rows(ft)

  resps <- ft$header$dataset[irows,] %>% as.character()

  df <- data.frame(jcol =1:length(ft$header$col_keys), resps = resps)
  df_cols <- response_cols(ft)

  df %>%
    left_join(df_cols %>% select(min, max), by = join_by(jcol == min)) %>%
    mutate(left = !is.na(max)) %>%
    select(jcol,left) %>%
    left_join(df_cols %>% select(max, min), by = join_by(jcol == max)) %>%
    mutate(right  = !is.na(min)) %>%
    select(jcol, left, right) %>%
    mutate(mid = !(left | right)) %>%
    left_join(df, by = join_by(jcol)) %>% filter(resps != "")

}

