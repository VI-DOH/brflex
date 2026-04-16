
table_rc <- function(ft) {

  parts <- c("header", "body", "footer")

  purrr::map(parts, \(part) {

    list(rows = 1:nrow_part(ft, part),
         cols =  1:length(ft$col_keys),
         part = part
    )
  })
}

header_rc <- function(ft) {

  list(
    list(rows = 1:ft$header$content$nrow,
         cols = 1:length(ft$col_keys),
         part = "header"
    )
  )
}

footer_rc <- function(ft) {

  list(
    list(rows = 1:nrow_part(ft, "footer"),
         cols =  1:length(ft$col_keys),
         part = "footer"
    )
  )
}

titles_rc <- function(ft) {

  list(
    list(rows = titles_rows(ft),
         cols = 1:length(ft$col_keys),
         part = "header"
    )
  )
}


responses_rc <- function(ft) {

  if(ft$header$sections$nrow$responses == 0) return(list())

  cols <- response_cols(ft)

  cols <- purrr::map2(cols$min, cols$max,\(min, max) min:max) %>% unlist()

  list(
    list(rows = responses_rows(ft),
         cols = cols,
         part = "header")
  )

}



years_rc <- function(ft) {

  if(!ft$properties[["multi_year"]]) return(list())

  cols <- years_cols(ft)

  cols <- purrr::map2(cols$min, cols$max,\(min, max) min:max) %>% unlist()

  list(
    list(rows = years_rows(ft),
         cols = cols,
         part = "header")
  )

}


stats_rc <- function(ft) {

  list(
    list(
      rows = stats_rows(ft),
      cols = 1:length(ft$body$col_keys),
      part = "header"
    )
  )

}


subsets_rc <- function(ft) {

  rows <- subvar_rows(ft)

  rc <- purrr::map2(rows$min, rows$max,\(min, max) {
    list(rows = min:max,
         cols = 2 - (ft$properties$sub_placement == "top"),
         part = "body")

  })

  rc
}

subvars_rc <- function(ft) {

  rows <- subvar_rows(ft)

  if(ft$properties$sub_placement == "top") {

    rows <- rows %>% tail(-1)

    rc <- purrr::map2(rows$min, rows$max,\(min, max) {
      list(rows = min-1,
           cols = 1:length(ft$col_keys),
           part = "body")

    })

  } else {

    rc <- purrr::map2(rows$min, rows$max,\(min, max) {
      list(rows = min:max,
           cols = 1,
           part = "body")

    })
  }


  rc
}


data_rc <- function(ft) {

  rows <- subvar_rows(ft)

  rc <- purrr::map2(rows$min, rows$max,\(min, max) {
    list(rows = min:max,
         cols = (3 - (ft$properties$sub_placement == "top")):length(ft$col_keys),
         part = "body")

  })

  rc
}

footnotes_rc <- function(ft) {

  list(
    list(
      rows = 1:nrow_part(ft, part = "footer"),
      cols = 1:length(ft$body$col_keys),
      part = "footer"
    )
  )


}


titles_rows <- function(ft) {

  1:(ft$header$sections$nrow$titles)

}

responses_rows <- function(ft) {

  irow <- ft$header$sections$nrow$titles + ft$header$sections$nrow$years + 1
  #irow:(irow + ft$header$sections$nrow$responses - 1)
  irow
}

years_rows <- function(ft) {

  irow <- ft$header$sections$nrow$titles + 1


}

titles_rows <- function(ft) {

  1:(ft$header$sections$nrow$titles)

}


stats_rows <- function(ft) {

  irow <- ft %>% nrow_part(part = "header")
  irow:(irow + ft$header$sections$nrow$stats - 1)

}

subvar_rows <- function(ft) {

  x <- ft$body$dataset$subvar %>% as.data.frame() %>% rename(subvar = 1) %>%
    group_by(subvar)

  subvar <- group_keys(x)
  min <- group_rows(x) %>% purrr::map_int(~min(.x))
  max <- group_rows(x) %>% purrr::map_int(~max(.x))

  data.frame(subvar, min, max) %>%
    arrange(min) %>% as.data.frame() %>%
    mutate(max = if_else(row_number() == 1, 1, max))

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


years_cols <- function(ft) {

  irows <-years_rows(ft)

  yrs <- ft$header$dataset[irows,] %>% as.character()

  df <- data.frame(jcol =1:length(ft$header$col_keys), yrs = yrs)

  yrss <-yrs %>% unique()

  grp <- yrs %>% purrr::map_int(\(r) {
    which(r == yrss)
  })

  df <- df %>% mutate(grp = grp)

  df %>% group_by(yrs ,grp) %>%
    summarise(min = min(jcol), max = max(jcol)) %>%
    filter(yrs != "") %>%
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

