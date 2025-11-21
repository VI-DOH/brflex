
add_footnotes <- function(ft, hdr_2_stats, population) {

  population <- population %>% gsub("(.*ales)",paste0("All eligible ","\\1"), .)

  next_ftnt <- 1

  ftnotes <- list(
    den = paste0("Denominator - ",population),
    num = "Numerator - number of respondents with that response",
    percent =  "Weighted percent - value is not numerator/denominator",
    pct =  "Weighted percent - value is not numerator/denominator",
    ci = "95% confidence interval"
  )


  mapply(function(ftnote, nm) {


    cols <- which(hdr_2_stats == nm)
    part  <-  "header"


    if(length(cols)>0) {

      stats_row <-  nrow_part(ft, part = part)

      ft <<- ft %>%
        flextable::footnote(i = stats_row, j=cols,
                            value = as_paragraph(ftnote),
                            ref_symbols = as.character(next_ftnt), part = "header" ) %>%
        flextable::padding(i = next_ftnt,padding.top = 2, padding.bottom = 2,
                           part = "footer")%>%
        merge_at(i = next_ftnt, part = "footer" )
      next_ftnt <<- next_ftnt + 1
    }
  }, ftnotes, names(ftnotes))

  ft

}
