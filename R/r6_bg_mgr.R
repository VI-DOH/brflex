
#' @export
FT_BGsMgr <-
  R6::R6Class(
    classname = "FT_BGsMgr",

    private = list(

      bg = list(
        table_pvt = NULL,
        data_pvt = NULL,
        header_pvt = NULL,
        responses_pvt = NULL,
        stats_pvt = NULL,
        subsets_pvt = NULL,
        subvars_pvt = NULL,
        titles_pvt = NULL,
        footnotes_pvt = NULL,
        footer_pvt = NULL
      ),

      set_active = function(value, area) {

        if(inherits(value, "FT_Color")) {
        } else if(inherits(value[[1]], "FT_Color")) {

          value <-value[[1]]

        } else if(inherits(value, "character") && FT_Color$is_valid(value)){
          value <- FT_Bg$new(value)
        } else  {
          message("value must be an FT_Color object or a character representation of a color")
          return(NULL)
        }

        private$bg[[paste0(area, "_pvt")]] <- list(value)

      }

    ),

    public = list(

      initialize = function(table = NULL,
                            data = NULL,
                            header = NULL,
                            responses = NULL,
                            stats = NULL,
                            subsets = NULL,
                            subvars = NULL,
                            titles = NULL,
                            footnotes = NULL,
                            footer = NULL) {


        args <- as.list(match.call()) %>% tail(-1) %>% names()

        purrr::walk(args, \(arg) {


          value <- get(arg)

          if(is.character(value) && FT_Color$is_valid(value)) {
            value <- list(FT_Bg$new(value))
          } else if(inherits(value, FT_Color)) {
            value <- list(value)
          } else {
            value <- NULL
          }

          private$bg[[paste0(arg, "_pvt")]] <- value

        })




      },

      as.data.frame = function() {

        priv_list <- as.list(private$bg)

        df <- purrr::imap(priv_list, \(value, nm){
          if(!is.null(value)) {

            purrr::map(value,\(bg) {

              bg$as.data.frame()
            }) %>%
              bind_rows() %>%
              mutate(area = gsub("_pvt","",nm)) %>%
              mutate(index = row_number()) %>%
              relocate(area, index)
          }
        }
        ) %>% bind_rows()

        df
      },

      print = function() {


        print(self$as.data.frame())


      },

      n_bgs = function(area) {

        length(private$bg[[paste0(area, "_pvt")]])
      },

      add_bg = function(bg, area) {

        if (!inherits(bg, "FT_Bg")) {

          if(inherits(bg, "character") && FT_Color$is_valid(bg)){
            bg <- FT_Bg$new(bg)
          } else {
            message("bg must be of class FT_Bg")
            return(NULL)
          }
        }

        n <- self$n_bgs(area)
        if(n == 0) private$bg[[paste0(area, "_pvt")]] <- list()

        private$bg[[paste0(area, "_pvt")]][[n + 1]] <- bg
      },

      apply = function(ft) {


        has_subvar_col <- "subvar" %in% ft$col_keys

        if(has_subvar_col) {
          jcols_subvars <- 1
          irows_subvars <- NULL

          jcols_subsets <- 2
          irows_subsets <- NULL

        } else {

          rowspans <- ft$body$spans$rows

          if(!is.null(dim(rowspans))) {

            jcols_subvars <- 1
            irows_subvars <- which(ft$body$spans$rows[,1] > 1)

            jcols_subsets <- 1
            irows_subsets <- which(ft$body$spans$rows[,1] == 1)
          }else {
            jcols_subvars <- 1
            irows_subvars <- 1

            jcols_subsets <- 0
            irows_subsets <- 0
          }
        }

        nrow_header <- ft$header$content$nrow
        nrow_titles <- nrow_header - 2

        #handle_bgs_header(bgs)
        # handle_bgs_response(bgs)%>%
        #   handle_bgs_stats(bgs) %>%
        #self$apply_titles(ft)


        ft <- ft %>% self$apply_area("table")
        ft <- ft %>% self$apply_area("header")
        ft <- ft %>%self$apply_area("titles", i = 1:nrow_titles, part = "header")
        ft <- ft %>%self$apply_area("responses", i = nrow_header - 1, part = "header")
        ft <- ft %>%self$apply_area("stats", i = nrow_header, part = "header")
        ft <- ft %>%self$apply_area("data", part = "body")
        ft <- ft %>%self$apply_area("footnotes",  part = "footer")
        ft <- ft %>%self$apply_area("subvars", i = irows_subvars,
                                    j = jcols_subvars, part = "body")

        if(irows_subsets[1] > 0) ft <- ft %>%
          self$apply_area("subsets", i = irows_subsets, j = jcols_subsets, part = "body")

        ft

      },

      apply_area = function(ft, area, i = NULL, j = NULL, part) {

        bgs <- private$bg[[paste0(area,"_pvt")]]

        if(is.null(bgs)) return(ft)

        use_rc <- TRUE

        if(use_rc) {

          f <- paste0(area, "_rc")
          rc <- do.call(f, args = list(ft))

          if(length(rc) == 0) return(ft)

          if(!is.list(rc[[1]])) rc <- list(rc)

          purrr::walk(rc, \(rc0) {

            rows <- rc0$rows
            cols <- rc0$cols
            part <- rc0$part

            bg  <-  bgs[[1]]$color


            ft <<- self$apply_bg(ft, i = rows, j = cols,
                                 bg = bg, part = part)
          })


        } else {
          if(is.null(i)) i <- 1:(ft[[part]]$content$nrow)

          nrow <- length(i)

          nbgs <- self$n_bgs(area)

          if(nrow != nbgs) {
            ibgs <- rep(1,nrow)
          } else {
            ibgs <- 1:nrow
          }

          purrr::walk2(i, ibgs,\(irow, ibg) {
            #if(area == "responses") browser()

            bg  <-  bgs[[ibg]]$color

            ft <<- self$apply_bg(ft, i = irow, j = j,
                                 bg = bg, part = part)
          })

        }
        ft
      },

      apply_bg = function(ft, i = NULL, j = NULL, bg , part = "header") {

        ft %>%
          flextable::bg(i = i, j = j, bg = bg , part = part)

      }
    ),

    active = list(

      names = function(value) {

        if(!missing(value)) {
          message("This property is read-only")
          return(NULL)
        }

        ls(private$bg, sorted = FALSE) %>% gsub("_pvt","", .)


      },

      table = function(value) {

        if(missing(value)) {

          obj <- private$bg$table_pvt
          if(length(obj) == 1) obj <-  obj[[1]]
          return(obj)
        }

        private$set_active(value, "table")

      },

      data = function(value) {

        if(missing(value)) {

          obj <- private$bg$data_pvt
          if(length(obj) == 1) obj <-  obj[[1]]
          return(obj)
        }
        private$set_active(value, "data")

      },

      header = function(value) {

        if(missing(value)) {

          obj <- private$bg$header_pvt
          if(length(obj) == 1) obj <-  obj[[1]]
          return(obj)
        }
        private$set_active(value, "header")

      },

      responses = function(value) {

        if(missing(value)) {

          obj <- private$bg$responses_pvt
          if(length(obj) == 1) obj <-  obj[[1]]
          return(obj)
        }
        private$set_active(value, "responses")

      },

      stats = function(value) {

        if(missing(value)) {

          obj <- private$bg$stats_pvt
          if(length(obj) == 1) obj <-  obj[[1]]
          return(obj)
        }
        private$set_active(value, "stats")

      },

      subsets = function(value) {

        if(missing(value)) {

          obj <- private$bg$subsets_pvt
          if(length(obj) == 1) obj <-  obj[[1]]
          return(obj)
        }

        private$set_active(value, "subsets")

      },

      subvars = function(value) {

        if(missing(value)) {

          obj <- private$bg$subvars_pvt
          if(length(obj) == 1) obj <-  obj[[1]]
          return(obj)
        }
        private$set_active(value, "subvars")

      },

      titles = function(value) {

        if(missing(value)) {

          obj <- private$bg$titles_pvt
          if(length(obj) == 1) obj <-  obj[[1]]
          return(obj)
        }

        private$set_active(value, "titles")

      },

      footer = function(value) {


        if(missing(value)) {

          obj <- private$bg$footer_pvt
          if(length(obj) == 1) obj <-  obj[[1]]
          return(obj)
        }

        private$set_active(value, "footer")

      }
    )
  )


#' @export
FT_DefaultBGsMgr <-
  R6::R6Class(
    classname = "FT_DefaultBGsMgr",
    inherit = FT_BGsMgr,

    public = list(

      initialize = function() {

        super$initialize(stats = "grey88", subvars = "grey88")
      }
    )

  )

makeActiveBinding(
  "names",
  function() {
    c("table", "header", "titles", "responses", "stats",
      "data", "subsets", "subvars", "footer", "footnotes")
  },
  env = FT_BGsMgr
)

#' @export
FT_Bg <-
  R6::R6Class(
    classname = "FT_Bg",
    inherit = FT_Color,

    private = list(
    ),

    public = list(


    ),

    active = list(

    )

  )
