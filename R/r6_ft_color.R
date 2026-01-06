#' Create a color object
#'
#' @param x A color name or hex string
#' @export
FT_Color <-
  R6::R6Class(
    classname = "FT_Color",

    private = list(
      color_pvt = NA,
      alpha_pvt = "FF"
    ),

    public = list(

      initialize = function (color = NULL) {

        # if(!FT_Color$is_valid(color)) {
        #   message(paste0("Invalid color ...", color))
        #   return()
        # }

        self$color <- color
        private$color_pvt <- ifelse(is.null(color), NA, color)


      },

      print = function() {

        color <- private$color_pvt

        if(grepl("^#",color)) {
          func <- cli::make_ansi_style(color)
        } else {
          func <- paste0("cli::col_", color)
        }


        func <- cli::make_ansi_style(color)
        res_txt <- do.call(func, list(color))


        cat(res_txt)


      },

      as.data.frame = function() {


        data.frame(color = private$color_pvt
        )

      },

      as.list = function() {

        list(
          color  = self$color
        )
      },

      ft_add_bg = function(ft, i = NULL, j = NULL,
                           part = c('all', 'body', 'header', 'footer')) {

        part <- match.arg(part, c('all', 'body', 'header', 'footer'))

        ft %>%
          flextable::bg(i = i, j = j, bg = private$color_pvt, part = part)
      }


    ),

    active = list(
      color = function(value) {

        if(missing(value)) return (private$color_pvt)

        if(!FT_Color$is_valid(value)) {
          message(paste0("Invalid color ...", value))
          return()
        }

        private$color_pvt <- value

      }
    )

  )


#' Test whether a color is valid
#'
#' @param x A color name or hex string
#' @export
FT_Color$is_valid <- function(x) {
  tryCatch({
    grDevices::col2rgb(x, alpha = TRUE)
    TRUE
  }, error = function(e) FALSE)
}

