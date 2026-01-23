#' FT_Color: An R6 Color Object
#'
#' An R6 class for representing and manipulating colors.
#' Supports named colors and hex strings (RGB or RGBA),
#' printing with ANSI styling, coercion helpers, and
#' alpha blending against a background color.
#'
#' @details
#' The `FT_Color` class stores a single color value internally and
#' provides helper methods for printing, flattening alpha transparency,
#' and coercing to common R data structures.
#'
#' Color validity is checked using `FT_Color$is_valid()`.
#'
#' @section Active bindings:
#' \describe{
#'   \item{color}{Get or set the color value. Must be a valid color name or hex string.}
#' }
#'
#' @section Public methods:
#' \describe{
#'   \item{new(color = NULL)}{Create a new `FT_Color` object.}
#'   \item{print()}{Print the color using ANSI styling via the cli package.}
#'   \item{as.data.frame()}{Coerce the color to a data frame.}
#'   \item{as.list()}{Coerce the color to a list.}
#'   \item{flatten(background_hex = "#FFFFFF")}{Blend an RGBA color against a background color.}
#' }
#'
#' @examples
#' col <- FT_Color$new("#FF000080")
#' col
#'
#' col$flatten()
#' col$as.data.frame()
#'
#' @export
FT_Color <-
  R6::R6Class(
    classname = "FT_Color",

    private = list(
      color_pvt = NA,
      alpha_pvt = "FF"
    ),

    public = list(
      #' @description
      #' Initialize a new `FT_Color` object.
      #'
      #' @param color A color name or hex string. If `NULL`, the color is set to `NA`.
      initialize = function(color = NULL) {

        self$color <- color
        private$color_pvt <- ifelse(is.null(color), NA, color)

      },

      #' @description
      #' Print the color using ANSI styling.
      #'
      #' Uses the \pkg{cli} package to render the color in the console
      #' when supported.
      print = function() {

        color <- private$color_pvt

        if (grepl("^#", color)) {
          func <- cli::make_ansi_style(color)
        } else {
          func <- paste0("cli::col_", color)
        }

        func <- cli::make_ansi_style(color)
        res_txt <- do.call(func, list(color))

        cat(res_txt)
      },

      #' @description
      #' Coerce the color to a data frame.
      #'
      #' @return A data frame with a single column `color`.
      as.data.frame = function() {
        data.frame(color = private$color_pvt)
      },

      #' @description
      #' Coerce the color to a list.
      #'
      #' @return A list containing the color value.
      as.list = function() {
        list(
          color = self$color
        )
      },

      #' @description
      #' Flatten an RGBA color against a background color.
      #'
      #' If the stored color is an 8-digit hex string (`#RRGGBBAA`),
      #' it will be alpha-blended against the specified background color.
      #' If the color is already RGB or not a hex string, it is returned unchanged.
      #'
      #' @param background_hex A hex color string to blend against.
      #'   Defaults to white (`"#FFFFFF"`).
      #'
      #' @return A 6-digit hex color string.
      flatten = function(background_hex = "#FFFFFF") {

        rgba_hex = private$color_pvt

        if (!substr(rgba_hex, 1, 1) == "#") return(rgba_hex)

        rgba_hex <- gsub("#", "", rgba_hex)
        background_hex <- gsub("#", "", background_hex)

        # Parse background (RGB)
        bg_r <- as.numeric(paste0("0x", substr(background_hex, 1, 2)))
        bg_g <- as.numeric(paste0("0x", substr(background_hex, 3, 4)))
        bg_b <- as.numeric(paste0("0x", substr(background_hex, 5, 6)))

        # Parse foreground (RGBA)
        fg_r <- as.numeric(paste0("0x", substr(rgba_hex, 1, 2)))
        fg_g <- as.numeric(paste0("0x", substr(rgba_hex, 3, 4)))
        fg_b <- as.numeric(paste0("0x", substr(rgba_hex, 5, 6)))

        # Alpha channel
        if (nchar(rgba_hex) == 8) {
          alpha <- as.numeric(paste0("0x", substr(rgba_hex, 7, 8))) / 255
        } else {
          return(paste0("#", rgba_hex))
        }

        # Blend
        res_r <- round((fg_r * alpha) + (bg_r * (1 - alpha)))
        res_g <- round((fg_g * alpha) + (bg_g * (1 - alpha)))
        res_b <- round((fg_b * alpha) + (bg_b * (1 - alpha)))

        sprintf("#%02X%02X%02X", res_r, res_g, res_b)
      }

    ),

    active = list(
      #' @field color (`character(1)`)
      #' Get or set the color value.
      #'
      color = function(value) {

        if (missing(value)) return(private$color_pvt)

        if (!FT_Color$is_valid(value)) {
          message(paste0("Invalid color ...", value))
          return()
        }

        private$color_pvt <- value
      }
    )
  )


#' @export
FT_Color$is_valid <- function(x) {
  tryCatch({
    grDevices::col2rgb(x, alpha = TRUE)
    TRUE
  }, error = function(e) FALSE)
}

