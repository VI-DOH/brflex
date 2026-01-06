library(R6)

#' @export
FT_StatPropsMgr <- R6Class(
  classname = "FT_StatPropsMgr",

  private = list(
    population_pvt = NULL,
    stats_pvt = c("den", "num", "percent", "ci"),
    exclude_pvt = "^$",
    responses_pvt = ".*",
    rename_pvt = c(percent = "pct"),
    widths_pvt = c(subset = 2, ci = 1),
    aligns_pvt = c(ci_pvt = "center"),
    subset_placement_pvt = "left",
    denom_sep_pvt = flextable::fp_border_default(),
    data_sep_pvt = flextable::fp_border_default(),
    response_sep_pvt = flextable::fp_border_default(),
    stats_sep_pvt = flextable::fp_border_default(),
    line_spacing_pvt = 1.0,
    title_spacing_pvt = 1.0,

    header_pvt = NULL,
    titles_pvt = NULL,
    title_max_char_pvt = 9999,
    highlights_pvt = NULL,
    footers_pvt = NULL,
    footnotes_pvt = TRUE,

    borders_pvt = ft_borders_list(),

    bgs_pvt = list(),
    bgs_mgr_pvt = list(),

    fonts_pvt = list(),
    fonts_mgr_pvt = NULL,

    paddings_pvt = list(),

    box_pvt = NULL,
    grid_pvt = NULL
  ),

  public = list(

    initialize = function(...) {

      args <- list(...)

      purrr::imap(args, \(val, nm) {

        nm <- paste0(nm ,"_pvt")
        private[[nm]] <- val
      })
    },

    ######################################################################
    ##
    ##        convenience functions

    std_borders = function(){

      solid_brdr <- officer::fp_border(style = "solid", color = "grey11", width = 1)
      dotted_brdr <- officer::fp_border(style = "dotted", color = "grey11", width = 1)
      dashed_brdr <- officer::fp_border(style = "dashed", color = "grey11", width = 1)

      private$borders_pvt = ft_borders_list(
        responses = ft_borders(left = solid_brdr,
                               right = solid_brdr,
                               midv = dotted_brdr),
        stats = ft_borders( bottom = solid_brdr),
        subvars = ft_borders(top = solid_brdr),
        subsets = ft_borders(top = solid_brdr,
                             left = solid_brdr,
                             right = solid_brdr,
                             midh = dotted_brdr))

    },

    set_border_value = function(name, side, item, value) {

      if(any(missing(name), missing(side), missing(item), missing(value))) return(FALSE)

      nm <- paste0(name,"_borders")
      brdrs <- private$borders_pvt[[nm]]

      if(is.null(brdrs)) {

        brdrs <- ft_borders()
      }

      brdr <- brdrs[[side]]

      if(!inherits(brdr,"fp_border")) {
        brdr <- fp_border()
      }

      brdr[[item]] <- value
      brdrs[[side]] <- brdr

      private$borders_pvt[[nm]] <- brdrs
    },

    set_border = function(name, side, border) {

      if(any(missing(name), missing(side), missing(border))) return(FALSE)


      nm <- paste0(name,"_borders")
      brdrs <- private$borders_pvt[[nm]]

      if(is.null(brdrs)) {

        brdrs <- ft_borders()
      }

      brdrs[[side]] <- border

      private$borders_pvt[[nm]] <- brdrs
    },

    add_widths = function(value) {

      if(missing(value)) return(NULL)

      if(class(value) != "numeric" || length(names(value)) == 0) return(NULL)

      private$widths_pvt <- c(private$widths_pvt, value)


    },

    add_highlight = function(value) {

      if(missing(value)) return(NULL)

      if(!grepl("ft_highlight", class(value))) return(NULL)

      private$highlights_pvt[[length(private$highlights_pvt) + 1]] <- value


    },

    highliter = function(where, color = "yellow", cols = c(1,2)) {

      hlt <- ft_highlight_row(
        where = where, bg = color,
        col_nums = cols,
        part = "body"
      )

      self$add_highlight(hlt)

    },

    add_aligns = function(value) {

      if(missing(value)) return(NULL)

      if(class(value) != "character" || length(names(value)) == 0) return(NULL)

      private$aligns_pvt <- c(private$aligns_pvt, value)

    }

  ),

  active = list(

    fonts_mgr = function(value) {

      if(missing(value)) return(private$fonts_mgr_pvt)

      if(!inherits(value, "FT_FontsMgr") && !is.null(value)) {
        message("fonts_mgr must be class <FT_FontsMgr>")
        return()
      }
      private$fonts_mgr_pvt <- value
    },

    bgs_mgr = function(value) {

      if(missing(value)) return(private$bgs_mgr_pvt)

      if(!inherits(value, "FT_BGsMgr") && !is.null(value)) {
        message("bgs_mgr must be class <FT_BGsMgr>")
        return()
      }
      private$bgs_mgr_pvt <- value
    },

    fonts = function(value) {

      if(missing(value)) return(private$fonts_pvt)

      if(class(value) != "ft_fonts_list") return(NULL)

      private$fonts_pvt <- value
    },

    bgs = function(value) {

      if(missing(value)) return(private$bgs_pvt)

      if(class(value) != "ft_bgs_list") return(NULL)

      private$bgs_pvt <- value
    },

    paddings = function(value) {

      if(missing(value)) return(private$paddings_pvt)

      if(class(value) != "ft_paddings_list") return(NULL)

      private$paddings_pvt <- value
    },

    borders = function(value) {

      if(missing(value)) return(private$borders_pvt)

      if(class(value) != "ft_borders_list") return(NULL)

      private$borders_pvt <- value
    },

    exclude = function(value) {

      if(missing(value)) return(private$exclude_pvt)

      if(class(value) != "character") return(NULL)

      private$exclude_pvt <- value
    },

    responses = function(value) {

      if(missing(value)) return(private$responses_pvt)

      if(class(value) != "character") return(NULL)

      private$responses_pvt <- value
    },

    titles = function(value) {

      if(missing(value)) return(private$titles_pvt)

      if(class(value) != "character") return(NULL)

      private$titles_pvt <- value
    },

    box = function(value) {

      if(missing(value)) return(private$box_pvt)

      if(class(value) != "ft_box") return(NULL)

      private$box_pvt <- value
    },

    grid = function(value) {

      if(missing(value)) return(private$grid_pvt)

      if(class(value) != "fp_border") return(NULL)

      private$grid_pvt <- value
    },

    footnotes = function(value) {

      if(missing(value)) return(private$footnotes_pvt)

      if(class(value) != "logical") return(NULL)

      private$footnotes_pvt <- value
    },

    stats = function(value) {

      if(missing(value)) return(private$stats_pvt)

      if(class(value) != "character") return(NULL)

      value <- value[value %in% c("num", "den", "percent", "se", "ci", "cv")]

      private$stats_pvt <- value
    },

    population = function(value) {

      if(missing(value)) return(private$population_pvt)

      if(class(value) != "character") return(NULL)

      private$population_pvt <- value
    },

    aligns = function(value) {

      if(missing(value)) return(private$aligns_pvt)

      if(class(value) != "character" || length(names(value)) == 0) return(NULL)

      private$aligns_pvt <- value
    },

    highlights = function(value) {

      if(missing(value)) return(private$highlights_pvt)

      if(class(value) != "ft_highlight") {

        if(!inherits(value, "list") ||  !grepl("ft_highlight",class(value[[1]]))) {

          return(NULL)
        }
      }

      private$highlights_pvt <- value
    },

    widths = function(value) {

      if(missing(value)) return(private$widths_pvt)

      if(class(value) != "numeric" || length(names(value)) == 0) return(NULL)

      private$widths_pvt <- value
    },

    subset_placement = function(value) {

      if(missing(value)) return(private$subset_placement_pvt)

      if(class(value) != "character") return(NULL)

      private$subset_placement_pvt <- value
    },

    yes_only = function(value) {

      if(missing(value)) return(invisible())

      if(class(value) != "logical") return(NULL)

      private$responses_pvt <- "^Yes$"
    }



  )
)
