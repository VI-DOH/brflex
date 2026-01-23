library(R6)

#' @export
FT_StandardStatPropsMgr <- R6Class(
  classname = "FT_StandardStatPropsMgr",
  inherit = FT_StatPropsMgr,

  private = list(

  ),

  public = list(

    initialize = function() {

      fnts_mgr <- FT_FontsMgr$new()
      fnts_mgr$add_font(FT_Font$new(color = "grey22", font.size = 13, bold = TRUE), "titles")
      fnts_mgr$add_font(FT_Font$new(color = "grey22", font.size = 12), "data")
      fnts_mgr$merge_fonts()

      bg_mgr <- FT_BGsMgr$new(stats = "grey88", subvars = "grey88")


      super$initialize(

        footnotes = TRUE,
        titles = c("{year} BRFSS", "{label}", "{coi}"),
        box = ft_box(),
        bgs_mgr = bg_mgr,
        fonts_mgr = fnts_mgr,

        stats = c("den","num","percent"),

        aligns = c(ci = "center"),
        exclude = "^Don|^AI",
        widths = c(subset = 1.2, ci = 2),
        subset_placement = "top",
        paddings = ft_paddings_list(data = ft_padding(2,1,5,5))

      )

      self$std_borders()
    }
  ),

  active = list(



  )
)
