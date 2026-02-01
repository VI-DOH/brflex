library(R6)

#' @export
FT_DefaultStatPropsMgr <- R6Class(
  classname = "FT_DefaultStatPropsMgr",
  inherit = FT_StatPropsMgr,

  private = list(

  ),

  public = list(

    initialize = function() {

      super$initialize(

        footnotes = TRUE,
        titles = c("{year} BRFSS", "{label}", "{coi}"),
        box = ft_box(),
        bgs_mgr = FT_DefaultBGsMgr$new(),
        fonts_mgr = FT_DefaultFontsMgr$new(),
        borders_mgr = FT_DefaultBordersMgr$new(),
        stats = c("den","num","percent"),

        aligns = c(ci = "center"),
        exclude = "^Don|^AI",
        widths = c(subset = 1.2, ci = 2),
        subset_placement = "top",
        paddings = ft_paddings_list(data = ft_padding(2,1,5,5))

      )

 #     self$std_borders()
    }
  ),

  active = list(



  )
)
