library(R6)

#' @export
FT_StandardStatProps <- R6Class(
  classname = "FT_StandardStatProps", inherit = FT_StatProps,

  private = list(

  ),

  public = list(

    initialize = function() {

      super$initialize(
        footnotes = TRUE,
        titles = c("{year} BRFSS", "{label}", "{coi}"),
        box = ft_box(),
        bgs = ft_bg_list(stats = "grey88", subvars = "grey88"),
        fonts = ft_font_list(data = ft_font_parts(font.size = 12, color = "grey22"),
                             titles = ft_font_parts(font.size = 14, color = "blue", bold = TRUE)),
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
