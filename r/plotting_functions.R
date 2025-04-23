#' Custom Color and Fill Scales for Sampling Date
#'
#' These functions define manual color and fill scales for representing sampling dates in `ggplot2` visualizations.
#' Different colors are assigned to each sampling date, allowing for distinct visualization of date-related data.
#'
#' @return Both functions return a scale object for use in `ggplot2` plots.
#' @export
#'
#' @examples
#' # Example usage in a ggplot
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), fill = factor(gear))) +
#'   geom_bar(position = "dodge") +
#'   fill_sample_date()
#'
#' ggplot(mtcars, aes(factor(cyl), color = factor(gear))) +
#'   geom_line() +
#'   color_sample_date()


color_column <- function() {
  ggplot2::scale_color_manual(name =  "Column Position",
                              values = c("#164C6B", "#F3A712", "#E4572E"),
                              guide = "legend")
}

#' Column Names and Labeller
#'
#' These functions handle custom column names and labeling for use in `ggplot2` or other visualizations.
#' The `col_names` vector provides a mapping between short column identifiers and their full descriptive names.
#' The `column_labeller` function retrieves the appropriate label for each column based on its value, useful for customizing facet labels or other plot components.
#'
#' @return
#' - `col_names`: A named vector that maps column identifiers to descriptive column names.
#' - `column_labeller`: A function that returns the appropriate column label for a given value.
#' @export
#'
#' @examples
#' # Accessing column names
#' col_names
#'
#' # Using column_labeller with ggplot2
#' column_labeller(variable = NULL, value = "Col1")
#'
#' # Example usage with ggplot2 for custom facet labeling
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), mpg)) +
#'   geom_boxplot() +
#'   facet_wrap(~gear, labeller = column_labeller)
col_names <- c(
  `C1` = "Col 1",
  `C2` = "Col 2",
  `C3` = "Col 3"
)

column_labeller <- function(variable, value) {
  return(col_names[value])
}

#' Day Names and Labeller
#'
#' These functions handle custom column names and labeling for use in `ggplot2` or other visualizations.
#' The `day_names` vector provides a mapping between short column identifiers and their full descriptive names.
#' The `day_labeller` function retrieves the appropriate label for each column based on its value, useful for customizing facet labels or other plot components.
#'
#' @return
#' - `day_names`: A named vector that maps column identifiers to descriptive column names.
#' - `day_labeller`: A function that returns the appropriate column label for a given value.
#' @export
#'
#' @examples
#' # Accessing column names
#' col_names
#'
#' # Using column_labeller with ggplot2
#' column_labeller(variable = NULL, value = "Col1")
#'
#' # Example usage with ggplot2 for custom facet labeling
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), mpg)) +
#'   geom_boxplot() +
#'   facet_wrap(~gear, labeller = day_labeller)
day_names <- c(
  `Day1` = "1",
  `Day2` = "2",
  `Day3` = "3",
  `Day7` = "7",
  `Day9` = "9",
  `Day10` = "10",
  `Day12` = "12",
  `Day14` = "14",
  `Day17` = "17"
)

day_labeller <- function(variable, value) {
  return(day_names[value])
}

#' Optical Variable Labeller
#'
#' These functions handle custom column names and labeling for use in `ggplot2` or other visualizations.
#' The `variables` vector provides a mapping between short column identifiers and their full descriptive names.
#' The `variable_labeller` function retrieves the appropriate label for each column based on its value, useful for customizing facet labels or other plot components.
#'
#' @return
#' - `variables`: A named vector that maps column identifiers to descriptive column names.
#' - `variable_labeller`: A function that returns the appropriate column label for a given value.
#' @export
#'
#' @examples

variables <- c(
  `log_ratio_bix` = "BIX",
  `log_ratio_fi` = "FI",
  `log_ratio_hix` = "HIX",
  `log_ratio_a254` = "a254",
  `log_ratio_E2_E3` = "E2:E3",
  `log_ratio_SR` = "SR"
)

variable_labeller <- function(variable, value) {
  return(variables[value])
}

#' Custom Theme for Boxplots
#'
#' This function applies a custom `ggplot2` theme designed specifically for boxplot visualizations.
#' It removes panel backgrounds and grid lines, adjusts text and axis line styles, and positions the legend at the bottom.
#'
#' @return A `ggplot2` theme object that can be applied to ggplot visualizations.
#' @export
#'
#' @examples
#' # Example usage in a ggplot
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), mpg)) +
#'   geom_boxplot() +
#'   theme_boxplot()
theme_boxplot <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      # General text settings
      text = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 11),
      # X-axis text settings
      axis.text.x = ggplot2::element_text(color = "black", size = 10),
      # Y-axis text settings
      axis.text.y = ggplot2::element_text(color = "black", size = 10),

      # Legend settings
      legend.position = "bottom",

      # Strip settings
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 11, color = "black"),

      # Panel and axis line settings
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.5),
      panel.border = ggplot2::element_rect(color = "black", size = 0.5),  # Add panel border
      axis.line.y.right = ggplot2::element_line(color = "black", linewidth = 0.5),

      # Grid settings
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),

      # Plot background
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),

      # Boxplot specific settings
      axis.ticks = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 10, 10, 10)  # Adjust margins as needed
    )
}
