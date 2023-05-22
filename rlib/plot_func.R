library(ggplot2)
library(ggprism)
library(stringr)
library(shiny)

UngroupedGGViolin <- function(data, sig_level, fig_args) {
  fig_args$fill_color <- if (length(str_split(fig_args$fill_color, ",")[[1]]) == 1 && str_split(fig_args$fill_color, ",")[[1]] != "") paste0(rep(str_split(fig_args$fill_color, ",")[[1]], length(unique(data$group))), collapse = ",") else fig_args$fill_color
  fig_args$line_color <- if (length(str_split(fig_args$line_color, ",")[[1]]) == 1 && str_split(fig_args$line_color, ",")[[1]] != "") paste0(rep(str_split(fig_args$line_color, ",")[[1]], length(unique(data$group))), collapse = ",") else fig_args$line_color
  
  final_fill_color <- if (length(str_split(fig_args$fill_color, ",")[[1]]) == length(unique(data$group))) scale_fill_manual(values = str_split(fig_args$fill_color, ",")[[1]]) else scale_fill_discrete()
  final_line_color <-   if (length(str_split(fig_args$line_color, ",")[[1]]) == length(unique(data$group))) scale_color_manual(values = str_split(fig_args$line_color, ",")[[1]]) else scale_color_discrete()
  
  ggplot(data, aes(group, value, fill = group, color = group)) +
    geom_violin(linewidth = fig_args$violin_line_width,
                scale = fig_args$violin_scale) +
    geom_boxplot(outlier.shape = NA, width = fig_args$boxplot_width,
                 linewidth = fig_args$boxplot_line_width) +
    geom_text(aes(group, label_y, label = sig.level), data = sig_level,
              size = fig_args$sig_level_label_size) +
    scale_x_discrete(guide = guide_axis(angle = fig_args$x_axis_label_rotation),
                     labels = fig_args$x_labels) +
    scale_y_continuous(limits = fig_args$y_range, breaks = fig_args$y_breaks,
                       expand = expansion(mult = c(0.05, 0), add = 0)) +
    final_fill_color + final_line_color +
    labs(x = if (fig_args$x_axis_title == "") NULL else fig_args$x_axis_title,
         y = if (fig_args$y_axis_title == "") NULL else fig_args$y_axis_title) +
    theme_prism(base_size = fig_args$base_font_size) +
    theme(legend.position = "none",
          axis.title.x = ggtext::element_markdown(),
          axis.title.y = ggtext::element_markdown(),
          axis.text.x = ggtext::element_markdown(),
          axis.text.y = ggtext::element_markdown())
}

UngroupedGGBar <- function(data, data_stat, sig_level, fig_args) {
  jitter_or_dotplot <-   if (fig_args$using_jitter) geom_jitter(aes(as.integer(group) - fig_args$point_offset, value), data = data,
                                                                shape = fig_args$point_shape, size = fig_args$point_size,
                                                                width = fig_args$jitter_width, height = fig_args$jitter_height) else geom_dotplot(aes(group, value), data = data, binaxis = "y", dotsize = fig_args$dot_size,
                                                                                                                                                  stackdir = fig_args$stack_dir, stackratio = fig_args$stack_ratio, binwidth = fig_args$bin_width)

  fig_args$fill_color <- if (length(str_split(fig_args$fill_color, ",")[[1]]) == 1 && str_split(fig_args$fill_color, ",")[[1]] != "") paste0(rep(str_split(fig_args$fill_color, ",")[[1]], length(unique(data$group))), collapse = ",") else fig_args$fill_color
  fig_args$line_color <- if (length(str_split(fig_args$line_color, ",")[[1]]) == 1 && str_split(fig_args$line_color, ",")[[1]] != "") paste0(rep(str_split(fig_args$line_color, ",")[[1]], length(unique(data$group))), collapse = ",") else fig_args$line_color
  
  final_fill_color <- if (length(str_split(fig_args$fill_color, ",")[[1]]) == length(unique(data$group))) scale_fill_manual(values = str_split(fig_args$fill_color, ",")[[1]]) else scale_fill_discrete()
  final_line_color <-   if (length(str_split(fig_args$line_color, ",")[[1]]) == length(unique(data$group))) scale_color_manual(values = str_split(fig_args$line_color, ",")[[1]]) else scale_color_discrete()
  
  ggplot(data_stat, aes(group, mean, fill = group, color = group)) +
    geom_bar(stat = "identity", linewidth = fig_args$bar_line_width, width = fig_args$bar_width) +
    geom_errorbar(aes(ymin = mean - if (fig_args$using_sd) sd else sem, ymax = mean + if (fig_args$using_sd) sd else sem),
                  linewidth = fig_args$errorbar_line_width, width = fig_args$errorbar_width) +
    jitter_or_dotplot +
    geom_text(aes(group, label_y, label = sig.level), data = sig_level,
              size = fig_args$sig_level_label_size) +
    scale_x_discrete(guide = guide_axis(angle = fig_args$x_axis_label_rotation),
                     labels = fig_args$x_labels) +
    scale_y_continuous(limits = fig_args$y_range, breaks = fig_args$y_breaks,
                       expand = expansion(mult = 0, add = 0)) +
    final_fill_color + final_line_color +
    labs(x = if (fig_args$x_axis_title == "") NULL else fig_args$x_axis_title,
         y = if (fig_args$y_axis_title == "") NULL else fig_args$y_axis_title) +
    theme_prism(base_size = fig_args$base_font_size) +
    theme(legend.position = "none",
          axis.title.x = ggtext::element_markdown(),
          axis.title.y = ggtext::element_markdown(),
          axis.text.x = ggtext::element_markdown(),
          axis.text.y = ggtext::element_markdown())
}
