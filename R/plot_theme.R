gtfswizard_colors <- function(){
  c(
    ink = "#24323D",
    blue = "#2F6B8A",
    teal = "#2F8174",
    coral = "#D95D39",
    gold = "#C9952E",
    gray = "#87939D",
    light = "#E8EDF0"
  )
}

gtfswizard_palette <- function(n){
  base <- unname(gtfswizard_colors()[c("teal", "blue", "coral", "gold", "ink")])
  if(n <= length(base)){
    return(base[seq_len(n)])
  }
  grDevices::hcl.colors(n, palette = "Dark 3")
}

theme_gtfswizard <- function(base_size = 11){
  colors <- gtfswizard_colors()
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(color = colors[["ink"]]),
      plot.title = ggplot2::element_text(
        face = "bold", size = ggplot2::rel(1.25), margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        color = colors[["gray"]], margin = ggplot2::margin(b = 10)
      ),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        color = colors[["light"]], linewidth = 0.35
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(
        fill = "#F4F6F7", color = NA
      ),
      plot.margin = ggplot2::margin(10, 12, 10, 10)
    )
}

theme_gtfswizard_map <- function(base_size = 11){
  theme_gtfswizard(base_size) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}

hour_scale <- function(hours){
  finite_hours <- hours[is.finite(hours)]
  maximum <- if(length(finite_hours)){
    max(24, finite_hours)
  } else {
    24
  }
  ggplot2::scale_x_continuous(
    breaks = seq(0, ceiling(maximum / 6) * 6, by = 6)
  )
}
