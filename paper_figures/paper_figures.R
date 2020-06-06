library(metaconfoundr)
library(tidyverse)
library(ggtext)
library(prismatic)

values <- c(
  "#EA4335", #red
  "#FBBC05", #yellow
  "#34A853" #green
)

study_labels <- ipi_metaanalysis %>%
  mutate(study_label = paste(lead_author, year)) %>%
  select(study, study_label) %>%
  deframe()

critical_constructs <- c(
  "Sociodemographics",
  "Socioeconomics",
  "Reproductive Hx"
)

wrap_labeller <- function(x) {
  x <- ifelse(
    x %in% critical_constructs,
    paste0("**", x, "**^1"),
    x
  )
  x <- str_wrap(x, 10)
  x <- str_replace_all(x, "(Underlying|Reproductive)\n", "\\1 ")
  str_replace_all(x, "\n", "<br/>")
}

build_heatmap <- function(pal = "viridis") {
  ipi_heatmap <- ipi %>%
    metaconfoundr() %>%
    mutate(
      variable = str_wrap(variable, 10),
      construct = fct_relevel(construct, critical_constructs)
    ) %>%
    mc_heatmap(sort = TRUE, by_group = TRUE) +
    theme_mc() +
    theme(
      strip.text = element_markdown(),
      plot.caption = element_markdown()
    ) +
    facet_grid(
      . ~ construct,
      scales = "free_x",
      space = "free_x",
      labeller = as_labeller(wrap_labeller)
    ) +
    guides(x = guide_axis(n.dodge = 2)) +
    scale_y_discrete(labels = study_labels) +
    labs(caption = "^1 Construct critical for confounder control")

  if (pal == "cochrane") {
    ipi_heatmap <- ipi_heatmap +
      scale_fill_manual(values = clr_desaturate(values, .2))
  }

  plot_grobs <- ggplotGrob(ipi_heatmap)

  for(i in which(grepl("strip-t", plot_grobs$layout$name))){
    plot_grobs$grobs[[i]]$layout$clip <- "off"
  }

  plot_grobs
}

draw_heatmap <- function(x) {
  grid::grid.newpage()
  grid::grid.draw(x)
}

ggsave("paper_figures/paper_gyr.png", build_heatmap("cochrane"), width = 17, height = 9)
ggsave("paper_figures/paper_viridis.png", build_heatmap(), width = 23, height = 12)
