library(metaconfoundr)
library(tidyverse)
library(ggtext)
library(prismatic)

study_labels <- ipi_metaanalysis %>%
  mutate(study_label = paste(lead_author, year)) %>%
  select(study, study_label) %>%
  deframe()

critical_constructs <- c(
  "Sociodemographics",
  "Socioeconomic Factors",
  "Reproductive Hx"
)

summary_df <- summarize_control_quality(
  metaconfoundr(ipi),
  "Socio-<br/>demo-<br/>graphics^†" = `Maternal age` & `Race/ethnicity` & `Marital status`,
  "Socio-<br/>economic<br/>Factors^‡" = `SES category` | Insurance & Education,
  "Repro-<br/>ductive Hx^§" = `Prior pregnancy outcome`
)


wrap_labeller <- function(x) {
  x <- ifelse(
    x %in% critical_constructs,
    paste0("**", x, "**^\\*"),
    x
  )

  x <- case_when(
    str_detect(x, critical_constructs[1]) ~ paste0(x, "^†"),
    str_detect(x, critical_constructs[2]) ~ paste0(x, "^‡"),
    str_detect(x, critical_constructs[3]) ~ paste0(x, "^§"),
    TRUE ~ x
  )

  x <- str_wrap(x, 10)
  x <- str_replace_all(x, "(Underlying|Reproductive)\n", "\\1 ")
  str_replace_all(x, "\n", "<br/>")
}

build_plot <- function(type = "heatmap", pal = "viridis", sym = FALSE) {
  ipi_data <- ipi %>%
    metaconfoundr() %>%
    mutate(
      construct = ifelse(construct == "Socioeconomics", "Socioeconomic Factors", construct),
      variable = str_wrap(variable, 10),
      variable = ifelse(variable == "Hypertension", "Hyper-\ntension", variable),
      construct = fct_relevel(construct, critical_constructs)
    )

  if (type == "heatmap") {
    ipi_plot <- ipi_data %>%
      mc_heatmap(sort = TRUE, by_group = TRUE)
  } else {
    ipi_plot <- ipi_data %>%
      mc_trafficlight(sort = TRUE, by_group = TRUE, size = 12.5)
  }

  if (sym) ipi_plot <- ipi_plot + geom_cochrane(size = 6.5)

  ipi_plot <- ipi_plot +
    theme_mc() +
    theme(
      strip.text = element_markdown(),
      plot.caption = element_markdown(size = rel(.7))
    ) +
    facet_grid(
      . ~ construct,
      scales = "free_x",
      space = "free_x",
      labeller = as_labeller(wrap_labeller)
    ) +
    guides(x = guide_axis(n.dodge = 2)) +
    scale_y_discrete(labels = study_labels) +
    labs(
      caption = paste(c(
        "<sup>\\*</sup>Core set of confounding constructs",
        "<sup>†</sup>Measured by maternal age, paternity or marital status, and race/ethnicity",
        "<sup>‡</sup>Measured by SES category or both education and insurance status",
        "<sup>§</sup>Measured by outcome of the prior pregnancy, e.g. liveborn versus stillbirth"
      ), collapse = "<br/>")
    )

  if (pal == "cochrane") {
    ipi_plot <- ipi_plot +
      scale_fill_cochrane()
  }

  plot_grobs <- ggplotGrob(ipi_plot)

  for(i in which(grepl("strip-t", plot_grobs$layout$name))){
    plot_grobs$grobs[[i]]$layout$clip <- "off"
  }

  plot_grobs
}

draw_heatmap <- function(x) {
  grid::grid.newpage()
  grid::grid.draw(x)
}

build_summary_plot <- function(x = summary_df, type = "heatmap", pal = "viridis", sym = FALSE) {
  if (type == "heatmap") {
    ipi_plot <- x %>%
      mc_heatmap()
  } else {
    ipi_plot <- x %>%
      mc_trafficlight(size = 10)
  }

  if (sym) ipi_plot <- ipi_plot + geom_cochrane()

  if (pal == "cochrane") {
    ipi_plot <- ipi_plot +
      scale_fill_cochrane()
  }


  ipi_plot +
    scale_y_discrete(labels = study_labels) +
    facet_constructs() +
    theme_mc() +
    theme(
      legend.position = "right",
      plot.caption.position = "plot",
      axis.text.x = element_markdown(),
      plot.caption = element_markdown(size = rel(.5))
    ) +
    guides(x = guide_axis(n.dodge = 2)) +
    labs(
      caption = paste(c(
        "<sup>†</sup>Measured by maternal age, paternity or marital status, and race/ethnicity",
        "<sup>‡</sup>Measured by SES category or both education and insurance status",
        "<sup>§</sup>Measured by outcome of the prior pregnancy, e.g. liveborn versus stillbirth"
      ), collapse = "<br/>")
    )
}

# ------ Individual plots ------

# heatmap + no symbols
ggsave("paper_figures/01_paper_gyr.png", build_plot(pal = "cochrane"), width = 17, height = 9.5)
ggsave("paper_figures/02_paper_viridis.png", build_plot(), width = 17, height = 9.5)

# heatmap + symbols
ggsave("paper_figures/03_paper_gyr_sym.png", build_plot(pal = "cochrane", sym = TRUE), width = 17, height = 9.5)
ggsave("paper_figures/04_paper_viridis_sym.png", build_plot(sym = TRUE), width = 17, height = 9.5)

# traffic light + no symbols
ggsave("paper_figures/05_paper_gyr_tl.png", build_plot(type = "tl", pal = "cochrane"), width = 17, height = 9.5)
ggsave("paper_figures/06_paper_viridis_tl.png", build_plot(type = "tl"), width = 17, height = 9.5)

# traffic light + symbols
ggsave("paper_figures/07_paper_gyr_tl_sym.png", build_plot(type = "tl", pal = "cochrane", sym = TRUE), width = 17, height = 9.5)
ggsave("paper_figures/08_paper_viridis_tl_sym.png", build_plot(type = "tl", sym = TRUE), width = 17, height = 9.5)

# ------ Summary plots ------

# heatmap + no symbols
ggsave("paper_figures/09_paper_gyr_summary.png", build_summary_plot(pal = "cochrane"), width = 5.5, height = 7)
ggsave("paper_figures/10_paper_viridis_summary.png", build_summary_plot(), width = 5.5, height = 7)

# heatmap + symbols
ggsave("paper_figures/11_paper_gyr_sym_summary.png", build_summary_plot(pal = "cochrane", sym = TRUE), width = 5.5, height = 7)
ggsave("paper_figures/12_paper_viridis_sym_summary.png", build_summary_plot(sym = TRUE), width = 5.5, height = 7)

# traffic light + no symbols
ggsave("paper_figures/13_paper_gyr_tl_summary.png", build_summary_plot(type = "tl", pal = "cochrane"), width = 5.5, height = 7)
ggsave("paper_figures/14_paper_viridis_tl_summary.png", build_summary_plot(type = "tl"), width = 5.5, height = 7)

# traffic light + symbols
ggsave("paper_figures/15_paper_gyr_tl_sym_summary.png", build_summary_plot(type = "tl", pal = "cochrane", sym = TRUE), width = 5.5, height = 7)
ggsave("paper_figures/16_paper_viridis_tl_sym_summary.png", build_summary_plot(type = "tl", sym = TRUE), width = 5.5, height = 7)

fs::dir_ls("paper_figures/", regexp = "\\.png") %>%
  zip(zipfile = "paper_figures/paper_figures.zip", files = .)
