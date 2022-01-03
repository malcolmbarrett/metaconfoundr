library(metaconfoundr)
library(tidyverse)
library(ggtext)
library(prismatic)
library(janitor)

critical_constructs <- c(
  "Reproductive Hx",
  "Sociodemographics",
  "Socioeconomic Factors"
)

ds_wide <- readxl::read_xlsx("Data confounder matrix - IPI example 2021-08_main analysis2.xlsx", sheet = "Variables") %>%
  clean_names() %>%
  mutate_at(-1:-3, ~ replace_na(., 0)) %>%
  mutate_at(
    -1:-3,
    function(x) case_when(x == 3 ~ 0, x == 2 ~ 1, x == 1 ~ 2)
  )
ds_wide <- filter(ds_wide, !construct %in% c("Biology/Genetics", "Structural Racism"))

ds <- metaconfoundr(ds_wide)

ds <- ds %>%
  mutate(
    construct = ifelse(construct == "Socioeconomics", "Socioeconomic Factors", construct),
    variable = str_wrap(variable, 10),
    variable = ifelse(variable == "Hypertension", "Hyper-\ntension", variable),
    construct = fct_relevel(construct, critical_constructs)
  )

study_labels <- sort(c("Zhu 2001a", "Zhu 2001b", "Zhu 1999", "Salihu 2012a", "Salihu 2012b", "Smith 2003", "Shachar 2016", "Ball 2014", "Coo 2017", "Hanley 2017", "de Weger 2011"))

wrap_labeller <- function(x) {
  x <- ifelse(
    x %in% critical_constructs,
    paste0("", x, "^*"),
    x
  )

  x <- case_when(
    str_detect(x, critical_constructs[1]) ~ paste0(x, "^???"),
    str_detect(x, critical_constructs[2]) ~ paste0(x, "^???"),
    str_detect(x, critical_constructs[3]) ~ paste0(x, "^?"),
    TRUE ~ x
  )

  x <- str_wrap(x, 10)
  x <- str_replace_all(x, "(Underlying|Reproductive)\n", "\ ")
  str_replace_all(x, "\n", "
")
}

scale_fill_cochrane <- function(...) {
  ggplot2::scale_fill_manual(
    values = c("#D85247FF", "#409C58FF"),
    ...
  )
}

build_plot <- function() {
  ipi_plot <- ds %>%
    mc_trafficlight(sort = TRUE, by_group = TRUE, size = 12.5) +
    geom_cochrane(size = 6.5) +
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
        "*Core set of confounding constructs",
        "???Measured by outcome of the prior pregnancy, e.g. liveborn versus stillbirth",
        "???Measured by maternal age, paternity or marital status, and race/ethnicity",
        "?Measured by SES index or both education and insurance status"
      ), collapse = "
")
    ) +
    scale_fill_cochrane()

  plot_grobs <- ggplotGrob(ipi_plot)

  for (i in which(grepl("strip-t", plot_grobs$layout$name))) {
    plot_grobs$grobs[[i]]$layout$clip <- "off"
  }

  plot_grobs
}

ggsave("confounders.pdf", build_plot(), width = 17, height = 9.5)

summary_df <- read.csv("summary_ipi2021.csv")
summary_df$control_quality <- factor(summary_df$control_quality, levels = c("inadequate", "some concerns", "adequate"))
summary_df <- summary_df[summary_df$variable %in% c("overall", "Socio-
demo-
graphics^???", "Socio-
economic
Factors^???", "Repro-
ductive Hx^?"), ]

summary_df <- summary_df[order(match(summary_df$variable, c("overall", "Socio-
demo-
graphics^???", "Socio-
economic
Factors^???", "Repro-
ductive Hx^?"))), ]

build_summary_plot <- function() {
  ipi_plot <- summary_df %>%
    mc_trafficlight(size = 10) +
    geom_cochrane() +
    scale_fill_cochrane() +
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
        "???Measured by maternal age, paternity or marital status, and race/ethnicity",
        "???Measured by SES category or both education and insurance status",
        "?Measured by outcome of the prior pregnancy, e.g. liveborn versus stillbirth"
      ), collapse = "
")
    )
}

ggsave("confounder_summary.pdf", build_summary_plot(), width = 5.5, height = 7)

or <- c(1.20, 1.30, 1.40, 1.58, 1.98, 1.60, 1.71, 1.41, 1.53, 1.78, 1.92)
lb <- c(1.10, 1.20, 1.30, 1.37, 0.50, 1.30, 1.65, 1.31, 1.35, 1.63, 1.79)
ub <- c(1.30, 1.40, 1.50, 1.83, 7.85, 2.00, 1.78, 1.51, 1.73, 1.95, 2.07)
logor <- log(or)
selogor <- (log(ub) - log(lb)) / (2 * qnorm(0.975))
study <- c("Zhu 2001a", "Zhu 2001b", "Zhu 1999", "Salihu 2012a", "Salihu 2012b", "Smith 2003", "Shachar 2016", "Ball 2014", "Hanley 2017", "Coo 2017", "de Weger 2011")
studyc <- c("zhu_2001a", "zhu_2001b", "zhu_1999", "salihu_2012a", "salihu_2012b", "smith_2003", "shachar_2016", "ball_2014", "hanley_2017", "coo_2017", "de_weger_2011")
dat <- data.frame(study, studyc, logor, selogor)

summary <- read.csv("summary_ipi2021.csv")
summary$construct <- NULL
summary_wide <- spread(summary, variable, control_quality)
summary_wide <- rename(summary_wide, c(
  "studyc" = "study", "familyplanning" = "Family
Planning^?",
  "healthbehaviors" = "Health
Behaviors",
  "reproductivehistory" = "Repro-
ductive Hx^?",
  "sociodemographics" = "Socio-
demo-
graphics^???",
  "socioeconomics" = "Socio-
economic
Factors^???",
  "structuredracism" = "Structured
Racism^#",
  "underlyinghealth" = "Underlying
Health"
))

dat <- merge(dat, summary_wide, by = "studyc")

dat$overall <- factor(dat$overall, levels = c("adequate", "some concerns", "inadequate"))
dat$sociodemographics <- factor(dat$sociodemographics, levels = c("adequate", "some concerns", "inadequate"))
dat$socioeconomics <- factor(dat$socioeconomics, levels = c("adequate", "some concerns", "inadequate"))
dat$reproductivehistory <- factor(dat$reproductivehistory, levels = c("adequate", "some concerns", "inadequate"))

library(meta)
res1 <- metagen(TE = logor, seTE = selogor, studlab = study, method.tau = "REML", hakn = TRUE, sm = "OR", comb.fixed = FALSE, data = dat)
summary(res1)
funnel(res1)
pdf(file = "confounder matrix\fig0.ma.pdf", width = 7, height = 4)
forest(res1, leftcols = c("studlab"), backtransf = TRUE, hetstat = TRUE, hetlab = "", print.pval.Q = FALSE, sortvar = dat$logor)
dev.off()

metainf(res1, "random")

# subgroup analysis by overall control
res2 <- metagen(TE = logor, seTE = selogor, studlab = study, method.tau = "REML", hakn = TRUE, sm = "OR", comb.fixed = FALSE, byvar = overall, tau.common = FALSE, data = dat)
pdf(file = "confounder matrix\fig1.overall.pdf", width = 7, height = 5)
forest(res2,
  leftcols = c("studlab"), backtransf = TRUE, bylab = "", hetstat = TRUE, resid.hetstat = FALSE,
  hetlab = "", resid.hetlab = "", print.pval.Q = FALSE, sortvar = dat$logor, test.subgroup.random = TRUE, label.test.subgroup.random = "Interaction test", print.Q.subgroup = FALSE
)
dev.off()
summary(res2)
metareg(res2)

res.concerns <- metagen(TE = logor, seTE = selogor, studlab = study, method.tau = "REML", hakn = TRUE, sm = "OR", comb.fixed = FALSE, data = dat[which(dat$overall == "some concerns"), ])
metainf(res.concerns, "random")
res.inadequate <- metagen(TE = logor, seTE = selogor, studlab = study, method.tau = "REML", hakn = TRUE, sm = "OR", comb.fixed = FALSE, data = dat[which(dat$overall == "inadequate"), ])
metainf(res.inadequate, "random")

res2 <- metagen(TE = logor, seTE = selogor, studlab = study, method.tau = "REML", hakn = TRUE, sm = "OR", comb.fixed = FALSE, byvar = overall, tau.common = FALSE, data = dat[dat$study != "Zhu 2001b", ])
forest(res2,
  leftcols = c("studlab"), backtransf = TRUE, bylab = "", hetstat = TRUE, resid.hetstat = FALSE,
  hetlab = "", resid.hetlab = "", print.pval.Q = FALSE, test.subgroup.random = TRUE, label.test.subgroup.random = "Interaction test", print.Q.subgroup = FALSE
)

# subgroup analysis by socioeconomics
res3 <- metagen(TE = logor, seTE = selogor, studlab = study, method.tau = "REML", hakn = TRUE, sm = "OR", comb.fixed = FALSE, byvar = socioeconomics, tau.common = FALSE, data = dat)
pdf(file = "confounder matrix\fig2.socioeconomics.pdf", width = 7, height = 6)
forest(res3,
  leftcols = c("studlab"), backtransf = TRUE, bylab = "", hetstat = TRUE, resid.hetstat = FALSE,
  hetlab = "", resid.hetlab = "", print.pval.Q = FALSE, sortvar = dat$logor, test.subgroup.random = TRUE, label.test.subgroup.random = "Interaction test", print.Q.subgroup = FALSE
)
dev.off()
summary(res3)
metareg(res3)

# subgroup analysis by sociodemographics
res4 <- metagen(TE = logor, seTE = selogor, studlab = study, method.tau = "REML", hakn = TRUE, sm = "OR", comb.fixed = FALSE, byvar = sociodemographics, tau.common = FALSE, data = dat)
summary(res4)
pdf(file = "confounder matrix\fig3.sociodemographics.pdf", width = 7, height = 6)
forest(res4,
  leftcols = c("studlab"), backtransf = TRUE, bylab = "", hetstat = TRUE, resid.hetstat = FALSE,
  hetlab = "", resid.hetlab = "", print.pval.Q = FALSE, sortvar = dat$logor, test.subgroup.random = TRUE, label.test.subgroup.random = "Interaction test", print.Q.subgroup = FALSE
)
dev.off()
metareg(res4)

# subgroup analysis by reproductive history
res5 <- metagen(TE = logor, seTE = selogor, studlab = study, method.tau = "REML", hakn = TRUE, sm = "OR", comb.fixed = FALSE, byvar = reproductivehistory, tau.common = TRUE, data = dat)
pdf(file = "fig4.reproductive history.pdf", width = 7, height = 6)
forest(res5,
  leftcols = c("studlab"), backtransf = TRUE, bylab = "", hetstat = TRUE, resid.hetstat = FALSE,
  hetlab = "", resid.hetlab = "", print.pval.Q = FALSE, sortvar = dat$logor, test.subgroup.random = TRUE, label.test.subgroup.random = "Interaction test", print.Q.subgroup = FALSE
)
dev.off()
summary(res5)
metareg(res5)
