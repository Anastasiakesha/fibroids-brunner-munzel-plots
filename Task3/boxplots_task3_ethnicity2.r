install.packages(c(
  "readxl", "ggplot2", "dplyr", "showtext",
  "rstatix", "brunnermunzel", "purrr", "ggpubr"
))

install.packages("ggpubr")

library(readxl)
library(ggplot2)
library(dplyr)
library(showtext)
library(rstatix)
library(brunnermunzel)
library(purrr)
library(ggpubr)

showtext_auto()

data1 <- read_excel("dataset.xls")

data1 <- data1 %>%
  filter(!is.na(day_cycle_code), ethnicity==2) %>%
  mutate(
    phase = factor(
      day_cycle_code,
      levels = c(1, 2, 3, 4),
      labels = c("ФФ", "ОФ", "ЛФ", "ОА")
    ),
    fibroids = factor(
      fibroids,
      levels = c(0, 1),
      labels = c("без ММ", "ММ")
    )
  )

upper_whisker <- function(x) {
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  upper <- q3 + 1.5 * iqr
  min(max(x, na.rm = TRUE), upper)
}

# Функция Brunner–Munzel
bm_test_by_phase <- function(data, variable) {
  
  data %>%
    group_by(phase) %>%
    summarise(
      p.value = brunnermunzel.test(
        as.formula(paste(variable, "~ fibroids")),
        data = cur_data()
      )$p.value,
      
      max_whisker = max(
        tapply(
          cur_data()[[variable]],
          cur_data()$fibroids,
          upper_whisker
        ),
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      phase_num = as.numeric(phase),
      xmin = phase_num - 0.2,
      xmax = phase_num + 0.2,
      
      y.position = max_whisker * 1.05,
      
      group1 = "без ММ",
      group2 = "ММ",
      
      p_label = paste0(
        "p = ",
        formatC(p.value, format = "f", digits = 3)
      )
    )
}

# Функция построения графика
plot_box_bm <- function(data, variable, y_label, title) {
  
  bm_results <- bm_test_by_phase(data, variable)
  y_top <- max(bm_results$y.position) * 1.05
  
  y_95 <- quantile(data[[variable]], 0.95, na.rm = TRUE)
  
  ggplot(data, aes(x = phase, y = .data[[variable]], fill = fibroids)) +
    geom_boxplot(
      position = position_dodge(0.8),
      outlier.shape = NA,
      alpha = 0.8,
      color = "black"
    ) +
    stat_pvalue_manual(
      bm_results,
      label = "p_label",
      xmin = "xmin",
      xmax = "xmax",
      y.position = "y.position",
      tip.length = 0.01,
      bracket.size = 0.5
    ) +
    scale_y_continuous(
      limits = c(NA, y_top),
      expand = expansion(mult = c(0.05, 0))
    ) + 
    labs(
      title = title,
      x = "",
      y = y_label,
      fill = "",
      shape = ""
    ) +
    scale_fill_manual(values = c("grey80", "grey30")) +
    scale_shape_manual(values = c(16, 17)) +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial"),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      legend.position = "right"
    )
}

# Построение графиков
prl_plot <- plot_box_bm(
  data1, "prl", "мЕд/л", "Пролактин"
)

dheas_plot <- plot_box_bm(
  data1, "dheas", "мкг/дл", "ДГЭА-С"
)

testosterone_plot <- plot_box_bm(
  data1, "testosteron", "нг/дл", "Общий тестостерон"
)


if (!dir.exists("plots")) {
  dir.create("plots")
}

ggsave("plots/prl_plot.pdf",
       plot = prl_plot,
       width = 6, height = 4, dpi = 600)

ggsave("plots/dheas_plot.pdf",
       plot = dheas_plot,
       width = 6, height = 4, dpi = 600)

ggsave("plots/testosterone_plot.pdf",
       plot = testosterone_plot,
       width = 6, height = 4, dpi = 600)


# Вывод количества строк для каждой из фаз и наличие ММ для отладки
data1 %>%
  filter(ethnicity == 1) %>%
  count(phase, fibroids)

data1 %>%
  filter(ethnicity == 2) %>%
  count(phase, fibroids)