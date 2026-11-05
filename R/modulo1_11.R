library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(readxl)

ggplot2::theme_set(ggplot2::theme_minimal(base_size = 11) +
                     ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                                    plot.title = ggplot2::element_text(face = "bold"),
                                    legend.position = "bottom"))