# Lab 5 
# Henry Sun 
library("tidyverse")
# Step 1
essentia.data = read_csv("data/essentia.data.csv")
#rangetest <- function(feature)
  feature <- "overall.loudness"
  essentia.data|>
    group_by(artist) |>
    summarize(min = min(overall_loudness),
              LF = quantile(overall_loudness, 0.25) - 1.5 * IQR(overall_loudness),
              UF = quantile(overall_loudness, 0.75) + 1.5 * IQR(overall_loudness),
              max = max(overall_loudness)) |>
    mutate(out.of.range = if_else(overall_loudness < min | overall_loudness > max, 
                                  TRUE, FALSE)) |>
    mutate(unusual = if_else(overall_loudness < LF | overall_loudness > UF, 
                             TRUE, FALSE))
    mutate(description = if_else(out.of.range == TRUE | out.of.range == TRUE, 
                                 "Out of Range", "Within Range"))
    

    
          