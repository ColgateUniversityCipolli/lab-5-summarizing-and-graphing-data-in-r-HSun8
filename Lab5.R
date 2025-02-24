# Lab 5 
# Henry Sun 
library("tidyverse")
# Step 1
# load in data
essentia.data = read_csv("data/essentia.data.csv")
allentown.data = read_csv("data/essentia.data.allentown.csv")
# function
rangetest <- function(feature){
  #feature <- "overall_loudness"
  essentia.summary <- essentia.data |>
    # group data by artist
    group_by(artist) |>
    # summarize artist with min, LF, UF, max
    summarize(minimum = min(get(feature)),
              LF = quantile(get(feature), 0.25) - 1.5 * IQR(get(feature)),
              UF = quantile(get(feature), 0.75) + 1.5 * IQR(get(feature)),
              maximum = max(get(feature))) |>
    # out.of.range
    mutate(out.of.range = if_else(allentown.data[[feature]] < minimum | 
                                    allentown.data[[feature]] > maximum, 
                                  TRUE, FALSE)) |>
    # unsual
    mutate(unusual = if_else(allentown.data[[feature]] < LF | 
                               allentown.data[[feature]] > UF,
                             TRUE, FALSE)) |>
    mutate(description = case_when(out.of.range == TRUE ~ "Out of Range",
                                   unusual == TRUE ~ "Outlying",
                                   .default = "Within Range")) |>
    select(all_of(c("artist", "out.of.range", "unusual", "description")))
  # return tibble 
  essentia.summary
}

# Step 2
all.features = c(names(allentown.data))
# numeric data
numeric.cols <- essentia.data |> 
  select(where(is.numeric)) 
# categorical data
cat.cols <- essentia.data |>
  select(!where(is.numeric))
# apply function to all numeric data
numeric.names = names(numeric.cols)
for (i in 1:numeric.names){
  
}
  
  

  



    
        