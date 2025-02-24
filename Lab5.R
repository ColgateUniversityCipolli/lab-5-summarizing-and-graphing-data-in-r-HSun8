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
    summarize(minimum = min(get(feature), na.rm = T),
              LF = quantile(get(feature), 0.25, na.rm = T) 
                   - 1.5 * IQR(get(feature), na.rm =T),
              UF = quantile(get(feature), 0.75, na.rm = T) 
                   + 1.5 * IQR(get(feature), na.rm = T),
              maximum = max(get(feature), na.rm = T)) |>
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
                                     .default = "Within Range")) #|>
    # select(all_of(c("artist", "out.of.range", "unusual", "description")))
    # return tibble 
    essentia.summary
}

# Step 2
# numeric data
numeric.cols <- essentia.data |> 
  select(where(is.numeric)) 
# apply function to all numeric data
numeric.names = names(numeric.cols)
key.features = c()
for (i in 1:length(numeric.names)){
  diff.check <- pull(rangetest(numeric.names[i])) 
  if ("Outlying" %in% diff.check | "Out of Range" %in% diff.check){
    key.features <- c(key.features, numeric.names[i])
  }
}
# categorical data
cat.cols <- essentia.data |>
  select(!where(is.numeric))
  
  

  



    
        