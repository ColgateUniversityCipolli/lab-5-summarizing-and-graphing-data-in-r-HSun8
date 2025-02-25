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
features.tab = tibble()
for (i in 1:length(numeric.names)){
  diff.check <- pull(rangetest(numeric.names[i])) 
  # save all features that contain a single outlier
  if ("Outlying" %in% diff.check | "Out of Range" %in% diff.check){
    key.features = c(key.features, numeric.names[i])
    features.tab <- rbind(features.tab, diff.check)
  }
}


# categorical data to be processed
cat.cols <- essentia.data |>
  select(!where(is.numeric))

# Step 3
library("xtable")
# consolidate all features into a tibble
all.features <- tibble(key.features, features.tab) 
final.features <- all.features |>
  rename("Key Features" = "key.features",
         "All Get Out" = "X.Within.Range.",
         "Manchester Orchestra" = "X.Within.Range..1",
         "The Front Bottoms" = "X.Outlying.") |>
  # still too many variables, 
  # now only sort through vars with one band in range
  filter((rowSums(all.features == "Within Range")) == 1)
  
features.tab = xtable(final.features)

#print(features.tab) 

# Step 4
# plots

# categorical data (probably not included)
df.cat <- essentia.data %>%
  dplyr::select("chords_key", "artist") %>%
  drop_na() %>%
  group_by(!!sym("chords_key"), !!sym("artist")) %>%
  summarise(Observations = sum(!is.na(!!sym("chords_key"))), .groups = "drop") %>%
  tidyr::complete(!!sym("chords_key"), !!sym("artist")) %>%
  replace_na(list(Observations = 0)) %>%
  group_by(!!sym("artist")) %>%
  mutate(Proportion = Observations / sum(Observations)) %>%
  arrange(desc(!!sym("chords_key"))) %>%
  mutate(Percent = Proportion * 100) %>%
  mutate(denoted.group = paste("artist", " = ", !!sym("artist"), sep = ""))
####################################
# Create Plot
####################################
cat.plot <- ggplot(df.cat, aes(x = !!sym("chords_key"), y = Proportion)) +
  geom_bar(stat = "identity", width = 0.5, fill = "lightblue") +
  get("theme_bw")() +
  xlab("chords_key") +
  ylab(ifelse("Proportion" == "", "Proportion", "Proportion")) +
  ggtitle("", "") +
  geom_hline(yintercept = 0) +
  facet_wrap(~denoted.group)
####################################
# Print Plot
####################################
cat.plot
# numeric data 
poswords.plot <- ggplot(data=essentia.data,                       
       aes(x=positivewords, y=artist)) +      
  #geom_violin(fill="grey80")+
  geom_boxplot(fill="grey80") +
  theme_bw()+                                  
  xlab("Positive Words")+                       
  ylab("Artist")+   
  geom_vline(xintercept=allentown.data$positivewords)
coord_flip()
poswords.plot

# plot showing within range for each band
dat.within = final.features |>
  summarize(fb.within = sum(pull(final.features) == "Within Range"),
            mo.within = sum(pull(final.features, var = -2) == "Within Range"),
            ago.within = sum(pull(final.features, var = -3) == "Within Range"))

# one more continuous plot  
avgloudness.plot <- ggplot(data=essentia.data,                       
                        aes(x=average_loudness, y=artist)) +      
  geom_violin(fill="grey80")+
  geom_boxplot(width = 0.05) +
  theme_bw()+                                  
  xlab("Average Loudness")+                       
  ylab("Artist")+   
  geom_vline(xintercept=allentown.data$average_loudness)
coord_flip()
avgloudness.plot
    
        