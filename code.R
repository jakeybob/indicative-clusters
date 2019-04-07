library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(purrr)
library(cluster)
library(factoextra)

# Vote data comes from e.g. :  https://commonsvotes.digiminster.com/Divisions/Details/669
# 655: no deal
# 656: common market 2.0
# 657: EFTA & EEA
# 658: customs union
# 659: Labour's plan
# 660: revocation
# 661: confirmatory public vote
# 662: contingent preferential arrangements 
#
# 666: customs union
# 667: common market 2.0
# 668: confirmatory public vote
# 669: parliamentary supremacy


#### SETUP ####
NVR_value <- 0  #  set "no vote recorded" to 0 or NA (latter effectively removes data as distance to NA is not defined)
# NVR_value <- NA_real_

data_folder <- "data"
csv_names <- c("Division655.csv", "Division656.csv", "Division657.csv", "Division658.csv",
               "Division659.csv", "Division660.csv", "Division661.csv", "Division662.csv",
               "Division666.csv", "Division667.csv", "Division668.csv", "Division669.csv") # in form "DivisionXXX.csv"


#### DATA PREP ####
make_df <- function(data_folder, csv_name){
  path <- file.path(data_folder, csv_name)
  division_num <- paste(str_extract_all(csv_name, "[\\d]", simplify = TRUE), collapse = "") # Division669.csv -> 669
  score_col_name <- paste0("score.", division_num)
  date_col_name <- paste0("date.", division_num)
  date <- dmy(read_lines(path, skip = 1, n_max = 1)) # extract date from 2nd line of csv
  
  df <- read_csv(path, skip = 9) %>%
    mutate(!!score_col_name := recode(Vote, "Aye" = 1, "No" = -1, "No Vote Recorded" = NVR_value),
           !!date_col_name := date) %>%
    select(-`Proxy Member`, -Vote) %>%
    mutate(Party = recode(Party, "Sinn F?in" = "Sinn Féin")) %>%
    filter(Party != "Speaker", Party != "Deputy Speaker", Party != "Sinn Féin") %>% # drop those who don't vote
    mutate(Party = if_else(Member=="Nick Boles", "Independent", Party)) # fix for party change
  
  return(df)
}

# read in all the csv data, perform common manipulations and assemble into one df
df_votes <- map2(.x = data_folder, .y = csv_names, .f = make_df) %>%
  reduce(full_join, by=c("Member", "Party", "Constituency"))

# df of n-dimensional Manhattan distances (n = num of votes) between each MP
df_distances <- as_tibble(as.matrix(dist(select(df_votes, starts_with("score.")), method = "manhattan"))) %>%
  setNames(paste0("dist.", df_votes$Member)) %>%
  mutate(Member = df_votes$Member) %>%
  select("Member", names(.)[-length(names(.))]) # reorder to put member name column first

df <- full_join(df_votes, df_distances, by="Member")


#### PLOTS ####
# jittered scatter plot to show groups
p <- ggplot(df, aes(x=score.666, y=score.667, color=Party)) + 
  geom_point() +
  geom_jitter()
ggplotly(p)
plot_ly(x=df$score.666, y=df$score.667, z=df$score.668, type="scatter3d", mode="markers", color=df$Party)


#### SCRATCH ####


d <- df %>% 
  column_to_rownames("Member") %>%
  select(starts_with("score."))

t <- hcut(d, k=2, hc_func="diana", hc_metric="manhattan")
fviz_cluster(t) + theme_minimal()
fviz_dend(t, repel=T, horiz=F, type="phylogenic", show_labels = F, phylo_layout = "layout_with_drl", rect=T)
fviz_dend(t, repel=T, horiz=F, show_labels = F)

a <- fviz_cluster(t)
b <- ggplot(data=a$data, aes(x=x, y=y)) + geom_jitter(color=a$data$cluster, height=.4, width=.4) + 
  xlim(-4, 4) + ylim(-4,4) + 
  geom_density_2d() + theme_bw()

ggplotly(b)
# dist <- dist(select(df_votes, starts_with("score.")), method = "manhattan")

# these two are equivalent
# dv <- diana(dist, metric = "manhattan", diss = TRUE, stand = FALSE) # use precalculated distances
dv <- diana(d, metric = "manhattan", diss = FALSE, stand = FALSE) # diana calculates the manhattan dist

clust <- cutree(dv, k = 3)
pltree(dv, hang=-1, cex = 0.6)
rect.hclust(dv, k = 3, border = 2:10)
fviz_cluster(list(data = d, cluster = clust, stand=F, show.clust.cent	=T, repel=T, ellipse.type="norm"))


