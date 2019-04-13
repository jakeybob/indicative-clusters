library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(purrr)
library(factoextra)
library(RColorBrewer)
library(dendextend)
library(heatmaply)
library(cluster)

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
plot_folder <- "plots"
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
    mutate(Party = recode(Party, "Sinn F?in" = "Sinn Féin"),
           Member = recode(Member, "Th?r?se Coffey" = "Thérèse Coffey")) %>%
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


#### ANALYSIS ####

# just the data
dat <- df %>% 
  column_to_rownames("Member") %>%
  select(starts_with("score."))
 
dat_votenames <- dat 
names(dat_votenames) <- recode(dat %>% select(starts_with("score.")) %>% names,
                               score.655 = "No Deal",
                               score.656 = "Common Market 2.0",
                               score.657 = "EFTA & EEA",
                               score.658 = "Customs Union",
                               score.659 = "Labour's Plan",
                               score.660 = "Revoke Article 50",
                               score.661 = "Confirmatory Public Vote",
                               score.662 = "Contingent Preferential Arrangements",
                               score.666 = "Customs Union (2nd time)",
                               score.667 = "Common Market 2.0 (2nd time)",
                               score.668 = "Confirmatory Public Vote (2nd time)",
                               score.669 = "Parliamentary Supremacy")
num_clusters <- 3
cluster_pal <- "Dark2" # from color brewer

# tree cut to three clusters, based on Manhattan distance
tree <- hcut(dat, k = num_clusters, hc_func = "diana", hc_metric = "manhattan")

# cluster designations joined back to original tibble
clustered_data <- full_join(fviz_cluster(tree)$data, select(df, Member, Party, starts_with("score.")), by=c("name" = "Member")) %>%
  # mutate(shp = recode(factor(cluster), "3" = "■", "2" = "▲", "1" = "●")) %>% 
  mutate(shp = recode(factor(cluster), "3" = " ???", "2" = " Leave?", "1" = " Remain?")) %>% 
  arrange(cluster, name)


#### PLOTS ####
#### dendro-heatmap
row_dend  <- dat_votenames %>% 
  diana(metric = "manhattan", diss = FALSE, stand = FALSE)  %>% 
  as.dendrogram() %>%
  set("branches_k_color", k = num_clusters, brewer.pal(n = num_clusters, cluster_pal)) %>% 
  set("branches_lwd", c(.3, .3)) %>%
  ladderize()

dendro_heatmap <- heatmaply(dat_votenames, Rowv = row_dend, Colv = NULL, row_dend_left = F, plot_method = "ggplot", 
          showticklabels = c(T, F), hide_colorbar = TRUE, column_text_angle = -45,
          margins = c(0, 0, 0, 0),
          label_names = c("name", "division", "vote"),
          scale_fill_gradient_fun = scale_fill_viridis(option = "cividis", alpha = 1),
          subplot_widths = c(.4, .6)
          # file = file.path(plot_folder, "dendro_heatmap.png")
          )
# images on medium are 1400, 2040, or 2500 px wide (for col width, outset and full-screen images respectively)
plotly_IMAGE(dendro_heatmap, width = 1400, height = 800, format = "png", out_file = file.path(plot_folder, "dendro_heatmap_1400.png"))
# dendro_heatmap_link = api_create(dendro_heatmap, filename = "dendro_heatmap") # is over 512k limit for plots
# markdown::rpubsUpload

#### PCA 2D clusters
clustered_data %>% ggplot(aes(x=x, y=y)) + 
  geom_jitter(height=.4, width=.4, aes(color=Party, shape=shp, alpha=.8, label=name)) + 
  guides(alpha = FALSE) +  #  remove from legend
  scale_fill_brewer(palette = "Dark2", aesthetics = "color", direction = -1) +
  xlim(-4, 4) + ylim(-4,4) + 
  geom_density_2d() +
  theme_bw() + 
  theme(legend.title=element_blank()) +
  theme(panel.background = element_rect(fill = '#ffffff')) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab("dimension 1") + ylab("dimension 2")

PCA_2D_clusters <- ggplotly(tooltip=c("name", "Party")) 
PCA_2D_clusters_link = api_create(PCA_2D_clusters, filename = "PCA_2D_clusters") # upload to plotly

#### PCA contribution plots
pca <- prcomp(dat_votenames)
plot_a <- fviz_pca_var(pca)

get_eigenvalue(pca)

fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 70)) + 
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank()) +
  xlab("dimensions") + 
  ylab("percentage of explained variables") + 
  ggtitle(element_blank())
ggsave(file.path(plot_folder, "pca_1_1400.png"), dpi = 300, width = (1400/300))

fviz_contrib(pca, choice = "var", axes = 1, top = 8) + 
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
        plot.margin = margin(t = 0, r = 2, b = 0, l = .5, unit = "cm")) +
  ylab("contributions (%)") + 
  xlab(element_blank()) +
  ggtitle(element_blank())

ggsave(file.path(plot_folder, "pca_2_1400.png"), dpi = 300, width = (1400/300))


fviz_contrib(pca, choice = "var", axes = 2, top = 8) + 
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
        plot.margin = margin(t = 0, r = 2.5, b = 0, l = .5, unit = "cm")) +
  ylab("contributions (%)") + 
  xlab(element_blank()) +
  ggtitle(element_blank())

ggsave(file.path(plot_folder, "pca_3_1400.png"), dpi = 300, width = (1400/300))
