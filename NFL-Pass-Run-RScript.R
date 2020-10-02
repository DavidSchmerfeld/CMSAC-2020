#Optimum Pass/Run Ratio in NFL
#CMSAC Reproducible Research Competition
#October 2020

#=========================================================================

#I. Data Set

#Pre-Processing Steps

#Load the necessary packages
library(tidyverse)
library(na.tools)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggimage)
library(rootSolve)

#Read 2019 play-by-play data from NFLScrapR
pbp19all <- read_csv(url("http://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2019.csv"))

#Select the relevant columns in the play-by-play data
pbp19 <- pbp19all %>% 
  select(posteam, down, ydstogo, play_type, qb_kneel, qb_spike, qb_scramble, run_location, run_gap, epa, wp, third_down_converted, fourth_down_converted, sack)

#Only include pass & run plays & remove 2-pt conversions (down=NA), & NAs for EPAs & WPs
pbp19_rp <- pbp19 %>% 
  filter(play_type=="pass" | play_type=="run", down<=4, !is_na(epa), !is_na(wp))

#Create new column for specific run direction & redefine QB scrambles as pass plays
pbp19_rp <- pbp19_rp %>%
  mutate(play_dir = case_when(
    qb_scramble == 0 & play_type == "run" & run_location == "middle" ~ "middle run",
    qb_scramble == 0 & play_type == "run" & run_location == "left" & run_gap == "guard" ~ "middle run",
    qb_scramble == 0 & play_type == "run" & run_location == "left" & run_gap == "tackle" ~ "outside run",
    qb_scramble == 0 & play_type == "run" & run_location == "left" & run_gap == "end" ~ "outside run",
    qb_scramble == 0 & play_type == "run" & run_location == "right" & run_gap == "guard" ~ "middle run",
    qb_scramble == 0 & play_type == "run" & run_location == "right" & run_gap == "tackle" ~ "outside run",
    qb_scramble == 0 & play_type == "run" & run_location == "right" & run_gap == "end" ~ "outside run",
    play_type == "pass" ~ "pass play",
    qb_scramble == 1 ~ "pass play"
  ))

#Create new columns based on redefined run & pass plays
pbp19_rp <- pbp19_rp %>%
  mutate(run_pass = ifelse(play_dir=="pass play", "pass", "run")) %>%
  mutate(pass_pct_count = ifelse(play_dir=="pass play", 1, 0))

#=========================================================================

#II. Expected Points Added (Pass Plays vs. Running Plays)

#Compare EPAs of pass plays & each run direction
pbp19_rp %>%
  filter(!is_na(run_pass)) %>%
  group_by(play_dir) %>%
  summarize(mean_epa = mean(epa)) %>%
  arrange(desc(mean_epa))

#-------------------------------------------------------------------------

#First Down Analysis of EPA by Play Type

#Check how the EPAs of each play type vary based on yards-to-go to first down
ytg_1 <- pbp19_rp %>%
  filter(!is_na(run_pass), down==1) %>%
  group_by(ydstogo, play_dir) %>%
  summarize(mean_epa = mean(epa))

#Plot the EPAs for each play type
ytg_1_plot <- ggplot(ytg_1,aes(x=ydstogo,y=mean_epa,color=play_dir)) + 
  geom_smooth(method="lm", fullrange=TRUE, se=FALSE) +
  scale_x_reverse() + coord_cartesian(xlim=c(20, 0), ylim=c(-0.25,0.25)) +
  ggtitle("First Down") +
  labs(x="Yards to Go", y="Expected Points Added\n (EPA)") +
  geom_label(aes(x=16, y=0.175, label=("Pass\n Plays"), fontface="bold",
                 color="pass play"), size=2.5, show.legend = FALSE) + 
  geom_label(aes(x=16, y=0.025, label=("Outside\n Runs"), fontface="bold",
                 color="outside run"), size=2.5, show.legend = FALSE) +
  geom_label(aes(x=16, y=-0.1, label=("Middle\n Runs"), fontface="bold",
                 color="middle run"), size=2.5, show.legend = FALSE) +
  geom_vline(xintercept=0, color="yellow", size=5) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

#-------------------------------------------------------------------------

#Second Down Analysis of EPA by Play Type

#Check how the EPAs of each play type vary based on yards-to-go to first down
ytg_2 <- pbp19_rp %>%
  filter(!is_na(run_pass), down==2) %>%
  group_by(ydstogo, play_dir) %>%
  summarize(mean_epa = mean(epa))

#Plot the EPAs for each play type
ytg_2_plot <- ggplot(ytg_2,aes(x=ydstogo,y=mean_epa,color=play_dir)) + 
  geom_smooth(method="lm", fullrange=TRUE, se=FALSE) +
  scale_x_reverse() + coord_cartesian(xlim=c(20, 0), ylim=c(-0.25,0.25)) +
  ggtitle("Second Down") +
  labs(x="Yards to Go", y="Expected Points Added\n (EPA)") +
  geom_label(aes(x=16, y=-0.075, label=("Pass\n Plays"), fontface="bold",
                 color="pass play"), size=2.5, show.legend = FALSE) + 
  geom_label(aes(x=18, y=-0.2, label=("Outside\n Runs"), fontface="bold",
                 color="outside run"), size=2.5, show.legend = FALSE) +
  geom_label(aes(x=13, y=-0.2, label=("Middle\n Runs"), fontface="bold",
                 color="middle run"), size=2.5, show.legend = FALSE) +
  geom_vline(xintercept=0, color="yellow", size=5) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

#------------------------------------------------------------------------

#Compare down & distance plots for first and second downs
grid.arrange(ytg_1_plot, ytg_2_plot, nrow=1, 
             bottom=textGrob("Data from nflscrapR", gp=gpar(fontsize=9)))

#------------------------------------------------------------------------

#Determine average EPAs on 1st and 10
ytg_1_10 <- ytg_1 %>%
  filter(ydstogo==10)

ytg_1_10

#------------------------------------------------------------------------

#Third Down Analysis of EPA by Play Type

#Check how the EPAs of each play type vary based on yards-to-go to first down
ytg_3 <- pbp19_rp %>%
  filter(!is_na(run_pass), down==3, third_down_converted==1) %>%
  group_by(ydstogo, play_dir) %>%
  summarize(mean_epa = mean(epa))

#Plot the EPAs for each play type
ytg_3_plot <- ggplot(ytg_3,aes(x=ydstogo,y=mean_epa,color=play_dir)) + 
  geom_smooth(method="lm", fullrange=TRUE, se=FALSE) +
  scale_x_reverse() + coord_cartesian(xlim=c(15, 0), ylim=c(0.5,3.5)) +
  ggtitle("Third Down") +
  labs(x="Yards to Go", y="Expected Points Added\n (EPA)") +
  geom_label(aes(x=8, y=2.4, label=("Pass\n Plays"), fontface="bold",
                 color="pass play"), size=2.5, show.legend = FALSE) + 
  geom_label(aes(x=10.75, y=2.25, label=("Outside\n Runs"), fontface="bold", color="outside run"), size=2.5, show.legend = FALSE) +
  geom_label(aes(x=12.5, y=2, label=("Middle\n Runs"), fontface="bold",
                 color="middle run"), size=2.5, show.legend = FALSE) +
  geom_vline(xintercept=0, color="yellow", size=5) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

#----------------------------------------------------------------------

#Fourth Down Analysis of EPA by Play Type

#Check how the EPAs of each play type vary based on yards-to-go to first down
ytg_4 <- pbp19_rp %>%
  filter(!is_na(run_pass), down==4, fourth_down_converted==1) %>%
  group_by(ydstogo, play_dir) %>%
  summarize(mean_epa = mean(epa))

#Plot the EPAs for each play type
ytg_4_plot <- ggplot(ytg_4,aes(x=ydstogo,y=mean_epa,color=play_dir)) + 
  geom_smooth(method="lm", fullrange=TRUE, se=FALSE) +
  scale_x_reverse() + coord_cartesian(xlim=c(10, 0), ylim=c(0,4.5)) +
  ggtitle("Fourth Down") +
  labs(x="Yards to Go", y="Expected Points Added\n (EPA)") +
  geom_label(aes(x=6.25, y=3, label=("Pass\n Plays"), fontface="bold",
                 color="pass play"), size=2.5, show.legend = FALSE) + 
  geom_label(aes(x=7.5, y=4, label=("Outside\n Runs"), fontface="bold",
                 color="outside run"), size=2.5, show.legend = FALSE) +
  geom_label(aes(x=5, y=1.35, label=("Middle\n Runs"), fontface="bold",
                 color="middle run"), size=2.5, show.legend = FALSE) +
  geom_vline(xintercept=0, color="yellow", size=5) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

#----------------------------------------------------------------------

#Compare down & distance plots for third and fourth downs
grid.arrange(ytg_3_plot, ytg_4_plot, nrow=1, 
             bottom=textGrob("Data from nflscrapR", gp=gpar(fontsize=9)))

#----------------------------------------------------------------------

#Determine the overall EPA averages for fourth down conversions
pbp19_rp %>%
  filter(!is_na(run_pass), down==4, fourth_down_converted==1) %>%
  group_by(play_dir) %>%
  summarize(mean_epa = mean(epa), total=n())

#Determine R squared values of linear regression models

#First Down
lm1_pass <- lm(mean_epa ~ ydstogo, 
               data=ytg_1[ytg_1$play_dir=="pass play",])
summary(lm1_pass) #0.0541

lm1_outside_run <- lm(mean_epa ~ ydstogo, 
                      data=ytg_1[ytg_1$play_dir=="outside run",])
summary(lm1_outside_run) #0.0115

lm1_middle_run <- lm(mean_epa ~ ydstogo, 
                     data=ytg_1[ytg_1$play_dir=="middle run",])
summary(lm1_middle_run) #0.1335

#Second Down
lm2_pass <- lm(mean_epa ~ ydstogo, 
               data=ytg_2[ytg_2$play_dir=="pass play",])
summary(lm2_pass) #0.3326

lm2_outside_run <- lm(mean_epa ~ ydstogo, 
                      data=ytg_2[ytg_2$play_dir=="outside run",])
summary(lm2_outside_run) #0.3303

lm2_middle_run <- lm(mean_epa ~ ydstogo, 
                     data=ytg_2[ytg_2$play_dir=="middle run",])
summary(lm2_middle_run) #0.3617

#Third Down Conversions
lm3_pass <- lm(mean_epa ~ ydstogo, 
               data=ytg_3[ytg_3$play_dir=="pass play",])
summary(lm3_pass) #0.8089

lm3_outside_run <- lm(mean_epa ~ ydstogo, 
                      data=ytg_3[ytg_3$play_dir=="outside run",])
summary(lm3_outside_run) #0.4327

lm3_middle_run <- lm(mean_epa ~ ydstogo, 
                     data=ytg_3[ytg_3$play_dir=="middle run",])
summary(lm3_middle_run) #0.3865

#Fourth Down Conversions
lm4_pass <- lm(mean_epa ~ ydstogo, 
               data=ytg_4[ytg_4$play_dir=="pass play",])
summary(lm4_pass) #0.0254

lm4_outside_run <- lm(mean_epa ~ ydstogo, 
                      data=ytg_4[ytg_4$play_dir=="outside run",])
summary(lm4_outside_run) #0.4161

lm4_middle_run <- lm(mean_epa ~ ydstogo, 
                     data=ytg_4[ytg_4$play_dir=="middle run",])
summary(lm4_middle_run) #1.0

#======================================================================

#III. Expected Points Added (Pass Plays vs. Running Plays)

#Determine total number of pass and run plays in 2019
pbp19_rp_sum <- pbp19_rp %>%
  filter(!is_na(run_pass)) %>%
  group_by(run_pass) %>%
  summarize(play_count=n(), scrambles=sum(qb_scramble), sacks=sum(sack))

pbp19_rp_sum

#Pass Play Percentage
pbp19_rp_sum$play_count/sum(pbp19_rp_sum$play_count)

#Pass/Run Ratio
pbp19_rp_sum$play_count[pbp19_rp_sum$run_pass=="pass"]/pbp19_rp_sum$play_count[pbp19_rp_sum$run_pass=="run"]

#Total scrambles & sacks
pbp19_rp_sum

#Total QB knees excluded
sum(pbp19$qb_kneel)

#Total QB spikes excluded
sum(pbp19$qb_spike)

#======================================================================

#IV. Relationship Between WP & EPA

#Load packages to include embedded tweet
library(tweetrmd)
library(webshot2)

#Embed tweet
tweet_screenshot(tweet_url("benbaldwin", "1206640759899918336"))

#Create new column with rounded WP values to nearest 10
rnd_base <- 10

pbp19_rp <- pbp19_rp %>%
  mutate(wp_round = rnd_base*round((wp*100)/rnd_base))

#Determine EPA at each rounded WP value for pass plays & run plays
epa_wp <- pbp19_rp %>%
  filter(!is_na(run_pass)) %>%
  group_by(wp_round, run_pass) %>%
  summarize(mean_epa = mean(epa), plays=n())

#Determine EPA at each rounded WP value for pass plays
epa_wp_pass <- pbp19_rp %>%
  filter(!is_na(run_pass), run_pass=="pass") %>%
  group_by(wp_round) %>%
  summarize(mean_epa = mean(epa), plays=n())

#Determine EPA at each rounded WP value for run plays
epa_wp_run <- pbp19_rp %>%
  filter(!is_na(run_pass), run_pass=="run") %>%
  group_by(wp_round) %>%
  summarize(mean_epa = mean(epa), plays=n())

#Plot the EPAs at each rounded WP value for pass plays & run plays
plot_epa_wp <- ggplot(epa_wp, aes(x=wp_round, y=mean_epa, color=run_pass)) +
  geom_point() + 
  geom_smooth(method="loess", aes(fill=run_pass)) +
  labs(x="Win Probability (WP)", y="Expected Points Added\n(EPA)", 
       caption = "Data from nflscrapR") +
  scale_color_discrete(labels=c("Pass EPA", "Run EPA")) +
  scale_fill_discrete(labels=c("Pass EPA", "Run EPA")) +
  theme(legend.title=element_blank())

plot_epa_wp

#Determine the WP where the lines intersect
f1 <- loess(epa_wp_pass$mean_epa ~ epa_wp_pass$wp_round, data=epa_wp_pass)
f2 <- loess(epa_wp_run$mean_epa ~ epa_wp_run$wp_round, data=epa_wp_run)

intersect <- function(x) {predict(f1, x)-predict(f2,x)}
intersect_val <- uniroot.all(intersect, lower=9, upper=15) 

intersect_val

#Annotate the plot to illustrate conclusion
plot_epa_wp_ann <- plot_epa_wp + 
  geom_vline(aes(xintercept=intersect_val)) +
  annotate(geom="label", x=12.5, y=0.21, label="WP = 13%", 
           fontface="bold", size=3) +
  geom_segment(aes(x=12.5, y=0.18, xend=-4, yend=0.18, color="run"), 
               arrow = arrow(length = unit(0.5, "cm")),
               size=1.5, show.legend = FALSE) +
  geom_segment(aes(x=13.7, y=0.18, xend=31, yend=0.18, color="pass"), 
               arrow = arrow(length = unit(0.5, "cm")),
               size=1.5, show.legend = FALSE) +
  geom_label(aes(x=5, y=0.12, label=("Run\n Higher\n EPA"), fontface="bold", color="run"), size=3, show.legend = FALSE) + 
  geom_label(aes(x=21.5, y=0.12, label=("Pass\n Higher\n EPA"), fontface="bold", color="pass"), size=3, show.legend = FALSE)

plot_epa_wp_ann

#======================================================================

#V. Relationship Between WP & Passing Plays

#Determine total number of plays at each WP
epa_wp_total <- pbp19_rp %>%
  filter(!is_na(run_pass)) %>%
  group_by(wp_round) %>%
  summarize(total_plays=n())

#Add the total number of plays at each WP as a separate column
epa_wp_pct <- merge(epa_wp, epa_wp_total, by="wp_round")

#Add a new column for percentage of each play type at each WP
epa_wp_pct <- epa_wp_pct %>%
  mutate(pct_plays = (epa_wp_pct$plays/epa_wp_pct$total_plays)*100)

#Filter out the pass plays
epa_wp_pct_pass <- epa_wp_pct %>%
  filter(run_pass=="pass")

#Plot the percentage of pass plays vs. WP
plot_pass_pct_wp <- ggplot(epa_wp_pct_pass, aes(x=wp_round, y=pct_plays, 
                                                lty = "Pass %")) +
  geom_point(color="blue") +
  labs(x="Win Probability (WP)",y="\nPass Play Percentage\n ", 
       caption = "Data from nflscrapR") +
  geom_smooth(method="loess", color="blue") + 
  theme(legend.title=element_blank())

plot_pass_pct_wp

#Model pass percentage as function of WP
f3 <- loess(epa_wp_pct_pass$pct_plays ~ epa_wp_pct_pass$wp_round, 
            data=epa_wp_pct_pass)

opt_pr <- predict(f3, intersect_val)

opt_pr

#Annotate the plot to illustrate conclusion
plot_pass_pct_wp_ann <- plot_pass_pct_wp + 
  geom_segment(x=intersect_val, y=0, xend=intersect_val, yend=opt_pr) +
  geom_segment(x=intersect_val, y=opt_pr, xend=-5, yend=opt_pr) + 
  geom_segment(x=20, y=85, xend=-5, yend=opt_pr, 
               arrow = arrow(length = unit(0.5, "cm")), size=0.75, 
               show.legend = FALSE) +
  geom_label(x=12.5, y=35, label="WP = 13%", fontface="bold",
             size=3, show.legend = FALSE) +
  geom_label(x=22, y=85, label="Optimum Pass%\n= 73%", fontface="bold",
             size=3, show.legend = FALSE)

plot_pass_pct_wp_ann

#Compare (1) pass plays % vs. WP to (2) EPAs of run & pass plays vs. EPA
grid.arrange(plot_pass_pct_wp_ann + 
               labs(caption="") + 
               theme(legend.position=c(.95,.6), legend.justification='right'), 
             plot_epa_wp_ann + 
               labs(caption="") + 
               theme(legend.position=c(.95, .475),
                     legend.justification='right'),
             nrow=2, 
             bottom=textGrob("Data from nflscrapR", gp=gpar(fontsize=9)))

#======================================================================

#VI. Years 2015-2018

#Download the data for the past 5 seasons into a data frame

first <- 2015 #first season to grab
last <- 2019 #last season to grab

pbpold_list = list()
for (yr in first:last) {
  pbp <- read_csv(url(paste0("http://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_", yr, ".csv")))
  pbp <- pbp %>% select(posteam, down, play_type, qb_scramble, run_location, epa, wp) %>% mutate(season_yr=yr)
  pbpold_list[[yr]] <- pbp #add it to the list
}

pbpold <- dplyr::bind_rows(pbpold_list)

#Create new column with rounded WPs to nearest 10
rnd_base <- 10

pbpold <- pbpold %>%
  mutate(wp_round = rnd_base*round((wp*100)/rnd_base))

#Redefine QB scrambles to be pass plays & filter out 2-pt conversions (down=NA), & NAs for EPAs & WPs
pbpold <- pbpold %>%
  filter(play_type=="pass" | play_type=="run", down<=4, 
         !is_na(epa), !is_na(wp)) %>%
  mutate(run_pass = ifelse(play_type=="pass" | qb_scramble==1, "pass", "run"))

#Filter out run location NAs on running play types (ex., fumbled snaps)
pbpold <- pbpold %>%
  filter(run_pass=="pass" | play_type=="run" & !is_na(run_location))

#Determine actual pass play percentages for leagues from 2015-2018
pbp_rp_list = list()
for (yr in first:last) {
  
  pbp19_rp_sum_old <- pbpold %>%
    filter(season_yr==yr) %>%
    group_by(run_pass) %>%
    summarize(play_count=n())
  
  pbp_rp_list[[yr]] <- pbp19_rp_sum_old$play_count/sum(pbp19_rp_sum_old$play_count)
}

pbp_rp_list[[2015]] 
pbp_rp_list[[2016]]
pbp_rp_list[[2017]]
pbp_rp_list[[2018]]
pbp_rp_list[[2019]]

#Plots of WP vs. Pass Play Percentage
epa_wp_list = list()
epa_wp_pct_pass_list = list()

for (yr in first:last) {
  
  #Determine EPA at each rounded WP values for all plays for the last 4 seasons
  epa_wp_old <- pbpold %>%
    filter(season_yr==yr) %>%
    group_by(wp_round, run_pass) %>%
    summarize(mean_epa = mean(epa), plays=n())
  epa_wp_list[[yr]] <- epa_wp_old
  
  #Determine total number of plays at each WP
  epa_wp_total_old <- pbpold %>%
    filter(!is_na(epa), season_yr==yr) %>%
    group_by(wp_round) %>%
    summarize(total_plays=n())
  
  #Add the total number of plays at each WP as a separate column
  epa_wp_pct_old <- merge(epa_wp_list[[yr]], epa_wp_total_old, by="wp_round")
  
  #Add a new column for percentage of each play type at each WP
  epa_wp_pct_old <- epa_wp_pct_old %>%
    mutate(pct_plays = (epa_wp_pct_old$plays/epa_wp_pct_old$total_plays)*100)
  
  #Filter out the pass plays
  epa_wp_pct_pass_old <- epa_wp_pct_old %>%
    filter(run_pass=="pass")
  
  #Add pass play % vs. WP to list
  epa_wp_pct_pass_list[[yr]] <- epa_wp_pct_pass_old
}

#Plot each of the 5 seasons on the same graph
epa_wp_pct_pass_compare <- epa_wp_pct_pass_list[[2015]] %>% 
  mutate(Season="2015") %>%
  bind_rows(epa_wp_pct_pass_list[[2016]] %>% mutate(Season="2016")) %>%
  bind_rows(epa_wp_pct_pass_list[[2017]] %>% mutate(Season="2017")) %>%
  bind_rows(epa_wp_pct_pass_list[[2018]] %>% mutate(Season="2018")) %>%
  bind_rows(epa_wp_pct_pass_list[[2019]] %>% mutate(Season="2019"))

ggplot(epa_wp_pct_pass_compare, aes(y=pct_plays, x=wp_round, color=Season)) +
  geom_smooth(method="loess", se=FALSE) + 
  labs(x="Win Probability (WP)",y="Pass Play\nPercentage", 
       caption="Data from nflscrapR")

#Plot the EPAs at each rounded WP value for pass plays & run plays
plot_epa_wp_list = list()
for (yr in first:last) {
  plot_epa_wp <- ggplot(epa_wp_list[[yr]], aes(x=wp_round, y=mean_epa, color=run_pass)) +
    geom_point() + 
    geom_smooth(method="loess", aes(fill=run_pass)) +
    labs(x="Win Probability (WP)", y="Expected Points Added\n(EPA)", 
         title = paste0(yr," Season")) +
    scale_color_discrete(labels=c("Pass EPA", "Run EPA")) +
    scale_fill_discrete(labels=c("Pass EPA", "Run EPA")) +
    theme(legend.title=element_blank())
  plot_epa_wp_list[[yr]] <- plot_epa_wp 
}

#Compare the 4 seasons
grid.arrange(plot_epa_wp_list[[2015]], plot_epa_wp_list[[2016]],
             plot_epa_wp_list[[2017]], plot_epa_wp_list[[2018]],
             nrow=2, bottom=textGrob("Data from nflscrapR", 
                                     gp=gpar(fontsize=9)))

#Determine WP intersect points
intersect_val_list = list()
for (yr in first:last) {
  epa_wp_pass_old <- pbpold %>%
    filter(season_yr==yr, run_pass=="pass") %>%
    group_by(wp_round) %>%
    summarize(mean_epa = mean(epa))
  
  epa_wp_run_old <- pbpold %>%
    filter(!is_na(epa), season_yr==yr, run_pass=="run") %>%
    group_by(wp_round) %>%
    summarize(mean_epa = mean(epa))
  
  f1_old <- loess(epa_wp_pass_old$mean_epa ~ epa_wp_pass_old$wp_round, data=epa_wp_pass_old)
  f2_old <- loess(epa_wp_run_old$mean_epa ~ epa_wp_run_old$wp_round, data=epa_wp_run_old)
  
  intersect_old <- function(x) {predict(f1_old, x)-predict(f2_old,x)}
  intersect_val_old <- uniroot.all(intersect_old, lower=0, upper=15)
  
  intersect_val_list[[yr]] <- intersect_val_old 
}

intersect_val_list[[2015]]
intersect_val_list[[2016]]
intersect_val_list[[2017]]
intersect_val_list[[2018]]
intersect_val_list[[2019]]
```

```{r include=FALSE}
#Model pass percentage as function of WP & determine optimum pass pct
opt_pr_list = list()
for (yr in first:last) {
  f3 <- loess(epa_wp_pct_pass_list[[yr]]$pct_plays ~ epa_wp_pct_pass_list[[yr]]$wp_round, 
              data=epa_wp_pct_pass_list[[yr]])
  
  opt_pr <- predict(f3, intersect_val_list[[yr]])
  
  opt_pr_list[[yr]] <- opt_pr
}

opt_pr_list[[2015]]
opt_pr_list[[2016]]
opt_pr_list[[2017]]
opt_pr_list[[2018]]
opt_pr_list[[2019]]