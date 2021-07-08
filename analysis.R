# 1. loading, setting up

library(tidyverse)
library(googlesheets4)
library(patchwork)
library(tidygraph)
library(ggraph)
library(igraph)
library(janitor)
library(ggplot2)
library(sf)
library(maps)

user_codes <- read_sheet("https://docs.google.com/spreadsheets/d/1fixNNd7RtTfqvaERHbJX_Ex40w_C-m21D_smXuSsIvs/edit#gid=930818301")

user_codes <- clean_names(user_codes)

codebook <- readxl::read_excel("user-roles-codebook.xlsx") %>% 
  janitor::clean_names()

# plot geo

users_to_geocode <- user_codes %>% 
  group_by(screen_name) %>% 
  fill(user_role, .direction = "downup") %>% 
  select(screen_name, user_role, location) %>%
  ungroup() %>% 
  count(screen_name, user_role, location) %>% 
  arrange(desc(n)) %>% 
  filter(n > 1)

users_to_geocode

# locs <- tidytags::geocode_tags(users_to_geocode)
# locs <- locs %>% filter(!is.na(latitude))
# write_rds(locs, "locs.rds")

locs <- read_rds("locs.rds")

locations <- sf::st_as_sf(
  locs,
  coords = c(x = "longitude", y = "latitude"),
  crs = 4326)

locations <- locations %>% 
  left_join(rename(codebook, user_role = code)) %>% 
  filter(!is.na(role))

m <- mapview::mapview(locations, 
                 zcol = "role",
                 cex = "n",
                 alpha = .3)

usa = st_as_sf(map('world', plot = FALSE, fill = TRUE))

sf::point
# usa <- st_transform(usa, crs = 4326)

ggplot() + 
  geom_sf(data = usa) +
  geom_sf(data = locations, aes(color = role)) +
  theme_void() +
  theme(legend.position = "bottom")
 
ggsave("eloned-map.png", width = 10, height = 10)

# 2. prep

user_codes <- user_codes %>% 
  mutate(status_id = str_sub(status_id, start = 2),
         reply_to_status_id = str_sub(reply_to_status_id, start = 2))

dd <- tidytags::process_tweets(user_codes)

regex <- "@([A-Za-z]+[A-Za-z0-9_]+)(?![A-Za-z0-9_]*\\.)"

distinct_roles <- user_codes %>% distinct(screen_name, created_at, user_role, x2nd_role)

source("edge-funcs.R")

edge <- create_edgelist(dd)

receiver_edges_to_add_for_timestamps <- edge %>% 
  distinct(receiver, date_time) %>% 
  rename(screen_name = receiver)

all_roles <- distinct_roles %>% 
  rename(date_time = created_at) %>% 
  bind_rows(receiver_edges_to_add_for_timestamps) %>% 
  arrange(screen_name, date_time) %>% 
  group_by(screen_name) %>% 
  fill(user_role, .direction = "downup") %>% 
  ungroup()

distinct_roles_sender <- select(all_roles, sender = screen_name, date_time, sender_user_role = user_role)
distinct_roles_receiver <- select(all_roles, receiver = screen_name, date_time, receiver_user_role = user_role)

edge

distinct_roles_sender <- distinct_roles_sender %>% 
  filter(!is.na(sender_user_role))

distinct_roles_receiver <- distinct_roles_receiver %>% 
  filter(!is.na(receiver_user_role))

distinct_roles_sender
distinct_roles_receiver

codebook_sender <- select(codebook,
                          sender_role = role,
                          sender_user_role = code)

codebook_receiver <- select(codebook, 
                            receiver_role = role,
                            receiver_user_role = code)

to_model <- edge %>% 
  left_join(distinct_roles_sender) %>% 
  left_join(distinct_roles_receiver) %>% 
  left_join(codebook_sender) %>% 
  left_join(codebook_receiver) %>% 
  select(sender, receiver, edge_type, sender_role, receiver_role)

# 3. analysis

user_codes %>% 
  distinct(screen_name, user_role) %>% 
  left_join(rename(codebook, user_role = code)) %>% 
  select(-role_description) %>% 
  count(role, sort = TRUE) %>% 
  mutate(prop = n / sum(n)) %>% clipr::write_clip()

to_model

to_model %>% 
  count(sender_role, sort = TRUE)

to_model %>% 
  count(sender_role, sort = TRUE) %>% 
  clipr::write_clip()

to_model %>% 
  count(receiver_role, sender_role, sort = TRUE) %>% 
  filter(!is.na(receiver_role) | !is.na(sender_role)) %>% 
  clipr::write_clip()

g <- igraph::graph_from_data_frame(to_model)

# mean out-degree centrality
p1 <- count(to_model, sender, sender_role) %>% 
  group_by(sender_role) %>% 
  summarize(sender_role_m = mean(n),
            sender_role_sd = sd(n)) %>% 
  arrange(desc(sender_role_m)) %>% 
  ggplot(aes(x = reorder(sender_role, sender_role_m), y = sender_role_m)) +
  geom_col() +
  coord_flip() + 
  xlab(NULL)

# mean in-degree centrality
p2 <- count(to_model, receiver, receiver_role) %>% 
  group_by(receiver_role) %>% 
  summarize(receiver_role_m = mean(n),
            receiver_role_sd = sd(n)) %>% 
  arrange(desc(receiver_role_m)) %>% 
  ggplot(aes(x = reorder(receiver_role, receiver_role_m), y = receiver_role_m)) +
  geom_col() +
  coord_flip() +
  xlab(NULL)

p1 + p2 + patchwork::plot_layout(ncol = 1)

ggsave("centrality-fig.png", width = 7, height = 10)

# Create graph

senders <- to_model %>% 
  distinct(sender, sender_role)

receivers <- to_model %>%
  distinct(receiver, receiver_role) %>% 
  rename(sender = receiver, sender_role = receiver_role)

all_users <- bind_rows(senders, receivers) %>% 
  distinct() %>% 
  rename(user = sender, user_role = sender_role)

# by types

graph <- tbl_graph(nodes = all_users,
                   edges = edge)

graph <- graph %>%
  mutate(Centrality = centrality_degree(mode = 'all')) %>% 
  filter(Centrality > 75) %>% 
  filter(!is.na(user_role)) %>% 
  activate(edges)

graph

# plot using ggraph
ggraph(graph, layout = 'kk') + 
  geom_edge_link(alpha = .1) +
  geom_node_point(aes(size = Centrality, color = user_role)) + 
  theme_graph() +
  facet_edges(~edge_type)

ggsave("eloned-sociogram-types.png", width = 14,
       height = 10)

# by year

graph <- tbl_graph(nodes = all_users,
                   edges = edge)

graph <- graph %>%
  mutate(Centrality = centrality_degree(mode = 'all')) %>% 
  filter(Centrality > 100) %>% 
  filter(!is.na(user_role)) %>% 
  activate(edges) %>% 
  mutate(year = lubridate::year(date_time))

graph

# plot using ggraph
ggraph(graph, layout = 'kk') + 
  geom_edge_link(alpha = .1) +
  geom_node_point(aes(size = Centrality, color = user_role)) + 
  theme_graph() +
  facet_edges(~year)

ggsave("eloned-sociogram-years.png", width = 15.5,
       height = 10)
