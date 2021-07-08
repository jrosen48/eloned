library(academictwitteR)
library(rtweet)
library(lubridate)

bearer_token <- "AAAAAAAAAAAAAAAAAAAAAJxaMwEAAAAArXLlGGg3UigTUtLrR7VPQ07MSPI%3Dn9m4Iqk8KRwciUyz81iwB1OugTMLRGY0aSsEwB8gXYjYwzBc7Q"

elon <- get_hashtag_tweets("#eloned", "2010-01-01T00:00:00Z", "2021-05-13T00:00:00Z", bearer_token, data_path = NULL)

d <- lookup_statuses(elon$id)
d <- tidytags::process_tweets(d)
dd <- tidytags::process_tweets(d)
dd <- tidytags::get_upstream_replies(dd)

user_codes_ss <- user_codes %>% 
  select(screen_name, created_at, user_role) %>% 
  mutate(created_at = round_date(created_at, "day")) %>% 
  filter(!is.na(user_role))

ddd <- dd %>% 
  mutate(created_at = round_date(created_at, "day")) %>% 
  semi_join(user_codes_ss)

de <- ddd %>% 
  tidytags::create_edgelist()

