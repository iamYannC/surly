library(glue)
library(rvest)
library(tidyverse)

url_bikes <- "https://www.surlybikes.com/bikes/"
bike_names <- read_html(url_bikes) |> html_elements(".title") |> html_text()

bike_link <- paste0(url_bikes,bike_names) |> str_replace_all(" ","_")

sections <- map_chr(1:4,\(titl) bike_link[8] |> read_html() |> html_element(glue(".spec-grid-block:nth-child({titl})")) |> html_text2() |>
                    str_extract("(.*)\\n") |> str_remove("\n")) 
                  # ok to leave at bike_link[8]. It's just for titles that are constant across all bikes [16]

# Bike categories
bike_categories <- map(bike_link, \(link) link |> read_html() |> html_elements("a+ a") |> html_text2() |> str_remove("Find Your Bike") |> str_squish())
bike_categories <-
  bike_categories |> map(\(x) str_extract(x,"\\w+")) |> unlist() %>%.[!is.na(.)] |>
  bind_cols(bike_names) |> set_names(c("category","bike"))


# Loop over all items in bike-spec tables
items <- vector("list", length(bike_link))

for(i in 1:length(bike_link)){ 
  
  if(is.null( items[[i]] )) {
    
    cat("\nbike: ",bike_names[i],"\n")
    
    tmp.web_page <- bike_link[i] |> read_html()
    for(item in 1:length(sections)){ 
      tmp.elements <- tmp.web_page |> html_elements(glue(".spec-grid-block:nth-child({item}) .spec-row"))
      items[[i]][[item]] <- tmp.elements |> html_text2()
    }
  } 
}
rm(i,item,tmp.web_page,tmp.elements)
# Category => Bike => title => header <=> description
# 4 categories => 16 Bikes => 4 subtitles => info 

items |> list_flatten() |> map_dbl(length) |> sum()
# total of 603 items
headers <- vector("list", length(bike_link))
descriptions <- vector("list", length(bike_link))
for(i in 1:length(bike_names)){
  for(x in 1:4){
    headers[[i]][[x]] <- items[[i]][[x]] |> str_extract("(.*)\n") |> str_remove("\n")
    descriptions[[i]][[x]] <- items[[i]][[x]] |> str_remove("(.*)\n") 
  }
}
rm(i,x)
length(unlist(headers)) == length(unlist(descriptions));length(unlist(headers))

n_elements_each_section <- map(1:length(headers),\(x) map_dbl(headers[[x]],length)) |> unlist()

# use bcol and multiply_bike
bike_colors <- map(bike_link, \(link) map(6:8, \(n) link |> read_html() |> html_elements("#builds-kit .tablist") |> html_elements(glue("#tab{n}")) |> html_text2()),.progress = T)

bcol <- vector("list", length(bike_names)) 

for(i in 1:length(bike_names)){
  
  for(b in 1:3){ # 3 because i manually checked and saw that max is three kinds of bikes

    
    if(!is_empty(bike_colors[[i]][[b]])){
      bcol[[i]][[b]] <- bike_colors[[i]][[b]]
      
    }
  }
  
}

bike_multiply <- bcol |> map_dbl(length) |> str_replace_all("0","1") |> as.numeric() |> bind_cols(bike_names) |> set_names(c("n","bike"))
bike_multiply <- bike_multiply |> mutate(bike_vec = str_c(rep(bike,n),collapse = ","),.by = bike)
rm(i,b,bcol)

bike_n_elements <- bike_multiply |> rowwise() |> mutate(section = str_c(sections,collapse = ",")) |> 
  separate_longer_delim(section,delim=",") |> mutate(elements = n_elements_each_section) |> 
  select(bike,section,elements)

bike_secton_long <- bike_n_elements |> rowwise() |> mutate( reps = str_c(rep(bike,elements),collapse = ",")) |> 
  separate_longer_delim(reps,delim=",") |> filter(elements!=0)

# TABLE ####
surly_bikes <- 
  tibble(
    bike = bike_secton_long$bike,
    section =bike_secton_long$section,
    header = unlist(headers),
    description = unlist(descriptions)
  ) |> left_join(bike_categories) |> 
  mutate(no_punct = str_to_lower(description) |> str_remove_all("[[:punct:]]"),
         description  = ifelse(str_detect(str_to_upper(description),"N/A"),NA,description),
         header = str_to_title(header)) |> 
  distinct(bike,section,header,no_punct,.keep_all = TRUE) |> select(-no_punct) |> 
  relocate(category,.before = bike) 

# Adding scores 
ratings <- map(bike_link,\(b) b |> read_html() |> html_elements(".bubble-chart-wrap") |> html_text2())
ratings_clean <- 
  map(ratings, \(b) b |> str_extract(".*(?=\\sout)") |> 
        str_remove_all("\t")
  )

score <- map(ratings_clean, \(r) str_extract(r,"\\d") |> as.numeric())
cat <-  map(ratings_clean, \(r) str_remove(r,":") |> str_remove("\\d") |> str_squish())

# Since all category names are one or two words that begin with a capital letter, we can use this to extract the category names

cate <- map(cat, \(c) str_extract_all(c,"[A-Z][a-z]+") )
for(i in 1:length(cate)){
  cate[[i]] <- cate[[i]] |> map(\(x) x |> unique() |> str_c(collapse = " ")) |> unlist()  
}
score_category <- unlist(cate)
score <- unlist(score)

scores <- 
  tibble(bike = bike_names, rep_n = map_dbl(cat,length)) |> rowwise() |> 
  mutate(reps = str_c(rep(bike,rep_n),collapse = ",")) |> 
  separate_longer_delim(reps,delim=",") |> select(!c(rep_n,reps)) |> 
  bind_cols(score_category,score) |> rename(score_category = ...2, score = ...3) |> 
  mutate(mean_score = mean(score,na.rm = T),.by = bike)

scores_wide <- scores |> pivot_wider(id_cols = bike,names_from = score_category, values_from = score) |> janitor::clean_names()
scores_wide <- scores_wide |> mutate(mean_score = rowMeans(scores_wide[,2:length(scores_wide)],na.rm=TRUE),.after = 1)
rm(score, score_category, scores,cat,cate)

# MODIFYING TABLE ####
# Storing long and wider version of data, along with original data shape
surly_bikes_w <- surly_bikes |> left_join(scores_wide) 
surly_bikes_l <- surly_bikes_w |> pivot_longer(cols = where(is.numeric),names_to = "score_category",values_to = "score",values_drop_na = T)

price <- bike_link |> map(\(p) p |> read_html() |> html_elements(".dotted-white") |> html_text2())
price_usd <- price |> map(\(p) p |> str_extract("(.*)USD") )

# There is range in $ as well as msrp price.
# I will extract between 1-2 numbers each. If there is a dash `-`, then make it min usd, max usd. if no, and there are two numbers,
# make it as usd and msrp
# If only one number, then make it as usd

p <- price_usd |> map(
  ~ case_when(
    str_detect(.,"-") ~ str_extract_all(., "\\d+") |> map(\(x) as.numeric(x) |> c(min(x),max(x))) |> unlist(),
    str_detect(.,",") ~ str_extract_all(., "\\d+") |> map(\(x) as.numeric(x) |> c(x[1],x[2])) |> unlist(),
    TRUE ~ str_extract_all(., "\\d+") |> map(\(x) as.numeric(x) |> c(x[1],NA)) |> unlist()
  )
)

for(i in 1:length(p)){
  if(as.numeric(p[[i]][1]) < as.numeric(p[[i]][2])){
    p[[i]] <- replace_na(p[[i]],"range")
    
  } else if(as.numeric(p[[i]][1]) > as.numeric(p[[i]][2])){
    p[[i]] <- replace_na(p[[i]],"msrp")
  } else {
    p[[i]] <- replace_na(p[[i]],"usd")
  }
}

# Shit does not look like R code!! Get an error but it runs smoothly.
p <- p |> map( ~ if(length(.) > 3)  unique(.) else . )

df_price <- 
  bind_cols(
    bike = bike_names,
    msrp = NA,
    min_price = NA,
    max_price = NA,
    price_type = NA
    
  )

for(i in 1:length(p)){
  if(p[[i]][3]  %in%  c('usd','range') ){
    df_price[i,3] <- p[[i]][1]
    df_price[i,4] <- p[[i]][2]
    if(p[[i]][3]=='usd') df_price[i,5] <- 'fixed' else df_price[i,5] <- 'range'
  } else{
    
    df_price[i,2] <- p[[i]][1]
    df_price[i,3] <- p[[i]][2] 
    df_price[i,4] <- p[[i]][2] # 2nd is constant, no range.
    df_price[i,5] <- p[[i]][3] 
    
  }  
  
}
df_price <- df_price |> mutate(across(msrp:max_price,as.numeric))
rm(p,i,price,price_usd)
rm(bike_categories,bike_colors,bike_multiply,bike_n_elements,headers,descriptions,items,sections,scores_wide) # cleanup

# MODIFYING TABLE 2 ####
surly_bikes_w <- surly_bikes_w |> left_join(df_price) |> relocate(msrp,contains("price"),.after = description) |> mutate( max_price = ifelse(price_type == 'range',max_price,NA) )
surly_bikes_l <- surly_bikes_l |> left_join(df_price) |> pivot_longer(cols = msrp:max_price,names_to = "price_i",values_to = "price_usd",values_drop_na = TRUE)

surly_bikes_l <- 
  surly_bikes_l |> 
  mutate(
    price_i = case_when(
      price_type == 'msrp' & price_i != 'msrp' ~ 'price',
      price_type == 'fixed' ~ 'price',
      .default = price_i
    )
  ) |> distinct() 

# Store Long & Wider version of only prices AND only scores

surly_prices_l <- surly_bikes_l |> select(category,bike,contains("price")) |> distinct() 
surly_scores_l <- surly_bikes_l |> select(category,bike,contains("score")) |> distinct() 

surly_prices_w <- surly_bikes_w |> select(category,bike,msrp:price_type) |> distinct()
surly_scores_w <- surly_bikes_w |> select(category,bike,mean_score:hauling) |> distinct() 

# STORE ALL TABLES ####
my_surly_tables <- 
  tibble(
  x = c('general','long','wide'),
  general = list(surly_bikes,surly_bikes_l,surly_bikes_w),
  scores = list(tibble(),surly_scores_l,surly_scores_w),
  price = list(tibble(),surly_prices_l,surly_prices_w)
)
my_surly_tables |> write_rds("surly_tables.rds");paste('\nSaved surly tables @',now()) |> cat()

# Get reviews

review_raw <-map_chr(bike_names |> str_replace_all(" ","_") |> str_to_lower(),\(b) glue("https://display.powerreviews.com/m/981592/l/en_US/product/sur_{b}/reviews?apikey=38eac5e7-99d8-4209-b388-29eaa23714b0&paging.size=25&paging.from=0&_noconfig=true") )

# I have all reviews links, and I want to store the json files for each review
review_list <- map(review_raw, \(x) httr::GET(x) |> httr::content(as = "parsed"))
review_json <- map(review_raw, \(x) httr::GET(x) |> httr::content(as = "parsed") |> jsonlite::toJSON(pretty = T))

reviews_meta <- tibble(bike = bike_names,
                       n_pages = map_dbl(1:length(review_list), \(i) review_list[[i]][['paging']]$pages_total),
                       n_reviews = map_dbl(1:length(review_list), \(i) review_list[[i]][['paging']]$total_results)
) |>
  # maximum page size is 25, so if there are more than 25 n_reviews, get a start_at = 24+1
  mutate(start_at_2nd = ifelse(n_reviews > 25,24+1,NA)) # 0 indexed


# due to design, I'll recreate review_raw, as well as the derrived review_list, only this time, the &paging.from would be 25, since zero indexed 0 to 24, then 25 to end


# Get review list for page 2:
bike_names_only_2pages <- bike_names |> str_replace_all(" ","_") |> str_to_lower() %>% .[bike_names %in% (reviews_meta |> drop_na(start_at_2nd) |> pull(bike))]

review_raw2 <-map_chr(bike_names_only_2pages, \(b2) glue("https://display.powerreviews.com/m/981592/l/en_US/product/sur_{b2}/reviews?apikey=38eac5e7-99d8-4209-b388-29eaa23714b0&paging.size=25&paging.from=25&_noconfig=true") )

# reviews for sec page
review_list2 <- map(review_raw2, \(x2) httr::GET(x2) |> httr::content(as = "parsed"))
review_json2 <- map(review_raw2, \(x2) httr::GET(x2) |> httr::content(as = "parsed") |> jsonlite::toJSON(pretty = T))

# create a function for a specific bike, get all the comment data, store as a list and then do unnest if ncesesery

bike_comment <- function(bike){
  # bike is index. 1:16
  
  tmp.length <- review_list[[bike]][['results']] [[1]][["reviews"]] |> length()
  if(tmp.length == 0) return(NULL)
  
  # bike level, no need to map over reviews
  scores_dist_1..5 <- review_list[[bike]][['results']][[1]][[2]]['review_histogram'] |> unlist() |> str_c(collapse = ",")
  avg_rating <- review_list[[bike]][['results']][[1]][[2]]$average_rating
  recommended_ratio <- review_list[[bike]][['results']][[1]][[2]]$recommended_ratio
  
  name <- map_chr(1:tmp.length,\(l) review_list[[bike]][['results']][[1]][['reviews']][[l]][['details']]$nickname %>% if(is.null(.)) NA else .)
  headline <- map_chr(1:tmp.length,\(l) review_list[[bike]][['results']][[1]][['reviews']][[l]][['details']]$headline %>% if(is.null(.)) NA else .)
  content <- map_chr(1:tmp.length,\(l) review_list[[bike]][['results']][[1]][['reviews']][[l]][['details']]$comments %>% if(is.null(.)) NA else .)
  bottom_line <- map_chr(1:tmp.length,\(l) review_list[[bike]][['results']][[1]][['reviews']][[l]][['details']]$bottom_line %>% if(is.null(.)) NA else .)
  rating <- map_dbl(1:tmp.length,\(l)  review_list[[bike]][['results']] [[1]][["reviews"]][[l]][["metrics"]]$rating %>% if(is.null(.)) NA else .)
  helpful <- map_dbl(1:tmp.length,\(l) review_list[[bike]][['results']] [[1]][["reviews"]][[l]][["metrics"]]$helpful_votes %>% if(is.null(.)) NA else .)
  not_helpful <- map_dbl(1:tmp.length,\(l) review_list[[bike]][['results']] [[1]][["reviews"]][[l]][["metrics"]]$not_helpful_votes %>% if(is.null(.)) NA else .)
  helpful_score <- map_dbl(1:tmp.length,\(l) review_list[[bike]][['results']] [[1]][["reviews"]][[l]][["metrics"]]$helpful_score %>% if(is.null(.)) NA else .)
  location <- map_chr(1:tmp.length,\(l) review_list[[bike]][['results']] [[1]][["reviews"]][[l]][["details"]]$location %>% if(is.null(.)) NA else .)
  full_date <- map_dbl(1:tmp.length,\(l) review_list[[bike]][['results']][[1]][['reviews']][[l]][['details']]$created_date/1000) |> as.POSIXct(origin = "1970-01-01") |> strptime(format = "%Y-%m-%d %H:%M:%S" %>% if(is.null(.)) NA else .)
  hour <- map_chr(full_date,\(d) d |> str_split_i(" ",i=2) |> str_extract(".{5}") %>% if(is.null(.)) NA else .)
  date <- full_date |> as.Date() 
  
  comment_df <- tibble(
    bike = bike_names[bike],
    scores_dist_1..5 = scores_dist_1..5,
    avg_rating = avg_rating,
    recommended_ratio = recommended_ratio,
    name = name,
    headline = headline,
    content = content,
    bottom_line = bottom_line,
    rating = rating,
    helpful = helpful,
    not_helpful = not_helpful,
    helpful_score = helpful_score,
    location = location,
    full_date = full_date,
    hour = hour, 
    date = date
  ) |> distinct()
  
  return(comment_df)
}

bike_commetns <- map_dfr(1:length(bike_names),bike_comment)

bike_comment2 <- function(bike_2pages){
  # bike_2pages is index. 1:4
  
  tmp.length <- review_list2[[bike_2pages]][['results']] [[1]][["reviews"]] |> length()
  if(tmp.length == 0) return(NULL)
  
  
  name <- map_chr(1:tmp.length,\(l) review_list2[[bike_2pages]][['results']][[1]][['reviews']][[l]][['details']]$nickname %>% if(is.null(.)) NA else .)
  headline <- map_chr(1:tmp.length,\(l) review_list2[[bike_2pages]][['results']][[1]][['reviews']][[l]][['details']]$headline %>% if(is.null(.)) NA else .)
  content <- map_chr(1:tmp.length,\(l) review_list2[[bike_2pages]][['results']][[1]][['reviews']][[l]][['details']]$comments %>% if(is.null(.)) NA else .)
  bottom_line <- map_chr(1:tmp.length,\(l) review_list2[[bike_2pages]][['results']][[1]][['reviews']][[l]][['details']]$bottom_line %>% if(is.null(.)) NA else .)
  rating <- map_dbl(1:tmp.length,\(l)  review_list2[[bike_2pages]][['results']] [[1]][["reviews"]][[l]][["metrics"]]$rating %>% if(is.null(.)) NA else .)
  helpful <- map_dbl(1:tmp.length,\(l) review_list2[[bike_2pages]][['results']] [[1]][["reviews"]][[l]][["metrics"]]$helpful_votes %>% if(is.null(.)) NA else .)
  not_helpful <- map_dbl(1:tmp.length,\(l) review_list2[[bike_2pages]][['results']] [[1]][["reviews"]][[l]][["metrics"]]$not_helpful_votes %>% if(is.null(.)) NA else .)
  helpful_score <- map_dbl(1:tmp.length,\(l) review_list2[[bike_2pages]][['results']] [[1]][["reviews"]][[l]][["metrics"]]$helpful_score %>% if(is.null(.)) NA else .)
  location <- map_chr(1:tmp.length,\(l) review_list2[[bike_2pages]][['results']] [[1]][["reviews"]][[l]][["details"]]$location %>% if(is.null(.)) NA else .)
  full_date <- map_dbl(1:tmp.length,\(l) review_list2[[bike_2pages]][['results']][[1]][['reviews']][[l]][['details']]$created_date/1000) |> as.POSIXct(origin = "1970-01-01") |> strptime(format = "%Y-%m-%d %H:%M:%S" %>% if(is.null(.)) NA else .)
  hour <- map_chr(full_date,\(d) d |> str_split_i(" ",i=2) |> str_extract(".{5}") %>% if(is.null(.)) NA else .)
  date <- full_date |> as.Date() 
  
  comment_df2 <- tibble(
    bike = bike_names[bike_names |> str_replace_all(" ","_") |> str_to_lower() == bike_names_only_2pages[bike_2pages]], # ugly code that takes only ones with 2 pages
    name = name,
    headline = headline,
    content = content,
    bottom_line = bottom_line,
    rating = rating,
    helpful = helpful,
    not_helpful = not_helpful,
    helpful_score = helpful_score,
    location = location,
    full_date = full_date,
    hour = hour, 
    date = date
  ) |> distinct()
  
  return(comment_df2)
}

bike_commetns2 <- map_dfr(1:length(bike_names_only_2pages),bike_comment2)

bike_comments_all <- bind_rows(bike_commetns,bike_commetns2) |> 
  left_join(reviews_meta |> select(bike,n_reviews)) |> relocate(n_reviews,.after = bike)
 
bike_comments_all <- # fix meta comments: distribution of ratings, avg rating, recommended ratio
  bike_comments_all |> 
  mutate(scores_dist_1..5 = paste(unique(scores_dist_1..5),collapse = "") |> str_remove("NA"),
         avg_rating = paste(unique(avg_rating),collapse = "") |> str_remove("NA"),
         recommended_ratio = paste(unique(recommended_ratio),collapse = "") |> str_remove("NA"),
         .by = bike)

bike_comments_all <- bike_comments_all |> mutate(across(avg_rating:recommended_ratio,as.numeric)) |> select(-full_date)

#TABLE 3####
surly_bikes_l <- surly_bikes_l |> left_join(bike_comments_all)
# does not suit for wider type
# meta-bike review
# bike_comments_all |> select(bike:recommended_ratio) |> distinct()
# meta-reviewer


# Implement QA
page_start <- 0

qa_raw <-map_chr(bike_names |> str_replace_all(" ","_") |> str_to_lower(),\(b) glue("https://display.powerreviews.com/m/981592/l/en_US/product/sur_{b}/questions?_noconfig=true&apikey=38eac5e7-99d8-4209-b388-29eaa23714b0&paging.from={page_start}&paging.size=25") )

qa_list <- map(qa_raw, \(x) httr::GET(x) |> httr::content(as = "parsed"))
qa_json <- map(qa_raw, \(x) httr::GET(x) |> httr::content(as = "parsed") |> jsonlite::toJSON(pretty = T))

qa_list[[3]] |> names() # name mapging results

# Start by finding N questions. considering each page has up to 25 questions displayed, then create a loop to go through all pages of questions
total_qa <- map_dbl(1:length(qa_list),\(q) qa_list[[q]][['paging']]$total_results)

# total_pages_qa <-  
qa_to_pages <- function(n_qa){
  
  if(n_qa %% 25 == 0){
    return(n_qa %/% 25)
  } else {
    return(n_qa %/% 25 + 1)
  }
  
}
tibble(bike_names,total_qa, pages = map_dbl(total_qa,qa_to_pages))

# create a function that iterates and returns a tibble. in the function take argument to then make it iterate over different bikes. 
# but the function should just iterate over pages
# it will merely be gathering of the data, and only after analysing.

qa_collector <- function(bike_id){
  cat("\nbike: ",bike_names[bike_id],"\n")
  
  # This function returns a list of all QA for a specific bike.
  # It iterates over all pages (given 25 page size) and extracts the details & answer section from each QA
  # First qa_raw_page can be with page start = 0, hardcoded, but if i'll copy this function i have a feeling it will cause problems
  # After this process, the actual extraction of text should be performed.
  
  page_start_scope <- 0
  bike_name_scope <- bike_names[bike_id] |> str_replace_all(" ","_") |> str_to_lower()
  qa_raw_page <- glue("https://display.powerreviews.com/m/981592/l/en_US/product/sur_{bike_name_scope}/questions?_noconfig=true&apikey=38eac5e7-99d8-4209-b388-29eaa23714b0&paging.from={page_start_scope}&paging.size=25") 
  
  qa_list_page <- httr::GET(qa_raw_page) |> httr::content(as = "parsed")
  
  # populate an empty list with qa_list_page[['paging']]$total_results elements
  
  list_for_qa <- vector("list", length = qa_list_page[['paging']]$total_results)
  
  while(qa_list_page[['paging']]$current_page_number <= qa_list_page[['paging']]$pages_total ){
    
    for(element_ in 1:length(qa_list_page[['results']])){
      
      element <-  element_ + 25*(qa_list_page[['paging']]$current_page_number - 1)
      
      list_for_qa[[element]] <- qa_list_page[['results']][[element_]]
      # this is just the details for each QA. It has both  details [Q] & answer [A] attributes.
    }
    
    page_start_scope <- page_start_scope + 25
    # after updating start_question (as a variable), scrape new questions
    qa_raw_page <- glue("https://display.powerreviews.com/m/981592/l/en_US/product/sur_{bike_name_scope}/questions?_noconfig=true&apikey=38eac5e7-99d8-4209-b388-29eaa23714b0&paging.from={page_start_scope}&paging.size=25") 
    qa_list_page <- httr::GET(qa_raw_page) |> httr::content(as = "parsed")
    
    element <- 1     # make sure it returns to 1
  }
  return(list_for_qa)
}

list_QA <- map(1:length(bike_names),qa_collector)


qa_generator <- function(bike_id){
  
  n_elements <- list_QA[[bike_id]] |> length()
  tmp_df <- tibble(bike = rep(bike_names[bike_id],n_elements),
                   
                   q_nickname = NA,
                   q_content = NA,
                   q_location = NA,
                   q_created_date = NA,
                   
                   a_nickname = NA,
                   a_content = NA,
                   a_helpful = NA,
                   a_not_helpful = NA,
                   a_location = NA,
                   a_created_date = NA,
                   is_verified = NA,
                   is_expert = NA,
                   author_type = NA,
                   q_answer_count = NA,
  )
  
  for(ele_i in 1:n_elements){
    if(list_QA[[bike_id]][[ele_i]]$answer_count > 1){
      cli::cli_inform(glue::glue("In question {ele_i}, there are {list_QA[[bike_id]][[ele_i]]$answer_count} answers."))
    }
    # Q  
    tmp_df[ele_i,'q_nickname'] <- list_QA[[bike_id]][[ele_i]][['details']]$nickname
    tmp_df[ele_i,'q_content'] <-list_QA[[bike_id]][[ele_i]][['details']]$text
    tmp_df[ele_i,'q_location'] <-list_QA[[bike_id]][[ele_i]][['details']]$location
    tmp_df[ele_i,'q_created_date'] <-list_QA[[bike_id]][[ele_i]][['details']]$created_date
    tmp_df[ele_i,'q_answer_count'] <-list_QA[[bike_id]][[ele_i]]$answer_count
    
    # A TODO: modify to map_chr or map_dbl to accomodate multiple answers.
    tmp_df[ele_i,'a_nickname'] <-map_chr(1:list_QA[[bike_id]][[ele_i]]$answer_count,\(a) list_QA[[bike_id]][[ele_i]][['answer']] [[a]] [["details"]]$nickname) |> str_c(collapse = " || ")
    tmp_df[ele_i,'a_content'] <-map_chr(1:list_QA[[bike_id]][[ele_i]]$answer_count,\(a) list_QA[[bike_id]][[ele_i]][['answer']] [[a]] [["details"]]$text) |> str_c(collapse = " || ")
    tmp_df[ele_i,'a_helpful_votes'] <-list_QA[[bike_id]][[ele_i]][['answer']] [[1]] [['metrics']]$helpful_votes
    tmp_df[ele_i,'a_not_helpful_votes'] <-list_QA[[bike_id]][[ele_i]][['answer']] [[1]] [['metrics']]$not_helpful_votes
    tmp_df[ele_i,'a_location'] <-list_QA[[bike_id]][[ele_i]][['answer']][[1]][['details']]$location
    tmp_df[ele_i,'a_created_date'] <-list_QA[[bike_id]][[ele_i]][['answer']][[1]][['details']]$created_date
    
    tmp_df[ele_i,'a_is_verified'] <-list_QA[[bike_id]][[ele_i]][['answer']][[1]][['details']]$is_verified
    tmp_df[ele_i,'a_is_expert'] <-list_QA[[bike_id]][[ele_i]][['answer']][[1]][['details']]$is_expert
    tmp_df[ele_i,'a_author_type'] <-list_QA[[bike_id]][[ele_i]][['answer']][[1]][['details']]$author_type
    
  }
  return(tmp_df |> separate_longer_delim(cols = contains("a_"),delim = " || "))
}

answers <- map_dfr(1:length(bike_names), qa_generator) |> janitor::remove_empty()
answers <- 
  answers |> mutate(across(contains("created"),\(ts) (as.numeric(ts)/1e3) |> as.POSIXct())) |> 
  mutate(q_date = as.Date(q_created_date),
         q_hour = format(q_created_date,format = "%H:%M"),
         a_date = as.Date(a_created_date),
         a_hour = format(a_created_date,format = "%H:%M")
  ) |> select(-contains("created")) |> 
  select(bike,starts_with("q"),starts_with("a"))



# GEOMETRIES
# Getting geometry is a pain in the ass.
# this code should work. TODO: since rows are the same, add them only after conbining or whatever. dont calculkate each time.
# its also very slow code
get_geometry <- function(bike_id){
  cat("\nbike: ",bike_names[bike_id],"\n")
  types <- read_html(bike_link[bike_id]) |> html_elements("#geometry-wrap .tab") |> html_text2()
  cols <- read_html(bike_link[bike_id]) |> html_elements("thead th") |> html_text2()
  cols <- cols[cols!='SIZE']
  if(!'rows' %in% ls()) rows <<- read_html(bike_link[bike_id]) |> html_elements("tbody th") |> html_text2() # rows should only computed once
  cells <- read_html(bike_link[bike_id]) |> html_elements("tbody td") |> html_text2()
  
  if(length(types) == 0) types <- 1
  
  start_idx <- 1
  end_idx <- length(cells)/length(types)
  geoms <- vector("list",length(types))  |> set_names(types)          
  n_cols <- length(cols)/length(types)
  start_colname_idx <- 1
  end_colname_idx <- n_cols
  
  if(any(complete.cases(parse_number(cols)))){ # checking if the col names can be parsed as numbers. otherwise im screwed
    
    ns <- parse_number(cols)
    idx_to_capture <- 1  
    while(ns[idx_to_capture] < ns[idx_to_capture + 1]){ # assuming the new table will start with colnames smaller than largest previous one. again, otherwise im screwed
      idx_to_capture <- idx_to_capture + 1
      if(idx_to_capture == length(ns)) break
    }
    
    end_idx <- idx_to_capture * length(unique(rows)) # setting end index as multipcation of number of rows and number of columns to capture in first table
    n_cols <- idx_to_capture
    end_colname_idx <- idx_to_capture
  }
  
  for(i in 1:length(types)){
    type <- types[i]
    geoms[[types[i]]] <- matrix(cells[start_idx:end_idx],ncol = n_cols,nrow = length(unique(rows)),byrow = T) |>
      as_tibble() |> set_names(unique(cols[start_colname_idx:end_colname_idx])) |> mutate(type = types[i],
                                                                                          bike = bike_names[bike_id])
    start_idx <- end_idx + 1
    end_idx <- end_idx + length(cells)/length(types)
    
    if(any(complete.cases(parse_number(cols)))){
      end_idx <- (length(ns)-idx_to_capture) * length(unique(rows)) + end_idx
      n_cols <- length(ns)-n_cols
      start_colname_idx <- end_colname_idx + 1
      end_colname_idx <- length(cols)
    }
    
  }
  
  size_ <- unique(rows)
  # add rows and reorder columns, as well as clean column names before returning
  add_rows <- function(geoms_){
    if(length(geoms_) > 1){
      return(geoms_ |> map(\(df) df |> mutate(size=size_) |>
                             select(bike,type,size,everything()) |> janitor::clean_names())
      )
      
    } else{
      return(geoms_[[1]] |> mutate(size = size_,type=NA) |>
               select(bike,type,size,everything()) |> janitor::clean_names()
      )
    }
  }
  
  # apply the add_rows function 
  geoms <- add_rows(geoms)
  
  return(geoms)
  # legacy to get mm / in
  # tmp.table[[1]] |> mutate(across(2:9,\(col) str_extract(col,"[0-9.]+.$")))
}

geometries <- vector("list",length(bike_link)) |> set_names(bike_names)
for(i in seq_along(geometries)){
  geometries[[bike_names[i]]] <- get_geometry(i) |> suppressWarnings()
}

geometries