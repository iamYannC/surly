format_geom <- function(geom_df){
  
  geom_df |> mutate(across(-c(bike:size),\(col) str_replace_all(col, pattern = "\\D or ", replacement = "/") |> 
                             str_replace_all(", ", "/") |> 
                             str_replace_all(" or ","/") |> 
                             str_replace_all("t \\d","t\\d")|>
                             str_squish() |>
                             str_replace_all(" - ","-")
                           )
                    )
}


# Pivot geometries to either long or wide format

geometries


make_geom_wide <- function(geom_df,convert_to_numeric=FALSE,show_first=NULL){
  
  # core function:
  make_geom_wide.core <- function(geom_df,convert_to_numeric=FALSE,show_first=NULL){
    
    
    if(!is.data.frame(geom_df)){
      cli::cli_abort(glue::glue("input should be a data frame, not a {class(geom_df)}. try using purrr::map()."))  
    }
    
    
    
    geom_df_fmt <- format_geom(geom_df)
    
    geom_df_wider <- 
      geom_df_fmt |> separate_wider_delim(cols= -c(bike:size), delim = " ",
                                          names = c('inches','milimeters'),
                                          too_few = "align_start",
                                          too_many = 'merge',
                                          names_sep = "_")
    
    if(convert_to_numeric){
      
      geom_df_wider <- geom_df_wider |> mutate(across(-c(bike:size),\(col) str_remove(col,'°') |> as.numeric(col)))
      
    }
    
    if(!is.null(show_first)){
      
      if(show_first=='mm') show_first <- 'milimeters' else if(show_first=='in') show_first <- 'inches'
      
      show_first <- switch(show_first,
                           milimeters  =  'milimeters', 
                           inches = 'inches',
                           cli::cli_abort(glue::glue("Cannot accept '{show_first}'. show_first should be either 'inches' [in] or 'milimeters' [mm]"))
      )
      
      return(geom_df_wider |> select(bike:size,ends_with(show_first),everything()))
    }
    
    
    return(geom_df_wider)
  }
  
  
  if(!is.data.frame(geom_df)){
    geom_df |> map(function(geom_df){
      make_geom_wide.core(geom_df,convert_to_numeric,show_first)
    })
  } else{
    make_geom_wide.core(geom_df,convert_to_numeric,show_first)
    
  }
}



make_geom_long <- function(geom_df,convert_to_numeric=FALSE,arrange_by = 'size'){
  
  # core function:
  make_geom_long.core <- function(geom_df,convert_to_numeric=FALSE,arrange_by = 'size'){
    
    
    geom_df_fmt <- format_geom(geom_df)
    
    # geom_df expects a data frame, that is, the inner product of a list element, [[]].
    
    geom_df_longer <- geom_df_fmt |> separate_longer_delim(cols= -c(bike:size), delim = " ")
    
    # check on the first column if there is a dot its inches, otherwise its mm (or degress which is a special case)
    first_col <- names(geom_df_longer[4])
    
    geom_df_longer <- 
      geom_df_longer |> mutate(n_sizes = consecutive_id(size)) |> 
      mutate(sizes_within = row_number(),.by = n_sizes) |> 
      mutate(is_angle = n_distinct(sizes_within)==1 , .by = n_sizes,
             sizes_within = ifelse(is_angle,3, sizes_within)) |> 
      mutate(measure = case_when(
        sizes_within == 1 ~ "inches",
        sizes_within == 2 ~ "mm",
        sizes_within == 3 ~ "degres[°]"
      )) |> select(-c(n_sizes:is_angle))
    if(convert_to_numeric){
      geom_df_longer <- geom_df_longer |> mutate(across(-c(bike:size,measure),\(col) str_remove(col,'°') |> as.numeric(col)))
    }
    
    # modify that size can be either 'size' or 's' as user input:
    
    if(arrange_by=='s') arrange_by <- 'size' else if(arrange_by=='m') arrange_by <- 'measure'
    
    arrange_by <- switch(arrange_by,
                         size  =  'size', 
                         measure = 'measure',
                         cli::cli_abort(glue::glue("Cannot accept '{arrange_by}'. arrange_by should be either 'size' [s] or 'measure' [m]"))
    )
    return(geom_df_longer |> arrange(!!sym(arrange_by)))
  }
  
  if(!is.data.frame(geom_df)){
    geom_df |> map(function(geom_df){
      make_geom_long.core(geom_df,convert_to_numeric,arrange_by)
    })
  } else{
    make_geom_long.core(geom_df,convert_to_numeric,arrange_by)
    
  }
}


map(geometries,make_geom_wide)
make_geom_long(geometries[[15]]) # fix the issue with indx 15 in geoms, only then move code into prod!