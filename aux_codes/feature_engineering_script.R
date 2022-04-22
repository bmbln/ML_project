feature_engineering_df <- function( df) {
  
  #Group the variables by type
  non_pred <- c("timedelta" , "article_id")
  
  dummy_vars <- c(grep( "data_channel_is_" , names(df) , value = T ),
                  grep( "weekday_is_" , names(df) , value = T ) ,
                  "is_weekend")
  
  num_vars <- setdiff( names(df) , 
                       c( non_pred , y_var , dummy_vars )  )
  
  #numeric variables between 0 and abs(1)
  num_vars_01 <- c(grep( "LDA_" , names(df) , value = T ),
                   grep( "global_" , names(df) , value = T ),
                   grep( "rate_" , names(df) , value = T ) ,
                   grep( "_polarity" , names(df) , value = T ) , 
                   grep( "title_" , names(df) , value = T ) ,
                   grep( "abs_" , names(df) , value = T ) )
  
  #other type of numeric variables -> potential for feature engineering, later step 
  num_vars_other <- setdiff( num_vars , num_vars_01 )
  
  fe_vars <- dummy_vars
  
  title_nums_01 <- unique( grep( "*title_" , num_vars_01 , value = T) )
  num_vars_01_rest <- setdiff( num_vars_01 , title_nums_01 )
  
  df <- df %>% 
    mutate( title_sentiment_polarity = title_sentiment_polarity / 2 + .5 , 
            abs_title_subjectivity = abs_title_subjectivity * 2 )
  
  df <- df %>% 
    mutate( fe_title_sentiment_polarity = case_when( title_sentiment_polarity < .25 ~ "very neg" , 
                                                     title_sentiment_polarity < .45 ~ "more neg" , 
                                                     title_sentiment_polarity <= .55 ~ "neutral" , 
                                                     title_sentiment_polarity <= .75 ~ "more pos" , 
                                                     TRUE ~ "very pos") , 
            fe_abs_title_sentiment_polarity = case_when( abs_title_sentiment_polarity < .1 ~ 'neutral' , 
                                                         abs_title_sentiment_polarity < .5 ~ 'slightly pol' , 
                                                         TRUE ~ 'very pol' ) , 
            fe_title_subjectivity = case_when( title_subjectivity < .1 ~ 'bttm low' , 
                                               title_subjectivity < .45 ~ 'mid low' ,
                                               title_subjectivity < .55 ~ 'middle' ,
                                               title_subjectivity < .9 ~ 'mid high' ,
                                               TRUE ~ 'high' ) , 
            fe_abs_title_subjectivity = case_when( abs_title_subjectivity < .25 ~ 'low' ,
                                                   abs_title_subjectivity < .75 ~ 'middle' , 
                                                   TRUE ~ 'high' ) )
  
  fe_vars <- c(fe_vars , 'fe_title_sentiment_polarity' , 'fe_abs_title_sentiment_polarity' , 
               'fe_title_subjectivity' , 'fe_abs_title_subjectivity' )
  
  LDA_nums_01 <- unique( grep( "LDA_" , num_vars_01_rest , value = T) )
  num_vars_01_rest <- setdiff( num_vars_01_rest , LDA_nums_01 )
  
  df <- df %>% 
    mutate( fe_LDA_00 = case_when( LDA_00 < .05 ~ "very close" , 
                                   LDA_00 < .25 ~ "close" , 
                                   LDA_00 < .5 ~ "far", 
                                   TRUE ~ 'very far' ) , 
            fe_LDA_01 = case_when( LDA_01 < .05 ~ "very close" , 
                                   LDA_01 < .25 ~ "close" , 
                                   LDA_01 < .5 ~ "far", 
                                   TRUE ~ 'very far' ) ,
            fe_LDA_02 = case_when( LDA_02 < .05 ~ "very close" , 
                                   LDA_02 < .25 ~ "close" , 
                                   LDA_02 < .5 ~ "far", 
                                   TRUE ~ 'very far' ) ,
            fe_LDA_03 = case_when( LDA_03 < .05 ~ "very close" , 
                                   LDA_03 < .25 ~ "close" , 
                                   LDA_03 < .5 ~ "far", 
                                   TRUE ~ 'very far' ) ,
            fe_LDA_04 = case_when( LDA_04 < .05 ~ "very close" , 
                                   LDA_04 < .25 ~ "close" , 
                                   LDA_04 < .5 ~ "far", 
                                   TRUE ~ 'very far' ) )
  
  fe_vars <- c( fe_vars , 'fe_LDA_00' , 'fe_LDA_01' , 
                'fe_LDA_02' , 'fe_LDA_03' , 'fe_LDA_04' )
  
  df <- df %>% 
    mutate( global_sentiment_polarity = global_sentiment_polarity / 2 + .5 , 
            avg_negative_polarity = abs(avg_negative_polarity) ,
            min_negative_polarity = abs(min_negative_polarity) ,
            max_negative_polarity = abs(max_negative_polarity))
  
  fe_vars <- c(fe_vars , num_vars_01_rest )
  
  n_num_vars_other <- unique( grep( "^n_" , num_vars_other , value = T) )
  num_vars_other_rest <- setdiff( num_vars_other , n_num_vars_other )
  
  df <- df %>% 
    mutate( n_non_stop_unique_tokens = ifelse( n_non_stop_unique_tokens > 1 , NA , n_non_stop_unique_tokens  ) ,
            n_unique_tokens = ifelse(n_unique_tokens > 1 , NA , n_unique_tokens  ) ,
            n_non_stop_words = ifelse( n_non_stop_words > 1 , 1 , n_non_stop_words  ) , 
            fe_n_tokens_content = log( n_tokens_content + 1 ) , 
            f_n_tokens_content = ifelse( n_tokens_content == 0 , 1 , 0 ) ) %>% 
    mutate( n_non_stop_unique_tokens = ifelse( is.na(n_non_stop_unique_tokens) , 
                                               mean( n_non_stop_unique_tokens , na.rm = T ) , 
                                               n_non_stop_unique_tokens  ) , 
            n_unique_tokens = ifelse( is.na( n_unique_tokens ) , 
                                      mean( n_unique_tokens , na.rm = T ) , 
                                      n_unique_tokens ) )
  
  fe_vars <- c(fe_vars , 'n_tokens_title' , "fe_n_tokens_content" , 'n_non_stop_unique_tokens' , 'n_non_stop_words' , 'n_unique_tokens' )
  flag_vars <- c("f_n_tokens_content")
  
  kw_num_vars_other <- unique( grep( "^kw_" , num_vars_other , value = T) )
  num_vars_other_rest <- setdiff( num_vars_other_rest , kw_num_vars_other )
  
  df <- df %>% 
    mutate( kw_min_avg = ifelse( kw_min_avg < 0 , 0 , kw_min_avg ) , 
            kw_avg_min = ifelse( kw_avg_min < 0 , 0 , kw_avg_min ) ,
            kw_min_min = ifelse( kw_min_min < 0 , 0 , kw_min_min ) )
  
  df <- df %>% 
    mutate( fe_kw_min_min = ifelse( kw_min_min < 5 , 0 , 1 ) , 
            f_kw_min_avg = ifelse( kw_min_avg == 0 , 1 , 0 ) , 
            fe_kw_avg_min = log( kw_avg_min + 1 ) , 
            f_kw_avg_min = ifelse( kw_avg_min == 0 , 1 , 0 ) , 
            fe_kw_max_min = log( kw_max_min + 1 ) , 
            f_kw_max_min = ifelse( kw_max_min == 0 , 1 , 0 ),
            fe_kw_min_max = log( kw_min_max + 1 ) , 
            f_kw_min_max = ifelse( kw_min_max == 0 , 1 , 0 ),
            fe_kw_max_avg = log( kw_max_avg + 1 ) , 
            f_kw_max_avg = ifelse( kw_max_avg == 0 , 1 , 0 ),
            fe_kw_avg_avg = log( kw_avg_avg + 1 ) , 
            f_kw_avg_avg = ifelse( kw_avg_avg == 0 , 1 , 0 ),
            fe_kw_avg_max = log( kw_avg_max + 1 ) , 
            f_kw_avg_max = ifelse( kw_avg_max < 60000 , 1 , 0 ),
            fe_kw_max_max = ifelse(kw_max_max == max(kw_max_max) , 1 , 0 ) )
  
  fe_vars <- c(fe_vars , 'kw_min_avg' , 'fe_kw_min_min' , 'fe_kw_avg_min' , 'fe_kw_max_min' ,  'fe_kw_min_max' , 'fe_kw_max_avg' , 'fe_kw_avg_avg' , 'fe_kw_avg_max' , 'fe_kw_max_max')
  flag_vars <- c( flag_vars , 'f_kw_min_avg' , 'f_kw_avg_min' , 'f_kw_max_min' , 'f_kw_min_max' ,  
                  'f_kw_max_avg' , 'f_kw_avg_avg' , 'f_kw_avg_max')
  
  self_num_vars_other <- unique( grep( "^self_" , num_vars_other , value = T) )
  num_vars_other_rest <- setdiff( num_vars_other_rest , self_num_vars_other )
  
  df <- df %>% 
    mutate( fe_self_reference_min_shares = log( self_reference_min_shares + 1 ) , 
            f_self_reference_min_shares = ifelse( self_reference_min_shares == 0 , 1 , 0 ) , 
            fe_self_reference_max_shares = log( self_reference_max_shares + 1 ) , 
            f_self_reference_max_shares = ifelse( self_reference_max_shares == 0 , 1 , 0 ),
            fe_self_reference_avg_sharess = log( self_reference_avg_sharess + 1 ) , 
            f_self_reference_avg_sharess = ifelse( self_reference_avg_sharess == 0 , 1 , 0 ) ) 
  
  fe_vars <- c( fe_vars , 'fe_self_reference_min_shares' , 'fe_self_reference_max_shares' , 'fe_self_reference_avg_sharess'  )
  flag_vars <- c( flag_vars , 'f_self_reference_min_shares' , 'f_self_reference_max_shares' , 'f_self_reference_avg_sharess' )
  
  df <- df %>% 
    mutate( fe_average_token_length = ifelse( average_token_length == 0 , 
                                              mean(average_token_length) , 
                                              ifelse( average_token_length > 6 , 
                                                      mean(average_token_length) , 
                                                      average_token_length ) ) , 
            f_average_token_length = ifelse( average_token_length == 0 , 
                                             1 , ifelse( average_token_length > 6 ,  1 , 0 ) ) , 
            fe_num_videos = ifelse( num_videos == 0 , 0 , 1 ) , 
            fe_num_imgs = case_when( num_imgs == 0 ~ "no image" , 
                                     num_imgs == 1 ~ "1 image" , 
                                     num_imgs < 6 ~ "2-5 images", 
                                     TRUE ~ '6 or more images' ) ,
            fe_num_hrefs = log(num_hrefs + 1) , 
            f_num_hrefs = ifelse( num_hrefs == 0 , 1 , 0 ) , 
            fe_num_self_hrefs = log(num_self_hrefs+1) , 
            f_num_self_hrefs = ifelse( num_self_hrefs == 0 , 1 , 0 ) )
  
  fe_vars <- c( fe_vars ,  'num_keywords' , 'fe_num_imgs' , 
                'fe_average_token_length'  , 'fe_num_videos' , 'fe_num_hrefs' , 'fe_num_self_hrefs' )
  flag_vars <- c( flag_vars , 'f_average_token_length' , 'f_num_hrefs' , 'f_num_self_hrefs' )
  
  clean_df <- df %>% 
    select( all_of( c( "article_id" , fe_vars , flag_vars ) ) ) %>% 
    mutate_if( is.character , as.factor )
  
  return(clean_df)
}




