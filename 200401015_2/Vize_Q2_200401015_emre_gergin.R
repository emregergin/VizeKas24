spotify_search_artist <- function(artist_name) {
  # Spotify Token al
  access_token <- spotify_token()$token
  access_token <- gsub("^Bearer\\s+", "", access_token)
  
  # API endpoint
  endpoint <- "https://api.spotify.com/v1/search"
  
  # Arama parametreleri
  params <- list(
    q = artist_name,
    type = "artist"
  )
  
  # API isteğini oluşturma
  response <- httr::GET(
    url = endpoint,
    httr::add_headers(
      Authorization = paste("Bearer ", access_token)
    ),
    query = params
  )
  
  # HTTP status kodunu alma
  status_code <- as.numeric(httr::status_code(response))
  
  if (status_code == 200) {
    # Başarılı bir şekilde sonuç alındıysa
    content <- httr::content(response, as = "parsed")
    
    # Boş olmayan ve anlamsız olmayan sonuçları filtreleme
    valid_results <- sapply(content$artists$items, function(item) !is.null(item$name) & item$name != "")
    
    # Artist ve id'yi çıkarma
    artist_names <- sapply(content$artists$items[valid_results], function(item) item$name)
    artist_ids <- sapply(content$artists$items[valid_results], function(item) item$id)
    
    # Çıktıyı oluşturma
    output <- list(
      status_code = status_code,
      search_results = data.frame(artist = artist_names, id = artist_ids, stringsAsFactors = FALSE)
    )
  } else {
    # Hata durumunda
    output <- list(
      status_code = status_code,
      search_results = NULL
    )
  }
  
  return(output)
}