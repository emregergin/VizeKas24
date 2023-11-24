# spotify_search_artist() fonksiyonu önceden tanımlanmıştır.

# En sevdiğim 4 ve en sevmediğim 1 Spotify sanatçısının isimlerini belirleyin.
artist_names <- c("Lena", "Shawn Mendes", "Billie Ellish", "The Weeknd", "Justin Bieber")

# Her sanatçının id'sini bulun ve bir data.frame içinde saklayın.
artist_ids <- sapply(artist_names, function(name) {
  result <- spotify_search_artist(name)
  if (length(result$id) > 0) {
    return(result$id[0])
  } else {
    return(NA)  # Sanatçı bulunamazsa NA döndürün.
  }
})

my_artists <- data.frame(artist=artist_names, id=artist_ids)
