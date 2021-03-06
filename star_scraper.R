#base script to scrape stars


badge_stars <- function(color = "green", url = NULL, cookie = Sys.getenv("ADVENT_COOKIE")) {
  
  req <- httr::GET(glue::glue('https://adventofcode.com/'),
                   httr::add_headers(.headers = c(
                     "user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.198 Safari/537.36",
                     "cookie" = paste0("session=", cookie)
                   )))
  
  
  #parsed <- httr::content(req, as = "text")
  
  parsed <- xml2::read_html(req)
  
  stars <- rvest::html_node(parsed, ".user")
  
  star_text <- rvest::html_text(stars)
  
  
  
  badge <- paste0("![](https://img.shields.io/badge/", "star count", "-", star_text, "-", color, ".svg)")
  
  if (is.null(url))
    return(badge)
  
  paste0("[", badge, "](", url, ")")
}


