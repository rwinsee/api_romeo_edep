readRenviron("~/work/api_romeo_edep/.Renviron")

Sys.getenv("FT_API_ENDPOINT")
Sys.getenv("AUTH_API_ENDPOINT")
Sys.getenv("CLIENT_SECRET")
Sys.getenv("CLIENT_ID")

library(httr)
library(jsonlite)
