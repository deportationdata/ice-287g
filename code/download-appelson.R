library(httr)
library(jsonlite)
library(openssl)

# appelson repo
SRC_OWNER  <- "appelson"
SRC_REPO   <- "Tracking_287g"
SRC_FOLDER <- "sheets"
SRC_BRANCH <- "main"

# 287(g) repo
DEST_OWNER  <- "deportationdata"
DEST_REPO   <- "ice-287g"
DEST_FOLDER <- "code"
DEST_BRANCH <- "main"

TOKEN <- Sys.getenv("GITHUB_PAT")

auth_header <- add_headers(
  Authorization = paste("Bearer", TOKEN),
  Accept        = "application/vnd.github.v3+json"
)

# find most recent folder in appelson repo
src_url  <- paste0("https://api.github.com/repos/", SRC_OWNER, "/", SRC_REPO,
                   "/contents/", SRC_FOLDER, "?ref=", SRC_BRANCH)

response <- GET(src_url, add_headers(Accept = "application/vnd.github.v3+json"))
stop_for_status(response)

contents <- content(response, as = "parsed")
folders  <- Filter(function(x) x$type == "dir", contents)

if (length(folders) == 0) stop("No subfolders found in /sheets")

latest_folder <- folders[[length(folders)]]
message("Most recent folder: ", latest_folder$name)

# list files in that folder
folder_url      <- paste0("https://api.github.com/repos/", SRC_OWNER, "/", SRC_REPO,
                          "/contents/", latest_folder$path, "?ref=", SRC_BRANCH)
folder_response <- GET(folder_url, add_headers(Accept = "application/vnd.github.v3+json"))
stop_for_status(folder_response)

files <- Filter(function(x) x$type == "file", content(folder_response, as = "parsed"))
message(length(files), " file(s) found in '", latest_folder$name, "'")

# download each file and push to 287(g) repo
for (f in files) {
  
  # download raw file content into memory as raw bytes
  dl       <- GET(f$download_url)
  raw_data <- content(dl, as = "raw")
  
  # base64-encode it
  b64_content <- base64_encode(raw_data)
  
  # destination path inside your repo
  dest_path <- paste0(DEST_FOLDER, "/", latest_folder$name, "/", f$name)
  dest_url  <- paste0("https://api.github.com/repos/", DEST_OWNER, "/", DEST_REPO,
                      "/contents/", dest_path)
  
  # check if file already exists in your repo
  check    <- GET(dest_url, auth_header)
  existing <- if (status_code(check) == 200) content(check, as = "parsed") else NULL
  
  # build the PUT request body
  body <- list(
    message = paste0("Add ", f$name, " from ", latest_folder$name),
    content = b64_content,
    branch  = "main"
  )

  # if file exists, include its SHA (for updates)
  if (!is.null(existing)) body$sha <- existing$sha

  # push to 287(g) repo
  put_response <- PUT(
    dest_url,
    auth_header,
    body   = toJSON(body, auto_unbox = TRUE),
    encode = "raw",
    add_headers("Content-Type" = "application/json")
  )
  
  if (status_code(put_response) %in% c(200, 201)) {
    message("Pushed: ", f$name, " → ", dest_path)
  } else {
    warning("Failed: ", f$name, " (HTTP ", status_code(put_response), ")")
    print(content(put_response))
  }
}