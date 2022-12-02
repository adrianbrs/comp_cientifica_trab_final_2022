files <- list(
  c("https://www.kaggle.com/datasets/shivamb/netflix-shows/download", "netflix_titles.csv")
)

for (f in files) {
  url <- f[1]
  base_dir <- "./data";
  target <- paste(base_dir, "/", stringr::str_trim(f[2]), sep = '')
  cat("Checking for file", target, "...\n")
  
  if (!dir.exists(base_dir)) {
    dir.create(base_dir)
  }
  
  if (!file.exists(target)) {
    cat("Downloading file data:", target, "\n")
    download.file(url, target, method = "auto", )
    cat("Success!\n")
  } else {
    cat("File ", target, " already exists, skip...\n")
  }
}
