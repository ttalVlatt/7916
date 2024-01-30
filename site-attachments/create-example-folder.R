## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Create Downloadable Example Folder in Output Directory]
##' [INIT: 2023-12-22]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## ----------------------------------------------------------------------------

library(tidyverse)

output <- Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR")

##'[Remove duplicates in output created by Quarto glitch]
dup_files <- list.files(output, pattern = "\\s\\d\\.[a-zA-Z0-9]+$")
dup_folders <- list.files(output, pattern = "\\s\\d$")

for(i in dup_files) {
  unlink(file.path(output, i))
}

for(i in dup_folders) {
  fs::dir_delete(file.path(output, i))
}

##'[Remove duplicates in main created by Quarto glitch]
dup_files <- list.files(pattern = "\\s\\d\\.[a-zA-Z0-9]+$")
dup_folders <- list.files(pattern = "\\s\\d$")

for(i in dup_files) {
  unlink(i)
}

for(i in dup_folders) {
  fs::dir_delete(i)
}

##'[Remove duplicates in r-scripts created by Quarto glitch]
dup_files <- list.files(file.path(output, "r-scripts"), pattern = "\\s\\d\\.[a-zA-Z0-9]+$")
dup_folders <- list.files(file.path(output, "r-scripts"), pattern = "\\s\\d$")

for(i in dup_files) {
  unlink(file.path(output, "r-scripts", i))
}

for(i in dup_folders) {
  fs::dir_delete(file.path(output, "r-scripts", i))
}

##'[Remove duplicates in data created by Quarto glitch]
dup_files <- list.files(file.path("data"),
                                  recursive = T,
                                  pattern = "\\s\\d\\.[a-zA-Z0-9]+$")

dup_folders <- list.files(file.path("data"),
                          recursive = T,
                          pattern = "\\s\\d$")

for(i in dup_files) {
  unlink(file.path("data", i))
}

for(i in dup_folders) {
  fs::dir_delete(file.path("data", i))
}

##'[Create example folder]

## Delete the old example folder to remake it
if(dir.exists(file.path(output, "EDH-7916"))) {
  ## Delete old unzipped example folder
  unlink(file.path(output, "EDH-7916"), recursive = T)
}

example <- file.path(output, "EDH-7916")
dir.create(example)

##'[Create data sub-folder]
fs::dir_copy("data", file.path(example, "data"), overwrite = T)

##'[Copy Lesson R Scripts]
r_scripts <- list.files(file.path(output, "r-scripts"))

## Delete non-lesson scripts from list
r_scripts <- r_scripts |>
  discard(~str_detect(.x, "^_.*")) |>
  discard(~str_detect(.x, "index")) |>
  discard(~str_detect(.x, "syllabus")) |>
  discard(~str_detect(.x, "99")) |>
  discard(~str_detect(.x, "07")) |>
  discard(~str_detect(.x, "14")) |>
  discard(~str_detect(.x, "x-01")) |>
  discard(~str_detect(.x, "x-02")) |>
  discard(~str_detect(.x, "x-04"))

##'[Add lesn- to all lesson scripts for grouping]

for(i in r_scripts) {
  
  # new_name <- paste0("l-", i)
  file.copy(from = file.path(output, "r-scripts", i),
            to = file.path(example, i))
  
}

##'[Copy in R Script Template]
file.copy(from = file.path("site-attachments", "r-script-template.R"),
          to = file.path(example, "r-script-template.R"))

##'[Copy in Syllabus .pdf]
file.copy(from = file.path(output, "syllabus.pdf"),
          to = file.path(example, "syllabus.pdf"))

##'[Create Final Project Sub-Folder]

final <- file.path(example, "reproducible-report")
dir.create(final)
# Copy .gitignore both for function and for placeholder
file.copy(from = ".gitignore", to = file.path(final, ".gitignore"))
dir.create(file.path(final, "data"))
# Copy hd2007 as placeholder to ensure folder is made in zipping
file.copy(from = file.path("data", "hd2007.csv"),
          to = file.path(final, "data", "placeholder.csv"))


##' [Remove any Duplicates in Example Folder created by Quarto/icloud glitch]

dup_files <- list.files(example,
                        recursive = T,
                        include.dirs = T,
                        all.files = T,
                        pattern = "\\s\\d(\\.[a-zA-Z0-9]+)?$")

for(i in dup_files) {
  unlink(file.path(example, i), recursive = T)
}

##'[ZIP Example Folder]

## To avoid empty files, setwd to the example
setwd(example)
## Then list files from the newly set wd
files <- list.files(full.names = T, recursive = T)
## Then zip from this new wd (the default root)
## Can't just set the root here, as the files need to be listed from the same wd
zip::zip(file.path("..", "EDH-7916.zip"),
         files = files,
         mode = "mirror")
## Reset working directory back to project folder
setwd(file.path("..", ".."))

##'[Update git EDH-7916 with Fresh Scripts]

## Delete old temp (fail-safe, should have already been un-linked)
# if(dir.exists(file.path("..", "temp"))) {
#   unlink(file.path("..", "temp"), recursive = T)
# }

## Copy fresh temp folder using EDH-7916 from website rendering
# fs::dir_copy(file.path(output, "EDH-7916"),  #example,
#              new_path = file.path("..", "temp"),
#              overwrite = TRUE)

## Delete old EDH-7916
if(dir.exists("EDH-7916")) {
  unlink("EDH-7916", recursive = T)
}

## Send git commands directly to terminal
system("
   git clone git@github.com:ttalVlatt/EDH-7916.git;
   cp -r _site/EDH-7916/* EDH-7916;
   cd EDH-7916;
   git add .;
   git commit -m 'update scripts';
   git push origin
   cd ..
       ")

## Delete temp file
# unlink(file.path("..", "temp"), recursive = T)


## Use git commands directly to terminal
# system("cd ../EDH-7916;
#        git init;
#        touch .gitignore;
#        echo '.DS_Store' >> .gitignore;
#        echo '.Rhistory' >> .gitignore;
#        echo '.Rproj.user/' >> .gitignore;
#        git add .;
#        git commit -m 'update scripts';
#        git remote add EDH-7916 https://github.com/ttalVlatt/EDH-7916;
#        git push -u --force EDH-7916 main;
#        cd ../7916") 

  
       # git init;
       # touch .gitignore;
       # echo '.DS_Store' >> .gitignore;
       # echo '.Rhistory' >> .gitignore;
       # echo '.Rproj.user/' >> .gitignore;
       # git add .;
       # git commit -m 'update scripts';
       # git remote add EDH-7916 https://github.com/ttalVlatt/EDH-7916;
       # git push -u --force EDH-7916 main;
       # cd ../7916")


# writeLines(paste("This repo was last updated", Sys.time()),
#            "Last-Updated.txt")

## Clone down the EDH-7916 folder to Desktop if not there
# if(!dir.exists("../EDH-7916")) {
# 
#   git2r::clone("https://github.com/ttalVlatt/EDH-7916", "../EDH-7916")
#   
# }

## Copies files over from created EDH-7916 to cloned repo
# fs::dir_copy(file.path("_site", "EDH-7916"),  #example,
#              new_path = file.path("..", "EDH-7916"),
#              overwrite = TRUE)

# file.copy()

## Back out of 7916 repo into new student repo
# setwd(file.path("..", "EDH-7916"))

## Initialize new folder as a git repo
## git2r::init()

## Pull down for any updates
# git2r::pull()

## Write a .txt file with last updated time (primarily a file to inital commmit)


# ## Add the .txt we just created
# git2r::add(path = "Last-Updated.txt")
# 
# ## Make an initial commit so we can push to remote
# git2r::commit(message = "Updates Scripts", all = TRUE)
# 
# ## Set up the remote
# git2r::push()







## Add the EDH-7916 remote repo as the remote_url
# git2r::remote_add(name = "EDH-7916",
#                   url = "https://github.com/ttalVlatt/EDH-7916")
# 
# git2r::repository_head()

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------
