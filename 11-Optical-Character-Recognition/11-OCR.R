
### set wd to source file dir
setwd("~/work/Teaching/DSIER23/11-Optical-Character-Recognition")

if (!require("pacman")) install.packages("pacman"); library(pacman)
if (!require("magick")) install.packages("magick"); pacman::p_load(magick)
if (!require("tesseract")) install.packages("tesseract"); pacman::p_load(tesseract)
if (!require("hunspell")) install.packages("hunspell"); pacman::p_load(hunspell)


# system-level commands and should be run in the terminal
# sudo apt-get update
# sudo apt-get install libfftw3-dev
# sudo apt-get install libcurl4-openssl-dev

file_directory <- "input"
my_files <- list.files(file_directory, full.names = TRUE)


# load the image 
img <- load.image(my_files[1]) 
# convert the image to grayscale
img_gray <- grayscale(img)

# display the grayscale image
plot(img_gray)
# read the image into a magick image object
img <- image_read(img_gray)
# convert the image to grayscale using magick
img <- image_convert(img, 'grayscale')
# enhance contrast to differentiate text from background
img <- image_contrast(img)
# binarize te image: it converts the image to black and white, which might improve OCR results
img <- image_threshold(img, 'white')
# image after preprocessing, how is it? save it
plot(img)
image_write(img, 'input/preprocessed.jpg')

# create an OCR engine with English language
eng <- tesseract("eng")
# perform OCR on the preprocessed image
ocr_text <- ocr('input/preprocessed.jpg', engine = eng)
file.remove('input/preprocessed.jpg')
# compare with unprocessed
ocr_text_withoutpreproc <- ocr('input/orig_5752154_16544.jpg', engine = eng)
print(ocr_text)
print(ocr_text_withoutpreproc)

# replace newline characters with a space
ocr_text <- gsub("\n", " ", ocr_text_withoutpreproc)

# split the OCR text into words using whitespace as a delimiter
words <- strsplit(ocr_text, "\\s")[[1]]

# check spelling of each word
correct <- hunspell_check(words)

# identify incorrect words
incorrect_words <- words[!correct]

# get spelling suggestions for the incorrect words
suggestions <- hunspell_suggest(incorrect_words)

# create a dataframe to hold the incorrect word and its first suggestion
corrections_df <- data.frame(word = character(), suggestion = character())
for(i in seq_along(incorrect_words)) {
  corrections_df[i, "word"] <- incorrect_words[i]
  
  #  if there are no suggestions assign NA
  if(length(suggestions[[i]]) > 0) {
    corrections_df[i, "suggestion"] <- suggestions[[i]][1]
  } else {
    corrections_df[i, "suggestion"] <- NA
  }
}

# print the dataframe
print(corrections_df)

# correct the OCR text using the first suggestion for each incorrect word
corrected_text <- ocr_text
for (i in 1:nrow(corrections_df)) {
  # Replace incorrect word with the first suggestion
  if (!is.na(corrections_df$suggestion[i]) & nchar(corrections_df$word[i])!=1) {
    corrected_text <- gsub(corrections_df$word[i], corrections_df$suggestion[i], corrected_text, fixed = TRUE)
  }
}
print(corrected_text)


# create a list of file paths
file_paths  <- list.files("input", full.names = TRUE)

# initialize an empty list to store corrected texts
corrected_texts <- list()

# create an OCR engine with English language
eng <- tesseract("eng")

# loop over all files
for (file_path in file_paths) {
  
  cat("Processing file", i, "of", length(file_paths), ": ", file_path, "\n")
  ocr_text <- ocr(file_path, engine = eng)
  ocr_text <- gsub("\n", " ", ocr_text)

  words <- unlist(strsplit(ocr_text, "\\s"))
  correct <- hunspell_check(words)
  incorrect_words <- words[!correct]
  suggestions <- hunspell_suggest(incorrect_words)
  corrections_df <- data.frame(word = incorrect_words, suggestion = NA, stringsAsFactors = FALSE)
  
  # update the dataframe with suggestions
  for (i in seq_along(suggestions)) {
    if (length(suggestions[[i]]) > 0) {
      corrections_df$suggestion[i] <- suggestions[[i]][1]
    }
  }
  
  # correct the OCR text using the first suggestion for each incorrect word
  corrected_text <- ocr_text
  for (i in seq_len(nrow(corrections_df))) {
    if (!is.na(corrections_df$suggestion[i]) && nchar(corrections_df$word[i]) != 1) {
      corrected_text <- gsub(corrections_df$word[i], corrections_df$suggestion[i], corrected_text, fixed = TRUE)
    }
  }
  
  # append corrected text to the list
  corrected_texts[[file_path]] <- corrected_text
}

# attac all corrected texts into a single dataframe
all_corrections_df <- data.frame(file_path = names(corrected_texts), corrected_text = unlist(corrected_texts), stringsAsFactors = FALSE)





