# download and execute Dave Harris' BBS data extraction code

# ~~~~~~~~~~~~~~~~
# download code

# create directory for code & data
dave_code <- 'test/dave_data/extras/BBS-analysis/'
dir.create(dave_code,
           recursive = TRUE,
           showWarnings = FALSE)

# and the two sub folders
dir.create(paste0(dave_code, 'data_extraction/'),
           showWarnings = FALSE)
dir.create(paste0(dave_code, 'BBS_evaluation/'),
           showWarnings = FALSE)

# get the three key scripts
download.file('https://raw.githubusercontent.com/davharris/mistnet/c038a17ef9e5af6852783c544368a7e45c7bc28e/extras/BBS-analysis/data_extraction/data-extraction.R',
              destfile = paste0(dave_code, 'data_extraction/data-extraction.R'),
              method = 'curl')

download.file('https://raw.githubusercontent.com/davharris/mistnet/c038a17ef9e5af6852783c544368a7e45c7bc28e/extras/BBS-analysis/data_extraction/species-handling.R',
              destfile = paste0(dave_code, 'data_extraction/species-handling.R'),
              method = 'curl')

download.file('https://raw.githubusercontent.com/davharris/mistnet/c038a17ef9e5af6852783c544368a7e45c7bc28e/extras/BBS-analysis/BBS_evaluation/define-train-folds.R',
              destfile = paste0(dave_code, 'BBS_evaluation/define-train-folds.R'),
              method = 'curl')

# edit one of these scripts

# load the script as lines
script_lines <- readLines('test/dave_data/extras/BBS-analysis/data_extraction/species-handling.R')

# remove a failing data check from one of these scripts
script_lines <- script_lines[-(30:32)]

# amend the header editing to account for the lost line
script_lines[6:26] <- gsub('6', '5', script_lines[6:26])
script_lines[6:26] <- gsub('7', '6', script_lines[6:26])

# write to disk
cat(paste(script_lines, collapse = '\n'),
    file = 'test/dave_data/extras/BBS-analysis/data_extraction/species-handling.R')



# get the current  working directory
owd <- getwd()

# change to Dave's working directory
setwd('test/dave_data/')

# check there's a folder containing the proprietary BBS data
if (!(dir.exists('proprietary.data/BBS/50-StopData/1997ToPresent_SurveyWide/') &&
      dir.exists('proprietary.data/BBS/DataFiles/'))) {
  stop (paste('\n\nProprietary BBS data must be downloaded and unzipped in the folder: ',
              paste0(getwd(), '/proprietary.data/'),
              '\n\nThese files can be downloaded from: ',
              'ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/',
              '\n\nThe terms of use are here: ',
              'https://www.pwrc.usgs.gov/bbs/rawdata/'))
}

# assuming that's fine, run Dave's code
source('extras/BBS-analysis/data_extraction/data-extraction.R')
source('extras/BBS-analysis/BBS_evaluation/define-train-folds.R')

# change back to the old working directory
setwd(owd)
