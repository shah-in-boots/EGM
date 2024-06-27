# Code to extract models saved on shah-in-boots/models

#' Tensorflow-based electrophysiology models
#'
#' @description This function loads pre-trained electrophysiology signal
#'   processing models trained using `{keras}` and `{tensorflow}`. This function
#'   serves as a wrapper around [keras::load_model_tf()]. The source for these
#'   do not contain any personal or protected health information. The models are
#'   generally public-domain, and can be used for research purposes.
#'   Importantly, this function requires an internet connection to work. If
#'   internet is not available, the function will return empty with a message.
#'
#'   To select a model, please use the parameters which in turn help decide what
#'   model type is of interest. To allow for flexibility in the future, the
#'   model names have been parameterized. For example, a model may be named
#'   *CNN_500x12_Binary_AF_v1*
#'
#'   The model types are detailed below.
#'
#' @param model_architecture Neural network structure, such as *CNN*. The supported types currently are:
#'
#' - *CNN* = convolutional neural network
#'
#' @param input_shape The dimensions of the input array for a single sample,
#'   listed as \eqn{M \\\times N}. For example, an ECG signal of 500 samples
#'   with 12 leads would be written as *500x12*.
#'
#' @param output_distribution The outcome of the model is usually a
#'   classification problem, and the option s below are commonly used:
#'
#' - *Binary* = a binary classification output
#'
#' - *Multiclass* = a multi-class classification problem
#'
#' @param output_distribution The individual models may have a specific type of
#'   output type. For example, if doing an ECG classification into atrial
#'   fibrillation versus sinus (yes/no), the output type may be labeled as
#'   *AF*
#'
#' @param version Some models may be have iterations that are stored. In any
#'   models that are associated with core functions in `{EGM}`, several versions
#'   will be retained. Some models may not have a version, and thus may be
#'   `NULL`
#'
#' @param ... Additional parameters to be passed to the function
#'
#' @returns A `SavedModel` format from `{keras}` and `{tensorflow}`
#'
#' @name ep_models
#' @export
load_electrophysiology_models <- function(model_architecture,
																					input_shape,
																					output_distribution,
																					output_type,
																					version = NULL,
																					...) {

	# Ensure internet is avaible, otherwise end early
	if (is.null(curl::nslookup("google.com", error = FALSE))) {
		message("Internet connection is not available. Please check your connection.")
		return()
	}

	# Get list of possible names of models that are supported currently
	# This will use the github API to look at the contents of the repository
	repoRoot <- "shah-in-boots"
	repoName <- "model_archive"
	# Get the contents of the folder
	url <- sprintf("https://api.github.com/repos/%s/%s/contents", repoRoot, repoName)
	folderContent <-
		url |>
		httr::GET() |>
		httr::content("text") |>
		jsonlite::fromJSON()
	possibleModels <- folderContent$name[folderContent$type == "dir"]

	# Name of model based on parameters
	# This will end up being the model name (which is contained in a folder)
	modelName <- paste0(model_architecture, "_", input_shape, "_", output_distribution, "_", output_type, "_", version)

	if (!(modelName %in% possibleModels)) {
		stop("The model selected is not within the list of current models.")
	}

	# Check to make sure model name is within the list of possible names

	# Create a temporary workspace that will be deleted at the end of the function
	# Use this space to download files

	tmpFolder <- tempdir()
	download_github_folder(owner = repoRoot,
												 repo = repoName,
												 folder_name = modelName,
												 download_dir = tmpFolder)

	# Read in / load the model
	model <- keras::load_model_tf(fs::path(tmpFolder, modelName))

	# Unlink and delete the file at the end
	unlink(tmpFolder, recursive = TRUE)

	# Return the model
	model

}


#' Internal function to download a github folder
#' @keywords internal
#' @noRd
download_github_folder <- function(owner, repo, folder_name, download_dir) {

	# Get the contents of the folder
	url <- sprintf("https://api.github.com/repos/%s/%s/contents/%s", owner, repo, folder_name)
	folderContent <-
		url |>
		httr::GET() |>
		httr::content("text") |>
		jsonlite::fromJSON()

	# The content of the url is a combination of files and folders
	# Will need to identify the item type
	# 	If a file, can download
	# 	If a folder, need to recursively download its contents

	for (i in 1:nrow(folderContent)) {

		# The individual types of data for that item
		rowData <- folderContent[i, ]
		itemName <- rowData$name
		itemPath <- rowData$path
		itemType <- rowData$type

		# If its a directory, must recurse through
		if (itemType == "dir") {
			#fs::dir_create(fs::path(download_dir, itemPath))
			download_github_folder(owner, repo, itemPath, download_dir)
		} else if (itemType == "file") {
			# Download the file into the download directory
			filePath <- fs::path(download_dir, itemPath)
			# Create a directory for it if it does not exist
			fs::dir_create(fs::path_dir(filePath))
			httr::GET(rowData$download_url, httr::write_disk(filePath, overwrite = TRUE))
		}

	}

}
