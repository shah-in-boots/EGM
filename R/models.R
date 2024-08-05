# Code to extract models saved on shah-in-boots/models

#' Tensorflow-based electrophysiology models
#'
#' @description This function loads pre-trained electrophysiology signal
#'   processing models trained using `{keras}` and `{tensorflow}`. This function
#'   serves as a wrapper around [keras::load_model_tf()], and other loading
#'   functions. The default source for these do not contain any personal or
#'   protected health information. The models are generally public-domain, and
#'   can be used for research purposes. Importantly, this function requires an
#'   internet connection to work. If internet is not available, the function
#'   will return empty with a message.
#'
#' @details In an interactive mode, all models available based on the default
#'   repository will be listed. In a non-interactive (or scripted) mode, the
#'   user must define the model upfront. To select a model, please use the
#'   parameters which in turn help decide what model type is of interest. To
#'   allow for flexibility in the future, the model names have been
#'   parameterized.
#'
#'   For example, a model may be named "CNN_500x12_Binary_AF_v1", which
#'   represents the general model architecture, its input type, the output
#'   distribution, the general output type, followed by the version.
#'   Alternatively, the full model name can be used as a shortcut (and to help
#'   with models that don't follow this pattern).
#'
#' @param model_format The format of the model to be loaded. The supported
#'  formats are:
#'
#'  - `SavedModel` = a model saved using [keras::save_model_tf()]
#'
#' @param model_name Full model name, regardless of parameters. Supersedes the
#'   other model calling parameter names.
#'
#' @param model_architecture Neural network structure, such as *CNN*. The
#'   supported types currently are:
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
#' @returns Depending on the the model format, will return a `SavedModel` format
#'   from `{keras}` and `{tensorflow}`
#'
#' @name ep_models
#' @export
load_electrophysiology_models <- function(model_format,
																					model_name = NULL,
																					model_architecture = NULL,
																					input_shape = NULL,
																					output_distribution = NULL,
																					output_type = NULL,
																					version = NULL,
																					.repository_root = NULL,
																					.repository_name = NULL,
																					...) {

	# Ensure internet is available, otherwise end early
	if (is.null(curl::nslookup("google.com", error = FALSE))) {
		message("Internet connection is not available. Please check your connection.")
		return()
	}

	# Get list of possible names of models that are supported currently
	# This will use the github API to look at the contents of the repository
	if (is.null(.repository_root)) {
		repoRoot <- "shah-in-boots"
	} else {
		repoRoot <- .repository_root
	}

	if (is.null(.repository_name)) {
		repoName <- "model_archive"
	} else {
		repoName <- .repository_name
	}

	# Get the contents of the folder
	url <- sprintf("https://api.github.com/repos/%s/%s/contents", repoRoot, repoName)
	folderContent <-
		url |>
		httr::GET() |>
		httr::content("text") |>
		jsonlite::fromJSON()
	possibleModels <- folderContent$name[folderContent$type == "dir"]

	if (interactive()) {
		# Present the list of models to the user
		cat("Available models:\n")
		for (i in seq_along(possibleModels)) {
			cat(i, ": ", possibleModels[i], "\n", sep = "")
		}

		# Let the user choose a model
		choice <- as.integer(readline(prompt = "Enter the number of the model you want to load: "))

		if (is.na(choice) || choice < 1 || choice > length(possibleModels)) {
			stop("Invalid choice. Please run the function again and select a valid number.")
		}

		modelName <- possibleModels[choice]

	} else if (is.null(model_name)) {
		# Non-interactive mode as well, but specified modle name in full
		modelName <- model_name

		if (!(modelName %in% possibleModels)) {
			stop("The specified model does not exist in the repository.")
		}
	} else {
		# Non-interactive mode: construct model name from parameters
		if (is.null(model_architecture) || is.null(input_shape) ||
				is.null(output_distribution) || is.null(output_type)) {
			stop(
				"In non-interactive mode, you must provide `model_architecture`, `input_shape`, `output_distribution`, and `output_type`."
			)
		}

		# Pasting together the model name
		modelName <- paste0(model_architecture,
												"_",
												input_shape,
												"_",
												output_distribution,
												"_",
												output_type)

		# Sometimes models will be un-versioned
		if (!is.null(version)) {
			modelName <- paste0(modelName, "_", version)
		}

		if (!(modelName %in% possibleModels)) {
			stop("The specified model does not exist in the repository.")
		}
	}

	# Create temporary workspace that will be deleted at the end of the function
	# Use this space to download files
	tmpFolder <- tempdir()
	download_github_folder(owner = repoRoot,
												 repo = repoName,
												 folder_name = modelName,
												 download_dir = tmpFolder)

	# Read in / load the model
	if (model_format == "SavedModel") {
		mdl <- keras::load_model_tf(fs::path(tmpFolder, modelName))
	}

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
