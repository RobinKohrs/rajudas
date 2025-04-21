#' Extract and select subdataset information from a raster file using gdalinfo
#'
#' This function runs gdalUtilities::gdalinfo() on a given file,
#' extracts all subdataset information into a tidy data frame, and optionally
#' allows the user to interactively select subdataset variables of interest.
#'
#' @param file Character. Path to the raster file (e.g., NetCDF file)
#' @param interactive Logical. If TRUE, opens an interactive selection for subdatasets. Default: FALSE
#' @param ... Additional arguments passed to gdalUtilities::gdalinfo()
#'
#' @return A list with two elements:
#'   \item{table}{A data frame containing all subdataset information}
#'   \item{selected}{Character vector of selected variable names (only if interactive=TRUE)}
#'
#' @examples
#' \dontrun{
#' # Extract subdatasets from a NetCDF file
#' result = gdal_info_table("data_raw/ensemble/ensemble_2025040800.nc")
#' subdatasets_table = result$table
#'
#' # Extract and interactively select subdatasets
#' result = gdal_info_table("data_raw/ensemble/ensemble_2025040800.nc", interactive = TRUE)
#' selected_variables = result$selected
#' }
#'
#' @export
gdal_info_table = function(file, interactive = FALSE, ...) {
  # Check if file exists
  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  # Check if gdalUtilities package is available
  if (!requireNamespace("gdalUtilities", quietly = TRUE)) {
    stop("Package 'gdalUtilities' is needed for this function to work. Please install it.")
  }

  # Run gdalinfo on the file
  gdalinfo_output = gdalUtilities::gdalinfo(file, ...)

  # Handle case where gdalinfo_output is already split into lines
  if (length(gdalinfo_output) > 1) {
    lines = gdalinfo_output
  } else {
    # Split by lines
    lines = strsplit(gdalinfo_output, "\n")[[1]]
  }

  # Find the indices where subdataset information starts and ends
  start_idx = which(lines == "Subdatasets:")

  # If no subdatasets section found, return empty data frame
  if (length(start_idx) == 0) {
    warning("No subdatasets section found in the gdalinfo output")
    empty_df = data.frame(
      index = integer(),
      name = character(),
      variable = character(),
      description = character(),
      dimensions = character(),
      data_type = character(),
      stringsAsFactors = FALSE
    )
    return(list(table = empty_df, selected = character(0)))
  }

  # Find end of subdatasets section
  # Look for possible section headers that might follow "Subdatasets:"
  possible_end_sections = c("Corner Coordinates:", "Band", "Image Structure Metadata:", "Metadata:")
  end_indices = sapply(possible_end_sections, function(section) {
    idx = which(lines == section)
    if (length(idx) > 0 && idx[1] > start_idx) {
      return(idx[1] - 1)
    } else {
      return(Inf)
    }
  })

  end_idx = min(end_indices, length(lines))
  if (is.infinite(end_idx)) {
    end_idx = length(lines)
  }

  # Extract only the subdataset lines
  subdataset_lines = lines[(start_idx+1):end_idx]

  # If no subdataset lines found, return empty data frame
  if (length(subdataset_lines) == 0) {
    warning("No subdataset entries found in the gdalinfo output")
    empty_df = data.frame(
      index = integer(),
      name = character(),
      variable = character(),
      description = character(),
      dimensions = character(),
      data_type = character(),
      stringsAsFactors = FALSE
    )
    return(list(table = empty_df, selected = character(0)))
  }

  # Create a data frame to store the subdataset information
  subdatasets = data.frame(
    index = integer(),
    name = character(),
    variable = character(),
    description = character(),
    dimensions = character(),
    data_type = character(),
    stringsAsFactors = FALSE
  )

  # Process NAME lines
  name_lines = subdataset_lines[grep("_NAME=", subdataset_lines)]
  for (i in seq_along(name_lines)) {
    # Extract index number
    idx = as.integer(gsub(".*SUBDATASET_([0-9]+)_NAME=.*", "\\1", name_lines[i]))

    # Extract full name
    full_name = gsub(".*SUBDATASET_[0-9]+_NAME=", "", name_lines[i])

    # Extract variable name (the part after the last colon)
    variable = gsub(".*:", "", full_name)

    # Add to data frame
    subdatasets[i, "index"] = idx
    subdatasets[i, "name"] = full_name
    subdatasets[i, "variable"] = variable
  }

  # Process DESC lines
  desc_lines = subdataset_lines[grep("_DESC=", subdataset_lines)]
  for (i in seq_along(desc_lines)) {
    # Handle case where there's no match with the expected format
    if (!grepl("\\[.*\\].*\\(.*\\)", desc_lines[i])) {
      subdatasets[i, "description"] = gsub(".*_DESC=", "", desc_lines[i])
      next
    }

    # Extract dimensions
    dimensions = gsub(".*\\[([^\\]]+)\\].*", "\\1", desc_lines[i])

    # Extract description and data type
    desc_match = regexpr("\\] ([^(]+) \\(([^)]+)\\)", desc_lines[i])
    if (desc_match > 0) {
      match_text = regmatches(desc_lines[i], desc_match)
      desc_and_type = gsub("\\] ([^(]+) \\(([^)]+)\\)", "\\1|\\2", match_text)
      parts = strsplit(desc_and_type, "\\|")[[1]]

      description = trimws(parts[1])
      data_type = parts[2]
    } else {
      # If the pattern doesn't match, try a simpler extraction
      description = gsub(".*\\] ([^(]+).*", "\\1", desc_lines[i])
      data_type = gsub(".*\\(([^)]+)\\).*", "\\1", desc_lines[i])
    }

    # Add to data frame
    subdatasets[i, "description"] = description
    subdatasets[i, "dimensions"] = dimensions
    subdatasets[i, "data_type"] = data_type
  }

  # Sort by index to ensure correct order
  subdatasets = subdatasets[order(subdatasets$index), ]

  # Interactive selection if requested
  selected = character(0)
  if (interactive && nrow(subdatasets) > 0) {
    if (!requireNamespace("utils", quietly = TRUE)) {
      warning("Package 'utils' is needed for interactive selection. Skipping.")
    } else {
      # Create a descriptive display for selection
      display_info = paste0(
        subdatasets$index, ": ",
        subdatasets$variable, " (",
        subdatasets$description, ")"
      )

      # Get user selection
      cat("Available subdatasets:\n")
      selected_indices = utils::select.list(
        display_info,
        title = "Select subdatasets (multiple selections allowed)",
        multiple = TRUE,
        graphics = TRUE
      )

      if (length(selected_indices) > 0) {
        # Convert selected display info back to indices
        selected_idx = as.numeric(gsub("^([0-9]+):.*", "\\1", selected_indices))
        # Get the variable names for the selected indices (not the full subdataset names)
        selected = subdatasets$variable[match(selected_idx, subdatasets$index)]
      }
    }
  }

  # Return both the table and selected variables
  return(list(
    table = subdatasets,
    selected = selected
  ))
}
