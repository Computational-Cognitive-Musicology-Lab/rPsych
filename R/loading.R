extensions <- list(audio = c('\\.wav$', '\\.mp3$'),
                   video = c('\\.mp4$', '\\.ogg$', '\\.webm$'),
                   image = c('\\.png$', '\\.jpg$', '\\.jpeg$'))

setGeneric('loadFiles', function(object, directory, stimuliTable = NULL) NULL)

setMethod('loadFiles', 'trial',
          \(object, directory, stimuliTable = NULL) { # directory arg not used
            if (grepl('Audio|Video|Image', object@Type)) {
              originalFile <- object@Args$stimulus
              stimNames <- if (is.null(stimuliTable)) '' else names(stimuliTable)
                if (originalFile  %in% stimNames) {
                  originalFile <- c()
                } else {
                  if (any(sapply(unlist(extensions), \(ext) str_detect(originalFile, ext)))) {
                        object@Args$stimulus <- paste0('stimuli', .Platform$file.sep, basename(originalFile))
                  }
                  
                }
              
              list(object = object, files = originalFile)
              
            } else {
              list(object = object, files = c())
            }
          })

setMethod('loadFiles', 'block',
          \(object, directory, stimuliTable = NULL) { # directory arg not used
            trials <- lapply(object@Trials, loadFiles, stimuliTable = object@Stimuli)
            
            object@Trials <- lapply(trials, '[[', i = 'object')
            originalFiles <- lapply(trials, '[[', i = 'files') |>
              unlist() |>
              unique()
            
            timelineVar <- originalFiles[originalFiles %in% names(object@Stimuli)]
            
            originalFiles <- setdiff(originalFiles, timelineVar)
            originalFiles <- c(originalFiles, unlist(object@Stimuli[timelineVar]))
            
            list(object = object, files = originalFiles)
            
          })

setMethod('loadFiles', 'experiment',
          \(object, directory) {
            parts <- lapply(object@Parts, loadFiles)
            
         
            originalFiles <- lapply(parts, '[[', i = 'files') |>
              unlist() |>
              unique()
            
            
            exist <- sapply(originalFiles, file.exists)
            if (any(!exist)) {
              stop(if (all(!exist)) "The " else "Some of the ",
                   "files you have indicated as stimuli sources can't be found on your system.",
                   " Did you mispell their names or directory paths?",
                   "\n\t\tThe missing files are:\n", paste(originalFiles[!exist], collapse = '\n\t\t\t'),
                   .call = FALSE)
            }
            
            
            
            
            # prepare the preload js, and copy files to the experiment directory

            
            if (length(originalFiles)) {
              object@Parts <- lapply(parts, '[[', i = 'object')
              
              # there could be a problem if files with same name in different directories
              # we should check this
              newFiles <- paste0(directory, .Platform$file.sep, 
                                 'stimuli', .Platform$file.sep,
                                  basename(originalFiles))
              message('Copying ', length(originalFiles), ' stimuli files to "stimuli" directory...', appendLF = FALSE)
              Map(\(from, to) file.copy(from, to), originalFiles, newFiles)
              message('done.')
              
              stimuliFiles <- paste0('stimuli', .Platform$file.sep, basename(originalFiles))
              stimuliFiles_bytype <- lapply(extensions, 
                                     \(ext) {
                                       
                                       matchingfiles <- stimuliFiles[Reduce('|', lapply(ext, \(pat) grepl(pat, basename(originalFiles))))]
                                       if (length(matchingfiles)) {
                                         paste0('[', 
                                                paste(paste0('"', 
                                                             matchingfiles,
                                                             '"'),
                                                      collapse = ', '), '],')
                                       }
                                   
                                     }) |> setNames(names(extensions))
              
              
              list(js = c('/* preload audio/video/image files */',
                          'var preload = {',
                          '    type: jsPsychPreload,',
                          if (length(stimuliFiles_bytype$audio)) paste0('    audio: ', stimuliFiles_bytype$audio),
                          if (length(stimuliFiles_bytype$video)) paste0('    video: ', stimuliFiles_bytype$video),
                          if (length(stimuliFiles_bytype$image)) paste0('    images: ', stimuliFiles_bytype$image),
                          '};', ''),
                   experiment = object)
             
             
            } else {
              list(js = c(), experiment = object)
            }
            
          })
