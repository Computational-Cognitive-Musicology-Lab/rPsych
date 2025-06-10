setGeneric('loadFiles', function(object) NULL)

setMethod('loadFiles', 'trial',
          \(object) {
            if (grepl('Audio|Video|Image', object@Type)) {
              object@Args$stimulus
            }
          })

setMethod('loadFiles', 'block',
          \(object) {
            stimuliFiles <- unlist(lapply(object@Trials, loadFiles)) |> unique()
            timelineVar <- stimuliFiles[stimuliFiles %in% names(object@Stimuli)]
            
            stimuliFiles <- setdiff(stimuliFiles, timelineVar)
            stimuliFiles <- c(stimuliFiles, unlist(object@Stimuli[timelineVar]))
            stimuliFiles
            
          })

setMethod('loadFiles', 'experiment',
          \(object) {
            stimuliFiles <- unlist(lapply(object@Parts, loadFiles)) |> unique()
            exist <- sapply(stimuliFiles, file.exists)
            if (any(!exist)) {
              stop(if (all(!exist)) "The " else "Some of the ",
                   "files you have indicated as stimuli sources can't be found on your system.",
                   " Did you mispell their names or directory path?",
                   "\n\t\tThe missing files are:\n", paste(stimuliFiles[!exist], collapse = '\n\t\t\t'),
                   .call = FALSE)
            }
            
            extensions <- list(audio = c('\\.wav$', '\\.mp3$'),
                               video = c('\\.mp4$', '\\.ogg$', '\\.webm$'),
                               image = c('\\.png$', '\\.jpg$', '\\.jpeg$'))
            
            if (length(stimuliFiles)) {
              stimuliFiles <- lapply(extensions, 
                                     \(ext) {
                                       
                                       matchingfiles <- stimuliFiles[Reduce('|', lapply(ext, \(pat) grepl(pat, stimuliFiles)))]
                                       if (length(matchingfiles)) {
                                         paste0('[', 
                                                paste(paste0('"', 
                                                             matchingfiles,
                                                             '"'),
                                                      collapse = ', '), '],')
                                       }
                                   
                                     }) |> setNames(names(extensions))
              
              
             c('/* preload audio/video/image files */',
               'var preload = {',
               '    type: jsPsychPreload,',
               if (length(stimuliFiles$audio)) paste0('audio: ', stimuliFiles$audio),
               if (length(stimuliFiles$video)) paste0('video: ', stimuliFiles$video),
               if (length(stimuliFiles$image)) paste0('images: ', stimuliFiles$image),
               '};', '')
             
             
            }
            
          })
