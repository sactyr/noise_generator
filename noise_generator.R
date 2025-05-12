library(tuneR)
library(purrr)
library(glue)

setwd("./Noise")

sample_duration <- 60
sample_rate <- 44100

# Function to create a single 60-second stereo MP3 segment
generate_single_mp3_chunk <- function(
    noise_type
    ,sample_duration_sec = sample_duration
    ,sample_rate = sample_rate
) {

  # Generate noise
  noise_wave <- noise(
    kind = noise_type
    ,duration = sample_duration_sec
    ,samp.rate = sample_rate
    ,bit = 32
    ,stereo = TRUE
    ,xunit = "time"
  )
  
  # Normalize to avoid clipping
  noise_wave <- normalize(noise_wave, unit = "32", pcm = TRUE)
  
  # File paths
  wav_path <- file.path("./Audio/Sample", glue("{noise_type}.wav"))
  mp3_path <- file.path("./Audio/Sample", glue("{noise_type}.mp3"))
  
  # Write to a temporary WAV file
  writeWave(noise_wave, wav_path)

  # Save as MP3
  system(glue("ffmpeg -y -i {shQuote(wav_path)} -codec:a libmp3lame -qscale:a 2 {shQuote(mp3_path)}"))
  
  # Remove temp WAV file
  file.remove(wav_path)
}

# Create long mp3 noises

walk(
  .x = c("white", "pink")
  ,.f = ~{
    
    # Generate white and pink noise samples
    generate_single_mp3_chunk(noise_type = .x)
    
    noise_duration <- c(120, 240, 8*60*60)
    
    # Number of sample files to copy
    max_chunks <- ceiling(max(noise_duration) / sample_duration) 
    
    # Copy samples chunk times
    walk(
      .x = seq_len(max_chunks)
      ,.f = function(y) {
        
        print(glue("Copying {.x} audio chunks: {y} of {max_chunks}"))

        file.copy(
          from = file.path("./Audio/Sample", glue({.x}, ".mp3"))
          ,to = file.path("./Audio/Temp", glue({.x}, "_", {y}, ".mp3"))
        )

      }
    )

    mp3_files <- file.path(
      "./Audio/Temp"
      ,paste0({.x}, "_", seq_len(max_chunks), ".mp3")
    )

    walk(
      .x = noise_duration
      ,.f = function(y) {
        
        chunks <- ceiling(y / 60)
        
        mp3_files_chunked <- mp3_files[1:chunks]
        
        mp3_list_file <- tempfile(fileext = ".txt")
        writeLines(sprintf("file '%s'", normalizePath(mp3_files_chunked)), mp3_list_file)
        
        mp3_output_file <- file.path("./Audio", glue({.x}, "_", {y}, ".mp3"))
        
        print(glue("Combining {.x} audio {y} seconds"))
        
        system(glue("ffmpeg -y -f concat -safe 0 -i {shQuote(mp3_list_file)} -acodec libmp3lame -q:a 2 {shQuote(mp3_output_file)}"))
        
        mp4_output_file <- file.path("./Videos", glue({.x}, "_", {y}, ".mp4"))
        
        # Create a black screen video with the audio
        
        print(glue("Creating {.x} video {y} seconds"))
        
        system(
          glue("ffmpeg -y -f lavfi -i color=black:s=1280x720:d={y} -i {shQuote(mp3_output_file)} \ -c:v libx264 -preset veryfast -tune stillimage -c:a aac -shortest {shQuote(mp4_output_file)}")
        )
        
      }
    )
    
    file.remove(mp3_files)
    
  }
)
