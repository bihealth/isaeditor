## Loads example data to work with

file <- system.file("extdata", "s_isatab.txt", package="isaeditor")
isa_s <- read_isa(file)
file <- system.file("extdata", "a_isatab_transcriptome_profiling_nucleotide_sequencing.txt", package="isaeditor")
isa_a <- read_isa(file)


