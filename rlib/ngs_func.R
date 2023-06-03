library(stringr)
library(magrittr)
library(Biostrings)

pairwise_alignment <- function(ref_fa, query_fa, alignment_type = "global", alignment_strategy = "Many-to-One", block_width = 50) {
  ref_ls <- seqinr::read.fasta(file = textConnection(ref_fa), seqtype = "DNA", as.string = T, forceDNAtolower = F, set.attributes = F)
  query_ls <- seqinr::read.fasta(file = textConnection(query_fa), seqtype = "DNA", as.string = T, forceDNAtolower = F, set.attributes = F)
  ref_set <- DNAStringSet(unlist(ref_ls), use.names = T)
  query_set <- DNAStringSet(unlist(query_ls), use.names = T)
  
  if (alignment_strategy == "One-to-One") {
    if (length(ref_set) != length(query_set)) {
      stop("LengthNotEqualError: the number of reference items must equal the number of query items in One-to-One mode!")
    }
    pa_res <- pairwiseAlignment(ref_set, query_set, type = alignment_type)
    paste0(capture.output(writePairwiseAlignments(pa_res, block.width = block_width)), collapse = "\n")
  } else if (alignment_strategy == "Many-to-One") {
    pa_res_ls <- sapply(1:length(query_set), function(x) {
      pairwiseAlignment(ref_set, query_set[x], type = alignment_type)
    })
    sapply(pa_res_ls, function(x) {
      paste0(capture.output(writePairwiseAlignments(x, block.width = block_width)), collapse = "\n")
    }) %>% paste0(collapse = "\n\n\n")
  } else {
    stop(glue::glue("ValueError: {alignment_strategy} is not a valid alignment strategy!"))
  }
}

seq_check <- function(x, type = "DNA") {
  if (type == "DNA") {
    all(str_split(x, "")[[1]] %in% c("A", "T", "C", "G", "N"))
  } else if (type == "RNA") {
    all(str_split(x, "")[[1]] %in% c("A", "U", "C", "G", "N"))
  } else {
    stop(paste0("TypeError: unsupported type - ", type, "!"))
  }
}

seq_rev <- function(x) {
  paste0(rev(str_split(x, "")[[1]]), collapse = "")
}

seq_compm <- function(x, type = "DNA") {
  # DNA: DNA --> DNA.
  # RNA: RNA --> RNA.
  # DNA2RNA: DNA --> RNA.
  # RNA2DNA: RNA --> DNA.
  dna_mpt <- c("A" = "T", "T" = "A", "C" = "G", "G" = "C", "N" = "N")
  rna_mpt <- c("A" = "U", "U" = "A", "C" = "G", "G" = "C", "N" = "N")
  dna2rna_mpt <- c("A" = "U", "T" = "A", "C" = "G", "G" = "C", "N" = "N")
  rna2dna_mpt <- c("A" = "T", "U" = "A", "C" = "G", "G" = "C", "N" = "N")
  
  if (type == "DNA") {
    if (seq_check(x, "DNA")) {
      paste0(dna_mpt[str_split(x, "")[[1]]], collapse = "") 
    } else {
      "Error: unsupported character(s) in the given sequence!"
    }
  } else if (type == "RNA") {
    if (seq_check(x, "RNA")) {
      paste0(rna_mpt[str_split(x, "")[[1]]], collapse = "") 
    } else {
      "Error: unsupported character(s) in the given sequence!"
    }
  } else if (type == "DNA2RNA") {
    if (seq_check(x, "DNA")) {
      paste0(dna2rna_mpt[str_split(x, "")[[1]]], collapse = "") 
    } else {
      "Error: unsupported character(s) in the given sequence!"
    }
  } else if (type == "RNA2DNA") {
    if (seq_check(x, "RNA")) {
      paste0(rna2dna_mpt[str_split(x, "")[[1]]], collapse = "") 
    } else {
      "Error: unsupported character(s) in the given sequence!"
    }
  } else {
    stop(paste0("TypeError: unsupported type - ", type, "!"))
  }
}

seq_rev_compm <- function(x, type = "DNA") {
  seq_compm(seq_rev(x), type)
}
