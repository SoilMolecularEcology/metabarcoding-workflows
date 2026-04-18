# General QIIME2 workflow for amplicon sequencing data analysis

This document provides a general QIIME2 workflow for the analysis of amplicon sequencing datasets, including bacterial and archaeal 16S rRNA, fungal ITS, and animal COI markers.  
Study-specific adaptations may involve different target markers, denoising parameters, reference databases, taxonomic classifiers, and output file names.

## Step 1. Denoising, quality filtering, chimera removal, and ASV inference with DADA2

```bash
# Denoise paired-end reads and infer ASVs
qiime dada2 denoise-paired \
  --i-demultiplexed-seqs <demux-paired-end.qza> \
  --p-trim-left-f <trim-left-f> \
  --p-trim-left-r <trim-left-r> \
  --p-trunc-len-f <trunc-len-f> \
  --p-trunc-len-r <trunc-len-r> \
  --o-table <table.qza> \
  --o-representative-sequences <rep-seqs.qza> \
  --o-denoising-stats <denoising-stats.qza> \
  --p-n-reads-learn <n-reads-learn> \
  --p-max-ee-f <max-ee-f> \
  --p-max-ee-r <max-ee-r> \
  --verbose
```

## Step 2. De novo clustering at 97% sequence identity with VSEARCH

```bash
# Cluster denoised features at 97% sequence identity
qiime vsearch cluster-features-de-novo \
  --i-table <table.qza> \
  --i-sequences <rep-seqs.qza> \
  --p-perc-identity 0.97 \
  --o-clustered-table <table-dn-97.qza> \
  --o-clustered-sequences <rep-seqs-dn-97.qza>
```

## Step 3. Taxonomic assignment with a reference classifier or database

```bash
# Assign taxonomy to clustered representative sequences
qiime feature-classifier classify-sklearn \
  --i-classifier <reference-classifier.qza> \
  --i-reads <rep-seqs-dn-97.qza> \
  --o-classification <taxonomy-dn-97.qza> \
  --p-n-jobs -2 \
  --p-reads-per-batch 100000 \
  --verbose
```

## Notes

This workflow is intended as a general template. Depending on the study design and target marker, parameters and reference resources should be adapted accordingly.
