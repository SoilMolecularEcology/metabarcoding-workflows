#!/usr/bin/env bash
set -euo pipefail


# Target gene:
# - Animal COI gene

echo "Starting QIIME 2 pipeline..."

# ==========================
# COI dataset (soil animals)
# ==========================

echo "Processing COI dataset..."

# Step 1. Denoising, quality filtering, chimera removal, and ASV inference with DADA2
qiime dada2 denoise-paired \
  --i-demultiplexed-seqs demux-paired-end_COI.qza \
  --p-trim-left-f 20 \
  --p-trim-left-r 20 \
  --p-trunc-len-f 200 \
  --p-trunc-len-r 200 \
  --o-table table_COI.qza \
  --o-representative-sequences rep-seqs_COI.qza \
  --o-denoising-stats denoising-stats_COI.qza \
  --verbose

# Step 2. De novo clustering at 97% sequence identity with VSEARCH
qiime vsearch cluster-features-de-novo \
  --i-table table_COI.qza \
  --i-sequences rep-seqs_COI.qza \
  --p-perc-identity 0.97 \
  --o-clustered-table table-COI-dn-97.qza \
  --o-clustered-sequences rep-seqs-COI-dn-97.qza

# Step 3. Taxonomic assignment with the BOLD classifier
qiime feature-classifier classify-sklearn \
  --i-classifier BOLD-classifier.qza \
  --i-reads rep-seqs-COI-dn-97.qza \
  --o-classification taxonomy-dn-COI-97.qza \
  --p-n-jobs -2 \
  --p-reads-per-batch 100000 \
  --verbose

echo "Pipeline completed successfully."


