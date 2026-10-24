#!/usr/bin/env bash

set -euo pipefail

# Overlooked Soil Engineers: Digger Bees Induce Localised Shifts
# in Soil Chemical Properties and Microbiome
#
# Ferrari A, Tiziani R, Borsotti A, Ronchetti F, Fracasso I,
# Bouaicha O, Mimmo T, Borruso L & Polidori C (2026).
# Soil Biology and Biochemistry.
# https://doi.org/10.1016/j.soilbio.2026.110341
#
# Target genes:
# - Bacterial 16S rRNA gene
# - Fungal ITS2 region

echo "Starting QIIME 2 pipeline..."

# ============================
# Bacterial 16S rRNA dataset
# ============================

echo "Processing bacterial 16S rRNA dataset..."

# Step 1. Denoising, quality filtering, chimera removal, and ASV inference with DADA2
qiime dada2 denoise-paired \
  --i-demultiplexed-seqs demux-paired-end_16S_bac.qza \
  --p-trim-left-f 21 \
  --p-trim-left-r 21 \
  --p-trunc-len-f 235 \
  --p-trunc-len-r 245 \
  --o-table table_16S_bac.qza \
  --o-representative-sequences rep-seqs_16S_bac.qza \
  --o-denoising-stats denoising-stats_16S_bac.qza \
  --p-n-reads-learn 500000 \
  --p-max-ee-f 2 \
  --p-max-ee-r 2 \
  --verbose

# Step 2. De novo clustering at 97% sequence identity with VSEARCH
qiime vsearch cluster-features-de-novo \
  --i-table table_16S_bac.qza \
  --i-sequences rep-seqs_16S_bac.qza \
  --p-perc-identity 0.97 \
  --o-clustered-table table-16S-dn-97_bac.qza \
  --o-clustered-sequences rep-seqs-16S-dn-97_bac.qza

# Step 3. Taxonomic assignment with the SILVA v138-99 classifier
qiime feature-classifier classify-sklearn \
  --i-classifier silva-v138-99-nb-classifier.qza \
  --i-reads rep-seqs-16S-dn-97_bac.qza \
  --o-classification taxonomy-dn-16S-97_bac.qza \
  --p-n-jobs -2 \
  --p-reads-per-batch 100000 \
  --verbose

# ===================
# Fungal ITS2 dataset
# ===================

echo "Processing fungal ITS2 dataset..."

# Step 1. Denoising, quality filtering, chimera removal, and ASV inference with DADA2
qiime dada2 denoise-paired \
  --i-demultiplexed-seqs demux-paired-end_ITS.qza \
  --p-trim-left-f 21 \
  --p-trim-left-r 21 \
  --p-trunc-len-f 210 \
  --p-trunc-len-r 250 \
  --o-table table_ITS.qza \
  --o-representative-sequences rep-seqs_ITS.qza \
  --o-denoising-stats denoising-stats_ITS.qza \
  --p-n-reads-learn 500000 \
  --p-max-ee-f 2 \
  --p-max-ee-r 2 \
  --verbose

# Step 2. De novo clustering at 97% sequence identity with VSEARCH
qiime vsearch cluster-features-de-novo \
  --i-table table_ITS.qza \
  --i-sequences rep-seqs_ITS.qza \
  --p-perc-identity 0.97 \
  --o-clustered-table table-ITS-dn-97.qza \
  --o-clustered-sequences rep-seqs-ITS-dn-97.qza

# Step 3. Taxonomic assignment with the UNITE classifier
qiime feature-classifier classify-sklearn \
  --i-classifier unite-INSD-v10.0-classifier.qza \
  --i-reads rep-seqs-ITS-dn-97.qza \
  --o-classification taxonomy-dn-ITS-97.qza \
  --p-n-jobs -2 \
  --p-reads-per-batch 100000 \
  --verbose

echo "Pipeline completed successfully."
