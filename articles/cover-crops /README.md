# Legume and grass cover crops: Implications for pedofauna diversity, and soil chemical and physical characteristics

**Status:** Manuscript in preparation

## Overview
This repository contains the data and analytical workflow used to investigate the effects of legume- and grass-dominated cover crops on soil pedofauna diversity and soil chemical and physical characteristics in an apple orchard in Northern Italy. The study focuses on how different cover crop types influence the composition and richness of soil animal communities, assessed through eDNA metabarcoding, and whether these biological patterns are associated with changes in soil chemistry and structure.

## Authors
Ferrari A, Tiziani R, Fracasso I, Bouaicha O, Foley L, Taskin E, Mimmo T and Borruso L

## Target genes
- animal COI gene

## COI dataset (soil animals)

### Step 1. Denoising, quality filtering, chimera removal, and ASV inference with DADA2

```bash
qiime dada2 denoise-paired \
  --i-demultiplexed-seqs demux-paired-end_COI.qza \
  --p-trim-left-f 20 \
  --p-trim-left-r 20 \
  --p-trunc-len-f 235 \
  --p-trunc-len-r 245 \
  --o-table table_COI.qza \
  --o-representative-sequences rep-seqs_COI.qza \
  --o-denoising-stats denoising-stats_COI.qza \
  --verbose
```

### Step 2. De novo clustering at 97% sequence identity with VSEARCH

```bash
qiime vsearch cluster-features-de-novo \
  --i-table table_COI.qza \
  --i-sequences rep-seqs_COI.qza \
  --p-perc-identity 0.97 \
  --o-clustered-table table-COI-dn-97.qza \
  --o-clustered-sequences rep-seqs-COI-dn-97.qza
```

### Step 3. Taxonomic assignment with the BOLD classifier

```bash
qiime feature-classifier classify-sklearn \
  --i-classifier BOLD-classifier.qza \
  --i-reads rep-seqs-COI-dn-97.qza \
  --o-classification taxonomy-dn-COI-97.qza \
  --p-n-jobs -2 \
  --p-reads-per-batch 100000 \
  --verbose
```

