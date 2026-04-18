# Rosy Discolouration in an Alpine Chapel: Beyond Salt Dependence

**Status:** Manuscript in preparation

## Target genes
- bacterial 16S rRNA gene
- archaeal 16S rRNA gene
- fungal ITS2 region

## Bacterial 16S rRNA dataset

### Step 1. Denoising, quality filtering, chimera removal, and ASV inference with DADA2

```bash
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
```

### Step 2. De novo clustering at 97% sequence identity with VSEARCH

```bash
qiime vsearch cluster-features-de-novo \
  --i-table table_16S_bac.qza \
  --i-sequences rep-seqs_16S_bac.qza \
  --p-perc-identity 0.97 \
  --o-clustered-table table-16S-dn-97_bac.qza \
  --o-clustered-sequences rep-seqs-16S-dn-97_bac.qza
```

### Step 3. Taxonomic assignment with the SILVA v138-99 classifier

```bash
qiime feature-classifier classify-sklearn \
  --i-classifier silva-v138-99-nb-classifier.qza \
  --i-reads rep-seqs-16S-dn-97_bac.qza \
  --o-classification taxonomy-dn-16S-97_bac.qza \
  --p-n-jobs -2 \
  --p-reads-per-batch 100000 \
  --verbose
```

## Archaeal 16S rRNA dataset

### Step 1. Denoising, quality filtering, chimera removal, and ASV inference with DADA2

```bash
qiime dada2 denoise-paired \
  --i-demultiplexed-seqs demux-paired-end_16S_arch.qza \
  --p-trim-left-f 21 \
  --p-trim-left-r 21 \
  --p-trunc-len-f 235 \
  --p-trunc-len-r 245 \
  --o-table table_16S_arch.qza \
  --o-representative-sequences rep-seqs_16S_arch.qza \
  --o-denoising-stats denoising-stats_16S_arch.qza \
  --p-n-reads-learn 50000 \
  --p-max-ee-f 2 \
  --p-max-ee-r 2 \
  --verbose
```

### Step 2. De novo clustering at 97% sequence identity with VSEARCH

```bash
qiime vsearch cluster-features-de-novo \
  --i-table table_16S_arch.qza \
  --i-sequences rep-seqs_16S_arch.qza \
  --p-perc-identity 0.97 \
  --o-clustered-table table-16S-dn-97_arch.qza \
  --o-clustered-sequences rep-seqs-16S-dn-97_arch.qza
```

### Step 3. Taxonomic assignment with the SILVA v138-99 classifier

```bash
qiime feature-classifier classify-sklearn \
  --i-classifier silva-v138-99-nb-classifier.qza \
  --i-reads rep-seqs-16S-dn-97_arch.qza \
  --o-classification taxonomy-dn-16S-97_arch.qza \
  --p-n-jobs -2 \
  --p-reads-per-batch 100000 \
  --verbose
```

## Fungal ITS2 dataset

### Step 1. Denoising, quality filtering, chimera removal, and ASV inference with DADA2

```bash
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
```

### Step 2. De novo clustering at 97% sequence identity with VSEARCH

```bash
qiime vsearch cluster-features-de-novo \
  --i-table table_ITS.qza \
  --i-sequences rep-seqs_ITS.qza \
  --p-perc-identity 0.97 \
  --o-clustered-table table-ITS-dn-97.qza \
  --o-clustered-sequences rep-seqs-ITS-dn-97.qza
```

### Step 3. Taxonomic assignment with the UNITE classifier

```bash
qiime feature-classifier classify-sklearn \
  --i-classifier unite-INSD-v10.0-classifier.qza \
  --i-reads rep-seqs-ITS-dn-97.qza \
  --o-classification taxonomy-dn-ITS-97.qza \
  --p-n-jobs -2 \
  --p-reads-per-batch 100000 \
  --verbose
```
