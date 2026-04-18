# Barrier or bridge? Testing buffer zones in mitigating agrochemical spread and soil biodiversity shifts in Isabela Island (Galápagos)

Riviere S, Bouaicha O, Fracasso I, Trevisan F, Keiblinger K, Gerzabek MH, Jäger H, Chaves JA, Cesco S, Zehetner F, Mimmo T, Borruso L (2026). *Agriculture, Ecosystems and Environment* **405**: 110372.  
https://doi.org/10.1016/j.agee.2026.110372

## Target gene
- COI

## COI dataset

### Step 1. Denoising, quality filtering, chimera removal, and ASV inference with DADA2

```bash
qiime dada2 denoise-paired \
  --i-demultiplexed-seqs demux-paired-end_COI.qza \
  --p-trim-left-f 20 \
  --p-trim-left-r 20 \
  --p-trunc-len-f 235 \
  --p-trunc-len-r 245 \
  --o-table table_COI.qza \
  --o-representative-sequences rep-seqs_COI.qza
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

### Step 3. Taxonomic assignment with a BOLD-based reference classifier

```bash
qiime feature-classifier classify-sklearn \
  --i-classifier BOLD-classifier.qza \
  --i-reads rep-seqs-COI-dn-97.qza \
  --o-classification taxonomy-dn-COI-97.qza \
  --p-n-jobs -2 \
  --p-reads-per-batch 100000 \
  --verbose
```
