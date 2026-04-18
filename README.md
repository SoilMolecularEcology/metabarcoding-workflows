# Metabarcoding data analysis workflows

This repository provides reusable workflows for metabarcoding data analysis across multiple studies and target markers, encompassing primary sequence processing, taxonomic assignment, and downstream analytical workflows.

## General qiime2 workflow

The general workflow available in [`general_workflow/README.md`](general_workflow/README.md) refers to the core QIIME2-based amplicon processing pipeline, including denoising, feature clustering, and taxonomic assignment.

## Study-specific implementations

Study-specific workflows are organized in the [`articles/`](articles/) directory.

Each article-specific subdirectory may contain:
- `README.md`, summarizing the study reference, target marker(s), and workflow structure
- `pipeline.sh`, providing the bash workflow for primary sequence processing
- additional scripts, including `R` scripts, for downstream analyses, statistics, and visualization
