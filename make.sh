#!/bin/sh
R CMD build anchor-r
R CMD INSTALL anchor_1.0.tar.gz

R CMD build anchor-r-features
R CMD INSTALL anchor.features_1.0.tar.gz

R CMD build anchor-r-nuclei
R CMD INSTALL anchor.nuclei_1.0.tar.gz