#
#  Copyright (C) 2019 F. Hoffmann-La Roche Ltd
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

library(R6)

#
# An Anchor-Root: a file-system location to and from anchor reads and writes its files
#
AnchorRoot <- R6Class("AnchorRoot",
    public = list(
      root = NULL,
      subRoot = NULL,
      
      initialize = function(root, subRoot) {
        self$root <- root
        self$subRoot <- subRoot
      },
      
      #
      # resolves a relativePath against the AnchorRoot (ignores subRoot)
      #
      # Args:
      #   relativePath: a path relative to the anchor-root
      #
      # returns:
      #   a resolved file-path
      #
      path = function(relativePath) {
        file.path(self$root,relativePath);
      },
      
      #
      # returns the resolved path to a file produced in an experiment
      #
      # Args:
      #   experimentType: the identifier given to the category of experiments e.g. calculateDNAGradient
      #   experimentName: the particular experiment-name (dataset-name)  e.g. EXPERIMENTNAME_1.0
      #   experimentVersion: the version associated with the experimentName e.g. 1.0
      #   relativePathExperiment: a relative-path that makes sense associated with the experiment-directory
      #   category: a top-level folder indicating the output e.g. experiments (usually this), annotations, images etc.
      #
      # returns:
      #   a resolved file-path
      #
      pathExp = function( experimentType, experimentName, experimentVersion, relativePathExperiment, category="experiments" ) {
        experimentIdentifier = paste(experimentName, "_", experimentVersion, sep="");
        relPathRoot = file.path(category, self$subRoot, experimentType, experimentIdentifier, relativePathExperiment);
        self$path(relPathRoot);
      },
      
      
      #
      # returns the resolved path to a file produced in an experiment
      #
      # Args:
      #   experimentType: the identifier given to the category of experiments e.g. calculateDNAGradient
      #   experimentName: the particular experiment-name (dataset-name)  e.g. EXPERIMENTNAME_1.0
      #   experimentVersion: the version associated with the experimentName e.g. 1.0
      #   relativePathExperiment: a relative-path that makes sense associated with the experiment-directory
      #   category: a top-level folder indicating the output e.g. experiments (usually this), annotations, images etc.
      #   createDir: iff TRUE, the directory is created if it doesn't exist.
      #
      # returns:
      #   a resolved file-path to a directory
      #
      pathDir = function( experimentType, experimentName, version, category="experiments", createDir=FALSE ) {
        path = self$pathExp(experimentType, experimentName, version, '/', category);
        dir.create(file.path(path), showWarnings=FALSE, recursive=TRUE)
        path
      }
    )
)