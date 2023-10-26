library(mcpredomics)
library(predomics)
library(ggplot2)
library(gridExtra)
library(pROC)
library(reshape2)

setwd("~/Documents/multiclasse_predomics/mcpredomics/data")
load(file = "mc.input.rda")
mcinput <- mc.input
str(mcinput, max.level = 1)
# Filter the non informative variables
X <- mcinput$X; y <- mcinput$y # set global variables
X <- X[rowSums(X)!=0,]; dim(X) # filter out variables with only zero values
X <- filterNoSignal(X = X, side = 1, threshold = "auto", verbose = FALSE); dim(X)

clf <- terga1_ovo(nCores = 1,
                  seed = 1,
                  plot = TRUE
)
printy(clf) # print the object for more information
isClf(clf)  # test whether the object is a classifier
class(clf)  # the class of the classifier object
y <-  y[, 1]
y <- as.vector(y)

test_that('function getSign_ovo',
          {
            coeffs <- getSign_ovo(X = X, y = y, clf = clf, parallel.local = FALSE)
            expect_length(coeffs, 6)
            expect_length(coeffs[[5]], 3389)

          }
)



test_that('functions listOfSparseVecToListOfModels_ovo and evolve_ovo ',
          {
            pop         <- population(clf = clf,
                                      size_ind = 1,
                                      size_world = nrow(X),
                                      best_ancestor = NULL,
                                      size_pop = clf$params$size_pop,
                                      seed = clf$params$current_seed)
            pop_last      <- evolve_ovo(X, y, clf, pop, seed = clf$params$current_seed)
            pop_last.mod <- listOfSparseVecToListOfModels_ovo(X, y , clf = clf, v = pop_last[[1]])
            mod.res <- evaluateModel_ovo(mod = pop_last.mod[[1]],
                                         X = X,
                                         y = y,
                                         clf = clf,
                                         eval.all = FALSE,
                                         force.re.evaluation = FALSE,
                                         estim.feat.importance = FALSE,
                                         mode = "train")
            pop.last.eval <- evaluatePopulation_ovo(X , y, clf, pop_last.mod, force.re.evaluation = TRUE, eval.all = TRUE)
            individual_ovo <- individual_ovo(X, y, clf, ind  = pop_last[[1]],  eval.all = eval.all, obj = obj)

            expect_length(pop, 100)
            expect_length(pop_last, 6)
            expect_length(pop_last.mod, 100)
            expect_length(mod.res, 25)
            expect_length(pop.last.eval, 100)
            expect_length(individual_ovo, 6)




          }
)
