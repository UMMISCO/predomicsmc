library(mcpredomics)
library(predomics)
# load the data
data("mc.input")
clf <- terga1_ovo(nCores = 1,
                  seed = 1,
                  plot = TRUE
)
#data("mc.input")
yvec <- mc.input$y$Enterotype[match(colnames(mc.input$X), rownames(mc.input$y))]
#Divide the dataset into training and testing
proportion_test <- 0.2
taille_test <- round(proportion_test * ncol(mc.input$X))
indices_test <- sample(1:ncol(mc.input$X), taille_test)
X <- mc.input$X[,-indices_test]
X.test <- mc.input$X[,indices_test]
y <- as.vector(yvec[-indices_test])
y.test <- as.vector(yvec[indices_test])



test_that('function getSign_ovo',
          {
            coeffs <- getSign_ovo(X = X, y = y, clf = clf, parallel.local = FALSE)
            expect_length(coeffs, 6)
            expect_length(coeffs[[5]], 3463)

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


            mod.res <- evaluateModel_ovo(mod = pop_last.mod[[1]] ,
                                        X = X,
                                         y = y,
                                         clf = clf,
                                         eval.all = FALSE,
                                         force.re.evaluation = FALSE,
                                         estim.feat.importance = FALSE,
                                        mode = "train")
            pop.last.eval <- evaluatePopulation_ovo(X , y, clf, pop_last.mod, force.re.evaluation = TRUE, eval.all = TRUE)


            expect_length(pop, 100)
            expect_length(pop_last, 6)
            expect_length(pop_last.mod, 100)
            expect_length(mod.res, 25)
            expect_length(pop.last.eval, 100)





          }
)
