library(mcpredomics)
library(predomics)
# load the data

chemin_du_dataset <- system.file("data", "mc.input.Rda", package = "mcpredomics")
experiences <- system.file("data", "Unique_Predomics_aggregation_ova.rda", package = "mcpredomics")
load(chemin_du_dataset)

clf <- terBeam_mc(sparsity = c(2,3,4),
                  max.nb.features = 1000,
                  seed = 1,
                  nCores = 1,
                  evalToFit = "accuracy_",
                  objective = "auc",
                  experiment.id = "terBeam_mc",
                  experiment.save = "nothing")
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

approch ="ova"

test_that('function getSign_mc',
          {
            coeffss <- getSign_mc(X = X, y = y, clf = clf, parallel.local = FALSE, approch = approch)
            expect_length(coeffss, 4)
            expect_length(coeffss[[1]], 3463)

          }
)

experiences

