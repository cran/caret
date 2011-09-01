
modelLookup <- function(model = NULL)
{
  mods <- c(## ada
            'ada', 'ada', 'ada',
            ## avnnet
            'avNNet', 'avNNet', 'avNNet',               
            ## bag
            'bag', 
            ## bagEarth
            'bagEarth', 'bagEarth', 
            ## bagFDA
            'bagFDA', 'bagFDA', 
            ## blackboost
            'blackboost', 'blackboost',
            ## Boruta
            'Boruta',
            ## bstTree
            'bstTree', 'bstTree', 'bstTree',
             ## bstLs
            'bstLs', 'bstLs',
             ## bstSm
            'bstSm', 'bstSm',           
            ## cforest
            'cforest', 
            ## ctree
            'ctree', 
            ## ctree2
            'ctree2',
            ## Cubist
            'cubist', 'cubist',            
            ## earth
            'earth', 'earth', 
            ## earthTest
            'earthTest', 'earthTest', 
            ## enet
            'enet', 'enet', 
            ## fda
            'fda', 'fda', 
            ## foba
            'foba', 'foba',
            ## gam
            'gam', 'gam',
            ## gamLoess
            'gamLoess', 'gamLoess',
            ## gamSpline
            'gamSpline',
            ## gamboost
            'gamboost', 'gamboost', 
            ## GAMens
            'GAMens', 'GAMens', 'GAMens', 
            ## gaussprLinear
            'gaussprLinear', 
            ## gaussprPoly
            'gaussprPoly', 'gaussprPoly', 
            ## gaussprRadial
            'gaussprRadial', 
            ## gbm
            'gbm', 'gbm', 'gbm',
            ## gcvEarth
            'gcvEarth',
            ## glm
            'glm', 
            ## glmboost
            'glmboost', 'glmboost', 
            ## glmnet
            'glmnet', 'glmnet', 
            ## glmrob
##            'glmrob', 
            ## glmStepAIC
            'glmStepAIC', 
            ## gpls
            'gpls', 
            ## hda
            'hda', 'hda', 'hda', 
            ## hdda
            'hdda', 'hdda', 
            ## icr
            'icr', 
            ## J48
            'J48', 
            ## JRip
            'JRip', 
            ## knn
            'knn', 
            ## lars
            'lars', 
            ## lars2
            'lars2', 
            ## lasso
            'lasso', 
            ## lda
            'lda',
            ## leapForward
            'leapForward',
            ## leapBackward
            'leapBackward',
            ## leapSeq
            'leapSeq',
            ## Linda
            'Linda', 
            ## lm
            'lm', 
            ## lmStepAIC
            'lmStepAIC', 
            ## LMT
            'LMT', 
            ## logforest
            'logforest', 
            ## logicBag
            'logicBag', 'logicBag', 
            ## logitBoost
            'logitBoost', 
            ## logreg
            'logreg', 'logreg', 
            ## lssvmLinear
            'lssvmLinear', 
            ## lssvmPoly
            'lssvmPoly', 'lssvmPoly', 
            ## lssvmRadial
            'lssvmRadial', 
            ## lvq
            'lvq', 'lvq',
            ## M5
            'M5', 'M5', 'M5',             
            ## M5Rules
            'M5Rules', 'M5Rules', 
            ## mars
            'mars', 'mars', 
            ## mda
            'mda', 
            ## multinom
            'multinom', 
            ## nb
            'nb', 'nb', 
            ## neuralnet
            'neuralnet', 'neuralnet', 'neuralnet', 
            ## nnet
            'nnet', 'nnet', 
            ## nodeHarvest
            'nodeHarvest', 'nodeHarvest', 
            ## obliqueTree
            'obliqueTree', 'obliqueTree', 
            ## OneR
            'OneR', 
            ## pam
            'pam', 
            ## parRF
            'parRF', 
            ## PART
            'PART', 'PART', 
            ## partDSA
            'partDSA', 'partDSA', 
            ## pcaNNet
            'pcaNNet', 'pcaNNet', 
            ## pcr
            'pcr', 
            ## pda
            'pda', 
            ## pda2
            'pda2', 
            ## penalized
            'penalized', 'penalized', 
            ## plr
            'plr', 'plr', 
            ## pls
            'pls',
            ## pls glm binomial
##            'plsGlmBinomial',
            ## pls glm Gaussian
##            'plsGlmGaussian',
            ## pls glm Gamma
##            'plsGlmGamma',
            ## pls glm Poisson
##            'plsGlmPoisson',            
            ## plsTest
            'plsTest', 
            ## ppr
            'ppr', 
            ## qda
            'qda', 
            ## QdaCov
            'QdaCov',
            ## qrnn
            'qrnn', 'qrnn', 'qrnn',
            ## qrf
            'qrf', 
            ## rda
            'rda', 'rda', 
            ## relaxo
            'relaxo', 'relaxo', 
            ## rf
            'rf', 
            ## rfLSF
            'rfLSF', 
            ## rfNWS
            'rfNWS',
            ## ridge reg
            'ridge',
            ## rlm
            'rlm', 
            ## rocc
            'rocc', 
            ## rpart
            'rpart', 
            ## rvmLinear
            'rvmLinear', 
            ## rvmPoly
            'rvmPoly', 'rvmPoly', 
            ## rvmRadial
            'rvmRadial', 
            ## scrda
            'scrda', 'scrda', 
            ## sda
            'sda', 
            ## sddaLDA
            'sddaLDA', 
            ## sddaQDA
            'sddaQDA', 
            ## slda
            'slda', 
            ## smda
            'smda', 'smda', 'smda', 
            ## sparseLDA
            'sparseLDA', 'sparseLDA', 
            ## spls
            'spls', 'spls', 'spls', 
            ## stepLDA
            'stepLDA', 'stepLDA', 
            ## stepQDA
            'stepQDA', 'stepQDA', 
            ## superpc
            'superpc', 'superpc', 
            ## svmLinear
            'svmLinear', 
            ## svmpoly
            'svmpoly', 'svmpoly', 'svmpoly', 
            ## svmPoly
            'svmPoly', 'svmPoly', 'svmPoly', 
            ## svmradial
            'svmradial', 'svmradial', 
            ## svmRadial
            'svmRadial', 'svmRadial',
            ## svmRadialCost
            'svmRadialCost',              
            ## treebag
            'treebag', 
            ## vbmpRadial
            'vbmpRadial')

  pNames <- c(## ada
              'iter', 'maxdepth', 'nu',
              ## avnnet
              'size', 'decay', 'bag',              
              ## bag
              'vars', 
              ## bagEarth
              'nprune', 'degree', 
              ## bagFDA
              'nprune', 'degree', 
              ## blackboost
              'mstop', 'maxdepth',
              ## Boruta
              'mtry',
              ## bstTree
              'mstop', 'maxdepth', 'nu',
              ## bstLs
              'mstop', 'nu',
              ## bstSm
              'mstop', 'nu',              
              ## cforest
              'mtry', 
              ## ctree
              'mincriterion', 
              ## ctree2
              'maxdepth',
              ## Cubist
              'committees', 'neighbors', 
              ## earth
              'nprune', 'degree', 
              ## earthTest
              'nprune', 'degree', 
              ## enet
              'fraction', 'lambda', 
              ## fda
              'nprune', 'degree', 
              ## foba
              'k', 'lambda',
              ## gam
              'select', 'method',
              ## gamLoess
              'span', 'degree',
              ## gamSpline
              'df',
              ## gamboost
              'mstop', 'prune', 
              ## GAMens
              'iter', 'rsm_size', 'fusion', 
              ## gaussprLinear
              'parameter', 
              ## gaussprPoly
              'degree', 'scale', 
              ## gaussprRadial
              'sigma', 
              ## gbm
              'n.trees', 'interaction.depth', 'shrinkage',
              ## gcvEarth
              'degree',
              ## glm
              'parameter', 
              ## glmboost
              'mstop', 'prune', 
              ## glmnet
              'lambda', 'alpha', 
              ## glmrob
##              'parameter', 
              ## glmStepAIC
              'parameter', 
              ## gpls
              'K.prov', 
              ## hda
              'gamma', 'lambda', 'newdim', 
              ## hdda
              'threshold', 'model', 
              ## icr
              'n.comp', 
              ## J48
              'C', 
              ## JRip
              'NumOpt', 
              ## knn
              'k', 
              ## lars
              'fraction', 
              ## lars2
              'step', 
              ## lasso
              'fraction', 
              ## lda
              'parameter',
              ## leapForward
              'nvmax',
              ## leapBackward
              'nvmax',
              ## leapSeq
              'nvmax',
              ## Linda
              'parameter', 
              ## lm
              'parameter', 
              ## lmStepAIC
              'parameter', 
              ## LMT
              'iter', 
              ## logforest
              'parameter', 
              ## logicBag
              'nleaves', 'ntrees', 
              ## logitBoost
              'nIter', 
              ## logreg
              'treesize', 'ntrees', 
              ## lssvmLinear
              'parameter', 
              ## lssvmPoly
              'degree', 'scale', 
              ## lssvmRadial
              'sigma', 
              ## lvq
              'size', 'k',
              ## M5
              'pruned', 'smoothed', 'rules',
              ## M5Rules
              'pruned', 'smoothed', 
              ## mars
              'nprune', 'degree', 
              ## mda
              'subclasses', 
              ## multinom
              'decay', 
              ## nb
              'fL', 'usekernel', 
              ## neuralnet
              'layer1', 'layer2', 'layer3', 
              ## nnet
              'size', 'decay', 
              ## nodeHarvest
              'maxinter', 'mode', 
              ## obliqueTree
              'oblique.splits', 'variable.selection', 
              ## OneR
              'parameter', 
              ## pam
              'threshold', 
              ## parRF
              'mtry', 
              ## PART
              'threshold', 'pruned', 
              ## partDSA
              'cut.off.growth', 'MPD', 
              ## pcaNNet
              'size', 'decay', 
              ## pcr
              'ncomp', 
              ## pda
              'lambda', 
              ## pda2
              'df', 
              ## penalized
              'lambda1', 'lambda2', 
              ## plr
              'lambda', 'cp', 
              ## pls
              'ncomp',
              ## pls glm binomial
##              'nt',   
              ## pls glm Gaussian
##              'nt',   
              ## pls glm Gamma
##              'nt',   
              ## pls glm Poisson
##              'nt',   
              ## plsTest
              'ncomp', 
              ## ppr
              'nterms', 
              ## qda
              'parameter', 
              ## QdaCov
              'parameter',
              ## qrnn
              'n.hidden', 'penalty', 'bag',              
              ## qrf
              'mtry', 
              ## rda
              'gamma', 'lambda', 
              ## relaxo
              'lambda', 'phi', 
              ## rf
              'mtry', 
              ## rfLSF
              'mtry', 
              ## rfNWS
              'mtry',
              ## ridge
              'lambda',
              ## rlm
              'parameter', 
              ## rocc
              'xgenes', 
              ## rpart
              'maxdepth', 
              ## rvmLinear
              'parameter', 
              ## rvmPoly
              'degree', 'scale', 
              ## rvmRadial
              'sigma', 
              ## scrda
              'alpha', 'delta', 
              ## sda
              'diagonal', 
              ## sddaLDA
              'parameter', 
              ## sddaQDA
              'parameter', 
              ## slda
              'parameter', 
              ## smda
              'NumVars', 'R', 'lambda', 
              ## sparseLDA
              'NumVars', 'lambda', 
              ## spls
              'K', 'eta', 'kappa', 
              ## stepLDA
              'maxvar', 'direction', 
              ## stepQDA
              'maxvar', 'direction', 
              ## superpc
              'threshold', 'n.components', 
              ## svmLinear
              'C', 
              ## svmpoly
              'C', 'degree', 'scale', 
              ## svmPoly
              'C', 'degree', 'scale', 
              ## svmradial
              'C', 'sigma', 
              ## svmRadial
              'C', 'sigma',
              ## svmRadialCost
              'C',               
              ## treebag
              'parameter', 
              ## vbmpRadial
              'estimateTheta')


  pLabel <- c(## ada
              '#Trees',
              'Max Tree Depth',
              'Learning Rate',
              ## avnnet
              '#Hidden Units',
              'Weight Decay',
              'Bagging',
              ## bag
              '#Randomly Selected Predictors',
              ## bagEarth
              '#Terms',
              'Product Degree',
              ## bagFDA
              '#Terms',
              'Product Degree',
              ## blackboost
              '#Trees',
              'Max Tree Depth',
              ## Boruta
              '#Randomly Selected Predictors',
              ## bstTree
              '# Boosting Iterations', 'Max Tree Depth', 'Shrinkage',
              ## bstLs
              '# Boosting Iterations', 'Shrinkage',
              ## bstSm
              '# Boosting Iterations', 'Shrinkage',                
              ## cforest
              '#Randomly Selected Predictors',
              ## ctree
              'P-Value Threshold',
              ## ctree2
              'Max Tree Depth',
              ## Cubist
              '#Committees', '#Instances', 
              ## earth
              '#Terms',
              'Product Degree',
              ## earthTest
              '#Terms',
              'Product Degree',
              ## enet
              'Fraction of Full Solution',
              'Weight Decay',
              ## fda
              '#Terms',
              'Product Degree',
              ## foba
              '#Variables Retained',
              'L2 Penalty',
              ## gam
              'Feature Selection', 'Method',
              ## gamLoess
              'Span', 'Degree',
              ## gamSpline
              'Degrees of Freedom',
              ## gamboost
              '# Boosting Iterations',
              'AIC Prune?',
              ## GAMens
              'Ensemble Size',
              '#Random Feature Subsets',
              'Data Fusion Function',
              ## gaussprLinear
              'none',
              ## gaussprPoly
              'Polynomial Degree',
              'Scale',
              ## gaussprRadial
              'Sigma',
              ## gbm
              '#Trees',
              'Interaction Depth',
              'Learning Rate',
              ## gcvEarth
              '#Terms',
              ## glm
              'none',
              ## glmboost
              '# Boosting Iterations',
              'AIC Prune?',
              ## glmnet
              'Regularization Parameter',
              'Mixing Percentage',
              ## glmrob
##              'none',
              ## glmStepAIC
              'none',
              ## gpls
              '#Components',
              ## hda
              'Gamma',
              'Lambda',
              'Dimension of the Discriminative Subspace',
              ## hdda
              'Threshold',
              'Model Type',
              ## icr
              '#Components',
              ## J48
              'Confidence Threshold',
              ## JRip
              '# Optimizations',
              ## knn
              '#Neighbors',
              ## lars
              'Fraction',
              ## lars2
              '#Steps',
              ## lasso
              'Fraction of Full Solution',
              ## lda
              'none',
              ## leapForward
              'Maximum Size of Subsets',
              ## leapBackward
              'Maximum Size of Subsets',
              ## leapSeq
              'Maximum Size of Subsets',
              ## Linda
              'none',
              ## lm
              'none',
              ## lmStepAIC
              'none',
              ## LMT
              '# Iteratons',
              ## logforest
              'none',
              ## logicBag
              'Maximum Number of Leaves',
              'Number of Trees',
              ## logitBoost
              '# Boosting Iterations',
              ## logreg
              'Maximum Number of Leaves',
              'Number of Trees',
              ## lssvmLinear
              'none',
              ## lssvmPoly
              'Polynomial Degree',
              'Scale',
              ## lssvmRadial
              'Sigma',
              ## lvq
              'Codebook Size', '#Prototypes',
              ## M5
              'Pruned', 'Smoothed', 'Rules',
              ## M5Rules
              'Pruned', 'Smoothed',
              ## mars
              '#Terms',
              'Product Degree',
              ## mda
              '#Subclasses Per Class',
              ## multinom
              'Weight Decay',
              ## nb
              'Laplace Correction', 'Distribution Type',
              ## neuralnet
              '#Hidden Units in Layer 1',
              '#Hidden Units in Layer 2',
              '#Hidden Units in Layer 3',
              ## nnet
              '#Hidden Units',
              'Weight Decay',
              ## nodeHarvest
              'Maximum Interaction Depth',
              'Prediction Mode',
              ## obliqueTree
              'Oblique Splits',
              'Variable Selection Method',
              ## OneR
              'none',
              ## pam
              'Shrinkage Threshold',
              ## parRF
              '#Randomly Selected Predictors',
              ## PART
              'Confidence Threshold',
              'Pruning',
              ## partDSA
              'Number of Terminal Partitions',
              'Minimum Percent Difference',
              ## pcaNNet
              '#Hidden Units',
              'Weight Decay',
              ## pcr
              '#Components',
              ## pda
              'Shrinkage Penalty Coefficient',
              ## pda2
              'Degrees of Freedom',
              ## penalized
              'L1 Penalty',
              'L2 Penalty',
              ## plr
              'L2 Penalty',
              'Complexity Parameter',
              ## pls
              '#Components',
              ## pls glm binomial
##              '#Components',
              ## pls glm Gaussian
##              '#Components',
              ## pls glm Gamma
##              '#Components',
              ## pls glm Poisson
##              '#Components',      
              ## plsTest
              '#Components',
              ## ppr
              '# Terms',
              ## qda
              'none',
              ## QdaCov
              'none',
              ## qrnn
              '#Hidden Units', ' Weight Decay', 'Bagged Models?',                     
              ## qrf
              '#Randomly Selected Predictors',
              ## rda
              'Gamma',
              'Lambda',
              ## relaxo
              'Penalty Parameter',
              'Relaxation Parameter',
              ## rf
              '#Randomly Selected Predictors',
              ## rfLSF
              '#Randomly Selected Predictors',
              ## rfNWS
              '#Randomly Selected Predictors',
              ## ridge
              'Penalty',
              ## rlm
              'none',
              ## rocc
              '#Variables Retained',
              ## rpart
              'Max Tree Depth',
              ## rvmLinear
              'none',
              ## rvmPoly
              'Polynomial Degree',
              'Scale',
              ## rvmRadial
              'Sigma',
              ## scrda
              'Regularization Value',
              'Threshold',
              ## sda
              'Diagonalize',
              ## sddaLDA
              'none',
              ## sddaQDA
              'none',
              ## slda
              'none',
              ## smda
              '# Predictors',
              '# Subclasses',
              'Lambda',
              ## sparseLDA
              '# Predictors',
              'Lambda',
              ## spls
              '#Components',
              'Threshold',
              'Kappa',
              ## stepLDA
              'Maximum #Variables',
              'Search Direction',
              ## stepQDA
              'Maximum #Variables',
              'Search Direction',
              ## superpc
              'Threshold',
              '#Components',
              ## svmLinear
              'C',
              ## svmpoly
              'Cost',
              'Polynomial Degree',
              'Scale',
              ## svmPoly
              'Cost',
              'Polynomial Degree',
              'Scale',
              ## svmradial
              'Cost',
              'Sigma',
              ## svmRadial
              'Cost',
              'Sigma',
              ## svmRadialCost
              'Cost',
              ## treebag
              'none',
              ## vbmpRadial
              'Theta Estimated')

  isSeq <- c(## ada
             FALSE, FALSE, FALSE,
             ## avnnet
             FALSE, FALSE, FALSE,             
             ## bag
             FALSE, 
             ## bagEarth
             FALSE, FALSE, 
             ## bagFDA
             FALSE, FALSE, 
             ## blackboost
             TRUE, FALSE,
             ## Boruta
             FALSE,
             ## bstTree
             TRUE, FALSE, FALSE,
             ## bstLm
             TRUE, FALSE,
             ## bstSm
             TRUE, FALSE,             
             ## cforest
             FALSE, 
             ## ctree actually is a sequential model, but treeresponse can't do probs from multiple models
             FALSE, 
             ## ctree2
             FALSE,
             ## Cubist
             FALSE, TRUE, 
             ## earth
             TRUE, FALSE, 
             ## earthTest
             FALSE, FALSE, 
             ## enet
             TRUE, FALSE, 
             ## fda
             FALSE, FALSE, 
             ## foba
             TRUE, FALSE,
             ## gam
             FALSE, FALSE,
             ## gamLoess
             FALSE, FALSE,
             ## gamSpline
             FALSE,
             ## gamboost
             TRUE, FALSE, 
             ## GAMens
             FALSE, FALSE, FALSE, 
             ## gaussprLinear
             FALSE, 
             ## gaussprPoly
             FALSE, FALSE, 
             ## gaussprRadial
             FALSE, 
             ## gbm
             TRUE, FALSE, FALSE,
             ## gcvEarth
             FALSE,
             ## glm
             FALSE, 
             ## glmboost
             TRUE, FALSE, 
             ## glmnet
             TRUE, FALSE, 
             ## glmrob
##             FALSE, 
             ## glmStepAIC
             FALSE, 
             ## gpls
             FALSE, 
             ## hda
             FALSE, FALSE, FALSE, 
             ## hdda
             FALSE, FALSE, 
             ## icr
             FALSE, 
             ## J48
             FALSE, 
             ## JRip
             FALSE, 
             ## knn
             FALSE, 
             ## lars
             TRUE, 
             ## lars2
             TRUE, 
             ## lasso
             TRUE, 
             ## lda
             FALSE,
             ## leapForward
             TRUE,
             ## leapBackward
             TRUE,
             ## leapSeq
             TRUE,
             ## Linda
             FALSE, 
             ## lm
             FALSE, 
             ## lmStepAIC
             FALSE, 
             ## LMT
             FALSE, 
             ## logforest
             FALSE, 
             ## logicBag
             FALSE, FALSE, 
             ## logitBoost
             TRUE, 
             ## logreg
             FALSE, FALSE, 
             ## lssvmLinear
             FALSE, 
             ## lssvmPoly
             FALSE, FALSE, 
             ## lssvmRadial
             FALSE, 
             ## lvq
             FALSE, FALSE,
             ## M5
             FALSE, FALSE, FALSE,           
             ## M5Rules
             FALSE, FALSE, 
             ## mars
             FALSE, FALSE, 
             ## mda
             FALSE, 
             ## multinom
             FALSE, 
             ## nb
             FALSE, FALSE, 
             ## neuralnet
             FALSE, FALSE, FALSE, 
             ## nnet
             FALSE, FALSE, 
             ## nodeHarvest
             FALSE, FALSE, 
             ## obliqueTree
             FALSE, FALSE, 
             ## OneR
             FALSE, 
             ## pam
             TRUE, 
             ## parRF
             FALSE, 
             ## PART
             FALSE, FALSE, 
             ## partDSA
             TRUE, FALSE, 
             ## pcaNNet
             FALSE, FALSE, 
             ## pcr
             TRUE, 
             ## pda
             FALSE, 
             ## pda2
             FALSE, 
             ## penalized
             FALSE, FALSE, 
             ## plr
             FALSE, FALSE, 
             ## pls
             TRUE,
             ## pls glm binomial
##             FALSE,
             ## pls glm Gaussian
##             FALSE,
             ## pls glm Gamma
##             FALSE,
             ## pls glm Poisson
##             FALSE,                
             ## plsTest
             FALSE, 
             ## ppr
             FALSE, 
             ## qda
             FALSE, 
             ## QdaCov
             FALSE,
             ## qrnn
             FALSE, FALSE, FALSE,                 
             ## qrf
             FALSE, 
             ## rda
             FALSE, FALSE, 
             ## relaxo
             TRUE, FALSE, 
             ## rf
             FALSE, 
             ## rfLSF
             FALSE, 
             ## rfNWS
             FALSE,
             ## ridge
             FALSE,
             ## rlm
             FALSE, 
             ## rocc
             FALSE, 
             ## rpart
             TRUE, 
             ## rvmLinear
             FALSE, 
             ## rvmPoly
             FALSE, FALSE, 
             ## rvmRadial
             FALSE, 
             ## scrda
             TRUE, TRUE, 
             ## sda
             FALSE, 
             ## sddaLDA
             FALSE, 
             ## sddaQDA
             FALSE, 
             ## slda
             FALSE, 
             ## smda
             FALSE, FALSE, FALSE, 
             ## sparseLDA
             FALSE, FALSE, 
             ## spls
             FALSE, FALSE, FALSE, 
             ## stepLDA
             FALSE, FALSE, 
             ## stepQDA
             FALSE, FALSE, 
             ## superpc
             TRUE, TRUE, 
             ## svmLinear
             FALSE, 
             ## svmpoly
             FALSE, FALSE, FALSE, 
             ## svmPoly
             FALSE, FALSE, FALSE, 
             ## svmradial
             FALSE, FALSE, 
             ## svmRadial
             FALSE, FALSE,
             ## svmRadialCost
             FALSE,               
             ## treebag
             FALSE, 
             ## vbmpRadial
             FALSE)
  isRegMod <- c(## ada
                FALSE, FALSE, FALSE,
                ## avnnet
                TRUE, TRUE, TRUE,                 
                ## bag
                TRUE, 
                ## bagEarth
                TRUE, TRUE, 
                ## bagFDA
                FALSE, FALSE, 
                ## blackboost
                TRUE, TRUE,
                ## Boruta
                TRUE,
                ## bstTree
                TRUE, TRUE, TRUE,
                ## bstLm
                TRUE, TRUE,
                ## bstSm
                TRUE, TRUE,                
                ## cforest
                TRUE, 
                ## ctree
                TRUE, 
                ## ctree2
                TRUE,
                ## Cubist
                TRUE, TRUE,
                ## earth
                TRUE, TRUE, 
                ## earthTest
                TRUE, TRUE, 
                ## enet
                TRUE, TRUE, 
                ## fda
                FALSE, FALSE, 
                ## foba
                TRUE, TRUE,
                ## gam
                TRUE, TRUE,
                ## gamLoess
                TRUE, TRUE,
                ## gamSpline
                TRUE,
                ## gamboost
                TRUE, TRUE, 
                ## GAMens
                FALSE, FALSE, FALSE, 
                ## gaussprLinear
                TRUE, 
                ## gaussprPoly
                TRUE, TRUE, 
                ## gaussprRadial
                TRUE, 
                ## gbm
                TRUE, TRUE, TRUE,
                ## gcvEarth
                TRUE,
                ## glm
                TRUE, 
                ## glmboost
                TRUE, TRUE, 
                ## glmnet
                TRUE, TRUE, 
                ## glmrob
##                TRUE, 
                ## glmStepAIC
                TRUE, 
                ## gpls
                FALSE, 
                ## hda
                FALSE, FALSE, FALSE, 
                ## hdda
                FALSE, FALSE, 
                ## icr
                TRUE, 
                ## J48
                FALSE, 
                ## JRip
                FALSE, 
                ## knn
                TRUE, 
                ## lars
                TRUE, 
                ## lars2
                TRUE, 
                ## lasso
                TRUE, 
                ## lda
                FALSE,
                ## leapForward
                TRUE,
                ## leapBackward
                TRUE,
                ## leapSeq
                TRUE,
                ## Linda
                FALSE, 
                ## lm
                TRUE, 
                ## lmStepAIC
                TRUE, 
                ## LMT
                FALSE, 
                ## logforest
                FALSE, 
                ## logicBag
                TRUE, TRUE, 
                ## logitBoost
                FALSE, 
                ## logreg
                TRUE, TRUE, 
                ## lssvmLinear
                FALSE, 
                ## lssvmPoly
                FALSE, FALSE, 
                ## lssvmRadial
                FALSE, 
                ## lvq
                FALSE, FALSE,
                ## M5
                TRUE, TRUE, TRUE,                
                ## M5Rules
                TRUE, TRUE,
                ## mars
                TRUE, TRUE, 
                ## mda
                FALSE, 
                ## multinom
                FALSE, 
                ## nb
                FALSE, FALSE, 
                ## neuralnet
                TRUE, TRUE, TRUE, 
                ## nnet
                TRUE, TRUE, 
                ## nodeHarvest
                TRUE, TRUE, 
                ## obliqueTree
                FALSE, FALSE, 
                ## OneR
                FALSE, 
                ## pam
                FALSE, 
                ## parRF
                TRUE, 
                ## PART
                FALSE, FALSE, 
                ## partDSA
                TRUE, TRUE, 
                ## pcaNNet
                TRUE, TRUE, 
                ## pcr
                TRUE, 
                ## pda
                FALSE, 
                ## pda2
                FALSE, 
                ## penalized
                TRUE, TRUE, 
                ## plr
                FALSE, FALSE, 
                ## pls
                TRUE,
                ## pls glm binomial
##                FALSE,
                ## pls glm Gaussian
##                TRUE,
                ## pls glm Gamma
##                TRUE,
                ## pls glm Poisson
##                TRUE,                          
                ## plsTest
                TRUE, 
                ## ppr
                TRUE, 
                ## qda
                FALSE, 
                ## QdaCov
                FALSE,
                ## qrnn
                TRUE, TRUE, TRUE,                
                ## qrf
                TRUE, 
                ## rda
                FALSE, FALSE, 
                ## relaxo
                TRUE, TRUE, 
                ## rf
                TRUE, 
                ## rfLSF
                TRUE, 
                ## rfNWS
                TRUE,
                ## ridge
                TRUE,
                ## rlm
                TRUE, 
                ## rocc
                FALSE, 
                ## rpart
                TRUE, 
                ## rvmLinear
                TRUE, 
                ## rvmPoly
                TRUE, TRUE, 
                ## rvmRadial
                TRUE, 
                ## scrda
                FALSE, FALSE, 
                ## sda
                FALSE, 
                ## sddaLDA
                FALSE, 
                ## sddaQDA
                FALSE, 
                ## slda
                FALSE, 
                ## smda
                FALSE, FALSE, FALSE, 
                ## sparseLDA
                FALSE, FALSE, 
                ## spls
                TRUE, TRUE, TRUE, 
                ## stepLDA
                FALSE, FALSE, 
                ## stepQDA
                FALSE, FALSE, 
                ## superpc
                TRUE, TRUE, 
                ## svmLinear
                TRUE, 
                ## svmpoly
                TRUE, TRUE, TRUE, 
                ## svmPoly
                TRUE, TRUE, TRUE, 
                ## svmradial
                TRUE, TRUE, 
                ## svmRadial
                TRUE, TRUE, 
                ## svmRadialCost
                TRUE,
                ## treebag
                TRUE, 
                ## vbmpRadial
                FALSE)

  isClassMod <- c(## ada
                  TRUE, TRUE, TRUE,
                  ## avnnet
                  TRUE, TRUE, TRUE,                  
                  ## bag
                  TRUE, 
                  ## bagEarth
                  TRUE, TRUE, 
                  ## bagFDA
                  TRUE, TRUE, 
                  ## blackboost
                  TRUE, TRUE,
                  ## Boruta
                  TRUE,
                  ## bstTree
                  TRUE, TRUE, TRUE,
                  ## bstLm
                  TRUE, TRUE,
                  ## bstSm
                  TRUE, TRUE,                  
                  ## cforest
                  TRUE, 
                  ## ctree
                  TRUE, 
                  ## ctree2
                  TRUE,
                  ## Cubist
                  FALSE, FALSE,
                  ## earth
                  TRUE, TRUE, 
                  ## earthTest
                  FALSE, FALSE, 
                  ## enet
                  FALSE, FALSE, 
                  ## fda
                  TRUE, TRUE, 
                  ## foba
                  FALSE, FALSE,
                  ## gam
                  TRUE, TRUE,
                  ## gamLoess
                  TRUE, TRUE,
                  ## gamSpline
                  TRUE,
                  ## gamboost
                  TRUE, TRUE, 
                  ## GAMens
                  TRUE, TRUE, TRUE, 
                  ## gaussprLinear
                  TRUE, 
                  ## gaussprPoly
                  TRUE, TRUE, 
                  ## gaussprRadial
                  TRUE, 
                  ## gbm
                  TRUE, TRUE, TRUE,
                  ## gcvEarth
                  TRUE,
                  ## glm
                  TRUE, 
                  ## glmboost
                  TRUE, TRUE, 
                  ## glmnet
                  TRUE, TRUE, 
                  ## glmrob
##                  TRUE, 
                  ## glmStepAIC
                  TRUE, 
                  ## gpls
                  TRUE, 
                  ## hda
                  TRUE, TRUE, TRUE, 
                  ## hdda
                  TRUE, TRUE, 
                  ## icr
                  FALSE, 
                  ## J48
                  TRUE, 
                  ## JRip
                  TRUE, 
                  ## knn
                  TRUE, 
                  ## lars
                  FALSE, 
                  ## lars2
                  FALSE, 
                  ## lasso
                  FALSE, 
                  ## lda
                  TRUE,
                  ## leapForward
                  FALSE,
                  ## leapBackward
                  FALSE,
                  ## leapSeq
                  FALSE,
                  ## Linda
                  TRUE, 
                  ## lm
                  FALSE, 
                  ## lmStepAIC
                  FALSE, 
                  ## LMT
                  TRUE, 
                  ## logforest
                  TRUE, 
                  ## logicBag
                  TRUE, TRUE, 
                  ## logitBoost
                  TRUE, 
                  ## logreg
                  TRUE, TRUE, 
                  ## lssvmLinear
                  TRUE, 
                  ## lssvmPoly
                  TRUE, TRUE, 
                  ## lssvmRadial
                  TRUE, 
                  ## lvq
                  TRUE, TRUE,
                  ## M5
                  FALSE, FALSE, FALSE,                 
                  ## M5Rules
                  FALSE, FALSE,
                  ## mars
                  FALSE, FALSE, 
                  ## mda
                  TRUE, 
                  ## multinom
                  TRUE, 
                  ## nb
                  TRUE, TRUE,
                  ## neuralnet
                  FALSE, FALSE, FALSE, 
                  ## nnet
                  TRUE, TRUE, 
                  ## nodeHarvest
                  TRUE, TRUE, 
                  ## obliqueTree
                  TRUE, TRUE, 
                  ## OneR
                  TRUE, 
                  ## pam
                  TRUE, 
                  ## parRF
                  TRUE, 
                  ## PART
                  TRUE, TRUE, 
                  ## partDSA
                  TRUE, TRUE, 
                  ## pcaNNet
                  TRUE, TRUE, 
                  ## pcr
                  FALSE, 
                  ## pda
                  TRUE, 
                  ## pda2
                  TRUE, 
                  ## penalized
                  FALSE, FALSE, 
                  ## plr
                  TRUE, TRUE, 
                  ## pls
                  TRUE,
                  ## pls glm binomial
##                  TRUE,
                  ## pls glm Gaussian
##                  FALSE,
                  ## pls glm Gamma
##                  FALSE,
                  ## pls glm Poisson
##                  FALSE,                          
                  ## plsTest
                  TRUE, 
                  ## ppr
                  FALSE, 
                  ## qda
                  TRUE, 
                  ## QdaCov
                  TRUE,
                  ## qrnn
                  FALSE, FALSE, FALSE,                  
                  ## qrf
                  FALSE, 
                  ## rda
                  TRUE, TRUE, 
                  ## relaxo
                  FALSE, FALSE, 
                  ## rf
                  TRUE, 
                  ## rfLSF
                  TRUE, 
                  ## rfNWS
                  TRUE,
                  ## ridge
                  FALSE,
                  ## rlm
                  FALSE, 
                  ## rocc
                  TRUE, 
                  ## rpart
                  TRUE, 
                  ## rvmLinear
                  FALSE, 
                  ## rvmPoly
                  FALSE, FALSE, 
                  ## rvmRadial
                  FALSE, 
                  ## scrda
                  TRUE, TRUE, 
                  ## sda
                  TRUE, 
                  ## sddaLDA
                  TRUE, 
                  ## sddaQDA
                  TRUE, 
                  ## slda
                  TRUE, 
                  ## smda
                  TRUE, TRUE, TRUE, 
                  ## sparseLDA
                  TRUE, TRUE, 
                  ## spls
                  TRUE, TRUE, TRUE, 
                  ## stepLDA
                  TRUE, TRUE, 
                  ## stepQDA
                  TRUE, TRUE, 
                  ## superpc
                  FALSE, FALSE, 
                  ## svmLinear
                  TRUE, 
                  ## svmpoly
                  TRUE, TRUE, TRUE, 
                  ## svmPoly
                  TRUE, TRUE, TRUE, 
                  ## svmradial
                  TRUE, TRUE, 
                  ## svmRadial
                  TRUE, TRUE, 
                  ## svmRadialCost
                  TRUE,
                  ## treebag
                  TRUE, 
                  ## vbmpRadial
                  TRUE)

  hasProbModel <- c(## ada
                    TRUE, TRUE, TRUE,
                    ## avnnet
                    TRUE, TRUE, TRUE,                     
                    ## bag
                    TRUE, 
                    ## bagEarth
                    TRUE, TRUE, 
                    ## bagFDA
                    TRUE, TRUE, 
                    ## blackboost
                    TRUE, TRUE,
                    ## Boruta
                    TRUE,
                    ## bstTree
                    FALSE, FALSE, FALSE,
                    ## bstLs
                    FALSE, FALSE,
                    ## bstSm
                    FALSE, FALSE,                    
                    ## cforest
                    TRUE, 
                    ## ctree
                    TRUE, 
                    ## ctree2
                    TRUE,
                    ## Cubist
                    FALSE, FALSE,
                    ## earth
                    TRUE, TRUE, 
                    ## earthTest
                    FALSE, FALSE, 
                    ## enet
                    FALSE, FALSE, 
                    ## fda
                    TRUE, TRUE, 
                    ## foba
                    FALSE, FALSE,
                    ## gam
                    TRUE, TRUE,
                    ## gamLoess
                    TRUE, TRUE,
                    ## gamSpline
                    TRUE,
                    ## gamboost
                    TRUE, TRUE, 
                    ## GAMens
                    TRUE, TRUE, TRUE, 
                    ## gaussprLinear
                    TRUE, 
                    ## gaussprPoly
                    TRUE, TRUE, 
                    ## gaussprRadial
                    TRUE, 
                    ## gbm
                    TRUE, TRUE, TRUE,
                    ## gcvEarth
                    TRUE,
                    ## glm
                    TRUE, 
                    ## glmboost
                    TRUE, TRUE, 
                    ## glmnet
                    TRUE, TRUE, 
                    ## glmrob
##                    TRUE, 
                    ## glmStepAIC
                    TRUE, 
                    ## gpls
                    TRUE, 
                    ## hda
                    TRUE, TRUE, TRUE, 
                    ## hdda
                    TRUE, TRUE, 
                    ## icr
                    FALSE, 
                    ## J48
                    TRUE, 
                    ## JRip
                    TRUE, 
                    ## knn
                    TRUE, 
                    ## lars
                    FALSE, 
                    ## lars2
                    FALSE, 
                    ## lasso
                    FALSE, 
                    ## lda
                    TRUE,
                    ## leapForward
                    FALSE,
                    ## leapBackward
                    FALSE,
                    ## leapSeq
                    FALSE,
                    ## Linda
                    TRUE, 
                    ## lm
                    FALSE, 
                    ## lmStepAIC
                    FALSE, 
                    ## LMT
                    TRUE, 
                    ## logforest
                    TRUE, 
                    ## logicBag
                    TRUE, TRUE, 
                    ## logitBoost
                    TRUE, 
                    ## logreg
                    TRUE, TRUE, 
                    ## lssvmLinear
                    FALSE, 
                    ## lssvmPoly
                    FALSE, FALSE, 
                    ## lssvmRadial
                    FALSE, 
                    ## lvq
                    FALSE, FALSE,
                    ## M5
                    FALSE, FALSE, FALSE,                    
                    ## M5Rules
                    FALSE, FALSE,
                    ## mars
                    FALSE, FALSE, 
                    ## mda
                    TRUE, 
                    ## multinom
                    TRUE, 
                    ## nb
                    TRUE, TRUE,
                    ## neuralnet
                    FALSE, FALSE, FALSE, 
                    ## nnet
                    TRUE, TRUE, 
                    ## nodeHarvest
                    TRUE, TRUE, 
                    ## obliqueTree
                    TRUE, TRUE, 
                    ## OneR
                    TRUE, 
                    ## pam
                    TRUE, 
                    ## parRF
                    TRUE, 
                    ## PART
                    TRUE, TRUE, 
                    ## partDSA
                    FALSE, FALSE, 
                    ## pcaNNet
                    TRUE, TRUE, 
                    ## pcr
                    FALSE, 
                    ## pda
                    TRUE, 
                    ## pda2
                    TRUE, 
                    ## penalized
                    FALSE, FALSE, 
                    ## plr
                    TRUE, TRUE, 
                    ## pls
                    TRUE,
                    ## pls glm binomial
##                    TRUE,
                    ## pls glm Gaussian
##                    FALSE,
                    ## pls glm Gamma
##                    FALSE,
                    ## pls glm Poisson
##                    FALSE,                        
                    ## plsTest
                    TRUE, 
                    ## ppr
                    FALSE, 
                    ## qda
                    TRUE, 
                    ## QdaCov
                    TRUE,
                    ## qrnn
                    FALSE, FALSE, FALSE,                        
                    ## qrf
                    FALSE, 
                    ## rda
                    TRUE, TRUE, 
                    ## relaxo
                    FALSE, FALSE, 
                    ## rf
                    TRUE, 
                    ## rfLSF
                    TRUE, 
                    ## rfNWS
                    TRUE,
                    ## ridge
                    FALSE,
                    ## rlm
                    FALSE, 
                    ## rocc
                    FALSE, 
                    ## rpart
                    TRUE, 
                    ## rvmLinear
                    FALSE, 
                    ## rvmPoly
                    FALSE, FALSE, 
                    ## rvmRadial
                    FALSE, 
                    ## scrda
                    TRUE, TRUE, 
                    ## sda
                    TRUE, 
                    ## sddaLDA
                    TRUE, 
                    ## sddaQDA
                    TRUE, 
                    ## slda
                    TRUE, 
                    ## smda
                    FALSE, FALSE, FALSE, 
                    ## sparseLDA
                    TRUE, TRUE, 
                    ## spls
                    TRUE, TRUE, TRUE, 
                    ## stepLDA
                    TRUE, TRUE, 
                    ## stepQDA
                    TRUE, TRUE, 
                    ## superpc
                    FALSE, FALSE, 
                    ## svmLinear
                    TRUE, 
                    ## svmpoly
                    TRUE, TRUE, TRUE, 
                    ## svmPoly
                    TRUE, TRUE, TRUE, 
                    ## svmradial
                    TRUE, TRUE, 
                    ## svmRadial
                    TRUE, TRUE, 
                    ## svmRadialCost
                    TRUE,
                    ## treebag
                    TRUE, 
                    ## vbmpRadial
                    TRUE)
  
  
  paramKey <- data.frame(model = mods,
                         parameter = pNames,
                         label = pLabel,
                         seq = isSeq,
                         forReg = isRegMod,
                         forClass = isClassMod,
                         probModel = hasProbModel,
                         stringsAsFactors  = FALSE)         
  
  if(!is.null(model))
    {
      if(!any(model == paramKey$model)) stop("value of model unknown")
      out <- paramKey[paramKey$model == model,]
      
    } else out <- paramKey        
  out     

}

if(FALSE)
  {
    ## Some code to clean up the modelLookup data when needed
    
    tt <- caret:::modelLookup()

    tt <- subset(tt, !(model %in% c("plsTest", "earthTest")))

    tt2 <- split(tt, tt$model)

    tt3 <- lapply(tt2,
                  function(x, ind)
                  {
                    mod <- as.character(x$model[1])
                    var <- if(is.logical(x[,ind])) as.character(x[,ind]) else paste('\'', x[,ind], '\'', sep = "")
                    if(names(x)[ind] == "label")
                      {
                        var <- paste(paste(var, ",\n", sep = ""), collapse = "")
                        cat(paste("## ", mod, "\n", var, "" , sep = ""))
                      } else {
                        var <- paste(paste(var, ", ", sep = ""), collapse = "")
                        cat(paste("\n## ", mod, "\n", var, "" , sep = ""))
                      }
                    invisible(var)
                    
                  },
                  ind = 7)

  }
