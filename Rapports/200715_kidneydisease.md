Context
-------

Ce morceau code vient de l’excellent blog du Dr Shirin Glanders (
<a href="https://shirinsplayground.netlify.app/2017/12/lime_sketchnotes/" class="uri">https://shirinsplayground.netlify.app/2017/12/lime_sketchnotes/</a>
). L’objectif est de modéliser et extraire une interprétation
individuelle des prévisions de déficience chronique des reins.

Charger les librairies et les données
-------------------------------------

    library(tidyverse)  # for tidy data analysis
    library(farff)      # for reading arff file
    library(missForest) # for imputing missing values
    library(dummies)    # for creating dummy variables
    library(caret)      # for modeling
    library(lime)       # for explaining predictions

    library(dplyr)       # pour manipuler les objets

Il faut au choix cloner le projet avec les données correspondantes ou
copier le code et télécharger les données en
“<a href="http://archive.ics.uci.edu/ml/datasets/Chronic_Kidney_Disease" class="uri">http://archive.ics.uci.edu/ml/datasets/Chronic_Kidney_Disease</a>”.

    data_file <- here::here("data", "Chronic_Kidney_Disease_full.arff")
    data <- farff::readARFF(data_file)

Vérification de la qualitées des données, imputation
----------------------------------------------------

    data %>%
            visdat::vis_miss()

![](200715_kidneydisease_files/figure-markdown_strict/data%20miss%20viz-1.png)

Application d’une statégie de complétion des valeurs manquantes via
`missForest::missForest`. D’autres alternatives existes (e.g avec le
package `missMDA`).

    data_imp <- missForest(data)

    ##   missForest iteration 1 in progress...done!
    ##   missForest iteration 2 in progress...done!
    ##   missForest iteration 3 in progress...done!
    ##   missForest iteration 4 in progress...done!
    ##   missForest iteration 5 in progress...done!
    ##   missForest iteration 6 in progress...done!
    ##   missForest iteration 7 in progress...done!
    ##   missForest iteration 8 in progress...done!
    ##   missForest iteration 9 in progress...done!

    data_imp_final <- data_imp$ximp

Création de la matrice d’apprentissage, normalisation (ici 0-1)
---------------------------------------------------------------

    data_dummy <- dummies::dummy.data.frame(dplyr::select(data_imp_final, -class), sep = "_")
    data <- cbind(dplyr::select(data_imp_final, class),
                  scale(data_dummy,
                        center = apply(data_dummy, 2, min),
                        scale = apply(data_dummy, 2, max)
                        )
                  )

Echantillon apprentissage / test
--------------------------------

    # training and test set
    set.seed(42)
    index <- createDataPartition(data$class, p = 0.9, list = FALSE)
    train_data <- data[index, ]
    test_data  <- data[-index, ]

    # saveRDS(object = train_data, file = here::here("data", "train_shirin.rds"))
    # saveRDS(object = test_data, file = here::here("data", "test_shirin.rds"))

Entraînement
------------

    # modeling
    model_rf <- caret::train(class ~ .,
                             data = train_data,
                             method = "rf", # random forest
                             trControl = trainControl(method = "repeatedcv",
                                                      number = 10,
                                                      repeats = 5,
                                                      verboseIter = FALSE))

    model_rf

    ## Random Forest 
    ## 
    ## 360 samples
    ##  48 predictor
    ##   2 classes: 'ckd', 'notckd' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold, repeated 5 times) 
    ## Summary of sample sizes: 323, 323, 323, 325, 323, 325, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##    2    0.9860892  0.9709400
    ##   25    0.9916315  0.9825729
    ##   48    0.9882973  0.9753387
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was mtry = 25.

Interprétation globale
----------------------

    caret::varImp(model_rf)

    ## rf variable importance
    ## 
    ##   only 20 most important variables shown (out of 48)
    ## 
    ##               Overall
    ## hemo         100.0000
    ## pcv           53.0445
    ## sc            32.2548
    ## rbc_abnormal  17.6603
    ## al_0          15.8996
    ## rbc_normal    14.4861
    ## rbcc           8.4968
    ## bgr            7.4957
    ## sg_1.010       5.5563
    ## sg_1.015       3.1551
    ## pe_no          1.8537
    ## su_0           1.5541
    ## pe_yes         1.5320
    ## htn_no         1.0089
    ## dm_yes         0.7925
    ## dm_no          0.7358
    ## su_4           0.6288
    ## appet_poor     0.4040
    ## htn_yes        0.3948
    ## appet_good     0.3905

    caret::dotPlot(caret::varImp(model_rf))

![](200715_kidneydisease_files/figure-markdown_strict/global%20interpret-1.png)

    # save the model for plumber app
    # saveRDS(model_rf, here::here("model_rf_shirin.rds"))

Evaluation
----------

    # predictions
    pred <- data.frame(sample_id = 1:nrow(test_data), predict(model_rf, test_data, type = "prob"), actual = test_data$class) %>%
            mutate(prediction = colnames(.)[2:3][apply(.[, 2:3], 1, which.max)], correct = ifelse(actual == prediction, "correct", "wrong"))

    confusionMatrix(pred$actual, factor(pred$prediction) )

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction ckd notckd
    ##     ckd     25      0
    ##     notckd   0     15
    ##                                      
    ##                Accuracy : 1          
    ##                  95% CI : (0.9119, 1)
    ##     No Information Rate : 0.625      
    ##     P-Value [Acc > NIR] : 6.842e-09  
    ##                                      
    ##                   Kappa : 1          
    ##                                      
    ##  Mcnemar's Test P-Value : NA         
    ##                                      
    ##             Sensitivity : 1.000      
    ##             Specificity : 1.000      
    ##          Pos Pred Value : 1.000      
    ##          Neg Pred Value : 1.000      
    ##              Prevalence : 0.625      
    ##          Detection Rate : 0.625      
    ##    Detection Prevalence : 0.625      
    ##       Balanced Accuracy : 1.000      
    ##                                      
    ##        'Positive' Class : ckd        
    ## 

Interprétation locale
---------------------

    train_x <- dplyr::select(train_data, -class)
    test_x <- dplyr::select(test_data, -class)

    train_y <- dplyr::select(train_data, class)
    test_y <- dplyr::select(test_data, class)

    explainer <- lime(train_x, model_rf, n_bins = 5, quantile_bins = TRUE)

    explanation_df <- lime::explain(test_x, explainer, 
                                    n_labels = 1, n_features = 8, 
                                    n_permutations = 1000, 
                                    feature_select = "forward_selection",
                                    
                                    )

    explanation_df %>%
            ggplot(aes(x = model_r2, fill = label)) +
            geom_density(alpha = 0.5)

![](200715_kidneydisease_files/figure-markdown_strict/model%20local%20interpretation-1.png)

    explanation_df %>%
            # glimpse()
            # filter(case == unique(case)[1:10])
            filter(label == "notckd") %>%
            arrange(desc(model_r2)) %>%
            # view()
            # count(case)
            filter(case %in% c("258", "313", "319")) %>%
            plot_features(. , ncol = 1)

![](200715_kidneydisease_files/figure-markdown_strict/model%20local%20interpretation-2.png)

Pour aller plus loin
--------------------

-   interpreter l’influence par variables avec
    \`randomForest::partialPlot()
-   utiliser d’autres modèles
-   analyser les cas mal prédits / bien prédits (lift curve, …)
-   tester `iml` :
    <a href="https://github.com/christophM/iml" class="uri">https://github.com/christophM/iml</a>
