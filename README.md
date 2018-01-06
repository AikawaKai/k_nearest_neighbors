#dataset
adult.csv: dataset originale
parsed2.csv: si tratta del dataset privo di normalizzazione
parsed_normalized.csv: il dataset con la standardizzazione

# Contenuto
- test_.r : nested cross-validation di K-NN base
- test_parallel.r : versione in parallelo per la nested cross-validation di K-NN base
- test_sequential.r : versione in parallelo per la nested cross-validation di K-NN online
- pca_analysis.r : permette di eseguire la pca sul dataset selezionato.
- knn_library.r : libreria che contiene le funzioni necessarie a K-NN e alla cross-validation.
- search.cpp : implementazione in c++ di K-NN utilizzata nella knn_library
- knnByCaret.r : versione di caret per la valutazione di K-NN con nested cross-validation.
- parsercsv.py : parser in python per creazione delle variabili dummy
- normalize_csv.py: script in python per la normalizzazione delle feature 

## test_parallel.r
Per eseguire la nested cross-validation per KNN va modificata il path della source della libreria con il proprio path (linea 1 di test_parallel.r)

## test_sequential.r
Per eseguire la nested cross-validation per KNN va modificata il path della source della libreria con il proprio path (linea 1 di test_sequential.r)

