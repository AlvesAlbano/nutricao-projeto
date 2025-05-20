(ns org.nutricao-projeto.core
  (:gen-class)
  (:require [org.nutricao-projeto.teste :as teste])
  )


(defn menu[]
  (println "1. Teste")
  (println "2. Teste")
  (println "3. Sair")
  (println "Escolha um opção"))


(defn -main[]
  (menu)

  (let [opcao (read)]
    (cond
    (= opcao 1) (do
                  (teste/metodoTeste)
                  (recur)
                  )
    (= opcao 2) (do
                  (teste/metodoTeste)
                  (recur)
                  )
    (= opcao 3) (do
                  (println "saindo")
                  )
    )
  )
  )