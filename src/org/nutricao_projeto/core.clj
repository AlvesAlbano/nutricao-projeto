(ns org.nutricao-projeto.core
  (:require [org.nutricao-projeto.exercicio.exercicio-controller :as exercicio])
  )

(defn menu[]
  (println "1. Procurar exercicio")
  (println "9. Sair")
  (println "Escolha uma opção")
  )

(defn -main []
  ;(executar [])
  (menu)
  (let [opcao (read)]

    (cond
      (= opcao 1) (do
                    (println "Diga um exercicio: skiing")
                    (let [nome-exercicio (read)]
                      (println (exercicio/listar-exercicios nome-exercicio))
                      (println "Escolha um exercicio:")
                      ;(println (type (exercicio/buscar-exercicio "skiing")))
                      (recur)
                      )
                    )

      (= opcao 9) (println "encerrando")
      )
    ))