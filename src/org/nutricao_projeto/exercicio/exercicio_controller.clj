(ns org.nutricao-projeto.exercicio.exercicio-controller
  (:require [clj-http.client :as http]
            [cheshire.core :as json])
  )
;{
; "name": "Skiing, water skiing",
; "calories_per_hour": 354,
; "duration_minutes": 60,
; "total_calories": 354
; },
(def api-url "http://localhost:3000/exercicio/")

(defn enumerar-lista [lista]
  (mapv (fn [[indice item]]
          ;(str (inc indice) ". " (:name item) "\n")
          (format "%d. %s - %d CAL\n" (inc indice) (:name item) (:total_calories item))
          )
        (map-indexed vector lista)
        )
  )

(defn listar-exercicios[nome-exercicio]
  (let [url-requisicao (str api-url nome-exercicio)
        resposta (http/get url-requisicao {:accept :json})
        corpo (json/parse-string (:body resposta) true)]

    (if (and (= 200 (:status resposta)) (not-empty corpo))
      (enumerar-lista corpo)
       ;corpo
      "Exercicio Não Encontrado")
    ))

