(ns org.nutricao_projeto.exercicio.exercicio_operacoes
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [org.nutricao_projeto.traducao.traduzir_frase :as traduzir]))

(def URL "http://localhost:3000/")

(defn formatar-corpo [corpo]
  {:nome (traduzir/ingles-portugues (:name corpo))
   :calorias-por-hora (:calories_per_hour corpo)
   :duracao-minutos (:duration_minutes corpo)
   :total-calorias (:total_calories corpo)}
  )

(defn formatar-perda-lista [lista-perda]
  (let [nome (:nome lista-perda)
        calorias-por-hora (:calorias-por-hora lista-perda)
        duracao-minutos (:duracao-minutos lista-perda)
        total-calorias (:total-calorias lista-perda)
        data (:data lista-perda)]

    (format "%s | Cal/H %d | Total Calorias %d | Duração %d | Data %s \n" nome calorias-por-hora total-calorias duracao-minutos data)
    )
  )

(defn indexar-lista-perda [lista-perda]

  )

(defn adicionar-data [exercicio data]
  (assoc exercicio :data data)
  )

(defn listar-exercicios[nome-exercicio peso duracao]
  (let [nome-traduzido (traduzir/portugues-ingles nome-exercicio)
        resposta (http/get (str URL "exercicio") {
                                :headers {"Accept" "application/json"}
                                          :query-params {:activity nome-traduzido
                                                         :weight peso
                                                         :duration duracao}
                                :as :json})
        corpo (:body resposta)]
    (mapv formatar-corpo corpo)
    )
  )

(defn indexar-exercicio [indice exercicio]
  (let [nome-exercicio (:nome exercicio)
        calorias-por-hora (:calorias-por-hora exercicio)
        total-calorias (:total-calorias exercicio)]
    (format "%d - %s | Cal/H %d | Total Calorias %d \n" indice nome-exercicio calorias-por-hora total-calorias)
    )
  )

 (defn enumerar-exercicios [lista-exercicios]
    (map-indexed indexar-exercicio lista-exercicios)
   )

(defn selecionar-exercicio [lista-exercicios indice]
  (get lista-exercicios indice)
  )

(defn registrar-perda [exercicio]
  (http/post "http://localhost:3000/registrar-perda"
             {:headers {"Content-Type" "application/json"
                        "Accept" "application/json"}
              :body (json/generate-string exercicio)
              :as      :json})
  )


(defn calorias-perdidas []
  (let [resposta (http/get "http://localhost:3000/calorias-perdidas" {:headers {"Accept" "application/json"}
                                                  :as :json})
    corpo (:body resposta)]

    (mapv formatar-perda-lista corpo)
    )
  )