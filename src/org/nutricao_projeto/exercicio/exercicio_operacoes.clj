(ns org.nutricao_projeto.exercicio.exercicio_operacoes
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [public.URL :as resource]))

(defn formatar-perda-lista [lista-perda]
  (let [nome (:nome lista-perda)
        calorias-por-hora (:calorias-por-hora lista-perda)
        duracao-minutos (:duracao-minutos lista-perda)
        total-calorias (:total-calorias lista-perda)
        data (:data lista-perda)]

    (format "%s | Cal/H %d | Total Calorias %d | Duração %d min | Data %s \n" nome calorias-por-hora total-calorias duracao-minutos data)
    )
  )

(defn lista-calorias-perdidas[]
  (let [resposta (http/get (str resource/URL "calorias-perdidas") {:headers {"Accept" "application/json"}
                                                                      :as :json})
        corpo (:body resposta)]
    corpo
    )
  )

(defn adicionar-data [exercicio data]
  (assoc exercicio :data data)
  )

(defn total-calorias-perdidas
  ([]
  (let [lista-calorias-perdidas (lista-calorias-perdidas)]
    (reduce + 0 (map :total-calorias lista-calorias-perdidas))
    )
   )

  ([lista-filtrada]
   (reduce + 0 (map :total-calorias lista-filtrada))
   )
  )

(defn listar-exercicios[nome-exercicio peso duracao]
  (let [resposta (http/get (str resource/URL "exercicio") {
                                :headers {"Accept" "application/json"}
                                          :query-params {:activity nome-exercicio
                                                         :weight peso
                                                         :duration duracao}
                                :as :json})
        corpo (:body resposta)]
    corpo
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
  (http/post (str resource/URL "registrar-perda")
             {:headers {"Content-Type" "application/json"
                        "Accept" "application/json"}
              :body (json/generate-string exercicio)
              :as      :json})
  )

(defn imprimir-calorias-perdidas
  ([]
  (mapv #(print (formatar-perda-lista %)) (lista-calorias-perdidas)))

  ([lista]
   (mapv #(print (formatar-perda-lista %)) lista))
  )