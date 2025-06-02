(ns org.nutricao-projeto.usuario.usuario-operacoes
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [org.nutricao-projeto.data :as data]))

(defn cadastrar-usuario [usuario]
  (http/post "http://localhost:3000/cadastrar-usuario"
               {:headers {"Content-Type" "application/json"
                          "Accept" "application/json"}
                :body (json/generate-string usuario)
                :as      :json})
  )

(defn get-usuario []
  (let [resposta (http/get (str data/URL "/usuario")
                           {:headers {"Accept" "application/json"}
                            :as :json})
        corpo (:body resposta)]
    corpo
  )
  )

(defn get-peso[]
  (:peso (get-usuario))
  )