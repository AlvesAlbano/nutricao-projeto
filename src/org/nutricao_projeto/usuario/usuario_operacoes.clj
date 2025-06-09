(ns org.nutricao_projeto.usuario.usuario_operacoes
  (:require [cheshire.core :as json]
            [clj-http.client :as http]))

(defn cadastrar-usuario [usuario]
  (http/post "http://localhost:3000/cadastrar-usuario"
               {:headers {"Content-Type" "application/json"
                          "Accept" "application/json"}
                :body (json/generate-string usuario)
                :as      :json})
  )

(defn get-usuario []
  (let [resposta (http/get (str "http://localhost:3000" "/usuario")
                           {:headers {"Accept" "application/json"}
                            :as :json})
        corpo (:body resposta)]
    corpo
  )
  )

(defn get-peso[]
  (double (:peso (get-usuario)))
  )
