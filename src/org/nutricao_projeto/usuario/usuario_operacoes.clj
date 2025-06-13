(ns org.nutricao_projeto.usuario.usuario_operacoes
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [public.URL :as resource]))

(defn cadastrar-usuario [usuario]
  (http/post (str resource/URL "cadastrar-usuario")
               {:headers {"Content-Type" "application/json"
                          "Accept" "application/json"}
                :body (json/generate-string usuario)
                :as      :json})
  )

(defn get-usuario []
  (let [resposta (http/get (str resource/URL "usuario")
                           {:headers {"Accept" "application/json"}
                            :as :json})
        corpo (:body resposta)]
    corpo
  )
  )

(defn get-peso[]
  (double (:peso (get-usuario)))
  )
