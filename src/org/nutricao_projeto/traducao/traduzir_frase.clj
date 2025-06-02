(ns org.nutricao_projeto.traducao.traduzir_frase
 (:require [cheshire.core :as json]
            [clj-http.client :as http]))

(def api-url "https://ftapi.pythonanywhere.com/")

(defn retorna-primeiro-elemento [conteudo]
  (first (get-in conteudo [:translations :possible-translations])))

(defn portugues-ingles [frase]
  (let [url-requisicao (str api-url "translate?sl=pt&dl=en&text=" frase)
        resposta (http/get url-requisicao {:accept :json})
        corpo (json/parse-string (:body resposta) true)]
    (retorna-primeiro-elemento corpo)))

(defn ingles-portugues [frase]
  (let [url-requisicao (str api-url "translate?sl=en&dl=pt&text=" frase)
        resposta (http/get url-requisicao {:accept :json})
        corpo (json/parse-string (:body resposta) true)]
    (retorna-primeiro-elemento corpo)))

(defn -main []
  (println (portugues-ingles "uma bola quadrada"))
  (println (ingles-portugues "a red dog")))
