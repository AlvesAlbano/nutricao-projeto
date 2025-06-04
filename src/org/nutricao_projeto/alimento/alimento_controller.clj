(ns org.nutricao_projeto.alimento.alimento_controller
  (:require [cheshire.core :as json]
            [clj-http.client :as client]
            [clojure.string :as str]
            [org.nutricao_projeto.traducao.traduzir_frase :as trad])
  (:import (java.time LocalDateTime)
           (java.time.format DateTimeFormatter)))

(defn agora-formatado []
  (let [agora (LocalDateTime/now)
        formatador (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")]
    (.format agora formatador)))

(defn inteiro? [s]
  (try
    (Integer/parseInt s)
    true
    (catch Exception _ false)))

(defn buscar-alimento [nome]
  (let [url (str "http://localhost:3000/alimento/" nome)
        response (client/get url {:headers {"Accept" "application/json"}
                                  :as :json})]
    (:body response)))

(defn enviar-refeicao [refeicao]
  (client/post "http://localhost:3000/refeicoes"
               {:headers {"Content-Type" "application/json"
                          "Accept" "application/json"}
                :body (json/generate-string refeicao)
                :as :json}))

(defn obter-nutriente [nutrientes nome]
  (if (empty? nutrientes)
    0
    (let [n (first nutrientes)]
      (if (= nome (:nutrientName n))
        (:value n)
        (recur (rest nutrientes) nome)))))

(defn double-str? [s]
  (boolean (re-matches #"^\d+(\.\d+)?$" s)))

(defn traduzir-e-filtrar-alimentos [nome-portugues]
  (let [nome-ingles (trad/portugues-ingles nome-portugues)
        resultados (buscar-alimento nome-ingles)
        alimentos-filtrados (take 5 resultados)]
    (mapv
      (fn [a]
        (let [desc-eng (:description a)
              desc-pt (try
                        (if (and desc-eng (not (str/blank? desc-eng)))
                          (trad/ingles-portugues desc-eng)
                          "Sem descrição")
                        (catch Exception _ (or desc-eng "Sem descrição")))]
          (assoc a :descricao-pt desc-pt)))
      alimentos-filtrados)))

(defn montar-refeicao [selecionado qtd]
  (let [descricao (:descricao-pt selecionado)
        nutrientes (:foodNutrients selecionado)
        cal-100g (obter-nutriente nutrientes "Energy")
        prot-100g (obter-nutriente nutrientes "Protein")
        gord-100g (obter-nutriente nutrientes "Total lipid (fat)")
        carb-100g (obter-nutriente nutrientes "Carbohydrate, by difference")
        fator (/ qtd 100.0)]
    {:alimento descricao
     :quantidade qtd
     :calorias (* cal-100g fator)
     :proteina (* prot-100g fator)
     :gordura  (* gord-100g fator)
     :carboidrato (* carb-100g fator)
     :data-hora (agora-formatado)}))
