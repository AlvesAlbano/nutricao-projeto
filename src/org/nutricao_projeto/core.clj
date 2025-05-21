(ns org.nutricao-projeto.core
  (:require [clj-http.client :as client]
            [cheshire.core :as json]))

(def api-key "wEs6WCnpgqM3QnNWYwQKBqzjch59uWB7Nye9VT0Q")

(defn buscar-alimento [nome]
  (let [url "https://api.nal.usda.gov/fdc/v1/foods/search"
        response (client/get url {:query-params {"api_key" api-key
                                                 "query" nome}
                                  :headers {"Accept" "application/json"}
                                  :as :json})]
    (get-in response [:body :foods])))

(defn obter-nutriente [nutrientes nome]
  (if (empty? nutrientes)
    nil
    (let [n (first nutrientes)]
      (if (= nome (:nutrientName n))
        (:value n)
        (recur (rest nutrientes) nome)))))

(defn mostrar-alimentos-rec [alimentos count]
  (if (or (empty? alimentos) (>= count 5))
    nil
    (do
      (let [alimento (first alimentos)
            descricao (:description alimento)
            categoria (:foodCategory alimento)
            nutrientes (:foodNutrients alimento)
            calorias (obter-nutriente nutrientes "Energy")
            proteina (obter-nutriente nutrientes "Protein")
            gordura (obter-nutriente nutrientes "Total lipid (fat)")]
        (println "===========================")
        (println "Nome: " descricao)
        (println "Categoria: " categoria)
        (println "Calorias: " (or calorias "N/A"))
        (println "Proteína: " (or proteina "N/A"))
        (println "Gordura: " (or gordura "N/A"))
        (println "==========================="))
      (recur (rest alimentos) (inc count)))))

(defn inteiro? [s]
  (re-matches #"\d+" s))

(defn menu []
  (println "=== Menu Nutricional ===")
  (println "1. Consultar alimento")
  (println "2. Outra opção (placeholder)")
  (println "3. Sair")
  (println "Escolha uma opção:")
  (let [entrada (read-line)]
    (if (inteiro? entrada)
      (Integer/parseInt entrada)
      -1)))

(defn -main []
  (letfn [(executar []
            (let [opcao (menu)]
              (cond
                (= opcao 1) (do
                              (println "Digite o nome do alimento:")
                              (let [nome (read-line)
                                    resultados (buscar-alimento nome)]
                                (if (and resultados (not (empty? resultados)))
                                  (mostrar-alimentos-rec resultados 0)
                                  (println "Nenhum alimento encontrado.")))
                              (recur))
                (= opcao 2) (do
                              (println "Função ainda não implementada.")
                              (recur))
                (= opcao 3) (println "Saindo...")
                :else (do
                        (println "Opção inválida.")
                        (recur)))))]
    (executar)))
