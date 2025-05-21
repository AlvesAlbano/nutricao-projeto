(ns org.nutricao-projeto.core
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.string :as str]))

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
      (println "===========================")
      (recur (rest alimentos) (inc count)))))

(defn inteiro? [s]
  (boolean (re-matches #"\d+" s)))

(defn double-str? [s]
  (boolean (re-matches #"^\d+(\.\d+)?$" s)))

(defn menu []
  (println "=== Menu Nutricional ===")
  (println "1. Consultar alimento")
  (println "2. Adicionar refeição")
  (println "3. Sair")
  (println "Escolha uma opção:")
  (let [entrada (read-line)]
    (if (inteiro? entrada)
      (Integer/parseInt entrada)
      -1)))

(defn adicionar-refeicao [refeicoes]
  (println "Digite os alimentos consumidos, separados por vírgula (ex: rice, eggs, meat, pasta):")
  (let [linha (read-line)
        nomes (str/split linha #",\s*")]
    (letfn [(processar [restantes refeicoes-atualizadas]
              (if (empty? restantes)
                (let [total-cal (reduce + (map :calorias refeicoes-atualizadas))
                      total-prot (reduce + (map :proteina refeicoes-atualizadas))
                      total-gord (reduce + (map :gordura refeicoes-atualizadas))
                      total-carb (reduce + (map :carboidrato refeicoes-atualizadas))]
                  (println (format "Total consumido hoje: %.2f kcal | %.2f g proteínas | %.2f g gorduras | %.2f g carboidratos"
                                   total-cal total-prot total-gord total-carb))
                  refeicoes-atualizadas)
                (let [nome (first restantes)
                      resultados (buscar-alimento nome)]
                  (if (and resultados (not (empty? resultados)))
                    (let [alimento (first resultados)
                          descricao (:description alimento)
                          nutrientes (:foodNutrients alimento)
                          cal-100g (obter-nutriente nutrientes "Energy")
                          prot-100g (obter-nutriente nutrientes "Protein")
                          gord-100g (obter-nutriente nutrientes "Total lipid (fat)")
                          carb-100g (obter-nutriente nutrientes "Carbohydrate, by difference")]
                      (if cal-100g
                        (do
                          (println (str "Digite a quantidade consumida em gramas de " descricao ":"))
                          (let [qtd-str (read-line)]
                            (if (double-str? qtd-str)
                              (let [qtd (Double/parseDouble qtd-str)
                                    cal (* cal-100g (/ qtd 100))
                                    prot (* (or prot-100g 0) (/ qtd 100))
                                    gord (* (or gord-100g 0) (/ qtd 100))
                                    carb (* (or carb-100g 0) (/ qtd 100))
                                    nova-ref {:alimento descricao
                                              :quantidade qtd
                                              :calorias cal
                                              :proteina prot
                                              :gordura gord
                                              :carboidrato carb}]
                                (println (format "Adicionado: %s - %.2f g | %.2f kcal | %.2f g proteína | %.2f g gordura | %.2f g carboidrato"
                                                 descricao qtd cal prot gord carb))
                                (recur (rest restantes) (conj refeicoes-atualizadas nova-ref)))
                              (do
                                (println "Quantidade inválida.")
                                (recur (rest restantes) refeicoes-atualizadas)))))
                        (do
                          (println "Não foi possível obter as calorias.")
                          (recur (rest restantes) refeicoes-atualizadas))))
                    (do
                      (println (str "Alimento não encontrado: " nome))
                      (recur (rest restantes) refeicoes-atualizadas))))))]

      (processar nomes refeicoes))))


(defn executar [refeicoes]
  (let [opcao (menu)]
    (cond
      (= opcao 1)
      (do
        (println "Digite o nome do alimento:")
        (let [nome (read-line)
              resultados (buscar-alimento nome)]
          (if (and resultados (not (empty? resultados)))
            (mostrar-alimentos-rec resultados 0)
            (println "Nenhum alimento encontrado.")))
        (recur refeicoes))

      (= opcao 2)
      (let [novas-refeicoes (adicionar-refeicao refeicoes)]
        (recur novas-refeicoes))

      (= opcao 3)
      (println "Saindo...")

      :else
      (do
        (println "Opção inválida.")
        (recur refeicoes)))))

(defn -main []
  (executar []))