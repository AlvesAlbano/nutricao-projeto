(ns org.nutricao-projeto.alimento.alimento-controller
  (:require [cheshire.core :as json]
            [clj-http.client :as client]
            [clojure.string]
            [org.nutricao-projeto.traducao.traduzir-frase :as trad]
            )
  (:import (java.time LocalDateTime)
           (java.time.format DateTimeFormatter))
  )

(defn agora-formatado []
  (let [agora (LocalDateTime/now)
        formatador (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")]
    (.format agora formatador)))

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

(defn imprimir-refeicoes [refeicoes]
  (println "----------------------------------------------------------------------------------------------------------")
  (println (format "%-25s %8s %10s %10s %10s %14s %20s"
                   "Alimento" "Qtd(g)" "Calorias" "Proteína" "Gordura" "Carboidrato" "Data/Hora"))
  (println "----------------------------------------------------------------------------------------------------------")
  (letfn [(imprimir-rec [lst acc]
            (if (empty? lst)
              (do
                (println "----------------------------------------------------------------------------------------------------------")
                (println (format "Total de calorias consumidas no dia: %.2f kcal" acc)))
              (let [r (first lst)
                    cal (:calorias r)]
                (do
                  (println (format "%-25s %8.1f %10.2f %10.2f %10.2f %14.2f %20s"
                                   (:alimento r)
                                   (:quantidade r)
                                   (:calorias r)
                                   (:proteina r)
                                   (:gordura r)
                                   (:carboidrato r)
                                   (:data-hora r)))
                  (recur (rest lst) (+ acc cal))))))
          ]
    (imprimir-rec refeicoes 0.0)))

(defn inteiro? [s]
  (boolean (re-matches #"\d+" s)))

(defn double-str? [s]
  (boolean (re-matches #"^\d+(\.\d+)?$" s)))

(defn imprimir-opcoes [traduzidos i]
  (if (>= i (count traduzidos))
    nil
    (do
      (let [alimento (nth traduzidos i)]
        (println (str (+ i 1) ". " (:descricao-pt alimento))))
      (recur traduzidos (+ i 1)))))

(defn adicionar-refeicao [refeicoes]
  (println "Digite os alimentos consumidos separados por vírgula (ex: arroz, feijao, ovo):")
  (let [linha (read-line)
        nomes-pt (clojure.string/split linha #",\s*")
        nomes-en (map traduzir-pt-en nomes-pt)]
    (letfn [(processar [alimentos nomes-pt acumulado]
              (if (empty? alimentos)
                acumulado
                (let [nome (first alimentos)
                      resultado (buscar-alimento nome)
                      alimentos-filtrados (take 5 resultado)
                      traduzidos (mapv (fn [a]
                                         (assoc a :descricao-pt
                                                  (try
                                                    (trad/ingles-portugues (:descricao-detalhada a))
                                                    (catch Exception _ (:descricao-detalhada a)))))
                                       alimentos-filtrados)]
                  (if (empty? traduzidos)
                    (do
                      (println (str "Nenhum alimento encontrado para: " nome))
                      (recur (rest alimentos) acumulado))
                    (do
                      (println "\nSelecione o alimento correspondente:")
                      (imprimir-opcoes traduzidos 0)
                      (let [opcao-str (read-line)
                            opcao (try (Integer/parseInt opcao-str) (catch Exception _ 0))]
                        (if (or (< opcao 1) (> opcao (count traduzidos)))
                          (do
                            (println "Opção inválida. Pulando alimento.")
                            (recur (rest alimentos) acumulado))
                          (let [alimento (nth traduzidos (dec opcao))
                                descricao (:descricao-pt alimento)
                                nutrientes (:foodNutrients alimento)
                                cal-100g (obter-nutriente nutrientes "Energy")
                                prot-100g (obter-nutriente nutrientes "Protein")
                                gord-100g (obter-nutriente nutrientes "Total lipid (fat)")
                                carb-100g (obter-nutriente nutrientes "Carbohydrate, by difference")]
                            (println (str "Digite a quantidade consumida em gramas de " descricao ":"))
                            (let [qtd-str (read-line)]
                              (if (double-str? qtd-str)
                                (let [qtd (Double/parseDouble qtd-str)
                                      fator (/ qtd 100.0)
                                      nova-refeicao {:alimento descricao
                                                     :quantidade qtd
                                                     :calorias (* cal-100g fator)
                                                     :proteina (* prot-100g fator)
                                                     :gordura  (* gord-100g fator)
                                                     :carboidrato (* carb-100g fator)
                                                     :data-hora (agora-formatado)}]
                                  (println (str "Adicionado: " descricao " (" qtd "g)"))
                                  (enviar-refeicao nova-refeicao)
                                  (recur (rest alimentos) (conj acumulado nova-refeicao)))
                                (do
                                  (println "Quantidade inválida. Pulando alimento.")
                                  (recur (rest alimentos) acumulado))))))))))))]
      (processar nomes refeicoes))))

(defn mostrar-alimentos-rec [alimentos count]
  (if (or (empty? alimentos) (>= count 5))
    nil
    (let [alimento (first alimentos)
          descricao (:descricao-pt alimento)
          categoria (:foodCategory alimento)
          nutrientes (:foodNutrients alimento)
          calorias (obter-nutriente nutrientes "Energy")
          proteina (obter-nutriente nutrientes "Protein")
          gordura (obter-nutriente nutrientes "Total lipid (fat)")]
      (println "===========================")
      (println "Nome: " descricao)
      (println "Categoria: " categoria)
      (println "Calorias: " calorias)
      (println "Proteína: " proteina)
      (println "Gordura: " gordura)
      (println "===========================")
      (recur (rest alimentos) (inc count)))))
