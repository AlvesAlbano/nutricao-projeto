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


(defn mostrar-alimentos-rec
  ([alimentos] (mostrar-alimentos-rec alimentos 1))
  ([alimentos i]
   (if (empty? alimentos)
     nil
     (let [alimento (first alimentos)
           nome (:description alimento)
           marca (or (:brandName alimento) "-")
           porcao (or (:servingSize alimento) "Porção não informada")
           categoria (or (:foodCategory alimento) "Categoria não informada")
           ingredientes (or (:ingredients alimento) "Ingredientes não informados")
           nutrientes (:foodNutrients alimento)
           calorias (or
                      (first (map :value (filter #(= "Energy" (:nutrientName %)) nutrientes)))
                      "N/A")]
       (println (format "%d. %s (Marca: %s)\n   Porção: %s | Calorias: %s kcal\n   Categoria: %s\n   Ingredientes: %s\n"
                        i nome marca porcao calorias categoria ingredientes))
       (mostrar-alimentos-rec (rest alimentos) (inc i))))))

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
                (println (format "%-25s %8.1f %10.2f %10.2f %10.2f %14.2f %20s"
                                 (:alimento r)
                                 (:quantidade r)
                                 (:calorias r)
                                 (:proteina r)
                                 (:gordura r)
                                 (:carboidrato r)
                                 (:data-hora r)))
                (recur (rest lst) (+ acc cal)))))]
    (imprimir-rec refeicoes 0.0)))

(defn double-str? [s]
  (boolean (re-matches #"^\d+(\.\d+)?$" s)))

(defn imprimir-opcoes [alimentos i]
  (if (>= i (count alimentos))
    nil
    (let [alimento (nth alimentos i)
          nome (or (:descricao-pt alimento) (:description alimento))
          marca (or (:brandName alimento) (:brand_owner alimento) "-")
          porcao (or (:servingSize alimento) "Porção não informada")
          unidade (or (:servingSizeUnit alimento) "g")
          categoria-en (or (:foodCategory alimento) "Categoria não informada")
          categoria-pt (try (trad/ingles-portugues categoria-en) (catch Exception _ categoria-en))
          nutrientes (:foodNutrients alimento)
          calorias (or
                     (first (map :value (filter #(= "Energy" (:nutrientName %)) nutrientes)))
                     "N/A")]
      (println (format "%d. %s (Marca: %s)\n   Porção: %s %s | Calorias: %s kcal\n   Categoria: %s\n"
                       (inc i) nome marca porcao unidade calorias categoria-pt))
      (recur alimentos (inc i)))))

(defn adicionar-refeicao [refeicoes]
  (println "Digite os alimentos consumidos separados por vírgula (ex: arroz, feijao, ovo):")
  (let [linha (read-line)
        nomes-pt (str/split linha #",\s*")
        nomes-en (map trad/portugues-ingles nomes-pt)]
    (letfn [(processar [alimentos nomes-pt acumulado]
              (if (empty? alimentos)
                acumulado
                (let [nome (first alimentos)
                      resultado (buscar-alimento nome)
                      alimentos-filtrados (take 5 resultado)
                      traduzidos (mapv
                                   (fn [a]
                                     (let [desc-eng (:description a)
                                           desc-pt (try
                                                     (if (and desc-eng (not (str/blank? desc-eng)))
                                                       (trad/ingles-portugues desc-eng)
                                                       "Sem descrição")
                                                     (catch Exception _ (or desc-eng "Sem descrição")))]
                                       (assoc a :descricao-pt desc-pt)))
                                   alimentos-filtrados)]
                  (if (empty? traduzidos)
                    (do
                      (println (str "Nenhum alimento encontrado para: " nome))
                      (recur (rest alimentos) (rest nomes-pt) acumulado))
                    (do
                      (println "\nSelecione o alimento correspondente:")
                      (imprimir-opcoes traduzidos 0)
                      (let [opcao-str (read-line)
                            opcao (try (Integer/parseInt opcao-str) (catch Exception _ 0))]
                        (if (or (< opcao 1) (> opcao (count traduzidos)))
                          (do
                            (println "Opção inválida. Pulando alimento.")
                            (recur (rest alimentos) (rest nomes-pt) acumulado))
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
                                  (recur (rest alimentos) (rest nomes-pt) (conj acumulado nova-refeicao)))
                                (do
                                  (println "Quantidade inválida. Pulando alimento.")
                                  (recur (rest alimentos) (rest nomes-pt) acumulado))))))))))))]
      (processar nomes-en nomes-pt refeicoes))))
