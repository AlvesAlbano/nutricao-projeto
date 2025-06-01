(ns org.nutricao-projeto.alimento.alimento-controller
  (:require [cheshire.core :as json]
            [clj-http.client :as client]
            [clojure.string :as str]
            [org.nutricao-projeto.traducao.traduzir-frase :as trad]))

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
  (println "---------------------------------------------------------")
  (println (format "%-25s %8s %10s %10s %10s %14s"
                   "Alimento" "Qtd(g)" "Calorias" "Proteína" "Gordura" "Carboidrato"))
  (println "---------------------------------------------------------")
  (letfn [(imprimir-rec [lst]
            (if (empty? lst)
              (println "---------------------------------------------------------")
              (let [r (first lst)]
                (println (format "%-25s %8.1f %10.2f %10.2f %10.2f %14.2f"
                                 (:alimento r)
                                 (:quantidade r)
                                 (:calorias r)
                                 (:proteina r)
                                 (:gordura r)
                                 (:carboidrato r)))
                (recur (rest lst)))))]
    (imprimir-rec refeicoes)))

(defn inteiro? [s]
  (boolean (re-matches #"\d+" s)))

(defn double-str? [s]
  (boolean (re-matches #"^\d+(\.\d+)?$" s)))

(defn imprimir-alimentos-indexados [alimentos idx]
  (if (empty? alimentos)
    nil
    (let [a (first alimentos)
          i idx
          linha (str (inc i) ". "
                     (try
                       (trad/ingles-portugues (:descricao-detalhada a))
                       (catch Exception _ (:descricao-detalhada a))))]
      (println linha)
      (recur (rest alimentos) (inc i)))))

(defn adicionar-refeicao [refeicoes]
  (println "Digite os alimentos consumidos separados por vírgula (ex: arroz, feijao, ovo):")
  (let [linha (read-line)
        nomes (str/split linha #",\s*")]
    (letfn [(processar [alimentos acumulado]
              (if (empty? alimentos)
                acumulado
                (let [nome (first alimentos)
                      resultado (buscar-alimento nome)
                      alimentos-filtrados (take 5 resultado)
                      traduzidos (mapv #(assoc % :descricao-pt
                                                 (try
                                                   (trad/ingles-portugues (:descricao-detalhada %))
                                                   (catch Exception _ (:descricao-detalhada %))))
                                       alimentos-filtrados)]
                  (if (empty? traduzidos)
                    (do
                      (println (str "Nenhum alimento encontrado para: " nome))
                      (recur (rest alimentos) acumulado))
                    (do
                      (println "\nSelecione o alimento correspondente:")
                      (imprimir-alimentos-indexados traduzidos 0)
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
                                                     :carboidrato (* carb-100g fator)}]
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
          descricao (try
                      (trad/ingles-portugues (:descricao-detalhada alimento))
                      (catch Exception _ (:descricao-detalhada alimento)))
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
