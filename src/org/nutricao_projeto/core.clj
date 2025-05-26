(ns org.nutricao-projeto.core
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.string :as str]))

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
  (println (format "%-10s %8s %10s %10s %10s %14s"
                   "Alimento" "Qtd(g)" "Calorias" "Proteína" "Gordura" "Carboidrato"))
  (println "---------------------------------------------------------")
  (letfn [(imprimir-rec [lst]
            (if (empty? lst)
              (println "---------------------------------------------------------")
              (do
                (let [r (first lst)]
                  (println (format "%-10s %8.1f %10.2f %10.2f %10.2f %14.2f"
                                   (:alimento r)
                                   (:quantidade r)
                                   (:calorias r)
                                   (:proteina r)
                                   (:gordura r)
                                   (:carboidrato r))))
                (recur (rest lst)))))]
    (imprimir-rec refeicoes)))

(defn inteiro? [s]
  (boolean (re-matches #"\d+" s)))

(defn double-str? [s]
  (boolean (re-matches #"^\d+(\.\d+)?$" s)))

(defn menu []
  (println "=== Menu Nutricional ===")
  (println "1. Consultar alimento")
  (println "2. Adicionar refeição")
  (println "3. Mostrar refeições do dia")
  (println "4. Sair")
  (println "Escolha uma opção:")
  (let [entrada (read-line)]
    (if (inteiro? entrada)
      (Integer/parseInt entrada)
      -1)))

(defn adicionar-refeicao [refeicoes]
  (println "Digite os alimentos consumidos separados por vírgula (ex: arroz, feijao, ovo):")
  (let [linha (read-line)
        nomes (str/split linha #",\s*")]
    (letfn [(processar [alimentos acumulado]
              (if (empty? alimentos)
                acumulado
                (let [nome (first alimentos)
                      resultado (buscar-alimento nome)
                      alimento (first resultado)
                      descricao (:description alimento)
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
                        (recur (rest alimentos) acumulado)))))))]
      (processar nomes refeicoes))))

(defn executar [refeicoes]
  (let [opcao (menu)]
    (cond
      (= opcao 1)
      (do
        (println "Digite o nome do alimento:")
        (let [nome (read-line)
              resultados (buscar-alimento nome)]
          (if (not (empty? resultados))
            (loop [lst resultados
                   count 0]
              (if (or (empty? lst) (>= count 5))
                nil
                (let [alimento (first lst)
                      descricao (:description alimento)
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
                  (recur (rest lst) (inc count)))))
            (println "Nenhum alimento encontrado.")))
        (recur refeicoes))

      (= opcao 2)
      (let [novas (adicionar-refeicao refeicoes)]
        (recur novas))

      (= opcao 3)
      (do
        (imprimir-refeicoes refeicoes)
        (recur refeicoes))

      (= opcao 4)
      (println "Saindo...")

      :else
      (do
        (println "Opção inválida.")
        (recur refeicoes)))))

(defn -main []
  (executar []))
