(ns org.nutricao_projeto.core
  (:require [org.nutricao_projeto.alimento.alimento_controller :as alimento]
            [org.nutricao_projeto.usuario.usuario_operacoes :as usuario]
            [org.nutricao_projeto.traducao.traduzir_frase :as trad]
            [org.nutricao_projeto.saldo_calorico :as saldo]
            [org.nutricao_projeto.exercicio.exercicio_operacoes :as exercicio]
            [clojure.string :as str]))

(defn menu-usuario []
  (if (empty? (usuario/get-usuario))
    (do
      (println "Insira seus dados")
      (println "Digite sua altura: ")
      (let [altura (read)]
        (println "Digite seu peso: (Em Kg)")
        (let [peso (read)]
          (println "Digite sua idade: ")
          (let [idade (read)]
            (println "Digite seu sexo: ")
            (let [sexo (read)
                  usuario-dados {:altura altura :peso peso :idade idade :sexo sexo}]
              (usuario/cadastrar-usuario usuario-dados)
              (println "Usuário cadastrado!" usuario-dados))))))
    (println "Usuário já cadastrado! " (usuario/get-usuario))))

(defn menu []
  (println "=== Menu Nutricional ===")
  (println "1. Adicionar refeição")
  (println "2. Mostrar refeições do dia")
  (println "3. Adicionar exercício")
  (println "4. Mostrar exercicios realizados")
  (println "5. Calcular saldo calorico")
  (println "6. Sair")
  (println "Escolha uma opção:")
  (let [entrada (read-line)]
    (if (alimento/inteiro? entrada)
      (Integer/parseInt entrada)
      -1)))

(defn executar [refeicoes]
  (let [opcao (menu)]
    (cond
      (= opcao 1)
      (do
        (println "Digite os alimentos consumidos separados por vírgula (ex: arroz, feijao, ovo):")
        (let [linha (read-line)
              nomes (clojure.string/split linha #",\s*")]
          (letfn [(processar [restantes refeicoes-atualizadas]
            (if (empty? restantes)
              refeicoes-atualizadas
              (let [nome (first restantes)
                    opcoes (alimento/traduzir-e-filtrar-alimentos nome)]
                (println (str "\nSelecione o alimento correspondente para: " nome))
                (letfn [(mostrar-opcoes [lst idx]
                          (if (empty? lst)
                            nil
                            (let [a (first lst)
                                  desc-en (:description a)
                                  desc-pt (try
                                            (trad/ingles-portugues desc-en)
                                            (catch Exception _ desc-en))
                                  marca (or (:brandOwner a) "-")
                                  categoria-en (:foodCategory a)
                                  categoria-pt (try
                                                 (if (and categoria-en (not (clojure.string/blank? categoria-en)))
                                                   (trad/ingles-portugues categoria-en)
                                                   "Categoria não informada")
                                                 (catch Exception _
                                                   "Categoria não informada"))
                                  porcao (or (:servingSize a) "Porção não informada")
                                  calorias (or (get-in a [:foodNutrients 0 :value]) "Calorias não informadas")]
                              (println (format "%d. %s | Marca: %s | Categoria: %s | Porção: %s | Calorias: %s kcal"
                                               idx desc-pt marca categoria-pt porcao calorias))
                              (recur (rest lst) (inc idx)))))]
                  (mostrar-opcoes opcoes 1))
                (println "Digite a opção desejada:")
                (let [idx-str (read-line)
                      idx (try (Integer/parseInt idx-str) (catch Exception _ 0))]
                  (if (or (< idx 1) (> idx (count opcoes)))
                    (do
                      (println "Opção inválida, pulando alimento.")
                      (recur (rest restantes) refeicoes-atualizadas))
                    (let [selecionado (nth opcoes (dec idx))]
                      (println (str "Digite a quantidade consumida em gramas de " (:descricao-pt selecionado) ":"))
                      (let [qtd-str (read-line)]
                        (if (alimento/double-str? qtd-str)
                          (let [qtd (Double/parseDouble qtd-str)
                                nova-refeicao (alimento/montar-refeicao selecionado qtd)
                                refeicoes-novas (conj refeicoes-atualizadas nova-refeicao)]
                            (println (str "Adicionado: " (:descricao-pt selecionado) " (" qtd "g)"))
                            (recur (rest restantes) refeicoes-novas))
                          (do
                            (println "Quantidade inválida. Pulando alimento.")
                            (recur (rest restantes) refeicoes-atualizadas))))))))))]

          (let [resultado (processar nomes refeicoes)]
          (recur resultado)))))

          (= opcao 2)
          (do
          (let [{linhas :linhas total-cal :total-calorias} (alimento/imprimir-refeicoes refeicoes)]
            (println "----------------------------------------------------------------------------------------------------------")
            (letfn [(imprimir-linhas [lst]
                      (if (empty? lst)
                        nil
                        (do
                          (println (first lst))
                          (recur (rest lst)))))]
              (imprimir-linhas linhas))
              (println "----------------------------------------------------------------------------------------------------------")
              (println (format "Total de calorias consumidas no dia: %.2f kcal" total-cal))
              (recur refeicoes)))

              (= opcao 3)
                (do
                (println "Diga o nome do exercício: ")
                (let [nome-exercicio (read-line)
                    peso-usuario (usuario/get-peso)
                    _ (println "Digite a duração do exercício em minutos: ")
                    duracao (read)
                    _ (println (format "Perda calórica baseada no seu peso atual %.2f Kg e tempo gasto %d min"
                                       (double peso-usuario) duracao))
                    lista-exercicios (exercicio/listar-exercicios nome-exercicio peso-usuario duracao)
                    _ (println (exercicio/enumerar-exercicios lista-exercicios))
                    _ (println "Escolha o exercício realizado: ")
                    indice (read)
                    exercicio-selecionado (exercicio/selecionar-exercicio lista-exercicios indice)
                    _ (println "Informe a data em que o exercício foi realizado: (Ex: dia/mês/ano)")
                    data (read-line)
                    exercicio-com-data (exercicio/adicionar-data exercicio-selecionado data)
                    _ (exercicio/registrar-perda exercicio-com-data)]
                  (println (str "Exercício adicionado com sucesso: "
                              (:nome exercicio-com-data)
                              ", Data: " (:data exercicio-com-data)
                              ", Calorias: " (:total-calorias exercicio-com-data) " kcal"))
                  (recur refeicoes)))

      (= opcao 4)
          (do
          (println "Perdas calóricas registradas: ")
          (let [lista-calorias-perdidas (exercicio/calorias-perdidas)
                calorias-perdidas (exercicio/soma-calorias-perdidas)]
            (println "" lista-calorias-perdidas)
            (println "Total de calorias perdidas:" calorias-perdidas))
          (recur refeicoes))

          (= opcao 5)
          (do
          (let [calorias-ganhas (alimento/somar-calorias refeicoes)
                calorias-perdidas (exercicio/soma-calorias-perdidas)
                saldo-calorico (saldo/calcular-saldo-calorico calorias-ganhas calorias-perdidas)]
            (println "Total de calorias ganhas: " calorias-ganhas)
            (println "Total de calorias perdidas: " calorias-perdidas)
            (println saldo-calorico))
          (recur refeicoes))

          (= opcao 6)
          (println "Saindo...")

          :else
          (do
          (println "Opção inválida.")
          (recur refeicoes)))))

(defn -main []
  (menu-usuario)
  (executar []))
