(ns org.nutricao_projeto.core
  (:require [org.nutricao_projeto.alimento.alimento_controller :as alimento]
            [org.nutricao_projeto.data :as data]
            [org.nutricao_projeto.exercicio.exercicio_operacoes :as exercicio]
            [org.nutricao_projeto.saldo_calorico :as saldo]
            [org.nutricao_projeto.usuario.usuario_operacoes :as usuario]))

(defn menu-usuario []
  (if (empty? (usuario/get-usuario))
    (do
      (println "Insira seus dados")
      (println "Digite sua altura: (em m)")
      (let [altura (read)]
        (println "Digite seu peso: (Em kg)")
        (let [peso (read)]
          (println "Digite sua idade: ")
          (let [idade (read)]
            (println "Digite seu sexo: ")
            (let [sexo (read)
                  usuario-dados {:altura altura :peso peso :idade idade :sexo sexo}]
              (usuario/cadastrar-usuario usuario-dados)
              (println "Usuário cadastrado!" usuario-dados))))))
    (println "Usuário já cadastrado! " (usuario/get-usuario))))

(defn mostrar-menu []
  (println "=== Menu Nutricional ===")
  (println "1. Adicionar refeição")
  (println "2. Mostrar refeições do dia")
  (println "3. Adicionar exercício")
  (println "4. Mostrar exercicios realizados")
  (println "5. Mostrar exercicios realizados período")
  (println "6. Calcular saldo calorico")
  (println "7. Calcular saldo calorico por período")
  (println "8. Sair")
  (println "Escolha uma opção:"))

(defn menu []
  (let [entrada (read-line)]
    (if (alimento/inteiro? entrada)
      (Integer/parseInt entrada)
      -1)))

(defn executar [refeicoes]
  (mostrar-menu)
  (let [opcao (menu)]
    (cond
      (= opcao 1)
      (let [data (do (println "Informe a data da refeição: (Ex: 06/06/2025)") (read-line))
            linha (do (println "Digite os alimentos consumidos separados por vírgula (ex: arroz, feijao, ovo):") (read-line))
            nomes (clojure.string/split linha #",\s*")]

        (letfn [(exibir-opcoes [lst idx]
                  (if (empty? lst)
                    nil
                    (let [a (first lst)
                          desc-pt (:descricao-pt a)
                          marca (or (:brandOwner a) "-")
                          categoria (or (:foodCategory a) "Categoria não informada")
                          porcao (or (:servingSize a) "Porção não informada")
                          calorias (or (get-in a [:foodNutrients 0 :value]) "Calorias não informadas")]
                      (do
                        (println (format "%d. %s | Marca: %s | Categoria: %s | Porção: %s | Calorias: %s kcal"
                                         idx desc-pt marca categoria porcao calorias))
                        (exibir-opcoes (rest lst) (inc idx))))))

                (processar [restantes refeicoes-atualizadas]
                  (if (empty? restantes)
                    refeicoes-atualizadas
                    (let [nome (first restantes)
                          opcoes (alimento/traduzir-e-filtrar-alimentos nome)]
                      (println (str "\nSelecione o alimento correspondente para: " nome))
                      (exibir-opcoes opcoes 1)
                      (println "Digite a opção desejada:")
                      (let [idx-str (read-line)
                            idx (try (Integer/parseInt idx-str) (catch Exception _ 0))]
                        (if (or (< idx 1) (> idx (count opcoes)))
                          (do
                            (println "Opção inválida, pulando alimento.")
                            (processar (rest restantes) refeicoes-atualizadas))
                          (let [selecionado (nth opcoes (dec idx))]
                            (println (str "Digite a quantidade consumida em gramas de " (:descricao-pt selecionado) ":"))
                            (let [qtd-str (read-line)]
                              (if (alimento/double-str? qtd-str)
                                (let [qtd (Double/parseDouble qtd-str)
                                      nova-refeicao (alimento/montar-refeicao selecionado qtd data)
                                      refeicoes-novas (conj refeicoes-atualizadas nova-refeicao)]
                                  (println (str "Adicionado: " (:descricao-pt selecionado) " (" qtd "g)"))
                                  (processar (rest restantes) refeicoes-novas))
                                (do
                                  (println "Quantidade inválida. Pulando alimento.")
                                  (processar (rest restantes) refeicoes-atualizadas))))))))))]
          (let [resultado (processar nomes refeicoes)]
            (alimento/salvar-refeicoes resultado)
            (executar resultado))))

      (= opcao 2)
      (let [data-inicio (do (println "Digite a data inicial: (Ex: dia/mês/ano)") (read-line))
            data-fim (do (println "Digite a data final: (Ex: dia/mês/ano)") (read-line))
            refeicoes-filtradas (data/entre-datas refeicoes data-inicio data-fim)
            {linhas :linhas total-cal :total-calorias} (alimento/imprimir-refeicoes refeicoes-filtradas)]
        (println "----------------------------------------------------------------------------------------------------------")
        (if (empty? linhas)
          (println "Nenhuma refeição encontrada no período.")
          (letfn [(printar [lst]
                    (if (empty? lst)
                      nil
                      (do
                        (println (first lst))
                        (printar (rest lst)))))]
            (printar linhas)))
        (println "----------------------------------------------------------------------------------------------------------")
        (format "Total de calorias consumidas no período: %.2f kcal" (double total-cal))
        (executar refeicoes))

      (= opcao 3)
      (let [nome-exercicio (do (println "Diga o nome do exercício: ") (read-line))
            peso-usuario (usuario/get-peso)
            duracao (do (println "Digite a duração do exercício em minutos: ") (read))
            _ (println (format "Perda calórica baseada no seu peso atual %.2f Kg e tempo gasto %d min" peso-usuario duracao))
            lista-exercicios (exercicio/listar-exercicios nome-exercicio peso-usuario duracao)
            _ (println (exercicio/enumerar-exercicios lista-exercicios))
            indice (do (println "Escolha o exercício realizado: ") (read))
            _ (read-line)
            exercicio-selecionado (exercicio/selecionar-exercicio lista-exercicios indice)
            data (do (println "Informe a data em que o exercício foi realizado: (Ex: dia/mês/ano)") (read-line))
            exercicio-com-data (exercicio/adicionar-data exercicio-selecionado data)]
        (exercicio/registrar-perda exercicio-com-data)
        (println (str "Exercício adicionado com sucesso: "
                      (:nome exercicio-com-data)
                      ", Data: " (:data exercicio-com-data)
                      ", Calorias: " (:total-calorias exercicio-com-data) " kcal"))
        (executar refeicoes))

      (= opcao 4)
      (let [calorias-perdidas (exercicio/total-calorias-perdidas)]
        (println "Perdas calóricas registradas: \n")
        (exercicio/imprimir-calorias-perdidas)
        (println "Total de calorias perdidas:" calorias-perdidas)
        (executar refeicoes))

      (= opcao 5)
      (let [lista-calorias-perdidas (exercicio/lista-calorias-perdidas)
            data-inicial (do (println "Digite a data inicial: (Ex: dia/mês/ano)") (read-line))
            data-final (do (println "Digite a data final: (Ex: dia/mês/ano)") (read-line))
            exercicios-filtrados (data/entre-datas lista-calorias-perdidas data-inicial data-final)
            calorias-perdidas (exercicio/total-calorias-perdidas exercicios-filtrados)]

        (println (format "Perdas calóricas registradas: (Entre %s - %s)\n" data-inicial data-final))
        (exercicio/imprimir-calorias-perdidas lista-calorias-perdidas)
        (mapv exercicio/formatar-perda-lista exercicios-filtrados)
        (println "Total de calorias perdidas:" calorias-perdidas)
        (executar refeicoes))

      (= opcao 6)
      (let [calorias-ganhas (alimento/somar-calorias refeicoes)
            calorias-perdidas (exercicio/total-calorias-perdidas)
            saldo-calorico (saldo/calcular-saldo-calorico calorias-ganhas calorias-perdidas)]
        (println "Total de calorias ganhas: " calorias-ganhas)
        (println "Total de calorias perdidas: " calorias-perdidas)
        (println saldo-calorico)
        (executar refeicoes))

      (= opcao 7)
      (let [data-inicial (do (println "Digite a data inicial: (Ex: dia/mês/ano)") (read-line))
            data-final (do (println "Digite a data final: (Ex: dia/mês/ano)") (read-line))
            lista-calorias-perdidas (exercicio/lista-calorias-perdidas)

            exercicios-filtrados (data/entre-datas lista-calorias-perdidas data-inicial data-final)
            refeicoes-filtradas (data/entre-datas refeicoes data-inicial data-final)
            calorias-ganhas (alimento/somar-calorias refeicoes-filtradas)
            calorias-perdidas (exercicio/total-calorias-perdidas exercicios-filtrados)

            saldo-calorico (saldo/calcular-saldo-calorico calorias-ganhas calorias-perdidas)]
        (println (format "Total de calorias ganhas: %.2f (Entre %s - %s)" calorias-ganhas data-inicial data-final))
        (println (format "Total de calorias perdidas: %d (Entre %s - %s)" calorias-perdidas data-inicial data-final))
        (println saldo-calorico)
        (executar refeicoes))

      (= opcao 8)
      (println "Saindo...")

      :else
      (do
        (println "Opção inválida.")
        (executar refeicoes)))))

(defn -main []
  (menu-usuario)
  ;(executar[]))
  (let [refeicoes-salvas (alimento/carregar-refeicoes)]
    (executar refeicoes-salvas)))