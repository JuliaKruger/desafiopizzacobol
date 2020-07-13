      *Divis�o de identifica��o do programa
       Identification Division.
       Program-id. "desafiopizzacobol".
       Author. "Julia Kr�ger".
       Installation. "PC".
       Date-written. 09/07/2020.
       Date-compiled. 09/07/2020.

      *Divis�o para configura��o do ambiente
       Environment Division.
       Configuration Section.
           special-names. decimal-point is comma.

      *   Declara��o dos recursos externos
       Input-output Section.
       File-control.
       I-O-Control.


      *Declara��o de vari�veis
       Data Division.

      *----Vari�veis de arquivos
       File Section.

      *----Vari�veis de trabalho
       Working-storage Section.

      * declara��o das vari�veis
       01 relatorio occurs 50.
           05 diametro                             pic 9(03)V99
                                                   value 0.
           05 preco                                pic 9(03)V99
                                                   value 0.
           05 preco_cm2                            pic 9(03)V99
                                                   value 0.
           05 nome                                 pic x(15).

       77 ind                                      pic 9(2)
                                                   value 0.
       77 auxpreco                                 pic 9(03)V99.
       77 auxnome                                  pic x(15).
       77 auxdiametro                              pic 9(03)V99.
       77 aux                                      pic x(1).
       77 fimprograma                              pic x(03).
       77 controle                                 pic x(08).
       77 quantidade                               pic 9(3).
       01 varporcentagem occurs 50.
           05 porcent                              pic S9(03)V99
                                                   value 0.
           05 diferenca                            pic S9(03)V99
                                                   value 0.

      *----Vari�veis para comunica��o entre programas
       .Linkage Section.

      *----Declara��o de tela
       Screen Section.


      *Declara��o do corpo do programa
       Procedure Division.

      * executando as sections
           perform inicializa.
           perform processamento.
           perform finaliza.

      * section inicializa(vazia)
           inicializa section.
               next sentence
               .
           inicializa-exit.
               exit.

      * section processamento, recebendo as informa��es
           processamento section.
               display erase
               perform until fimprograma = "Nao"
                   add 1 to ind
                   if ind <= 50
                       display "Informe o nome da pizza: "
                       accept nome(ind)
                       display "Informe o diametro da pizza: "
                       accept diametro(ind)
                       display "Informe o preco da pizza: "
                       accept preco(ind)

      *                c�lculo do pre�o por cent�metro quadrado
                       compute preco_cm2(ind) = preco(ind) /
                       (((diametro(ind) / 2)*(diametro(ind) / 2))* 3,14)

                       display "Deseja cadastrar outra pizza? (Sim/Nao)"
                       accept fimprograma
                   else
                       display "Voce atingiu o limite de pizzas!"
                       display "Tecle enter!"
                       accept aux
                       move "Nao" to fimprograma
                   end-if
                   display erase
               end-perform

               move ind to quantidade

      * executar a section ordenar
               perform ordenar
      * executar a section porcentagem
               perform porcentagem
      * executar a section imprimetab
               perform imprimetab
               .
           processamento-exit.
               exit.


      * perform para ordenar as informa��es (onde mais tive
      * dificuldade de fazer funcionar, ainda n�o sei se est� certa,
      * mas pelo menos est� fazendo alguma coisa)
           ordenar section.
               move "trocou" to controle
               move 1 to ind
               perform until controle <> "trocou"
                   move "Ntrocou" to controle
                   perform until ind >= quantidade
                       if preco_cm2(ind) > preco_cm2(ind + 1) then
                           move preco_cm2(ind + 1) to auxpreco
                           move nome(ind + 1) to auxnome
                           move diametro(ind + 1) to auxdiametro
                           move preco_cm2(ind) to preco_cm2(ind + 1)
                           move nome(ind) to nome(ind + 1)
                           move diametro(ind) to diametro(ind + 1)
                           move auxpreco to preco_cm2(ind)
                           move auxnome to nome(ind)
                           move auxdiametro to diametro(ind)
                           move "trocou" to controle
                       end-if
                       add 1 to ind
                   end-perform
                   move 1 to ind
               end-perform
               .
           ordenar-exit.
               exit.


      * c�lculo da diferen�a percentual, fiz como o prodessor, e
      * coloquei quantidade - 1 para n�o gerar porcentagem no �ltmo,
      * pois n�o tem como comparar com um pr�ximo
           porcentagem section.
               move 2 to ind
               perform until ind > quantidade
                   compute diferenca(ind) = preco_cm2(ind) -
                   preco_cm2(ind - 1)
                   compute porcent(ind)
                   = (diferenca(ind) * 100) / preco_cm2(ind - 1)
                   add 1 to ind
               end-perform
               .
           porcentagem-exit.
               exit.

      * section para exibir na tela as informa��es finais
           imprimetab section.
               display "Ordem de melhor custo beneficio: "
               perform varying ind from 1 by 1 until ind > quantidade
                   display "Pizza: " nome(ind)
                   display "Tamanho: " diametro(ind) " cm"
                   display "Preco por cm2: R$" preco_cm2(ind)

      *            criei um if para n�o mostrar a porcentagem do
      *            primeiro item
                   if ind > 1 then
                       display "Percentual de diferenca: "
                       porcent(ind) "%"
                   end-if
               end-perform
               .
           imprimetab-exit.
               exit.

      * section para finalizar o programa
           finaliza section.
               stop run
               .
           finaliza-exit.
               exit.

























