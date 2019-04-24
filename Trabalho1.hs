{- **Enunicado do trabalho:
	  
UnBCare
                                      
O objetivo desse trabalho é fornecer apoio ao gerenciamento de medicamentos a serem administrados a um 
paciente. Segundo orientação médica, um paciente segue um plano de medicamentos, através do qual 
medicamentos são prescritos a partir de um horário inicial e seguindo uma grade horária. Cada medicamento 
tem um nome e uma quantidade de comprimidos associada. Com base nisso, um paciente pode tomar remédios 
seguindo o plano ou em caráter emergencial(SOS). Eventualmente, o paciente ou responsável irá comprar 
medicamentos, o que é feito num mercado em que farmácias oferecem determinados medicamentos a preços 
específicos. Um objetivo prático é diminuir o custo com a aquisição de medicamentos. Considerando a 
definição de tipos abaixo:-}

type Nome = String
type Quantidade = Int
type HorarioProximo = Int
type HoraAtual = Int
type Horario = [Int]
type Medicamento = (Nome,Quantidade)
type Medicamentos = [Medicamento]
type Prescricao = (Nome,Horario,HorarioProximo)
type PlanoMedicamento = [Prescricao]
type Preco = Int
type Farmacia = (Nome,[(Medicamento,Preco)])
type Mercado = [Farmacia]
type Compra = (Preco, Nome)
type CompraFlex = (Preco, Nome, Medicamentos)


{-Exemplo de Medicamentos-}
f_Medicamentos :: Medicamentos
f_Medicamentos = [("Dipirona", 4), 
                  ("Buscopan", 1), 
                  ("Lorotadina", 2), 
                  ("Stezza",5), 
                  ("Ritalina", 3), 
                  ("Viagra", 4),
                  ("Ritalina", 0),
                  ("Ibuprofeno", 0)]

f_plano :: PlanoMedicamento
f_plano = [("Dipirona",[6,10,14],14),
           ("Buscopan",[8,10,14],14),
           ("Dorflex",[11,15,24],11)]


{-::Funções auxiliares::-}

--Função soma tuplas
somaTuplas :: Medicamentos -> Medicamento -> Medicamento
somaTuplas [(a,b)] (c,d) = (a, b+d) --Função auxiliar da questão 1. Pega uma lista de medicamentos e um medicamento e retorna uma tupla composta de
                                    -- um remédio e a quantidade adicionada da quantidade fornecida

subtraiRemedio :: Medicamentos -> Medicamento -> Medicamento
subtraiRemedio [(a,b)] (c,d) = (a, b-1) --Função auxiliar para questão 5 que trata de remo

quickSort [ ] = [ ]
quickSort (a:as) = quickSort [e | e <- as, e < a] ++ [a] ++quickSort [e | e <- as, e > a]

consultarQuantidade :: Quantidade -> Medicamentos -> Medicamentos
consultarQuantidade p fa = if (elem p(map medicamento fa)) then [(n, q) | (n, q) <- fa, q == p] else [("",0)]
                       where medicamento (n, q) = q
{-
**QUESTÃO 1, valor 1,0 ponto**
Defina a função adicionarMedicamento que adiciona medicamento à lista de medicamentos em qualquer 
posição da lista de medicamentos.  Se o medicamento já existir na lista de medicamentos, então 
 a sua quantidade deve ser atualizada na lista. O tipo da função adicionarMedicamento é o seguinte:

 adicionarMedicamento :: Medicamento -> Medicamentos -> Medicamentos 
-}
-- <\Questão 01/> Função que adiciona remedios na Lista
adicionarMedicamento :: Medicamento -> Medicamentos -> Medicamentos
adicionarMedicamento (n, q) fa = if (consultarMedicamento n fa == [("",0)]) then ((n, q) : fa) 
    else [(somaTuplas (consultarMedicamento n fa) (n, q))] ++ removerMedicamento n fa

{-
**QUESTÃO 2, valor 1,0 ponto**
Defina a função removerMedicamento que, a partir do nome de um medicamento e de uma lista de medicamentos,
retorna uma nova lista em que o medicamento com esse nome é removido da lista. Se o medicamento não 
existir, a lista original de medicamentos é retornada. O tipo da função removerMedicamento é o seguinte:

removerMedicamento :: Nome -> Medicamentos -> Medicamentos
-}
-- <\Questão 02/> Função que remove medicamentos da lista.
removerMedicamento :: Nome -> Medicamentos -> Medicamentos
removerMedicamento m fa =  [ (n,q) | (n,q) <- fa, n /= m ]

{- 
**QUESTÃO 3, valor 0,5 ponto**
Defina a função consultarMedicamento que, a partir do nome de um medicamento e de uma lista de medicamentos,
retorna esse medicamento, ou seja, um par (Nome,Quantidade) com as informações desse medicamento.
Se o medicamento não existir, retorne o par ("",0). O tipo da função consultarMedicamento é o seguinte:

consultarMedicamento :: Nome -> Medicamentos -> Medicamento
-}
-- <\Questão 03/> Função que consulta na lista de medicamentos.
consultarMedicamento :: Nome -> Medicamentos -> Medicamentos
consultarMedicamento m fa = if (elem m(map medicamento fa)) then [(n, q) | (n, q) <- fa, n == m] else [("",0)]
                       where medicamento (n, q) = n

{- 
**QUESTÃO 4, valor 1,0 ponto**
Defina a função alterarMedicamento que, a partir de um medicamento e de uma lista de medicamentos,
retorna uma nova lista de medicamentos em que esse medicamento seja atualizado na lista de medicamentos.
Se o medicamento não existir, a lista orginal de medicamentos é retornada.
O tipo da função alterarMedicamento é o seguinte:

alterarMedicamento :: Medicamento -> Medicamentos -> Medicamentos
-}
-- <\Questão 04/> Função que altera um medicamento da lista.
alterarMedicamento :: Medicamento -> Medicamentos -> Medicamentos
alterarMedicamento (n, q) fa = if (consultarMedicamento n fa == [("",0)]) then fa 
    else [(n,q)] ++ removerMedicamento n fa

{- 
**QUESTÃO 5, valor 1,0 ponto
Defina a função tomarMedicamentoSOS que, a partir do nome de um medicamento e de uma lista de medicamentos,
retorna uma nova lista de medicamentos em que a quantidade desse remédio é diminuída em uma unidade.
Se não existir medicamento com o nome fornecido, a lista orginal de medicamentos é retornada.
O tipo da função tomarMedicamentoSOS é o seguinte:

tomarMedicamentoSOS  ::  Nome -> Medicamentos ->  Medicamentos
-}
-- <\Questão 05/> Função que toma um medicamento de uma lista.
tomarMedicamentoSOS  ::  Nome -> Medicamentos ->  Medicamentos
tomarMedicamentoSOS n fa = if (consultarMedicamento n fa == [("",0)]) then fa 
    else [(subtraiRemedio (consultarMedicamento n fa) (n, -1))] ++ removerMedicamento n fa

{-Questão 06-}
--tomarMedicamentosHorario :: PlanoMedicamento -> Medicamentos -> HoraAtual -> (PlanoMedicamento,Medicamentos)

{- 
**QUESTÃO 7, valor 1,0 ponto
Defina a função cadastrarAlarmes que, dado um plano de medicamento, retorna uma grade horária única 
crescentemente ordenada de todos os instantes em que o paciente deve tomar seus medicamentos.
O tipo da função cadastrarAlarmes é o seguinte:

cadastrarAlarmes :: PlanoMedicamento -> Horario
-}
-- <\Questão 07> :: Função que cria uma lista de horarios.
cadastrarAlarmes :: PlanoMedicamento -> Horario
cadastrarAlarmes [ ] = [ ]
cadastrarAlarmes ((_,h,_):fa) = quickSort (h ++ cadastrarAlarmes fa)

{- 
**QUESTÃO 8, valor 1,0 ponto
Defina a função listarMedicamentosComprar que, sendo fornecida uma lista de medicamentos, retorna
uma lista dos medicamentos que precisam ser comprados, ou seja, aqueles cuja quantidade tenha zerado.
O tipo da função listarMedicamentosComprar é o seguinte:

listarMedicamentosComprar :: Medicamentos ->  Medicamentos
-}
-- <\Questão 08> :: Função que cria uma lista de medicamentos que estão zerados.
listarMedicamentosComprar :: Medicamentos ->  Medicamentos
listarMedicamentosComprar fa = consultarQuantidade 0 fa

{- 
**QUESTÃO 9, valor 1,0 ponto
Defina a função comprarMedicamentosDias que, a partir de um plano de medicamentos, uma lista de 
medicamentos e um número de dias, deve retornar os medicamentos a serem comprados na quantidade exata
de acordo o plano de medicamentos para o número de dias informado e a partir de um estoque inicial de 
medicamentos especificado pela lista de medicamentos informado na chamada da função. 
O tipo da função comprarMedicamentosDias é o seguinte: 

comprarMedicamentosDias ::  PlanoMedicamento -> Medicamentos -> Int -> Medicamentos
-}
-- <\Questão 09> :: Função de comprar medicamento.
comprarMedicamentosDias :: PlanoMedicamento -> Medicamentos -> Int -> Medicamentos
comprarMedicamentosDias [ ] [ ] _ = [ ]
comprarMedicamentosDias _ [ ] _ = [ ]
comprarMedicamentosDias [ ] _ _ = [ ]
comprarMedicamentosDias ((a,b,c):as) ((x,y):xs) n = if y<(n*length(b)) then (x,length(b)*n-y):comprarMedicamentosDias as xs n
  else (x,0):comprarMedicamentosDias as xs n

--<\Questão 10/>
--comprarMedicamentosPreco :: Medicamentos -> Mercado -> Compra


--comprarMedicamentosPrecoFlex :: Medicamentos -> Mercado -> [CompraFlex]


--careTaker :: PlanoMedicamento -> Int -> Medicamentos-> (PlanoMedicamento,Medicamentos)
