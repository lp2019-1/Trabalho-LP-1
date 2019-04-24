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
                  ("Ibuprofeno", 0),
                  ("​Nimesulida", 8)]

f_plano :: PlanoMedicamento
f_plano = f_plano = [("Dipirona",[6,10,14],14),
                     ("Buscopan",[8,10,14],14),
                     ("Dorflex",[11,15,24],11)]


{-::Funções auxiliares::-}

--Função soma tuplas
somaTuplas :: Medicamentos -> Medicamento -> Medicamento
somaTuplas [(a,b)] (c,d) = (a, b+d)

subtraiRemedio :: Medicamentos -> Medicamento -> Medicamento
subtraiRemedio [(a,b)] (c,d) = (a, b-1)

quickSort [ ] = [ ]
quickSort (a:as) = quickSort [e | e <- as, e < a] ++ [a] ++quickSort [e | e <- as, e > a]

consultarQuantidade :: Quantidade -> Medicamentos -> Medicamentos
consultarQuantidade p fa = if (elem p(map medicamento fa)) then [(n, q) | (n, q) <- fa, q == p] else [("",0)]
                       where medicamento (n, q) = q

-- <\Questão 01/> Função que adiciona remedios na Lista
adicionarMedicamento :: Medicamento -> Medicamentos -> Medicamentos
adicionarMedicamento (n, q) fa = if (consultarMedicamento n fa == [("",0)]) then ((n, q) : fa) 
    else [(somaTuplas (consultarMedicamento n fa) (n, q))] ++ removerMedicamento n fa


-- <\Questão 02/> Função que remove medicamentos da lista.
removerMedicamento :: Nome -> Medicamentos -> Medicamentos
removerMedicamento m fa =  [ (n,q) | (n,q) <- fa, n /= m ]


-- <\Questão 03/> Função que consulta na lista de medicamentos.
consultarMedicamento :: Nome -> Medicamentos -> Medicamentos
consultarMedicamento m fa = if (elem m(map medicamento fa)) then [(n, q) | (n, q) <- fa, n == m] else [("",0)]
                       where medicamento (n, q) = n


-- <\Questão 04/> Função que altera um medicamento da lista.
alterarMedicamento :: Medicamento -> Medicamentos -> Medicamentos
alterarMedicamento (n, q) fa = if (consultarMedicamento n fa == [("",0)]) then fa 
    else [(n,q)] ++ removerMedicamento n fa


-- <\Questão 05/> Função que toma um medicamento de uma lista.
tomarMedicamentoSOS  ::  Nome -> Medicamentos ->  Medicamentos
tomarMedicamentoSOS n fa = if (consultarMedicamento n fa == [("",0)]) then fa 
    else [(subtraiRemedio (consultarMedicamento n fa) (n, -1))] ++ removerMedicamento n fa

{-Questão 06-}
--tomarMedicamentosHorario :: PlanoMedicamento -> Medicamentos -> HoraAtual -> (PlanoMedicamento,Medicamentos)

-- <\Questão 07> :: Função que cria uma lista de horarios.
cadastrarAlarmes :: PlanoMedicamento -> Horario
cadastrarAlarmes [ ] = [ ]
cadastrarAlarmes ((_,h,_):fa) = quickSort (h ++ cadastrarAlarmes fa)


-- <\Questão 08> :: Função que cria uma lista de medicamentos que estão zerados.

listarMedicamentosComprar :: Medicamentos ->  Medicamentos
listarMedicamentosComprar fa = consultarQuantidade 0 fa


--comprarMedicamentosDias ::  PlanoMedicamento -> Medicamentos -> Int -> Medicamentos
comprarMedicamentosDias :: PlanoMedicamento -> Medicamentos -> Int -> Medicamentos
comprarMedicamentosDias = 

--comprarMedicamentosPreco :: Medicamentos -> Mercado -> Compra

--comprarMedicamentosPrecoFlex :: Medicamentos -> Mercado -> [CompraFlex]

--careTaker :: PlanoMedicamento -> Int -> Medicamentos-> (PlanoMedicamento,Medicamentos)
