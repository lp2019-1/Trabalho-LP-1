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
                  ("Viagra", 4)]

{-::Funções auxiliares::-}

--Função soma tuplas
somaTuplas :: Medicamentos -> Medicamento -> Medicamento
somaTuplas [(a,b)] (c,d) = (a, b+d)


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


--alterarMedicamento :: Medicamento -> Medicamentos -> Medicamentos

--tomarMedicamentoSOS  ::  Nome -> Medicamentos ->  Medicamentos

--tomarMedicamentosHorario :: PlanoMedicamento -> Medicamentos -> HoraAtual -> (PlanoMedicamento,Medicamentos)

--cadastrarAlarmes :: PlanoMedicamento -> Horario

--listarMedicamentosComprar :: Medicamentos ->  Medicamentos

--comprarMedicamentosDias ::  PlanoMedicamento -> Medicamentos -> Int -> Medicamentos

--comprarMedicamentosPreco :: Medicamentos -> Mercado -> Compra

--comprarMedicamentosPrecoFlex :: Medicamentos -> Mercado -> [CompraFlex]

--careTaker :: PlanoMedicamento -> Int -> Medicamentos-> (PlanoMedicamento,Medicamentos)
