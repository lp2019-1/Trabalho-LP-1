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

adicionarMedicamento :: Medicamento -> Medicamentos -> Medicamentos

removerMedicamento :: Nome -> Medicamentos -> Medicamentos

consultarMedicamento :: Nome -> Medicamentos -> Medicamento

alterarMedicamento :: Medicamento -> Medicamentos -> Medicamentos

tomarMedicamentoSOS  ::  Nome -> Medicamentos ->  Medicamentos

tomarMedicamentosHorario :: PlanoMedicamento -> Medicamentos -> HoraAtual -> (PlanoMedicamento,Medicamentos)

cadastrarAlarmes :: PlanoMedicamento -> Horario

listarMedicamentosComprar :: Medicamentos ->  Medicamentos

comprarMedicamentosDias ::  PlanoMedicamento -> Medicamentos -> Int -> Medicamentos

comprarMedicamentosPreco :: Medicamentos -> Mercado -> Compra

comprarMedicamentosPrecoFlex :: Medicamentos -> Mercado -> [CompraFlex]

careTaker :: PlanoMedicamento -> Int -> Medicamentos-> (PlanoMedicamento,Medicamentos)
