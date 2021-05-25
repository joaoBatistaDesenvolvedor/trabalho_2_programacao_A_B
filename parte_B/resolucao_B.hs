{-
Exercício 6) Dada a definição de tipos abaixo, similar à vista em aula.
data Exp =
 Val Int -- um numero
| Add Exp Exp -- soma de duas expressoes
| Sub Exp Exp -– subtração de duas expressoes
avalia :: Num a => Exp a -> a
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
a) Expanda as definições acima para que sejam incluídas expressões
usando as operações multiplicação e divisão (além de soma e subtração).
b) Avalie as expressões abaixo, primeiro declarando-as de acordo com a
sintaxe do tipo algébrico e depois executando a função avalia sobre essas
declarações:
(3+12)*(15-5)/(1*3) e - ((6+8-5+1)*(2+6/2))

-}

{-- a) Expanda as definições acima para que sejam incluídas expressões
usando as operações multiplicação e divisão (além de soma e subtração).--}



{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
data Exp a=
 Val Int -- um numero
    | Add (Exp a) (Exp a) -- soma de duas expressoes
    | Sub (Exp a) (Exp a) --subtração de duas expressoes
    | Mult (Exp a) (Exp a) -- multiplicacao de duas expressoes
    | Divisao (Exp a) (Exp a) --divsao duas expressoes

avalia ::( Num a, Fractional Int) => Exp a -> Int
avalia (Val x) = x
avalia (Add exp1 exp2) = avalia exp1 + avalia exp2
avalia (Sub exp1 exp2) = avalia exp1 - avalia exp2
avalia (Mult exp1 exp2) = avalia exp1 * avalia exp2
avalia (Divisao exp1 exp2) = avalia exp1 / avalia exp2



{-b) Avalie as expressões abaixo, primeiro declarando-as de acordo com a
sintaxe do tipo algébrico e depois executando a função avalia sobre essas
declarações:-}

--(3+12)*(15-5)/(1*3) e - ((6+8-5+1)*(2+6/2))

e1 :: Exp a
e1 = Mult (Add (Val 3) (Val 12)) (Divisao (Sub (Val 15) (Val 5)) (Mult (Val 1) (Val 3)))

e2:: Exp a

e2 = Sub (Val 0) (Mult (Add (Sub (Add (Val 6) (Val 8)) (Val 5)) (Val 1)) (Add (Val 2) (Divisao (Val 6) (Val 2))))



{-
Exercício 7) Jogo “Pedra, Papel, Tesoura”
a) Defina um tipo algébrico Jogada, para representar uma possível jogada, ou
seja: Pedra ou Papel ou Tesoura.
b) Defina a função vence, que recebe duas instâncias de Jogada j1 e j2 e retorna
True se j1 vence j2 no jogo. Ex:
> Pedra ‘vence’ Tesoura
True
> Tesoura ‘vence’ Pedra
False
> Tesoura ‘vence’ Tesoura
False
b) Defina a função vencedoras que recebe uma lista de duplas de jogadas e retorna
uma lista com as jogadas vencedoras. No caso de uma dupla que resulte em empate,
retorna o primeiro elemento da tupla. Ex:
> vencedoras [(Pedra,Tesoura),(Tesoura,Pedra), (Tesoura,Tesoura)]
[Pedra, Pedra, Tesoura]

-}
data Jogada = Pedra|Papel |Tesoura deriving (Eq,Show)

vence::Jogada->Jogada->Bool

vence Pedra Papel=False
vence Tesoura Pedra=False
vence Papel Tesoura=False
vence Tesoura Papel=True
vence Pedra Tesoura=True
vence Papel Pedra=True
vence Tesoura Tesoura=False
vence Papel Papel =False
vence Pedra Pedra =False

{-
b) Defina a função vencedoras que recebe uma lista de duplas de jogadas e retorna
uma lista com as jogadas vencedoras. No caso de uma dupla que resulte em empate,
retorna o primeiro elemento da tupla. Ex:
> vencedoras [(Pedra,Tesoura),(Tesoura,Pedra), (Tesoura,Tesoura)]
[Pedra, Pedra, Tesoura]
-}

vencedoras::(Eq Jogada)=>[(Jogada,Jogada)]->[Jogada]
vencedoras []=[]
vencedoras ((j1,j2):xs) |j1==Tesoura && j2==Papel=j1:vencedoras xs
                        | j1 == Pedra && j2==Tesoura=j1:vencedoras xs
                        | j1 == Papel && j2==Pedra=j1:vencedoras xs
                        | j1 == Pedra && j2==Pedra=j1:vencedoras xs
                        | j1 == Papel && j2==Papel=j1:vencedoras xs
                        | j1 == Tesoura && j2==Tesoura=j1:vencedoras xs
                        |j1==Papel && j2==Tesoura=j2:vencedoras xs
                        | j1 == Tesoura && j2==Pedra=j2:vencedoras xs
                        | j1 == Pedra && j2==Papel=j2:vencedoras xs
                        |otherwise =vencedoras xs


   {-
   Exercício 8) Lógica Nebulosa
a) Crie o tipo algébrico Nebuloso que pode ter os valores Verdadeiro; Falso; ou Talvez que
define um conceito intermediário entre Verdadeiro e Falso. No caso do valor Talvez, ele deve
ser associado a ele um valor float que representa o grau de pertinência. O valores Verdadeiro e
Falso não devem estar associados a valores. 
--}
--sim,nao,talvez
data Nebuloso = Verdadeiro |Falso |Talvez  deriving (Show)

{-
b) Crie uma função fuzzifica que recebe um valor numérico (Float) representando o grau de
pertinência e retorna um conceito Nebuloso, com o valor Falso caso o grau seja menor ou igual
a 0, Verdadeiro se for maior ou igual a 1 e Talvez associado ao grau de pertinência, caso
contrário.
-}

fuzzifica::Float->Nebuloso
fuzzifica grau
  | grau<=0 = Falso
  | grau >=1 = Verdadeiro
  | otherwise = Talvez

{-c) Considere a definição nebulosa do conceito “pessoa alta” como: dada a altura da pessoa ,
então pertinência a pessoa alta é dada por (altura-1,70)/0,20 -}

pessoa_alta::Float->Nebuloso
pessoa_alta altura
     | (altura-1.70)/0.20<=0 = Falso
     | (altura-1.70)/0.20>=1 = Verdadeiro
     | otherwise = Talvez



{--d) Considere a definição nebulosa do conceito “carro barato” como: dado o custo de um carro,
então pertinência a carro barato é dado por: (50.000-Custo)/ 20.000--}
carro_barato::Float->Nebuloso
carro_barato preco
  | ((50.000-preco)/ 20.000)<=0 = Falso
  | ((50.000-preco)/ 20.000) >=1 = Verdadeiro
  | otherwise = Talvez





{-
Exercício 9) a) Defina o Tipo Algébrico Estudante para registrar informações de alunos
de universidades e colégios. O objetivo é realizar a união das informações destes
estudantes. Cada ocorrência de estudante deve ter um identificador que diferencie
universitários de colegiais. Além disso, se for um aluno de colégio, devem existir as
informações: ano (1o
, 2o
 ou 3o
), nome do colégio (Nacional, Olimpo e Gabarito)
matricula (5 letras), altura e peso. Se for um aluno universitário, devem existir as
informações: nome da universidade (UFU, UNITRI e UNA), nome do curso
(Computação, Medicina, Direito e Música), matricula (8 dígitos), altura e idade. Crie
uma pequena base de estudantes com pelo menos 20 instâncias, sendo 10 alunos
colegiais e 10 alunos universitários. Em cada grupo, 3 alunos devem ter altura abaixo
de 1,70, 5 alunos devem ter altura entre 1,70 e 1,90 e 2 alunos devem ter altura acima
de 1,90.
-}
type Nome_universidade = String 
type Nome_Curso= String
type Matricula_U = Int
type Peso_U =Float 
type Idade_U = Int
type Altura_U =Float 
type Ano = String 
type Nome_colegio =String 

type Matricula_C =Int
type Altura =Float 
type Peso_C=Float 

data Estudante = Universidade Nome_universidade Nome_Curso Matricula_U Peso_U Idade_U  Altura_U| Colegio Ano Nome_colegio Matricula_C Altura Peso_C 
  deriving (Show)
    
    
    
    
aluno1 :: Estudante
aluno1 =Colegio "1º" "Nacional" 12345 1.50 7

aluno2 :: Estudante
aluno2=Colegio "2º" "atenas" 101112131415 1.60 8

aluno3 :: Estudante
aluno3=Colegio "3º" "Escola Estadual Messias Pedreiro - EEMP" 101112131415 1.65 72.00

aluno4 ::Estudante
aluno4= Universidade "UFU" "Ciencia da coputacao" 12345678  62.00 19 1.70


aluno5 :: Estudante
aluno5= Universidade "Unitri" "engenharia" 81234468  78.00 20 1.75

aluno6 :: Estudante
aluno6= Universidade "Uniessa" "medicina" 11554766  82.00 20 1.80
aluno7 :: Estudante
aluno7= Universidade "Pitguras" "Administracao" 25489275  86.00 20 1.85
aluno8 :: Estudante
aluno8= Universidade "Unipac" "BSI" 42891011  86.00 23 1.90

aluno9 :: Estudante
aluno9= Universidade "Usp" "direito" 25489275  70.00 29 1.95

aluno10 :: Estudante
aluno10= Universidade "Usp" "Engenharia mecatronica" 25489275  70.00 29 1.93

aluno11 :: Estudante
aluno11= Universidade "Unipac" "BSI" 42891011  86.00 23 1.85

aluno12 :: Estudante
aluno12= Universidade "Usp" "direito" 25489275  70.00 29 1.66

aluno13 :: Estudante
aluno13= Universidade "Usp" "Engenharia mecatronica" 25489275  70.00 29 1.79


aluno14 :: Estudante
aluno14=Colegio "2º serie" "EM Prof Leôncio do Carmo Chaves" 512348 1.72 8

aluno15 :: Estudante
aluno15=Colegio "4º serie" "EM Prof Leôncio do Carmo Chaves" 84235 1.82 11

aluno16 :: Estudante
aluno16=Colegio "5º serie" "Teotônio Vilela" 987456 1.54 13

aluno17 :: Estudante
aluno17=Colegio "6º serie" "Teotônio Vilela" 56489 1.54 14

aluno18 :: Estudante
aluno18 =Colegio "1º" "Nacional" 69854 1.68 9

aluno19 :: Estudante
aluno19 =Colegio "1º" "Gabarito" 79214 1.50 7

aluno20 :: Estudante
aluno20 =Colegio "4º" "Olimpo" 79214 1.65 12

lista_alunos :: [Estudante]
lista_alunos=[aluno1,aluno2,aluno3,aluno4,aluno5,aluno6,aluno7,aluno8,aluno9,aluno10,aluno11,aluno12,aluno13,aluno14,aluno15,aluno16,aluno17,aluno18,aluno19,aluno20]





descobre_altos::[Estudante]->[(Int,Nebuloso)]
descobre_altos []=[]
descobre_altos (Universidade _ _ matricula_u _ _ altura_U:xs) =(matricula_u,pessoa_alta altura_U):descobre_altos xs
descobre_altos (Colegio _ _ matricula_c altura _:xs)=(matricula_c,pessoa_alta altura):descobre_altos xs



{-
Exercício 10) Considere o tipo algébrico ArvBinInt visto em sala para representar
árvores binárias que armazenam números inteiros. Elabore as funções a seguir que
manipulam árvores binárias:
folhas: recebe uma árvore binária e devolve uma listagem com todos os nós
folhas (não internos) existentes na árvore. Percorrer a árvore em préordem.
somaNosinternos: somar os valores de todos os elementos da árvore binária que
são nós internos. Percorrer a árvore em pós-ordem.
pertence: recebe um valor inteiro e verifica se esse valor é igual a algum dos
elementos da árvore binária. Percorrer a árvore em ordem simétrica.


-}
data ArvoreBinInt = Nulo | No Int ArvoreBinInt ArvoreBinInt
  deriving Show

arvEx :: ArvoreBinInt
arvEx = (No 2 (No 7 (No 2 Nulo Nulo)
  (No 6 (No 5 Nulo Nulo)
  (No 11 Nulo Nulo)))
  (No 5 Nulo
  (No 9 (No 4 Nulo Nulo)
  Nulo)))

folhas :: ArvoreBinInt -> [Int]
folhas Nulo = []
folhas (No n Nulo Nulo) = [n]
folhas (No _ esq dir) = folhas esq ++ folhas dir

soma_arvore::ArvoreBinInt -> Int
soma_arvore Nulo = 0
soma_arvore (No n Nulo Nulo) = n
soma_arvore (No n esq dir) = soma_arvore esq + soma_arvore dir + n



pertenceArv::Int -> ArvoreBinInt -> Bool
pertenceArv x Nulo = False
pertenceArv x (No v esq dir) = x==v || if x<v
                                  then (pertenceArv x esq)
                          else (pertenceArv x dir)