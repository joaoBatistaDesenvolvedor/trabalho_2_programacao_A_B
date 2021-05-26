{-
Para os exercícios de ordenação, considere as 14 listas a seguir como exemplos para teste dos
diversos algoritmos:
l1=[1..2000]
l2=[2000,1999..1]
l3=l1++[0]
l4=[0]++l2
l5=l1++[0]++l2
l6=l2++[0]++l1
l7=l2++[0]++l2
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7 = [20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]

-}

l1 :: [Integer]
l1 = [1 .. 2000]

l2 :: [Integer]
l2 = [2000, 1999 .. 1]

l3 :: [Integer]
l3 = l1 ++ [0]

l4 :: [Integer]
l4 = 0 : l2

l5 :: [Integer]
l5 = l1 ++ [0] ++ l2

l6 :: [Integer]
l6 = l2 ++ [0] ++ l1

l7 :: [Integer]
l7 = l2 ++ [0] ++ l2

x1 :: [Integer]
x1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

x2 :: [Integer]
x2 = [20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

x3 :: [Integer]
x3 = [11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

x4 :: [Integer]
x4 = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11]

x5 :: [Integer]
x5 = [11, 12, 13, 14, 15, 5, 4, 3, 2, 1, 16, 17, 18, 19, 20, 10, 9, 8, 7, 6]

x6 :: [Integer]
x6 = [1, 12, 3, 14, 5, 15, 4, 13, 2, 11, 6, 17, 8, 19, 20, 10, 9, 18, 7, 16]

x7 :: [Integer]
x7 = [20, 8, 2, 11, 13, 3, 7, 18, 14, 4, 16, 10, 15, 1, 9, 17, 19, 12, 5, 6]



{-
enunciado

Exercício 1) Em relação ao algoritmo de ordenação Bolha, faça as implementações em Haskell
solicitadas abaixo. Em todas as variações abaixo, incluindo o código original, você deve incluir
um contador de trocas realizadas durante a ordenação.
- Original Bolha: implementação do código visto em aula.
- Variação 1: parada do algoritmo é antecipada quando uma iteração de trocas é finalizada sem
que nenhuma troca efetiva seja realizada na iteração completa.
- Variação 2: faz parada antecipada e, a cada iteração de trocas, a avaliação é realizada
desconsiderando-se o último elemento cuja posição foi fixada. Ou seja, diminui o tamanho da
lista a ser ordenada a cada iteração.
- Realizar execuções comparativas nas listas dadas como exemplo entre os algoritmos das
variações e do algoritmo original para avaliar: 1) o número de trocas em cada execução; 2) o
tempo de execução (apenas verifiquem se existe uma mudança aparente de tempo de
processamento). Apresentar esses resultados em um arquivo PDF, fazendo uma análise dos
resultados encontrados.

-}
{-
percorre os elementos de 2  a 2
se o x > y jogo y pra lista de saida mais trocando o x eo y
de lugar
caso contrario eu permaneco com os elemento nas suas posicoes sem fazer alteracoes

-}
{-
original
-}
bolha :: (Num b, Ord a) => [a] -> ([a], b)
bolha [] = ([],0)
bolha lista = bolhaOrd  lista 0 (length lista)



bolhaOrd :: (Eq t1, Num t1, Ord a, Num t2) => [a] -> t2 -> t1 -> ([a], t2)
bolhaOrd lista i 0 = (lista,i)

bolhaOrd lista i n = bolhaOrd lst j (n-1)

    where (lst,j) = trocas (lista,i)



trocas :: (Ord a, Num b) => ([a], b) -> ([a], b)
trocas ([x],i) = ([x],i)

trocas ((x:y:zs),i)

 | x > y = ((y:lst1),j1)

 | otherwise = ((x:lst2),j2)

     where   (lst1,j1) = trocas((x:zs),(i+1))   -- como inverte x com y soma 1 no contador
             (lst2,j2) = trocas((y:zs),(i))     -- como não inverte x com y mantem o valor



--variação 1

bolha_v2 :: (Num b, Ord a) => [a] -> ([a], b)
bolha_v2 [] = ([],0)
bolha_v2 lista = bolhaOrd  lista 0 (length lista)



bolhaOrd_v2 :: (Eq t1, Num t1, Ord a, Num t2) => [a] -> t2 -> t1 -> ([a], t2)
bolhaOrd_v2 lista i 0 = (lista,i)

bolhaOrd_v2 lista i n =if(lista ==lst) then (lst,j) else bolhaOrd_v2 lst j (n-1)
    where (lst,j) = trocas_v2 (lista,i)



trocas_v2 :: (Ord a, Num b) => ([a], b) -> ([a], b)
trocas_v2 ([x],i) = ([x],i)

trocas_v2 ((x:y:zs),i)

 | x > y = ((y:lst1),j1)
 | otherwise = ((x:lst2),j2)

     where   (lst1,j1) = trocas_v2((x:zs),(i+1))   -- como inverte x com y soma 1 no contador
             (lst2,j2) = trocas_v2((y:zs),(i))     -- como não inverte x com y mantem o valor






-- variacao2

bolha_v3 :: (Ord a) => [a] -> ([a],Int )
bolha_v3 [] = ([],0)
bolha_v3 lista = bolhaOrd_v3  lista 0 (length lista-1)


bolhaOrd_v3 :: (Ord a) => [a] -> Int  -> Int -> ([a], Int )
bolhaOrd_v3 lista i 0 = (lista,i)

bolhaOrd_v3 lista i n =if(lista ==lst) then (lst,j) else bolhaOrd_v3 lst j (n-1)
    where (lst,j) = trocas_v3 (lista,i) n  



trocas_v3 :: (Ord a) => ([a], Int ) ->Int ->([a], Int )
trocas_v3 (x,i) 0 = (x,i)

trocas_v3 ((x:y:zs),i) n

 | x > y = ((y:lst1),j1)
 | otherwise = ((x:lst2),j2)

     where   (lst1,j1) = trocas_v3((x:zs),(i+1)) (n-1)   -- como inverte x com y soma 1 no contador
             (lst2,j2) = trocas_v3((y:zs),(i)) (n-1)
{-

Exercício 2) Em relação ao algoritmo de ordenação Selecao, faça as implementações em
Haskell solicitadas abaixo. Em todas as variações abaixo, incluindo o código original, você
deve incluir um contador de trocas realizadas durante a ordenação.
- Original Seleção: implementação do código visto em aula.
- Variação1: Refaça o código original para que a busca pelo menor elemento (função mínimo) e
a eliminação desse menor elemento da lista a ser ordenada (função remove) ocorra numa
mesma função (remove_menor), sem a necessidade de se percorrer a lista duas vezes a cada
iteração.
- Variação 2: Refaça a implementação do algoritmo Seleção usando funções genéricas (foldr ou
foldr1) .
- Realizar execuções comparativas nas listas dadas como exemplo entre os algoritmos das
variações e do algoritmo original para avaliar: 1) o número de trocas em cada execução; 2) o
tempo de execução (apenas verifiquem se existe uma mudança aparente de tempo de
processamento). Apresentar esses resultados em um arquivo PDF, fazendo uma análise dos
resultados encontrados.
-}


{-
Original Seleção: implementação do código visto em aula.
- Variação1: Refaça o código original para que a busca pelo menor elemento (função mínimo) e
a eliminação desse menor elemento da lista a ser ordenada (função remove) ocorra numa
mesma função (remove_menor), sem a necessidade de se percorrer a lista duas vezes a cada
iteração.
-}
selecao:: (Ord a) => [a]->([a],Int )
selecao [] = ([],0)
selecao (cabeca:xs) = let 
                        x = minimo (cabeca:xs)
                        (listaresultante, contaTroca) = selecao (remove x (cabeca:xs))
                       in if x == cabeca then  (x:listaresultante, contaTroca) else (x:listaresultante, contaTroca + 1)






remove:: (Ord a) => a->[a]->[a]
remove a [] = []
remove a (x:xs)
    | a==x = xs
    | otherwise = x:(remove a xs)

minimo::(Ord a) => [a]->a
minimo [] = undefined
minimo [x] = x
minimo (x:xs)
    | x <= (minimo xs) = x
    | otherwise = minimo xs


{-

- Variação1: Refaça o código original para que a busca pelo menor elemento (função mínimo) e
a eliminação desse menor elemento da lista a ser ordenada (função remove) ocorra numa


-}
selectionsort2T::Ord a=>[a]->([a],Int)
selectionsort2T []=([],0)
selectionsort2T xs = (x:zs,cont1+cont2)
                        where 
                             (ys,x,cont1)= (remove_menorT xs)
                             (zs,cont2)=selectionsort2T ys

remove_menorT::Ord a=>[a]->([a],a,Int)
remove_menorT [x] =([],x,0)
remove_menorT (x:xs)
                   | x2>x = (x2:xs2,x,cont)
                   | otherwise = (x:xs2,x2,cont+1)
                   where
                     (xs2,x2,cont)=remove_menorT xs
{-
 Variação 2: Refaça a implementação do algoritmo Seleção usando funções genéricas (foldr ou
foldr1) .
-}



selecao_foldr1:: (Ord a) => [a]->([a],Int )
selecao_foldr1 [] = ([],0)
selecao_foldr1 (cabeca:xs) = let 
                        x = (foldr1 min   (cabeca:xs))
                        (listaresultante, contaTroca) = selecao (remove x (cabeca:xs))
                       in if x == cabeca then  (x:listaresultante, contaTroca) else (x:listaresultante, contaTroca + 1)






remove_foldr1:: (Ord a) => a->[a]->[a]
remove_foldr1 a [] = []
remove_foldr1 a (x:xs)
    | a==x = xs
    | otherwise = x:(remove_foldr1 a xs)






{-

Exercício 3) Em relação ao algoritmo de ordenação Inserção, faça as implementações em
Haskell solicitadas abaixo. Em todas as variações abaixo, incluindo o código original, você
deve incluir um contador de comparações realizadas durante a ordenação.
- Original Inserção: implementação do código visto em aula.
- Variação 1: Refaça a implementação do algoritmo Inserção usando funções genéricas (foldr
ou foldr1).
- Realizar execuções comparativas nas listas dadas como exemplo entre os algoritmos das
variações e do algoritmo original para avaliar: 1) o número de trocas em cada execução; 2) o
tempo de execução (apenas verifiquem se existe uma mudança aparente de tempo de
processamento). Apresentar esses resultados em um arquivo PDF, fazendo uma análise dos
resultados encontrados.

-}

insertionsortT::Ord a=>[a]->([a],Int)
insertionsortT [] =([],0)
insertionsortT (x:xs)= insertionsort2 [x] xs 


insertionsort2::Ord a=>[a]->[a]->([a],Int)
insertionsort2 xs []= (xs,0)
insertionsort2 xs (y:ys)= (z,cont1+cont2)
                           where
                                 (z,cont2)=insertionsort2 zs ys
                                 (zs,cont1)=(insereordT y xs)

insereordT::Ord a=>a->[a]->([a],Int)
insereordT x [] = ([x],0)
insereordT x (y:ys)
                   | x<=y = (x:y:ys,1)
                   | otherwise= (y:zs,cont+1)
                      where
                      	(zs,cont)=insereordT x ys


insereordT_foldr::Ord z=>z->([z],Int)->([z],Int)

insereordT_foldr x ([],contador) = ([x],contador)

insereordT_foldr x ((y:ys),contador)
                   | x<=y = (x:y:ys,(contador+1))
                   | otherwise= (y:zs,cont)
                      where
                          (zs,cont)=insereordT_foldr x (ys,(contador+1))


insercao_foldr :: Ord a=>[a]->([a], Int)
insercao_foldr =foldr insereordT_foldr ([],0)



{-

Exercício 4) Em relação ao algoritmo de ordenação quicksort visto em sala de aula e
laboratório. Em todas as variações abaixo, incluindo o código original, você deve incluir um
contador de comparações realizadas durante a ordenação.
- Original Quicksort: implementação do código visto em aula.
- Variação 1: modifique o algoritmo original para que ao invés dos elementos maiores e
menores serem encontrados com buscas independentes, que seja elaborada e utilizada a
função divide que percorre a lista uma única vez, retornando os elementos menores em uma
lista e os elementos menores em outra.
EX: > divide 'j' "pindamonhangaba" Resposta: ("idahagaba","pnmonn")
- Variação 2: modifique a variação 1 para que o elemento pivô seja obtido a partir da análise
dos 3 primeiros elementos da lista, sendo que o pivô será o elemento mediano entre eles.
Exemplo: na lista [3, 9, 4, 7, 8, 1, 2], os elementos 3, 9 e 4 seriam analisados e o pivô escolhido
seria 4. Caso a lista a ser analisada tenha menos que 3 elementos, o pivô é sempre o primeiro.
- Realizar execuções comparativas nas listas dadas como exemplo entre os algoritmos da
Variação 1, 2 e do algoritmo original para avaliar: 1) o número de comparações em cada
execução; 2) o tempo de execução (apenas verifiquem se existe uma mudança aparente de
tempo de processamento). Apresentar esses resultados em um arquivo PDF, fazendo uma
análise dos resultados encontrados.
-}


quicksort::(Ord a) => [a] -> ([a],Int )
quicksort [] = ([],0)
quicksort (s:xs) = (listaM++ [s] ++listaMaiores,total+1)
    where
            (listaM,contaM)=quicksort [x|x <- xs,x < s] 
            (listaMaiores,contaMaiores)=quicksort [x|x <- xs,x >= s] 
            nummaior=sum [1|x <- xs,x >= s]
            nummenor=sum [1|x <- xs,x < s]
            total=nummaior+nummenor

{-
- Variação 1: modifique o algoritmo original para que ao invés dos elementos maiores e
menores serem encontrados com buscas independentes, que seja elaborada e utilizada a
função divide que percorre a lista uma única vez, retornando os elementos menores em uma
lista e os elementos menores em outra
-}






quicksort2T::Ord a=>[a]->([a],Int)
quicksort2T []=([],0)
quicksort2T (x:xs)= ( ys1++[x]++ys2,cont+cont2+cont3)
                   where
                        (zs1,zs2,cont)= divideT x xs
                        (ys1,cont2)=quicksort2T(zs1)
                        (ys2,cont3)=quicksort2T(zs2)


divideT::Ord a=>a->[a]->([a],[a],Int)
divideT _ []=([],[],0)
divideT x (y:ys)
                | x>y = (y:zs1,zs2,cont+1)
                | otherwise =(zs1,y:zs2,cont+1)
                   where
                       (zs1,zs2,cont)=divideT x ys



--V3)

{-

Variação 2: modifique a variação 1 para que o elemento pivô seja obtido a partir da análise
dos 3 primeiros elementos da lista, sendo que o pivô será o elemento mediano entre eles.
Exemplo: na lista [3, 9, 4, 7, 8, 1, 2], os elementos 3, 9 e 4 seriam analisados e o pivô escolhido
seria 4. Caso a lista a ser analisada tenha menos que 3 elementos, o pivô é sempre o primeiro

-}
quicksort3T::Ord a=>[a]->([a],Int)
quicksort3T []=([],0)
quicksort3T xs = (ys1++[pivo]++ys2,cont+cont2+cont3+1)
                   where
                        (ys,pivo,cont4)=esc_pivoT xs
                        (zs1,zs2,cont)= divide2T pivo ys
                        (ys1,cont2)=quicksort2T(zs1)
                        (ys2,cont3)=quicksort2T(zs2)


esc_pivoT::Ord a=>[a]->([a],a,Int)
esc_pivoT [x]=([],x,0)
esc_pivoT (x:y:[])=([y],x,0)
esc_pivoT (x:y:z:xs)
                    | (x>=y && x<=z) || (x<y && x>z) = (y:z:xs,x,4)
                    | (y>=x && y<=z) || (y<x && y>z) =(x:z:xs,y,8)
                    |otherwise = (x:y:xs,z,8)



divide2T::Ord a=>a->[a]->([a],[a],Int)
divide2T _ []=([],[],0)
divide2T x (y:ys)
                | x>y = (y:zs1,zs2,cont+1)
                | otherwise =(zs1,y:zs2,cont+1)
                   where
                       (zs1,zs2,cont)=divide2T x ys



{-
Exercício 5)
- Pesquise e implemente em Haskell o algoritmo mergesort, incluindo um contador de
comparações elementares realizadas durante a ordenação.
- Realizar execuções comparativas nas listas dadas como exemplo entre os algoritmos
selecionados como as melhores versões do Seleção e Quicksort, além do algoritmo Mergesort,
para avaliar: 1) o número de comparações em cada execução; 2) o tempo de execução (apenas
verifiquem se existe uma mudança aparente de tempo de processamento). Apresentar esses
resultados em um arquivo PDF, fazendo uma análise dos resultados encontrados.
Obs: as análises solicitadas nos exercícios 1, 2, 3, 4 e 5, devem ser entregues em um único
arquivo PDF, que deve ser enviado junto com os códigos dos exercícios.

-}


merge_sortT::Ord a=>[a]->([a],Int)
merge_sortT [] = ([],0)
merge_sortT xs = (zs,cont1+cont2)
                 where
                  (ys,cont1)= dividT xs
                  (zs,cont2)=merge_sort2T ys


merge_sort2T::Ord a=>[[a]]->([a],Int)
merge_sort2T [] = ([],0)
merge_sort2T [x]= (x,0)
merge_sort2T (x:y:xs)= (z3,cont3+cont1+cont2)
                       where
                        (z,cont1)= merge_sort3T x y
                        (z2,cont2) = merge_sort2T xs
                        (z3,cont3)=merge_sort3T z z2


dividT::Ord a=>[a]->([[a]],Int)
dividT [x]= ([[x]],0)
dividT (x:y:xs) 
                |x<y=(([x,y]:zs),cont+1)
                |otherwise = (([y,x]:zs),cont+1)
                where
                  (zs,cont)= dividT xs

merge_sort3T::Ord a=>[a]->[a]->([a],Int)
merge_sort3T x [] = (x,0)
merge_sort3T [] x = (x,0)
merge_sort3T (x:xs) (y:ys)
                          | x<y = (x:zs1,cont1+1)
                          | otherwise = (y:zs2,cont2+1)
                             where
                              (zs1,cont1)=merge_sort3T xs (y:ys)
                              (zs2,cont2)=merge_sort3T (x:xs) ys



