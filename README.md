Compilar:
ghc -o main main.hs

Rodar:
./main

// TO DO

- criar função que recebe a posição de uma célula e retorna as posições dos seus vizinhos (base para percorrer a matriz) (leo)

- preencher os candidatos das células com valor == -1 com os valores possíveis para aquela célula (matheus)

- ao inserir um valor na célula, verificar se o valor é um candidato (leo)

- ao inserir um valor em uma célula, apagar o valor do candidato no grupo e nos adjacentes (leo)

- função que recebe a posição da célula e retira o valor de lá, colocando o valor retirado nos candidatos das células adjacentes (matheus)

----

- criar função que percorre a matriz procurando por células que tenham somente 1 vizinho e inserir esse valor nele (matheus)

- criar função que pega uma célula aleatória (de preferência que tenha menos candiatos ?) e coloca um valor nela (start do backtracking) (vamos pensar no seu caso, seu safado)

----
