# Estruturas de dados puramente funcionais - Dia 02
http://pesquisa.ufabc.edu.br/haskell/cursos/19.q3.eds_funcionais/

Universidade Federal do ABC

Emilio Francesquini

e.francesquini@ufabc.edu.br

2019.Q3

Código e exercícios relativos ao segundo dia do curso.
- [Slides](http://pesquisa.ufabc.edu.br/haskell/cursos/19.q3.eds_funcionais/files/dia02.pdf)
  - Toda a explicação do código pode ser encontrada nos slides de aula.


Estes exercícios envolvem a alteração do código dado em aula. Uma suite de testes (parcial!) que testa apenas os casos 'happy day' foi fornecida. Para executá-la basta fazer `stack test` no diretório raiz. Caso deseje, estenda a suite de testes com os seus próprios.

## Exercícios

  1. Escreva uma função `paraArvore` que receba uma lista ordenada de elementos, sem duplicação, e que devolva uma árvore rubro-negra bem formada. Sua função deve rodar em tempo O(n).

  2. A função `balance` tal como está faz diversos testes desnecessários. Por exemplo, quando a função `insert` faz a chamada recursiva no filho esquerdo, não é necessário que sejam feitas verificações de quebras da regra 2 (nenhum nó vermelho tem filhos vermelhos) no filho direito.

     a) Quebre a função `balance` em duas funções `lbalance` e `rbalance` que testem por violações envolvendo os filhos esquerdo e direito respectivamente. Substitua as chamadas à `balance` pelas novas funções.

     b) Estendendo um pouco mais a lógica adotada no item a, um dos testes nos netos de um nó também é desnecessário. Modifique a função `insert` para que ela nunca teste a cor de nós no caminho da busca.

  3. Altere a implementação da função `insert` para heaps esquerdistas `src/Heaps.hs` para que ela insira o elemento diretamente no Heap, sem o auxílio da função `merge`.

  4. Qual a complexidade da execução da função `fromList` no caso de heaps esquerdistas? E para heaps binomiais?

  5. Uma das vantagens da atual implementação dos heaps esquerdistas em comparação aos heaps binomiais é o fato de que `head` leva tempo O(1) em vez do tempo O(lg n) alcaçado por heaps binomiais. Altere a implementação dos heaps binimiais para que:
    - `head` leve O(1)
    - `tail`, `insert` e `merge` executem em O(lg n)
