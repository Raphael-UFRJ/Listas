# Simulação de Probabilidade de Perda em Carteiras de Investimentos

Este projeto simula a probabilidade de perda em uma carteira de investimentos composta por 10 ativos, considerando diferentes coeficientes de correlação entre os ativos. Utilizamos a função `mvrnorm` do pacote `MASS` em R para gerar amostras normais correlacionadas e calcular a probabilidade de que o retorno total da carteira seja negativo.

## Requisitos

- R (versão 3.6 ou superior)
- Pacote `MASS`

## Instalação

Se você ainda não possui o R instalado, faça o download e a instalação a partir do site oficial: [https://www.r-project.org/](https://www.r-project.org/).

Para instalar o pacote `MASS`, execute o seguinte comando no console R:

```R
install.packages("MASS")
```

## Uso
1. Clone este repositório ou baixe os arquivos para o seu computador.
2. Abra o R ou RStudio.
3. Carregue o código-fonte principal (por exemplo, simulacao_probabilidade_perda.R).

## Conclusão:
Sim, a diversificação com 10 investimentos idênticos reduz o risco de perda de forma considerável.

- Redução do Risco: Observamos que, à medida que a correlação entre os investimentos aumenta, a probabilidade de perda diminui. Isso ocorre porque a diversificação com ativos correlacionados reduz o impacto de eventos negativos em um único investimento sobre o retorno total da carteira.
- Significância: A redução de 22,69% na probabilidade de perda entre rho = 0 e rho = 0,9 demonstra o efeito positivo da diversificação.

### Considerações Adicionais:

- Diversificação e Correlação: É importante notar que a diversificação não elimina totalmente o risco, mas sim o reduz. Além disso, o efeito da diversificação depende do nível de correlação entre os investimentos. Ativos com alta correlação tendem a apresentar movimentos semelhantes, reduzindo o benefício da diversificação.
- Outras Formas de Diversificação: A diversificação não se limita apenas a investimentos idênticos. É possível diversificar investindo em diferentes classes de ativos, setores da economia e regiões geográficas.
- Análise Detalhada: Uma análise mais aprofundada pode considerar diferentes distribuições de retorno, além de métricas de risco como o desvio padrão e o Value at Risk (VaR).

### Recomendações:
- Diversificação Estratégica: É crucial diversificar a carteira de investimentos de forma estratégica, considerando o perfil de risco do investidor e os objetivos financeiros.
- Assessoria Profissional: Consultar um profissional de investimentos pode auxiliar na construção de uma carteira diversificada e adequada às necessidades individuais.

### Conclusão:
A diversificação com 10 investimentos idênticos é uma forma eficaz de reduzir o risco de perda, especialmente quando os investimentos possuem alta correlação. No entanto, é importante considerar outras formas de diversificação e buscar orientação profissional para construir uma carteira de investimentos sólida e alinhada com seus objetivos.
