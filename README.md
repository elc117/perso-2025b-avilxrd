## Simulador de Aplicativo de Conversas
###### Miguel Avila de Oliveira, Sistemas de Informação

### Proposta
O objetivo do trabalho é desenvolver um aplicativo de mensagens que permite aos usuários enviar, armazenar e recuperar mensagens de um banco de dados. O aplicativo formata as mensagens de forma a simular uma conversa. <br>
Podemos pensar como uma caixa de correios. Podemos deixar mensagens na caixa de outras pessoas e verificar se há mensagens na nossa caixa de correios. O problema é que para simular uma conversa, precisamos ficar "verificando a caixa de correios" constantemente. Na solução encontrada, por conta da implementação das funções que mostram as informações no front-end, ocorre um flickering, pois os contatos ficam sendo carregados constantemente.

### Desenvolvimento
O desenvolvimento do aplicativo foi desafiador. Comecei com a implementação de uma interface gráfica e configuração das rotas. Em seguida uma implementação salvendo as mensagens na memória, que posteriormente foram substituidas pela implementação com banco de dados.<br>

Ao modificar as funções para utilizar banco de dados, consegui entender melhor como a linguagem estava se comportando na minha aplicação, e foi muito mais fácil o entendimento da linguagem a partir desse momento, inclusive ajudando no estudo para a avaliação de compreensão de código.<br>

### Problemas Encontrados
A maior fonte de problemas foi a alta necessidade de diferentes bibliotecas, a modularização do código e a pouca familiaridade com a linguagem.<br>

Comecei tentando separar as funções em Lib.hs e Main.hs, porém estava com muitos erros ao compilar o programa. A solução provisória foi fazer o programa todo no main e separar depois em funções melhores para facilitar a manutenção.<br>

Novamente, ao tentar separar em outros arquivos, tive muitos erros ao buildar o projeto. Relendo o código, consegui identificar os problemas e quais imports estavam gerando os problemas.

## Orientações para Execução
Para executar o aplicativo, siga as etapas abaixo:

**Instalação de Dependências:**
   - Certifique-se de ter o GHC e o Cabal instalados.
   - Clone o repositório:
     ```bash
     git clone [URL do repositório]
     cd [nome do diretório]
     ```
   - Instale as dependências:
     ```bash
     cabal update
     cabal install --only-dependencies
     ```
**Execução do Aplicativo:**
   - Compile e execute o aplicativo:
     ```bash
     cabal run
     ```
   - O aplicativo estará disponível em `http://localhost:3000`.

### Resultados Finais
[Assista o vídeo de demonstração] (https://youtu.be/-oG3fOEBMU0)

## Referências e Créditos
- [Haskell Documentation](https://www.haskell.org/documentation/)
- [Web.Scotty](https://hackage.haskell.org/package/scotty)
- [SQLite Simple](https://hackage.haskell.org/package/sqlite-simple)
