# Base de dados
Em astronomia, a classificação estelar é a classificação das estrelas com base em suas características espectrais. O esquema de classificação de galáxias, quasares e estrelas é um dos mais fundamentais em astronomia. A catalogação inicial de estrelas e sua distribuição no céu levou ao entendimento de que elas compõem nossa própria galáxia e, seguindo a distinção de que Andrômeda era uma galáxia separada da nossa, inúmeras galáxias começaram a ser pesquisadas à medida que telescópios mais poderosos foram construídos. . Este datasat visa classificar estrelas, galáxias e quasares com base em suas características espectrais.

Os dados consistem em 100.000 observações do espaço feitas pelo SDSS (Sloan Digital Sky Survey). Cada observação é descrita por 17 colunas de características e 1 coluna de classe que a identifica como uma estrela, galáxia ou quasar.

## Variáveis

obj_ID = Object Identifier, o valor único que identifica o objeto no catálogo de imagens utilizado pelo CAS

alfa = ângulo de Ascensão Reta (na época J2000)

delta = Ângulo de declinação (na época J2000)

u = filtro ultravioleta no sistema fotométrico

g = filtro verde no sistema fotométrico

r = Filtro vermelho no sistema fotométrico

i = Filtro de infravermelho próximo no sistema fotométrico

z = Filtro infravermelho no sistema fotométrico

run_ID = Run Number usado para identificar a varredura específica

rereun_ID = Número de reexecução para especificar como a imagem foi processada

cam_col = Coluna da câmera para identificar a linha de varredura na execução

field_ID = Número do campo para identificar cada campo

spec_obj_ID = ID exclusivo usado para objetos espectroscópicos ópticos (isso significa que 2 observações diferentes com o mesmo spec_obj_ID devem compartilhar a classe de saída)

class = classe do objeto (objeto galáxia, estrela ou quasar)

redshift = valor de redshift baseado no aumento do comprimento de onda

placa = ID da placa, identifica cada placa no SDSS

MJD = Data Juliana Modificada, usada para indicar quando um dado dado do SDSS foi obtido

fiber_ID = ID da fibra que identifica a fibra que apontou a luz no plano focal em cada observação

