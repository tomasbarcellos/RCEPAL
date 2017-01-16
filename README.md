# RCEPAL

Este pacote contém funções para download dos dados disponíveis na API da [CEPALstat](http://estadisticas.cepal.org/cepalstat/WEB_CEPALSTAT/openDataAPI.asp?idioma=i).

# Instalação

```
install.packages("devtools")
devtools::install_github("tomasbarcellos/RCEPAL")
library('RCEPAL')
```
# Exemplo

```
# Retorna data.frame com séries disponíveis na
stat_disp <- series_CEPAL()

# Retorna tabela com indicadores de termos de troca
termos_troca <- CEPAL_stat(id_serie = 883)
```

# Contribuições

Este trabalho ainda está em desenvolvimento. Caso encontre algum problema ou tenha alguma sugestão, faça-as [aqui](https://github.com/tomasbarcellos/RCEPAL/issues)
