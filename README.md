
# Análise de série temporal com método de regressão linear de Prais Winsten

Vamos executar um script no R, aplicando de forma simples o método de Prais Winsten para descever e organzizar informações temporais em fenômenos de interesse para a saúde pública!

## Qual fenômeno?

Bom, a idéia do projeto é observar o comportamento do indicador Coeficiente de Mortalidade Infantil (CMI) no município de São Paulo (MSP) ao longo do tempo, captar a sua tendência temporal e quantificar a partir da regressão de Prais Winsten a porcentagem de variação anual desse indicador. 
Após vizualizarmos o comportamento do CMI para MSP, vamos desagregar essa informação por Distrito Administrativo (DA) e descobrir se existe uma iniquidade evidente atribuída ao fator espacial.

#### Direto ao assunto!Temos a nossa série temporal anual (2013-2019) nas linhas, e os Distritos nas colunas.

## Após coleta e organização, essa é a cara do banco de dados inicial:
###### Fonte dos dados: Sistema de Informações de Mortalidade (SIM), 2020.

| Ano  | Água Rasa   | Alto de Pinheiros | Anhanguera  | Aricanduva  | Artur Alvim | Bela Vista  | Belém       | Bom Retiro  | Brás        | Brasilândia | Cachoeirinha | Cambuci     | Campo Belo  | Campo Grande | Campo Limpo | Cangaíba    | Capão Redondo | Carrão      | Casa Verde  | Cidade Ademar | Cidade Dutra | Cidade Líder | Cidade Tiradentes | Consolação  | Cursino     | Ermelino Matarazzo | Freguesia do Ó | Grajaú      | Guaianases  | Iguatemi    | Ipiranga    | Itaim Bibi  | Itaim Paulista | Itaquera    | Jabaquara   | Jaçanã      | Jaguara     | Jaguaré     | Jaraguá     | Jardim Ângela | Jardim Helena | Jardim São Luís | José Bonifácio | Lajeado     | Lapa        | Liberdade   | Limão       | Mandaqui    | Moema       | Mooca       | Morumbi     | Parelheiros | Pari        | Parque do Carmo | Pedreira    | Penha       | Perdizes    | Perus       | Pinheiros   | Pirituba    | Ponte Rasa  | Raposo Tavares | República   | Rio Pequeno | Sacomã      | Santa Cecília | Santana     | Santo Amaro | São Domingos | São Lucas   | São Mateus  | São Miguel  | São Rafael  | Sapopemba   | Saúde       | Sé          | Socorro     | Tatuapé     | Tremembé    | Tucuruvi    | Vila Andrade | Vila Curuçá | Vila Formosa | Vila Guilherme | Vila Jacuí  | Vila Leopoldina | Vila Maria  | Vila Mariana | Vila Matilde | Vila Medeiros | Vila Prudente | Vila Sônia  | MSP         |
|------|-------------|-------------------|-------------|-------------|-------------|-------------|-------------|-------------|-------------|-------------|--------------|-------------|-------------|--------------|-------------|-------------|---------------|-------------|-------------|---------------|--------------|--------------|-------------------|-------------|-------------|--------------------|----------------|-------------|-------------|-------------|-------------|-------------|----------------|-------------|-------------|-------------|-------------|-------------|-------------|---------------|---------------|-----------------|----------------|-------------|-------------|-------------|-------------|-------------|-------------|-------------|-------------|-------------|-------------|-----------------|-------------|-------------|-------------|-------------|-------------|-------------|-------------|----------------|-------------|-------------|-------------|---------------|-------------|-------------|--------------|-------------|-------------|-------------|-------------|-------------|-------------|-------------|-------------|-------------|-------------|-------------|--------------|-------------|--------------|----------------|-------------|-----------------|-------------|--------------|--------------|---------------|---------------|-------------|-------------|
| 2013 | 7,751937984 | 5,698005698       | 12,48884924 | 10,08403361 | 8,778346745 | 11,24859393 | 2,991026919 | 9,310986965 | 16,10017889 | 11,93080135 | 12,71028037  | 5,714285714 | 8,793969849 | 10,48689139  | 11,28854626 | 9,387572642 | 12,78162912   | 15,09054326 | 14,80836237 | 10,93013288   | 14,1509434   | 11,72273191  | 14,97635313       | 2,415458937 | 12,92175162 | 15,59139785        | 10,83591331    | 11,77639047 | 8,337420304 | 16,02023609 | 7,42741391  | 4,359197908 | 12,58851298    | 13,55514479 | 8,463949843 | 10,99830795 | 3,205128205 | 9,87654321  | 13,12089972 | 12,1263877    | 12,54901961   | 11,01218369     | 12,13171577    | 13,61673863 | 6,41025641  | 9,222661397 | 5,743000718 | 9,592326139 | 1,094091904 | 6,109979633 | 5,018820577 | 17,89976134 | 9,74025974  | 12,87208367     | 11,90948789 | 13,40645948 | 7,346189164 | 14,90514905 | 1,524390244 | 14,61377871 | 10,11673152 | 5,387931034    | 22,22222222 | 10,24765158 | 10,37684326 | 13,93728223   | 9,946442234 | 2,190580504 | 5,993150685  | 9,599096556 | 16,89328389 | 9,592326139 | 13,4083045  | 11,34044001 | 1,601281025 | 12          | 5,063291139 | 8,810572687 | 10,79784043 | 7,89733465  | 8,595988539  | 12,60675071 | 10,3950104   | 7,918552036    | 14,28571429 | 6,172839506     | 15,3531218  | 5,287009063  | 8,276899925  | 12,65822785   | 7,86163522    | 8,982035928 | 11,15402852 |
| 2014 | 12,03703704 | 9,493670886       | 11,60714286 | 10,06711409 | 10,92498179 | 7,237635706 | 5,703422053 | 28,19548872 | 21,12676056 | 13,27967807 | 11,5648717   | 7,462686567 | 3,671970624 | 9,62250185   | 11,92088865 | 14,7505423  | 10,52405498   | 6,179196704 | 9,640666082 | 8,686210641   | 6,772009029  | 10,24498886  | 14,66127401       | 17,85714286 | 13,80813953 | 9,183673469        | 10,6075217     | 12,15153681 | 13,70533529 | 14,79654747 | 5,141388175 | 0,807754443 | 14,18799088    | 14,8215366  | 10,75619296 | 16,89460981 | 13,55932203 | 9,280742459 | 13,96648045 | 9,260790475   | 12,21840397   | 10,83198894     | 9,692132269    | 13,27433628 | 3,588516746 | 2,509410289 | 17,09401709 | 9,654062751 | 4,683840749 | 5,279831045 | 3,978779841 | 11,9379228  | 11,94029851 | 15,81027668     | 9,29668553  | 14,18439716 | 5,319148936 | 19,28191489 | 3,048780488 | 9,748172218 | 12,5984252  | 12,61829653    | 20,80237741 | 7,978723404 | 9,164420485 | 6,688963211   | 5,681818182 | 9,814612868 | 10,5605199   | 16,5471594  | 12,58116883 | 11,84132623 | 12,34567901 | 10,27631879 | 8,448540707 | 11,67315175 | 12,13592233 | 7,494646681 | 13,18488134 | 8,482563619 | 7,840062721  | 13,18944844 | 15,71709234  | 7,025761124    | 14,30685742 | 4,304160689     | 14,20598681 | 5,974607916  | 8,8823094    | 11,03752759   | 11,32930514   | 11,69950739 | 11,10833039 |
| 2015 | 5,504587156 | 3,300330033       | 15,13877208 | 6,420545746 | 10,90116279 | 5,599104143 | 13,19509896 | 15,50387597 | 14,62522852 | 15,48287207 | 13,01518438  | 10,50788091 | 8,244994111 | 4,946996466  | 10,73298429 | 10,21711367 | 11,65048544   | 9,910802775 | 6,266786034 | 10,27097902   | 13,2144037   | 6,653992395  | 13,47978033       | 9,592326139 | 8,058608059 | 11,53039832        | 8,827856793    | 11,98677322 | 14,64435146 | 11,10670369 | 10,32258065 | 5,723630417 | 14,6642655     | 9,924109749 | 9,352037408 | 14,26174497 | 3,389830508 | 7,05052879  | 12,86269818 | 13,03415278   | 11,07715814   | 10,37437979     | 15,4109589     | 14,65901848 | 13,5301353  | 7,863695937 | 11,74743025 | 7,955449483 | 7,981755986 | 6,876227898 | 4,016064257 | 10,11235955 | 23,64864865 | 19,2            | 9,958506224 | 12,76595745 | 4,599816007 | 8,730691739 | 1,592356688 | 11,43583227 | 13,40694006 | 12,763596      | 16,02564103 | 11,01422671 | 10,76398005 | 9,422850412   | 11,21656601 | 9,216589862 | 9,795918367  | 14,38848921 | 9,933774834 | 9,063444109 | 11,47396293 | 10,0536193  | 3,977724741 | 9,615384615 | 10,12658228 | 8,537886873 | 11,4638448  | 10,18518519 | 8,359872611  | 10,28963415 | 13,95812562  | 10,34482759    | 13,1445905  | 2,865329513     | 11,88707281 | 3,927729772  | 7,158196135  | 11,79487179   | 5,31107739    | 5,316007088 | 10,86907193 |
| 2016 | 8,189262966 | 14,03508772       | 10,6075217  | 8,417508418 | 18,07228916 | 5,102040816 | 12,60911736 | 3,571428571 | 7,8125      | 15,16683519 | 10,286554    | 5,917159763 | 8,939974457 | 4,074979625  | 12,3699747  | 12,8144281  | 11,72825846   | 6,787330317 | 14,74654378 | 10,24042743   | 12,83880171  | 11,24144673  | 13,16829076       | 5,012531328 | 6,88599847  | 9,239130435        | 16,55982906    | 13,04029304 | 9,947643979 | 15,1642797  | 10,5890139  | 4,233700254 | 12,40226476    | 9,975062344 | 11,71112557 | 10,1095198  | 3,50877193  | 7,462686567 | 14,13427562 | 11,83118488   | 13,50245499   | 10,79489696     | 14,01050788    | 16,76878426 | 3,95256917  | 9,345794393 | 13,61031519 | 10,55194805 | 3,802281369 | 4,504504505 | 5,780346821 | 12,63399694 | 13,98601399 | 13,38912134     | 9,005145798 | 9,894867038 | 1,040582726 | 13,29394387 | 3,115264798 | 9,395973154 | 9,700176367 | 10,54969461    | 15,54404145 | 7,515265383 | 10,83634343 | 13,21003963   | 9,274873524 | 8,284023669 | 15,91511936  | 8,220786847 | 13,13002965 | 5,141388175 | 11,4790287  | 11,55234657 | 6,622516556 | 21,82539683 | 15,87301587 | 10,89324619 | 15,15151515 | 8,26446281  | 6,162695152  | 12,58917331 | 14,59854015  | 13,08900524    | 15,6841536  | 1,689189189     | 15,50387597 | 4,166666667  | 11,50306748  | 12,60273973   | 11,18210863   | 9,296148738 | 11,30291329 |
| 2017 | 2,958579882 | 6,896551724       | 13,40482574 | 10,5913504  | 21,31018153 | 12,37623762 | 12,62135922 | 10,65719361 | 5,524861878 | 11,86579378 | 13,77000372  | 9,98003992  | 7,874015748 | 9,71659919   | 11,67728238 | 11,0619469  | 10,62867481   | 6,849315068 | 13,09164149 | 8,167770419   | 12,60063003  | 10,46622265  | 15,09433962       | 10,63829787 | 6,578947368 | 17,91713326        | 13,29080276    | 13,75274323 | 13,5        | 12,35584843 | 6,451612903 | 4,618937644 | 17,76441651    | 13,18613922 | 7,049345418 | 12,53132832 | 2,976190476 | 11,29943503 | 8,92582333  | 8,860112455   | 12,82051282   | 13,64705882     | 11,35684399    | 13,69863014 | 6,180469716 | 8,547008547 | 5,695687551 | 7,751937984 | 4,608294931 | 10,71811361 | 7,776049767 | 10,12292119 | 12,65822785 | 12,53132832     | 12,37201365 | 8,187134503 | 4,016064257 | 18,94451962 | 4,424778761 | 12,31751825 | 16,07142857 | 8,54214123     | 10,47120419 | 7,045561296 | 7,569386039 | 8,663366337   | 11,07325383 | 7,847533632 | 14,33691756  | 10,95890411 | 11,92434211 | 11,86017478 | 13,51960342 | 11,35557133 | 5,516154452 | 10,50420168 | 2,544529262 | 10,88139282 | 13,64175563 | 9,56937799  | 8,986928105  | 17,61573126 | 10,2145046   | 8,423586041    | 16,93175988 | 3,311258278     | 12,08897485 | 3,098373354  | 6,144393241  | 8,561643836   | 7,674597084   | 5,312084993 | 11,19427231 |
| 2018 | 6,21761658  | 7,662835249       | 5,4894785   | 8,467400508 | 8,723235527 | 2,304147465 | 8,866995074 | 14,20959147 | 8,695652174 | 13,3583803  | 10,91305929  | 11,34215501 | 9,937888199 | 10,91519731  | 8,80932083  | 10,15228426 | 14,41647597   | 8           | 8,720930233 | 11,41127154   | 8,300252616  | 11,18133204  | 15,5822854        | 2,538071066 | 10,14832162 | 9,153318078        | 11,3085622     | 11,87335092 | 11,83431953 | 12,96596434 | 9,097270819 | 1,563721658 | 14,4766147     | 10,52305788 | 6,896551724 | 14,49275362 | 16,18122977 | 9,291521487 | 12,34166937 | 9,464285714   | 14,12999596   | 10,88534107     | 18,91252955    | 14,81237656 | 3,921568627 | 9,222661397 | 11,17798796 | 8,968609865 | 3,516998828 | 7,633587786 | 8,012820513 | 11,30369254 | 15,43209877 | 19,55782313     | 13,80846325 | 7,233273056 | 1,068376068 | 10,75268817 | 3,053435115 | 10,90047393 | 14,604811   | 10,36866359    | 24,24242424 | 12,99376299 | 10,75268817 | 4,756242568   | 8,503401361 | 4,733727811 | 5,4894785    | 12,62019231 | 13,11891663 | 10,93951094 | 11,94161875 | 13,19721116 | 6,573541495 | 6,896551724 | 18,56763926 | 10,86956522 | 9,575104728 | 4,99001996  | 8,490930143  | 11,90982561 | 12,40951396  | 12,10653753    | 14,78352693 | 5,110732538     | 10,33057851 | 7,36497545   | 8,792965627  | 13,7136588    | 12,98701299   | 15,48205489 | 11,01200375 |
| 2019 | 5,050505051 | 3,225806452       | 7,441860465 | 7,279344859 | 4,125412541 | 14,17322835 | 6,315789474 | 5,780346821 | 19,49025487 | 18,91040072 | 12,07327227  | 5,870841487 | 7,281553398 | 14,22594142  | 10,4638009  | 13,02460203 | 10,58070866   | 10,97560976 | 16,66666667 | 14,61988304   | 11,96449247  | 12,76813075  | 11,68939605       | 8,152173913 | 11,07594937 | 17,59708738        | 10,56556868    | 12,64637002 | 16,35768811 | 12,8314799  | 8,02919708  | 3,915426782 | 12,03758074    | 11,80599872 | 8,037997808 | 13,84364821 | 7,01754386  | 8,641975309 | 11,97206518 | 13,27592393   | 15,34526854   | 10,8805668      | 10             | 10,06944444 | 5,044136192 | 8,174386921 | 11,09057301 | 4,591368228 | 2,493765586 | 9,592326139 | 16,63893511 | 13,03680982 | 14,28571429 | 10,40681173     | 11,05216622 | 11,4213198  | 3,386004515 | 10,80246914 | 4,846526656 | 13,47449471 | 11,01928375 | 6,806930693    | 6,420545746 | 10,12373453 | 9,644364075 | 9,626955475   | 9,803921569 | 11,65501166 | 8,387698043  | 8,274984087 | 9,218612818 | 20,2757502  | 14,21590404 | 12,20462218 | 6,589785832 | 5,012531328 | 19,10828025 | 5,336179296 | 16,3026761  | 10,48218029 | 4,460665045  | 9,476534296 | 12,93103448  | 5,031446541    | 14,96259352 | 3,787878788     | 13,95348837 | 5,733005733  | 12,3253903   | 11,5194734    | 9,577015164   | 9,453072248 | 11,18671215 |

### Agora, vamos para o RStudio!

Vamos importar os dados csv para o ambiente do RStudio e Plotar um gráfico utilizando pacote ggplot2 e vizualizar a distribuição temporal do CMI para MSP.

```
Rplot1 <- ggplot(Dados, aes(x = Ano, y = MSP)) + geom_line(color = 'red2', size = 1.8) +
  theme_light() + geom_smooth(method = 'lm', color = "blue4", se = FALSE) +
  labs(x ="", y ="CMI (a cada 1.000)")
```

  <p align="center">
  <img src="https://raw.githubusercontent.com/Luccan97/Prais_Winsten/master/Rplot.png" />
</p>

## Além de uma tímida tendência crescente, o gráfico não nos diz muito... Portanto, vamos rodar o algorítmo de Prais Winsten

```
setwd("C:\\Users\\User\\Desktop\\projetos\\script_pw")

Dados <- read.csv("cmi_msp_da_2013_2019.csv", sep = ";", dec = ",", encoding = "windows-1250")

# Filtrando só os dados para o município
MSP <- Dados[,c(1,94)]

# Pacotes necessários
library(prais)
library(dplyr)

dataset_length <- length(names(MSP))

#Transformando valores em log

for (i in 2:dataset_length) {
  MSP[,i] <- log10(MSP[,i])
}

# Função Prais Winsten
pw_resultados <- lapply(names(MSP)[-1], function(a){
  resp <- paste0("`", a, "`")
  fmla <- paste(resp, "Ano", sep = "~")
  fmla <- as.formula(fmla)
  pw <- prais_winsten(fmla, data = MSP)
  cf <- coef(summary(pw))[2, ]
  cbind.data.frame(coluna = a, t(cf))
})

pw_resultados_MSP <- do.call(rbind, pw_resultados)


bmin <- pw_resultados_MSP$Estimate - (1.96 *pw_resultados_MSP$`Std. Error`)
bmax <- pw_resultados_MSP$Estimate + (1.96 *pw_resultados_MSP$`Std. Error`)

ICmin <- (-1 + exp(bmin))*100
ICmax <- (-1 + exp(bmax))*100
ICmin <- format(round(ICmin, 2), nsmall = 2)
ICmax <- format(round(ICmax, 2), nsmall = 2)

pw_resultados_MSP <- mutate(pw_resultados_MSP, APC = (-1 + exp(Estimate))*100,  IC = paste("(", ICmin," / ", ICmax, ")"))

head(pw_resultados_MSP)

# Exportando arquivo csv com os resultados
write.table(pw_resultados_MSP, file = "pw_resultados.csv", sep = "\t", na = "", quote = FALSE, dec = ",")


```

### Vamos obsevar os resultados gerados pelo Prais Winsten!
Temos que, para o município de São Paulo (2013-2019) a porcentagem de variação anual do CMI foi de 0,04. Isso é, uma tendência crescente, tendo em vista
o valor positivo, porém, não é um valor estatísticamente significante, pois o valor de P é alto (0,69)

| coluna | Estimate    | Std. Error  | t value     | Pr(>\|t\|) | APC  | IC                 |
|--------|-------------|-------------|-------------|------------|------|--------------------|
| MSP    | 0,000356731 | 0,000836568 | 0,426422089 | 0,69       | 0,04 | ( -0.13  /  0.20 ) |


É importante ressaltar que, o método de Prais-Winsten é uma regressão linear delineada especialmente para problemas de série temporal.
Esse método considera a existência de uma autocorrelação serial. Isso é, existe uma dependência entre valores seriais e os próprios valores anteriores.

A grande vantagem desse Script que disponibilizo é a capacidade de rodar o método para um grande banco de dados simultâneamente.
Agora que já vimos a tendência temporal para o Coeficiente de Mortalidade Infantil para o MSP, vamos rodar o mesmo script para todos os Distritos Administrativos!

```
dataset_length <- length(names(Dados))

for (i in 2:dataset_length) {
  Dados[,i] <- log10(Dados[,i])
}

pw_resultados <- lapply(names(Dados)[-1], function(a){
  resp <- paste0("`", a, "`")
  fmla <- paste(resp, "Ano", sep = "~")
  fmla <- as.formula(fmla)
  pw <- prais_winsten(fmla, data = Dados)
  cf <- coef(summary(pw))[2, ]
  cbind.data.frame(coluna = a, t(cf))
})
pw_resultados <- do.call(rbind, pw_resultados)


bmin <- pw_resultados$Estimate - (1.96 *pw_resultados$`Std. Error`)
bmax <- pw_resultados$Estimate + (1.96 *pw_resultados$`Std. Error`)

ICmin <- (-1 + exp(bmin))*100
ICmax <- (-1 + exp(bmax))*100
ICmin <- format(round(ICmin, 2), nsmall = 2)
ICmax <- format(round(ICmax, 2), nsmall = 2)

pw_resultados_Dados <- mutate(pw_resultados, APC = (-1 + exp(Estimate))*100,  IC = paste("(", ICmin," / ", ICmax, ")"))

head(pw_resultados_Dados)

write.table(pw_resultados_Dados, file = "pw_resultados.csv", sep = "\t", na = "", quote = FALSE, dec = ",")

```

## Breve olhada na tabela de resultados gerada em poucos segundos após o script rodar:
| Distritos Administrativos | Estimate     | Std. Error  | t value      | Pr(>\|t\|) | APC   | IC                   |
|---------------------------|--------------|-------------|--------------|------------|-------|----------------------|
| Água.Rasa                 | -0,058635964 | 0,017479211 | -3,354611648 | 0,02       | -5,69 | (  -8.87  /  -2.41 ) |
| Alto.de.Pinheiros         | -0,012584212 | 0,026346368 | -0,477645033 | 0,65       | -1,25 | (  -6.22  /   3.98 ) |
| Anhanguera                | -0,052518679 | 0,017781585 | -2,953543221 | 0,03       | -5,12 | (  -8.37  /  -1.75 ) |
| Aricanduva                | -0,01188546  | 0,014795173 | -0,80333365  | 0,46       | -1,18 | (  -4.01  /   1.73 ) |
| Artur.Alvim               | -0,043456564 | 0,058551421 | -0,74219487  | 0,49       | -4,25 | ( -14.63  /   7.39 ) |
| Bela.Vista                | -0,032023441 | 0,019655815 | -1,62920951  | 0,18       | -3,15 | (  -6.81  /   0.65 ) |
| Belém                     | 0,051507109  | 0,056752027 | 0,907581848  | 0,41       | 5,29  | (  -5.80  /  17.67 ) |
| Bom.Retiro                | -0,052426726 | 0,046849326 | -1,119049745 | 0,31       | -5,11 | ( -13.43  /   4.02 ) |
| Brás                      | -0,01122609  | 0,055205996 | -0,203349111 | 0,85       | -1,12 | ( -11.26  /  10.18 ) |
| Brasilândia               | 0,017841086  | 0,012848621 | 1,388560329  | 0,22       | 1,80  | (  -0.73  /   4.40 ) |
| Cachoeirinha              | -0,003095267 | 0,002755613 | -1,123259203 | 0,31       | -0,31 | (  -0.85  /   0.23 ) |
| Cambuci                   | 0,018550055  | 0,016053952 | 1,155482186  | 0,30       | 1,87  | (  -1.28  /   5.13 ) |
| Campo.Belo                | 0,03593008   | 0,016232919 | 2,213408398  | 0,08       | 3,66  | (   0.41  /   7.01 ) |
| Campo.Grande              | 0,026267157  | 0,045696467 | 0,574818113  | 0,59       | 2,66  | (  -6.13  /  12.28 ) |
| Campo.Limpo               | -0,013209529 | 0,006828547 | -1,934456703 | 0,11       | -1,31 | (  -2.62  /   0.02 ) |
| Cangaíba                  | -0,004417365 | 0,005785262 | -0,76355491  | 0,48       | -0,44 | (  -1.56  /   0.69 ) |
| Capão.Redondo             | 0,006554167  | 0,004185878 | 1,565780538  | 0,18       | 0,66  | (  -0.16  /   1.49 ) |
| Carrão                    | -0,009062954 | 0,019335122 | -0,468730106 | 0,66       | -0,90 | (  -4.59  /   2.93 ) |
| Casa.Verde                | 0,015275205  | 0,023830974 | 0,64098115   | 0,55       | 1,54  | (  -3.09  /   6.39 ) |
| Cidade.Ademar             | 0,018144832  | 0,013784256 | 1,316344669  | 0,25       | 1,83  | (  -0.88  /   4.62 ) |
| Cidade.Dutra              | 0,001263004  | 0,016465931 | 0,076704095  | 0,94       | 0,13  | (  -3.05  /   3.41 ) |
| Cidade.Líder              | 0,014900609  | 0,015766466 | 0,945082355  | 0,39       | 1,50  | (  -1.59  /   4.69 ) |
| Cidade.Tiradentes         | -0,005192541 | 0,006767866 | -0,767234629 | 0,48       | -0,52 | (  -1.83  /   0.81 ) |
| Consolação                | -0,036778147 | 0,040331031 | -0,911906942 | 0,40       | -3,61 | ( -10.94  /   4.32 ) |
| Cursino                   | -0,01598781  | 0,030623478 | -0,522076863 | 0,62       | -1,59 | (  -7.32  /   4.50 ) |
| Ermelino.Matarazzo        | 0,014204236  | 0,010613586 | 1,338306987  | 0,24       | 1,43  | (  -0.66  /   3.56 ) |
| Freguesia.do.Ó            | 0,008267124  | 0,016146002 | 0,512023004  | 0,63       | 0,83  | (  -2.31  /   4.07 ) |
| Grajaú                    | 0,004686439  | 0,004348269 | 1,077771125  | 0,33       | 0,47  | (  -0.38  /   1.33 ) |
| Guaianases                | 0,018391176  | 0,012833747 | 1,433032535  | 0,21       | 1,86  | (  -0.67  /   4.45 ) |
| Iguatemi                  | -0,011586101 | 0,006679831 | -1,734490135 | 0,14       | -1,15 | (  -2.44  /   0.15 ) |
| Ipiranga                  | 0,016253443  | 0,017590514 | 0,923989102  | 0,40       | 1,64  | (  -1.81  /   5.20 ) |
| Itaim.Bibi                | 0,019096025  | 0,043271225 | 0,441310018  | 0,68       | 1,93  | (  -6.36  /  10.95 ) |
| Itaim.Paulista            | 0,003051281  | 0,008877338 | 0,343715731  | 0,75       | 0,31  | (  -1.42  /   2.07 ) |
| Itaquera                  | -0,013122546 | 0,011443424 | -1,146732506 | 0,30       | -1,30 | (  -3.49  /   0.94 ) |
| Jabaquara                 | -0,024384439 | 0,012701198 | -1,919853372 | 0,11       | -2,41 | (  -4.81  /   0.05 ) |
| Jaçanã                    | 0,002237271  | 0,014098752 | 0,158685726  | 0,88       | 0,22  | (  -2.51  /   3.03 ) |
| Jaguara                   | 0,035975479  | 0,050416642 | 0,713563576  | 0,51       | 3,66  | (  -6.09  /  14.43 ) |
| Jaguaré                   | 0,00108656   | 0,014606877 | 0,074386861  | 0,94       | 0,11  | (  -2.72  /   3.02 ) |
| Jaraguá                   | -0,016317168 | 0,00848086  | -1,923999223 | 0,11       | -1,62 | (  -3.24  /   0.03 ) |
| Jardim.Ângela             | -0,003339366 | 0,011873832 | -0,281237428 | 0,79       | -0,33 | (  -2.63  /   2.01 ) |
| Jardim.Helena             | 0,016488385  | 0,005025355 | 3,281039043  | 0,02       | 1,66  | (   0.67  /   2.67 ) |
| Jardim.São.Luís           | 0,004702811  | 0,00685508  | 0,686033019  | 0,52       | 0,47  | (  -0.87  /   1.83 ) |
| José.Bonifácio            | 0,019948173  | 0,009347654 | 2,134029782  | 0,10       | 2,01  | (   0.16  /   3.90 ) |
| Lajeado                   | -0,010172485 | 0,011720039 | -0,867956539 | 0,43       | -1,01 | (  -3.26  /   1.29 ) |
| Lapa                      | -0,02204389  | 0,020291844 | -1,086342341 | 0,33       | -2,18 | (  -5.99  /   1.79 ) |
| Liberdade                 | 0,052497749  | 0,025036405 | 2,096856501  | 0,09       | 5,39  | (   0.34  /  10.69 ) |
| Limão                     | -0,005952931 | 0,027363874 | -0,217547099 | 0,84       | -0,59 | (  -5.78  /   4.88 ) |
| Mandaqui                  | -0,025198745 | 0,008780321 | -2,869911608 | 0,05       | -2,49 | (  -4.15  /  -0.80 ) |
| Moema                     | 0,0223374    | 0,056335664 | 0,396505497  | 0,71       | 2,26  | (  -8.43  /  14.20 ) |
| Mooca                     | 0,042349534  | 0,010927933 | 3,875347163  | 0,01       | 4,33  | (   2.12  /   6.58 ) |
| Morumbi                   | 0,087868484  | 0,020101494 | 4,371241554  | 0,01       | 9,18  | (   4.97  /  13.57 ) |
| Parelheiros               | -0,015742814 | 0,014715551 | -1,069807947 | 0,33       | -1,56 | (  -4.36  /   1.32 ) |
| Pari                      | 0,015239313  | 0,022376014 | 0,68105576   | 0,53       | 1,54  | (  -2.82  /   6.09 ) |
| Parque.do.Carmo           | -0,007475077 | 0,011949204 | -0,625571156 | 0,56       | -0,74 | (  -3.04  /   1.61 ) |
| Pedreira                  | 0,01418758   | 0,012018658 | 1,180462923  | 0,29       | 1,43  | (  -0.93  /   3.85 ) |
| Penha                     | -0,040499699 | 0,014387332 | -2,814955414 | 0,04       | -3,97 | (  -6.64  /  -1.22 ) |
| Perdizes                  | -0,10521978  | 0,026698366 | -3,941056925 | 0,01       | -9,99 | ( -14.58  /  -5.15 ) |
| Perus                     | -0,020461101 | 0,01929754  | -1,060295812 | 0,34       | -2,03 | (  -5.66  /   1.75 ) |
| Pinheiros                 | 0,063614787  | 0,014202316 | 4,479183921  | 0,01       | 6,57  | (   3.64  /   9.58 ) |
| Pirituba                  | 0,003218384  | 0,009373123 | 0,34336301   | 0,75       | 0,32  | (  -1.50  /   2.18 ) |
| Ponte.Rasa                | 0,013361224  | 0,010757117 | 1,242082247  | 0,27       | 1,35  | (  -0.77  /   3.50 ) |
| Raposo.Tavares            | -0,004081099 | 0,025715751 | -0,158700369 | 0,88       | -0,41 | (  -5.30  /   4.74 ) |
| República                 | -0,039883098 | 0,011220655 | -3,554435826 | 0,02       | -3,91 | (  -6.00  /  -1.77 ) |
| Rio.Pequeno               | 0,009754987  | 0,015727185 | 0,620262741  | 0,56       | 0,98  | (  -2.08  /   4.14 ) |
| Sacomã                    | -0,003393687 | 0,007534242 | -0,450435079 | 0,67       | -0,34 | (  -1.80  /   1.14 ) |
| Santa.Cecília             | -0,029780055 | 0,023258847 | -1,280375382 | 0,26       | -2,93 | (  -7.26  /   1.59 ) |
| Santana                   | 0,016948648  | 0,011333728 | 1,495416838  | 0,20       | 1,71  | (  -0.53  /   3.99 ) |
| Santo.Amaro               | 0,040657681  | 0,039657416 | 1,025222636  | 0,35       | 4,15  | (  -3.64  /  12.57 ) |
| São.Domingos              | 0,002051168  | 0,037059901 | 0,055347381  | 0,96       | 0,21  | (  -6.82  /   7.75 ) |
| São.Lucas                 | -0,021950315 | 0,017234495 | -1,273626832 | 0,26       | -2,17 | (  -5.42  /   1.19 ) |
| São.Mateus                | -0,020446583 | 0,012355338 | -1,654878505 | 0,16       | -2,02 | (  -4.37  /   0.38 ) |
| São.Miguel                | 0,036255089  | 0,032581793 | 1,112740763  | 0,32       | 3,69  | (  -2.72  /  10.53 ) |
| São.Rafael                | 0,004233995  | 0,006009998 | 0,704491889  | 0,51       | 0,42  | (  -0.75  /   1.61 ) |
| Sapopemba                 | 0,014771844  | 0,004973292 | 2,970234306  | 0,03       | 1,49  | (   0.50  /   2.48 ) |
| Saúde                     | 0,047214179  | 0,021319972 | 2,214551652  | 0,08       | 4,83  | (   0.54  /   9.31 ) |
| Sé                        | -0,056115129 | 0,034864526 | -1,60951932  | 0,17       | -5,46 | ( -11.70  /   1.23 ) |
| Socorro                   | 0,040310742  | 0,046011663 | 0,876098353  | 0,42       | 4,11  | (  -4.86  /  13.94 ) |
| Tatuapé                   | -0,010462797 | 0,024600186 | -0,425313751 | 0,69       | -1,04 | (  -5.70  /   3.85 ) |
| Tremembé                  | 0,003781668  | 0,009323295 | 0,405614961  | 0,70       | 0,38  | (  -1.44  /   2.23 ) |
| Tucuruvi                  | -0,017505748 | 0,010611742 | -1,649658217 | 0,16       | -1,74 | (  -3.76  /   0.33 ) |
| Vila.Andrade              | -0,016931472 | 0,012163203 | -1,392024202 | 0,22       | -1,68 | (  -4.00  /   0.69 ) |
| Vila.Curuçá               | -0,007936713 | 0,017296597 | -0,458859819 | 0,67       | -0,79 | (  -4.10  /   2.63 ) |
| Vila.Formosa              | -0,006183792 | 0,011797227 | -0,524173323 | 0,62       | -0,62 | (  -2.89  /   1.71 ) |
| Vila.Guilherme            | 0,002435381  | 0,021080823 | 0,115525898  | 0,91       | 0,24  | (  -3.81  /   4.47 ) |
| Vila.Jacuí                | 0,007300043  | 0,006175609 | 1,182076753  | 0,29       | 0,73  | (  -0.48  /   1.96 ) |
| Vila.Leopoldina           | -0,021908252 | 0,043790764 | -0,500293907 | 0,64       | -2,17 | ( -10.21  /   6.60 ) |
| Vila.Maria                | -0,017446599 | 0,006887268 | -2,533166849 | 0,05       | -1,73 | (  -3.05  /  -0.39 ) |
| Vila.Mariana              | 0,006634651  | 0,024900075 | 0,266451041  | 0,80       | 0,67  | (  -4.13  /   5.70 ) |
| Vila.Matilde              | 0,010229231  | 0,01277998  | 0,800410544  | 0,46       | 1,03  | (  -1.47  /   3.59 ) |
| Vila.Medeiros             | -0,00091248  | 0,00804058  | -0,113484297 | 0,91       | -0,09 | (  -1.65  /   1.50 ) |
| Vila.Prudente             | 0,021541161  | 0,011688807 | 1,842887948  | 0,12       | 2,18  | (  -0.14  /   4.55 ) |
| Vila.Sônia                | 0,013718241  | 0,023031415 | 0,595631697  | 0,58       | 1,38  | (  -3.09  /   6.06 ) |
| MSP                       | 0,000356731  | 0,000836568 | 0,426422089  | 0,69       | 0,04  | (  -0.13  /   0.20 ) |


### Agora, farei um filtro para observarmos apenas os resultados significantes (com valor P < 0,05)

Os Distritos que apresentam valor de P maior que 0,05, interpreta-se que existe uma tendência temporal estacionária! 
```
P_sig <- filter(pw_resultados_Dados, pw_resultados_Dados$`Pr(>|t|)` <= 0.05)

P_sig_ordenado <- arrange(P_sig, desc(P_sig$APC))

write.table(P_sig_ordenado, file = "P_sig.csv", sep = "\t", na = "", quote = FALSE, dec = ",")

```

### Primeiro, vamos obsevrar os Distritos cuja porcentagem de variação anual é positiva! Ou seja, distritos onde o Coeficiente de Mortalidade Infantil está aumentando!

Se nos contentássemos apenas com os valores calculados para o município intiero, sem desagregarmos por unidades territóriais menores, as informações descritas na tabela
abaixo passariam despercebidas!!!
É importante ressaltar que o CMI é um indicador de saúde extremamente sensível que traduz condições socioeconômicas, sanitárias e de acesso à atenção básica de saúde!


| Distritos Administrativos | Estimate    | Std. Error  | t value     | Pr(>\|t\|) | APC  | IC                   |
|---------------------------|-------------|-------------|-------------|------------|------|----------------------|
| Morumbi                   | 0,087868484 | 0,020101494 | 4,371241554 | 0,01       | 9,18 | (   4.97  /  13.57 ) |
| Pinheiros                 | 0,063614787 | 0,014202316 | 4,479183921 | 0,01       | 6,57 | (   3.64  /   9.58 ) |
| Mooca                     | 0,042349534 | 0,010927933 | 3,875347163 | 0,01       | 4,33 | (   2.12  /   6.58 ) |
| Jardim.Helena             | 0,016488385 | 0,005025355 | 3,281039043 | 0,02       | 1,66 | (   0.67  /   2.67 ) |
| Sapopemba                 | 0,014771844 | 0,004973292 | 2,970234306 | 0,03       | 1,49 | (   0.50  /   2.48 ) |


### Agora, os Distritos que apresentam APC negativo significante, isso é, onde o CMI apresenta tendência descrescente.

| Distritos Administrativos | Estimate     | Std. Error  | t value      | Pr(>\|t\|) | APC   | IC                   |
|---------------------------|--------------|-------------|--------------|------------|-------|----------------------|
| Mandaqui                  | -0,025198745 | 0,008780321 | -2,869911608 | 0,05       | -2,49 | (  -4.15  /  -0.80 ) |
| República                 | -0,039883098 | 0,011220655 | -3,554435826 | 0,02       | -3,91 | (  -6.00  /  -1.77 ) |
| Penha                     | -0,040499699 | 0,014387332 | -2,814955414 | 0,04       | -3,97 | (  -6.64  /  -1.22 ) |
| Anhanguera                | -0,052518679 | 0,017781585 | -2,953543221 | 0,03       | -5,12 | (  -8.37  /  -1.75 ) |
| Água.Rasa                 | -0,058635964 | 0,017479211 | -3,354611648 | 0,02       | -5,69 | (  -8.87  /  -2.41 ) |
| Perdizes                  | -0,10521978  | 0,026698366 | -3,941056925 | 0,01       | -9,99 | ( -14.58  /  -5.15 ) |

