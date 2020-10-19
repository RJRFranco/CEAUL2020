## Apêndice - Códigos para as pirâmides 

#### Pirâmide para um dia i:

ggplot(subset(newWorkingSubset, 
              newWorkingSubset$data==unique(newWorkingSubset$data)[i]),
       aes(x = diasobs[[i]]$ageGrp, fill = diasobs[[i]]$sex,
           y = ifelse(test = diasobs[[i]]$sex == "M",
                      yes = -diasobs[[i]]$value,
                      no = diasobs[[i]]$value))) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = abs,
                     limits = max(newWorkingSubset$value) * c(-1,1)) + 
  labs(title="Pirâmide Etária",
       y = paste("Indivíduos Infetados (Confirmados) no dia",
                 diasobs[[i]]$data),
       x="Grupo Etário",fill="Sexo",caption = "Dados obtidos em: 
         https://raw.githubusercontent.com/saghirb/Dados_COVID-19_PT/master/data/") +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="right", legend.box = "vertical")


#### Pirâmides para todos os dias do conjunto de dados, num só ficheiro:


ggplot(newWorkingSubset, aes(x = newWorkingSubset$ageGrp,
                             fill = newWorkingSubset$sex,
                             y = ifelse(test = newWorkingSubset$sex == "M",
                                        yes = -newWorkingSubset$value,
                                        no = newWorkingSubset$value))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(newWorkingSubset$value) * c(-1,1)) + 
  labs(title="Pirâmide Etária",y = "Indivíduos Infetados (Confirmados)",
       x="Grupo Etário",fill="Sexo",caption = "Dados obtidos em: 
           https://raw.githubusercontent.com/saghirb/Dados_COVID-19_PT/master/data/") +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="right", legend.box = "vertical") + 
  facet_wrap(~ newWorkingSubset$data)


#### Pirâmides para todos os dias, guardados em ficheiros diferentes, numa só pasta (newWorkingSubset é o nome do objeto no qual estão guardados os dados a trabalhar)


#Criação de um objeto (lista) que contém cada dia
diasobs <- split(newWorkingSubset,newWorkingSubset$data)

#criação de uma função para guardar a piramide correspondente a cada dia numa pasta
day.graph <- function(df, na.rm=TRUE, ...){
  
  #criação de uma lista de dias para percorrer
  day_list <- unique(df$data)
  
  #criação do ciclo for que produz gráficos ggplot2
  for(i in seq_along(day_list)){
    
    #cria uma pirâmide para cada dia no dataframe
    plot <-  ggplot(subset(df, df$County==day_list[i]),
                    aes(x = diasobs[[i]]$ageGrp,
                        fill = diasobs[[i]]$sex,
                        y = ifelse(test = diasobs[[i]]$sex == "M",
                                   yes = -diasobs[[i]]$value,
                                   no = diasobs[[i]]$value))) +
      geom_bar(stat = "identity") + 
      scale_y_continuous(labels = abs,
                         limits = max(newWorkingSubset$value) * c(-1,1)) + 
      labs(title = "Pirâmide Etária",
           y = paste("Indivíduos Infetados (Confirmados) no dia",
                     diasobs[[i]]$data),
           x = "Grupo Etário", fill = "Sexo", caption = "Dados obtidos em: 
           https://raw.githubusercontent.com/saghirb/Dados_COVID-19_PT/master/data/") +
      coord_flip() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            legend.position="right", legend.box = "vertical")
    
    #grava cada uma das pirâmides feitas
    ggsave(plot, 
           file = paste("workingDirectory/pasta_onde_ficarao_as_piramides",
                        day_list[i], ".png", sep = ''), scale = 2)
    print(plot)}
}

#guarda as pirâmides em ficheiros
day.graph(newWorkingSubset)
