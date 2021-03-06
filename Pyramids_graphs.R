## Ap�ndice - C�digos para as pir�mides 

#### Pir�mide para um dia i:

ggplot(subset(newWorkingSubset, 
              newWorkingSubset$data==unique(newWorkingSubset$data)[i]),
       aes(x = diasobs[[i]]$ageGrp, fill = diasobs[[i]]$sex,
           y = ifelse(test = diasobs[[i]]$sex == "M",
                      yes = -diasobs[[i]]$value,
                      no = diasobs[[i]]$value))) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = abs,
                     limits = max(newWorkingSubset$value) * c(-1,1)) + 
  labs(title="Pir�mide Et�ria",
       y = paste("Indiv�duos Infetados (Confirmados) no dia",
                 diasobs[[i]]$data),
       x="Grupo Et�rio",fill="Sexo",caption = "Dados obtidos em: 
         https://raw.githubusercontent.com/saghirb/Dados_COVID-19_PT/master/data/") +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="right", legend.box = "vertical")


#### Pir�mides para todos os dias do conjunto de dados, num s� ficheiro:


ggplot(newWorkingSubset, aes(x = newWorkingSubset$ageGrp,
                             fill = newWorkingSubset$sex,
                             y = ifelse(test = newWorkingSubset$sex == "M",
                                        yes = -newWorkingSubset$value,
                                        no = newWorkingSubset$value))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(newWorkingSubset$value) * c(-1,1)) + 
  labs(title="Pir�mide Et�ria",y = "Indiv�duos Infetados (Confirmados)",
       x="Grupo Et�rio",fill="Sexo",caption = "Dados obtidos em: 
           https://raw.githubusercontent.com/saghirb/Dados_COVID-19_PT/master/data/") +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="right", legend.box = "vertical") + 
  facet_wrap(~ newWorkingSubset$data)


#### Pir�mides para todos os dias, guardados em ficheiros diferentes, numa s� pasta (newWorkingSubset � o nome do objeto no qual est�o guardados os dados a trabalhar)


#Cria��o de um objeto (lista) que cont�m cada dia
diasobs <- split(newWorkingSubset,newWorkingSubset$data)

#cria��o de uma fun��o para guardar a piramide correspondente a cada dia numa pasta
day.graph <- function(df, na.rm=TRUE, ...){
  
  #cria��o de uma lista de dias para percorrer
  day_list <- unique(df$data)
  
  #cria��o do ciclo for que produz gr�ficos ggplot2
  for(i in seq_along(day_list)){
    
    #cria uma pir�mide para cada dia no dataframe
    plot <-  ggplot(subset(df, df$County==day_list[i]),
                    aes(x = diasobs[[i]]$ageGrp,
                        fill = diasobs[[i]]$sex,
                        y = ifelse(test = diasobs[[i]]$sex == "M",
                                   yes = -diasobs[[i]]$value,
                                   no = diasobs[[i]]$value))) +
      geom_bar(stat = "identity") + 
      scale_y_continuous(labels = abs,
                         limits = max(newWorkingSubset$value) * c(-1,1)) + 
      labs(title = "Pir�mide Et�ria",
           y = paste("Indiv�duos Infetados (Confirmados) no dia",
                     diasobs[[i]]$data),
           x = "Grupo Et�rio", fill = "Sexo", caption = "Dados obtidos em: 
           https://raw.githubusercontent.com/saghirb/Dados_COVID-19_PT/master/data/") +
      coord_flip() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            legend.position="right", legend.box = "vertical")
    
    #grava cada uma das pir�mides feitas
    ggsave(plot, 
           file = paste("workingDirectory/pasta_onde_ficarao_as_piramides",
                        day_list[i], ".png", sep = ''), scale = 2)
    print(plot)}
}

#guarda as pir�mides em ficheiros
day.graph(newWorkingSubset)
