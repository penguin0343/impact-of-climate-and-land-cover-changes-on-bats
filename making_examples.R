

#demonstrate additive distribution
habitat_suitability <- c(0.8,0.6,0.7,0.8,0.4,0.6,0.6,0.2,0.4,0.4,0.2,0.3)
models <- rep(c('Climate only','Landcover only','Additive'),4)
presence <- c(1,1,1,1,0,1,1,0,0,0,0,0)
combination <- c(rep('Two_presence',3),rep('Opposite_1',3),rep('Opposite_2',3),rep('Two_absence',3))
sum <- data.frame(habitat_suitability,models,presence,combination)
figure <- sum%>%ggplot(aes(fill=factor(presence),y=habitat_suitability,x=factor(models,levels=c('Climate only','Landcover only','Additive'))))+
  geom_bar(position = 'dodge',stat = 'identity')+ylim(0,1)+
  geom_hline(data = sum, aes(yintercept = 0.5),color='red',linetype = 'dashed') +
  labs(x='',y='Habitat suitability')+ 
  scale_fill_manual('Presence',values = c('darkgrey','blue'))+
  geom_text(aes(label=habitat_suitability), position=position_dodge(width=0.9), vjust=-0.25)+
  facet_wrap(~factor(combination,levels=c('Two_presence','Opposite_1','Opposite_2','Two_absence')),nrow = 4)+
  theme_classic()+
  theme(text = element_text(size = 12),strip.text.x = element_blank(),legend.position="none",axis.text.x = element_text(angle = 90))
ggsave(figure,filename='F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/Additive_presence.tiff', width = 6, height = 25, units = 'cm',dpi = 300)

#demonstrate interaction
species.richness <- c(3,2,1,1,0,
                      0,0,0,0,0,
                      3,2,2,1,1,
                      3,2,1,1,2,
                      3,2,1,1,4,
                      
                      2,3,1,2,0,
                      2,3,1,2,1,
                      2,3,1,2,2,
                      2,3,1,2,3,
                      2,3,1,2,4,
                      
                      1,2,2,2,0,
                      1,2,2,2,1,
                      1,2,2,2,2,
                      0,0,0,0,0,
                      1,2,2,2,3)
respones <- rep(rep(c('Current','Climate','Land cover','Additive','Combined'),5),3)
interactions <- rep(c(rep('-S',5),rep('+A',5),rep('AD',5),rep('-A',5),rep('+S',5)),3)
combinations<- c(rep('Two_negative',25),rep('Opposite',25),rep('Two_positive',25))
sum.twonegative <- data.frame(species.richness,respones,interactions,combinations)
figure <- sum.twonegative%>%ggplot(aes(fill=factor(respones,levels=c('Current','Climate','Land cover','Additive','Combined')),y=species.richness,
                                       x=factor(respones,levels=c('Current','Climate','Land cover','Additive','Combined'))))+
  geom_bar(position = 'dodge',stat = 'identity')+
  labs(x='',y='Distribution range size/species richness')+
  scale_fill_manual('Response',values = c('darkgrey','blue','blue','orange','brown'))+
  geom_text(aes(label=species.richness), position=position_dodge(width=0.9), vjust=-0.25)+
  facet_grid(rows = vars(factor(combinations,levels = c('Two_negative','Opposite','Two_positive'))),cols = vars(factor(interactions,levels=c('-S','+A','AD','-A','+S'))))+
  theme_classic()+
  theme(text = element_text(size = 12),axis.text.x = element_text(angle = 90))
ggsave(figure,filename='F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/interaction_classification.tiff', width = 32, height = 20, units = 'cm',dpi = 300)
