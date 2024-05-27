rcmat <- matrix(c(0,0.2,1,
                  0.2,0.4,2,
                  0.4,0.6,3,
                  0.6,0.8, 4,
                  0.8,1, 5), ncol=3, byrow = T)

classexp <- classify(exp_2020, rcmat, include.lowest = T) %>% 
  mask(nonfuel_2020)

plot(classexp)


studyarea <- crop(classexp, fpa, overwrite = T) %>% 
  mask(fpa)
studyareafires <- crop(fire_perims, fpa)
firesarea <- crop(classexp, studyareafires, overwrite = T) %>% 
  mask(studyareafires)

totalstudyarea <- count(as.data.frame(studyarea), exposure) %>% 
  mutate(proptotsarea = n/sum(n))
totalfirearea <- count(as.data.frame(firesarea), exposure) %>% 
  mutate(proptotfires = n/sum(n)) %>% 
  left_join(totalstudyarea, by = "exposure")

sampletotal <- round(sum(totalstudyarea$n)*0.001)
firestotal <- round(sum(totalfirearea$n.x)*0.001)

samplestudy <- count(spatSample(studyarea, sampletotal, na.rm = T, as.df = T, method = 'random'), exposure) %>% 
  mutate(propsampsarea = n/sum(n)) %>% 
  select(-n) %>% 
  left_join(totalfirearea, by = "exposure")
samplefires <- count(spatSample(firesarea, firestotal, na.rm = T, as.df = T, method = 'random'), exposure) %>% 
  mutate(propsampfires = n/sum(n)) %>% 
  select(-n) %>% 
  left_join(samplestudy, by = "exposure")


props <- samplefires %>% 
  select(c(exposure, proptotsarea,proptotfires,propsampsarea,propsampfires))

ggplot(props, aes(x = exposure)) +
  geom_col(mapping = aes(y = proptotfires), fill = "grey") +
  geom_col(mapping = aes(y = proptotsarea), fill =NA, col = 'black', linetype = 2)

ggplot(props, aes(x = exposure)) +
  geom_col(mapping = aes(y = propsampfires), fill = "grey") +
  geom_col(mapping = aes(y = propsampsarea), fill =NA, col = 'black', linetype = 2)
