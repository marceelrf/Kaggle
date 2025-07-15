# os culpados

rf_partial_fil <- fit(final_wf, train_data)

v1 <- predict(rf_partial_fil, val_data)

v1 <- v1 %>% 
  bind_cols(val_data) 

v1 %>% 
  mutate(Personality = factor(Personality)) %>% 
  precision(truth = Personality, estimate = .pred_class)

v1 %>% 
  mutate(Personality = factor(Personality)) %>% 
  accuracy(truth = Personality, estimate = .pred_class)

v1 %>% 
  mutate(Personality = factor(Personality)) %>% 
  f_meas(truth = Personality, estimate = .pred_class)

problemas <- 
  v1 %>% 
  filter(Personality != .pred_class)


problemas %>% 
  group_by(id) %>% 
  ggplot(aes(x = Stage_fear, fill = Personality, col = .pred_class)) +
  geom_bar() +
  facet_wrap(~.pred_class)

problemas %>% 
  group_by(Stage_fear, Personality, .pred_class) %>%
  count()

problemas %>% 
  group_by(Drained_after_socializing, Personality, .pred_class) %>%
  count()


problemas %>% 
  ggplot(aes(x = Time_spent_Alone, y = Going_outside,
             col = Personality)) +
  geom_point() +
  facet_wrap(~.pred_class)

problemas %>% 
  ggplot(aes(x = Time_spent_Alone, y = Social_event_attendance,
             col = Personality)) +
  geom_point() +
  facet_wrap(~.pred_class)

# Acertou? ----------------------------------------------------------------

v1 <- v1 %>% 
  mutate(Acertou = case_when(
    .pred_class == Personality ~ "Sim",
    TRUE~ "Não"
  )) 

v1 %>% 
  ggplot(aes(x = Time_spent_Alone, y = Social_event_attendance,
             col = Personality)) +
  geom_hex() +
  facet_wrap(Acertou~.pred_class)

v1 %>% 
  group_by(Stage_fear, Drained_after_socializing,
           Personality, Acertou) %>%
  count(sort = T)

v1 %>% 
  ggplot(aes(x = Time_spent_Alone, fill = Stage_fear)) +
  geom_histogram(position = "dodge")+
  facet_wrap(~Acertou, scales = "free")

v1 %>% 
  ggplot(aes(x = Time_spent_Alone, fill = Stage_fear)) +
  geom_histogram(position = "dodge")+
  facet_wrap(Drained_after_socializing~Acertou, scales = "free")
