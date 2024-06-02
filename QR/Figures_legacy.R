### Legacy figures; 

##To be moved

{r open access}
#| echo: false
#| message: false
#| warning: false
nor_bib |> 
  group_by(oa_status) |> 
  tally() |> 
  ggplot(aes(oa_status, n, fill=oa_status))+
  geom_histogram(stat="identity")+
  scale_fill_manual(values=c(bronze="#CD7F32", closed="red", gold="gold", green="green", hybrid="forestgreen"))+
  labs(x="", y="Antall publikasjoner")+
  theme(legend.position = "Null")


{r topic model}
#| echo: false
#| message: false
#| warning: false



jn_dtm<-join_Nor_form |> 
  select(study_id, title, abstract, year)

#jn_dtm$study_id<-unlist(jn_dtm$study_id)

jn_dtm<-jn_dtm |> 
  mutate(title_abs=paste0(title,abstract)) |>
  unnest_tokens(word,title_abs) |> 
  rowid_to_column()
jn_dtm<-jn_dtm |> 
  mutate(word=removeNumbers(word))

jn_dtm<-jn_dtm |> 
  filter(word !="")

jn_test<-jn_dtm |> 
  distinct(study_id,.keep_all=TRUE)

jn_dtm<-jn_dtm|> 
  group_by(study_id) |> 
  count(study_id, word, sort = TRUE)
data(stop_words)
jn_dtm<-jn_dtm |> 
  anti_join(stop_words)



jn_dtm<-
  jn_dtm |> 
  cast_dtm(study_id, word, n)

{r}
#| echo: false
#| message: false
#| warning: false
# number of topics

result <- FindTopicsNumber(
  jn_dtm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

#FindTopicsNumber_plot(result)

saveRDS(result, "data/cache_data/result.RDS")

{r}
#| echo: false
#| message: false
#| warning: false
jn_lda <- LDA(jn_dtm, k = 6, control = list(seed = 1234))
saveRDS(jn_lda, "data/cache_data/jn_lda.RDS")


{r}
#| echo: false
#| message: false
#| warning: false
jn_topics <- tidy(jn_lda, matrix = "beta")
top_terms <- jn_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>% 
  ungroup() %>%
  arrange(topic, -beta)

p1<-top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

p1


{r}
#| echo: false
#| message: false
#| warning: false


#as.data.frame(terms(jn_lda, 10))

#data.frame(Topic = topics(jn_lda))

lda.topics<- 
  data.frame(jn_test, Topic = topics(jn_lda)) |> 
  add_count(title, Topic) |> 
  group_by(title) |>  
  mutate(Share = n/sum(n)) |> 
  ungroup() |>  
  mutate(Topic = paste0("Topic ", sprintf("%02d", Topic)))  |>  
  mutate(Article = as_factor(title))

# ggplot(lda.topics, aes(title, Share, color = Topic, fill = Topic)) + geom_bar(stat="identity") + ggtitle("LDA topics in Norwegian LULCC peer-reviewed studies") + xlab("") + ylab("Share of topics [%]") + theme(axis.text.x = element_text(angle = 45, hjust = 1))



{r keyword co-occurrence}
#| echo: false
#| message: false
#| warning: false

# Create keyword co-occurrences network

NetMatrix <- biblioNetwork(nor_bib, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

{r co-citation}
#| echo: false
#| message: false
#| warning: false

# Create keyword co-occurrences network

NetMatrix <- biblioNetwork(nor_bib, analysis = "co-citation", network = "references", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=FALSE,label=FALSE,edgesize = 10, edges.min=1)

{r}
#| echo: false
#| message: false
#| warning: false
# nor_bib=metaTagExtraction(nor_bib,"CR_SO",sep=";")
# 
# NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "sources", sep = ";")
# net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "auto", size.cex=TRUE, size=15, remove.multiple=FALSE, labelsize=1,edgesize = 10, edges.min=5)

{r}
#| echo: false
#| message: false
#| warning: false
# results<-biblioAnalysis(nor_bib)
# histResults <- histNetwork(nor_bib, sep = ";")


{r}
#| echo: false
#| message: false
#| warning: false
#| fig-cap: "High quality"
sysRev |> 
  group_by(high_quality_systematic_review) |> 
  tally() |> 
  drop_na() |> 
  ggplot(aes(reorder(high_quality_systematic_review,n), n, fill=high_quality_systematic_review))+
  geom_histogram(stat="identity") +
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner")+
  coord_flip()+
  scale_fill_viridis(discrete = TRUE, option = "D") 


