library(stringr)


      


dt <- read.csv("wiadomosci.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")



df2 <- dt %>%
  filter(tolower(author_role) == "assistant") %>%
  mutate(message_text = as.character(message_text))

normalize_text <- function(txt) {
  txt[is.na(txt)] <- ""
  txt <- tolower(txt)
  txt <- str_replace_all(txt, regex("c\\+\\+", ignore_case = TRUE), "cplusplus")
  txt <- str_replace_all(txt, regex("c#", ignore_case = TRUE), "csharp")
  txt <- str_replace_all(txt, "[^\\p{L}\\p{N}]+", " ")
  str_trim(txt)
}

to_root <- function(word) {
  str_replace(word,
              "(owani|owanie|owania|owaniu|owaniach|owaniom|owaniami|ami|ach|owie|owego|owej|owi|om|ie|a|e|i|y|u|em|ą|ę|o|ego|ych|ymi|ów|ówka|ówce|kę|ką|arz|acja|acje|acyj|owy|owy|owy)$",
              "")
}

allowed_short <- c("r","js","py","sql","go","c","cpp","csharp")



tematy <- list(
  "topologia" = c("topologia","otwarty","domkni","zwarto","homeomorfizm","homotopia",
                  "homologia","fundamental group","grupa fundamentalna","przestrzen metryczna",
                  "metryka","topologia produktowa","topologia zariski"),
  "teoria_miary" = c("miara","sigma","sigma-algebra","mierzaln","Lebesgue","całka","miarowy",
                     "zbior miary zero","twierdzenie Fubiniego","dominated convergence","monotone convergence","L^p","lp","Lp"),
  "geometria_rozniczkowa" = c("różniczkow","manifold","rozmaitoś","metryka Riemanna","tensor","krzywizna","geodez","geodesic",
                              "forma","różniczka","connection","Christoffel","Riemann","Gauss"),
  "analiza_rzeczywista" = c("granica","ciąg","szereg","konwergencj","pochodn","pochodna","całk","funkcja ciagła",
                            "Cauchy","epsilon","delta","Taylor","szereg Taylora","zespolon","holomorf","residuum",
                            "prawdopodobie","wariancj","odchylenie","rozkład","estymator","hipotez","p-value",
                            "regresj","MLE","maximum likelihood","proces stochastyczny","markov","Brownian","kowariancj",
                            "równanie różniczkowe","ODE","PDE","metoda Eulera","Runge-Kutta","stabilnoś","Laplace",
                            "chaos","bifurkacja","atraktor","Lyapunov","iteracja",
                            "optymalizacj","gradient","hessian","convex","funkcja celu","Lagrange","simplex",
                            "numeryk","aproksymacj","interpolacj","FEM","metoda rónic skończonych"),
  "algebra_liniowa" = c("macierz","wektor","wyznacznik","determinant","ród","rank","odwrotn","wartość własna",
                        "wektor własny","eigen","svd","ortogonaln","diagonalizacj","spektral",
                        "grupa","pierścień","moduł","homomorfizm","izomorfizm","ideal","Galois","ciało","ring",
                        "banach","hilbert","operator liniow","norma","spektrum","operator kompaktowy","riesz","dual",
                        "liczba pierwsz","kongruencj","modulo","Fermat","Euler","resztk","gcd","lcm",
                        "kombinatoryk","permutacj","kombinacj","graf","drzewo","sciezka","cykl","matching","kolorowan","MST",
                        "kategoria","funktor","morphism","naturaln transformacj","adjunction"),
  # 4b) TEORIA PRAWDOPODOBIEŃSTWA (wydzielona osobno)
  "teoria_prawdopodobienstwa" = c(
    # podstawy
    "prawdopodobie","zdarzen","przestrzen probabilistyczna","omega","sigma-algebra",
    "zmienna losowa","losow","dystrybuanta","cdf","pdf","pmf",
    # rozkłady
    "rozkład","normaln","gauss","dwumian","bernoulli","poisson","wykładnicz",
    "gamma","beta","chi-kwadrat","student","t-Studenta",
    # momenty i charakterystyki
    "wartość oczekiwana","oczekiwan","wariancj","odchylenie","moment","kowariancj","korelacj",
    # zbieżności i twierdzenia graniczne
    "prawo wielkich liczb","lln","centralne twierdzenie graniczne","clt",
    "zbieżność","prawie na pewno","w prawdopodobieństwie","w rozkładzie", "Borela-Cantallego",
    # procesy losowe
    "proces stochastyczny","łańcuch markowa","markov","ergodyczn",
    "proces poisson","ruch browna","brownian","martingale","martyngał",
    "czas zatrzymania","stopping time",
    # miara probabilistyczna / teoria miary w probabilistyce
    "miara probabilistyczna","p-rawie wszędzie","a\\.e\\.","niezależn",
    "warunkow","prawdopodobieństwo warunkowe","bayes","twierdzenie bayesa",
    # symulacje
    "monte carlo","symulacja","sampling","losowanie","bootstrap"
  ),
  "informatyka" = c(
    "java","jvm","python","py","pandas","numpy","scipy","matplotlib","seaborn","scikit","sklearn","r\\b","tidyverse",
    "dplyr","ggplot2","data\\.table","tidyr","list","stack","queue","heap","tree","graf","dfs","bfs",
    "algorytm","frontend","backend","docker","kubernetes","ci","cd","git","bug","error","exception")
)


tematy_patterns <- lapply(tematy, function(words) {
  if(length(words)==0) return(NA_character_)
  tokens <- unlist(lapply(words, function(k) str_split(normalize_text(k), "\\s+")[[1]]))
  tokens <- tokens[tokens!=""]
  roots <- unique(to_root(tokens))
  roots <- roots[nchar(roots)>=3 | roots %in% allowed_short]
  if(length(roots)==0) return(NA_character_)
  roots_esc <- vapply(roots, function(s) str_replace_all(s,"([\\W])","\\\\\\1"), "")
  paste0("\\b(", paste(roots_esc, collapse="|"), ")[\\p{L}]*\\b")
})

assign_topic <- function(text, patterns) {
  txt <- normalize_text(text)
  for(nm in names(patterns)) {
    pat <- patterns[[nm]]
    if(is.na(pat)) next
    if(str_detect(txt, regex(pat, ignore_case = TRUE))) return(nm)
  }
  "inne"
}

df2 <- df2 %>%
  mutate(topic = vapply(message_text, assign_topic, FUN.VALUE = character(1), patterns = tematy_patterns))

gotowe <- df2 %>%
  count(topic, name = "n") %>%
  arrange(desc(n)) %>%
  mutate(topic = forcats::fct_reorder(topic, n)) %>%
  arrange(desc(n))


write.csv(gotowe, "wiadomosci_przetworzone.csv",
          row.names = FALSE, fileEncoding = "UTF-8")


