text_prep <- function(data){
    data %>%
      tidytext::unnest_tokens(output = sentence, input = text,
                              token = "sentences", to_lower = FALSE) %>%
      dplyr::mutate(sentence_id = dplyr::row_number()) %>%
      dplyr::group_by(sentence_id, add=TRUE) %>%
      dplyr::group_modify(~ {
          .x %>%
              tidytext::unnest_tokens(output = word, input = sentence,
                                      token = "words", to_lower=FALSE) %>%
              dplyr::mutate(word_id = dplyr::row_number())
      }) %>%
      ungroup_by("sentence_id")
}
