import testthis


def trending(textOnPage: list) -> list:
    top_n_words = 10
    word_counts = {"placeholder": 0, }
    stopWords = ["the", "a", "with", "says", "will", "have"]
    for word in textOnPage:
        if word.lower in stopWords:
            continue
        if word in word_counts.keys():
            word_counts[word] += 1
        else:
            word_counts[word] = 1
    # print(word_counts.items())
    sorted_word_counts = sorted(word_counts.items(), key=lambda x: x[1], reverse=True)
    top_10_words = sorted_word_counts[:top_n_words]

    return top_10_words


if __name__ == '__main__':
    result = testthis.doload("https://lite.cnn.com")
    top_10_words = trending(result)
    print(top_10_words)
    words_boring = []
    for key_value_pair in top_10_words:
        words_boring.append(key_value_pair[0])
    print(f"this is words:\n{words_boring}")

    # serves same result as above but more fancy:
    words_fancy = [key_value_pair[0] for key_value_pair in top_10_words]
    print(f"this is words with list comprehension:\n{words_fancy}")
