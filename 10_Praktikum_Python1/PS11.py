import testthis


def trending(textOnPage: list):
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
    # Sort the dictionary by values in descending order
    sorted_word_counts = sorted(word_counts.items(), key=lambda x: x[1], reverse=True)

    # Return the top 10 words
    top_10_words = sorted_word_counts[:10]

    return top_10_words


if __name__ == '__main__':
    result = testthis.doload("https://lite.cnn.com")
    top_10_words = trending(result)
    print(top_10_words)