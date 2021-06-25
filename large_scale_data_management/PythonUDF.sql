DROP FUNCTION cossim(STRING, STRING);
CREATE FUNCTION cossim(titles STRING, query STRING)
RETURNS FLOAT
LANGUAGE PYTHON
{
    from sklearn.feature_extraction.text import CountVectorizer
    from sklearn.metrics.pairwise import cosine_similarity
    cv = CountVectorizer(strip_accents='unicode', stop_words='english', min_df=0.1)
    titles_fit = cv.fit_transform(titles)
    query_fit = cv.transform([query])
    results = cosine_similarity(query_fit, titles_fit)
    return results
};
