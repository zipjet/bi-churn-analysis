import pandas as pd
import numpy as np
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.metrics.pairwise import cosine_similarity

def get_text_similarities(products, product_types):
    """
    Runs a CountVectorizer to find text similarities between the list of product and product_types
    :param products: list of original product names
    :param product_types: list of product types that the product names should be compared to
    :return: matrix of similarities with shape len(products) x len(product_types)
    """

    vectorizer = CountVectorizer(products, ngram_range=(3, 5), analyzer='char')
    vectorizer.fit(products)

    X = vectorizer.transform(products).toarray()
    y = vectorizer.transform(product_types).toarray()

    similarities = cosine_similarity(X, y)

    return similarities


def get_product_types(products, product_types):
    """
    For each product in product list find the best matching product_type based on text similarity
    :param products: list of original product names
    :param product_types: list of product_types that the product names should be grouped to
    :return: dictionary with products and their corresponding product types
    """

    # get text similarities
    similarities = get_text_similarities(products, product_types)

    # for each product get the index of product_type that is the best match
    best_matches = similarities.argmax(axis=1)
    best_product_types = np.array(product_types)[best_matches]

    products_grouped = dict(zip(products, best_product_types))

    return products_grouped


def main():
    df_products = pd.read_csv("../data/products.csv")  # df with IDs and english names for all products
    products = df_products.product_name.sort_values().unique().tolist()

    df_itemization = pd.read_csv("../data/itemization.csv")
    df_itemization['product_type_sort'] = df_itemization.product_type_category + '_' + df_itemization.product_type
    product_types = df_itemization.product_type.unique().tolist()

    product_groups = get_product_types(products, product_types)

    print('done')

if __name__ == '__main__':
    main()