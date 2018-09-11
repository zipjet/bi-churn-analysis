import pandas as pd
import numpy as np
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.metrics.pairwise import cosine_similarity


def _get_text_similarities(products, product_types):
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


def _get_product_types(products, product_types):
    """
    For each product in product list find the best matching product_type based on text similarity
    :param products: list of original product names
    :param product_types: list of product_types that the product names should be grouped to
    :return: dictionary with products and their corresponding product types
    """

    # get text similarities
    similarities = _get_text_similarities(products, product_types)

    # for each product get the index of product_type that is the best match
    best_matches = similarities.argmax(axis=1)
    best_product_types = np.array(product_types)[best_matches]

    products_grouped = dict(zip(products, best_product_types))

    return products_grouped


def group_products(save_path='../data/product_groups.csv'):
    # load product names
    df_products = pd.read_csv("../data/products.csv")
    products = df_products.product_name.sort_values().unique().tolist()

    # load product types
    # https://docs.google.com/spreadsheets/d/1bWyhdLxkGqO6MsCuwc6aaj5ZIPTVXG14AJ_sOKh2-gQ/edit?usp=drive_web&ouid=109217700245759892602
    df_itemization = pd.read_csv("../data/itemization.csv")
    product_types = df_itemization.product_type.unique().tolist()

    product_groups = _get_product_types(products, product_types)

    df_products = pd.DataFrame.from_dict(product_groups, orient='index').reset_index()
    df_products.columns = ['product_name', 'product_type']
    df_products = df_products.merge(df_itemization, on='product_type')

    print('Writing grouped products to', save_path)
    df_products.to_csv(save_path, index=False)

if __name__ == '__main__':
    group_products()