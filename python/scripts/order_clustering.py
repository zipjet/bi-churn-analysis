import pandas as pd
from sklearn.externals import joblib
from sklearn.cluster import KMeans


def kmeans_clustering(X, n_clusters=10, dump_path=None):
    kmeans = KMeans(n_clusters=n_clusters)
    kmeans.fit(X)

    if dump_path:
        joblib.dump(kmeans, dump_path)

    return kmeans


def load_cluster_model(model_path):
    kmeans = joblib.load(model_path)
    return kmeans


def get_cluster_names(df_centers, threshold=0.5):

    cluster_names = {}

    for cluster in df_centers.index:
        names = []
        new_threshold = threshold

        while len(names) == 0:
            col = df_centers.T[cluster].sort_values(ascending=False)
            col_perc = col / col.sum()
            mask = (col_perc > new_threshold)
            names = col.loc[mask].index.tolist()
            new_threshold = new_threshold - 0.1

        cluster_names[cluster] = '+'.join(names)

    return cluster_names

def prepare_dummies(items_csv_path):

    df = pd.read_csv(items_csv_path) # df with items per order

    # for each customer and product_group get how many orders this group was included in and in what quantity
    df_group = df.groupby(['order_id', 'product_group'])\
                .agg({'quantity': 'sum'})\
                .reset_index()\
                .set_index('order_id')

    # get dummy values of customers and product_groups
    df_dumm = pd.get_dummies(df_group['product_group'])
    df_dumm = df_dumm.reset_index().groupby(['order_id']).max()

    return df_dumm


def cluster_orders(items_csv_path, save_csv_path,
                   n_clusters=10, load_model_path=None, save_model_path=None):

    df = prepare_dummies(items_csv_path)
    if load_model_path:
        kmeans = load_cluster_model(load_model_path)
    else:
        kmeans = kmeans_clustering(df.values, n_clusters=n_clusters, dump_path=save_model_path)

    clusters = kmeans.labels_.tolist()
    centers = kmeans.cluster_centers_

    df_centers = pd.DataFrame(data=centers, columns=df.columns)
    cluster_names = get_cluster_names(df_centers)
    df_centers['cluster_name'] = df_centers.index.map(cluster_names)

    df['cluster'] = clusters
    df['cluster_name'] = df.cluster.map(cluster_names)
    df = df.reset_index()[['order_id', 'cluster', 'cluster_name']]
    df.to_csv(save_csv_path, index=False)

    return kmeans, df, df_centers


if __name__ == '__main__':
    items_path = '../../data/items.csv'
    save_path = '../../data/clustered_orders.csv'

    load_model = '../../data/models/orders_clf.pkl'

    cluster_orders(items_path, save_path, load_model_path=load_model)