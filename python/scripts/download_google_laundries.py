import requests
import json
import time
import pandas as pd
import os

API_KEY = 'AIzaSyA_vJfQZzFLfLQoXX_68EdkZU3wS9F8riQ'


def nearby_laundry_search(lat=None, lng=None, radius=None, page_token=None):
    places_api_url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?'
    places_api_query = 'key={}'.format(API_KEY)
    if not page_token:
        places_api_query += '&location={lat},{lng}&radius={radius}&type=laundry'.format(
            lat=lat, lng=lng, radius=radius)
    else:
        places_api_query += '&pagetoken={}'.format(page_token)

    response = requests.get(places_api_url + places_api_query)
    data = json.loads(response.text)

    return data.get('results'), data.get('next_page_token')


def get_nearby_laundries(lat, lng, radius):
    laundry_list = []

    results, next_page_token = nearby_laundry_search(lat, lng, radius)
    laundry_list += results

    while next_page_token:
        time.sleep(10)
        results, next_page_token = nearby_laundry_search(page_token=next_page_token)
        laundry_list += results

    print('Downloaded {} records'.format(len(laundry_list)))
    return laundry_list


def get_nearby_laundries_df(lat, lng, radius):
    laundries = get_nearby_laundries(lat, lng, radius)

    df = pd.DataFrame.from_records(laundries)
    df = pd.concat([df, df['geometry'].apply(pd.Series)['location'].apply(pd.Series)], axis=1)
    df = df[['place_id', 'name', 'lat', 'lng', 'vicinity', 'rating']]

    return df


def download_laundry_data(data_path):
    cities = {
        'Berlin': {'lat': 52.52437, 'lng': 13.41053, 'radius': 300000},
        'London': {'lat': 51.509865, 'lng': -0.118092, 'radius': 30000},
        'Paris': {'lat': 48.864716, 'lng': 2.349014, 'radius': 30000}
    }
    file = os.path.join(data_path, 'laundries.csv')

    for idx, city in enumerate(cities.keys()):
        try:
            df_city = get_nearby_laundries_df(**cities[city])
            df_city['city'] = city
            df_city.to_csv(file, index=False, encoding='utf-8', mode='a', header=(idx == 0))
            print('Saved to {}'.format(file))
        except Exception as e:
            print('Problem with {}: {}'.format(city, e))


def main():
    download_laundry_data('../../data/input')


if __name__ == '__main__':
    main()