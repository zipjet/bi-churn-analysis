import pandas as pd
import glob


def parse_json_columns(df_braze, col_name, new_col_name, id_cols=None):
    if not id_cols:
        id_cols = ['customer_id']
    
    df = df_braze[id_cols + [col_name]]
    # explode the list of campaigns to columns
    df = pd.concat([df[id_cols], df[col_name].apply(pd.Series)], axis=1)
    # melt the exploded columns to rows
    df = df.melt(id_vars=id_cols, value_name=new_col_name)
    # remove NaNs campaigns
    df = df.drop('variable', axis=1)
    df = df.loc[~df[new_col_name].isnull()].reset_index(drop=True)
    # explode the campaign json to columns
    df_json = pd.io.json.json_normalize(df[new_col_name])
    df_conc = pd.concat([df[id_cols], df_json], axis=1)
    
    # sort and create counter
    try:
        df_conc = df_conc.sort_values(id_cols + ['last_received'])
    except:
        df_conc = df_conc.sort_values(id_cols + ['last_entered'])
    df_conc[new_col_name + '_num'] = df_conc.groupby(id_cols).cumcount()
    df_conc = df_conc.rename(columns={'name': new_col_name + '_name'})

    return df_conc


def load_data(data_dir):
    braze_files = glob.glob(data_dir)
    braze_list = []

    for f in braze_files:
        df = pd.read_json(f, orient='records', lines=True)
        braze_list.append(df)

    df_braze = pd.concat(braze_list)
    return df_braze

def get_braze_campaigns(df_braze, out_file):
    df_camp = parse_json_columns(df_braze, 'campaigns_received', 'campaign')

    df_camp.columns = [c.split('.')[-1] for c in df_camp.columns]
    
    df_camp.to_csv(out_file, index=False)
    print('Donwloaded campaign data to', out_file)
    
    return df_camp

def get_braze_canvases(df_braze, out_file):
    df_canv = parse_json_columns(df_braze, 'canvases_received', 'canvas')
    df_steps = parse_json_columns(df_canv, 'steps_received', 'canvas_step', 
                                  id_cols=['customer_id', 'api_canvas_id'])
    df_canv_steps = df_canv.merge(df_steps, on=['customer_id', 'api_canvas_id'], 
                                  how='left')
    
    df_canv_steps.to_csv(out_file, index=False)
    print('Downloaded canvas data to', out_file)
    
    return df_canv_steps
        
def main():
    df_braze = load_data('../../data/braze_export_segment/*.txt')
    df_braze = df_braze.rename(columns={'external_id': 'customer_id'})
    
    df_camp = get_braze_campaigns(df_braze, '../../data/braze_campaigns.csv')
    df_canv = get_braze_canvases(df_braze, '../../data/braze_canvases.csv')
    
    df_cust = df_braze.drop(['campaigns_received', 'canvases_received'], axis=1)
    df_cust.to_csv('../../data/braze_customers.csv', index=False)
    
    print('done')


if __name__ == '__main__':
    main()


