import pandas as pd
import numpy as np
import string as s

'''
Get size of tree database
The tree database has 683,788 rows.
'''
soql_url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?$limit=5&$offset=' + str(0) + \
            '&$select=count(tree_id)').replace(' ', '%20')
soql_size = pd.read_json(soql_url)
print('Size of data set is ' + str(soql_size))

'''
Shape of Data being Retrieved
There are over 600,000 rows in the entire data set. I am only going to retrieve the relevant columns and relevant aggregation through the API call.
Columns retrieved:
borocode: 1 (Manhattan), 2 (Bronx), 3 (Brooklyn), 4 (Queens), 5 (Staten Island)
spc_common: specie name (132 unique species)
health: good, fair, poor
steward: Indicates the number of unique signs of stewardship observed for this tree (none, 1or2, 3or4, 4ormore)
Through trial and error, I determined that there is total of 4565 rows when data is grouped by borocode, spc_common, health, and steward. The limit value is set to 1000 since this is the maximum number of rows that can be retrieved through each API call. The offset value increments by 1000. max_row is set to 5000. This allows us to retrieve all 4565 rows.
NOTE: I had to use 'borocode' because I was getting error when I used the 'borough' column to group the data by.
'''

offset = 1000
max_row = 5000

'''
Retrieve data set of NYC trees through API call using Socrata query.
'''

for x in range(0, max_row, offset):
    # print('x is ' + str(x))
    soql_url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?$limit=1000&$offset=' + str(x) + \
                '&$select=borocode,spc_common,health,steward,count(tree_id)' + \
                '&$group=borocode,spc_common,health,steward').replace(' ', '%20')
    soql_trees = pd.read_json(soql_url)
    if (x == 0):
        df = pd.DataFrame(columns=list(soql_trees.columns.values))
    df = df.append(soql_trees)
    # print(df)

df = df.reset_index(drop=True)

'''
Size of retrieved data set:
'''
print('Size of retrieved data set:' + str(len(df)))

'''
There are 132 unique tree specie name
'''
print('No. of unique species: ' + str(len(list(df.spc_common.unique()))))

'''
Remove rows that do not have complete data. 11 rows removed that do not have complete data.
'''
df = df.dropna(axis=0, how='any')

'''
Preview data set retrieved: 
'''
print(df.head(5))

'''
Prepare data to be able to provide functionality that arborist needs
Build a dash app for a arborist studying the health of various tree species (as defined by the variable ‘spc_common’) across each borough (defined by the variable ‘borough’). This arborist would like to answer the following two questions for each species and in each borough:
What proportion of trees are in good, fair, or poor health according to the ‘health’ variable?
Are stewards (steward activity measured by the ‘steward’ variable) having an impact on the health of trees?
'''

'''
Plan to meet requirements for question 1
For every specice and in each borough, what proportion of trees are in good, fair, or poor health?
The application will allow arborist to select one specie, and the application will display proportion of trees that are in good, fair, or poor health 
across all boroughs. Arborist will be able to compare health of particular specie across all five boroughs.
Bar graphs will be used to present the proportions. The orientation of the bar graphs will be vertical. 
The bar graphs will be first grouped by boroughs for each health status.
The goal of the code below is to create a dataframe that has the columns: borocode, spc_common, health, ratio.
Ratio is the proportion of spc_common in the given borough that has the given heath level. 
For example, a ratio for red maple in Queens with a health of good is the proportion of good red maple trees in Queens.
'''

# Reshape data for question 1:
df_totals = df.groupby(['borocode', 'spc_common'])['count_tree_id'].sum()
df_total_by_borocode_specie_health = df.groupby(['borocode', 'spc_common', 'health'])['count_tree_id'].sum()
df_totals = df_totals.reset_index(drop=False)
df_total_by_borocode_specie_health = df_total_by_borocode_specie_health.reset_index(drop=False)
df_totals.columns = ['borocode', 'spc_common', 'total_for_specie_in_borough']
df_total_by_borocode_specie_health.columns = ['borocode', 'spc_common', 'health', 'total']
tree_proportions = pd.merge(df_total_by_borocode_specie_health, df_totals, on=['borocode', 'spc_common'])
tree_proportions['ratio'] = tree_proportions['total'] / tree_proportions['total_for_specie_in_borough']
tree_proportions['spc_common'] = tree_proportions['spc_common'].apply(lambda x: x.title())
# print(tree_proportions.head(10))

# For species dropdown:
species = np.sort(tree_proportions.spc_common.unique())

'''
Preparing data to meet requirements for question 2
I would like to use a scatter plot to represent the overall health status of the selected specie across all the boroughs. 
An overall health index is determined by assigning a numeric value to each health level (Poor=1, Fair=2, Good=3) and 
then calculating a weighted average for the selected specie for each borough. 
The overall health index score has a minimum score of 1 and a maximum score of 3.
'''
# Reshape data for question 2:
df_total_by_steward = df.groupby(['borocode', 'spc_common', 'steward'])['count_tree_id'].sum()
df_total_by_steward = df_total_by_steward.reset_index(drop=False)
df_total_by_steward.columns = ['borocode', 'spc_common', 'steward', 'steward_total']
df['borocode'] = pd.to_numeric(df['borocode'])
df_steward = pd.merge(df, df_total_by_steward, on=['borocode', 'spc_common', 'steward'])
di = {'Poor': 1, 'Fair': 2, 'Good': 3}
df_steward['health_level'] = df_steward['health'].map(di)
# df_steward.sort_values(by=['borocode', 'spc_common', 'steward']).head(10)
df_steward['health_index'] = (df_steward['count_tree_id'] / df_steward['steward_total']) * df_steward['health_level']
# df_steward.sort_values(by=['borocode', 'spc_common', 'steward']).head(10)
df_overall_health_index = df_steward.groupby(['borocode', 'spc_common', 'steward'])['health_index'].sum()
df_overall_health_index = df_overall_health_index.reset_index(drop=False)
df_overall_health_index.columns = ['borocode', 'spc_common', 'steward', 'overall_health_index']
di2 = {'3or4': 3, '4orMore': 4, 'None': 1, '1or2': 2}
df_overall_health_index['steward_level'] = df_overall_health_index['steward'].map(di2)
di3 = {1: 'Manhattan', 2: 'Bronx', 3: 'Brooklyn', 4: 'Queens', 5: 'Staten Island'}
df_overall_health_index['borough'] = df_overall_health_index['borocode'].map(di3)
df_overall_health_index['spc_common'] = df_overall_health_index['spc_common'].apply(lambda x: x.title())
# print(df_overall_health_index.head(10))


'''
Code below if for DASH application
'''

import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output

import plotly.graph_objs as go

external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)

app.layout = html.Div([
    html.H4('Select Tree Specie'),

    dcc.Dropdown(
        id='specie',
        options=[{'label': i, 'value': i} for i in species],
        value="'Schubert' Chokecherry",
        style={'height': 'auto', 'width': '300px'}
    ),

    dcc.Graph(id='graph-ratio'),

    dcc.Graph(id='graph-health')

], style={'columnCount': 1})


# Display Proportion Graph
@app.callback(
    Output('graph-ratio', 'figure'),
    [Input('specie', 'value')])
def update_figure(selected_specie):
    filtered_df = tree_proportions[tree_proportions.spc_common == selected_specie]
    # borocode: 1 (Manhattan), 2 (Bronx), 3 (Brooklyn), 4 (Queens), 5 (Staten Island)
    manhattan = filtered_df[filtered_df.borocode == 1]
    bronx = filtered_df[filtered_df.borocode == 2]
    brooklyn = filtered_df[filtered_df.borocode == 3]
    queens = filtered_df[filtered_df.borocode == 4]
    staten_island = filtered_df[filtered_df.borocode == 5]

    traces = []

    traces.append(go.Bar(
        x=queens['health'],
        y=queens['ratio'],
        name='Queens',
        opacity=0.9
    ))

    traces.append(go.Bar(
        x=manhattan['health'],
        y=manhattan['ratio'],
        name='Manhattan',
        opacity=0.9
    ))

    traces.append(go.Bar(
        x=bronx['health'],
        y=bronx['ratio'],
        name='Bronx',
        opacity=0.9
    ))

    traces.append(go.Bar(
        x=brooklyn['health'],
        y=brooklyn['ratio'],
        name='Brooklyn',
        opacity=0.9
    ))

    traces.append(go.Bar(
        x=staten_island['health'],
        y=staten_island['ratio'],
        name='Staten Island',
        opacity=0.9
    ))

    return {
        'data': traces,
        'layout': go.Layout(
            xaxis={'title': 'Health of Trees'},
            yaxis={'title': 'Proportion of Trees in Borough'},
            margin={'l': 40, 'b': 40, 't': 10, 'r': 10},
            legend=dict(x=-.1, y=1.2)
        )
    }


# Display Steward-Health Graph for Question 2
@app.callback(
    Output('graph-health', 'figure'),
    [Input('specie', 'value')])
def update_figure2(selected_specie):
    # print('here: ' + selected_specie)
    filtered_df = df_overall_health_index[df_overall_health_index.spc_common == selected_specie]
    traces2 = []

    for i in filtered_df.borough.unique():
        df_by_borough = filtered_df[filtered_df['borough'] == i]
        traces2.append(go.Scatter(
            x=df_by_borough['steward_level'],
            y=df_by_borough['overall_health_index'],
            mode='markers',
            opacity=0.7,
            marker={
                'size': 15,
                'line': {'width': 0.5, 'color': 'white'}
            },
            name=i
        ))

    return {
        'data': traces2,
        'layout': go.Layout(
            # xaxis={'title': 'Steward Level'},
            yaxis={'title': 'Overall Health Index'},
            xaxis=dict(tickvals=[1, 2, 3, 4], ticktext=['None', '1or2', '3or4', '4orMore'], title='Steward'),
            margin={'l': 40, 'b': 40, 't': 10, 'r': 10},
            legend=dict(x=-.1, y=1.2)
        )
    }


if __name__ == '__main__':
    app.run_server(debug=True)
