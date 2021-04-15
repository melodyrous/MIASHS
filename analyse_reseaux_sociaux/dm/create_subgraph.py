import pandas as pd
import numpy as np
import argparse

if __name__ == '__main__':

    # construct the parser
    parser = argparse.ArgumentParser(description='Create a subgraph.')

    parser.add_argument('input', help='The original graph file (csv or compressed).')
    parser.add_argument('output', help='The destination graph file (csv or compressed).')
    parser.add_argument('--rows', '-r', dest='nb_rows', type=int, default=10000, help='Number of edges to select.')

    args = parser.parse_args()

    # load the original graph csv
    df = pd.read_csv(
        args.input,
        header='infer',
        sep=' ',
        low_memory=False,
        dtype={'dest': np.int32, 'source': np.int32},
        nrows=args.nb_rows,
    )
    print('Dataframe loaded with ' + str(len(df)) + ' rows.')
    # save the selected rows to diskargparse
    
    df.to_csv(args.output, header=True, sep=' ', index=False)