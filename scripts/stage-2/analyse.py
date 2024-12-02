import numpy as np
import pandas as pd

RESULTS_DIR = "data/erl_results"

def median_sequential(df):

    seq_df = df[df["implementation"] == "sequential"]
    print(seq_df)
    return seq_df["runtime"].mean()


def main():

    # Load the data
    ds1_df = pd.read_csv(f"{RESULTS_DIR}/ds1.csv")
    ds2_df = pd.read_csv(f"{RESULTS_DIR}/ds2.csv")
    ds3_df = pd.read_csv(f"{RESULTS_DIR}/ds3.csv")

    ds1_median = median_sequential(ds1_df)
    ds2_median = median_sequential(ds2_df)

    print(f"DS1 sequential Median: {ds1_median}")
    print(f"DS2 sequential Median: {ds2_median}")

  

        

if __name__ == "__main__":
    main()