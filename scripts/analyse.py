import pandas as pd


RESULTS_DIR = "data"

def find_best_speedups(df):

    # Get sequential runtimes for each language
    c_seq_time = df[(df['language'] == 'c') & 
                    (df['implementation'] == 'seq')]['runtime_median'].iloc[0]
    go_seq_time = df[(df['language'] == 'go') & 
                     (df['implementation'] == 'seq')]['runtime_median'].iloc[0]
    
    # Calculate speedup for parallel implementations
    c_parallel = df[(df['language'] == 'c') & (df['implementation'] == 'par')].copy()
    go_parallel = df[(df['language'] == 'go') & (df['implementation'] == 'par')].copy()
    
    c_parallel['speedup'] = c_seq_time / c_parallel['runtime_median']
    go_parallel['speedup'] = go_seq_time / go_parallel['runtime_median']
    
    # return configurations with best speedup   
    return {
        'c': c_parallel.loc[c_parallel['speedup'].idxmax()],
        'go': go_parallel.loc[go_parallel['speedup'].idxmax()]
    }



def main():

    # Load the data
    ds1_df = pd.read_csv(f"{RESULTS_DIR}/ds1.csv")
    ds2_df = pd.read_csv(f"{RESULTS_DIR}/ds2.csv")
    ds3_df = pd.read_csv(f"{RESULTS_DIR}/ds3.csv")

    ds1_best = find_best_speedups(ds1_df)
    ds2_best = find_best_speedups(ds2_df)

    print("DS1")
    print(f"Best Go configuration:\n\tparallel runtime: {ds1_best['go'].runtime_median:.5f} \n\tspeedup: {ds1_best['go'].speedup:.2f}x\n\tcore count: {ds1_best['go'].core_count}\n")
    print(f"Best OpenMP configuration:\n\tparallel runtime: {ds1_best['c'].runtime_median:.5f} \n\tspeedup: {ds1_best['c'].speedup:.2f}x\n\tcore count: {ds1_best['c'].core_count}")

    print("DS2")
    print(f"Best Go configuration:\n\tparallel runtime: {ds2_best['go'].runtime_median:.5f} \n\tspeedup: {ds2_best['go'].speedup:.2f}x\n\tcore count: {ds2_best['go'].core_count}\n")
    print(f"Best OpenMP configuration:\n\tparallel runtime: {ds2_best['c'].runtime_median:.5f} \n\tspeedup: {ds2_best['c'].speedup:.2f}x\n\tcore count: {ds2_best['c'].core_count}")


        

if __name__ == "__main__":
    main()