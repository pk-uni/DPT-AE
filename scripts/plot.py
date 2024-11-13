import pandas as pd
import matplotlib.pyplot as plt

RESULTS_DIR = "data"

config = {
    "title.fontsize": 20,
    "axisLabel.fontsize": 18,
    "legend.fontsize": 16,
}


def runtime_graph(df, dataset, log=False, no_seq=False, show=True):
    plt.figure(figsize=(12, 8))

    # Plot parallel implementations
    c_data = df[(df['language'] == 'c') & (df['implementation'] == 'par')]
    plt.plot(c_data['core_count'], c_data['runtime_median'], 
             marker='o', label='C Parallel', 
             color='#1f77b4', linewidth=2)

    go_data = df[(df['language'] == 'go') & (df['implementation'] == 'par')]
    plt.plot(go_data['core_count'], go_data['runtime_median'], 
             marker='s', label='Go Parallel', 
             color='#2ca02c', linewidth=2)

    # Calculate ideal scaling line
    if not no_seq:
        # Use sequential times if available
        c_seq = df[(df['language'] == 'c') & (df['implementation'] == 'seq')]['runtime_median'].iloc[0]
        go_seq = df[(df['language'] == 'go') & (df['implementation'] == 'seq')]['runtime_median'].iloc[0]
        
        plt.axhline(y=c_seq, color='#1f77b4', linestyle=':', label='C Sequential', alpha=0.7)
        plt.axhline(y=go_seq, color='#2ca02c', linestyle=':', label='Go Sequential', alpha=0.7)
        
        sequential_time = min(c_seq, go_seq)
    else:
        # For DS3, use the first measurement point (8 cores) to extrapolate ideal line
        first_core_count = df['core_count'].min()  # Should be 8 for DS3
        first_runtime = df[df['core_count'] == first_core_count]['runtime_median'].min()
        sequential_time = first_runtime * first_core_count  # Extrapolate back to 1 core

    # Plot ideal scaling line
    core_counts = df['core_count'].unique()
    ideal_times = [sequential_time/n for n in core_counts]
    plt.plot(core_counts, ideal_times, '--', label='Ideal Scaling', color='#7f7f7f', linewidth=2)

    # Add vertical lines for physical cores and hardware threads
    plt.axvline(x=16, color='#5a189a', linestyle='-.', alpha=0.5, label='Physical Cores (16)')
    plt.axvline(x=32, color='#b185db', linestyle='-.', alpha=0.5, label='Hardware Threads (32)')

    tag = ""
    if log:
        plt.xscale('log', base=2)
        plt.yscale('log', base=2)
        tag = "_log"

    plt.grid(True, which="both", ls="-", alpha=0.2)

    plt.xlabel('Number of Cores', fontsize=config["axisLabel.fontsize"])
    plt.ylabel('Runtime (seconds)', fontsize=config["axisLabel.fontsize"])

    plt.title("Actual vs Ideal Scaling",fontsize=config["title.fontsize"], pad=20)
    plt.legend(fontsize=config["legend.fontsize"])

    plt.grid(True, which='major', linestyle='-', alpha=0.3)
    plt.grid(True, which='minor', linestyle=':', alpha=0.2)

    plt.tight_layout()
    plt.savefig(f'{RESULTS_DIR}/plots/runtime_analysis_{dataset}{tag}.png', dpi=300, bbox_inches='tight')
    if show:
        plt.show()


def absolute_speedup_graph(df, dataset, show=True):

    plt.figure(figsize=(12, 8))

    # Get sequential times for absolute speedup calculation
    c_seq = df[(df['language'] == 'c') & (df['implementation'] == 'seq')]['runtime_median'].iloc[0]
    go_seq = df[(df['language'] == 'go') & (df['implementation'] == 'seq')]['runtime_median'].iloc[0]

    # Calculate and plot C speedup
    c_data = df[(df['language'] == 'c') & (df['implementation'] == 'par')]
    c_speedup = c_seq / c_data['runtime_median']
    plt.plot(c_data['core_count'], c_speedup, 
             marker='o', label='OpenMP Implementation', 
             color='#1f77b4', linewidth=2)

    # Calculate and plot Go speedup
    go_data = df[(df['language'] == 'go') & (df['implementation'] == 'par')]
    go_speedup = go_seq / go_data['runtime_median']
    plt.plot(go_data['core_count'], go_speedup, 
             marker='s', label='Go Implementation', 
             color='#2ca02c', linewidth=2)

    # Plot ideal speedup line (y = x line)
    core_counts = df['core_count'].unique()
    ideal_speedup = core_counts  # Ideal speedup equals number of cores
    plt.plot(core_counts, ideal_speedup, '--', label='Ideal Speedup', color='#7f7f7f', linewidth=2)

    # Add vertical lines for physical cores and hardware threads
    plt.axvline(x=16, color='#5a189a', linestyle='-.', alpha=0.5, label='Physical Cores (16)')
    plt.axvline(x=32, color='#b185db', linestyle='-.', alpha=0.5, label='Hardware Threads (32)')

    plt.grid(True, which="both", ls="-", alpha=0.2)

    plt.xlabel('Number of Cores', fontsize=config["axisLabel.fontsize"])
    plt.ylabel('Absolute Speedup', fontsize=config["axisLabel.fontsize"])
    plt.title('Speedup vs. Number of Cores',fontsize=config["title.fontsize"], pad=20)
    plt.legend(fontsize=config["legend.fontsize"])

    plt.grid(True, which='major', linestyle='-', alpha=0.3)
    plt.grid(True, which='minor', linestyle=':', alpha=0.2)

    # Add y=x reference line
    max_speedup = max(max(c_speedup), max(go_speedup))
    plt.plot([1, max_speedup], [1, max_speedup], ':', color='red', alpha=0.3, label='Linear Speedup')

    plt.tight_layout()
    plt.savefig(f'{RESULTS_DIR}/plots/absolute_speedup_analysis_{dataset}.png', dpi=300, bbox_inches='tight')
    if show:
        plt.show()


def relative_speedup_graph(df, dataset, baseline_cores=8, show=True):

    plt.figure(figsize=(12, 8))

    # Get baseline times (8-core runtime for each implementation)
    c_base = df[(df['language'] == 'c') & 
                (df['core_count'] == baseline_cores)]['runtime_median'].iloc[0]
    go_base = df[(df['language'] == 'go') & 
                 (df['core_count'] == baseline_cores)]['runtime_median'].iloc[0]

    # Calculate and plot C relative speedup
    c_data = df[df['language'] == 'c']
    c_speedup = (c_base * baseline_cores) / (c_data['runtime_median'] * c_data['core_count'])
    plt.plot(c_data['core_count'], c_speedup, 
             marker='o', label='OpenMP Implementation', 
             color='#1f77b4', linewidth=2)

    # Calculate and plot Go relative speedup
    go_data = df[df['language'] == 'go']
    go_speedup = (go_base * baseline_cores) / (go_data['runtime_median'] * go_data['core_count'])
    plt.plot(go_data['core_count'], go_speedup, 
             marker='s', label='Go Implementation', 
             color='#2ca02c', linewidth=2)

    # Plot ideal relative speedup line
    core_counts = df['core_count'].unique()
    ideal_speedup = core_counts / baseline_cores  # Relative to baseline core count
    plt.plot(core_counts, ideal_speedup, '--', label='Ideal Speedup', color='#7f7f7f', linewidth=2)

    # Add vertical lines for physical cores and hardware threads
    plt.axvline(x=16, color='#5a189a', linestyle='-.', alpha=0.5, label='Physical Cores (16)')
    plt.axvline(x=32, color='#b185db', linestyle='-.', alpha=0.5, label='Hardware Threads (32)')

    plt.grid(True, which="both", ls="-", alpha=0.2)

    plt.xlabel('Number of Cores', fontsize=config["axisLabel.fontsize"])
    plt.ylabel(f'Relative Speedup (normalized to {baseline_cores} cores)', fontsize=config["axisLabel.fontsize"])
    plt.title(f'Speedup vs. Number of Cores (Baseline: {baseline_cores} cores)', fontsize=config["title.fontsize"], pad=20)
    plt.legend(fontsize=config["legend.fontsize"])

    plt.grid(True, which='major', linestyle='-', alpha=0.3)
    plt.grid(True, which='minor', linestyle=':', alpha=0.2)

    plt.tight_layout()
    plt.savefig(f'{RESULTS_DIR}/plots/relative_speedup_analysis_{dataset}.png', dpi=300, bbox_inches='tight')
    if show:
        plt.show()


def efficiency_graph(df, dataset, show=True):

    plt.figure(figsize=(12, 8))

    # Get sequential times for speedup calculation
    c_seq = df[(df['language'] == 'c') & (df['implementation'] == 'seq')]['runtime_median'].iloc[0]
    go_seq = df[(df['language'] == 'go') & (df['implementation'] == 'seq')]['runtime_median'].iloc[0]

    # Calculate and plot C efficiency
    c_data = df[(df['language'] == 'c') & (df['implementation'] == 'par')]
    c_speedup = c_seq / c_data['runtime_median']
    c_efficiency = c_speedup / c_data['core_count']
    plt.plot(c_data['core_count'], c_efficiency, 
             marker='o', label='OpenMP Implementation', 
             color='#1f77b4', linewidth=2)

    # Calculate and plot Go efficiency
    go_data = df[(df['language'] == 'go') & (df['implementation'] == 'par')]
    go_speedup = go_seq / go_data['runtime_median']
    go_efficiency = go_speedup / go_data['core_count']
    plt.plot(go_data['core_count'], go_efficiency, 
             marker='s', label='Go Implementation', 
             color='#2ca02c', linewidth=2)

    # Plot ideal efficiency line (y = 1 line)
    plt.axhline(y=1, linestyle='--', color='#7f7f7f', label='Ideal Efficiency', linewidth=2)

    plt.grid(True, which="both", ls="-", alpha=0.2)

    plt.xlabel('Number of Cores', fontsize=config["axisLabel.fontsize"])
    plt.ylabel('Efficiency (Speedup/Cores)', fontsize=config ["axisLabel.fontsize"])
    plt.title('Efficiency vs. Number of Cores', fontsize=config["title.fontsize"], pad=20)
    plt.legend(fontsize=config["legend.fontsize"])

    plt.grid(True, which='major', linestyle='-', alpha=0.3)
    plt.grid(True, which='minor', linestyle=':', alpha=0.2)

    # Set y-axis limit from 0 to slightly above max efficiency
    max_eff = max(max(c_efficiency), max(go_efficiency))
    plt.ylim(0, max(1.1, max_eff * 1.1))  # Show up to 1.1 or 110% of max efficiency

    plt.tight_layout()
    plt.savefig(f'{RESULTS_DIR}/plots/efficiency_analysis_{dataset}.png', dpi=300, bbox_inches='tight')
    if show:
        plt.show()



def main():
    SHOW = True
    # Load the data
    ds1_df = pd.read_csv(f"{RESULTS_DIR}/ds1.csv")
    ds2_df = pd.read_csv(f"{RESULTS_DIR}/ds2.csv")
    ds3_df = pd.read_csv(f"{RESULTS_DIR}/ds3.csv")

    # Runtime graphs
    # runtime_graph(ds1_df, "ds1", log=True, show=SHOW)
    # runtime_graph(ds2_df, "ds2", log=True, show=SHOW)
    runtime_graph(ds3_df, "ds3", no_seq=True, log=True, show=SHOW)

    # Speedup graphs
    # absolute_speedup_graph(ds1_df, "ds1", show=SHOW)
    # absolute_speedup_graph(ds2_df, "ds2", show=SHOW)
    # relative_speedup_graph(ds3_df, "ds3", baseline_cores=8, show=SHOW)

    # Efficiency graphs
    # efficiency_graph(ds1_df, "ds1", show=SHOW)
    # efficiency_graph(ds2_df, "ds2", show=SHOW)
    # efficiency_graph(ds3_df, "ds3", show=SHOW)


if __name__ == "__main__":
    main()