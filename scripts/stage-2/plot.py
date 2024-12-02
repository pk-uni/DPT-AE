import pandas as pd
import matplotlib.pyplot as plt

# Basic configuration for plots
PLOT_CONFIG = {
    "figure.figsize": (12, 8),
    "title.fontsize": 24,
    "axes.labelsize": 20,
    "legend.fontsize": 20,
}

RESULTS_DIR = "data/erl_results"

def prepare_data(df):
    # Group by dataset, implementation, and workers, then calculate median runtime
    medians = df.groupby(['dataset', 'implementation', 'workers'])['runtime'].median().reset_index()
    return medians

def plot_runtime(df, dataset, output_dir="plots", show=True):
    plt.figure(figsize=PLOT_CONFIG["figure.figsize"])
    
    # Get median sequential runtime if available
    seq_data = df[df['implementation'] == 'sequential']
    if not seq_data.empty:
        seq_time = seq_data['runtime'].iloc[0]
        plt.axhline(y=seq_time, color='#ff7f0e', 
                   linestyle=':', label='Sequential Runtime', alpha=0.7)
        sequential_time = seq_time
    else:
        # Use first parallel measurement point to extrapolate
        first_point = df[df['implementation'] == 'parallel'].iloc[0]
        sequential_time = first_point['runtime'] * first_point['workers']

    # Plot parallel runtimes
    par_data = df[df['implementation'] == 'parallel']
    plt.plot(par_data['workers'], par_data['runtime'], 
             marker='o', label='Erlang Implementation', 
             color='#ff7f0e', linewidth=2)
    
    # Plot ideal scaling line
    worker_counts = par_data['workers'].unique()
    ideal_times = [sequential_time/n for n in worker_counts]
    plt.plot(worker_counts, ideal_times, '--', 
             label='Ideal Scaling', color='#7f7f7f', linewidth=2)

    # Add reference lines
    plt.axvline(x=16, color='#5a189a', linestyle='-.', 
                alpha=0.5, label='Physical Cores (16)')
    plt.axvline(x=32, color='#b185db', linestyle='-.', 
                alpha=0.5, label='Hardware Threads (32)')
    
    plt.grid(True, which='major', linestyle='-', alpha=0.3)
    plt.grid(True, which='minor', linestyle=':', alpha=0.2)
    
    plt.xscale('log', base=2)
    plt.yscale('log', base=2)
    
    plt.xlabel('Number of Workers', fontsize=PLOT_CONFIG["axes.labelsize"])
    plt.ylabel('Runtime (s)', fontsize=PLOT_CONFIG["axes.labelsize"])
    plt.title(f'Erlang Runtime Analysis - {dataset.upper()}',
              fontsize=PLOT_CONFIG["title.fontsize"], pad=20)
    plt.legend(fontsize=PLOT_CONFIG["legend.fontsize"])
    
    plt.tight_layout()
    plt.savefig(f'{output_dir}/erlang_runtime_{dataset}.png', 
                dpi=300, bbox_inches='tight')
    if show:
        plt.show()
    plt.close()

def plot_absolute_speedup(df, dataset, output_dir="plots", show=True):
    plt.figure(figsize=PLOT_CONFIG["figure.figsize"])
    
    # Get sequential runtime
    seq_time = df[df['implementation'] == 'sequential']['runtime'].iloc[0]
    
    # Calculate and plot speedup for parallel implementation
    par_data = df[df['implementation'] == 'parallel']
    speedup = seq_time / par_data['runtime']
    
    plt.plot(par_data['workers'], speedup, 
             marker='o', label='Actual Speedup', 
             color='#ff7f0e', linewidth=2)
    
    # Plot ideal speedup line
    max_workers = par_data['workers'].max()
    plt.plot([1, max_workers], [1, max_workers], '--', 
             label='Linear Speedup', color='#7f7f7f', linewidth=2)
    
    # Add reference lines
    plt.axvline(x=16, color='#5a189a', linestyle='-.', 
                alpha=0.5, label='Physical Cores (16)')
    plt.axvline(x=32, color='#b185db', linestyle='-.', 
                alpha=0.5, label='Hardware Threads (32)')
    
    plt.grid(True, which='major', linestyle='-', alpha=0.3)
    plt.grid(True, which='minor', linestyle=':', alpha=0.2)
    
    plt.xlabel('Number of Workers', fontsize=PLOT_CONFIG["axes.labelsize"])
    plt.ylabel('Speedup', fontsize=PLOT_CONFIG["axes.labelsize"])
    plt.title(f'Erlang Absolute Speedup - {dataset.upper()}',
              fontsize=PLOT_CONFIG["title.fontsize"], pad=20)
    plt.legend(fontsize=PLOT_CONFIG["legend.fontsize"])
    
    plt.tight_layout()
    plt.savefig(f'{output_dir}/erlang_absolute_speedup_{dataset}.png', 
                dpi=300, bbox_inches='tight')
    if show:
        plt.show()
    plt.close()

def plot_relative_speedup(df, dataset, baseline_workers=8, output_dir="plots", show=True):
    plt.figure(figsize=PLOT_CONFIG["figure.figsize"])
    
    par_data = df[df['implementation'] == 'parallel']
    
    # Get baseline time
    base_time = par_data[par_data['workers'] == baseline_workers]['runtime'].iloc[0]
    
    # Calculate and plot relative speedup
    relative_speedup = (base_time * baseline_workers) / (par_data['runtime'] * par_data['workers'])
    
    plt.plot(par_data['workers'], relative_speedup, 
             marker='o', label='Actual Speedup', 
             color='#ff7f0e', linewidth=2)
    
    # Plot ideal relative speedup
    worker_counts = par_data['workers'].unique()
    ideal_speedup = worker_counts / baseline_workers
    plt.plot(worker_counts, ideal_speedup, '--', 
             label='Ideal Speedup', color='#7f7f7f', linewidth=2)
    
    # Add reference lines
    plt.axvline(x=16, color='#5a189a', linestyle='-.', 
                alpha=0.5, label='Physical Cores (16)')
    plt.axvline(x=32, color='#b185db', linestyle='-.', 
                alpha=0.5, label='Hardware Threads (32)')
    
    plt.grid(True, which='major', linestyle='-', alpha=0.3)
    plt.grid(True, which='minor', linestyle=':', alpha=0.2)
    
    plt.xlabel('Number of Workers', fontsize=PLOT_CONFIG["axes.labelsize"])
    plt.ylabel(f'Speedup (normalized to {baseline_workers} workers)', 
              fontsize=PLOT_CONFIG["axes.labelsize"])
    plt.title(f'Erlang Relative Speedup - {dataset.upper()}',
              fontsize=PLOT_CONFIG["title.fontsize"], pad=20)
    plt.legend(fontsize=PLOT_CONFIG["legend.fontsize"])
    
    plt.tight_layout()
    plt.savefig(f'{output_dir}/erlang_relative_speedup_{dataset}.png', 
                dpi=300, bbox_inches='tight')
    if show:
        plt.show()
    plt.close()


def main():

    for ds in ['ds1', 'ds2', 'ds3']:
        # Load and prepare data
        df = pd.read_csv(f'{RESULTS_DIR}/{ds}.csv')
        df_medians = prepare_data(df)
        
        # Generate plots
        plot_runtime(df_medians, ds, output_dir=f"{RESULTS_DIR}/plots")
        
        # Only generate absolute speedup for ds1 and ds2 which have sequential data
        if ds in ['ds1', 'ds2']:
            plot_absolute_speedup(df_medians, ds, output_dir=f"{RESULTS_DIR}/plots")
        
        # Generate relative speedup for all datasets
        if ds == 'ds3':
            plot_relative_speedup(df_medians, ds, output_dir=f"{RESULTS_DIR}/plots", baseline_workers=8)


if __name__ == "__main__":
    main()