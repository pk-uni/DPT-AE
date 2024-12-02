import pandas as pd
import matplotlib.pyplot as plt

PLOT_CONFIG = {
    "figure.figsize": (12, 8),
    "title.fontsize": 24,
    "axes.labelsize": 20,
    "legend.fontsize": 20,
}

RESULTS_DIR = "data/stage-2"

def prepare_data(df):
    medians = df.groupby(['dataset', 'implementation', 'workers'])['runtime'].median().reset_index()
    return medians

def plot_runtime(df, dataset, output_dir="plots", show=True):
    plt.figure(figsize=PLOT_CONFIG["figure.figsize"])
    
    # median sequential runtime if available
    seq_data = df[df['implementation'] == 'sequential']
    if not seq_data.empty:
        seq_time = seq_data['runtime'].iloc[0]
        plt.axhline(y=seq_time, color='#ff7f0e', 
                   linestyle=':', label='Sequential Runtime', alpha=0.7)
        sequential_time = seq_time
    else:
        # use first parallel measurement point to extrapolate
        first_point = df[df['implementation'] == 'parallel'].iloc[0]
        sequential_time = first_point['runtime'] * first_point['workers']

    # parallel runtimes
    par_data = df[df['implementation'] == 'parallel']
    plt.plot(par_data['workers'], par_data['runtime'], 
             marker='o', label='Erlang Implementation', 
             color='#ff7f0e', linewidth=2)
    
    # ideal scaling line
    worker_counts = par_data['workers'].unique()
    ideal_times = [sequential_time/n for n in worker_counts]
    plt.plot(worker_counts, ideal_times, '--', 
             label='Ideal Scaling', color='#7f7f7f', linewidth=2)

    # reference lines
    plt.axvline(x=16, color='#5a189a', linestyle='-.', 
                alpha=0.5, label='Physical Cores (16)')
    plt.axvline(x=32, color='#b185db', linestyle='-.', 
                alpha=0.5, label='Hardware Threads (32)')
    
    plt.grid(True, which='major', linestyle='-', alpha=0.3)
    plt.grid(True, which='minor', linestyle=':', alpha=0.2)
    
    
    plt.xlabel('Number of Workers', fontsize=PLOT_CONFIG["axes.labelsize"])
    plt.ylabel('Runtime (s)', fontsize=PLOT_CONFIG["axes.labelsize"])
    plt.title(f'Runtime - {dataset.upper()}',
              fontsize=PLOT_CONFIG["title.fontsize"], pad=20)
    plt.legend(loc="center right", fontsize=PLOT_CONFIG["legend.fontsize"])
    
    plt.tight_layout()
    plt.savefig(f'{output_dir}/erlang_runtime_{dataset}.png', 
                dpi=300, bbox_inches='tight')
    if show:
        plt.show()
    plt.close()

def plot_absolute_speedup(df, dataset, output_dir="plots", show=True):
    plt.figure(figsize=PLOT_CONFIG["figure.figsize"])
    
    # sequential runtime
    seq_time = df[df['implementation'] == 'sequential']['runtime'].iloc[0]
    
    # speedup for parallel implementation
    par_data = df[df['implementation'] == 'parallel']
    speedup = seq_time / par_data['runtime']
    
    plt.plot(par_data['workers'], speedup, 
             marker='o', label='Actual Speedup', 
             color='#ff7f0e', linewidth=2)
    
    # ideal speedup line
    max_workers = par_data['workers'].max()
    plt.plot([1, max_workers], [1, max_workers], '--', 
             label='Linear Speedup', color='#7f7f7f', linewidth=2)
    
    # reference lines
    plt.axvline(x=16, color='#5a189a', linestyle='-.', 
                alpha=0.5, label='Physical Cores (16)')
    plt.axvline(x=32, color='#b185db', linestyle='-.', 
                alpha=0.5, label='Hardware Threads (32)')
    
    plt.grid(True, which='major', linestyle='-', alpha=0.3)
    plt.grid(True, which='minor', linestyle=':', alpha=0.2)
    
    plt.xlabel('Number of Workers', fontsize=PLOT_CONFIG["axes.labelsize"])
    plt.ylabel('Speedup', fontsize=PLOT_CONFIG["axes.labelsize"])
    plt.title(f'Absolute Speedup - {dataset.upper()}',
              fontsize=PLOT_CONFIG["title.fontsize"], pad=20)
    plt.legend(loc="upper left", fontsize=PLOT_CONFIG["legend.fontsize"])
    
    plt.tight_layout()
    plt.savefig(f'{output_dir}/erlang_absolute_speedup_{dataset}.png', 
                dpi=300, bbox_inches='tight')
    if show:
        plt.show()
    plt.close()

def plot_relative_speedup(df, dataset, baseline_workers=2, output_dir="plots", show=True):
    plt.figure(figsize=PLOT_CONFIG["figure.figsize"])
    
    par_data = df[df['implementation'] == 'parallel']
    
    # baseline time
    base_time = par_data[par_data['workers'] == baseline_workers]['runtime'].iloc[0]
    
    # relative speedup
    relative_speedup = (base_time) / (par_data['runtime'])
    
    plt.plot(par_data['workers'], relative_speedup, 
             marker='o', label='Actual Speedup', 
             color='#ff7f0e', linewidth=2)
    
    # ideal speedup
    worker_counts = par_data['workers'].unique()
    ideal_speedup = worker_counts / baseline_workers
    plt.plot(worker_counts, ideal_speedup, '--', 
             label='Ideal Speedup', color='#7f7f7f', linewidth=2)
    
    # reference lines
    plt.axvline(x=16, color='#5a189a', linestyle='-.', 
                alpha=0.5, label='Physical Cores (16)')
    plt.axvline(x=32, color='#b185db', linestyle='-.', 
                alpha=0.5, label='Hardware Threads (32)')
    
    plt.grid(True, which='major', linestyle='-', alpha=0.3)
    plt.grid(True, which='minor', linestyle=':', alpha=0.2)
    
    plt.xlabel('Number of Workers', fontsize=PLOT_CONFIG["axes.labelsize"])
    plt.ylabel(f'Speedup', 
              fontsize=PLOT_CONFIG["axes.labelsize"])
    plt.title(f'Relative Speedup - {dataset.upper()}',
              fontsize=PLOT_CONFIG["title.fontsize"], pad=20)
    plt.legend(loc="upper left", fontsize=PLOT_CONFIG["legend.fontsize"])
    
    plt.tight_layout()
    plt.savefig(f'{output_dir}/erlang_relative_speedup_{dataset}.png', 
                dpi=300, bbox_inches='tight')
    if show:
        plt.show()
    plt.close()


def main():

    # for ds in ['ds1', 'ds2', 'ds3']:
    for ds in ['ds3']:
        df = pd.read_csv(f'{RESULTS_DIR}/{ds}.csv')
        df_medians = prepare_data(df)
        
        # plot_runtime(df_medians, ds, output_dir=f"{RESULTS_DIR}/plots")

        if ds in ['ds1', 'ds2']:
            plot_absolute_speedup(df_medians, ds, output_dir=f"{RESULTS_DIR}/plots")
        
        if ds == 'ds3':
            plot_relative_speedup(df_medians, ds, output_dir=f"{RESULTS_DIR}/plots", baseline_workers=2)


if __name__ == "__main__":
    main()