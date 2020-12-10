source('scripts/clean_data.r')
source('scripts/modeling.r')
source('scripts/benchmark_metrics.r')

final_data <- generate_data()

print('Accuracy and f1 score for randomized prediction:')
print(generate_benchmarks(final_data))


modeling_outputs <- run_models()
print(modeling_outputs)
