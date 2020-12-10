source('scripts/clean_data.r')
source('scripts/modeling.r')
source('scripts/benchmark_metrics.r')


# final_data <- generate_data()

final_data = read.csv(here::here('data/final_teams_salary.csv'))[,-1]


print('Brenchmarks off of randomized predictions:')
print(run_benchmarks(final_data))


modeling_outputs <- fit_models(final_data)

print('Model Outputs:')
print(modeling_outputs)
