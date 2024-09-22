from snakemake.utils import min_version

min_version("7.20.0")
container: 'docker://koki/sparsearray-experiments:20240922'

ARITHMETICS = ["vec", "sum", "innerprod", "kronecker"]
METHODS = ["array", "sparsearray"]
SIZES = ["1E7", "5E7", "1E8", "5E8"]

rule all:
	input:
		expand('plot/memtime/time/{a}.png', a=ARITHMETICS),
		expand('plot/memtime/memory/{a}.png', a=ARITHMETICS),
		expand('plot/benchmark/time/{a}.png', a=ARITHMETICS),
		expand('plot/benchmark/memory/{a}.png', a=ARITHMETICS),
		expand('plot/gnutime/time/{a}.png', a=ARITHMETICS),
		expand('plot/gnutime/memory/{a}.png', a=ARITHMETICS)

rule memtime:
	output:
		'memtime/{a}_{m}_{s}.RData'
	resources:
		mem_gb=50
	benchmark:
		'benchmarks/{a}_{m}_{s}.txt'
	log:
		'logs/{a}_{m}_{s}.log'
	shell:
		'/usr/bin/time -v src/memtime.sh {wildcards.a} {wildcards.m} {wildcards.s} {output} >& {log}'

rule plot_memtime_time:
	input:
		expand('memtime/{a}_{m}_{s}.RData',
			a=ARITHMETICS, m=METHODS, s=SIZES)
	output:
		'plot/memtime/time/{a}.png'
	resources:
		mem_gb=50
	benchmark:
		'benchmarks/plot_memtime_time_{a}.txt'
	log:
		'logs/plot_memtime_time_{a}.log'
	shell:
		'src/plot_memtime_time.sh {wildcards.a} {output} >& {log}'

rule plot_memtime_memory:
	input:
		expand('memtime/{a}_{m}_{s}.RData',
			a=ARITHMETICS, m=METHODS, s=SIZES)
	output:
		'plot/memtime/memory/{a}.png'
	resources:
		mem_gb=50
	benchmark:
		'benchmarks/plot_memtime_memory_{a}.txt'
	log:
		'logs/plot_memtime_memory_{a}.log'
	shell:
		'src/plot_memtime_memory.sh {wildcards.a} {output} >& {log}'

rule plot_benchmark_time:
	input:
		expand('memtime/{a}_{m}_{s}.RData',
			a=ARITHMETICS, m=METHODS, s=SIZES)
	output:
		'plot/benchmark/time/{a}.png'
	resources:
		mem_gb=50
	benchmark:
		'benchmarks/plot_benchmark_time_{a}.txt'
	log:
		'logs/plot_benchmark_time_{a}.log'
	shell:
		'src/plot_benchmark_time.sh {wildcards.a} {output} >& {log}'

rule plot_benchmark_memory:
	input:
		expand('memtime/{a}_{m}_{s}.RData',
			a=ARITHMETICS, m=METHODS, s=SIZES)
	output:
		'plot/benchmark/memory/{a}.png'
	resources:
		mem_gb=50
	benchmark:
		'benchmarks/plot_benchmark_memory_{a}.txt'
	log:
		'logs/plot_benchmark_memory_{a}.log'
	shell:
		'src/plot_benchmark_memory.sh {wildcards.a} {output} >& {log}'

rule plot_gnutime_time:
	input:
		expand('memtime/{a}_{m}_{s}.RData',
			a=ARITHMETICS, m=METHODS, s=SIZES)
	output:
		'plot/gnutime/time/{a}.png'
	resources:
		mem_gb=50
	benchmark:
		'benchmarks/plot_gnutime_time_{a}.txt'
	log:
		'logs/plot_gnutime_time_{a}.log'
	shell:
		'src/plot_gnutime_time.sh {wildcards.a} {output} >& {log}'

rule plot_gnutime_memory:
	input:
		expand('memtime/{a}_{m}_{s}.RData',
			a=ARITHMETICS, m=METHODS, s=SIZES)
	output:
		'plot/gnutime/memory/{a}.png'
	resources:
		mem_gb=50
	benchmark:
		'benchmarks/plot_gnutime_memory_{a}.txt'
	log:
		'logs/plot_gnutime_memory_{a}.log'
	shell:
		'src/plot_gnutime_memory.sh {wildcards.a} {output} >& {log}'
