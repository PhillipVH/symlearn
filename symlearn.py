#!/usr/bin/env python3

import argparse
import subprocess
import uuid

def chunks(lst, n):
    """
    Yield successive n-sized chunks from lst.
    Source: https://stackoverflow.com/questions/312443/how-do-you-split-a-list-into-evenly-sized-chunks
    """
    for i in range(0, len(lst), n):
        yield lst[i:i + n]

def evaluate_parallel(args):

    # TODO Lightweight dict->edn implementation
    benchmark_config = '{:max-string-length 30, :oracle :' + args.oracle + ', :global-timeout ' + str(args.timeout) + '}'

    benchmarks = open(args.benchmark_file).read().split('\n')

    chunked = list(chunks(benchmarks, (len(benchmarks) // args.parallel))) # good idea to have these two divide without a remainder

    for i, chunk in enumerate(chunked):
        pod_name = args.name + '-' + str(i)
        # use check_output for Python 3.6 compatibility
        subprocess.run(['mkdir', 'results/' + pod_name], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

        benchmark_config_file = open('results/' + pod_name + '/benchmark.re', 'w')
        benchmark_config_file.write('\n'.join(chunk))
        benchmark_config_file.close()

        benchmark_config_file = open('results/' + pod_name + '/benchmark-config.edn', 'w')
        benchmark_config_file.write(benchmark_config)
        benchmark_config_file.close()

        if not args.dry:
            subprocess.run(['bash', 'docker/docker_start_pod.sh', pod_name])


def evaluate(args):
    if args.parallel:
        evaluate_parallel(args)
    else:
        args.parallel = 1
        evaluate_parallel(args)


def logs(args):
    subprocess.run(['bash', 'docker/docker_pod_logs.sh', args.name])


def init(args):
    subprocess.run(['git', 'submodule', 'init'])
    subprocess.run(['git', 'submodule', 'update'])
    subprocess.run(['bash', 'docker/docker_build_image.sh'])


def killall(args):
    subprocess.run(['bash', 'docker/docker_kill_all_pods.sh'], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)


# Create the top-level argument parser
parser = argparse.ArgumentParser()
subparsers = parser.add_subparsers()

# Create the parser for the deploy command
parser_evaluate = subparsers.add_parser('evaluate')
parser_evaluate.add_argument('--name', type=str, help='human-readable label for the evaluation', required=True)
parser_evaluate.add_argument('--benchmark-file', type=str, metavar='FILE', help='file containing newline delimited regular expressions', required=True)
parser_evaluate.add_argument('--timeout', type=int, metavar='N', help='timeout for each symbolic equivalence query in minutes', required=True)
parser_evaluate.add_argument('--oracle', type=str, metavar='ORACLE', default='coastal', help='use coastal (default), gtestr, or a perfect oracle for equivalence')
parser_evaluate.add_argument('--max-string-length', type=int, metavar='N', default=30, help='maximum string length used in equivalence queries (default 30)')

parser_evaluate.add_argument('--dry', action='store_true', help='do not start the experiments containers')
parser_evaluate.add_argument('--parallel', type=int, metavar='N', help='start N experiments in parallel, dividing the benchmark files between them')

parser_evaluate.set_defaults(func=evaluate)

# Create the parser for the logs command
parser_logs = subparsers.add_parser('logs')
parser_logs.add_argument('name', type=str, help='human-readable label of the deployment')

parser_logs.set_defaults(func=logs)

# Create the parser for the init command
parser_logs = subparsers.add_parser('init')

parser_logs.set_defaults(func=init)

# Create the parser for the stop all command
parser_killall = subparsers.add_parser('killall')

parser_killall.set_defaults(func=killall)

if __name__ == '__main__':
    # Parse the args and dispatch to the correct fn
    try:
        args = parser.parse_args()
        args.func(args)
    except AttributeError :
        parser.print_help()
