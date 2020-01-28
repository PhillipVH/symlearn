#!/usr/bin/env python3

import argparse
import subprocess
import uuid


def evaluate(args):
    subprocess.run(['mkdir', 'results/' + args.name])  # fails if the folder exists, sanity check for the lazy
    subprocess.run(['cp', args.benchmark_file, 'results/' + args.name + '/benchmark.re'])

    spec_file = open('results/' + args.name + '/benchmark.spec', 'w')

    spec_file.write(str(args.depth_limit) + '\n' + str(1000 * 60 * args.timeout))
    spec_file.close()

    if not args.dry:
        subprocess.run(['bash', 'docker_start_pod.sh', args.name])


def logs(args):
    subprocess.run(['bash', 'docker_pod_logs.sh', args.name])


def init(args):
    subprocess.run(['git', 'submodule', 'init'])
    subprocess.run(['git', 'submodule', 'update'])
    subprocess.run(['bash', 'docker_build_image.sh'])


# Create the top-level argument parser
parser = argparse.ArgumentParser()
subparsers = parser.add_subparsers()

# Create the parser for the deploy command
parser_evaluate = subparsers.add_parser('evaluate')
parser_evaluate.add_argument('name', type=str, help='human-readable label for the evaluation')
parser_evaluate.add_argument('benchmark_file', type=str, help='file containing newline delimited regular expressions')
parser_evaluate.add_argument('depth_limit', type=int, help='depth limit for symbolic equivalence queries')
parser_evaluate.add_argument('timeout', type=int, help='timeout for each symbolic equivalence query in minutes')

parser_evaluate.add_argument('--dry', action='store_true', help='do not start the Docker containers')

parser_evaluate.set_defaults(func=evaluate)

# Create the parser for the logs command
parser_logs = subparsers.add_parser('logs')
parser_logs.add_argument('name', type=str, help='human-readable label of the deployment')

parser_logs.set_defaults(func=logs)

# Create the parser for the init command
parser_logs = subparsers.add_parser('init')

parser_logs.set_defaults(func=init)

if __name__ == '__main__':
    # Parse the args and dispatch to the correct fn
    try:
        args = parser.parse_args()
        args.func(args)
    except AttributeError:
        parser.print_help()
