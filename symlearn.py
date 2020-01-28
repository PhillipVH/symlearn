#!/usr/bin/env python3

import argparse
import subprocess
import uuid


def deploy(args):
    subprocess.run(['mkdir', 'results/' + args.name])  # fails if the folder exists, sanity check for the lazy
    subprocess.run(['cp', args.benchmark_file, 'results/' + args.name + '/benchmark.re'])

    spec_file = open('results/' + args.name + '/benchmark.spec', 'w')

    spec_file.write(str(args.depth_limit) + '\n' + str(1000 * 60 * args.timeout))
    spec_file.close()

    if not args.dry:
        subprocess.run(['bash', 'docker_start_pod.sh', args.name])


def logs(args):
    subprocess.run(['bash', 'docker_pod_logs.sh', args.name])


# Create the top-level argument parser
parser = argparse.ArgumentParser()
subparsers = parser.add_subparsers()

# Create the parser for the deploy command
parser_deploy = subparsers.add_parser('deploy')
parser_deploy.add_argument('name', type=str, help='human-readable label for the deployment')
parser_deploy.add_argument('benchmark_file', type=str, help='file containing newline delimited regular expressions')
parser_deploy.add_argument('depth_limit', type=int, help='depth limit for symbolic equivalence queries')
parser_deploy.add_argument('timeout', type=int, help='timeout for each symbolic equivalence query in minutes')

parser_deploy.add_argument('--dry', action='store_true', help='do not start the docker containers')

parser_deploy.set_defaults(func=deploy)

# Create the parser for the logs command
parser_logs = subparsers.add_parser('logs')
parser_logs.add_argument('name', type=str, help='human-readable label of the deployment')

parser_logs.set_defaults(func=logs)

if __name__ == '__main__':
    # Parse the args and dispatch to the correct fn
    try:
        args = parser.parse_args()
        args.func(args)
    except AttributeError:
        parser.print_help()
