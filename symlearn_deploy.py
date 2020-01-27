import argparse
import subprocess
import uuid

parser = argparse.ArgumentParser()
parser.add_argument('name', type=str, help='human-readable label for the deployment')
parser.add_argument('benchmark_file', type=str, help='file containing newline delimited regular expressions')
parser.add_argument('depth_limit', type=int, help='the depth limit for symbolic equivalence queries')

parser.add_argument('--timeout_ms', 
					type=int, 
					help='the timeout for each symbolic equivalence query in milliseconds (defaults to 10 minutes)')

parser.add_argument('--dry', action='store_true', help='do not start the docker containers')

args = parser.parse_args()

subprocess.run(['mkdir', 'results/' + args.name]) # fails if the folder exists, sanity check for the lazy
subprocess.run(['cp', args.benchmark_file, 'results/' + args.name + '/benchmark.re'])

if not args.dry:
	subprocess.run(['bash', 'docker_startall.sh', args.name])
