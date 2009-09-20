#!/usr/bin/env python
"""
Devil: manage daemon processes.
"""

import base64, signal, subprocess, sys, os

commands = {}
def command(function):
    commands[function.__name__] = function
    return function

@command
def pid(args):
    process = subprocess.Popen(['ps', '-x', '-o', 'pid,command'], stdout=subprocess.PIPE)
    for line in process.stdout:
        tokens = line.split()
        pid, command = tokens[0], tokens[1:]
        if command == args:
            return int(pid)

@command
def start(args):
    if status(args) == 'running':
        return 'already started'
    process = subprocess.call(args)
    return status(args)

@command
def status(args):
    if pid(args):
        return 'running'
    return 'stopped'
                                            
@command
def stop(args):
    if status(args) == 'stopped':
        return 'already stopped'
    try:
        os.kill(pid(args), signal.SIGTERM)
        while status(args) == 'running':
            pass
    except Exception:
        pass
    return status(args)
                                                    
@command
def restart(args):
    stop(args)
    start(args)
    return status(args)

@command
def lookup(args, daemon_file=os.path.join(os.getenv('HOME'), '.daemons')):
    if os.path.exists(daemon_file):
        with open(daemon_file) as handle:
            for line in handle:
                name, daemon = line.split('\t', 1)
                if name.split() == args:
                    return daemon.split()
    return args

@command
def invalid(args):
    return 'did not specify a valid devil command'

def main():
    command, daemon = commands.get(sys.argv[1], invalid), sys.argv[2:]
    args = lookup(daemon)
    if args:
        print('%s: %s' % (' '.join(daemon), command(args)))

if __name__ == '__main__':
    main()
