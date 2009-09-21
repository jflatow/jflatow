#!/usr/bin/env python

import sys, fileinput, optparse, signal

signal.signal(signal.SIGINT, signal.SIG_DFL)
signal.signal(signal.SIGPIPE, signal.SIG_DFL)

class FormatWarning(UserWarning):
    pass

def expand(field):
    interval = map(int, field.split('-', 1))
    if len(interval) > 1:
        return range(interval[0] - 1, interval[1])
    return [int(field) - 1]
    
def main():
    parser = optparse.OptionParser()
    parser.add_option('-f', '--fields',
                      help='which fields to parallelize')
    parser.add_option('-s', '--skip', action='store_true',
                      help='skip lines without enough fields?')
    parser.add_option('-F', '--separator', default='\t',
                      help='the field separator')
    options, sys.argv = parser.parse_args(sys.argv)
    if not options.fields:
        parser.error('Must specify fields option')

    sep = options.separator
    fields = sum((expand(n) for n in options.fields.split(',') if n), [])

    for line in fileinput.input():
        parts = line.strip().split(sep)
        try:
            print sep.join(parts[f] for f in fields)
        except IndexError:
            if not options.skip:
                raise FormatWarning('Not obvious how to process line: %s' % line)

if __name__ == '__main__':
    main()
