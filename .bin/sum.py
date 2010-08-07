#!/usr/bin/env python

import fileinput

def main():
    print sum(float(line.strip()) for line in fileinput.input())

if __name__ == '__main__':
    main()
