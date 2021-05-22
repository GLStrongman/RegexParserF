#!/usr/bin/python3
#
# This script will check how many regular expression/target string pairs have
# produced the correct result, and report statistics on them.
# It can be used in three ways:
# - You can provide two filenames as arguments, the first containing the output
#   of your program, and the second the expected results for that set of
#   patterns as provided, and it will report how many were correct overall,
#   as well as how many matches/non-matches/syntax errors were computed
#   accurately.
#
#   It will write a list of any non-matching lines into mismatches.txt,
#   including line numbers and both the expected and actual result.
# - If you put your program's output into a file called e.g. parens-output.txt
#   next to the parens-expected.txt file, you can provide a single argument
#   "parens" to compare those two files, and produce the same report as above.
# - If you have many files parens-outputs.txt, wild-outputs.txt, etc, next to
#   the *-expected.txt files, you can provide a single argument --all, and it
#   will report just the percentage of each group that was correctly matched.
#   You can also provide a second argument that is the path to the directory
#   containing all of those files. This will not produce a list of mismatches,
#   or break down the matching/non-matching/error lines, just show the
#   percentage correct from each group.
#
# Run it as e.g.
#     python3 checker/check.py output.txt expected.txt
# Save your output into a file with e.g.
#     ruby regex1.rb expressions.txt targets.txt > output.txt

import sys
import os
import os.path

filename1, filename2 = '', ''
if len(sys.argv) < 2 or len(sys.argv) > 3:
    print("Usage:")
    print(f"{sys.argv[0]} <output-file> <expected-file>")
    print("\tCompares starts of lines of output-file and expected-file and reports.")
    print(f"{sys.argv[0]} <prefix>")
    print("\tCompares <prefix>-output.txt and <prefix>-expected.txt.")
    print(f"{sys.argv[0]} --all [<dir>]")
    print("\tFor each file *-output.txt, produces summary statistic only.")
    print()
    print("Prints statistics for total and for each possible result.")
    print("Writes lines where output did not match expected to mismatches.txt")
    sys.exit(0)
elif sys.argv[1] == '--all':
    for f in os.listdir('.' if len(sys.argv) == 2 else sys.argv[2]):
        if f.endswith('-output.txt'):
            prefix = f[:-11]
            path = os.path.join('.' if len(sys.argv) == 2 else sys.argv[2], prefix)
            with open(path + '-output.txt') as fp_output:
                with open(path + '-expected.txt') as fp_expect:
                    expected = fp_expect.readlines()
                    correct = 0
                    for got, expect in zip(fp_output.readlines(), expected):
                        if got[:2] == expect[:2]:
                            correct += 1
                    print(f"{prefix:12s}{correct/len(expected):2.2%}")
    sys.exit(0)
elif len(sys.argv) == 3:
    filename1 = sys.argv[1]
    filename2 = sys.argv[2]
elif len(sys.argv) == 2:
    filename1 = sys.argv[1] + '-output.txt'
    filename2 = sys.argv[1] + '-expected.txt'

lines1, lines2 = [], []
with open(filename1) as fp1:
    lines1 = fp1.readlines()
with open(filename2) as fp2:
    lines2 = fp2.readlines()

expected = {'YE': 0, 'NO': 0, 'SY': 0, 'ER': 0}
correct  = {'YE': 0, 'NO': 0, 'SY': 0, 'ER': 0}
extras   = {'YE': 0, 'NO': 0, 'SY': 0, 'ER': 0}
mismatches = []
total = len(lines2)
right = 0
count = 0
for line1, line2 in zip(lines1, lines2):
    start1 = line1[:2]
    start2 = line2[:2]
    count += 1
    expected[start2] += 1
    if start1 == start2:
        correct[start2] += 1
        right += 1
    else:
        mismatches.append((count, line1, line2))

print(f"Total: {right/total:2.2%} {right}/{total}")
print("Proportions of expected results right:")
for short, name in {'YE': 'Match', 'NO': 'No match', 'SY': 'Syntax error', 'ER': 'Other error'}.items():
    if expected[short] > 0:
        pc = correct[short] / expected[short]
        print(f"\t{name:12s} {pc: 2.2%} {correct[short]}/{expected[short]} + {extras[short]} unexpected")
    else:
        print(f"\t{name:12s} {0: 2.2%} {correct[short]}/{expected[short]} + {extras[short]} unexpected")
missing = len(lines2) - len(lines1)
if missing:
    print(f"Missing: {missing/len(lines2):2.2%} {missing}/{len(lines2)}")
    print("Output file finished early!")

if mismatches:
    with open('mismatches.txt', 'w') as fp:
        for line, got, expected in mismatches:
            fp.write(f"Mismatch on line {line}:\n")
            fp.write(f"  Expected: {expected}")
            fp.write(f"  But got:  {got}")
    print(f"Wrote list of {len(mismatches)} mismatched lines to mismatches.txt")
print(f"\nTotal: {right/total:2.2%} {right}/{total}")