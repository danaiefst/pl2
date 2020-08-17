import os
import sys

for i in ["test_1", "test_2", "test_3", "test_4", "test_5"]:
    out = os.popen("./{} < {}".format(sys.argv[1], i)).readlines()
    out_right = open("{}_out".format(i), "r").readlines()
    for j in range(len(out_right)):
        if out_right[j] != out[j]:
            print("Wrong output, file {}, line {}: \ngot : {}should be : {}".format(i, j+1, out[j], out_right[j]))
            exit(1)
    print("{}: Correct!".format(i))
