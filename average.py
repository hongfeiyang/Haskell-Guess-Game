# Some useful libraries imported
import subprocess
import itertools
import string
import time
import sys
import os

# Global lists representing notes and octaves in a musical note
NOTES = [chr(i) for i in range(ord('A'), ord('G') + 1)]
OCTAVES = [str(i) for i in range(1, 4)]

# No comment
NUMPITCHES = 3

def combinations(notes, octaves):
    """Function which creates possible pitches

    Args:
        param1 (list): list of notes(str)
        param2 (list): list of octaves(str)

    Returns:
        Possible combinations of pitches

    """
    combinations = []
    for note in notes:
        for octave in octaves:
            combinations.append(note + octave)

    return combinations

def k_combinations(lst, k):
    """Returns possible combinations of available pitches in 
       groups of three.

       Args:
           param1 (list): list of notes(str)
           param2 (list): list of octaves(str)

       Returns:
           Possible combinations of pitches in groups of 3

    """

    # Yes, list comprehensions are beautiful
    return [" ".join(item) for item in itertools.combinations(lst, k)]

def main():
    """Main function of program. Executes python script from here.

       Args:
            None

       Returns:
            None

    """

    # The clock has started, time is ticking
    start_time = time.time()

    # Lets create the possible targets
    possible_pitches = combinations(NOTES, OCTAVES)
    args = k_combinations(possible_pitches, NUMPITCHES)

    # initialise step size
    stepsize = None

    # check command line arguements
    if len(sys.argv) > 2:
        print("""This script get the average number of guesses
                 for n targets.

                 Usage: -Script -stepsize(positive integer)
                 step size is optional
                 default: all possible targets available""")
        sys.exit(1)

    elif len(sys.argv) == 2:
        stepsize = sys.argv[1]

        # check if step size is in fact an integer
        # used for naughty people who input bad things
        try:
            int(stepsize)
        except ValueError:
            print("Error: Step size must be an integer")
            sys.exit(1)

        stepsize = int(stepsize)

        # slice the list based on step size, how exciting
        if stepsize > 0:
            args = args[::stepsize]

    # guess count results from test go here
    results = []

    testcount = 1

    # run 'Proj1Test' on targets and collect the results
    for arg in args:
        last = subprocess.getoutput(os.getcwd() + '/Proj1Test ' + arg)

        # print out current test, something to look at while waiting I guess
        print("%s\t\t%d" % (last, testcount))

        testcount += 1

        results.append(int(str(last).split("\n")[-1].split(' ')[-2]))

    # print out some helpful stats
    print("\nAverage: %.2f guesses" % (sum(results) / len(results)))
    print("CPU time: %s" % (time.time() - start_time))
    print("Possible Targets Available: %d" % (len(args)))

    if stepsize != None:
        print("Step Size Chosen: %d" % (stepsize))
    else:
        print("Step Size Chosen: None")

if __name__ == "__main__":
    main()


