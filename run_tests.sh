#!/bin/bash

# Based on MicroC Regression Test Suite Script (microc/testall.sh)

# 0 stdin
# 1 stdout
# 2 stderr


MARMALADE="./marmac"
    #marmac depends on "marmalade" compiler

# Set time limit for all operations
ulimit -t 30

globallog=tests.log
rm -f $globallog
error=0
globalerror=0

keep=0

Usage() {
    echo "Usage: run_tests.sh [options] [.marm files]"
    echo "-k    Keep intermediate files"
    echo "-h    Print this help"
    exit 1
}

SignalError() {
    if [ $error -eq 0 ] ; then
	echo "FAILED"
	error=1
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    cat $1 >&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
	SignalError "$1 differs"
	echo "FAILED $1 differs from $2" 1>&2
    }
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

Check() {
    # $1    name of basename file   (i.e. test_arith_add1)
    # $2    name of testdir         (i.e. testdir_2015-11-24_061339)

    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.marm//'`
    reffile=`echo $1 | sed 's/.marm$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""


    # GENERATE .java .class <outfile>.t.out .t.diff FILES

    generatedfiles="$2/${basename}.t.out" 
    Run "$MARMALADE" "$1" "${basename}" 
    ./${basename} &> "./$2/${basename}.t.out"
    mv "${basename}" "$2/${basename}" 
    mv "${basename}.java" "$2/${basename}.java" 
    mv "${basename}.class" "$2/${basename}.class"
    mv *.class "$2/"

    Compare $2/${basename}.t.out ${reffile}.out $2/${basename}.t.diff

    echo

    generatedfiles="$generatedfiles $2/${basename}.t.out $2/${reffile}.out $2${basename}.t.diff"
    generatedfiles="$generatedfiles $2/${basename} $2/${basename}.java $2/${basename}.class"
    
    
    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
	if [ $keep -eq 0 ] ; then
	   # rm -f $generatedfiles
       echo ""
	fi
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}



#BEGINNING OF SCRIPT
#BEGINNING OF SCRIPT
#BEGINNING OF SCRIPT

while getopts kdpsh c; do
    case $c in
	k) # Keep intermediate files
	    keep=1
	    ;;
	h) # Help
	    Usage
	    ;;
    esac
done

shift `expr $OPTIND - 1`

if [ $# -ge 1 ]
then
    files=$@
else
    files="tests/fail_* tests/test_*"
    # files="tests/fail_*.marm tests/test_*.marm"
fi



# AUTO ARCHIVE TEST FILES
if [ -d "testing_archive" ]; then
    mv testdir_* testing_archive/
else
    mkdir "testing_archive"
fi


# CREATE NEW TEST DIR FOR INTERMEDIATE FILES
date=`date +%F_%H%M%S`
testdir="testdir_${date}"
mkdir "$testdir"


for file in $files
do
    case $file in
    *.out)
        ;;
	*test_*)
	    Check $file $testdir 2>> $globallog
	    ;;
	*fail_*)
	    CheckFail $file $testdir 2>> $globallog
	    ;;
	*)
	    echo "unknown file type $file"
	    globalerror=1
	    ;;
    esac
done


exit $globalerror
