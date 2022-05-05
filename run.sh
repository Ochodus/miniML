#!/bin/bash
export OCAMLLIB=/home/mario/.opam/4.13.1+mingw64c/lib/ocaml
export PATH=/home/mario/.opam/4.13.1+mingw64c/bin:/usr/x86_64-w64-mingw32/sys-root/mingw/bin:/usr/local/bin:/usr/bin:/cygdrive/c/Program:
echo -e "\n\n\n----------------------------------Start Build----------------------------------"
make clean && make && echo -e "\nDone."
read -s -n1 -p "Press d to test dynamic-scoping or s to test static-scoping or q to quit..."
while [ ${REPLY} != "q" ] && [ ${REPLY} != "d" ] && [ ${REPLY} != "s" ]; do
    echo -e "\n"
    read -s -n1 -p "Invalid input. Press d to test dynamic-scoping or s to test static-scoping or q to quit..." 
done 
while [ ${REPLY} != "q" ]; do
    if [ ${REPLY} = "d" ]; then 
        echo -e "\n\n----------------------------------Test dynamic-scoping----------------------------------"
        for f in ./test_dynamic/*.m; do
            echo $f;
            #./run -pp $f;
            ./run -dynamic $f;
            echo -e "\n";
        done;
        echo -e "Done."
    fi
    if [ ${REPLY} = "s" ]; then 
        echo -e "\n\n\n----------------------------------Test static-scoping----------------------------------"
        for f in ./test_static/*.m; do
            echo $f;
            #./run -pp $f;
            ./run -static $f;
            echo -e "\n";
        done;
        echo -e "Done."
    fi
    read -s -n1 -p "Press d to test dynamic-scoping or s to test static-scoping or q to quit..."
    while [ ${REPLY} != "q" ] && [ ${REPLY} != "d" ] && [ ${REPLY} != "s" ]; do
        echo -e "\n"
        read -s -n1 -p "Invalid input. Press d to test dynamic-scoping or s to test static-scoping or q to quit..." 
    done 
done