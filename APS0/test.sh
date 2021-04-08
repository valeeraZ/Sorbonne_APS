for file in Samples/*.aps
do
    echo $file ": " 
    type=$(./prologTerm $file | swipl -s typeChecker.pl -g main_stdin 2>&1)
    if [[ $type = *"void"* ]]; then
        echo "\033[32m Type Checking OK. \033[0m"
    else
        echo "\033[31m Type Checking error! Result: $type \033[0m"
    fi
    res=$(./evaluater $file)
    if [[ $res = "42" ]]; then
        echo "\033[32m Eval Checking OK. \033[0m"
    else
        echo "\033[31m Eval Checking error! Result: $res \033[0m" 
    fi
done